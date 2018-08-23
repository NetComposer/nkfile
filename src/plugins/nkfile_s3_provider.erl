%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc NkFILE callbacks

-module(nkfile_s3_provider).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behavior(nkfile_provider).

-export([config/0, parse_spec/1, upload/5, download/4, start/4]).
-export_type([result_meta/0]).

-include("nkfile.hrl").
-define(LLOG(Type, Txt, Args),lager:Type("NkFILE S3 "++Txt, Args)).

%% @doc returned metadata
-type result_meta() ::
    #{
        s3_headers => map()
    }.



%% @doc
config() ->
    #{
        storageClass => <<"s3">>
    }.


%% @doc
parse_spec(_Spec) ->
    {syntax, #{
        maxSize => pos_integer,
        encryption => {atom, [aes_cfb128]},
        debug => boolean,
        targets => {list, #{
            url => binary,
            opts => nkpacket_syntax:safe_syntax(),
            weight => {integer, 1, 1000},
            pool => {integer, 1, 1000},
            refresh => boolean,
            '__mandatory' => [url]
        }},
        s3_Id => binary,
        s3_Secret => binary,
        bucket => binary,
        resolveInterval => {integer, 0, none},
        '__mandatory' => [targets, s3_Id, s3_Secret, bucket],
        '__allow_unknown' =>true
    }}.



%% @private
upload(SrvId, PackageId, ProviderSpec, FileMeta, FileBin) ->
    #{name:=Name} = FileMeta,
    FilePath1 = maps:get(path, FileMeta, <<"/">>),
    FilePath2 = filename:join(FilePath1, to_bin(Name)),
    #{s3_Id:=Id, s3_Secret:=Secret, bucket:=Bucket} = ProviderSpec,
    PoolId = {nkfile_s3, SrvId, PackageId},
    lager:error("NKLOG pOOL ~p", [PoolId]),
    case nkpacket_pool:get_conn_pid(PoolId) of
        {ok, ConnPid, #{url:=Url}} ->
            S3 = #{
                key_id => Id,
                key => Secret,
                bucket => Bucket,
                host => Url
            },
            Hash = crypto:hash(sha256, FileBin),
            {_Uri, Path, Headers} = nkpacket_httpc_s3:put_object(Bucket, FilePath2, Hash, S3),
            case nkpacket_httpc:do_request(ConnPid, put, Path, Headers, FileBin, #{no_host_header=>true}) of
                {ok, 200, Hds, _} ->
                    {ok, #{s3_headers=>Hds}};
                {ok, 403, _, _} ->
                    {error, unauthorized};
                {ok, 404, _, _} ->
                    {error, file_not_found};
                {ok, Code, Hds, Body} ->
                    {error, {http_error, Code, Hds, Body}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
download(SrvId, PackageId, ProviderSpec, FileMeta) ->
    #{name:=Name} = FileMeta,
    Path1 = maps:get(path, FileMeta, <<"/">>),
    Path2 = filename:join(Path1, to_bin(Name)),
    #{s3_Id:=Id, s3_Secret:=Secret, bucket:=Bucket} = ProviderSpec,
    PoolId = {nkfile_s3, SrvId, PackageId},
    case nkpacket_pool:get_conn_pid(PoolId) of
        {ok, ConnPid, #{url:=Url}} ->
            S3 = #{
                key_id => Id,
                key => Secret,
                bucket => Bucket,
                host => Url
            },
            {_Uri, Path, Headers} = nkpacket_httpc_s3:get_object(Bucket, Path2, S3),
            case nkpacket_httpc:do_request(ConnPid, get, Path, Headers, <<>>, #{no_host_header=>true}) of
                {ok, 200, Hds, Body} ->
                    {ok, Body, #{s3_headers=>Hds}};
                {ok, 403, _, _} ->
                    {error, unauthorized};
                {ok, 404, _, _} ->
                    {error, file_not_found};
                {ok, Code, Hds, Body} ->
                    {error, {http_error, Code, Hds, Body}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
start(ProviderSpec, #{id:=PackageId}, SupPid, Service) ->
    #{id:=SrvId} = Service,
    PoolConfig = #{
        targets => maps:get(targets, ProviderSpec),
        debug => maps:get(debug, ProviderSpec, false),
        resolve_interval => maps:get(resolveInterval, ProviderSpec, 0)
    },
    PoolId = {nkfile_s3, SrvId, PackageId},
    Spec = #{
        id => PackageId,
        start => {nkpacket_httpc_pool, start_link, [PoolId, PoolConfig]}
    },
    case nkservice_packages_sup:update_child(SupPid, Spec, #{}) of
        {ok, ChildPid} ->
            nklib_proc:put(PoolId, undefined, ChildPid),
            ?LLOG(debug, "started ~s (~p)", [PackageId, ChildPid]),
            ok;
        not_updated ->
            ?LLOG(debug, "didn't upgrade ~s", [PackageId]),
            ok;
        {upgraded, ChildPid} ->
            nklib_proc:put(PoolId, undefined, ChildPid),
            ?LLOG(info, "upgraded ~s (~p)", [PackageId, ChildPid]),
            ok;
        {error, Error} ->
            ?LLOG(notice, "start/update error ~s: ~p", [PackageId, Error]),
            {error, Error}
    end.



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).