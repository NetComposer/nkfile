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

-export([config/0, parse_spec/1, upload/5, download/4]).
-export_type([result_meta/0]).

-include("nkfile.hrl").
-define(LLOG(Type, Txt, Args),lager:Type("NkFILE S3 "++Txt, Args)).



%% ===================================================================
%% Types
%% ===================================================================

%% @doc returned metadata
-type result_meta() ::
    #{
        s3_headers => map()
    }.



%% ===================================================================
%% Provider callbacks
%% ===================================================================


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
        url => binary,
        connect_opts => any,        % Supports httpc_pool
        s3Key => binary,
        s3Secret => binary,
        bucket => binary,
        '__mandatory' => [url, s3Key, s3Secret, bucket],
        '__allow_unknown' =>true
    }}.



%% @private
upload(_SrvId, _PackageId, ProviderSpec, FileMeta, FileBin) ->
    #{name:=Name} = FileMeta,
    FilePath1 = maps:get(path, FileMeta, <<"/">>),
    FilePath2 = filename:join(FilePath1, to_bin(Name)),
    #{s3Key:=Key, s3Secret:=Secret, bucket:=Bucket} = ProviderSpec,
    case get_connection(ProviderSpec) of
        {ok, Url, ConnPid, ConnOpts} ->
            S3 = #{
                key => Key,
                secret => Secret,
                bucket => Bucket,
                host => Url
            },
            Hash = crypto:hash(sha256, FileBin),
            {_Uri, Path, Headers} = nkpacket_httpc_s3:put_object(Bucket, FilePath2, Hash, S3),
            case
                nkpacket_httpc:do_request(ConnPid, put, Path, Headers, FileBin, ConnOpts)
            of
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
download(_SrvId, _PackageId, ProviderSpec, FileMeta) ->
    #{name:=Name} = FileMeta,
    Path1 = maps:get(path, FileMeta, <<"/">>),
    Path2 = filename:join(Path1, to_bin(Name)),
    #{s3Key:=Key, s3Secret:=Secret, bucket:=Bucket} = ProviderSpec,
    case get_connection(ProviderSpec) of
        {ok, Url, ConnPid, ConnOpts} ->
            S3 = #{
                key => Key,
                secret => Secret,
                bucket => Bucket,
                host => Url
            },
            {_Uri, Path, Headers} = nkpacket_httpc_s3:get_object(Bucket, Path2, S3),
            case
                nkpacket_httpc:do_request(ConnPid, get, Path, Headers, <<>>, ConnOpts)
            of
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



%% ===================================================================
%% Internal
%% ===================================================================


get_connection(ProviderSpec) ->
    Opts = maps:get(connection_opts, ProviderSpec, #{}),
    case Opts of
        #{httpc_pool:=PoolId} ->
            case nkpacket_pool:get_conn_pid(PoolId) of
                {ok, ConnPid, #{url:=Url}} ->
                    {ok, Url, ConnPid, Opts};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            #{url:=Url} = ProviderSpec,
            case nkpacket_httpc:do_connect(Url, Opts) of
                {ok, ConnPid} ->
                    {ok, Url, ConnPid, Opts};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, url_is_missing}
    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).