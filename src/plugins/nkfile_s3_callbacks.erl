%% -------------------------------------------------------------------
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

-module(nkfile_s3_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([nkfile_upload/5, nkfile_download/4]).

-include("nkfile.hrl").


%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.





%% ===================================================================
%% Mail callbacks
%% ===================================================================

%% @private
nkfile_upload(SrvId, PackageId, s3, Bin, #{name:=Name}=Meta) ->
    Path1 = maps:get(path, Meta, <<"/">>),
    Path2 = filename:join(Path1, to_bin(Name)),
    case nkpacket_pool:get_conn_pid({nkfile_s3, SrvId, PackageId}) of
        {ok, ConnPid, #{url:=Url}} ->
            S3 = nkservice_util:get_cache(SrvId, {nkfile_s3, PackageId, config}),
            S3Config1 = maps:with([key_id, key], S3),
            S3Config2 = S3Config1#{host=>Url},
            #{bucket:=Bucket} = S3,
            Hash = crypto:hash(sha256, Bin),
            {_Uri, Path, Headers} = nkpacket_httpc_s3:put_object(Bucket, Path2, Hash, S3Config2),
            case nkpacket_httpc:do_request(ConnPid, put, Path, Headers, Bin, #{no_host_header=>true}) of
                {ok, 200, Hds, _} ->
                    {ok, Meta#{s3_headers=>Hds}};
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
    end;

nkfile_upload(_SrvId, _PackageId, s3, _Bin, _Meta) ->
    {error, missing_file_name};

nkfile_upload(_SrvId, _PackageId, _StorageClass, _Bin, _Meta) ->
    continue.



%% @private
nkfile_download(SrvId, PackageId, s3, #{name:=Name}=Meta) ->
    Path1 = maps:get(path, Meta, <<"/">>),
    Path2 = filename:join(Path1, to_bin(Name)),
    case nkpacket_pool:get_conn_pid({nkfile_s3, SrvId, PackageId}) of
        {ok, ConnPid, #{url:=Url}} ->
            S3 = nkservice_util:get_cache(SrvId, {nkfile_s3, PackageId, config}),
            S3Config1 = maps:with([key_id, key], S3),
            S3Config2 = S3Config1#{host=>Url},
            #{bucket:=Bucket} = S3,
            {_Uri, Path, Headers} = nkpacket_httpc_s3:get_object(Bucket, Path2, S3Config2),
            case nkpacket_httpc:do_request(ConnPid, get, Path, Headers, <<>>, #{no_host_header=>true}) of
                {ok, 200, Hds, Body} ->
                    {ok, Body, Meta#{s3_headers=>Hds}};
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
    end;

nkfile_download(_SrvId, _PackageId, s3, _Meta) ->
    {error, missing_file_name};

nkfile_download(_SrvId, _PackageId, _StorageClass, _Meta) ->
    continue.


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).

