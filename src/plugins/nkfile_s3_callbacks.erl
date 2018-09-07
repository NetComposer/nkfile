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

%% @doc NkFILE S3 callbacks

-module(nkfile_s3_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([nkfile_parse_provider_spec/3, nkfile_upload/5, nkfile_download/4]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(CLASS, <<"s3">>).


%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
nkfile_parse_provider_spec(SrvId, PackageId, #{storageClass:=?CLASS}=Spec) ->
    Syntax = #{
        s3Config => #{
            region => binary,
            key => binary,
            secret => binary,
            bucket => binary,
            path => binary,
            scheme => {atom, [http, https]},
            host => binary,
            port => pos_integer,
            hackney_pool => binary,
            '__mandatory' => [key, secret, bucket]
        },
        '__mandatory' => [s3Config],
        '__allow_unknown' => true
    },
    case nklib_syntax:parse(Spec, Syntax) of
        {ok, Parsed, _} ->
            {continue, [SrvId, PackageId, Parsed]};
        {error, Error} ->
            {error, Error}
    end;

nkfile_parse_provider_spec(_SrvId, _PackageId, _Spec) ->
    continue.


%% @doc
nkfile_upload(_SrvId, _PackageId, #{storageClass:=?CLASS}=ProviderSpec, FileMeta, Bin) ->
    #{name:=Name, contentType:=CT} = FileMeta,
    #{s3Config:=#{bucket:=Bucket}=Config} = ProviderSpec,
    Path1 = maps:get(path, FileMeta, <<"/">>),
    Path2 = filename:join(Path1, to_bin(Name)),
    Hash = crypto:hash(sha256, Bin),
    {Method, Url, Hds} = nkpacket_httpc_s3:put_object(Bucket, Path2, CT, Hash, Config),
    case request(Method, Url, Hds, Bin, ProviderSpec) of
        {ok, _Body, Meta} ->
            {ok, Meta};
        {error, Error} ->
            {error, Error}
    end;

nkfile_upload(_SrvId, _PackageId, _ProviderSpec, _FileMeta, _Bin) ->
    continue.


%% @doc
nkfile_download(_SrvId, _PackageId, #{storageClass:=?CLASS}=ProviderSpec, FileMeta) ->
    #{name:=Name} = FileMeta,
    #{s3Config:=#{bucket:=Bucket}=Config} = ProviderSpec,
    Path1 = maps:get(path, FileMeta, <<"/">>),
    Path2 = filename:join(Path1, to_bin(Name)),
    {Method, Url, Hds} = nkpacket_httpc_s3:get_object(Bucket, Path2, Config),
    request(Method, Url, Hds, <<>>, ProviderSpec);

nkfile_download(_SrvId, _PackageId, _ProviderSpec, _FileMeta) ->
    continue.



%% ===================================================================
%% Internal
%% ===================================================================


request(Method, Url, Hds, Bin, ProviderSpec) ->
    PoolId = maps:get(hackney_pool, ProviderSpec, default),
    case hackney:request(Method, Url, Hds, Bin, [{pool, PoolId}, with_body]) of
        {ok, 200, ReplyHds, Body} ->
            {ok, Body, #{s3_headers=>ReplyHds}};
        {ok, 403, _, _} ->
            {error, unauthorized};
        {ok, 404, _, _} ->
            {error, file_not_found};
        {ok, Code, Hds, Body} ->
            {error, {http_error, Code, Hds, Body}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).