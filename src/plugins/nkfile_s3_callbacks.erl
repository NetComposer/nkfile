%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

-export([nkfile_parse_provider_spec/3, nkfile_upload/5, nkfile_download/4, nkfile_delete/4]).
-export([nkfile_make_upload_link/4, nkfile_make_download_link/4]).
-export([nkfile_get_file_meta/4]).

-include("nkfile.hrl").

-define(CLASS, nkfile_s3).

%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
nkfile_parse_provider_spec(SrvId, ?CLASS, Spec) ->
    Syntax = #{
        s3_config => #{
            region => binary,
            key => binary,
            secret => binary,
            bucket => binary,
            path => binary,
            url => binary,                        %% Use url or scheme/host/port
            scheme => {atom, [http, https]},
            host => binary,
            port => integer,
            hackney_pool => binary,
            '__mandatory' => [key, secret, bucket]
        },
        '__mandatory' => [s3_config]
    },
    case nklib_syntax:parse_all(Spec, Syntax) of
        {ok, Parsed} ->
            {continue, [SrvId, ?CLASS, Parsed]};
        {error, Error} ->
            {error, Error}
    end;

nkfile_parse_provider_spec(_SrvId, _Class, _Spec) ->
    continue.


%% @doc
nkfile_upload(_SrvId, ?CLASS, ProviderSpec, FileMeta, Bin) ->
    {Bucket, Path, Config} = get_config(ProviderSpec, FileMeta),
    CT = maps:get(content_type, FileMeta),
    Hash = crypto:hash(sha256, Bin),
    {Method, Url, Hds} = nkaws_s3:put_object(Bucket, Path, CT, Hash, Config),
    case request(Method, Url, Hds, Bin, ProviderSpec) of
        {ok, _Body, Meta} ->
            {ok, Meta};
        {error, Error} ->
            {error, Error}
    end;

nkfile_upload(_SrvId, _Class, _ProviderSpec, _FileMeta, _Bin) ->
    continue.


%% @doc
nkfile_download(_SrvId, ?CLASS, ProviderSpec, FileMeta) ->
    {Bucket, Path, Config} = get_config(ProviderSpec, FileMeta),
    {Method, Url, Hds} = nkaws_s3:get_object(Bucket, Path, Config),
    request(Method, Url, Hds, <<>>, ProviderSpec);

nkfile_download(_SrvId, _Class, _ProviderSpec, _FileMeta) ->
    continue.


%% @doc
nkfile_make_upload_link(_SrvId, ?CLASS, ProviderSpec, FileMeta) ->
    case
        (maps:get(direct_upload, ProviderSpec, ?FILE_DIRECT_UPLOAD) /= true) orelse
        maps:is_key(encryption_algo, ProviderSpec) orelse
        maps:is_key(hash_algo, ProviderSpec)
    of
        true ->
            {error, {storage_class_incompatible, ?CLASS}};
        false ->
            {Bucket, Path, Config} = get_config(ProviderSpec, FileMeta),
            CT = maps:get(content_type, FileMeta),
            TTL = maps:get(direct_upload_secs, ProviderSpec, ?FILE_DIRECT_UPLOAD_SECS),
            {Verb, Uri} = nkaws_s3:make_put_url(Bucket, Path, CT, TTL, Config),
            {ok, Verb, Uri, TTL}
    end;

nkfile_make_upload_link(_SrvId, _Class, _ProviderSpec, _FileMeta) ->
    continue.


%% @doc
nkfile_make_download_link(_SrvId, ?CLASS, ProviderSpec, FileMeta) ->
    case
        (maps:get(direct_download, ProviderSpec, ?FILE_DIRECT_DOWNLOAD) /= true) orelse
        maps:is_key(encryption_algo, ProviderSpec) orelse
        maps:is_key(hash_algo, ProviderSpec)
    of
        true ->
            {error, {storage_class_incompatible, ?CLASS}};
        false ->
            {Bucket, Path, Config} = get_config(ProviderSpec, FileMeta),
            TTL = maps:get(direct_download_secs, ProviderSpec, ?FILE_DIRECT_DOWNLOAD_SECS),
            {Verb, Uri} = nkaws_s3:make_get_url(Bucket, Path, TTL, Config),
            {ok, Verb, Uri, TTL}
    end;

nkfile_make_download_link(_SrvId, _Class, _ProviderSpec, _FileMeta) ->
    continue.


%% @doc
nkfile_get_file_meta(_SrvId, ?CLASS, ProviderSpec, Id) ->
    {Bucket, Path, Config} = get_config(ProviderSpec, #{name=>Id}),
    {<<"HEAD">>, Url, Hds} = nkaws_s3:get_meta(Bucket, Path, Config),
    case request(<<"HEAD">>, Url, Hds, <<>>, ProviderSpec) of
        {ok, _, #{s3_headers:=Headers}} ->
            Headers2 = [{nklib_util:to_lower(Key), Val} || {Key, Val} <- Headers],
            ContentType = nklib_util:get_value(<<"content-type">>, Headers2, <<>>),
            Size = binary_to_integer(nklib_util:get_value(<<"content-length">>, Headers2, <<>>)),
            case ProviderSpec of
                #{max_size:=MaxSize} when Size > MaxSize ->
                    {error, {file_too_large, Url}};
                _ ->
                    {ok, #{content_type=>ContentType, size=>Size}}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
nkfile_delete(_SrvId, _Class, #{storage_class:=?CLASS}=ProviderSpec, FileMeta) ->
    {Bucket, Path, Config} = get_config(ProviderSpec, FileMeta),
    {Method, Url, Hds} = nkaws_s3:delete(Bucket, Path, Config),
    case request(Method, Url, Hds, <<>>, ProviderSpec) of
        {ok, _, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end;

nkfile_delete(_SrvId, _Class, _ProviderSpec, _FileMeta) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================


%% @private
get_config(ProviderSpec , FileMeta) ->
    #{name:=Name} = FileMeta,
    #{s3_config:=#{bucket:=Bucket}=Config} = ProviderSpec,
    Path1 = maps:get(path, Config, <<"/">>),
    Path2 = filename:join([<<"/">>, Path1, to_bin(Name)]),
    {Bucket, Path2, Config}.


%% @private
request(Method, Url, Hds, Bin, ProviderSpec) ->
    PoolId = maps:get(hackney_pool, ProviderSpec, default),
    case hackney:request(Method, Url, Hds, Bin, [{pool, PoolId}, with_body]) of
        {ok, 200, ReplyHds} ->
            {ok, <<>>, #{s3_headers=>ReplyHds}};
        {ok, 200, ReplyHds, Body} ->
            {ok, Body, #{s3_headers=>ReplyHds}};
        {ok, 204, ReplyHds} ->
            {ok,<<>>, #{s3_headers=>ReplyHds}};
        {ok, 204, ReplyHds, _} ->
            {ok,<<>>, #{s3_headers=>ReplyHds}};
        {ok, 403, _, _} ->
            {error, unauthorized};
        {ok, 403, _} ->
            {error, unauthorized};
        {ok, 404, _, _} ->
            {error, file_not_found};
        {ok, 404, _} ->
            {error, file_not_found};
        {ok, Code, Hds, Body} ->
            {error, {http_error, Code, Hds, Body}};
        {ok, Code, Hds} ->
            {error, {http_error, Code, Hds, <<>>}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).