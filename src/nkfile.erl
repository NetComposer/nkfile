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

%% @doc NkFILE main library

-module(nkfile).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse_file_meta/1, parse_provider_spec/3]).
-export([upload/5, download/4]).
-export([luerl_upload/3, luerl_download/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================


%% @doc A file body
-type file_body() :: binary().


%% @see A file metadata
-type file_meta() ::
    #{
        id => binary(),
        name => binary(),
        contentType => binary(),
        path => binary(),
        sha256 => binary(),
        password => binary()
    }.

%% @see result_meta/0 types in providers
-type result_meta() ::
    #{
        password => binary()

    }.



%% ===================================================================
%% Public
%% ===================================================================



%% @doc Check for valid nkfile:file_meta()
-spec parse_file_meta(map()) ->
    {ok, file_meta()} | {error, term()}.

parse_file_meta(Meta) ->
    nkfile_util:parse_file_meta(Meta).


%% @doc Checks that an user-defined provider spec is valid
-spec parse_provider_spec(nkservice:id(), nkservice:package_id(), map()) ->
    {ok, nkfile_provider:spec()} | {error, term()}.

parse_provider_spec(SrvId, PackageId, #{storageClass:=Class}=Meta) ->
    case catch nkfile_plugin:get_class_module(SrvId, PackageId, Class) of
        {'EXIT', _} ->
            {error, {storage_class_unknown, Class}};
        Module ->
            nkfile_provider:parse_spec(Module, Meta)
    end.



%% @doc Sends a file to the provider defined destination
%% If the provider is defined in the package, use the second version
%% If you have a provider defined elsewhere, use the first option, but use
%% nkfile_provider:parse_spec/1
%% Use parse_meta/1 to check file's meta
-spec upload(nkservice:id(), nkservice:package_id(), nkfile_provider:spec(), file_meta(), file_body()) ->
    {ok, file_meta(), result_meta()} | {error, term()}.

upload(SrvId, PackageId, ProviderSpec, FileMeta, FileBody) when is_map(ProviderSpec) ->
    case nkfile_provider:encode_body(SrvId, PackageId, ProviderSpec, FileMeta, FileBody) of
        {ok, FileMeta2, Bin, Meta1} ->
            case nkfile_provider:upload(SrvId, PackageId, ProviderSpec, FileMeta2, Bin) of
                {ok, Meta2} ->
                    {ok, FileMeta2, maps:merge(Meta1, Meta2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

upload(SrvId, PackageId, ProviderId, FileMeta, FileBody) ->
    case nkfile_provider:get_spec(SrvId, PackageId, ProviderId) of
        undefined ->
            {error, {provider_unknown, ProviderId}};
        ProviderSpec when is_map(ProviderSpec) ->
            upload(SrvId, PackageId, ProviderSpec, FileMeta, FileBody)
    end.



%% @doc Sends a file to the provider defined destination
%% If the provider is defined in the package, use the second version
%% If you have a provider defined elsewhere, use the first option, but use
%% nkfile_provider:parse_spec/1
%% Use parse_meta/1 to check file's meta
-spec download(nkservice:id(), nkservice:package_id(), nkfile_provider:spec(), file_meta()) ->
    {ok, binary(), result_meta()} | {error, term()}.

download(SrvId, PackageId, ProviderSpec, FileMeta) when is_map(ProviderSpec) ->
    case nkfile_provider:download(SrvId, PackageId, ProviderSpec, FileMeta) of
        {ok, File1, Meta1} ->
            case nkfile_provider:decode_body(SrvId, PackageId, ProviderSpec, FileMeta, File1) of
                {ok, File2, Meta2} ->
                    {ok, File2, maps:merge(Meta1, Meta2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

download(SrvId, PackageId, ProviderId, FileMeta) ->
    case nkfile_provider:get_spec(SrvId, PackageId, ProviderId) of
        undefined ->
            {error, {provider_unknown, ProviderId}};
        ProviderSpec when is_map(ProviderSpec) ->
            download(SrvId, PackageId, ProviderSpec, FileMeta)
    end.


%% ===================================================================
%% Luerl
%% ===================================================================

%% @private
luerl_upload(SrvId, PackageId, [ProviderId, Meta, Body]) ->
    lager:error("NKLOG MM ~p", [Meta]),
    case upload(SrvId, PackageId, ProviderId, Meta, Body) of
        {ok, Meta2} ->
            [<<"ok">>, Meta2];
        {error, Error} ->
            {error, Error}
    end.


%% @private
luerl_download(SrvId, PackageId, [ProviderId, Meta]) ->
    case download(SrvId, PackageId, ProviderId, Meta) of
        {ok, Body, Meta2} ->
            [Body, Meta2];
        {error, Error} ->
            {error, Error}
    end.

