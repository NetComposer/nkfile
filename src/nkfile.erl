%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Carlos Gonzalez Florido.  All Rights Reserved.
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

-export([parse_file_meta/3, parse_provider_spec/3]).
-export([upload/5, download/4, make_download_link/4, make_upload_link/4, delete/4]).
-export([check_file_meta/4]).
-export([luerl_upload/3, luerl_download/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================

%% @doc Provider specification
-type provider_spec() ::
    #{
        id := binary(),
        storageClass := binary(),
        maxSize => pos_integer,
        encryptionAlgo => {atom, [aes_cfb128]},
        hashAlgo => {atom, [sha256]},
        directDownload => boolean,
        directUpload => boolean,
        directDownloadSecs => pos_integer,
        directUploadSecs => pos_integer,
        debug => boolean,
        atom() => term()
    }.


%% @doc A file body
-type file_body() :: binary().


%% @doc A file metadata
-type file_meta() ::
    #{
        name := binary(),
        contentType := binary(),
        id => binary(),
        size => integer(),
        hash => binary(),           % Base64
        password => binary()        % Base64
    }.




-type op_meta() ::
    #{
        crypt_usecs => integer(),
        s3_headers => list(),
        file_path => binary()
    }.



%% ===================================================================
%% Public
%% ===================================================================



%% @doc Check for valid file_meta()
-spec parse_file_meta(nkservice:id(), nkservice:package_id(), map()) ->
    {ok, file_meta()} | {error, term()}.

parse_file_meta(SrvId, PackageId, Meta) ->
    case nkfile_util:parse_file_meta(Meta) of
        {ok, Parsed} ->
            ?CALL_SRV(SrvId, nkfile_parse_file_meta, [SrvId, PackageId, Parsed]);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Checks that an user-defined provider spec is valid
-spec parse_provider_spec(nkservice:id(), nkservice:package_id(), map()) ->
    {ok, provider_spec()} | {error, term()}.

parse_provider_spec(SrvId, PackageId, Spec) ->
    case nkfile_util:parse_provider_spec(Spec) of
        {ok, Parsed} ->
            ?CALL_SRV(SrvId, nkfile_parse_provider_spec, [SrvId, PackageId, Parsed]);
        {error, Error} ->
            {error, Error}
    end.



%% @doc Sends a file to the provider defined destination
%% Use parse_file_meta/3 and parse_provider_spec/3
-spec upload(nkservice:id(), nkservice:package_id(), provider_spec(),
             file_meta(), file_body()) ->
    {ok, file_meta(), op_meta()} | {error, term()}.

upload(SrvId, PackageId, ProviderSpec, FileMeta, FileBody) ->
    case ?CALL_SRV(SrvId, nkfile_encode_body, [SrvId, PackageId, ProviderSpec, FileMeta, FileBody]) of
        {ok, FileMeta2, Bin, Meta1} ->
            case ?CALL_SRV(SrvId, nkfile_upload, [SrvId, PackageId, ProviderSpec, FileMeta2, Bin]) of
                {ok, Meta2} ->
                    {ok, FileMeta2, maps:merge(Meta1, Meta2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Sends a file to the provider defined destination
%% If the provider is defined in the package, use the second version
%% If you have a provider defined elsewhere, use the first option, but use
%% nkfile_provider:parse_spec/1
%% Use parse_meta/1 to check file's meta
-spec download(nkservice:id(), nkservice:package_id(), provider_spec(), file_meta()) ->
    {ok, binary(), op_meta()} | {error, term()}.

download(SrvId, PackageId, ProviderSpec, FileMeta) ->
    case ?CALL_SRV(SrvId, nkfile_download, [SrvId, PackageId, ProviderSpec, FileMeta]) of
        {ok, File1, Meta1} ->
            case ?CALL_SRV(SrvId, nkfile_decode_body, [SrvId, PackageId, ProviderSpec, FileMeta, File1]) of
                {ok, File2, Meta2} ->
                    {ok, File2, maps:merge(Meta1, Meta2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc For compatible storage's, generate an temporary upload link.
%% It will fail if encryption or hash is used
-spec make_upload_link(nkservice:id(), nkservice:package_id(), provider_spec(), file_meta()) ->
    {ok, Verb::binary(), Url::binary(), TTL::integer()} | {error, term()}.

make_upload_link(SrvId, PackageId, ProviderSpec, FileMeta) ->
    ?CALL_SRV(SrvId, nkfile_make_upload_link, [SrvId, PackageId, ProviderSpec, FileMeta]).


%% @doc For compatible storage's, generate an temporary download link.
%% It will fail if encryption or hash is used
-spec make_download_link(nkservice:id(), nkservice:package_id(), provider_spec(), file_meta()) ->
    {ok, Verb::binary(), Url::binary(), TTL::integer()} | {error, term()}.

make_download_link(SrvId, PackageId, ProviderSpec, FileMeta) ->
    ?CALL_SRV(SrvId, nkfile_make_download_link, [SrvId, PackageId, ProviderSpec, FileMeta]).


%% @doc For compatible storage's, gets as much metadata as possible
-spec check_file_meta(nkservice:id(), nkservice:package_id(), provider_spec(), binary()) ->
    {ok, file_meta()}.

check_file_meta(SrvId, PackageId, ProviderSpec, FileId) ->
    ?CALL_SRV(SrvId, nkfile_check_file_meta, [SrvId, PackageId, ProviderSpec, FileId]).


%% @doc Deletes a file
-spec delete(nkservice:id(), nkservice:package_id(), provider_spec(), file_meta()) ->
    ok | {error, term()}.

delete(SrvId, PackageId, ProviderSpec, FileMeta) ->
    ?CALL_SRV(SrvId, nkfile_delete, [SrvId, PackageId, ProviderSpec, FileMeta]).


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

