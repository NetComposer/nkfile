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

%% @doc NkFILE main library

-module(nkfile).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse_file_meta/2, parse_provider_spec/2]).
-export([upload/4, download/3, make_download_link/3, make_upload_link/3, delete/3]).
-export([get_file_meta/3]).

-include("nkfile.hrl").
-include_lib("nkserver/include/nkserver.hrl").
-export_type([provider_spec/0, storage_class/0, file_meta/0, file_body/0, op_meta/0]).

%% ===================================================================
%% Types
%% ===================================================================

%% @doc Provider specification
-type provider_spec() ::
    #{
        id := binary(),
        storage_class := storage_class(),
        max_size => pos_integer,
        encryption_algo => {atom, [aes_cfb128]},
        hash_algo => {atom, [sha256]},
        direct_download => boolean,
        direct_upload => boolean,
        direct_download_secs => pos_integer,
        direct_upload_secs => pos_integer,
        debug => boolean,
        atom() => term()
    }.


%% @doc A file body
-type file_body() :: binary().

-type storage_class() :: atom().


%% @doc A file metadata
-type file_meta() ::
    #{
        name := binary(),
        content_type := binary(),
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
-spec parse_file_meta(nkserver:id(), map()) ->
    {ok, file_meta()} | {error, term()}.

parse_file_meta(SrvId, Meta) ->
    Syntax = #{
        id => binary,
        name => binary,
        content_type => binary,
        hash => binary,
        password => binary,
        path => binary,
        size => pos_integer,
        '__mandatory' => [name, content_type]
    },
    case nklib_syntax:parse_all(Meta, Syntax) of
        {ok, Meta2} ->
            ?CALL_SRV(SrvId, nkfile_parse_file_meta, [SrvId, Meta2]);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Checks that an user-defined provider spec is valid
-spec parse_provider_spec(nkserver:id(), map()) ->
    {ok, provider_spec()} | {error, term()}.

parse_provider_spec(SrvId, Spec) ->
    Syntax = #{
        id => binary,
        storage_class => atom,
        module => module,
        max_size => pos_integer,
        encryption_algo => {atom, [aes_cfb128]},
        hash_algo => {atom, [sha256]},
        direct_download => boolean,
        direct_upload => boolean,
        direct_download_secs => pos_integer,
        direct_upload_secs => pos_integer,
        debug => boolean,
        '__mandatory' => [storage_class]
    },
    case nklib_syntax:parse_all(Spec, Syntax) of
        {ok, #{storage_class:=Class}=Spec2} ->
            ?CALL_SRV(SrvId, nkfile_parse_provider_spec, [SrvId, Class, Spec2]);
        {error, Error} ->
            {error, Error}
    end.


%% @doc Sends a file to the provider defined destination
%% Use parse_file_meta/2 and parse_provider_spec/2 before
-spec upload(nkserver:id(), provider_spec(),
             file_meta(), file_body()) ->
    {ok, file_meta(), op_meta()} | {error, term()}.

upload(SrvId, ProviderSpec, FileMeta, FileBody) ->
    #{storage_class:=Class} = ProviderSpec,
    case ?CALL_SRV(SrvId, nkfile_encode_body, [SrvId, Class, ProviderSpec, FileMeta, FileBody]) of
        {ok, FileMeta2, Bin, Meta1} ->
            case ?CALL_SRV(SrvId, nkfile_upload, [SrvId, Class, ProviderSpec, FileMeta2, Bin]) of
                {ok, Meta2} ->
                    {ok, FileMeta2, maps:merge(Meta1, Meta2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Sends a file to the provider defined destination
-spec download(nkserver:id(), provider_spec(), file_meta()) ->
    {ok, binary(), op_meta()} | {error, term()}.

download(SrvId, ProviderSpec, FileMeta) ->
    #{storage_class:=Class} = ProviderSpec,
    case ?CALL_SRV(SrvId, nkfile_download, [SrvId, Class, ProviderSpec, FileMeta]) of
        {ok, File1, Meta1} ->
            case ?CALL_SRV(SrvId, nkfile_decode_body, [SrvId, Class, ProviderSpec, FileMeta, File1]) of
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
-spec make_upload_link(nkserver:id(), provider_spec(), file_meta()) ->
    {ok, Verb::binary(), Url::binary(), TTL::integer()} | {error, term()}.

make_upload_link(SrvId, ProviderSpec, FileMeta) ->
    #{storage_class:=Class} = ProviderSpec,
    ?CALL_SRV(SrvId, nkfile_make_upload_link, [SrvId, Class, ProviderSpec, FileMeta]).


%% @doc For compatible storage's, generate an temporary download link.
%% It will fail if encryption or hash is used
-spec make_download_link(nkserver:id(), provider_spec(), file_meta()) ->
    {ok, Verb::binary(), Url::binary(), TTL::integer()} | {error, term()}.

make_download_link(SrvId, ProviderSpec, FileMeta) ->
    #{storage_class:=Class} = ProviderSpec,
    ?CALL_SRV(SrvId, nkfile_make_download_link, [SrvId, Class, ProviderSpec, FileMeta]).


%% @doc For compatible storage's, gets as much metadata as possible
-spec get_file_meta(nkserver:id(), provider_spec(), binary()) ->
    {ok, file_meta()}.

get_file_meta(SrvId, ProviderSpec, FileId) ->
    #{storage_class:=Class} = ProviderSpec,
    ?CALL_SRV(SrvId, nkfile_get_file_meta, [SrvId, Class, ProviderSpec, FileId]).


%% @doc Deletes a file
-spec delete(nkserver:id(), provider_spec(), file_meta()) ->
    ok | {error, term()}.

delete(SrvId, ProviderSpec, FileMeta) ->
    #{storage_class:=Class} = ProviderSpec,
    ?CALL_SRV(SrvId, nkfile_delete, [SrvId, Class, ProviderSpec, FileMeta]).


