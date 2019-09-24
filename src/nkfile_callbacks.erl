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

%% @doc NkFILE callbacks

-module(nkfile_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([status/1]).
-export([nkfile_parse_file_meta/2, nkfile_parse_provider_spec/3]).
-export([nkfile_encode_body/5, nkfile_decode_body/5, nkfile_upload/5, nkfile_download/4]).
-export([nkfile_make_upload_link/4, nkfile_make_download_link/4]).
-export([nkfile_get_file_meta/4, nkfile_delete/4]).


-include("nkfile.hrl").
-include_lib("nkserver/include/nkserver.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.
-type class() :: nkfile:storage_class().


%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
status(file_decryption_error)            -> "File decryption error";
status(file_encryption_error)            -> "File encryption error";
status({file_read_error, F})             -> {"File read error: '~s'", [F]};
status({file_too_large, F})              -> {"File is too large: '~s'", [F]};
status({file_write_error, F})            -> {"File write error: '~s'", [F]};
status({storage_class_incompatible, C})  -> {"Storage class incompatible: '~s'", [C]};
status({storage_class_invalid, C})       -> {"Storage class invalid: '~s'", [C]};
status({storage_class_unknown, C})       -> {"Storage class unknown: '~s'", [C]};
status({encryption_algo_unknown, Algo})  -> {"Unknown encryption algorithm: '~s'", [Algo]};
status(_)                                -> continue.


%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc Check for valid nkfile:file_meta()
-spec nkfile_parse_file_meta(nkserver:id(), map()) ->
    {ok, nkfile:file_meta()} | {error, term()} | continue().

nkfile_parse_file_meta(_SrvId, Meta) ->
    {ok, Meta}.


%% @doc Checks that an user-defined provider spec is valid
-spec nkfile_parse_provider_spec(nkserver:id(), class(), map()) ->
    {ok, nkfile:provider_spec()} | {error, term()}.

nkfile_parse_provider_spec(_SrvId, _Class, Spec) ->
    {ok, Spec}.


%% @doc Perform processing and encoding
%% - size calculation
%% - hash calculation
%% - encryption
-spec nkfile_encode_body(nkserver:id(), class(), nkfile:provider_spec(),
                         nkfile:file_meta(), nkfile:file_body()) ->
    {ok, nkfile:file_meta(), binary(), nkfile:op_meta()} | {error, term()} | continue().

nkfile_encode_body(_SrvId, _Class, ProviderSpec, FileMeta, File) ->
    nkfile_util:encode_body(ProviderSpec, FileMeta, File).


%% @doc Perform processing and decoding
%% - hash check
%% - un-encryption
-spec nkfile_decode_body(nkserver:id(), class(), nkfile:provider_spec(),
    nkfile:file_meta(), binary()) ->
    {ok, binary(), map()} | {error, term()} | continue().

nkfile_decode_body(_SrvId, _Class, ProviderSpec, FileMeta, File) ->
    nkfile_util:decode_body(ProviderSpec, FileMeta, File).


%% @doc Perform real upload
-spec nkfile_upload(nkserver:id(), class(), nkfile:provider_spec(),
    nkfile:file_meta(), binary()) ->
    {ok, map()} | {error, term()} | continue().

nkfile_upload(_SrvId, Class, _ProviderSpec, _FileMeta, _Bin) ->
    {error, {storage_class_unknown, Class}}.


%% @doc Perform real download
-spec nkfile_download(nkserver:id(), class(), nkfile:provider_spec(),
    nkfile:file_meta()) ->
    {ok, binary(), map()} | {error, term()} | continue().

nkfile_download(_SrvId, Class, _ProviderSpec, _FileMeta) ->
    {error, {storage_class_unknown, Class}}.


%% @doc For compatible storage's, generates a temporary upload link
-spec nkfile_make_upload_link(nkserver:id(), class(), nkfile:provider_spec(),
    nkfile:file_meta()) ->
    {ok, Verb::binary(), Url::binary(), TTL::integer()} | {error, term()} | continue().

nkfile_make_upload_link(_SrvId, Class, _ProviderSpec, _FileMeta) ->
    {error, {storage_class_incompatible, Class}}.


%% @doc For compatible storage's, generates a temporary download link
-spec nkfile_make_download_link(nkserver:id(), class(), nkfile:provider_spec(),
    nkfile:file_meta()) ->
    {ok, Verb::binary(), Url::binary(), TTL::integer()} | {error, term()} | continue().

nkfile_make_download_link(_SrvId, Class, _ProviderSpec, _FileMeta) ->
    {error, {storage_class_incompatible, Class}}.


%% @doc
-spec nkfile_get_file_meta(nkserver:id(), class(), nkfile:provider_spec(),
    binary()) ->
    {ok, nkfile:file_meta()} | {error, term()} | continue().

nkfile_get_file_meta(_SrvId, Class, _ProviderSpec, _FileMeta) ->
    {error, {storage_class_invalid, Class}}.


%% @doc Deletes a file
-spec nkfile_delete(nkserver:id(), class(),
                    nkfile:provider_spec(), nkfile:file_meta()) ->
    ok | {error, term()} | continue.

nkfile_delete(_SrvId, Class, _ProviderSpec, _FileMeta) ->
    {error, {storage_class_invalid, Class}}.
