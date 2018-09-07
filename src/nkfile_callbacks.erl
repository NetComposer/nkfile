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

-module(nkfile_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([msg/1]).
-export([nkfile_parse_file_meta/3, nkfile_parse_provider_spec/3]).
-export([nkfile_encode_body/5, nkfile_decode_body/5, nkfile_upload/5, nkfile_download/4]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
msg(base64_decode_error)              -> "BASE64 decode error";
msg(decryption_error)                 -> "Decryption error";
msg(encryption_error)                 -> "Encryption error";
msg(file_read_error)                  -> "File read error";
msg(file_write_error)                 -> "File write error";
msg(invalid_file_body)                -> "Invalid file body";
msg(invalid_store)                    -> "Invalid store";
msg({store_not_found, Id})            -> {"Store not found: ~p", [Id]};
msg({encryption_algo_unknown, Algo})  -> {"Unknown encryption algorithm: '~s'", [Algo]};
msg(_)                                -> continue.


%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc Check for valid nkfile:file_meta()
-spec nkfile_parse_file_meta(nkservice:id(), nkservice:package_id(), map()) ->
    {ok, nkfile:file_meta()} | {error, term()} | continue().

nkfile_parse_file_meta(_SrvId, _PackageId, Meta) ->
    {ok, Meta}.


%% @doc Checks that an user-defined provider spec is valid
-spec nkfile_parse_provider_spec(nkservice:id(), nkservice:package_id(), map()) ->
    {ok, nkfile:provider_spec()} | {error, term()}.

nkfile_parse_provider_spec(_SrvId, _PackageId, Spec) ->
    {ok, Spec}.


%% @doc Perform processing and encoding
%% - size calculation
%% - hash calculation
%% - encryption
-spec nkfile_encode_body(nkservice:id(), nkservice:package_id(), nkfile:provider_spec(),
                         nkfile:file_meta(), nkfile:file_body()) ->
    {ok, nkfile:file_meta(), binary(), nkfile:op_meta()} | {error, term()} | continue().

nkfile_encode_body(_SrvId, _PackageId, ProviderSpec, FileMeta, File) ->
    nkfile_util:encode_body(ProviderSpec, FileMeta, File).


%% @doc Perform processing and decoding
%% - hash check
%% - un-encryption
-spec nkfile_decode_body(nkservice:id(), nkservice:package_id(), nkfile:provider_spec(),
    nkfile:file_meta(), binary()) ->
    {ok, binary(), map()} | {error, term()} | continue().

nkfile_decode_body(_SrvId, _PackageId, ProviderSpec, FileMeta, File) ->
    nkfile_util:decode_body(ProviderSpec, FileMeta, File).



%% @doc Perform real upload
-spec nkfile_upload(nkservice:id(), nkservice:package_id(), nkfile:provider_spec(),
    nkfile:file_meta(), binary()) ->
    {ok, map()} | {error, term()} | continue().

nkfile_upload(_SrvId, _PackageId, _ProviderSpec, _FileMeta, _Bin) ->
    {error, storage_class_unknown}.


%% @doc Perform real download
-spec nkfile_download(nkservice:id(), nkservice:package_id(), nkfile:provider_spec(),
    nkfile:file_meta()) ->
    {ok, binary(), map()} | {error, term()} | continue().

nkfile_download(_SrvId, _PackageId, _ProviderSpec, _FileMeta) ->
    {error, storage_class_unknown}.

