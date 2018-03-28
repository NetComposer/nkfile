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

-export([error/1]).
-export([nkfile_upload/4, nkfile_download/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").
% -include_lib("nkapi/include/nkapi.hrl").



%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%%service_init(_Service, #{id:=SrvId}=State) ->
%%    % Loads app stores
%%    lists:foreach(
%%        fun
%%            (#{id:=Id}=Data) ->
%%                case nkfile:parse_store(SrvId, Data) of
%%                    {ok, Store, _} ->
%%                        lager:info("NkFILE: loading store ~s", [Id]),
%%                        nkfile_app:put_store(nklib_util:to_binary(Id), Store);
%%                    {error, Error} ->
%%                        lager:warning("NkFILE: could not load store ~p: ~p", [Data, Error])
%%                end;
%%            (Data) ->
%%                lager:warning("NkFILE: invalid store: ~p", [Data])
%%        end,
%%        nkfile_app:get(stores, [])),
%%    {ok, State}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(base64_decode_error)              -> "BASE64 decode error";
error(decryption_error)                 -> "Decryption error";
error(encryption_error)                 -> "Encryption error";
error(file_read_error)                  -> "File read error";
error(file_write_error)                 -> "File write error";
error(invalid_file_body)                -> "Invalid file body";
error(invalid_store)                    -> "Invalid store";
error({store_not_found, Id})            -> {"Store not found: ~p", [Id]};
error({unknown_encryption_algo, Algo})  -> {"Unknown encryption algorithm: '~s'", [Algo]};
error(_)                                -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc Stores the file
-spec nkfile_upload(nkservice:id(), nkservice:package_id(), binary(), nkfile:meta()) ->
    {ok, nkfile:file()} | {error, term()}.

nkfile_upload(_SrvId, _PackageId, _Body, _Meta) ->
    {error, invalid_store}.


%% @doc Retrieves the file
-spec nkfile_download(nkservice:id(), nkservice:package_id(), nkfile:meta()) ->
    {ok, nkfile:file(), binary()} | {error, term()}.

nkfile_download(_SrvId, _PackageId, _Meta) ->
    {error, invalid_store}.

