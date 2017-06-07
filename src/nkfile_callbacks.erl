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

-export([plugin_deps/0, plugin_syntax/0, service_init/2]).
-export([error/1]).
-export([nkfile_get_store/2, nkfile_parse_store/2, nkfile_get_body/3]).
-export([nkfile_upload/4, nkfile_download/3]).
-export([service_api_cmd/2, service_api_syntax/2]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkapi/include/nkapi.hrl").



%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Plugin callbacks
%%
%% These are used when NkFILE is started as a NkSERVICE plugin
%% ===================================================================


plugin_deps() ->
    [].


plugin_syntax() ->
	#{
	}.



service_init(_Service, #{id:=SrvId}=State) ->
    % Loads app stores
    lists:foreach(
        fun
            (#{id:=Id}=Data) ->
                case nkfile:parse_store(SrvId, Data) of
                    {ok, Store} ->
                        lager:info("NkFILE: loading store ~s", [Id]),
                        nkfile_app:put_store(nklib_util:to_binary(Id), Store);
                    {error, Error} ->
                        lager:warning("NkFILE: could not load store ~p: ~p", [Data, Error])
                end;
            (Data) ->
                lager:warning("NkFILE: invalid store: ~p", [Data])
        end,
        nkfile_app:get(stores, [])),
    {ok, State}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(base64_decode_error)              -> "BASE64 decode error";
error(decryption_error)                 -> "Decryption error";
error(encryption_error)                 -> "Encryption error";
error({file_read_error, Path, Error})   -> {"File read error at ~s: ~p", [Path, Error]};
error({file_write_error, Path, Error})  -> {"File write error at ~s: ~p", [Path, Error]};
error(invalid_file_body)                -> "Invalid file body";
error(invalid_store)                    -> "Invalid store";
error({store_not_found, Id})            -> {"Store not found: ~p", [Id]};
error(_)                                -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================



%% @doc Gets a store
-spec nkfile_get_store(nkservice:id(), nkfile:store_id()) ->
    {ok, map()} | {error, term()}.

nkfile_get_store(_SrvId, Id) ->
    case nkfile_app:get_store(Id) of
        not_found ->
            {error, {store_not_found, Id}};
        {ok, Store} ->
            {ok, Store}
    end.


%% @doc Parses a store
-spec nkfile_parse_store(map(), nklib_syntax:parse_opts()) ->
    {ok, map(), [binary()]} | {error, term()}.

nkfile_parse_store(_Store, _Opts) ->
    {error, invalid_store}.


%% @doc Extracts the file body
-spec nkfile_get_body(nkservice:id(), nkfile:store(), nkfile:file_body()) ->
    {ok, binary()} | {error, term()}.

nkfile_get_body(_SrvId, _Store, {base64, Base64}) ->
    case catch base64:decode(Base64) of
        {'EXIT', _} ->
            {error, base64_decode_error};
        Bin ->
            {ok, Bin}
    end;

nkfile_get_body(_SrvId, _Store, Bin) when is_binary(Bin) ->
    {ok, Bin};

nkfile_get_body(_SrvId, _Store, _FileBody) ->
    {error, invalid_file_body}.


%% @doc Stores the file
-spec nkfile_upload(nkservice:id(), nkfile:store(), nkfile:file(), binary()) ->
    {ok, Meta::map()} | {error, term()}.

nkfile_upload(_SrvId, _Store, _File, _Body) ->
    {error, invalid_store}.


%% @doc Retrieves the file
-spec nkfile_download(nkservice:id(), nkfile:store(), nkfile:file()) ->
    {ok, binary()} | {error, term()}.

nkfile_download(_SrvId, _Store, _File) ->
    {error, invalid_store}.


%% ===================================================================
%% API Server
%% ===================================================================

%% @doc
service_api_syntax(Syntax, #nkreq{cmd = <<"file", Cmd/binary>>}=Req) ->
    {nkfile_api_syntax:syntax(Cmd, Syntax), Req};

service_api_syntax(_Syntax, _Req) ->
    continue.


%% @doc
service_api_cmd(#nkreq{cmd = <<"file", Cmd/binary>>}=Req, State) ->
    nkfile_api:cmd(Cmd, Req, State);

service_api_cmd(_Req, _State) ->
    continue.

