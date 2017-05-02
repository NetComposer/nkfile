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

-export([plugin_deps/0, plugin_syntax/0, plugin_start/2, plugin_stop/2]).
-export([api_error/1]).
-export([nkfile_get_store/2, nkfile_parse_store/1]).
-export([api_server_cmd/2, api_server_syntax/3]).

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


plugin_start(Config, #{id:=_SrvId}) ->
	{ok, Config}.


plugin_stop(Config, #{id:=_SrvId}) ->
    {ok, Config}.


%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
api_error({store_not_found, Id})     -> {"Store not found: ~p", [Id]};
api_error(_)                            -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc Gets a store store
-spec nkfile_get_store(nkservice:id(), nkfile:store_id()) ->
    {ok, #nkfile_store{}} | {error, term()}.

nkfile_get_store(SrvId, Id) ->
    case nkfile_app:get_store(nklib_util:to_binary(Id)) of
        {ok, Map} ->
            SrvId:nkfile_parse_store(Map);
        not_found ->
            {error, {store_not_found, Id}}
    end.


%% @doc Parses a mail store
-spec nkfile_parse_store(map()) ->
    {ok, #nkfile_store{}} | {error, term()}.

nkfile_parse_store(_Provider) ->
    {error, invalid_store}.



%% ===================================================================
%% API Server
%% ===================================================================

%% @doc
api_server_syntax(Syntax, #nkapi_req{class=file, subclass=Sub, cmd=Cmd}=Req, State) ->
    {nkfile_api_syntax:syntax(Sub, Cmd, Syntax), Req, State};

api_server_syntax(_Syntax, _Req, _State) ->
    continue.


%% @doc
api_server_cmd(#nkapi_req{class=file, subclass=Sub, cmd=Cmd}=Req, State) ->
    nkfile_api:cmd(Sub, Cmd, Req, State);

api_server_cmd(_Req, _State) ->
    continue.

