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

%% @doc NkFILE OTP Application Module

-module(nkfile_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(application).

-export([start/0, start/2, stop/1]).
-export([get_store_ids/0, get_store/1, put_store/2]).
-export([get/1, get/2, put/2, del/1]).

-include("nkfile.hrl").

-define(APP, nkfile).
-compile({no_auto_import,[get/1, put/2]}).

%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts stand alone.
-spec start() -> 
    ok | {error, Reason::term()}.

start() ->
    case nklib_util:ensure_all_started(?APP, permanent) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.

%% @private OTP standard start callback
start(_Type, _Args) ->
    Syntax = #{},
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkFILE v~s is starting", [Vsn]),
            ok = nkservice_util:register_package(?PACKAGE_CLASS_FILE, nkfile),
            {ok, Pid} = nkfile_sup:start_link(),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error({syntax_error, Error})
    end.


%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @doc
get_store_ids() ->
    get(store_ids, []).


%% @doc
get_store(Id) ->
    get({store, nklib_util:to_binary(Id)}, not_found).


%% @doc
put_store(Id, Store) ->
    Ids = get(store_ids, []),
    put(store_ids, nklib_util:store_value(nklib_util:to_binary(Id), Ids)),
    put({store, Id}, Store).



%% Configuration access
get(Key) ->
    nklib_config:get(?APP, Key).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).

put(Key, Val) ->
    nklib_config:put(?APP, Key, Val).

del(Key) ->
    nklib_config:del(?APP, Key).





