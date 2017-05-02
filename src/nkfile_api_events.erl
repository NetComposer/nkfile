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

%% @doc NkFILE external events processing

-module(nkfile_api_events).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([event/3]).

-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Callbacks
%% ===================================================================

%% @private
-spec event(nkfile_session:id(), nkfile_session:event(),
            nkfile_session:session()) ->
	{ok, nkfile_session:session()}.

event(_SessId, _Event, Session) ->
    {ok, Session}.



%% ===================================================================
%% Internal
%% ===================================================================

