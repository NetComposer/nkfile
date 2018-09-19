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
%% Stores in cache
%% - {class_module, StorageClass} -> module()


-module(nkfile_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_api/1]).
-include("nkfile.hrl").

-define(LLOG(Type, Txt, Args),lager:Type("NkFILE "++Txt, Args)).



%% ===================================================================
%% Plugin callbacks
%% ===================================================================


%% @doc
plugin_deps() ->
	[].


%% @doc
plugin_api(?PACKAGE_CLASS_FILE) ->
	#{
		luerl => #{
			upload => {nkfile, luerl_upload},
            download => {nkfile, luerl_download}
		}
	};

plugin_api(_Class) ->
	#{}.
