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

-module(nkfile_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_api/1, plugin_config/3]).

-include("nkfile.hrl").

-define(LLOG(Type, Txt, Args),lager:Type("NkFILE "++Txt, Args)).




%% ===================================================================
%% Plugin callbacks
%% ===================================================================


%% @doc
plugin_deps() ->
	[].

%% @doc
plugin_api(?PKG_CLASS_FILE) ->
	#{
		luerl => #{
			upload => {nkfile, luerl_upload},
            download => {nkfile, luerl_download}
		}
	};

plugin_api(_Class) ->
	#{}.


%% @doc
plugin_config(?PKG_CLASS_FILE, #{id:=Id, config:=Config}=Spec, _Service) ->
    Syntax = #{
        storageClass => binary,
        maxSize => {integer, 1, none},
        encryption => {atom, [aes_cfb128]},
        debug => boolean,
        '__mandatory' => [storageClass]
	},
	case nklib_syntax:parse(Config, Syntax, #{allow_unknown=>true}) of
		{ok, Parsed, _} ->
			Debug = maps:get(debug, Parsed, false),
            DebugMap1 = maps:get(debug_map, Spec, #{}),
			DebugMap2 = DebugMap1#{{nkfile, Id, debug} => Debug},
            CacheMap1 = maps:get(cache_map, Spec, #{}),
            case maps:is_key({nkfile, Id, storage_class}, CacheMap1) of
                true ->
                    CacheMap2 = CacheMap1#{
                        {nkfile, Id, max_size} => maps:get(maxSize, Parsed, 0),
                        {nkfile, Id, encryption} => maps:get(encryption, Parsed, none)
                    },
                    Spec2 = Spec#{
                        config := Parsed,
                        cache_map => CacheMap2,
                        debug_map => DebugMap2
                    },
                    {ok, Spec2};
                false ->
                    {error, unknown_storage_class}
            end;
		{error, Error} ->
			{error, Error}
	end;

plugin_config(_Class, _Package, _Service) ->
	continue.


%% ===================================================================
%% Internal
%% ===================================================================
