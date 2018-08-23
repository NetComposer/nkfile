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

-module(nkfile_filesystem_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_config/3]).

-include("nkfile.hrl").


%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Plugin callbacks
%%
%% ===================================================================


plugin_deps() ->
    [nkfile].



%% @doc
plugin_config(?PKG_CLASS_FILE, #{id:=Id, config:=Config}=Spec, _Service) ->
	Syntax = #{
        storageClass => binary,
        filePath => binary
    },
    case nklib_syntax:parse(Config, Syntax, #{allow_unknown=>true}) of
        {ok, #{storageClass := <<"filesystem">>}=Parsed, _} ->
            case Parsed of
                #{filePath:=Path} ->
                    CacheMap1 = nkservice_config_util:get_cache_map(Spec),
                    CacheMap2 = nkservice_config_util:set_cache_key(nkfile, Id, storage_class, filesystem, CacheMap1),
                    CacheMap3 = nkservice_config_util:set_cache_key(nkfile_filesystem, Id, file_path, Path, CacheMap2),
                    Spec2 = nkservice_config_util:set_cache_map(CacheMap3, Spec),
                    {ok, Spec2#{config:=Parsed}};
                _ ->
                    {error, {field_missing, <<Id/binary, ".config.filePath">>}}
            end;
        {ok, _, _} ->
            continue;
        {error, Error} ->
            {error, Error}
    end;

plugin_config(_Class, _Package, _Service) ->
    continue.

