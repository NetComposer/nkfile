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
%% - {provider_config, ProviderId} -> map()
%%
%% fileProviders can be defined in package spec, or defined later

-module(nkfile_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0, plugin_api/1, plugin_config/3, plugin_start/4]).
-export([get_class_module/3, get_provider_config/3]).
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


%% @doc
plugin_config(?PACKAGE_CLASS_FILE, #{config:=Config}=Spec, _Service) ->
    Syntax = #{
        fileProviders => {list, #{
			id => binary,
			storageClass => binary,
			'__mandatory' => [id, storageClass],
            '__check_unique_keys' => [id],
			'__allow_unknown' => true
		}},
        fileProviderModules => {list, module},
        '__mandatory' => [fileProviderModules]
	},
	case nklib_syntax:parse(Config, Syntax) of
		{ok, Parsed, _} ->
			#{fileProviderModules:=Modules} = Parsed,
            {ok, Spec2} = add_modules_cache(Modules, Spec#{config:=Parsed}),
			Providers = maps:get(fileProviders, Parsed, []),
			case add_providers_cache(Providers, Spec2) of
				{ok, Spec3} ->
					{ok, Spec3#{config:=Parsed}};
				{error, Error} ->
					{error, Error}
			end;
		{error, Error} ->
			{error, Error}
	end;

plugin_config(_Class, _Package, _Service) ->
	continue.



%% @doc
plugin_start(?PACKAGE_CLASS_FILE, #{config:=Config}=Spec, Pid, Service) ->
    Providers = maps:get(fileProviders, Config, []),
    case start_providers(Providers, Spec, Pid, Service) of
        ok ->
            ok;
        {error, Error} ->
            {error, Error}
    end;

plugin_start(_Id, _Spec, _Pid, _Service) ->
    continue.


%%%% @doc
%%%% Even if we are called only with modified config, we check if the spec is new
%%plugin_update(?PACKAGE_CLASS_FILE, #{id:=Id, config:=#{apiUrl:=_}=NewConfig}, OldSpec, Pid, #{id:=SrvId}) ->
%%    case OldSpec of
%%        #{config:=NewConfig} ->
%%            ok;
%%        _ ->
%%            {ok, Listeners} = make_listen(SrvId, Id, NewConfig),
%%            insert_listeners(Id, Pid, Listeners)
%%    end;
%%
%%plugin_update(_Class, _NewSpec, _OldSpec, _Pid, _Service) ->
%%    ok.




%% ===================================================================
%% Getters
%% ===================================================================

-type cache_key() ::
	{class_module, Class::binary()} |
	{provider_config, binary()}.


%% @doc
-spec get_cache_key(nkservice:id(), nkservice:package_id(), cache_key()) ->
	term().

get_cache_key(SrvId, PkgId, CacheKey) ->
	nkservice_util:get_cache(SrvId, nkfile, to_bin(PkgId), CacheKey).


%% @doc Gets a storageClass callback module
-spec get_class_module(nkservice:id(), nkservice:package_id(), string()|binary()) ->
	module().

get_class_module(SrvId, PkgId, StorageClass) ->
	get_cache_key(SrvId, PkgId, {class_module, to_bin(StorageClass)}).


%% @doc Gets a configured provider
-spec get_provider_config(nkservice:id(), nkservice:package_id(), ProviderId::binary()) ->
	map().

get_provider_config(SrvId, PkgId, ProviderId) ->
	get_cache_key(SrvId, PkgId, {provider_config, to_bin(ProviderId)}).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
add_modules_cache([], Spec) ->
    {ok, Spec};

add_modules_cache([Module|Rest], Spec) ->
	Cache1 = nkservice_config_util:get_cache_map(Spec),
	code:ensure_loaded(Module),
	Config = nkfile_provider:config(Module),
	#{storageClass:=Class} = Config,
    #{id:=PkgId} = Spec,
	Cache2 = set_cache_key(PkgId, {class_module, to_bin(Class)}, Module, Cache1),
	Spec2 = nkservice_config_util:set_cache_map(Cache2, Spec),
	add_modules_cache(Rest, Spec2).


%% @private
add_providers_cache([], Spec) ->
    {ok, Spec};

add_providers_cache([ProviderSpec|Rest], Spec) ->
	#{id:=ProviderId, storageClass:=Class} = ProviderSpec,
	Cache1 = nkservice_config_util:get_cache_map(Spec),
	#{id:=PkgId} = Spec,
    case get_cache_key(PkgId, {class_module, Class}, Cache1, undefined) of
		undefined ->
			{error, {storage_class_unknown, Class}};
		Module ->
			case nkfile_provider:parse_spec(Module, ProviderSpec) of
				{ok, Parsed} ->
					Cache2 = set_cache_key(PkgId, {provider_config, ProviderId}, Parsed, Cache1),
					Spec2 = nkservice_config_util:set_cache_map(Cache2, Spec),
                    add_providers_cache(Rest, Spec2);
				{error, Error} ->
					{error, Error}
			end
	end.


%% @private
get_cache_key(PkgId, Key, Cache, Default) ->
	nkservice_config_util:get_cache_key(nkfile, PkgId, Key, Cache, Default).


%% @private
set_cache_key(PkgId, Key, Value, Cache) ->
	nkservice_config_util:set_cache_key(nkfile, PkgId, Key, Value, Cache).


%% @private
start_providers([], _Spec, _Pid, _Service) ->
    ok;

start_providers([ProviderSpec|Rest], PackageSpec, Pid, Service) ->
    #{storageClass:=Class} = ProviderSpec,
    #{id:=PackageId} = PackageSpec,
    #{id:=SrvId} = Service,
    Module = get_class_module(SrvId, PackageId, Class),
    case nkfile_provider:start(Module, ProviderSpec, PackageSpec, Pid, Service) of
        ok ->
            start_providers(Rest, PackageSpec, Pid, Service);
        {error, Error} ->
            {error, Error}
    end.



%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).