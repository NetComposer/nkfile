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

-module(nkfile_s3_plugin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([plugin_deps/0, plugin_config/3, plugin_start/4, plugin_update/5]).
-include("nkfile.hrl").


-define(LLOG(Type, Txt, Args),lager:Type("NkFILE S3 "++Txt, Args)).


%% ===================================================================
%% Plugin callbacks
%%
%% These are used when NkFILE is started as a NkSERVICE plugin
%% ===================================================================

plugin_deps() ->
    [nkfile].


%% @doc
plugin_config(?PKG_FILE, #{id:=Id, config:=Config}=Spec, _Service) ->
    case nklib_syntax:parse(Config, #{storageClass=>binary}) of
        {ok, #{storageClass:=<<"s3">>}, _} ->
            Syntax = #{
                targets => {list, #{
                    url => binary,
                    opts => nkpacket_syntax:safe_syntax(),
                    weight => {integer, 1, 1000},
                    pool => {integer, 1, 1000},
                    refresh => boolean,
                    '__mandatory' => [url]
                }},
                s3_Id => binary,
                s3_Secret => binary,
                bucket => binary,
                resolveInterval => {integer, 0, none},
                '__mandatory' => [targets, s3_Id, s3_Secret, bucket]
            },
            case nklib_syntax:parse(Config, Syntax, #{allow_unknown=>true}) of
                {ok, Parsed, _} ->
                    S3Config = #{
                        key_id => maps:get(s3_Id, Parsed),
                        key => maps:get(s3_Secret, Parsed),
                        bucket => maps:get(bucket, Parsed)
                    },
                    CacheMap1 = maps:get(cache_map, Spec, #{}),
                    CacheMap2 = CacheMap1#{
                        {nkfile, Id, storage_class} => s3,
                        {nkfile_s3, Id, config} => S3Config
                    },
                    Spec2 = Spec#{
                        config := Parsed,
                        cache_map => CacheMap2
                    },
                    {ok, Spec2};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end;

plugin_config(_Class, _Package, _Service) ->
    continue.



%% @doc
plugin_start(?PKG_FILE, #{id:=Id, config:=Config}, Pid, Service) ->
    case Config of
        #{storageClass:=<<"s3">>} ->
            insert(Id, Config, Pid, Service);
        _ ->
            continue
    end;

plugin_start(_Id, _Spec, _Pid, _Service) ->
    continue.


%% @doc
%% Even if we are called only with modified config, we check if the spec is new
plugin_update(?PKG_FILE, #{id:=Id, config:=NewConfig}, OldSpec, Pid, Service) ->
    case NewConfig of
        #{storageClass:=<<"s3">>} ->
            case OldSpec of
                #{config:=NewConfig} ->
                    ok;
                _ ->
                    insert(Id, NewConfig, Pid, Service)
            end;
        _ ->
            continue
    end;

plugin_update(_Class, _NewSpec, _OldSpec, _Pid, _Service) ->
    ok.



%% ===================================================================
%% Internal
%% ===================================================================



%% @private
insert(Id, Config, SupPid, #{id:=SrvId}) ->
	PoolConfig = #{
		targets => maps:get(targets, Config),
		debug => maps:get(debug, Config, false),
		resolve_interval => maps:get(resolveInterval, Config, 0)
	},
	Spec = #{
		id => Id,
		start => {nkpacket_httpc_pool, start_link, [{nkfile_s3, SrvId, Id}, PoolConfig]}
	},
	case nkservice_packages_sup:update_child(SupPid, Spec, #{}) of
		{ok, ChildPid} ->
			nklib_proc:put({nkfile_s3, SrvId, Id}, undefined, ChildPid),
			?LLOG(debug, "started ~s (~p)", [Id, ChildPid]),
			ok;
		not_updated ->
			?LLOG(debug, "didn't upgrade ~s", [Id]),
			ok;
		{upgraded, ChildPid} ->
			nklib_proc:put({nkfile_s3, SrvId, Id}, undefined, ChildPid),
			?LLOG(info, "upgraded ~s (~p)", [Id, ChildPid]),
			ok;
		{error, Error} ->
			?LLOG(notice, "start/update error ~s: ~p", [Id, Error]),
			{error, Error}
	end.

