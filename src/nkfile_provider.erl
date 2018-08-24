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

%% @doc NkFILE provider definition
-module(nkfile_provider).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([get_spec/3]).
-export([config/1, parse_spec/2, upload/5, download/4, start/5]).
-export([encode_body/5, decode_body/5]).
-export_type([id/0, spec/0, config/0]).

-include("nkfile.hrl").

-define(LLOG(Type, Txt, Args),lager:Type("NkFILE "++Txt, Args)).


%% ===================================================================
%% Types
%% ===================================================================

-type id() :: binary().


-type spec() ::
    #{
        id := binary(),
        storageClass := binary(),
        module := module(),
        atom() => term()
    }.


-type config() ::
    #{
        storageClass => nkfile:storage_class()
    }.


%% ===================================================================
%% Behaviour callbacks
%% ===================================================================

-callback config() -> config().


-callback parse_spec(map()) -> {ok, spec()} | {error, nkservice:msg()}.


-callback encode_body(nkservice:id(), nkservice:package_id(), spec(), nkfile:file_meta(), nkfile:file_body()) ->
    {ok, binary(), Meta::map()} | {error, term()}.


-callback upload(nkservice:id(), nkservice:package_id(), spec(), nkfile:file_meta(), binary()) ->
    {ok, Meta::map()} | {error, nkservice:msg()}.


-callback decode_body(nkservice:id(), nkservice:package_id(), spec(), nkfile:file_meta(), nkfile:file_body()) ->
    {ok, binary()} | {error, term()}.


-callback download(nkservice:id(), nkservice:package_id(), spec(), nkfile:file_meta()) ->
    {ok, binary(), Meta::map()} | {error, nkservice:msg()}.


-callback start(spec(), nkservice:package_spec(), pid(), nkservice:service()) ->
    ok | {error, term()}.


-optional_callbacks([start/4, encode_body/5, decode_body/5]).



%% ===================================================================
%% Public
%% ===================================================================


%% @doc Get specification for a provider, if it was defined in package
-spec get_spec(nkservice:id(), nkservice:package_id(), id()) ->
    spec() | undefined.

get_spec(SrvId, PackageId, ProviderId) ->
    case catch
        nkfile_plugin:get_provider_config(SrvId, to_bin(PackageId), to_bin(ProviderId))
    of
        {'EXIT', _} ->
            undefined;
        Spec ->
            Spec
    end.



%% ===================================================================
%% Plugin callbacks
%% ===================================================================

%% @doc
config(Module) ->
    Module:config().


%% @doc
parse_spec(Module, Map) ->
    case Module:parse_spec(Map) of
        {syntax, Syntax1} ->
            Mandatory1 = maps:get('__mandatory', Syntax1, []),
            Syntax2 = Syntax1#{
                id => binary,
                storageClass => binary,
                '__mandatory' => [id, storageClass|Mandatory1]
            },
            case nklib_syntax:parse(Map, Syntax2) of
                {ok, Parsed, _} ->
                    {ok, Parsed#{module=>Module}};
                {error, Error} ->
                    {error, Error}
            end
    end.


%% @doc
encode_body(SrvId, PackageId, #{module:=Module}=ProviderSpec, FileMeta, File) ->
    case erlang:function_exported(Module, encode_body, 5) of
        true ->
            Module:encode_body(SrvId, PackageId, ProviderSpec, FileMeta, File);
        false ->
            case nkfile_util:check_size(ProviderSpec, FileMeta, File) of
                true ->
                    case nkfile_util:set_hash(ProviderSpec, FileMeta, File) of
                        {ok, FileMeta2} ->
                            case nkfile_util:encrypt(ProviderSpec, FileMeta2, File) of
                                {ok, FileMeta3, File3, Meta} ->
                                    {ok, FileMeta3, File3, Meta};
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                false ->
                    {error, file_too_large}
            end
    end.


%% @doc
decode_body(SrvId, PackageId, #{module:=Module}=ProviderSpec, FileMeta, File) ->
    case erlang:function_exported(Module, decode_body, 5) of
        true ->
            Module:decode_body(SrvId, PackageId, ProviderSpec, FileMeta, File);
        false ->
            case nkfile_util:decrypt(ProviderSpec, FileMeta, File) of
                {ok, File2, Meta} ->
                    case nkfile_util:check_hash(ProviderSpec, FileMeta, File2) of
                        ok ->
                            {ok, File2, Meta};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
upload(SrvId, PackageId, #{module:=Module}=ProviderSpec, FileMeta, Bin) ->
    Module:upload(SrvId, to_bin(PackageId), ProviderSpec, FileMeta, Bin).


%% @doc
download(SrvId, PackageId, #{module:=Module}=ProviderSpec, FileMeta) ->
    Module:download(SrvId, to_bin(PackageId), ProviderSpec, FileMeta).


%% @doc
start(Module, ProviderSpec, PackageSpec, Pid, Service) ->
    case erlang:function_exported(Module, start, 4) of
        true ->
            Module:start(ProviderSpec, PackageSpec, Pid, Service);
        false ->
            ok
    end.


%% @private
to_bin(T) when is_binary(T)-> T;
to_bin(T) -> nklib_util:to_binary(T).