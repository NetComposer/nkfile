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

%% @doc NkFILE

-module(nkfile).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([upload/3, download/2]).
-export([get_store/2, parse_store/2, parse_store/3, parse_file/2, parse_file/3]).
-export_type([store_id/0, store_class/0]).
-include("nkfile.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type store_id() :: term().
-type store_class() :: atom().

-type file() ::
    #{
        name => binary(),
        store_id => store_id(),
        password => binary(),
        debug => boolean(),
        meta => map()
    }.


-type store() ::
    #{
        class => store_class(),
        encryption => undefined | aes_cfb128,
        config => map()
    }.



-type file_body() :: {base64, binary()} | binary() | term().


%% ===================================================================
%% Public
%% ===================================================================

%% @doc Sends a file to the backend
-spec upload(nkservice:id(), file(), file_body()) ->
    {ok, file()} | {error, term()}.

upload(Srv, #{store_id:=StoreId}=File, FileBody) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case get_store(SrvId, StoreId) of
                {ok, Store} ->
                    case nkfile_util:get_body(FileBody) of
                        {ok, BinBody} ->
                            case nkfile_util:encrypt(Store, File, BinBody) of
                                {ok, File2, BinBody2} ->
                                    SrvId:nkfile_upload(SrvId, Store, File2, BinBody2);
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @doc
-spec download(nkservice:id(), file()) ->
    {ok, file(), binary()} | {error, term()}.

download(Srv, #{store_id:=StoreId}=File) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case get_store(SrvId, StoreId) of
                {ok, Store} ->
                    case SrvId:nkfile_download(SrvId, Store, File) of
                        {ok, File, Enc} ->
                            nkfile_util:decrypt(Store, File, Enc);
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @doc
-spec get_store(nkservice:id(), store_id()) ->
    {ok, store()} | {error, term()}.

get_store(SrvId, StoreId) ->
    SrvId:nkfile_get_store(SrvId, StoreId).


%% @doc Parses a store
-spec parse_store(nkservice:id(), map()) ->
    {ok, store(), [binary()]} | {error, term()}.

parse_store(Srv, Map) ->
    parse_store(Srv, Map, #{}).


%% @doc
-spec parse_store(nkservice:id(), map(), nklib_syntax:parse_opts()) ->
    {ok, store(), [binary()]} | {error, term()}.

parse_store(Srv, Map, ParseOpts) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            SrvId:nkfile_parse_store(Map, ParseOpts);
        not_found ->
            {error, service_not_found}
    end.


%% @doc
-spec parse_file(nkservice:id(), map()) ->
    {ok, file(), [binary()]} | {error, term()}.

parse_file(Srv, Map) ->
    parse_store(Srv, Map, #{}).


%% @doc
-spec parse_file(nkservice:id(), map(), nklib_syntax:parse_opts()) ->
    {ok, file(), [binary()]} | {error, term()}.

parse_file(_Srv, Map, ParseOpts) ->
    Syntax = nkfile_util:file_syntax(),
    nklib_syntax:parse(Map, Syntax, ParseOpts).

