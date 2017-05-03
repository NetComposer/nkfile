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
-export_type([store_id/0, store_class/0]).
-include("nkfile.hrl").



%% ===================================================================
%% Types
%% ===================================================================

-type store_id() :: term().
-type store_class() :: atom().

-type file() ::
    #{
        obj_id => binary(),
        store_id => store_id(),
        name => binary(),
        content_type => binary(),
        encryption => undefined | aes_cfb128,
        password => binary(),
        debug => boolean()
    }.

-type file_body() :: {base64, binary()} | binary() | term().


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec upload(nkservice:id(), #nkfile{}|file(), file_body()) ->
    {ok, #nkfile{}, Meta::map()} | {error, term()}.

upload(Srv, #nkfile{store_id=StoreId}=File, FileBody) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:nkfile_get_store(SrvId, StoreId) of
                {ok, Store} ->
                    case SrvId:nkfile_get_body(SrvId, Store, FileBody) of
                        {ok, BinBody} ->
                            case nkfile_util:encrypt(File, BinBody) of
                                {ok, File2, BinBody2} ->
                                    case SrvId:nkfile_upload(SrvId, Store, File2, BinBody2) of
                                        {ok, Meta} ->
                                            {ok, File2, Meta};
                                        {error, Error} ->
                                            {error, Error}
                                    end;
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
    end;

upload(Srv, Msg, FileBody) ->
    case nkfile_util:parse_file(Msg) of
        {ok, #nkfile{}=File} ->
            upload(Srv, File, FileBody);
        {error, Error} ->
            {error, Error}
    end.



%% @doc
-spec download(nkservice:id(), #nkfile{}|file()) ->
    {ok, #nkfile{}, binary()} | {error, term()}.

download(Srv, #nkfile{store_id=StoreId}=File) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            case SrvId:nkfile_get_store(SrvId, StoreId) of
                {ok, Store} ->
                    case SrvId:nkfile_download(SrvId, Store, File) of
                        {ok, Enc} ->
                            nkfile_util:decrypt(File, Enc);
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end;

download(Srv, Msg) ->
    case nkfile_util:parse_file(Msg) of
        {ok, #nkfile{}=File} ->
            download(Srv, File);
        {error, Error} ->
            {error, Error}
    end.










