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

-export([upload/4, download/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(IV, <<"NetComposerFile.">>).


%% ===================================================================
%% Types
%% ===================================================================


-type file_body() :: {base64, binary()} | binary() | term().

%% Options dependant of storage class
%% ----------------------------------
%%
%% - id
%% - name: mandatory for filesystem
%% - password
%% - contentType

-type meta() :: map().

%% ===================================================================
%% Public
%% ===================================================================

%% @doc Sends a file to the backend
-spec upload(nkservice:id(), nkservice:package_id(), file_body(), meta()) ->
    {ok, meta()} | {error, term()}.

upload(SrvId, PackageId, FileBody, Meta) ->
    PackageId2 = nklib_util:to_binary(PackageId),
    case get_body(SrvId, PackageId2, FileBody) of
        {ok, BinBody1} ->
            case encrypt(SrvId, PackageId2, BinBody1, Meta) of
                {ok, BinBody2, Meta2} ->
                    Class = nkservice_util:get_cache(SrvId, {nkfile, PackageId2, storage_class}),
                    ?CALL_SRV(SrvId, nkfile_upload, [SrvId, PackageId2, Class, BinBody2, Meta2]);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
-spec download(nkservice:id(), nkpackage:id(), meta()) ->
    {ok, binary(), meta()} | {error, term()}.

download(SrvId, PackageId, Meta) ->
    PackageId2 = nklib_util:to_binary(PackageId),
    Class = nkservice_util:get_cache(SrvId, {nkfile, PackageId2, storage_class}),
    case ?CALL_SRV(SrvId, nkfile_download, [SrvId, PackageId2, Class, Meta]) of
        {ok, Bin1, Meta2} ->
            decrypt(SrvId, PackageId2, Bin1, Meta2);
        {error, Error} ->
            {error, Error}
    end.





%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_body(SrvId, PackageId, {base64, Base64}) ->
    case catch base64:decode(Base64) of
        {'EXIT', _} ->
            {error, base64_decode_error};
        Bin ->
            get_body(SrvId, PackageId, Bin)
    end;

get_body(SrvId, PackageId, Bin) when is_binary(Bin) ->
    case nkservice_util:get_cache(SrvId, {nkfile, PackageId, max_size}) of
        0 ->
            {ok, Bin};
        Size when byte_size(Bin) > Size ->
            {error, file_too_large};
        _ ->
            {ok, Bin}
    end;

get_body(_SrvId, _PackageId, _FileBody) ->
    {error, invalid_file_body}.


%% @private
encrypt(SrvId, PackageId, Bin, Meta) ->
    case nkservice_util:get_cache(SrvId, {nkfile, PackageId, encryption}) of
        none ->
            {ok, Bin, Meta};
        aes_cfb128 ->
            Pass = maps:get(password, Meta, crypto:strong_rand_bytes(16)),
            case catch crypto:block_encrypt(aes_cfb128, Pass, ?IV, Bin) of
                {'EXIT', _} ->
                    {error, encryption_error};
                Bin2 ->
                    {ok, Bin2, Meta#{password=>base64:encode(Pass)}}
            end;
        Other ->
            {error, {unknown_encryption_algo, Other}}
    end.


%% @doc
decrypt(SrvId, PackageId, Bin, Meta) ->
    case nkservice_util:get_cache(SrvId, {nkfile, PackageId, encryption}) of
        none ->
            {ok, Bin, Meta};
        aes_cfb128 ->
            case Meta of
                #{password:=Pass} ->
                    Pass2 = base64:decode(Pass),
                    case catch crypto:block_decrypt(aes_cfb128, Pass2, ?IV, Bin) of
                        {'EXIT', _} ->
                            {error, decryption_error};
                        Bin2 ->
                            {ok, Bin2, Meta}
                    end;
                _ ->
                    {error, missing_password}
            end;
        Other ->
            {error, {unknown_encryption_algo, Other}}
    end.
