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

%% @doc NkFILE Utilities

-module(nkfile_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([store_syntax/0, file_syntax/0, get_body/1, encrypt/3, decrypt/3]).

-include("nkfile.hrl").

-define(IV, <<"NetComposerFile.">>).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
store_syntax() ->
    #{
        class => atom,
        encryption => atom,
        config => map,
        '__mandatory' => [class, config]
    }.


%% @doc
file_syntax() ->
    #{
        store_id => binary,
        name => binary,
        password => binary,
        debug => boolean,
        '__mandatory' => [store_id, name]
    }.



%% @doc
get_body({base64, Base64}) ->
    case catch base64:decode(Base64) of
        {'EXIT', _} ->
            {error, base64_decode_error};
        Bin ->
            {ok, Bin}
    end;

get_body(Bin) when is_binary(Bin) ->
    {ok, Bin};

get_body(_FileBody) ->
    {error, invalid_file_body}.



%% @doc
encrypt(#{encryption:=aes_cfb128}, File, FileBody) ->
    Pass = crypto:strong_rand_bytes(16),
    case catch crypto:block_encrypt(aes_cfb128, Pass, ?IV, FileBody) of
        {'EXIT', _} ->
            {error, encryption_error};
        Enc ->
            lager:error("ENC"),

            {ok, File#{password=>base64:encode(Pass)}, Enc}
    end;

encrypt(#{encryption:=Algo}, _File, _FileBody) ->
    {error, {unknown_encryption_algo, Algo}};

encrypt(_Store, File, FileBody) ->
    {ok, File, FileBody}.



%% @doc
decrypt(#{encryption:=aes_cfb128}, #{password:=Pass}=File, Enc) ->
    Pass2 = base64:decode(Pass),
    case catch crypto:block_decrypt(aes_cfb128, Pass2, ?IV, Enc) of
        {'EXIT', _} ->
            {error, decryption_error};
        FileBody ->
            {ok, File, FileBody}
    end;

decrypt(#{encryption:=aes_cfb128}, _File, _Enc) ->
    {error, missing_password};

decrypt(#{encryption:=Algo}, _File, _FileBody) ->
    {error, {unknown_encryption_algo, Algo}};

decrypt(_Store, File, FileBody) ->
    {ok, File, FileBody}.
