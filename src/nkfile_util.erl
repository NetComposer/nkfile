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

-module(nkfile_util).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([parse_file_meta/1, decode_base64/1, check_size/2, encrypt/2, decrypt/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(IV, <<"NetComposerFile.">>).


%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public
%% ===================================================================

%% @doc Check for valid nkfile:file_meta()
parse_file_meta(Meta) ->
    Syntax = #{
        name => binary,
        contentType => binary,
        id => binary,
        password => binary,
        path => binary,
        '__mandatory' => [name, contentType],
        '__allow_unknown' => true
    },
    case nklib_syntax:parse(Meta, Syntax) of
        {ok, Parsed, _} ->
            {ok, Parsed};
        {error, Error} ->
            {error, Error}
    end.


%% @private Convert to raw if Base64 and check max_size
decode_base64({base64, Base64}) ->
    case catch base64:decode(Base64) of
        {'EXIT', _} ->
            {error, base64_decode_error};
        Bin ->
            {ok, Bin}
    end;

decode_base64(Bin) when is_binary(Bin) ->
    {ok, Bin}.


%% @private
check_size(#{maxSize:=MaxSize}, Bin) when MaxSize > 0 ->
    case byte_size(Bin) =< MaxSize of
        true ->
            ok;
        false ->
            {error, file_too_large}
    end;

check_size(_ProviderSpec, _Bin) ->
    ok.


%% @private
encrypt(#{encryption:=Algo}, Bin) ->
    case Algo of
        aes_cfb128 ->
            Start = nklib_date:epoch(usecs),
            Pass = crypto:strong_rand_bytes(16),
            case catch crypto:block_encrypt(aes_cfb128, Pass, ?IV, Bin) of
                {'EXIT', _} ->
                    {error, encryption_error};
                Bin2 ->
                    Meta = #{
                        password => base64:encode(Pass),
                        crypt_usecs => nklib_date:epoch(usecs) - Start
                    },
                    {ok, Bin2, Meta}
            end;
        Other ->
            {error, {encryption_algo_unknown, Other}}
    end;

encrypt(_, Bin) ->
    {ok, Bin, #{}}.


%% @private
decrypt(#{encryption:=Algo}, Meta, Bin) ->
    case Algo of
        aes_cfb128 ->
            case Meta of
                #{password:=Pass} ->
                    Start = nklib_date:epoch(usecs),
                    Pass2 = base64:decode(Pass),
                    case catch crypto:block_decrypt(aes_cfb128, Pass2, ?IV, Bin) of
                        {'EXIT', _} ->
                            {error, decryption_error};
                        Bin2 ->
                            Meta2 = #{
                                crypt_usecs => nklib_date:epoch(usecs) - Start
                            },
                            {ok, Bin2, Meta2}
                    end;
                _ ->
                    {error, password_missing}
            end;
        Other ->
            {error, {encryption_algo_unknown, Other}}
    end;

decrypt(_, _Meta, Bin) ->
    {ok, Bin, #{}}.


%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).
