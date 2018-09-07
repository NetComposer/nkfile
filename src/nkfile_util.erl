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

-export([parse_provider_spec/1, parse_file_meta/1, encode_body/3, decode_body/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(IV, <<"NetComposerFile.">>).


%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public
%% ===================================================================


%% @doc
parse_provider_spec(Map) ->
    Syntax = #{
        id => binary,
        storageClass => binary,
        module => module,
        maxSize => pos_integer,
        encryption => {atom, [aes_cfb128]},
        hash => {atom, [sha256]},
        debug => boolean,
        '__mandatory' => [storageClass],
        '__allow_unknown' =>true
    },
    case nklib_syntax:parse(Map, Syntax) of
        {ok, Parsed, _} ->
            {ok, Parsed};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Check for valid nkfile:file_meta()
parse_file_meta(Meta) ->
    Syntax = #{
        id => binary,
        name => binary,
        contentType => binary,
        hash => binary,
        password => binary,
        path => binary,
        size => pos_integer,
        '__mandatory' => [name, contentType],
        '__allow_unknown' => true
    },
    case nklib_syntax:parse(Meta, Syntax) of
        {ok, Parsed, _} ->
            {ok, Parsed};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
encode_body(ProviderSpec, FileMeta, File) ->
    % Adds 'size' parameter
    case check_size(ProviderSpec, FileMeta, File) of
        {ok, FileMeta2} ->
            % Adds 'hash' parameter
            case set_hash(ProviderSpec, FileMeta2, File) of
                {ok, FileMeta3} ->
                    % Adds 'password' parameter (if not specified before)
                    case encrypt(ProviderSpec, FileMeta3, File) of
                        {ok, FileMeta4, File4, Meta} ->
                            {ok, FileMeta4, File4, Meta};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            {error, file_too_large}
    end.


%% @doc
decode_body(ProviderSpec, FileMeta, File) ->
    case decrypt(ProviderSpec, FileMeta, File) of
        {ok, File2, Meta} ->
            case check_hash(ProviderSpec, FileMeta, File2) of
                ok ->
                    {ok, File2, Meta};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private Check size and adds 'size' param
check_size(ProviderSpec, FileMeta, Bin) ->
    MaxSize = maps:get(maxSize, ProviderSpec, 0),
    ByteSize = byte_size(Bin),
    case MaxSize==0 orelse byte_size(Bin) =< MaxSize of
        true ->
            {ok, FileMeta#{size=>ByteSize}};
        false ->
            {error, file_too_large}
    end.



%% @private Check hash and adds 'hash' param
set_hash(ProviderSpec, FileMeta, Bin) ->
    case maps:find(hash, ProviderSpec) of
        {ok, sha256} ->
            Hash = base64:encode(crypto:hash(sha256, Bin)),
            {ok, FileMeta#{hash => Hash}};
        {ok, Other} ->
            {error, {hash_algo_unknown, Other}};
        error ->
            {ok, FileMeta}
    end.


%% @private
encrypt(ProviderSpec, FileMeta, Bin) ->
    case maps:find(encryption, ProviderSpec) of
        {ok, aes_cfb128} ->
            Start = nklib_date:epoch(usecs),
            Pass = case maps:find(password, FileMeta) of
                error ->
                    crypto:strong_rand_bytes(16);
                Pass0 ->
                    base64:decode(Pass0)
            end,
            case catch crypto:block_encrypt(aes_cfb128, Pass, ?IV, Bin) of
                {'EXIT', _} ->
                    {error, encryption_error};
                Bin2 ->
                    FileMeta2 = FileMeta#{password => base64:encode(Pass)},
                    Meta = #{crypt_usecs => nklib_date:epoch(usecs) - Start},
                    {ok, FileMeta2, Bin2, Meta}
            end;
        {ok, Other} ->
            {error, {encryption_algo_unknown, Other}};
        error ->
            {ok, FileMeta, Bin, #{}}
    end.


%% @private
decrypt(ProviderSpec, FileMeta, Bin) ->
    case maps:find(encryption, ProviderSpec) of
        {ok, aes_cfb128} ->
            case FileMeta of
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
        {ok, Other} ->
            {error, {encryption_algo_unknown, Other}};
        error ->
            {ok, Bin, #{}}
    end.


%% @private
check_hash(ProviderSpec, FileMeta, Bin) ->
    case maps:find(hash, ProviderSpec) of
        {ok, sha256} ->
            case FileMeta of
                #{hash:=Hash1} ->
                    Hash2 = base64:decode(Hash1),
                    case Hash2 == crypto:hash(sha256, Bin) of
                        true ->
                            ok;
                        false ->
                            {error, hash_invalid}
                    end;
                _ ->
                    {error, hash_is_missing}
            end;
        {ok, Other} ->
            {error, {hash_algo_unknown, Other}};
        error ->
            ok
    end.




%%%% @private
%%to_bin(T) when is_binary(T)-> T;
%%to_bin(T) -> nklib_util:to_binary(T).
