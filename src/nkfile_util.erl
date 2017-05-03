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
-export([parse_file/1, encrypt/2, decrypt/2]).

-include("nkfile.hrl").

-define(IV, <<"NetComposerFile.">>).


%% ===================================================================
%% Public
%% ===================================================================


%% @doc
parse_file(Msg) ->
    Syntax = nkfile_api_syntax:file_syntax(),
    case nklib_syntax:parse(Msg, Syntax) of
        {ok, Parsed, _, []} ->
            #{
                store := Store,
                name := Name,
                content_type := CT
            } = Parsed,
            ObjId = case maps:find(id, Parsed) of
                {ok, UserObjId} -> UserObjId;
                error -> nklib_util:luid()
            end,
            File = #nkfile{
                obj_id = ObjId,
                store_id = Store,
                name = Name,
                content_type = CT,
                encryption = maps:get(encryption, Parsed, none),
                password = maps:get(password, Parsed, <<>>),
                debug = maps:get(debug, Parsed, false)
            },
            {ok, File};
        {ok, _Parsed, _, [Key|_]} ->
            {error, {unknown_field, Key}};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
encrypt(#nkfile{encryption=none}=File, FileBody) ->
    {ok, File#nkfile{size=byte_size(FileBody)}, FileBody};

encrypt(#nkfile{encryption=aes_cfb128}=File, FileBody) ->
    Pass = crypto:strong_rand_bytes(16),
    case catch crypto:block_encrypt(aes_cfb128, Pass, ?IV, FileBody) of
        {'EXIT', _} ->
            {error, encryption_error};
        Enc ->
            {ok, File#nkfile{size=byte_size(FileBody), password=Pass}, Enc}
    end;

encrypt(_, _) ->
    {error, encryption_error}.


%% @doc
decrypt(#nkfile{encryption=none}=File, FileBody) ->
    {ok, File#nkfile{size=byte_size(FileBody)}, FileBody};

decrypt(#nkfile{encryption=aes_cfb128, password=Pass}=File, Enc) ->
    case catch crypto:block_decrypt(aes_cfb128, Pass, ?IV, Enc) of
        {'EXIT', _} ->
            {error, decryption_error};
        FileBody ->
            {ok, File#nkfile{size=byte_size(FileBody)}, FileBody}
    end;

decrypt(_, _) ->
    {error, decryption_error}.
