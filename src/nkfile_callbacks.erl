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

-module(nkfile_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([msg/1]).
-export([nkfile_parse_meta/4, nkfile_upload/4, nkfile_download/3]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
msg(base64_decode_error)              -> "BASE64 decode error";
msg(decryption_error)                 -> "Decryption error";
msg(encryption_error)                 -> "Encryption error";
msg(file_read_error)                  -> "File read error";
msg(file_write_error)                 -> "File write error";
msg(invalid_file_body)                -> "Invalid file body";
msg(invalid_store)                    -> "Invalid store";
msg({store_not_found, Id})            -> {"Store not found: ~p", [Id]};
msg({encryption_algo_unknown, Algo})  -> {"Unknown encryption algorithm: '~s'", [Algo]};
msg(_)                                -> continue.



%% ===================================================================
%% Mail callbacks
%% ===================================================================


%% @doc
-spec nkfile_parse_meta(nkservice:id(), nkservice:package_id(), binary(), nkfile:meta()) ->
    {ok, nkfile:meta()} | {error, term()}.

nkfile_parse_meta(_SrvId, _PackageId, _Class, Meta) ->
    % Fields used by most plugins
    Syntax = #{
        id => binary,
        name => binary,
        password => binary,
        contentType => binary,
        path => binary,
        '__allow_unknown' => true
    },
    case nklib_syntax:parse(Meta, Syntax) of
        {ok, Parsed, _} ->
            {ok, Parsed};
        {error, Error} ->
            {error, Error}
    end.


%% @doc Stores the file
-spec nkfile_upload(nkservice:id(), nkservice:package_id(), binary(), nkfile:meta()) ->
    {ok, nkfile:file()} | {error, term()}.

nkfile_upload(_SrvId, _PackageId, _Body, _Meta) ->
    {error, invalid_store}.


%% @doc Retrieves the file
-spec nkfile_download(nkservice:id(), nkservice:package_id(), nkfile:meta()) ->
    {ok, nkfile:file(), binary()} | {error, term()}.

nkfile_download(_SrvId, _PackageId, _Meta) ->
    {error, invalid_store}.

