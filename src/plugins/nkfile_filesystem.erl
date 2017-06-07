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

-module(nkfile_filesystem).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([upload/4, download/3, parse_store/2]).

-include("nkfile.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
upload(_SrvId, #{config:=#{path:=Path}}, #{name:=Name}=File, Body) ->
    Path2 = filename:join(Path, Name),
    case file:write_file(Path2, Body) of
        ok ->
            Meta = maps:get(meta, File, #{}),
            {ok, File#{meta=>Meta#{file_path=>Path2}}};
        {error, Error} ->
            {error, {file_write_error, Path2, nklib_util:to_binary(Error)}}
    end.


%% @doc
download(_SrvId, #{config:=#{path:=Path}}, #{name:=Name}=File) ->
    Path2 = filename:join(Path, Name),
    case file:read_file(Path2) of
        {ok, File, Body} ->
            {ok, File, Body};
        {error, Error} ->
            {error, {file_read_error, Path2, nklib_util:to_binary(Error)}}
    end.


%% @doc
parse_store(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=filesystem}, _} ->
            case nklib_syntax:parse(Data, store_syntax(), ParseOpts) of
                {ok, Store, UnknownFields} ->
                    {ok, Store, UnknownFields};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.


%% @private
store_syntax() ->
    Base = nkfile_util:store_syntax(),
    Base#{
        config := #{
            path => binary,
            '__mandatory' => [path]
        }
    }.








