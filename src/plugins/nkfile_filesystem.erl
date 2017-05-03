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
-export([upload/4, download/3, parse_store/1]).

-include("nkfile.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
upload(_SrvId, #nkfile_store{config=#{path:=Path}}, #nkfile{obj_id=Id}, Body) ->
    Path2 = filename:join(Path, Id),
    case file:write_file(Path2, Body) of
        ok ->
            {ok, #{path=>Path2}};
        {error, Error} ->
            {error, {file_write_error, Path2, nklib_util:to_binary(Error)}}
    end.


%% @doc
download(_SrvId, #nkfile_store{config=#{path:=Path}}, #nkfile{obj_id=Id}) ->
    Path2 = filename:join(Path, Id),
    case file:read_file(Path2) of
        {ok, Body} ->
            {ok, Body};
        {error, Error} ->
            {error, {file_read_error, Path2, nklib_util:to_binary(Error)}}
    end.


%% @doc
parse_store(Data) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=filesystem}, _, _} ->
            case nklib_syntax:parse(Data, provider_syntax()) of
                {ok, #{id:=Id, class:=filesystem} = Parsed, _, _} ->
                    Provider = #nkfile_store{
                        id = Id,
                        class = filesystem,
                        config = maps:get(config, Parsed, #{})
                    },
                    {ok, Provider};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.


%% @private
provider_syntax() ->
    #{
        id => binary,
        class => atom,
        config => #{
            path => binary
        },
        '__mandatory' => [id, class, 'config.path']
    }.








