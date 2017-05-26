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

%% @doc NkFILE external API

-module(nkfile_api_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([syntax/2]).
-export([file_syntax/0, store_syntax/0]).
-export([parse_file_fun/2]).

%% ===================================================================
%% Syntax
%% ===================================================================

syntax(_Cmd, Syntax) ->
	Syntax.



%% ===================================================================
%% File parsing
%% ===================================================================

%% @private
file_syntax() ->
    #{
        id => binary,
        store => binary,
        name => binary,
        content_type => fun ?MODULE:parse_file_fun/2,
        encryption => {atom, [none, aes_cfb128]},
        password => binary,
        debug => boolean,
        '__mandatory' => [store, name, content_type]
    }.


%% @private
parse_file_fun(content_type, Val) ->
    Val2 = to_bin(Val),
    case binary:split(Val2, <<"/">>) of
        [_, _] -> {ok, Val2};
        _ -> error
    end.



%% ===================================================================
%% Provider parsing
%% ===================================================================

%% @private
store_syntax() ->
    #{
        id => binary,
        class => binary,
        config => map,
        '__mandatory' => [id, class]
    }.


%% @private
to_bin(T) -> nklib_util:to_binary(T).