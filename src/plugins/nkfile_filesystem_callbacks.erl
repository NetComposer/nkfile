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

-module(nkfile_filesystem_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0]).
-export([nkfile_parse_store/1, nkfile_upload/4, nkfile_download/3]).

-include("nkfile.hrl").


%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Plugin callbacks
%%
%% These are used when NkFILE is started as a NkSERVICE plugin
%% ===================================================================


plugin_deps() ->
    [nkfile].



%% ===================================================================
%% Mail callbacks
%% ===================================================================

%% @private
nkfile_parse_store(Data) ->
    nkfile_filesystem:parse_store(Data).


%% @private
nkfile_upload(SrvId, #nkfile_store{class=filesystem}=Store, File, Body) ->
    nkfile_filesystem:upload(SrvId, Store, File, Body).


%% @private
nkfile_download(SrvId, #nkfile_store{class=filesystem}=Store, File) ->
    nkfile_filesystem:download(SrvId, Store, File).




