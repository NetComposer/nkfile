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

-module(nkfile_s3_mini_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([plugin_deps/0]).
-export([nkfile_parse_store/2, nkfile_upload/4, nkfile_download/3]).

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
nkfile_parse_store(Data, ParseOpts) ->
    nkfile_s3_mini:parse_store(Data, ParseOpts).


%% @private
nkfile_upload(SrvId, #{class:=s3_mini}=Store, File, Body) ->
    nkfile_s3:upload(SrvId, Store, File, Body);

nkfile_upload(_SrvId, _Store, _File, _Body) ->
    continue.


%% @private
nkfile_download(SrvId, #{class:=s3_mini}=Store, File) ->
    nkfile_s3:download(SrvId, Store, File);

nkfile_download(_SrvId, _Store, _File) ->
    continue.

