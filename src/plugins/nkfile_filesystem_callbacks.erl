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

-export([nkfile_upload/5, nkfile_download/4]).

-include("nkfile.hrl").


%% ===================================================================
%% Types
%% ===================================================================

% -type continue() :: continue | {continue, list()}.





%% ===================================================================
%% Callbacks
%% ===================================================================


%% @private
nkfile_upload(SrvId, PackageId, filesystem, Bin, Meta) ->
    case Meta of
        #{name:=Name} ->
            Path = nkservice_util:get_cache(SrvId, {nkfile_filesystem, PackageId, file_path}),
            Path2 = filename:join(Path, Name),
            case file:write_file(Path2, Bin) of
                ok ->
                    {ok, Meta#{file_path=>Path2}};
                {error, Error} ->
                    lager:warning("NkFILE write error at ~s: ~p", [Path2, Error]),
                    {error, file_write_error}
            end;
        _ ->
            {error, missing_file_name1}
    end;

nkfile_upload(_SrvId, _PackageId, _StorageClass, _Bin, _Meta) ->
    continue.



%% @private
nkfile_download(SrvId, PackageId, filesystem, Meta) ->
    case Meta of
        #{name:=Name} ->
            Path = nkservice_util:get_cache(SrvId, {nkfile_filesystem, PackageId, file_path}),
            Path2 = filename:join(Path, Name),
            case file:read_file(Path2) of
                {ok, Body} ->
                    {ok, Body, Meta#{file_path=>Path2}};
                {error, Error} ->
                    lager:warning("NkFILE read error at ~s: ~p", [Path2, Error]),
                    {error, file_read_error}
            end;
        _ ->
            {error, missing_file_name2}
    end;

nkfile_download(_SrvId, _PackageId, _StorageClass, _Meta) ->
    continue.
