%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
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

%% @doc NkFILE Filesystem callbacks

-module(nkfile_filesystem_callbacks).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([nkfile_parse_provider_spec/3, nkfile_upload/5, nkfile_download/4, nkfile_delete/4]).

-include("nkfile.hrl").
-include_lib("nkservice/include/nkservice.hrl").

-define(CLASS, <<"filesystem">>).


%% ===================================================================
%% Callbacks
%% ===================================================================


%% @doc
nkfile_parse_provider_spec(SrvId, PackageId, #{storageClass:=?CLASS}=Spec) ->
    Syntax = #{
        filesystemConfig => #{
            filePath => binary,
            '__mandatory' => [filePath]
        },
        '__mandatory' => [filesystemConfig],
        '__allow_unknown' => true
    },
    case nklib_syntax:parse(Spec, Syntax) of
        {ok, Parsed, _} ->
            {continue, [SrvId, PackageId, Parsed]};
        {error, Error} ->
            {error, Error}
    end;

nkfile_parse_provider_spec(_SrvId, _PackageId, _Spec) ->
    continue.


%% @doc
nkfile_upload(_SrvId, _PackageId, #{storageClass:=?CLASS}=ProviderSpec, FileMeta, Bin) ->
    Path = get_path(ProviderSpec, FileMeta),
    case file:write_file(Path, Bin) of
        ok ->
            {ok, #{file_path=>Path}};
        {error, Error} ->
            lager:warning("write error at ~s: ~p", [Path, Error]),
            {error, file_write_error}
    end;

nkfile_upload(_SrvId, _PackageId, _ProviderSpec, _FileMeta, _Bin) ->
    continue.


%% @doc
nkfile_download(_SrvId, _PackageId, #{storageClass:=?CLASS}=ProviderSpec, FileMeta) ->
    Path = get_path(ProviderSpec, FileMeta),
    case file:read_file(Path) of
        {ok, Body} ->
            {ok, Body, #{file_path=>Path}};
        {error, Error} ->
            lager:warning("read error at ~s: ~p", [Path, Error]),
            {error, file_read_error}
    end;

nkfile_download(_SrvId, _PackageId, _ProviderSpec, _FileMeta) ->
    continue.


%% @doc
nkfile_delete(_SrvId, _PackageId, #{storageClass:=?CLASS}=ProviderSpec, FileMeta) ->
    Path = get_path(ProviderSpec, FileMeta),
    file:delete(Path);

nkfile_delete(_SrvId, _PackageId, _ProviderSpec, _FileMeta) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================

get_path(ProviderSpec, FileMeta) ->
    #{name:=Name} = FileMeta,
    #{filesystemConfig:=#{filePath:=FilePath}} = ProviderSpec,
    filename:join([<<"/">>, FilePath, Name]).

