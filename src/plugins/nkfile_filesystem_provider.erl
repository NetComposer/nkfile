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

%% @doc NkFILE Filesystem provider

-module(nkfile_filesystem_provider).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behavior(nkfile_provider).

-export([config/0, parse_spec/1, upload/5, download/4]).
-export_type([result_meta/0]).

-include("nkfile.hrl").
-define(LLOG(Type, Txt, Args),lager:Type("NkFILE Filesystem "++Txt, Args)).


%% @doc returned metadata
-type result_meta() ::
    #{
        file_path => binary()
    }.


%% @doc
config() ->
    #{
        storageClass => <<"filesystem">>
    }.


%% @doc
parse_spec(_Spec) ->
    {syntax, #{
        maxSize => pos_integer,
        encryption => {atom, [aes_cfb128]},
        debug => boolean,
        filePath => binary,
        '__mandatory' => [filePath],
        '__allow_unknown' =>true
    }}.



%% @private
upload(_SrvId, _PackageId, ProviderSpec, FileMeta, FileBin) ->
    #{name:=Name} = FileMeta,
    #{filePath:=ProviderPath} = ProviderSpec,
    FilePath = maps:get(path, FileMeta, <<>>),
    WritePath = filename:join([ProviderPath, FilePath, Name]),
    case file:write_file(WritePath, FileBin) of
        ok ->
            {ok, #{file_path=>FilePath}};
        {error, Error} ->
            lager:warning("write error at ~s: ~p", [WritePath, Error]),
            {error, file_write_error}
    end.


%% @private
download(_SrvId, _PackageId, ProviderSpec, FileMeta) ->
    #{name:=Name} = FileMeta,
    #{filePath:=ProviderPath} = ProviderSpec,
    FilePath = maps:get(path, FileMeta, <<>>),
    ReadPath = filename:join([ProviderPath, FilePath, Name]),
    case file:read_file(ReadPath) of
        {ok, Body} ->
            {ok, Body, #{file_path=>ReadPath}};
        {error, Error} ->
            lager:warning("read error at ~s: ~p", [ReadPath, Error]),
            {error, file_read_error}
    end.
