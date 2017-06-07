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

%% @doc NkFILE
-module(nkfile_sample).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-compile(export_all).

-include("nkfile.hrl").


up() ->
    File1 = #{store_id=>local, name=>n1},
    {ok, _} = nkfile:upload(root, File1, <<"123">>),
    {ok, _, <<"123">>} = nkfile:download(root, File1),

    File2 = #{store_id=>local_secure, name=>n2},
    {ok, #{password:=_}=File3} = nkfile:upload(root, File2, <<"321">>),
    {error, missing_password} = nkfile:download(root, File2),
    {ok, _, <<"321">>} = nkfile:download(root, File3).


up_s3() ->
    File1 = #{store_id=>'carlos.s3', name=>n1},
    {ok, _} = nkfile:upload(root, File1, <<"123">>),
    {ok, _, <<"123">>} = nkfile:download(root, File1),

    File2 = #{store_id=>'carlos.s3_secure', name=>n2},
    {ok, #{password:=_}=File3} = nkfile:upload(root, File2, <<"321">>),
    {error, missing_password} = nkfile:download(root, File2),
    {ok, _, <<"321">>} = nkfile:download(root, File3).


