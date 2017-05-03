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
    File = #{store=>local, name=>n1, content_type=><<"plain/text">>, id=>up1},
    {ok, _, _} = nkfile:upload(root, File, <<"123">>),
    {ok, _, <<"123">>} = nkfile:download(root, File),

    File2 = File#{name=>n2, encryption=>aes_cfb128, id=>up2},
    {ok, #nkfile{password=Pass}, _} = nkfile:upload(root, File2, <<"321">>),
    {error, decryption_error} = nkfile:download(root, File2),
    {ok, _, <<"321">>} = nkfile:download(root, File2#{password=>Pass}).


up_s3() ->
    File = #{store=>'carlos.s3', name=>n1, content_type=><<"plain/text">>, id=>up1},
    {ok, _, _} = nkfile:upload(root, File, <<"123">>),
    {ok, _, <<"123">>} = nkfile:download(root, File),

    File2 = File#{name=>n2, encryption=>aes_cfb128, id=>up2},
    {ok, #nkfile{password=Pass}, _} = nkfile:upload(root, File2, <<"321">>),
    {ok, _, <<"321">>} = nkfile:download(root, File2#{password=>Pass}).


login() ->
    nkdomain_sample:login().

