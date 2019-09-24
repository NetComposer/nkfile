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

%% @doc NkFILE
-module(nkfile_sample).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-define(SRV, file_test).

-compile(export_all).
-compile(nowarn_export_all).

-include("nkfile.hrl").


%% @doc Starts the service
start() ->
    Spec = #{
        plugins => [nkfile_filesystem, nkfile_s3]
    },
    nkserver:start_link(nkfile, ?SRV, Spec).

% export MINIO_ACCESS_KEY=5UBED0Q9FB7MFZ5EWIOJ; export MINIO_SECRET_KEY=CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI; minio server .



%% @doc Stops the service
stop() ->
    nkserver:stop(?SRV).


%%luerl_test_1() ->
%%    nkservice_luerl_instance:call({?SRV, s1, main}, [test_1], []).
%%
%%
%%s1() -> <<"
%%    fileConfig = {
%%        storage_class = 'filesystem',
%%        file_path = '/tmp'
%%    }
%%
%%    file2 = startPackage('File', fileConfig)
%%
%%    function test_1()
%%        result, meta1 = file2.upload('123', {name='test1'})
%%        if result == 'ok' then
%%            body, meta2 = file2.download({name='test1'})
%%            return body, meta2
%%        else
%%            return 'error'
%%        end
%%    end
%%
%%">>.


% Test filesystem with external config
test_filesystem() ->
    BaseProvider1 = #{
        storage_class => nkfile_filesystem,
        id => test_fs_2a,
        hash_algo => sha256,
        filesystem_config => #{
            file_path => "/tmp"
        }
    },
    {ok, Provider1} = nkfile:parse_provider_spec(?SRV, BaseProvider1),
    {ok, FileMeta} = nkfile:parse_file_meta(?SRV, #{name=>n1, content_type=>any}),

    SHA1 = base64:encode(crypto:hash(sha256, <<"123">>)),
    {ok, #{hash:=SHA1}, #{file_path:=_}} = nkfile:upload(?SRV, Provider1, FileMeta, <<"123">>),
    {error, hash_is_missing} = nkfile:download(?SRV, Provider1, FileMeta),
    {ok, <<"123">>} = file:read_file("/tmp/n1"),
    {ok, <<"123">>, #{file_path:=_}} = nkfile:download(?SRV, Provider1, FileMeta#{hash=>SHA1}),

    BaseProvider2 = #{
        id => test_fs_2b,
        storage_class => nkfile_filesystem,
        hash_algo => sha256,
        encryption_algo => <<"aes_cfb128">>,
        filesystem_config => #{
            file_path => "/tmp"
        }
    },
    {ok, Provider2} = nkfile:parse_provider_spec(?SRV, BaseProvider2),
    SHA2 = base64:encode(crypto:hash(sha256, <<"321">>)),
    {ok, #{password:=Pass, hash:=SHA2}, #{crypt_usecs:=_}} = nkfile:upload(?SRV, Provider2, FileMeta, <<"321">>),

    {error, password_missing} = nkfile:download(?SRV, Provider2, FileMeta),
    {ok, <<"321">>, _} = nkfile:download(?SRV, Provider2, FileMeta#{password=>Pass, hash=>SHA2}),
    {ok, Enc} = file:read_file("/tmp/n1"),
    true = Enc /= <<"321">>,

    ok = file:write_file("/tmp/n1", <<"abc">>),
    {error, hash_invalid} = nkfile:download(?SRV, Provider2, FileMeta#{password=>Pass, hash=>SHA2}),
    ok.



% Test s3 with external config
test_s3() ->
    BaseProvider = #{
        storage_class => nkfile_s3,
        id => test_s3_2a,
        encryption_algo => aes_cfb128,
        debug => true,
        s3_config => #{
            scheme => http,
            host => localhost,
            port => 9000,
            bucket => bucket1,
            key => "5UBED0Q9FB7MFZ5EWIOJ",
            secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
            hackney_pool => pool1
        }
    },
    {ok, Provider} = nkfile:parse_provider_spec(?SRV, BaseProvider),
    {ok, FileMeta} = nkfile:parse_file_meta(?SRV, #{name=>n3, content_type=>any}),
    {ok, #{password:=Pass}, _} = nkfile:upload(?SRV, Provider, FileMeta, <<"321">>),
    {ok, <<"321">>, #{s3_headers:=_}} = nkfile:download(?SRV, Provider, FileMeta#{password=>Pass}),
    ok.

