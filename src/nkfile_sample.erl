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

-define(SRV, file_test).

-compile(export_all).
-compile(nowarn_export_all).

-include("nkfile.hrl").


%% @doc Starts the service
start() ->
    Spec = #{
        plugins => [nkfile_filesystem, nkfile_s3],
        packages => [
            #{
                id => file_pkg,
                class => 'File',
                config => #{
                    fileProviders => [
                        #{
                            id => file1,
                            storageClass => filesystem,
                            filePath => "/tmp",
                            debug => true
                        },
                        #{
                            id => file2,
                            storageClass => filesystem,
                            filePath => "/tmp",
                            encryption => aes_cfb128
                        },
                        #{
                            id => s3,
                            storageClass => s3,
                            url => "http://localhost:9000",
                            s3Key => "5UBED0Q9FB7MFZ5EWIOJ",
                            s3Secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
                            bucket => bucket1,
                            encryption => aes_cfb128,
                            debug => true
                        }
                    ]
                }
            },
            #{
                id => pool1,
                class => 'HttpPool',
                config => #{
                    targets => [
                        #{
                            url => "http://localhost:9000",
                            weight => 1,
                            pool => 2
                        },
                        #{
                            url => "http://192.168.0.9:9000",
                            weight => 2,
                            pool => 2
                        }
                        %%  #{
                        %%     url => "https://s3-eu-west-1.amazonaws.com",
                        %%    pool => 2,
                        %%   opts => #{tls_verify=>host, debug=>false}
                        %% }
                    ],
                    resolveIntervalSecs => 15000,
                    debug => true
                }
            }
        ]
%%        modules => [
%%            #{
%%                id => s1,
%%                class => luerl,
%%                code => s1(),
%%                debug => true
%%            }
%%        ]
    },
    nkservice:start(?SRV, Spec).

%% Export MINIO
%% export MINIO_ACCESS_KEY=5UBED0Q9FB7MFZ5EWIOJ
%% export MINIO_SECRET_KEY=CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI
%% minio server http://127.0.0.1:9000/tmp


%% @doc Stops the service
stop() ->
    nkservice:stop(?SRV).


luerl_test_1() ->
    nkservice_luerl_instance:call({?SRV, s1, main}, [test_1], []).


s1() -> <<"
    fileConfig = {
        storageClass = 'filesystem',
        filePath = '/tmp'
    }

    file2 = startPackage('File', fileConfig)

    function test_1()
        result, meta1 = file2.upload('123', {name='test1'})
        if result == 'ok' then
            body, meta2 = file2.download({name='test1'})
            return body, meta2
        else
            return 'error'
        end
    end

">>.



% Test filesystem with in-package config
test_filesystem_1() ->
    FileMeta = #{name=>n1, contentType=>any},
    {ok, #{file_path:=_}} = nkfile:upload(?SRV, file_pkg, file1, FileMeta, <<"123">>),
    {ok, <<"123">>, #{file_path:=_}} = nkfile:download(?SRV, file_pkg, file1, FileMeta),

    {ok, #{password:=Pass}} = nkfile:upload(?SRV, file_pkg, file2, FileMeta, <<"321">>),
    {error, password_missing} = nkfile:download(?SRV, file_pkg, file2, FileMeta),
    {ok, <<"321">>, _} = nkfile:download(?SRV, file_pkg, file2, FileMeta#{password=>Pass}).


% Test filesystem with external config
test_filesystem_2() ->
    BaseProvider1 = #{
        id => test_fs_2a,
        storageClass => filesystem,
        filePath => "/tmp"
    },
    {ok, Provider1} = nkfile:parse_provider_spec(?SRV, file_pkg, BaseProvider1),
    FileMeta = #{name=>n1, contentType=>any},
    {ok, #{file_path:=_}} = nkfile:upload(?SRV, file_pkg, Provider1, FileMeta, <<"123">>),
    {ok, <<"123">>, #{file_path:=_}} = nkfile:download(?SRV, file_pkg, Provider1, FileMeta),

    BaseProvider2 = #{
        id => test_fs_2b,
        storageClass => filesystem,
        filePath => "/tmp",
        encryption => aes_cfb128
    },
    {ok, Provider2} = nkfile:parse_provider_spec(?SRV, file_pkg, BaseProvider2),
    {ok, #{password:=Pass}} = nkfile:upload(?SRV, file_pkg, Provider2, FileMeta, <<"321">>),
    {error, password_missing} = nkfile:download(?SRV, file_pkg, Provider2, FileMeta),
    {ok, <<"321">>, _} = nkfile:download(?SRV, file_pkg, Provider2, FileMeta#{password=>Pass}).


% Test s3 with in-package config
test_s3_1() ->
    FileMeta3 = #{name=>n3, contentType=>any},
    {ok, #{password:=Pass}} = nkfile:upload(?SRV, file_pkg, s3, FileMeta3, <<"321">>),
    {ok, <<"321">>, _} = nkfile:download(?SRV, file_pkg, s3, FileMeta3#{password=>Pass}).


% Test s3 with external config
test_s3_2() ->
    BaseProvider = #{
        id => test_s3_2a,
        storageClass => s3,
        url => "http://localhost:9000",
        s3Key => "5UBED0Q9FB7MFZ5EWIOJ",
        s3Secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
        bucket => bucket1,
        encryption => aes_cfb128,
        debug => true
    },
    {ok, Provider} = nkfile:parse_provider_spec(?SRV, file_pkg, BaseProvider),
    FileMeta = #{name=>n3, contentType=>any},
    {ok, #{password:=Pass}} = nkfile:upload(?SRV, file_pkg, Provider, FileMeta, <<"321">>),
    {ok, <<"321">>, _} = nkfile:download(?SRV, file_pkg, Provider, FileMeta#{password=>Pass}).


% Test s3 with external config & pool
test_s3_3() ->
    BaseProvider = #{
        id => test_s3_2a,
        storageClass => s3,
        url => "http://localhost:9000",
        s3Key => "5UBED0Q9FB7MFZ5EWIOJ",
        s3Secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
        bucket => bucket1,
        encryption => aes_cfb128,
        debug => true,
        connection_opts => #{
            httpc_pool => <<"file_test-pool1">>
        }
    },
    {ok, Provider} = nkfile:parse_provider_spec(?SRV, file_pkg, BaseProvider),
    FileMeta = #{name=>n3, contentType=>any},
    {ok, #{password:=Pass}} = nkfile:upload(?SRV, file_pkg, Provider, FileMeta, <<"321">>),
    {ok, <<"321">>, _} = nkfile:download(?SRV, file_pkg, Provider, FileMeta#{password=>Pass}).
