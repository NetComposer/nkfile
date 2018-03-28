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
                id => file1,
                class => 'File',
                config => #{
                    storageClass => filesystem,
                    filePath => "/tmp",
                    debug => true
                }
            },
            #{
                id => file2,
                class => 'File',
                config => #{
                    storageClass => filesystem,
                    filePath => "/tmp",
                    encryption => aes_cfb128
                }
            },
            #{
                id => s3,
                class => 'File',
                config => #{
                    storageClass => s3,
                    targets => [
%%                        #{
%%                            url => "http://localhost:9000",
%%                            weight => 1,
%%                            pool => 2
%%                        },
%%                        #{
%%                            url => "http://127.0.0.2:9000",
%%                            weight => 2,
%%                            pool => 2
%%                        },
                        #{
                            url => "https://s3-eu-west-1.amazonaws.com",
                            pool => 2
                        }

                    ],
%%                    s3_id => "5UBED0Q9FB7MFZ5EWIOJ",
%%                    s3_secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
%%                    s3_bucket => bucket1,

                    s3_id => "AKIAJZ7VECSGCJVB6JIQ",
                    s3_secret => "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",
                    s3_bucket => <<"carlos-publico">>,



                    encryption => aes_cfb128,
                    debug => true
                }
            }
        ],
        modules => [
            #{
                id => s1,
                class => luerl,
                code => s1(),
                debug => true
            }
        ]
    },
    nkservice:start(?SRV, Spec).


%% @doc Stops the service
stop() ->
    nkservice:stop(?SRV).


%%getRequest() ->
%%    nkservice_luerl_instance:call({?SRV, s1, main}, [getRequest], []).


s1() -> <<"
    fileConfig = {
        storageClass = 'filesystem',
        filePath = '/tmp'
    }

    file2 = startPackage('File', fileConfig)

    function getRequest()
        return es.request('get', '/')
    end

">>.


opts() ->
    nkservice_util:get_cache(?SRV, {nkelastic, <<"es1">>, opts}).




test() ->
    {ok, #{file_path:=_}} = nkfile:upload(?SRV, file1, <<"123">>, #{name=>n1}),
    {ok, <<"123">>, #{file_path:=_}} = nkfile:download(?SRV, file1, #{name=>n1}),

    {ok, #{password:=Pass}} = nkfile:upload(?SRV, file2, <<"321">>, #{name=>n2}),
    {error, missing_password} = nkfile:download(?SRV, file2, #{name=>n2}),
    {ok, <<"321">>, _} = nkfile:download(?SRV, file2, #{name=>n2, password=>Pass}),

    {ok, #{password:=Pass}} = nkfile:upload(?SRV, s3, <<"321">>, #{name=>n3}),
    {error, missing_password} = nkfile:download(?SRV, s3, #{name=>n2}),
    {ok, <<"321">>, _} = nkfile:download(?SRV, s3, #{name=>n3, password=>Pass}).


test2() ->
    {ok, #{password:=Pass}} = nkfile:upload(?SRV, s3, <<"321">>, #{name=>n3}),
    {error, missing_password} = nkfile:download(?SRV, s3, #{name=>n3}),
    {ok, <<"321">>, _} = nkfile:download(?SRV, s3, #{name=>n3, password=>Pass}).


