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

-module(nkfile_s3).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([upload/4, download/3, parse_store/1]).

-include("nkfile.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
upload(_SrvId, Store, #nkfile{obj_id=Id}, Body) ->
    {Bucket, AwsConfig} = get_config(Store),
    try
        Res = erlcloud_s3:put_object(Bucket, to_list(Id), Body, [], AwsConfig),
        {ok, #{res=>Res}}
    catch
        error:Error ->
            {error, {s3_error, nklib_util:to_binary(Error)}}
    end.


%% @doc
download(_SrvId, Store, #nkfile{obj_id=Id}) ->
    {Bucket, AwsConfig} = get_config(Store),
    try
        Res = erlcloud_s3:get_object(Bucket, to_list(Id), [], AwsConfig),
        Body = nklib_util:get_value(content, Res),
        {ok, Body}
    catch
        error:Error ->
            {error, {s3_error, nklib_util:to_binary(Error)}}
    end.



%% @doc
parse_store(Data) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=s3}, _, _} ->
            case nklib_syntax:parse(Data, provider_syntax()) of
                {ok, #{id:=Id, class:=s3} = Parsed, _, _} ->
                    Provider = #nkfile_store{
                        id = Id,
                        class = s3,
                        config = maps:get(config, Parsed, #{})
                    },
                    {ok, Provider};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.


%% @private
provider_syntax() ->
    #{
        id => binary,
        class => atom,
        config => #{
            bucket => binary,
            aws_id => binary,
            aws_secret => binary
        },
        '__mandatory' => [id, class, 'config.bucket', 'config.aws_id', 'config.aws_secret']
    }.



%% @private
get_config(#nkfile_store{config=Config}) ->
    #{bucket:=Bucket, aws_id:=AwsId, aws_secret:=AwsSecret} = Config,
    AwsConfig = #aws_config{
        access_key_id = to_list(AwsId),
        secret_access_key = to_list(AwsSecret),
        s3_follow_redirect = true
%%        s3_host=Host,     "nkobjects.s3.eu-central-1.amazonaws.com"
    },
    {to_list(Bucket), AwsConfig}.


%% @private
to_list(Term) -> nklib_util:to_list(Term).