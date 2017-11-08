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

-module(nkfile_s3_mini).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([upload/4, download/3, parse_store/2, store_syntax/0]).

-include("nkfile.hrl").

%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
upload(_SrvId, Store, #{name:=Name}=File, Body) ->
    {Bucket, AwsConfig} = get_config(Store),
    try
        case mini_s3:put_object(Bucket, to_list(Name), Body, [], [], AwsConfig) of
            [{version_id, _}]=Res -> 
                Meta = maps:get(meta, File, #{}),
                {ok, File#{meta=>Meta#{aws_res=>maps:from_list(Res)}}};
            Other-> 
                {error, {s3_error, nklib_util:to_binary(Other)}}
        end
    catch
        error:Error ->
            {error, {s3_error, nklib_util:to_binary(Error)}}
    end.


%% @doc
download(_SrvId, Store, #{name:=Name}=File) ->
    {Bucket, AwsConfig} = get_config(Store),
    try
        case mini_s3:get_object(Bucket, to_list(Name), [], AwsConfig) of
            [_|_]=Meta ->
                case proplists:get_value(content, Meta) of
                    undefined ->
                        {error, {s3_error, no_content}};
                    Body ->
                        {ok, File, Body}
                end;
            Other ->
                {error, {s3_error, nklib_util:to_binary(Other)}}
        end
    catch
        error:Error ->
            {error, {s3_error, nklib_util:to_binary(Error)}}
    end.



%% @doc
parse_store(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=s3_mini}, _} ->
            case nklib_syntax:parse(Data, store_syntax(), ParseOpts) of
                {ok, Store, UnknownFields} ->
                    {ok, Store, UnknownFields};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.


%% @private
store_syntax() ->
    Base = nkfile_util:store_syntax(),
    Base#{
        config := #{
            aws_id => binary,
            aws_secret => binary,
            bucket => binary,
            host => binary,
            port => integer,
            scheme => atom,
            bucket_access => atom,
            '__mandatory' => [host, port, scheme, bucket, aws_id, aws_secret],
            '__defaults' => #{
                bucket_access => virtual_hosted
            }
        }
    }.

%% @private
get_config(#{config:=Config}) ->
    #{ bucket := Bucket, 
       aws_id := AccessKey , 
       aws_secret := SecretKey,
       host := Host,
       port := Port, 
       scheme := Scheme,
       bucket_access := BucketAccess } = Config,
    Endpoint = endpoint(Scheme, Host, Port),
    { to_list(Bucket), mini_s3:new(AccessKey, SecretKey, Endpoint, BucketAccess) }.

endpoint(Scheme, Host, Port) ->
    string:join([
                 to_list(Scheme),
                 "://",
                 to_list(Host),
                 ":",
                 to_list(Port)]),


%% @private
to_list(Term) -> nklib_util:to_list(Term).
