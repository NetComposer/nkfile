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
-export([upload/4, download/3, parse_store/2, store_syntax/0]).

-include("nkfile.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

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
        Res = erlcloud_s3:put_object(Bucket, to_list(Name), Body, [], AwsConfig),
        lager:debug("S3 upload: ~p/~p => ~p", [Bucket, to_list(Name), Res]),
        Meta = maps:get(meta, File, #{}),
        {ok, File#{meta=>Meta#{aws_res=>Res}}}
    catch
        error:Error ->
            {error, {s3_error, nklib_util:to_binary(Error)}}
    end.


%% @doc
download(_SrvId, Store, #{name:=Name}=File) ->
    {Bucket, AwsConfig} = get_config(Store),
    try
        Res = erlcloud_s3:get_object(Bucket, to_list(Name), [], AwsConfig),
        Body = nklib_util:get_value(content, Res),
        {ok, File, Body}
    catch
        error:Error ->
            {error, {s3_error, nklib_util:to_binary(Error)}}
    end.



%% @doc
parse_store(Data, ParseOpts) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class:=s3}, _} ->
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
            bucket => binary,
            aws_id => binary,
            aws_secret => binary,
            host => binary,
            port => integer,
            bucket_access => atom,
            bucket_after_host => atom,
            scheme => binary,
            '__mandatory' => [bucket, 
                              aws_id, 
                              aws_secret, 
                              host, 
                              port,
                              scheme,
                              bucket_access]

            },
            '__defaults' => #{
              bucket_after_host => false
             }
        }.

%% @private
get_config(#{config:=Config}) ->
    #{ bucket:=Bucket, 
       aws_id:=AwsId, 
       aws_secret:=AwsSecret, 
       host := Host,
       port := Port,
       scheme := Scheme, 
       bucket_access := BucketAccess,
       bucket_after_host := BucketAfterHost } = Config,

    AwsConfig = #aws_config{
        access_key_id = to_list(AwsId),
        secret_access_key = to_list(AwsSecret),
        s3_follow_redirect = true,
        s3_bucket_access_method = BucketAccess,
        s3_bucket_after_host = BucketAfterHost,
        s3_scheme =to_list(Scheme),
        s3_host = to_list(Host),
        s3_port = Port
    },

    {to_list(Bucket), AwsConfig}.

%% @private
to_list(Term) -> nklib_util:to_list(Term).
