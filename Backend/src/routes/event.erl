-module(event).

-behaviour(cowboy_rest).

-export([init/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([content_types_accepted/2]).

-export([known_methods/2]).

%% Callback Callbacks
-export([get_prime/2]).

-export([post_prime/2]). % init(Req0, Opts) ->

init(Req0, State) -> {cowboy_rest, Req0, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_prime}],
     Req,
     State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, post_prime}],
     Req,
     State}.

get_prime(Req0, State0) ->
    QsVals = cowboy_req:parse_qs(Req0),
    case lists:keyfind(<<"Number">>, 1, QsVals) of
        {_, undefined} -> Message = {[{response, <<"Hello">>}]};
        {_, Number} ->
            Message = io:format("GET /prime Number=~p ~n", [Number]),
            {[{response, server:get(Number)}]}
    end,
    {jiffy:encode(Message), Req0, State0}.

post_prime(Req0, _State0) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),

    case DecodedData of
        {[{<<"Number">>, undefined}, {<<"prime">>, undefined}]} ->
            {Reply, Code} = {{response, <<"undefined attributed">>},
                             204};
        {[{<<"Number">>, undefined}, {<<"prime">>, _}]} ->
            {Reply, Code} = {{response, <<"undefined Number">>}, 206};
        {[{<<"Number">>, _}, {<<"prime">>, undefined}]} ->
            {Reply, Code} = {{response, <<"undefined prime">>}, 206};
        {[{<<"Number">>, Number}, {<<"prime">>, prime}]} ->
            {R, Code} = server:post(Number, prime),
            io:format("POST /prime Number=~p prime=~p~n", [Number, prime]),
            Reply = {response, R}
    end,
    EncodedReply = jiffy:encode({[Reply]}),

    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.