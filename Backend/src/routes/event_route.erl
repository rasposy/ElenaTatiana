-module(event_route).

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

known_methods(Req, State) ->
    Result = [<<"GET">>, <<"POST">>],
    {Result, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

% response on a http get request
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_response}],
     Req,
     State}.

% response on a http post request
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, post_response}],
     Req,
     State}.

get_response(Req0, State0) ->
    QsVals = cowboy_req:parse_qs(Req0),
    case lists:keyfind(<<"Number">>, 1, QsVals) of
        {_, undefined} -> Message = {[{response, <<"Hello">>}]};
        {_, Number} ->
            Message = io:format("GET /prime Number=~p ~n", [Number]),
            {[{response, server:get(Number)}]}
    end,
    {jiffy:encode(Message), Req0, State0}.

post_response(Req0, _State0) ->
    {ok, EncodedData, _} = cowboy_req:read_body(Req0),
    DecodedData = jiffy:decode(EncodedData),
    % we want only the content of the json data
    Decode_array = decoded_data(DecodedData),

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

response_to_post(Req0, State) ->
    
    

    % Filter out if DecodedData is "acceptable", TODO: add real control and different HTTP code (204, 206) ?
    % if yes, use Erlang backend logic to analyze it.
    case Decoded_current_grid_as_integers of
        [_,_,_,_,_,_,_,_,_] ->
            io:fwrite("[gameserver_route.erl] Analyze any list of length nine...~n", []),
            % a good format response to analyze
            {R, Code} = gameserver_process:analyze_post_request(Decoded_current_grid_as_integers),
            % io:fwrite("[gameserver_route.erl] gameserver_process analyzed the DecodedData, its answer is: ~p with code ~p.~n", [R, Code]),
            Reply = {resp, R}
    end,

    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).

decoded_data(DecodedData) ->
    % DecodedData = {[{<<.>>, <<.>>}]} = a tuple {} containing a list [] containg a tuple {} with two bit strings <<>>.

    DecodedArray = element(1, DecodedData), % DecodedData_elem1 = [{<<.>>, <<.>>}]
    [DecodedData_elem1_head | _DecodedData_elem1_body] = DecodedData_elem1, % DecodedData_elem1_head = {<<.>>, <<.>>}
    Decoded_current_grid = element(2, DecodedData_elem1_head), % Decoded_current_grid = <<.>>
    Decoded_current_grid
    