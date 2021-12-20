-module(event_route).

-behaviour(cowboy_rest).

-export([init/2]).

-export([known_methods/2]).

-export([allowed_methods/2]).

-export([content_types_provided/2]).

-export([content_types_accepted/2]).

-export([get_response/2]).

-export([post_response/2]).

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
    % we need to decode to get only the content of the json data
    Decoded_array = decoded_data(DecodedData),
    % we get the DecodedData in the following form:
    % {[{<<.>>, <<.>>}]} = a tuple {} containing a list [] containing a tuple {} with two bit strings <<>>.

    case Decoded_array of
        [] ->
            {Reply, Code} = server_process:analyze_post_request(Decoded_array),
            Reply = {response, R}
    end,

    EncodedReply = jiffy:encode({[Reply]}),
    cowboy_req:reply(Code,
                     #{<<"content-type">> => <<"application/json">>},
                     EncodedReply,
                     Req0).

%% decoded_data(DecodedData) ->
%%     % DecodedData_elem1 = [{<<.>>, <<.>>}]
%%     DecodedData_number = element(1, DecodedData), 
%%     % DecodedData_elem1_head = {<<.>>, <<.>>}
%%     [DecodedData_number_head | _DecodedData_number_body] = DecodedData_number, 
%%     % Decoded_array = <<.>>
%%     Decoded_array = element(2, DecodedData_number_head), 
%%     Decoded_array
    