-module(server).

-behaviour(gen_server).

%% API
-export([handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

-export([start/0]).

-export([get/1, gets/0, post/2]).

% These are all wrappers for calls to the Fermat
get(Number) -> gen_server:call(?MODULE, {get, Number}).

gets() -> gen_server:call(?MODULE, {get}).

post(Number, Prime) ->
    gen_server:call(?MODULE, {post, Number, Prime}).

start() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

% This is called when a connection is made to the Fermat
init([]) ->
    Fermat = dict:new(),
    {ok, Fermat}.

% handle_call is invoked in response to gen_server:call
handle_call({post, Number, Prime}, _From, Fermat) ->
    Response = case dict:is_key(Number, Fermat) of
                   true ->
                       NewFermat = Fermat,
                       {<<"It's not a prime">>, 409}; % http 409 conflict
                   false ->
                       NewFermat = dict:append(Number, Prime, Fermat),
                       {<<"It's a prime">>, 201} % http 201 created
               end,
    {reply, Response, NewFermat};
handle_call({get, Number}, _From, Fermat) ->
    Response = case dict:is_key(Number, Fermat) of
                   true ->
                       prime,
                       dict:fetch(Number, Fermat);
                   false -> <<"not found">>
               end,
    {reply, Response, Fermat};
handle_call({get}, _From, Fermat) ->
    Keys = dict:fetch_keys(Fermat),
    Response = [#{<<"Number">> => K,
                  <<"Prime">> => dict:fetch(K, Fermat)}
                || K <- Keys],
    {reply, Response, Fermat}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, Fermat) -> {noreply, Fermat}.

handle_info(_Message, Fermat) -> {noreply, Fermat}.

terminate(_Reason, Fermat) -> ok.
