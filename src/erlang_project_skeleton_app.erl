%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/health", health_route}],
                                        [{"/event", fermat_route}]}]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   middlewares =>
                                       [cowboy_router,
                                        ca_cowboy_middleware,
                                        cowboy_handler]}),
    %% populate the database with numbers
    server:post(<<"75654596987987976987">>, <<"Prime or not prime">>),
    server:post(<<"68756756757657656987">>, <<"Prime or not prime">>),
    server:post(<<"98789798798789796546">>, <<"Prime or not prime">>),
    server:post(<<"54654564217541236547">>, <<"Prime or not prime">>),
    server:post(<<"65421378512736521765">>, <<"Prime or not prime">>),                                    
    erlang_project_skeleton_sup:start_link().

stop(_State) -> ok = cowboy:stop_listener(http).

%% internal functions

