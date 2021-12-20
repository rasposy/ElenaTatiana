%%%-------------------------------------------------------------------
%% @doc erlang_project_skeleton top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlang_project_skeleton_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()}.

%% [] sucht die nächste Methode mit dem Input aus
start_link() ->
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%% supervisor.

init([]) ->
    % permanent = wenn crash dann neustarten, it's a must
    
    Procs = [{server, {server, start, []}, permanent, 1, worker, [server]}, 
             {fermat, {fermat, start, []}, permanent, 1, worker, [fermat]}],
    % one_for_one = crash process wird neu gestartet
    %10 tries in 10sec
    {ok, {{one_for_one, 10, 10}, Procs}}.

%% internal functions
