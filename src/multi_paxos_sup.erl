%%%-------------------------------------------------------------------
%% @doc multi_paxos top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(multi_paxos_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    %erlang:set_cookie(node(), 'chocolate chip cookie'),
    DataRoot = application:get_env(riak_ensemble, data_root, "./data"),
    NodeDataDir = filename:join(DataRoot, atom_to_list(node())),

    Ensemble = {riak_ensemble_sup,
                {riak_ensemble_sup, start_link, [NodeDataDir]},
                permanent, 20000, supervisor, [riak_ensemble_sup]},

    {ok, { {one_for_all, 0, 1}, [Ensemble]} }.

%%====================================================================
%% Internal functions
%%====================================================================
