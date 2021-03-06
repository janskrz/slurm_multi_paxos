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
    DataRoot = os:getenv("ENSEMBLE_DATA_ROOT", "./data"),
    PayloadSize = os:getenv("ENSEMBLE_COUNTER_PAYLOAD", "0"),
    NodeDataDir = filename:join(DataRoot, atom_to_list(node())),

    %% TODO: configurable?
    application:load(riak_ensemble),
    application:set_env(riak_ensemble, payload_size, list_to_integer(PayloadSize)),
    application:set_env(riak_ensemble, storage_delay, 10),
    application:set_env(riak_ensemble, storage_tick, 100),
    Ensemble = {riak_ensemble_sup,
                {riak_ensemble_sup, start_link, [NodeDataDir]},
                permanent, 20000, supervisor, [riak_ensemble_sup]},

    {ok, { {one_for_all, 0, 1}, [Ensemble]} }.

%%====================================================================
%% Internal functions
%%====================================================================
