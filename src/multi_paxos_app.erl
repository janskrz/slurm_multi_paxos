%%%-------------------------------------------------------------------
%% @doc multi_paxos public API
%% @end
%%%-------------------------------------------------------------------

-module(multi_paxos_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    multi_paxos_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
