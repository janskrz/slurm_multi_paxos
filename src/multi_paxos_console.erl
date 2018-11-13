%%% Some wrapper for riak_ensemble

-module(multi_paxos_console).

-export([join/1,
         create/1,
         ensemble_status/1]).

-export([read/1, inc/1]).

-define(ENSEMBLE, root).
-define(TIMEOUT, 600000).

read(Key) -> riak_ensemble_client:kget(node(), ?ENSEMBLE, Key, ?TIMEOUT).

inc(Key) -> update(Key, fun update_inc/1, 0).

join([NodeStr]) ->
    Node = list_to_atom(NodeStr),
    case net_adm:ping(Node) of
        pang ->
            {error, not_reachable};
        pong ->
            riak_ensemble_manager:join(Node, node())
    end.

create([]) ->
    riak_ensemble_manager:enable(),
    wait_stable().

ensemble_status([]) ->
    cluster_status(),
    ok.

cluster_status() ->
    case riak_ensemble_manager:enabled() of
        false ->
            {error, not_enabled};
        true ->
            Nodes = lists:sort(riak_ensemble_manager:cluster()),
            io:format("Nodes in cluster: ~p~n",[Nodes]),
            LeaderNode = node(riak_ensemble_manager:get_leader_pid(?ENSEMBLE)),
            io:format("Leader: ~p~n",[LeaderNode])
    end.

%% Internal functions

update(Key, UpdateFun, DefaultVal) ->
    {ok, _} = riak_ensemble_peer:kmodify(node(), ?ENSEMBLE, Key,
                                fun({_Epoch, _Seq}, CurVal) ->
                                        UpdateFun(CurVal)
                                end, DefaultVal, ?TIMEOUT),
    ok.


wait_stable() ->
    case check_stable() of
        true ->
            ok;
        false ->
            wait_stable()
    end.

check_stable() ->
    case riak_ensemble_manager:check_quorum(?ENSEMBLE, ?TIMEOUT) of
        true ->
            case riak_ensemble_peer:stable_views(?ENSEMBLE, ?TIMEOUT) of
                {ok, true} ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
    end.


%% Value manipulation functions
update_inc(X) -> X + 1.
