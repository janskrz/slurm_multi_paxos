%%% Some wrapper for riak_ensemble

-module(multi_paxos_console).

-export([join/1,
         create/1,
         ensemble_status/1]).

-export([read/1, write/3]).

-define(ENSEMBLE, root).

read(Key) ->
    riak_ensemble_client:kget(node(), ?ENSEMBLE, Key, _Timeout=10000).

write(Key, UpdateFun, DefaultVal) ->
    io:format("~p ~p ~p", [Key, UpdateFun, DefaultVal]),
    {ok, _} = riak_ensemble_peer:kmodify(node(), ?ENSEMBLE, Key,
                                fun({_Epoch, _Seq}, CurVal) ->
                                        UpdateFun(CurVal)
                                end, DefaultVal,_Timeout=10000),
    ok.


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
            LeaderNode = node(riak_ensemble_manager:get_leader_pid(root)),
            io:format("Leader: ~p~n",[LeaderNode])
    end.

%% Internal functions

wait_stable() ->
    case check_stable() of
        true ->
            ok;
        false ->
            wait_stable()
    end.

check_stable() ->
    case riak_ensemble_manager:check_quorum(root, 1000) of
        true ->
            case riak_ensemble_peer:stable_views(root, 1000) of
                {ok, true} ->
                    true;
                _ ->
                    false
            end;
        false ->
            false
end.
