%%% Some wrapper for riak_ensemble

-module(multi_paxos_console).

-export([join/1,
         create/1,
         ensemble_status/1]).

-export([read/1, inc/1, inc/2]).

-define(ENSEMBLE, root).
-define(TIMEOUT, 600000).
-define(ENSEMBLE_KEY, 11234). % should be the same as used by basho_bench

read(Key) ->
    {Val, _Payload} = riak_ensemble_client:kget(?ENSEMBLE, Key, ?TIMEOUT),
    Val.

inc(Key, Payload) -> update(Key, Payload, fun update_inc/2, {0, <<>>}).

inc(Key) -> update(Key, fun update_inc/1, {0, <<>>}).

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
    wait_stable(),

    {ok, PayloadSize} = application:get_env(riak_ensemble, payload_size),
    InitPayload = crypto:strong_rand_bytes(PayloadSize),
    InitValue = {0, InitPayload},
    _ = riak_ensemble_client:kput_once(node(), ?ENSEMBLE, ?ENSEMBLE_KEY, InitValue, ?TIMEOUT),
    ok.

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

update(Key, NewPayload, UpdateFun, DefaultVal) ->
    {ok, _} = riak_ensemble_peer:kmodify(node(), ?ENSEMBLE, Key,
                                fun({_Epoch, _Seq}, CurVal) ->
                                        UpdateFun(CurVal, NewPayload)
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
update_inc({X, Y}) -> {X + 1, Y}.

update_inc({X, _Y}, Payload) -> {X + 1, Payload}.
