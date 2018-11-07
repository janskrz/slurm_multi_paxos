#!/bin/bash
export WD="run/"

# TODO construct propper nodelist
export SLURM_JOB_NUM_NODES=3
export SLURM_JOB_NODELIST="'node1@127.0.0.1' 'node2@127.0.0.1' 'node3@127.0.0.1'"
NODELIST=($SLURM_JOB_NODELIST)
export HOSTLIST=$(IFS=,; echo "${NODELIST[*]}")

main(){
    # basho-bench write config
    mkdir -p $WD
    write_basho_config


    # build releases and startup ensemble

    declare -a RELEASE_NAMES
    declare -a RELEASE_PATHS

    rebar3 clean

    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        RELEASE_NAMES[$i]=$(cut -d '@' -f1 <<< ${NODELIST[$i]} | tr -d \')
        RELEASE_PATHS[$i]="_build/${RELEASE_NAMES[$i]}/rel/multi_paxos/bin/multi_paxos"

        echo "Building node ${RELEASE_NAMES[$i]}"
        rebar3 as ${RELEASE_NAMES[$i]} release

        echo "Starting node ${RELEASE_NAMES[$i]}"
        ${RELEASE_PATHS[$i]} start
    done

    echo "waiting a bit..."
    sleep 2

    echo "Using node ${RELEASE_NAMES[0]} as leader"
    ${RELEASE_PATHS[0]} rpc multi_paxos_console create

    echo "Building ensemble"
    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        echo "Pinging node ${RELEASE_NAMES[$i]}"
        ${RELEASE_PATHS[$i]} ping

        for j in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
        do
            if [ "$j" -ne "$i" ]; then
                echo "Trying to connect ${RELEASE_NAMES[$i]} to ${NODELIST[$j]}"
                ${RELEASE_PATHS[$i]} rpc multi_paxos_console join ${NODELIST[$j]}
            fi
        done
    done


    echo "Checking ensemble status"
    for i in `seq 0 $((SLURM_JOB_NUM_NODES-1))`;
    do
        echo "${RELEASE_NAMES[$i]}:"
        ${RELEASE_PATHS[$i]} rpc multi_paxos_console ensemble_status
    done
}

write_basho_config() {
    local PARALLEL_ID=1
    local max_key=$((10**17))
    local config=${WD}/lg${PARALLEL_ID}.config
    cat >  $config <<EOF
{mode, max}.
{duration, 1}.
{concurrent, 1}.
{operations, [{put,1}, {get, 0}]}.
{driver, basho_bench_driver_multi_paxos}.
{key_generator, {int_to_str, {uniform_int, $max_key}}}.
%% size in Bytes
{value_generator, {fixed_bin, 8}}.
{multi_paxos_client_mynode, ['benchclient${PARALLEL_ID}']}.
{multi_paxos_client_cookie, 'erlang'}.

{report_interval, 1}.
{log_level, info}.

{multi_paxos_client_nodes, [$HOSTLIST]}.
EOF
}

main
