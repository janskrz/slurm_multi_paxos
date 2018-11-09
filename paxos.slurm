#!/bin/bash -l

# This slurm batch script performs the following steps:
# 1. Based on PAXOS_NODE_NUM start another slurm job
# 2. This job creates sets up the riak ensamble (i.e. the multi-paxos cluster)
# 4. Once ensamble is done start basho-bench

# TODO: multiple configurations in a single run
# TODO(?): multiple load generator instances

#SBATCH -J load_gen_basho
#SBATCH -N 1
#SBATCH -t 10:00:00
#SBATCH --exclusive
#SBATCH -o run/bench_log.out

# set these as needed
NAME="testrun"
BENCH_DURATION=5 # in minutes
PAXOS_NODE_NUM=3

# location of cmd log written by ensemble nodes
# WARNING: will be cleared !!!!
export ENSEMBLE_DATA_ROOT="/dev/shm/$USER"

# location of writing benchmark data,logs etc.
export WD="$HOME/multi_paxos/run"
# file used for job communication and progress
export HOSTSFILE="$WD/paxos_nodelist-$SLURM_JOB_ID.lock"

# location of basho bench
BASHO_BENCH_DIR="$HOME/basho_bench"

##### script starts here
main(){
    # start batch
    RET=$( sbatch \
            ${SLURM_JOB_ACCOUNT:+-A $SLURM_JOB_ACCOUNT} \
            ${SLURM_JOB_PARTITION:+-p $SLURM_JOB_PARTITION} \
            ${PAXOS_NODE_NUM:+-N $PAXOS_NODE_NUM} \
            -J multi_paxos \
            $(pwd)/slurm/start_ensemble.slurm
        )

    # get the job id from the output of sbatch
    REGEX="Submitted batch job ([[:digit:]]*)"
    if [[ $RET =~ $REGEX ]]; then
        ENSEMBLE_SLURM_JOB_ID=${BASH_REMATCH[1]}
    else
        exit 1
    fi

    wait_for_ensemble_startup

    log info "Writing basho_bench config"
    HOSTLIST=$(head -n1 $HOSTSFILE)
    log info "Ensemble hostlist is $HOSTLIST"

    declare -a lg_pids
    PARALLEL_ID=1 # TODO: multiple load gens?
    RANDOM_SEED="{$((7*$PARALLEL_ID)), $((11*$PARALLEL_ID)), $((5*$PARALLEL_ID))}"
    CONFIG_FILE=${WD}/lg${PARALLEL_ID}.config
    write_basho_config

    log info "Starting benchmark"
    slurm/start_basho_bench.sh \
        "--bbdir=$BASHO_BENCH_DIR" \
        "--name=$NAME" \
        "--jobid=$ENSEMBLE_SLURM_JOB_ID" \
        "--config=$CONFIG_FILE" \
        "--parallel_id=$PARALLEL_ID" \
        "--wd=$WD" \
        "--rdir=$WD" &
    lg_pids[$i]=$!

    # wait for load generators to finish
    for pid in "${lg_pids[@]}"; do
        wait $pid
    done

    # remove hostfile so that ensemble management job can tear down the nodes
    rm_hostsfile

    exit 0
}

rm_hostsfile() {
    rm -f $HOSTSFILE
}

wait_for_ensemble_startup() {
    # after ensemble is started, it writes a file to tell this job where the nodes are

    echo -n "$(tag info) waiting for multi paxos ensemble to start"
    timer=0
    until [[ -e $HOSTSFILE ]]; do
        ((timer++))
        # display status every 5 seconds
        if ((timer%5==0)); then
            echo -ne "."
        fi
        sleep 1
    done
    echo ": ok (${timer}s)"
}


write_basho_config() {
    local max_key=$((10**17))
    local config=${WD}/lg${PARALLEL_ID}.config
    cat >  $config <<EOF
{mode, max}.
{duration, $BENCH_DURATION}.
{concurrent, 512}.
{operations, [{put,1}, {get, 9}]}.
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

log(){
    local level=$1
    local message=$2
    echo "%s %s\n" "$(tag $level)" "$message"
}

tag(){
    local level=$1
    printf "[bbench] %s  [%s]" "$(date +%H:%M:%S)" "$level"
}


main
