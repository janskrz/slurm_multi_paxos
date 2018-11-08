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
BENCH_DURATION=1 # in minutes
PAXOS_NODE_NUM=3

# TODO: proper data management for log collection and paxos log etc
export WD="$HOME/multi_paxos/run"
export HOSTSFILE="$WD/paxos_nodelist-$SLURM_JOB_ID.lock"

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


    #TODO...

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

log(){
    local level=$1
    local message=$2
    printf "%s %s\n" "$(tag $level)" "$message"
}

tag(){
    local level=$1
    printf "[bbench] %s  [%s]" "$(date +%H:%M:%S)" "$level"
}


main
