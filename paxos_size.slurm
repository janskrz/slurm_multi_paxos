#!/bin/bash -l

# This slurm batch script performs the following steps:
# 1. Based on PAXOS_NODE_NUM start another slurm job
# 2. This job creates sets up the riak ensamble (i.e. the multi-paxos cluster)
# 4. Once ensamble is done start basho-bench

#SBATCH -J load_gen_basho
#SBATCH -N 2
#SBATCH -t 3-10:00:00
#SBATCH --nodelist=cumu02-[00-01]
#SBATCH -p CUMU2
#SBATCH --exclusive
#SBATCH -o run/bench_log.out

## basho bench cannot handle erlang versions newer than 19
module purge
module load erlang/19.3

# set these as needed
NAME="rbrpaper_multi_paxos_size_counter_direct_leader-te"
REPEATS=1
BENCH_DURATION=10 # in minutes
PAXOS_NODE_NUM=3

LOAD="256"
SIZE_SERIES="8192"
OP_SERIES="[{put,1},{get,9}]"

# basho_bench_driver_multi_paxos -> command and data size increases
# bash_bench_driver_multi_paxos_fixed_cmd -> only data size increases
DRIVER="basho_bench_driver_multi_paxos"

# location of cmd log written by ensemble nodes
# WARNING: will be cleared !!!!
export ENSEMBLE_DATA_ROOT="/dev/shm/$USER"

# location of writing benchmark data,logs etc.
export WD_PREFIX="$HOME/consensus/benchmark/multi_paxos/$NAME"

# file used for job communication and progress
export HOSTSFILE="$WD_PREFIX/paxos_nodelist-$SLURM_JOB_ID.lock"

# location of basho bench
BASHO_BENCH_DIR="$HOME/basho_bench"

LOAD_GENERATORS=$SLURM_JOB_NUM_NODES
LG_HOSTLIST=`scontrol show hostnames`
LG_HOSTLIST=($LG_HOSTLIST)

##### script starts here
main(){

    WORKERS=$LOAD
    for SIZE in $SIZE_SERIES; do
        local count=0
        for OPS in $OP_SERIES; do
            let "count++"
            log info "Number of LGs: $LOAD_GENERATORS"
            log info "LG hostlist: ${LG_HOSTLIST[*]}"
            log info "Size of counter payload: $SIZE"
            log info "Workers per LG: $WORKERS"
            log info "Operations: $OPS"
            export ENSEMBLE_COUNTER_PAYLOAD=$SIZE
            for R in $(seq 1 $REPITITIONS); do
                log info "Repitition: $R"
                BENCH_NAME=$(printf "size%05i-$count-r$R" $((SIZE)))
                export WD="$WD_PREFIX/$BENCH_NAME"

                execute_benchmark
            done
        done
    done

    exit 0
}


execute_benchmark(){
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

    log info "Starting benchmark"
    declare -a lg_pids
    local c
    for i in $(seq 1 $LOAD_GENERATORS); do
        PARALLEL_ID=$i
        RANDOM_SEED="{$((7*$i)), $((11*$i)), $((5*$i))}"
        CONFIG_FILE=${WD}/lg${i}.config
        write_basho_config

        local arg1=${BASHO_BENCH_DIR:+"--bbdir=$BASHO_BENCH_DIR"}
        local arg2=${NAME:+"--name=$NAME"}
        local arg3=${ENSEMBLE_SLURM_JOB_ID:+"--jobid=$ENSEMBLE_SLURM_JOB_ID"}
        local arg4=${CONFIG_FILE:+"--config=$CONFIG_FILE"}
        local arg5=${PARALLEL_ID:+"--parallel_id=$PARALLEL_ID"}
        local arg6=${WD:+"--wd=$WD"}
        local arg7=${WD:+"--rdir=$WD"}
        declare -a args=($arg1 $arg2 $arg3 $arg4 $arg5 $arg6 $arg7)

        host=${LG_HOSTLIST[$((c++))]}
        echo "Starting LG on $host (Total nummber of LG hosts $LOAD_GENERATORS)"
        if [[ $(hostname) = $host ]]; then
            slurm/start_basho_bench.sh ${args[@]} &
            lg_pids[$i]=$!
        else
            ARGSTRING=$(printf '%s ' "${args[@]}")
            echo $ARGSTRING
            echo $host
            srun --nodelist=$host -N1 bash -c "$(pwd)/slurm/start_basho_bench.sh $ARGSTRING" &
            lg_pids[$i]=$!
        fi

    done

    # wait for load generators to finish
    for pid in "${lg_pids[@]}"; do
        wait $pid
    done

    # remove hostfile so that ensemble management job can tear down the nodes
    rm_hostsfile

    # wait until slurm job completed which means the ensemble was shut down
    while [[ $(squeue | grep $ENSEMBLE_SLURM_JOB_ID | wc -l) > 0 ]]; do
        sleep 1
    done
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
{rng_seed, $RANDOM_SEED}.
{mode, max}.
{duration, $BENCH_DURATION}.
{concurrent, $WORKERS}.
{operations, $OPS}.
{driver, $DRIVER}.
{key_generator, {int_to_str, {uniform_int, $max_key}}}.
%% size in Bytes
{value_generator, {fixed_bin, $ENSEMBLE_COUNTER_PAYLOAD}}.

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
