#!/bin/bash
SLURM_JOB_NUM_NODES=3

for i in `seq 1 $SLURM_JOB_NUM_NODES`;
do
    RELEASE_NAME="node$i"
    RELEASE_PATH="_build/$RELEASE_NAME/rel/multi_paxos/bin/multi_paxos"
    $RELEASE_PATH stop
done

