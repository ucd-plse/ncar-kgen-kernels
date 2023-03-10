#!/bin/bash
#PBS -N MG2_sub_job
#PBS -A UCDV0023
#PBS -l walltime=0:01:00
#PBS -q regular
#PBS -j oe
#PBS -k eod
#PBS -l select=1:ncpus=36:mem=45GB
#PBS -l inception=login

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

timeout 30 ./kernel.exe
status=$?
if [ $status -eq 124 ]; then
    touch ./execution_timeout
elif [ $status -eq 0 ]; then
    touch ./execution_success
else
    touch ./execution_fail
fi