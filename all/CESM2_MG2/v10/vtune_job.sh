#!/bin/bash
#PBS -N MG2_vtune_job
#PBS -A UCDV0023
#PBS -l walltime=0:05:00
#PBS -q regular
#PBS -j oe
#PBS -k eod
#PBS -l select=1:ncpus=36:mem=45GB
#PBS -l inception=login

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

vtune --collect=hotspots --result-dir=${result_dir} -- ./kernel.exe
vtune --report=gprof-cc --result-dir=${result_dir} --format=text --report-output=./${result_dir}/vtune_report.txt