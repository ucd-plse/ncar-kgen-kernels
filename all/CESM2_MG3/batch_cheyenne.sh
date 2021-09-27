#!/usr/bin/env bash

#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=72:mpiprocs=72
#PBS -l walltime=08:00:00
#PBS -q economy
#PBS -j oe
#PBS -k eod

ulimit -s unlimited

# unload any modules currently loaded
module purge

# load modules
module load ncarenv/1.3
module load intel/19.1.1
module load mpt/2.22
module load ncarcompilers/0.5.0

cd $PBS_O_WORKDIR
let pcol=384   # pcols value for input data
let ntask=36   # ntasks value for input data
let nrank=72   # number of mpi ranks for kernel run 

# add a loop to compile the code with different dfact
for i in 8
do
    # compile the code
    make clean
    make ntasks=$ntask pcols=$pcol dfact=$i

    # run the code
    mpiexec_mpt ./kernel.exe >& cheyenne_intel_mpiranks${nrank}_pcols${pcol}_dfact${i}_log

    # clean the files
    make clean
done
