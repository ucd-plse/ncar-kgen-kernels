#!/bin/bash
#PBS -N MG2_master_job
#PBS -A UCDV0023
#PBS -l walltime=01:00:00
#PBS -q regular
#PBS -j oe
#PBS -k eod
#PBS -l select=1:ncpus=36:mem=45GB

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

source ${PROSE_REPO_PATH}/scripts/cheyenne/activate_PROSE_environment.sh
module load intel/2022.1

make reset -f Makefile.rose
python3 ${PROSE_REPO_PATH}/scripts/prose_preprocess_project.py

### Run the executable
python3 ${PROSE_REPO_PATH}/scripts/prose_search.py -s setup.ini