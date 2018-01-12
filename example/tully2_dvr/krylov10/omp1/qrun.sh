#!/bin/bash 
#PBS -j oe
#PBS -V
#PBS -o qrun.log
#PBS -N check_time
#PBS -l nodes=athena49.cluster:ppn=1
cd $PBS_O_WORKDIR

ulimit -s unlimited
export OMP_NUM_THREADS=1
python3 run.py > run.log


