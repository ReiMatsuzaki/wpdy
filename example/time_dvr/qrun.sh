#!/bin/bash 
#PBS -j oe
#PBS -V
#PBS -o qrun.log
#PBS -N check_time
#PBS -l nodes=athena49.cluster:ppn=12
cd $PBS_O_WORKDIR

ulimit -s unlimited
export OMP_NUM_THREADS=1
./utest_dvr > res1.log

export OMP_NUM_THREADS=12
./utest_dvr > res12.log


