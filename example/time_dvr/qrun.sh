#!/bin/bash 
#PBS -j oe
#PBS -V
#PBS -o qrun.log
#PBS -N check_time
#PBS -l nodes=athena49.cluster:ppn=12
cd $PBS_O_WORKDIR


#UTEST=../../build/debug/time_dvr.x
#ulimit -s unlimited
#export OMP_NUM_THREADS=1
#${UTEST} > res1.log
#export OMP_NUM_THREADS=12
#${UTEST} > res12.log

UTEST=../../build/fast/time_dvr.x
export OMP_NUM_THREADS=1
${UTEST} > res1fast.log
export OMP_NUM_THREADS=12
${UTEST} > res12fast.log



