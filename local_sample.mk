WPDY_PATH=${HOME}/src/wpdy
## -- gfortran options --
FC=gfortran
FF=-Wall -pedantic -fbounds-check -O -Wuninitialized -fbacktrace -g -cpp -ffree-line-length-512

## -- ifort options --
#FC=ifort
#FF=-O0 -g -traceback -check all -check bounds -check uninit -debug all -fpp
