FC=gfortran
FF=-Wall -pedantic -fbounds-check -O -Wuninitialized -fbacktrace -g -cpp -ffree-line-length-512 -fopenmp
LDFLAGS=-llapack -lblas

%.o: %.f90
	${FC} ${FF} ${INCLUDE} -c $< -o $@
%.x: 
	${FC} ${LDFLAGS} ${FF} $^ -o $@



