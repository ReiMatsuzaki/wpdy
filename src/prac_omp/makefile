MKL=/opt/intel/compilers_and_libraries_2017.1.132/linux/mkl
FF+=-free -xHost -I${MKL}/include -fpp
MKLLIB=${MKL}/lib/intel64
MATH=${MKLLIB}/libmkl_lapack95_lp64.a -Wl,--start-group ${MKLLIB}/libmkl_intel_lp64.a ${MKLLIB}/libmkl_core.a ${MKLLIB}/libmkl_intel_thread.a  -Wl,--end-group -lpthread -lm -ldl

all:
	ifort -qopenmp hello.f90 ${MATH}
