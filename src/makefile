include ../local.mk

MODS0=const err_handle math istream fjson timer sys
OPT=0
BUILD=${WPDY_PATH}/build/${OPT}
EXAMPLE=${WPDY_PATH}/example

ifeq (${FC},ifort)
#	FF=-O0 -qopenmp -fpp -free -xHost -I${MKL}/include
	FF=-O3 -qopenmp -fpp -free -xHost -I${MKL}/include
	MKLLIB=${MKL}/lib/intel64
	LIBS=${MKLLIB}/libmkl_lapack95_lp64.a -Wl,--start-group ${MKLLIB}/libmkl_intel_lp64.a ${MKLLIB}/libmkl_core.a ${MKLLIB}/libmkl_intel_thread.a  -Wl,--end-group -lpthread -lm -ldl
endif

ifeq (${FC},gfortran)
	FF=-Wall -pedantic -fbounds-check -O -Wuninitialized -fbacktrace -g -cpp -ffree-line-length-512
	LIBS=-llapack -lblas
endif

# -- compile --
%.o: %.f90
	${FC} ${FF} -c $< -o $@

fft4g.o:
	${FC} ../external/fft/fft4g.f -c -o $@

fftsg2d.o:
	${FC} ../external/fft2d/fftsg2d.f -c -o $@


# -- Clean --
clean:
	rm -f *.o
	rm -f *.mod

# ==== utest ====
TARGET=utest_wpdy
MODULES=${MODS0} timer utest fft4g fftsg2d fft wpdy ${TARGET}
OBJS=$(addsuffix .o, ${MODULES})
${BUILD}/utest_wpdy: ${OBJS}
	${FC} ${FF} $^ -o $@ -cpp ${LIBS}
check_wpdy: ${BUILD}/utest_wpdy
	${BUILD}/utest_wpdy

# ==== utest (DVR) ====
# to know ulimit command, see https://www.isus.jp/products/fortran-compilers/sigsegv-or-sigbus-errors/
TARGET=utest_dvr
MODULES=${MODS0} timer utest dvr elnuc ${TARGET}
OBJS=$(addsuffix .o, ${MODULES})
utest_dvr: ${OBJS}
	${FC} ${FF} $^ -o ${BUILD}/$@ -cpp ${LIBS}
check_dvr: utest_dvr
	ulimit -s unlimited; ${BUILD}/utest_dvr

# ==== main ====
TARGET=wpdy
MODULES=${MODS0} timer fft4g fftsg2d fft wpdy timestep main
OBJS=$(addsuffix .o, ${MODULES})
${BUILD}/${TARGET}: ${OBJS}
	${FC} ${FF} $^ -o $@ -cpp  ${LIBS}
check: ${BUILD}/${TARGET}
	cd ${EXAMPLE}/harm; python3 run.py

# ==== main (DVR) ====
TARGET=wpdy_dvr
MODULES=${MODS0} strutil argparser timer dvr elnuc main_dvr
OBJS=$(addsuffix .o, ${MODULES})
${TARGET}: ${OBJS}
	${FC} ${FF} $^ -o ${BUILD}/${TARGET} -cpp  ${LIBS}
check_main_dvr: ${TARGET}
	cd ${EXAMPLE}/harm_dvr; python run.py


