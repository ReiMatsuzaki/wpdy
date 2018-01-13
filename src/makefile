include ../local.mk

MODS0=const err_handle math istream fjson timer sys timer strutil argparser
OPT=0
NTH=1  # number of threads
ARCH=debug
BUILD_BASE=${WPDY_PATH}/build
BUILD=${BUILD_BASE}/${ARCH}
SRC=${WPDY_PATH}/src
EXAMPLE=${WPDY_PATH}/example

ifeq (${FC},ifort)
	MKLLIB=${MKL}/lib/intel64
	INCLUDE=-I${MKL}/include
	LIBS=${MKLLIB}/libmkl_lapack95_lp64.a -Wl,--start-group ${MKLLIB}/libmkl_intel_lp64.a ${MKLLIB}/libmkl_core.a ${MKLLIB}/libmkl_intel_thread.a  -Wl,--end-group -lpthread -lm -ldl
	FF=-qopenmp -fpp -free -xHost
ifeq (${ARCH},debug)
	FF+=-O0
endif
ifeq (${ARCH},fast)
	FF+=-O3
endif
endif

ifeq (${FC},gfortran)
	FF=-Wall -pedantic -fbounds-check -O -Wuninitialized -fbacktrace -g -cpp -ffree-line-length-512
	LDFLAGS=-llapack -lblas
endif

# -- compile --
${BUILD}/%.x:
	@if [ ! -d ${BUILD} ]; \
	   then mkdir -p ${BUILD}; \
	    fi
	${FC} ${FF} $^ -o $@ -cpp ${LIBS} ${LDFLAGS}

${BUILD}/%.o: ${SRC}/%.f90
	@if [ ! -d ${BUILD} ]; \
	   then mkdir -p ${BUILD}; \
	    fi
	${FC} ${FF} ${INCLUDE} -c $< -o $@

${BUILD}/fft4g.o:
	${FC} ../external/fft/fft4g.f -c -o $@

${BUILD}/fftsg2d.o:
	${FC} ../external/fft2d/fftsg2d.f -c -o $@


# -- Clean --
clean:
	rm -rf ${BUILD_BASE}
	rm -f *.mod

# -- utility function
mod2obj = $(addprefix ${BUILD}/, $(addsuffix .o, $(1)))

# ==== utest ====
${BUILD}/utest_wpdy.x: $(call mod2obj, ${MODS0} utest fft4g fftsg2d fft wpdy utest_wpdy)
check_wpdy: ${BUILD}/utest_wpdy.x
	${BUILD}/utest_wpdy.x

# ==== utest (DVR) ====
# to know ulimit command, see https://www.isus.jp/products/fortran-compilers/sigsegv-or-sigbus-errors/
${BUILD}/utest_dvr.x: $(call mod2obj, ${MODS0} utest dvr elnuc utest_dvr)
utest_dvr.x: ${BUILD}/utest_dvr.x
check_dvr: ${BUILD}/utest_dvr.x
	export OMP_NUM_THREADS=${NTH}; ulimit -s unlimited; ${BUILD}/utest_dvr.x

# ==== main ====
${BUILD}/wpdy.x: $(call mod2obj, ${MODS0} fft4g fftsg2d fft wpdy timestep main)
wpdy.x: ${BUILD}/wpdy.x
check: ${BUILD}/wpdy.x
	cd ${EXAMPLE}/harm; python3 run.py

# ==== main (DVR) ====
${BUILD}/wpdy_dvr.x: $(call mod2obj, ${MODS0} dvr elnuc main_dvr)
wpdy_dvr.x: ${BUILD}/wpdy_dvr.x
check_wpdy_dvr: ${BUILD}/wpdy_dvr.x
	rm -rf ${BUILD_BASE}/wpdy_dvr
	ln -s ${BUILD}/wpdy_dvr.x ${BUILD_BASE}/wpdy_dvr
	export OMP_NUM_THREADS=${NTH}; ulimit -s unlimited; cd ${EXAMPLE}/tully2_dvr; python3 run.py

# ==== time check (DVR) ====
${BUILD}/time_dvr.x: $(call mod2obj, ${MODS0} utest dvr elnuc time_dvr)
time_dvr.x: ${BUILD}/time_dvr.x
check_time_dvr: ${BUILD}/time_dvr.x
	export OMP_NUM_THREADS=${NTH}; ulimit -s unlimited; $<


