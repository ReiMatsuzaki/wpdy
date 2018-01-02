include ../local.mk

MODS0=const err_handle math istream fjson timer sys
BUILD=${WPDY_PATH}/build
EXAMPLE=${WPDY_PATH}/example

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
	${FC} ${FF} $^ -o $@ -cpp  -llapack -lblas
check_wpdy: ${BUILD}/utest_wpdy
	${BUILD}/utest_wpdy

# ==== main ====
TARGET=wpdy
MODULES=${MODS0} timer fft4g fftsg2d fft wpdy timestep main
OBJS=$(addsuffix .o, ${MODULES})
${BUILD}/${TARGET}: ${OBJS}
	${FC} ${FF} $^ -o $@ -cpp  -llapack -lblas
check: ${BUILD}/${TARGET}
	cd ${EXAMPLE}/harm; python3 run.py



