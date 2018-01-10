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

# ==== utest (DVR) ====
TARGET=utest_dvr
MODULES=${MODS0} timer utest dvr elnuc ${TARGET}
OBJS=$(addsuffix .o, ${MODULES})
${BUILD}/utest_dvr: ${OBJS}
	${FC} ${FF} $^ -o $@ -cpp  -llapack -lblas
check_dvr: ${BUILD}/utest_dvr
	${BUILD}/utest_dvr

# ==== main ====
TARGET=wpdy
MODULES=${MODS0} timer fft4g fftsg2d fft wpdy timestep main
OBJS=$(addsuffix .o, ${MODULES})
${BUILD}/${TARGET}: ${OBJS}
	${FC} ${FF} $^ -o $@ -cpp  -llapack -lblas
check: ${BUILD}/${TARGET}
	cd ${EXAMPLE}/harm; python3 run.py

# ==== main (DVR) ====
TARGET=wpdy_dvr
MODULES=${MODS0} strutil argparser timer dvr elnuc main_dvr
OBJS=$(addsuffix .o, ${MODULES})
${TARGET}: ${OBJS}
	${FC} ${FF} $^ -o ${BUILD}/${TARGET} -cpp  -llapack -lblas
check_main_dvr: ${TARGET}
	cd ${EXAMPLE}/harm_dvr; python run.py


