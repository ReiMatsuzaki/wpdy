include ../local.mk

MODS0=const

# -- compile --
%.o: %.f90
	${FC} ${FF} -c $< -o $@

# -- Clean --
clean:
	rm -f *.o
	rm -f *.mod

# ==== utest ====
TARGET=utest_wpdy
MODULES=${MODS0} utest wpdy ${TARGET}
OBJS=$(addsuffix .o, ${MODULES})
${BUILD}/utest_wpdy: ${OBJS}
	${FC} ${FF} $^ -o $@ -cpp  -llapack -lblas
check_wpdy: ${BUILD}/utest_wpdy
	${BUILD}/utest_wpdy


