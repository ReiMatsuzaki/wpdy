#include "macros.fpp"
#include "macros_utest.fpp"

module Mod_TestDVR
  use Mod_UtestCheck
  use Mod_ExpDVR
  implicit none
contains
  subroutine TestDVR_run
    call Utest_sub_begin("test_at")
    call test_at
    call Utest_sub_end()

    call Utest_sub_begin("test_dmat")
    call test_dmat
    call Utest_sub_end()

    call Utest_sub_begin("test_fit")
    call test_fit
    call Utest_sub_end()

    call Utest_sub_begin("test_eig")
    call test_eig
    call Utest_sub_end()

    call Utest_sub_begin("test_tint")
    call test_time_inte
    call Utest_sub_end()
    
  end subroutine TestDVR_run
  subroutine test_at
    double precision :: dx = 0.001d0
    double precision :: x  = 0.3d0
    integer, parameter :: n = 3
    complex(kind(0d0)) :: cs(2*n+1)
    complex(kind(0d0)) :: y0, yp, ym, y1, y2
    integer i
    do i = -n, n
       cs(i+n+1) = (n+i+1) * 0.1d0
    end do
    call ExpDVR_new(n, -4.0d0, 4.0d0)
    call ExpDVR_at_x(cs, x,    0, y0)
    call ExpDVR_at_x(cs, x+dx, 0, yp)
    call ExpDVR_at_x(cs, x-dx, 0, ym)
    call ExpDVR_at_x(cs, x,    1, y1)
    call ExpDVR_at_x(cs, x,    2, y2)
    call expect_near((yp-ym)/(2*dx),       y1, 1.0d-5)
    call expect_near((yp+ym-2*y0)/(dx*dx), y2, 1.0d-5)

    call ExpDVR_delete
    
  end subroutine Test_At
  subroutine test_dmat
    double precision :: x0 = -2.0d0
    double precision :: xN = +2.0d0
    integer, parameter :: n = 2
    integer, parameter :: num = 2*n+1
    complex(kind(0d0)) :: d1(num,num), d2(num,num)
    complex(kind(0d0)) :: zero = 0.0d0
    integer, parameter :: nx = 1000
    integer :: ix, i, j
    double precision :: xs(nx), dx
    complex(kind(0d0)) :: y0s(nx), y1s(nx), y2s(nx)
    complex(kind(0d0)) :: ci(num), cj(num)
    
    call ExpDVR_new(n, x0, xN)
    call ExpDVR_d1mat(d1)
    call ExpDVR_d2mat(d2)

    ! -- check basic property --
    call expect_eq(d1(1,2), -d1(2,1))
    call expect_eq(zero,   d1(1,1))
    call expect_eq(zero,   d1(2,2))
    call expect_eq(d2(1,2), d2(2,1))

    ! -- check value with numerical integration --
    dx = (xN-x0)/nx
    do ix = 1, nx
       xs(ix) = xs_(1) + (ix-1) * dx
    end do
    i = 2; ci(:) = 0; ci(i) = 1
    j = 4; cj(:) = 0; cj(j) = 1
    call ExpDVR_at_xs(ci, xs, 0, y0s)
    call ExpDVR_at_xs(cj, xs, 1, y1s)
    call ExpDVR_at_xs(cj, xs, 2, y2s)

    call expect_near(dot_product(conjg(y0s), y1s)*dx, d1(i,j), 1.0d-5)
    call expect_near(dot_product(conjg(y0s), y2s)*dx, d2(i,j), 1.0d-5)

    call ExpDVR_delete
    
  end subroutine test_dmat
  subroutine test_fit
    integer, parameter :: n = 12
    integer, parameter :: num = 2*n+1
    complex(kind(0d0)) :: gs(num), cs(num)
    double precision :: x
    complex(kind(0d0)) y0, y1
    
    call ExpDVR_new(n, -3.0d0, 3.0d0)
    gs(:) = 1.3d0*exp(-1.2d0*(xs_-0.1d0)**2)
    call ExpDVR_fit(gs(:), cs(:))

    x = 0.2d0
    y0 = 1.3d0*exp(-1.2d0*(x-0.1d0)**2)
    call ExpDVR_at_x(cs(:), x, 0, y1)
    call expect_near(y0, y1, 1.0d-6)

    call ExpDVR_delete
    
  end subroutine test_fit
  subroutine test_eig
    use Mod_math, only : lapack_zgeev
    double precision :: m = 2000.0d0
    double precision :: k = 0.5d0
    integer, parameter :: n = 30
    integer, parameter :: num = 2*n+1
    double precision :: w
    complex(kind(0d0)) :: D2(num,num), H(num,num), ws(num), UR(num,num), UL(num,num)
    double precision :: x
    complex(kind(0d0)) :: y0, y1
    integer a, aa(1)
    w = sqrt(k/m)

    call ExpDVR_new(n, -3.0d0, 3.0d0)

    call ExpDVR_d2mat(D2)
    H(:,:) = - 1/(2*m)*D2
    do a = 1, num
       H(a,a) = H(a,a) + k/2*xs_(a)*xs_(a)
    end do

    call lapack_zgeev(H, num, ws, UL, UR)
    call expect_near((1.0d0,0.0d0), dot_product(UL(:,1), UR(:,1)), 1.0d-10)
    call expect_near((1.0d0,0.0d0), dot_product(UL(:,2), UR(:,2)), 1.0d-10)
    
    aa = minloc(real(ws))
    a = aa(1)
    
    call expect_near(w*0.5d0, real(ws(a)), 1.0d-6)
    x = 0.2d0
    call ExpDVR_at_x(UR(:,a), x, 0, y0)
    y1 = sqrt(sqrt(sqrt(k*m)/pi)) * exp(-sqrt(k*m)*x*x/2)
    call expect_near(y0, y1, 1.0d-7)

    call ExpDVR_delete
    
  end subroutine test_eig
  subroutine test_time_inte
    use Mod_const, only : ii
    use Mod_math, only : lapack_zgeev
    use Mod_TimeInte
    double precision, parameter :: m = 1.0d0
    double precision, parameter :: w =1.0d0
    double precision, parameter :: a = m*w/2
    double precision :: x0 = 1.0d0
    double precision :: p0 = 0.0d0
    integer, parameter :: n = 128
    integer, parameter :: num = 2*n+1
    complex(kind(0d0)) :: dt = 1.0d0
    complex(kind(0d0)) :: g0(num), c(num)
    complex(kind(0d0)) :: D2(num,num), H(num,num), ws(num), UR(num,num), UL(num,num)
    integer i

    call ExpDVR_new(n, -5.0d0, 5.0d0)

    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), c(:))
    c(:) = c(:) / sqrt(sum(abs(c(:))**2))

    call ExpDVR_d2mat(D2)
    H(:,:) = - 1/(2*m)*D2
    do i = 1, num
       H(i,i) = H(i,i) + m*w*w/2*xs_(i)*xs_(i)
    end do

    call lapack_zgeev(H, num, ws, UL, UR)

    write(*,*) sum(abs(c(:))**2)
    do i = 1, 100
       call TimeInte_eig(ws(:), UR(:,:), conjg(transpose(UL(:,:))), dt, c(:))
    end do
    call expect_near(1.0d0, sum(abs(c(:))**2), 1.0d-8)

    call ExpDVR_delete
    
  end subroutine test_time_inte
end module Mod_TestDVR

program main
  use Mod_Utest
  use Mod_UtestCheck
  use Mod_TestDVR

  call utest_new

  call TestDVR_run

  !  call Timer_result
  call utest_delete
  
end program main
