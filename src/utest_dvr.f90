#include "macros.fpp"
#include "macros_utest.fpp"

module Mod_Harm
  use Mod_ExpDVR
  implicit none
  complex(kind(0d0)), allocatable :: H_(:,:)
contains
  subroutine Harm_new(m, w)
    ! assume ExpDVR is already allocated
    double precision, intent(in) :: m, w
    integer i
    allocate(H_(num_,num_))
    call ExpDVR_d2mat(H_)
    H_(:,:) = -1/(2*m) * H_(:,:)
    do i = 1, num_
       H_(i,i) = H_(i,i) + m*w*w/2 * xs_(i)**2
    end do
  end subroutine Harm_new
  subroutine Harm_delete
    deallocate(H_)
  end subroutine Harm_delete
  subroutine Harm_hc(c0, Hc0)
    complex(kind(0d0)), intent(in) :: c0(:)
    complex(kind(0d0)), intent(out) :: Hc0(:)

    Hc0(:) = matmul(H_(:,:), c0(:))
    
  end subroutine Harm_hc
end module Mod_Harm

module Mod_TestDVR
  use Mod_UtestCheck
  use Mod_ExpDVR
  implicit none
contains
  subroutine TestDVR_run
    !$ use omp_lib
    !$omp parallel
    if(omp_get_thread_num().eq.0) then
       write(*,*) "thread number:", omp_get_num_threads()
    end if
    !$omp end parallel
    
    call Utest_sub_begin("test_at")
    call test_at
    call Utest_sub_end()

    call Utest_sub_begin("test_dmat")
    call test_dmat
    call Utest_sub_end()

    call Utest_sub_begin("test_fit")
    call test_fit
    call Utest_sub_end

    call Utest_sub_begin("test_eig")
    call test_eig
    call Utest_sub_end

    call Utest_sub_begin("test_tinte_simple")
    call test_tinte_simple
    call Utest_sub_end

    call Utest_sub_begin("test_hc_time0")
    call test_hc_time(0)
    call Utest_sub_end

    call Utest_sub_begin("test_hc_time1")
    call test_hc_time(1)
    call Utest_sub_end

    call Utest_sub_begin("test_elnuc")
    call test_elnuc
    call Utest_sub_end

    call Utest_sub_begin("test_tinte_harm")
    call test_tinte_harm
    call Utest_sub_end
    
    call Utest_sub_begin("test_tint_2state")
    call test_tinte_2state
    call Utest_sub_end
    
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
  subroutine test_tinte_simple
    use Mod_const, only : ii
    use Mod_math, only : lapack_zgeev
    use Mod_TimeInteDiag
    use Mod_TimeInteKrylov
    use Mod_Harm
    double precision, parameter :: m = 1.0d0
    double precision, parameter :: w =1.0d0
    double precision, parameter :: a = m*w/2
    double precision :: x0 = 1.0d0
    double precision :: p0 = 0.0d0
    integer, parameter :: n = 128, nt=1
    integer, parameter :: num = 2*n+1
    complex(kind(0d0)) :: dt = 1.0d0
    complex(kind(0d0)) :: g0(num), c(num), c0(num), c1(num)
    integer i

    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call Harm_new(m, w)

    ! -- initial condition --
    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), c(:))    

    ! -- integration by diagonalize --
    c0(:) = c(:) / sqrt(sum(abs(c(:))**2))
    call TimeInteDiag_new(num_)
    call TimeInteDiag_precalc(h_(:,:))
    do i = 1, nt
       call TimeInteDiag_calc(dt, c0(:))
    end do
    call TimeInteDiag_delete
    call expect_near(1.0d0, sum(abs(c0(:))**2), 1.0d-8)

    ! -- integration by Krylov --
    c1(:) = c(:) / sqrt(sum(abs(c(:))**2))
    call TimeInteKrylov_new(num, 10)
    do i = 1, nt
       call TimeInteKrylov_calc(Harm_hc, dt, c1(:))
    end do
    call TimeInteKrylov_delete
    write(*,*) sum(abs(c1(:)-c0(:)))/size(c(:))    

    c1(:) = c(:) / sqrt(sum(abs(c(:))**2))
    call TimeInteKrylov_new(num, 60)
    do i = 1, nt
       call TimeInteKrylov_calc(Harm_hc, dt, c1(:))
    end do
    call TimeInteKrylov_delete
    write(*,*) sum(abs(c1(:)-c0(:)))/size(c(:))
    
    ! -- finalize --
    call ExpDVR_delete
    call Harm_delete
    
  end subroutine test_tinte_simple
  subroutine test_elnuc
    use Mod_ElNuc
    integer, parameter :: n = 512
    integer, parameter :: num = 2*n+1
    double precision, parameter :: m = 1.2d0
    double precision, parameter :: w = 1.0d0
    double precision, parameter :: a = m*w/2
    double precision, parameter :: x0 = 0.0
    double precision, parameter :: p0 = 0.0
    integer, parameter :: nstate = 2
    complex(kind(0d0)) :: g0(num), cg0(num), c0(num*nstate), c1(num*nstate)
    complex(kind(0d0)) :: h(num*nstate,num*nstate), hc0(num*nstate), hc1(num*nstate)
    integer i
    
    ! -- initialize --
    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call ElNuc_new(m, nstate)

    ! -- Matrix --
    Hel_(:,1,1) = m*w*w/2 *xs_(:)**2
    Hel_(:,2,2) = m*w*w/2 *xs_(:)**2 + 1.0d0
    Hel_(:,1,2) = xs_(:)/10
    Hel_(:,2,1) = xs_(:)/10
    Xij_(:,:,:) = 0
    Xij_(:,2,1) = exp(-(xs_(:)-1)**2)
    Xij_(:,1,2) = -Xij_(:,2,1)

    ! -- coefficient --
    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), cg0(:))
    cg0(:) = cg0(:) / sqrt(real(dot_product(cg0, cg0)))
    c0(:) = 0
    do i = 1, num
       c0(2*i-1) = cg0(i)
    end do
    c1(:) = c0(:)
    
    ! -- Hc by matrix --
    call ElNuc_h(h)
    hc0(:) = matmul(h(:,:), c0(:))

    ! -- Hc by direct --
    call ElNuc_hc(c1(:), hc1(:))

    ! -- compare
    call expect_eq(0.0d0, sum(abs(hc0(:)-hc1(:)))/size(hc1))
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete
    
  end subroutine test_elnuc
  subroutine test_hc_time(hc_method)
    use Mod_ElNuc
    integer :: hc_method
    integer, parameter :: n = 512
    integer, parameter :: num = 2*n+1
    integer, parameter :: nstate = 150
    integer, parameter :: nn = num*nstate
    double precision, parameter :: m = 1.2d0
    double precision, parameter :: w = 1.0d0
    double precision, parameter :: a = m*w/2
    double precision, parameter :: x0 = 0.0
    double precision, parameter :: p0 = 0.0    
    complex(kind(0d0)) :: g0(num), cg0(num), c0(nn), hc0(nn)
    integer i, j
    
    ! == result at athena ==
    ! 2018/1/11 18:00
    ! nstate  | time(s) |
    !--------------------|
    !  2      |   0.458  |
    !  10     |   0.927  |
    !  20     |   2.038  |
    !  30     |   4.012  |
    !  40     |  18.012  |
    !  50     |  37.851  | 
    !  60     |  59.442  |

    ! == result at athena ==
    ! 2018/1/11 23:00
    ! nstate  | time(s) |
    !--------------------|
    !  2      |          |
    !  10     |          |
    !  20     |   0.489  |
    !  30     |          |
    !  40     |   1.321  |
    !  50     |          | 
    !  60     |   2.579  |
    !  80     |   3.445  |
    ! 100     |   4.646  |
    ! 150     |   6.993  |

    ! == results at athena49.cluster ==
    ! -O0
    ! 150     | 1core  | 5.111  |
    !         | 12core | 1.6144 |
    !
    ! -O3 
    ! 150     | 1core  | 0.997  |
    !         | 12core | 0.490  |    

    ! -- initialize --
    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call ElNuc_new(m, nstate)    

    ! -- Matrix --
    do i = 1, nstate       
       do j = 1, nstate
          Hel_(:,i,j) = m*w*w/2 *xs_(:)**2 + (i+j)/100
          Xij_(:,i,j) = exp(-(xs_(:)-1)**2) * (i-j)/100
       end do
    end do

    ! -- coefficient --
    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), cg0(:))
    cg0(:) = cg0(:) / sqrt(real(dot_product(cg0, cg0)))
    c0(:) = 0
    do i = 1, num
       c0(2*i-1) = cg0(i)
    end do
    
    ! -- Hc by direct --
    hc_method_ = hc_method
    call ElNuc_hc(c0(:), hc0(:))
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete
        
  end subroutine test_hc_time
  subroutine test_tinte_harm
    use Mod_math, only : lapack_zgeev
    use Mod_TimeInteKrylov
    use Mod_TimeInteDiag
    use Mod_ElNuc
    use Mod_Timer
    integer, parameter :: n = 128
    integer, parameter :: num = 2*n+1
    double precision, parameter :: m = 1.2d0
    double precision, parameter :: w = 1.1d0
    double precision, parameter :: a = m*w/2
    double precision, parameter :: a0 = a
    double precision, parameter :: x0 = 0.9d0
    double precision, parameter :: p0 = 0.0d0
    complex(kind(0d0)) :: dt = 1.0d0
    integer, parameter :: nstate = 1
    integer, parameter :: nn=num*nstate
    complex(kind(0d0)) :: g0(num), cg0(num), c0(nn)
    complex(kind(0d0)) :: h(nn,nn)
    integer, parameter :: nt = 1
    
    double precision :: xt, pt, gt, cwt, swt, x
    complex(kind(0d0)) :: psi0, psi1, at
    integer i

    x = 0.8d0

    ! -- initialize --
    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call ElNuc_new(m, nstate)

    ! -- Matrix --
    Hel_(:,1,1) = m*w*w/2 *xs_(:)**2
    Xij_(:,:,:) = 0

    ! -- coefficient --
    g0(:) = exp(-a0*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), cg0(:))
    cg0(:) = cg0(:) / sqrt(real(dot_product(cg0, cg0)))
    c0(:) = cg0(:)
    
    ! -- time integration by diagonalization--    
    call ElNuc_h(h)
    call TimeInteDiag_new(num_)
    call TimeInteDiag_precalc(h)
    do i = 1, nt
       call TimeInteDiag_calc(dt, c0(:))
    end do
    call TimeInteDiag_delete
    call ExpDVR_at_x(c0(:), x, 0, psi0)

    ! -- exact --
    ! see Tannor's book p.29
    cwt = cos(w*real(dt))
    swt = sin(w*real(dt))
    xt = x0*cwt + p0/(m*w)*swt
    pt = p0*cwt - m*w*x0*swt
    at = a * (a0*cwt+ii*a*swt) / (a*cwt + ii*a0*swt)
    gt = (pt*xt-p0*x0)/2 - w*real(dt)/2
    psi1 = (2*real(at)/pi)**(0.25d0) * exp(-at*(x-xt)**2 + ii*pt*(x-xt) + ii*gt)

    ! -- check --
    call expect_near(psi1, psi0, 1.0d-6)
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete

  end subroutine test_tinte_harm
  subroutine test_tinte_2state
    use Mod_TimeInteKrylov
    use Mod_TimeInteDiag
    use Mod_ElNuc
    use Mod_Timer
    integer, parameter :: n = 128
    integer, parameter :: num = 2*n+1
    double precision, parameter :: m = 1.2d0
    double precision, parameter :: w = 1.0d0
    double precision, parameter :: a = m*w/2
    double precision, parameter :: x0 = 1.0
    double precision, parameter :: p0 = 0.0
    complex(kind(0d0)) :: dt = 1.0d0
    integer, parameter :: nstate = 2
    integer, parameter :: nn=num*nstate
    complex(kind(0d0)) :: g0(num), cg0(num), c0(nn), c1(nn)
    complex(kind(0d0)) :: h(nn,nn)
    integer, parameter :: nt = 1
    type(Obj_Timer) :: timer
    integer i

    ! -- initialize --
    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call ElNuc_new(m, nstate)
    call Timer_new(timer, "time_inte2", .false.)

    ! -- Matrix --
    Hel_(:,1,1) = m*w*w/2 *xs_(:)**2
    Hel_(:,2,2) = m*w*w/2 *xs_(:)**2 + 1.0d0
    Hel_(:,1,2) = xs_(:)/10
    Hel_(:,2,1) = xs_(:)/10
    Xij_(:,:,:) = 0
    Xij_(:,2,1) = exp(-(xs_(:)-1)**2)/100
    Xij_(:,1,2) = -Xij_(:,1,2)

    ! -- coefficient --
    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), cg0(:))
    cg0(:) = cg0(:) / sqrt(real(dot_product(cg0, cg0)))
    c0(:) = 0
    do i = 1, num
       c0(2*i-1) = cg0(i)
    end do
    c1(:) = c0(:)
    
    ! -- time integration by diagonalization--
    call Timer_begin(timer, "elnuc_h")
    call ElNuc_h(h)
    call Timer_end(timer, "elnuc_h")
    call Timer_begin(timer, "diag")
    call TimeInteDiag_new(num_*nstate_)
    call TimeInteDiag_precalc(h)
    call Timer_end(timer, "diag")
    call Timer_begin(timer, "inte_eig")
    do i = 1, nt
       call TimeInteDiag_calc(dt, c0(:))
    end do
    call TimeInteDiag_delete
    call Timer_end(timer, "inte_eig")
    call expect_near(1.0d0, sum(abs(c0(:))**2), 1.0d-5)
    
    ! -- time integration by Krylov --
    call Timer_begin(timer, "inte_krylov")
    call TimeInteKrylov_new(nn, 50)
    if(get_err().ne.0) then
          begin_err(1)
          write(0,*) "Error on TimeInteKrylov_new"
          end_err()
       end if
    do i = 1, nt
       call TimeInteKrylov_calc(ElNuc_hc, dt, c1(:)); check_err()
    end do
    call TimeInteKrylov_delete
    call Timer_end(timer, "inte_krylov")
    
    ! -- compare --
    call expect_near(0.0d0, sum(abs(c0(:)-c1(:)))/size(c0), 1.0d-3)
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete
    
    call Timer_result(timer)
    call Timer_delete(timer)

  end subroutine test_tinte_2state
end module Mod_TestDVR

program main
  use Mod_Utest
  use Mod_UtestCheck
  use Mod_TestDVR

  call utest_new
  call ErrHandle_new

  call TestDVR_run

  !  call Timer_result
  call utest_delete
  call ErrHandle_delete
  
end program main
