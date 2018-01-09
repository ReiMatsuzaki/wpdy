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

module Mod_ElNuc
  use Mod_ExpDVR
  implicit none
  integer :: nstate_
  double precision :: m_
  complex(kind(0d0)), allocatable :: Hel_(:,:,:), Xij_(:,:,:), D1_(:,:), D2_(:,:)
contains
  subroutine ElNuc_new(m, nstate)
    ! assume ExpDVR is already prepared
    double precision, intent(in) :: m
    integer, intent(in) :: nstate
    m_ = m
    nstate_ = nstate
    allocate(Hel_(num_,nstate,nstate), Xij_(num_,nstate,nstate))
    allocate(D1_(num_,num_), D2_(num_,num_))
    call ExpDVR_d1mat(D1_)
    call ExpDVR_d2mat(D2_)
    
  end subroutine ElNuc_new
  subroutine ElNuc_delete
    deallocate(Hel_, Xij_, D1_, D2_)
  end subroutine ElNuc_delete
  subroutine ElNuc_h(h)
    complex(kind(0d0)), intent(out) :: h(:,:)
    integer a, b, i, j, idx, jdx
    
    h(:,:) = 0
    do a = 1, num_
       do b = 1, num_
          do i = 1, nstate_
             do j = 1, nstate_
                idx = a*(nstate_-1)+i
                jdx = b*(nstate_-1)+j
                if(a==b) then
                   h(idx,jdx) =  h(idx,jdx) + Hel_(a,i,j)
                end if
                if(i==j) then
                   h(idx,jdx) =  h(idx,jdx) -1/(2*m_)*D2_(a,b)
                end if
                h(idx,jdx) =  h(idx,jdx) - (1/m_)*Xij_(a,i,j)*D1_(a,b)
             end do
          end do
       end do
    end do
  end subroutine ElNuc_h
  subroutine ElNuc_hc(c, hc)
    complex(kind(0d0)), intent(in) :: c(:)
    complex(kind(0d0)), intent(out) :: hc(:)
    integer a, b, i0, i1, j0, j1

    hc(:) = 0
    do a = 1, num_
       i0 = a*(nstate_-1)+1
       i1 = a*(nstate_-1)+nstate_
       hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
       do b = 1, num_
          j0 = b*(nstate_-1)+1
          j1 = b*(nstate_-1)+nstate_
          hc(i0:i1) = hc(i0:i1) &
               - 1/m_*matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
               - 1/(2*m_)*D2_(a,b)*c(j0:j1)
       end do
    end do
    
  end subroutine ElNuc_hc
end module Mod_ElNuc

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
    call Utest_sub_end

    call Utest_sub_begin("test_eig")
    call test_eig
    call Utest_sub_end

    call Utest_sub_begin("test_tint")
    call test_time_inte
    call Utest_sub_end

    call Utest_sub_begin("test_elnuc")
    call test_elnuc
    call Utest_sub_end

    call Utest_sub_begin("test_tint2")
    call test_time_inte2
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
  subroutine test_time_inte
    use Mod_const, only : ii
    use Mod_math, only : lapack_zgeev, TimeInte_eig
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
    complex(kind(0d0)) :: ws(num), UR(num,num), UL(num,num)
    integer i

    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call Harm_new(m, w)

    ! -- initial condition --
    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), c(:))    

    ! -- integration by diagonalize --
    c0(:) = c(:) / sqrt(sum(abs(c(:))**2))
    call lapack_zgeev(H_, num, ws, UL, UR)    
    do i = 1, nt
       call TimeInte_eig(ws(:), UR(:,:), conjg(transpose(UL(:,:))), dt, c0(:))
    end do
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
    
  end subroutine test_time_inte
  subroutine test_elnuc
    use Mod_ElNuc
    integer, parameter :: n = 256
    integer, parameter :: num = 2*n+1
    double precision, parameter :: m = 1.2d0
    double precision, parameter :: w = 1.0d0
    double precision, parameter :: a = m*w/2
    double precision, parameter :: x0 = 1.0
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
  subroutine test_time_inte2
    use Mod_math, only : lapack_zgeev, TimeInte_eig
    use Mod_TimeInteKrylov
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
    complex(kind(0d0)) :: ws(nn), UR(nn,nn), UL(nn,nn)
    integer, parameter :: nt = 1
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
    call Timer_begin("elnuc_h")
    call ElNuc_h(h)
    call Timer_end("elnuc_h")
    call Timer_begin("diag")
    call lapack_zgeev(h, num*nstate_, ws, UL, UR)
    call Timer_end("diag")
    call Timer_begin("inte_eig")
    do i = 1, nt
       call TimeInte_eig(ws(:), UR(:,:), conjg(transpose(UL(:,:))), dt, c0(:))
    end do
    call Timer_end("inte_eig")
    
    ! -- time integration by Krylov --
    call Timer_begin("inte_krylov")
    call TimeInteKrylov_new(nn, 16)
    if(get_err().ne.0) then
          begin_err(1)
          write(0,*) "Error on TimeInteKrylov_new"
          end_err()
       end if
    do i = 1, nt
       call TimeInteKrylov_calc(ElNuc_hc, dt, c1(:)); check_err()
    end do
    call TimeInteKrylov_delete
    call Timer_end("inte_krylov")
    
    ! -- compare
    write(*,*) sum(abs(c0(1:10)-c1(1:10)))/size(c0)
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete
    
    call Timer_result

  end subroutine test_time_inte2
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
