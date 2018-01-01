#include "macros.fpp"

module Mod_WPDy
  use Mod_ErrHandle
  implicit none
  integer :: nx_, nstate_
  double precision :: dx_
  double precision, allocatable :: xs_(:), vs_(:,:,:), cvs_(:), frs_(:,:), frs0_(:,:)
  double precision :: m_
  logical :: setupq_=.false.
contains
  ! ==== constructors ====
  subroutine WPDy_new(nstate, nx, dx)
    integer nx, nstate
    double precision dx
    integer i
    nx_ = nx
    nstate_ = nstate
    dx_ = dx
    m_  = 1.0d0

    allocate(xs_(nx))                     ! grid points
    allocate(vs_(nstate_, nstate_, nx))     ! potential matrix
    allocate(cvs_(nx))    ! imaginary part of potential matrix
    allocate(frs_(nstate_, 0:2*nx-1))      ! real and imaginary part of WP
    allocate(frs0_(nstate_, 0:2*nx-1))     ! WP at t = 0

    xs_(:)     = 0.0d0
    vs_(:,:,:) = 0.0d0
    cvs_(:)    = 0.0d0
    frs_(:,:)  = 0.0d0
    frs0_(:,:) = 0.0d0

    do i = 2, nx_
       xs_(i) = xs_(1) + (i-1)*dx
    end do
    
  end subroutine WPDy_new
  subroutine WPDy_new_json(o)
    use Mod_fjson
    type(object) o
    integer nstate, nx
    double precision dx

    call object_get_i(o, "nstate", nstate); check_err()
    call object_get_i(o, "nx", nx); check_err()    
        
    call WPDy_new(nstate, nx, dx)

    call object_get_d(o, "dx", dx_); check_err()
    call object_get_d(o, "m",  m_); check_err()
    
  end subroutine WPDy_new_json
  ! ==== setter ====  
  subroutine WPDy_set_vs(ifile)
    integer, intent(in) :: ifile
    integer ii, jj, ix
    double precision val
    read(ifile, *)
    do
       read(ifile, *, end=100) ii, jj, ix, val
       vs_(ii,jj,ix) = val
    end do
100 continue
  end subroutine WPDy_set_vs
  subroutine WPDy_set_frs(ifile)
    integer, intent(in) :: ifile
    integer ix, n
    double precision re, im
    read(ifile, *)
    do
       read(ifile, *, end=100) ix, n, re, im
       frs_(n,2*(ix-1))   = re
       frs_(n,2*(ix-1)+1) = im
    end do
100 continue
  end subroutine WPDy_set_frs
  subroutine WPDy_setup
    double precision norm
    double precision, parameter :: tol = 1.0d-10
    norm = sqrt(WPDy_rn(0))
    if(norm < tol) then
       begin_err(1)
       write(0,*) "norm is too small"
       write(0,*) "norm:", norm
       end_err()
    end if
    frs_(:,:) = frs_(:,:) / norm
    frs0_(:,:) = frs_(:,:)
    setupq_ = .true.
  end subroutine WPDy_setup
  ! ==== calc ====
  function WPDy_rn(n) result(res)
    double precision res
    integer, intent(in) :: n
    integer ii, i
    res = 0.0d0
    do ii = 1, nstate_
       do i = 0, nx_-1
          res = res + (frs_(ii,2*i)**2 + frs_(ii,2*i+1)**2) * xs_(i+1)**n
       end do
    end do
    res = res*dx_
  end function WPDy_rn
  function WPDy_ac() result(res)
    complex(kind(0d0)) :: res
    double precision :: reac, imac
    integer i, ii
    reac = 0.0d0
    imac = 0.0d0
    do i = 0, nx_-1
       do ii = 1, nstate_
          reac = reac + frs0_(ii,2*i)*frs_(ii,2*i)   + frs0_(ii,2*i+1)*frs_(ii,2*i+1)
          imac = imac + frs0_(ii,2*i)*frs_(ii,2*i+1) - frs0_(ii,2*i+1)*frs_(ii,2*i)
       end do
    end do
    reac = reac * dx_
    imac = imac * dx_
    res = dcmplx(reac, imac)

  end function WPDy_ac
  function WPDy_prob(ii) result(res)
    integer, intent(in) :: ii
    double precision res
    integer ix
    res = 0
    do ix = 0, nx_-1
       res = res + frs_(ii,2*ix)**2 + frs_(ii,2*ix+1)**2
    end do
    res = res * dx_
  end function WPDy_prob
  ! ==== utils ====
  subroutine WPDy_check_setupq
    if(.not.setupq_) then
       throw_err("call WPDy_setup first", 1)
    end if
  end subroutine WPDy_check_setupq
  ! ==== IO ====
  subroutine WPDy_dump_coef(out_it_dir)
    character(*), intent(in) :: out_it_dir
    character(100) :: out_path
    integer, parameter :: ifile = 13536
    integer ix, is

    call WPDy_check_setupq
    
    out_path = trim(out_it_dir) // "/coef.csv"
    call open_w(ifile, out_path)
    write(ifile,'("i,j,re,im")') 
    do ix = 1, nx_
       do is = 1, nstate_
          write(ifile,'(i0,",",i0,",",f20.10,",",f20.10)') &
               ix,is,frs_(is, 2*(ix-1)),frs_(is, 2*(ix-1)+1)
       end do
    end do
    close(ifile)
    
  end subroutine WPDy_dump_coef
end module Mod_WPDy

module Mod_WPDy_SplitOp
  use Mod_WPDy
  use Mod_fft
  implicit none
  double precision :: dk_, K_
  double precision, allocatable :: ks_(:)
  logical :: splitop_setupq_=.false.
contains
  ! ==== setter ====
  subroutine WPDy_SplitOp_setup
    use Mod_const, only : pi
    integer i
    
    if(nstate_.ne.2 .and. nstate_.ne.1) then
       write(0,*) "only nstate=2 is supported"
       stop
    end if

    call WPDy_setup; check_err()

    allocate(ks_(nx_))

    K_ = pi/dx_
    dk_ = 2*pi/(nx_*dx_)
    do i = 1, nx_
       ks_(i) = (i-nx_/2)/dk_
    end do
    call fft_begin(2*nx_)

    splitop_setupq_ = .true.
    
  end subroutine WPDy_SplitOp_setup
  ! ==== calc ====
  subroutine WPDy_SplitOp_inte(dt)
    use Mod_const, only : ii
    complex(kind(0d0)), intent(in) :: dt
    double precision :: sqrt_nx, ene
    complex(kind(0d0)) :: sqD, v1, v2, v12, v21
    integer          :: i, is
    complex(kind(0d0)) :: fi, f1i, f2i, tmp, ex

    call WPDy_SplitOp_check_setupq; check_err()

    if(nstate_ .ne. 2) then
       write(0,*) "nstate must be 2"
       stop
    end if
    sqrt_nx = sqrt(1.0d0 * nx_)
    write(*,*) "1", frs_(1,1)

    ! -- operate exp[-iVdt/2] --
    do i = 0, nx_-1
       v1 = dcmplx(vs_(1,1,i+1), cvs_(i+1)); v2 = dcmplx(vs_(2,2,i+1), cvs_(i+1))
       v12 = vs_(1,2,i+1); v21 = vs_(2,1,i+1)
       sqD = sqrt((v2-v1)**2 + 4.0d0*v12*v21)
       ex = exp(-ii*(v1+v2)*dt/4.0d0)
       f1i = dcmplx(frs_(1,2*i), frs_(1,2*i+1))
       f2i = dcmplx(frs_(2,2*i), frs_(2,2*i+1))
       tmp = ex*(cos(sqD*dt/4.0d0)     * f1i + &
            ii*  sin(sqD*dt/4.0d0)/sqD * ((v2-v1)*f1i -2.0d0*v12*f2i))
       frs_(1,2*i)   = real(tmp)
       frs_(1,2*i+1) = aimag(tmp)
       tmp = ex*(cos(sqD*dt/4.0d0)     * f2i + &
            ii*  sin(sqD*dt/4.0d0)/sqD * (-2.0d0*v21*f1i +(v1-v2)*f2i))
       frs_(2,2*i)   = real(tmp)
       frs_(2,2*i+1) = aimag(tmp)
    end do
    write(*,*) "2", frs_(1,1)

    call fft_backward(frs_(1,:), int(xs_(1)/dx_), -nx_/2+1)
    call fft_backward(frs_(2,:), int(xs_(1)/dx_), -nx_/2+1)
    frs_(:,:) = frs_(:,:)/sqrt_nx
    write(*,*) frs_(1,1)

    ! -- operate exp[-iTdt] --
    do is = 1, nstate_
       do i = 0, nx_-1
          fi = dcmplx(frs_(is,2*i), frs_(is,2*i+1))
          ene = 1.0d0/(2.0d0*m_) * ks_(i+1)**2
          fi = exp(-II*ene*dt)*fi
          frs_(is,2*i) = real(fi)
          frs_(is,2*i+1) = aimag(fi)
       end do
    end do
    write(*,*) frs_(1,1)

    call fft_forward(frs_(1,:), -nx_/2+1, int(xs_(1)/dx_))
    call fft_forward(frs_(2,:), -nx_/2+1, int(xs_(1)/dx_))
    frs_(:, :) = frs_(:, :)/sqrt_nx
    write(*,*) frs_(1,1)

    ! -- operate exp[-iVdt/2] --
    do i = 0, nx_-1
       v1 = dcmplx(vs_(1,1,i+1), cvs_(i+1)); v2 = dcmplx(vs_(2,2,i+1), cvs_(i+1))
       v12 = vs_(1,2,i+1); v21 = vs_(2,1,i+1)
       sqD = sqrt((v2-v1)**2 + 4.0d0*v12*v21)
       ex = exp(-ii*(v1+v2)*dt/4.0d0)
       f1i = dcmplx(frs_(1,2*i), frs_(1,2*i+1))
       f2i = dcmplx(frs_(2,2*i), frs_(2,2*i+1))
       tmp = ex*(cos(sqD*dt/4.0d0)     * f1i + &
            ii*  sin(sqD*dt/4.0d0)/sqD * ((v2-v1)*f1i -2.0d0*v12*f2i))
       frs_(1,2*i)   = real(tmp)
       frs_(1,2*i+1) = aimag(tmp)
       tmp = ex*(cos(sqD*dt/4.0d0)     * f2i + &
            ii*  sin(sqD*dt/4.0d0)/sqD * (-2.0d0*v21*f1i +(v1-v2)*f2i))
       frs_(2,2*i)   = real(tmp)
       frs_(2,2*i+1) = aimag(tmp)
    end do
    write(*,*) frs_(1,1)
    
  end subroutine WPDy_SplitOp_Inte
  ! ==== utils ====
  subroutine WPDy_SplitOp_check_setupq
    call WPDy_check_setupq ; check_err()
    if(.not. splitop_setupq_) then
       begin_err(1)
       write(0,*) "call WPDy_SplitOp_setup first"
       end_err()
    end if
  end subroutine WPDy_SplitOp_check_setupq
end module Mod_WPDy_splitop
