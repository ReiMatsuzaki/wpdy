module Mod_WPDy
  implicit none
  integer :: nx_, nstate_, nt_
  double precision :: dx_, dt_
  double precision, allocatable :: xs_(:), vs_(:,:,:), csv_(:), frs_(:,:), frs0_(:,:)
  logical :: setupq_=.false.
contains
  ! ==== constructors ====
  subroutine WPDy_new(nstate, nx, x0, dx, nt, dt)
    integer nx, nstate, i, nt
    double precision x0, dx, dt
    nx_ = nx
    nstate_ = nstate_
    dx_ = dx
    nt_ = nt
    dt_ = dt

    allocate(xs_(nx))                     ! grid points
    allocate(vs_(nstate, nstate, nx))     ! potential matrix
    allocate(cvs_(nstate, nstate, nx))    ! imaginary part of potential matrix
    allocate(frs_(nstate, 0:2*nx-1))      ! real and imaginary part of WP
    allocate(frs0_(nstate, 0:2*nx-1))     ! WP at t = 0

    do i = 1, nx
       xs_(i) = x0 + (i-1)*dx
    end do
    
  end subroutine WPDy_new
  ! ==== setter ====
  subroutine WPDy_set_vs(ifile)
    integer, intent(in) :: ifile
    integer i
    double precision val
    read(ifile, *)
    do
       read(ifile, *, end=100) i, val
       vs(i) = val
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
    frs0_(:,:) = frs_(:,:)
    setupq = .true.
  end subroutine WPDy_setup  
  ! ==== calc ====
  function WPDy_norm2() result(res)
    double precision res
    integer ii, i
    res = 0.0d0
    do ii = 1, nstate_
       do i = 0, nx_-1
          res = res + frs(ii,2*i)**2 + frs(ii,2*i+1)**2
       end do
    end do
    res = res*dx
  end function WPDy_norm2
  function WPDy_rn(n) result(res)
    double precision res
    integer, intent(in) :: n
    integer ii, i
    res = 0.0d0
    do ii = 1, nstate_
       do i = 0, nx_-1
          res = res + (frs(ii,2*i)**2 + frs(ii,2*i+1)**2) * xs(i+1)**n
       end do
    end do
    res = res*dx
  end function WPDy_rn
  function WPDy_ac() result(res)
    complex(kind(0d0)) :: res
    double precision :: reac, imac
    integer i, ii
    reac = 0.0d0
    imac = 0.0d0
    do i = 0, nxs(1)-1
       do ii = 1, nstate
          reac = reac + frs0(ii,2*i)*frs(ii,2*i)   + frs0(ii,2*i+1)*frs(ii,2*i+1)
          imac = imac + frs0(ii,2*i)*frs(ii,2*i+1) - frs0(ii,2*i+1)*frs(ii,2*i)
       end do
    end do
    reac = reac * dxs(1)
    imac = imac * dxs(1)
    ac = cmplx(reac, imac)

  end function WPDy_ac
  ! ==== utils ====
  subroutine WPDy_normalize
    double precision :: norm2
    norm2 = WPDy_norm2()
    frs(:,:) = frs(:,:) / sqrt(norm2)
  end subroutine WPDy_normalize
  ! ==== IO ====
  subroutine WPDy_dump_coef(out_it_dir)
    character(*), intent(in) :: out_it_dir
    character(100) :: out_path
    integer, parameter :: ifile = 13536
    integer ix, is

    out_path = trim(out_it_dir) // "/coef.csv"
    call open_w(ifile, out_path); check_err()
    write(ifile,'("i,j,re,im")') 
    do ix = 1, nxs(1)
       do is = 1, nstate
          write(ifile,'(i0,",",i0,",",f20.10,",",f20.10)') ix,is,frs(is, 2*(ix-1)),frs(is, 2*(ix-1)+1)
       end do
    end do
    close(ifile)
    
  end subroutine FQ1d_dump_coef
end module Mod_WPDy

module Mod_WPDy_SplitOp
  use Mod_WPDy
  use Mod_fft
  implicit none
  double precision :: dk, K
  double precision, allocatable :: ks(:)
contains
  subroutine WPDy_SplitOp_new
    use Mod_const, only pi

    if(.not. setupq_) then
       write(0,*) "call WPDy_setup first"
       stop
    end if

    if(nstate_.ne.2) then
       write(0,*) "only nstate=2 is supported"
       stop
    end if

    allocate(ks(nx_))

    K = pi/dx_
    dk = 2*pi/(nx_*dx_)
    do i = 1, nx
       ks(i) = (i-nx/2)/dk
    end do
    
  end subroutine WPDy_SplitOp_New
  subroutine WPDy_setup
    call fft_begin(2*nx_)
  end subroutine WPDy_setup  subroutine inte
double precision :: dx, sqrt_nx, ene
    complex(kind(0d0)) :: sqD, v1, v2, v12, v21
    integer          :: i, nx, is
    complex(kind(0d0)) :: fi, f1i, f2i, tmp, ex

    if(nstate .ne. 2) then
       throw_err("nstate must be 2", 1)
    end if
    dx = dxs(1)
    nx = nxs(1)
    sqrt_nx = sqrt(1.0d0 * nx)

    ! -- operate exp[-iVdt/2] --
    do i = 0, nx-1
       v1 = dcmplx(vs(1,1,i+1), cvs(i+1)); v2 = dcmplx(vs(2,2,i+1), cvs(i+1))
       v12 = vs(1,2,i+1); v21 = vs(2,1,i+1)
       sqD = sqrt((v2-v1)**2 + 4.0d0*v12*v21)
       ex = exp(-ii*(v1+v2)*cdt/4.0d0)
       f1i = dcmplx(frs(1,2*i), frs(1,2*i+1))
       f2i = dcmplx(frs(2,2*i), frs(2,2*i+1))
       tmp = ex*(cos(sqD*cdt/4.0d0)     * f1i + &
            ii*  sin(sqD*cdt/4.0d0)/sqD * ((v2-v1)*f1i -2.0d0*v12*f2i))
       frs(1,2*i)   = real(tmp)
       frs(1,2*i+1) = aimag(tmp)
       tmp = ex*(cos(sqD*cdt/4.0d0)     * f2i + &
            ii*  sin(sqD*cdt/4.0d0)/sqD * (-2.0d0*v21*f1i +(v1-v2)*f2i))
       frs(2,2*i)   = real(tmp)
       frs(2,2*i+1) = aimag(tmp)
    end do

    call fft_backward(frs(1,:), int(xs(1)/dx), -nx/2+1)
    call fft_backward(frs(2,:), int(xs(1)/dx), -nx/2+1)
    frs(:,:) = frs(:,:)/sqrt_nx

    ! -- operate exp[-iTdt] --
    do is = 1, nstate
       do i = 0, nx-1
          fi = dcmplx(frs(is,2*i), frs(is,2*i+1))
          ene = 1.0d0/(2.0d0*ms(1)) * ks(i+1)**2
          fi = exp(-II*ene*cdt)*fi
          frs(is,2*i) = real(fi)
          frs(is,2*i+1) = aimag(fi)
       end do
    end do

    call fft_forward(frs(1,:), -nx/2+1, int(xs(1)/dx))
    call fft_forward(frs(2,:), -nx/2+1, int(xs(1)/dx))
    frs(:, :) = frs(:, :)/sqrt_nx

    ! -- operate exp[-iVdt/2] --
    do i = 0, nx-1
       v1 = dcmplx(vs(1,1,i+1), cvs(i+1)); v2 = dcmplx(vs(2,2,i+1), cvs(i+1))
       v12 = vs(1,2,i+1); v21 = vs(2,1,i+1)
       sqD = sqrt((v2-v1)**2 + 4.0d0*v12*v21)
       ex = exp(-ii*(v1+v2)*cdt/4.0d0)
       f1i = dcmplx(frs(1,2*i), frs(1,2*i+1))
       f2i = dcmplx(frs(2,2*i), frs(2,2*i+1))
       tmp = ex*(cos(sqD*cdt/4.0d0)     * f1i + &
            ii*  sin(sqD*cdt/4.0d0)/sqD * ((v2-v1)*f1i -2.0d0*v12*f2i))
       frs(1,2*i)   = real(tmp)
       frs(1,2*i+1) = aimag(tmp)
       tmp = ex*(cos(sqD*cdt/4.0d0)     * f2i + &
            ii*  sin(sqD*cdt/4.0d0)/sqD * (-2.0d0*v21*f1i +(v1-v2)*f2i))
       frs(2,2*i)   = real(tmp)
       frs(2,2*i+1) = aimag(tmp)
    end do
    
  end subroutine inte
end module Mod_WPDy_splitop
