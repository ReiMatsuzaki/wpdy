#include "macros.fpp"

module Mod_ExpDVR
  use Mod_ErrHandle
  use Mod_const, only : ii, pi
  implicit none
  integer num_, n_
  double precision, allocatable :: xs_(:), ws_(:)
  complex(kind(0d0)), allocatable :: u_(:,:)
  double precision :: dx_, L_, x0_, xN_
contains
  subroutine ExpDVR_new(n, x0, xN)
    integer, intent(in) :: n
    double precision, intent(in) :: x0, xN
    integer i, al, j
    double precision c
    complex(kind(0d0)) z
    n_ = n
    num_ = 2*n+1
    allocate(xs_(num_), ws_(num_), u_(num_,num_))

    xN_ = xN
    x0_ = x0
    L_ = xN-x0
    dx_ = L_/num_
    do i = 1, num_
       xs_(i) = x0 + dx_*i
    end do
    ws_(:) = dx_
    
    c = sqrt(1.0d0/num_)
    z = -2.0*ii*pi/num_
    do al = 1, num_
       do j = -n, n
          u_(j+n+1,al) = c*exp(z*al*j)
       end do
    end do
    
  end subroutine ExpDVR_new
  subroutine ExpDVR_delete
    deallocate(xs_, ws_, u_)
  end subroutine ExpDVR_delete
  subroutine ExpDVR_phi(xs, nd, res)
    double precision, intent(in) :: xs(:)
    integer, intent(in) :: nd
    complex(kind(0d0)), intent(out) :: res(:,:)
    integer j
    double precision s
    complex(kind(0d0)) r, z, a

    if(size(res,1) .ne. num_) then
       throw_err("res: size mismatch", 1)
    end if

    if(size(res,2) .ne. size(xs)) then
       throw_err("res: size mismatch", 1)
    end if

    r = 2*ii*pi/L_
    s = sqrt(1/L_)
    do j = -n_, n_
       z = r*j
       a = s * (z**nd) * exp(-z*x0_)
       res(j+n_+1,:) = a*exp(z*xs(:))
       !       do ix = 1, size(xs)
       !          res(j+n_+1,ix) = a*exp(z*xs(ix))
       !       end do
    end do
    
  end subroutine ExpDVR_phi
  subroutine ExpDVR_d1mat(res)
    complex(kind(0d0)), intent(out) :: res(:,:)
    integer a, b
    
    res(:,:) = 0.0
    do a = 1, num_
       do b = 1, num_
          if(a.ne.b) then
             res(a,b) = pi/L_ * ((-1)**(a-b)) / sin(pi*(a-b)/num_)
          end if
       end do
    end do
       
  end subroutine ExpDVR_d1mat
  subroutine ExpDVR_d2mat(res)
    complex(kind(0d0)), intent(out) :: res(:,:)
    integer a, b
    double precision c, s
    
    res(:,:) = 0.0
    do a = 1, num_
       do b = 1, num_
          if(a.eq.b) then
             res(a,a) = -pi*pi/(3*L_**2) * (num_**2-1)
          else
             s = sin(pi*(a-b)/num_)
             c = cos(pi*(a-b)/num_)
             res(a,b) = -2*pi*pi/(L_**2) * (-1)**(a-b)*c/(s*s)
          end if
       end do
    end do
       
  end subroutine ExpDVR_d2mat
  subroutine ExpDVR_at(cs, res)
    complex(kind(0d0)), intent(in) :: cs(:)
    complex(kind(0d0)) :: res(:)
    if(size(cs).ne.num_) then
       throw_err("size mismatch", 1)
    end if
    if(size(res).ne.num_) then
       throw_err("size mismatch", 1)
    end if
    res(:) = cs(:) / sqrt(ws_(:))
  end subroutine ExpDVR_at
  subroutine ExpDVR_at_xs(cs, xs, nd, res)
    complex(kind(0d0)):: cs(:)
    double precision, intent(in) ::   xs(:)
    integer, intent(in) :: nd
    complex(kind(0d0)), intent(out) :: res(:)
    complex(kind(0d0)) :: phi(num_, size(xs))
    complex(kind(0d0)) :: uc(num_)

    if(num_.ne.size(cs)) then
       throw_err("cs: size mismatch", 1)
    end if
    if(size(xs).ne.size(res)) then
       throw_err("xs and res: size mismatch", 1)
    end if

    call ExpDVR_phi(xs, nd, phi); check_err()
    
    uc(:)  = matmul(u_(:,:), cs(:))
    res(:) = matmul(transpose(phi(:,:)), uc(:))
    !write(*,*) "u:"
    !write(*,*) u_(1,:)
    !write(*,*) u_(2,:)
    !write(*,*) u_(3,:)
    !write(*,*) "c:"
    !write(*,*) cs
    !write(*,*) "uc:"
    !write(*,*) uc
    !write(*,*) "tr"
    !write(*,*) matmul(transpose(u_), cs)
    !write(*,*) "mod_uc"
    !u_(1,1) = 1*ii
    !u_(1,2) = 2*ii
    !u_(2,1) = 3*ii
    !u_(2,2) = 4*ii
    !cs(1) = 1*ii
    !cs(2) = 3*ii
    !write(*,*) u_(1:2,1:2)
    !write(*,*) cs(1:2)
    !write(*,*) matmul(u_(1:1,1:1), cs(1:1))
    !write(*,*) matmul(u_(1:2,1:2), cs(1:2))
    !write(*,*) matmul(u_(1:3,1:3), cs(1:3))
    !write(*,*) phi(1,1), uc(1), res(1)
    
  end subroutine ExpDVR_at_xs
  subroutine ExpDVR_at_x(cs, x, nd, res)
    complex(kind(0d0)), intent(in) :: cs(:)
    double precision, intent(in) ::   x
    integer, intent(in) :: nd
    complex(kind(0d0)), intent(out) :: res
    double precision :: xs(1)
    complex(kind(0d0)) :: ress(1)
    xs(1) = x
    call ExpDVR_at_xs(cs, xs, nd, ress)
    res = ress(1)
  end subroutine ExpDVR_at_x
  subroutine ExpDVR_fit(fs, res)
    complex(kind(0d0)), intent(in) :: fs(:)
    complex(kind(0d0)) :: res(:)
    res(:) = fs(:) * sqrt(ws_(:))
  end subroutine ExpDVR_fit
end module Mod_ExpDVR

