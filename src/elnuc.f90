#include "macros.fpp"
! electronic nuclear Hamiltonian

module Mod_ElNuc
  use Mod_ExpDVR
  implicit none
  integer :: print_level_
  integer :: nstate_
  double precision :: m_
  complex(kind(0d0)), allocatable :: Hel_(:,:,:), Xij_(:,:,:), D1_(:,:), D2_(:,:)
  integer :: hc_method_
  double precision :: hc_tol_
contains  
  subroutine ElNuc_new(m, nstate)
    ! assume ExpDVR is already prepared
    double precision, intent(in) :: m
    integer, intent(in) :: nstate
    print_level_ = 0
    m_ = m
    nstate_ = nstate
    allocate(Hel_(num_,nstate,nstate), Xij_(num_,nstate,nstate))        
    allocate(D1_(num_,num_), D2_(num_,num_))
    Hel_ = 0
    Xij_ = 0
    call ExpDVR_d1mat(D1_)
    call ExpDVR_d2mat(D2_)

    hc_method_ = 0
    hc_tol_ = 1.0d-10
    
  end subroutine ElNuc_new
  subroutine ElNuc_delete
    deallocate(Hel_, Xij_, D1_, D2_)
  end subroutine ElNuc_delete
  subroutine ElNuc_h(h)
    complex(kind(0d0)), intent(out) :: h(:,:)
    integer a, b, i, j, idx, jdx

    if(print_level_>0) then
       i = size(Hel_,1)/2-1
       write(*,*) "i = ", i
       write(*,*) "ElNuc_h: Hel_(i,1,:2)", Hel_(i,1,:2)
       write(*,*) "ElNuc_h: Hel_(i,2,:2)", Hel_(i,2,:2)
       write(*,*) "ElNuc_h: Xij_(i,1,:2)", Xij_(i,1,:2)
       write(*,*) "ElNuc_h: Xij_(i,2,:2)", Xij_(i,2,:2)
    end if
    
    h(:,:) = 0
    do a = 1, num_
       do b = 1, num_
          do i = 1, nstate_
             do j = 1, nstate_
                idx = ElNuc_idx(a,i)
                jdx = ElNuc_idx(b,j)
                if(a==b) then
                   h(idx,jdx) =  h(idx,jdx) + Hel_(a,i,j)
                end if
                if(i==j) then
                   h(idx,jdx) =  h(idx,jdx) -1/(2*m_)*D2_(a,b)
                else
                   h(idx,jdx) =  h(idx,jdx) - 1/(2*m_)*(Xij_(a,i,j)*D1_(a,b) - Xij_(b,i,j)*conjg(D1_(b,a)))
                end if
             end do
          end do
       end do
    end do
  end subroutine ElNuc_h
  subroutine ElNuc_hc(c, hc)
    complex(kind(0d0)), intent(in) :: c(:)
    complex(kind(0d0)), intent(out) :: hc(:)
    if(hc_method_.eq.0) then
       call ElNuc_hc_0(c, hc)
    else if(hc_method_.eq.1) then
       call ElNuc_hc_1(c, hc)
    else if(hc_method_.eq.2) then
       call ElNuc_hc_2(c, hc)
    else if(hc_method_.eq.3) then
       call ElNuc_hc_3(c, hc)       
    end if
  end subroutine ElNuc_hc
  subroutine ElNuc_hc_0(c, hc)
    complex(kind(0d0)), intent(in) :: c(:)
    complex(kind(0d0)), intent(out) :: hc(:)
    integer idx, a, b, i0, i1, j0, j1
    integer :: alist(num_), numa

    numa = 0
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       if(sum(abs(c(i0:i1))) > nstate_*hc_tol_) then
          numa = numa + 1
          alist(numa) = a
       end if
    end do

    hc(:) = 0
    !$omp parallel default(none) shared(alist,nstate_,num_,Hel_,c,D2_,D1_,Xij_,hc,m_,numa) private(idx,a,i0,i1,b,j0,j1)
    !$omp do 
    do idx = 1, numa
       a = alist(idx)
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
    end do
    !$omp end do
    !$omp do
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1    
       do idx = 1, numa
          b = alist(idx)
          j0 = ElNuc_idx(b,1)
          j1 = j0 + nstate_-1      
          hc(i0:i1) = hc(i0:i1) - 1/(2*m_) * ( &
               + matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
               - matmul(Xij_(b,:,:), c(j0:j1)) * conjg(D1_(b,a)) &
               + D2_(a,b)*c(j0:j1))
       end do
    end do    
    !$omp end do
    !$omp end parallel
    
  end subroutine ElNuc_hc_0
  subroutine ElNuc_hc_3(c, hc)
    complex(kind(0d0)), intent(in) :: c(:)
    complex(kind(0d0)), intent(out) :: hc(:)
    integer idx, a, b, i0, i1, j0, j1
    integer :: alist(num_), numa
    double precision :: mm 

    mm = 1/(2*m_)
    
    numa = 0
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       if(sum(abs(c(i0:i1))) > nstate_*hc_tol_) then
          numa = numa + 1
          alist(numa) = a
       end if
    end do

    hc(:) = 0
    !$omp parallel default(none) shared(D2_,D1_,Xij_,alist,nstate_,num_,Hel_,c,hc,mm,numa) private(idx,a,i0,i1,b,j0,j1)
    !$omp do 
    do idx = 1, numa
       a = alist(idx)
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
    end do
    !$omp end do
    !$omp do    
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1    
       do idx = 1, numa
          b = alist(idx)
          j0 = ElNuc_idx(b,1)
          j1 = j0 + nstate_-1      
          hc(i0:i1) = hc(i0:i1) - mm * ( &
               + matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
               + D2_(a,b)*c(j0:j1))
       end do
    end do
    !$omp end do
    !$omp end parallel
    
    do idx = 1, numa
       b = alist(idx)
       j0 = ElNuc_idx(b,1)
       j1 = j0 + nstate_-1
       !$omp parallel default(none) shared(b,j0,j1,alist,nstate_,num_,c,D2_,D1_,Xij_,hc,mm,numa) private(a,i0,i1)
       !$omp do
       do a = 1, num_
          i0 = ElNuc_idx(a,1)
          i1 = i0 + nstate_-1       
          hc(i0:i1) = hc(i0:i1) + mm * matmul(Xij_(b,:,:), c(j0:j1)) * conjg(D1_(b,a))
       end do
       !$omp end do
       !$omp end parallel
    end do
    
  end subroutine ElNuc_hc_3
  subroutine ElNuc_hc_1(c, hc)
    complex(kind(0d0)), intent(in) :: c(:)
    complex(kind(0d0)), intent(out) :: hc(:)
    integer idx, a, b, i0, i1, j0, j1
    logical :: nonzero(num_)
    throw_err("do not use this function", 1)
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       if(sum(abs(c(i0:i1))) > nstate_*hc_tol_) then
          nonzero(a) = .true.
       else
          nonzero(a) = .false.
       end if
    end do
    
    ! first parallel version
    !$omp parallel do default(none) shared(nstate_,num_,Hel_,c,D2_,D1_,Xij_,nonzero,hc,m_) private(a,i0,i1,b,j0,j1)
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       if(nonzero(a)) then
          hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
       end if
       do b = 1, num_
          if(nonzero(b)) then
             j0 = ElNuc_idx(b,1)
             j1 = ElNuc_idx(b+1,1)-1
             hc(i0:i1) = hc(i0:i1) - 1/(2*m_) * ( &
                  +matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
                  -matmul(Xij_(b,:,:), c(j0:j1)) * conjg(D1_(b,a)) & 
                  + D2_(a,b)*c(j0:j1))
          end if
       end do
    end do
    !$omp end parallel do
    
  end subroutine ElNuc_hc_1
  subroutine ElNuc_hc_2(c, hc)
    complex(kind(0d0)), intent(in) :: c(:)
    complex(kind(0d0)), intent(out) :: hc(:)
    integer idx, a, b, i0, i1, j0, j1
    double precision, parameter :: tol = 1.0d-10
    throw_err("do not use this function", 1)
    hc(:) = 0
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = ElNuc_idx(a+1,1)-1
       hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
       do b = 1, num_
          j0 = ElNuc_idx(b,1)
          j1 = ElNuc_idx(b+1,1)-1
          hc(i0:i1) = hc(i0:i1) - 1/(2*m_) * (&
               +matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
               -matmul(Xij_(b,:,:), c(j0:j1)) * conjg(D1_(b,a)) &
               +D2_(a,b)*c(j0:j1))
       end do
    end do
    
  end subroutine ElNuc_hc_2
  subroutine ElNuc_set_print_level(x)
    integer, intent(in) :: x
    print_level_ = x
  end subroutine ElNuc_set_print_level
  function ElNuc_idx(a, i) result(res)
    integer, intent(in) :: a, i
    integer res
    res = (a-1)*nstate_ + i
  end function ElNuc_idx
end module Mod_ElNuc

module Mod_TimeInteKrylovEN
  use Mod_ErrHandle
  use Mod_Timer
  implicit none
  private
  integer :: n_, kn_
  integer :: print_level_
  complex(kind(0d0)), allocatable :: u_(:,:), Hu_(:), kh_(:,:)
  complex(kind(0d0)), allocatable :: ck_(:), kuR_(:,:),kuL_(:,:), kw_(:)
  logical :: TimeInteKrylov_use_timer_=.false.
  type(Obj_Timer) :: timer_
  
  public :: TimeInteKrylov_new, TimeInteKrylov_delete, TimeInteKrylov_calc, TimeInteKrylov_set_print_level,  TimeInteKrylov_use_timer_, n_
contains
  subroutine TimeInteKrylov_new(n, kn)
    integer, intent(in) :: n, kn
    n_ = n
    kn_ = kn
    print_level_ = 0
    if(TimeInteKrylov_use_timer_) then
       call Timer_new(timer_, "Krylov", .true.)
    end if
    allocate(u_(n,kn));   u_=0
    allocate(Hu_(n));  Hu_=0
    allocate(kh_(kn,kn)); kh_=0
    allocate(ck_(kn));    ck_=0
    allocate(kuR_(kn,kn)); kuR_=0    
    allocate(kuL_(kn,kn)); kuL_=0
    allocate(kw_(kn));     kw_=0
  end subroutine TimeInteKrylov_new
  subroutine TimeInteKrylov_delete
    deallocate(u_, Hu_, kh_, ck_, kuR_, kuL_, kw_)
    if(TimeInteKrylov_use_timer_) then
       call Timer_result(timer_)
       call Timer_delete(timer_)
    end if
  end subroutine TimeInteKrylov_delete
  subroutine TimeInteKrylov_set_print_level(x)
    integer, intent(in) :: x
    print_level_ = x
  end subroutine TimeInteKrylov_set_print_level
  subroutine TimeIntekrylov_calc(dt, c)
    use Mod_ElNuc
    use Mod_Const, only : ii
    use Mod_Math, only  : lapack_zgeev, norm
    use Mod_ElNuc
    complex(kind(0d0)), intent(in) :: dt
    complex(kind(0d0)), intent(inout) :: c(:)
    integer k

    if(size(c).ne.n_) then
       begin_err(1)
       write(0,*) "c: invalid size"
       write(0,*) "n:", n_
       write(0,*) "size(c)", size(c)
       end_err()
    end if    
    
    ! -- 1st proces --
    u_(:,1) = c(:)
    u_(:,1) = u_(:,1) / norm(u_(:,1))
    if(TimeInteKrylov_use_timer_) call Timer_begin(timer_, "hc0")
    call ElNuc_hc(c(:), Hu_(:))
    if(TimeInteKrylov_use_timer_) call Timer_end(timer_, "hc0")
    if(TimeInteKrylov_use_timer_) call Timer_begin(timer_, "hc1")
    call ElNuc_hc(u_(:,1), Hu_(:))
    if(TimeInteKrylov_use_timer_) call Timer_end(timer_, "hc1")
    kh_(1,1) = dot_product(u_(:,1), Hu_(:))
    
    if(print_level_>0) then
       write(*,*) "u(:2,1)", u_(:2,1)
       write(*,*) "kh(1,1)", kh_(1,1)
    end if
    
    ! -- 2nd process --
    u_(:,2) = Hu_(:) - kh_(1,1)*u_(:,1)
    u_(:,2) = u_(:,2) / norm(u_(:,2))
    call ElNuc_hc(u_(:,2), Hu_(:))
    kh_(2,2) = dot_product(u_(:,2), Hu_(:))
    kh_(1,2) = dot_product(u_(:,1), Hu_(:))
    kh_(2,1) = conjg(kh_(1,2))

    if(print_level_>0) then
       write(*,*) "u(:2,2)", u_(:2,2)
       write(*,*) "kh(1,2)", kh_(1,2)
       write(*,*) "kh(2,2)", kh_(2,2)
    end if

    ! -- proceeding process --
    do k = 2, kn_-1
       u_(:,k+1) = Hu_(:) - kh_(k-1,k)*u_(:,k-1) - kh_(k,k)*u_(:,k)
       u_(:,k+1) = u_(:,k+1) / norm(u_(:,k+1))
       call ElNuc_hc(u_(:,k+1), Hu_(:))
       kh_(k+1,k+1) = dot_product(u_(:,k+1), Hu_(:))
       kh_(k,  k+1) = dot_product(u_(:,k),   Hu_(:))
       kh_(k+1,k)   = conjg(kh_(k,k+1))

       if(print_level_>0) then
          if(k<4) then
             write(*,*) "k = ", k
             write(*,*) "u(:2,k)", u_(:2,k)
             write(*,*) "kh(:2,k)", kh_(:2,k)
          end if
       end if

    end do

    ! -- integration --
    call lapack_zgeev(kh_(:,:), kn_, kw_(:), kuL_(:,:), kuR_(:,:))!; check_err()
    ck_(:) = exp(-ii*kw_*dt) * conjg(kuL_(1,:))
    ck_(:) = matmul(kuR_(:,:), ck_(:))
    c(:)  = matmul(u_(:,:), ck_(:))
    
  end subroutine TimeIntekrylov_calc
end module Mod_TimeInteKrylovEN
