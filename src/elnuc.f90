#include "macros.fpp"
! electronic nuclear Hamiltonian

module Mod_ElNuc
  use Mod_ExpDVR
  implicit none
  integer :: print_level_
  integer :: nstate_
  double precision :: m_
  complex(kind(0d0)), allocatable :: Hel_(:,:,:), Xij_(:,:,:), D1_(:,:), D2_(:,:)
  !integer :: hc_method_
  !double precision :: hc_tol_
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

    !    hc_method_ = 0
    ! hc_tol_ = 1.0d-10
    
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
    double precision, parameter :: tol = 1.0d-10
    logical :: nonzero(num_)

    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = i0 + nstate_-1
       if(sum(abs(c(i0:i1))) > nstate_*tol) then
          nonzero(a) = .true.
       else
          nonzero(a) = .false.
       end if
    end do

    hc(:) = 0

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
             hc(i0:i1) = hc(i0:i1) &
                  - 1/m_* (matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
                  +1.0d0/2*D2_(a,b)*c(j0:j1))
          end if
       end do
    end do
    !$omp end parallel do

    !
    ! -- slow version --
    !hc(:) = 0
    !do a = 1, num_
    !   i0 = ElNuc_idx(a,1)
    !   !i1 = ElNuc_idx(a+1,1)-1
    !   i1 = i0 + nstate_-1       
    !   hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
    !   do b = 1, num_
    !      j0 = ElNuc_idx(b,1)
    !      j1 = ElNuc_idx(b+1,1)-1
    !      hc(i0:i1) = hc(i0:i1) &
    !           - 1/m_* (matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
    !           +1.0d0/2*D2_(a,b)*c(j0:j1))
    !   end do
    !end do
    
  end subroutine ElNuc_hc
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
