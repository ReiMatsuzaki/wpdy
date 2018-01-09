#include "macros.fpp"
! electronic nuclear Hamiltonian

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
    
    hc(:) = 0
    do a = 1, num_
       i0 = ElNuc_idx(a,1)
       i1 = ElNuc_idx(a+1,1)-1
       hc(i0:i1) = hc(i0:i1) + matmul(Hel_(a,:,:), c(i0:i1))
       do b = 1, num_
          j0 = ElNuc_idx(b,1)
          j1 = ElNuc_idx(b+1,1)-1
          hc(i0:i1) = hc(i0:i1) &
               - 1/m_*matmul(Xij_(a,:,:), c(j0:j1)) * D1_(a,b) &
               - 1/(2*m_)*D2_(a,b)*c(j0:j1)
       end do
    end do
    
  end subroutine ElNuc_hc
  function ElNuc_idx(a, i) result(res)
    integer, intent(in) :: a, i
    integer res
    res = (a-1)*nstate_ + i
  end function ElNuc_idx
end module Mod_ElNuc
