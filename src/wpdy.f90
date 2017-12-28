module Mod_WPDy
  implicit none
  integer :: nx_, nstate_
  double precision, allocatable :: xs_(:), vs_(:,:,:), csv_(:), frs_(:,:), frs0_(:,:)
contains
  subroutine WPDy_new(nx, nstate)
    integer nx, nstate
    nx_ = nx
    nstate_ = nstate_

    allocate(xs_(nx))                     ! grid points
    allocate(vs_(nstate, nstate, nx))     ! potential matrix
    allocate(cvs_(nstate, nstate, nx))    ! imaginary part of potential matrix
    allocate(frs_(nstate, 0:2*nx-1))      ! real and imaginary part of WP
    allocate(frs0_(nstate, 0:2*nx-1))     ! WP at t = 0
    
  end subroutine WPDy_new
  subroutine WPDy_set_xs(x0, dx)
    double precision, intent(in) :: x0, dx
    integer i
    do i = 1, nx_
       xs_(i) = x0 + dx * (i-1)
    end do
  end subroutine WPDy_set_xs
end module WavePacketDynamics
