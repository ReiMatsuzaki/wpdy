#include "macros.fpp"
#include "macros_utest.fpp"

module Mod_TestWpdy
  use Mod_UtestCheck
  use Mod_WPDy_SplitOp  
  implicit none
contains
  subroutine TestWpdy_run
    call Utest_sub_begin("test1")
    call Test1
    call Utest_sub_end()
  end subroutine TestWpdy_run
  subroutine Test1
    use Mod_const, only : ii
    integer nx, ix
    double precision :: x0, p0, norm2
    complex(kind(0d0)) :: f
    
    x0 = 1.0d0
    p0 = 1.0d0
    nx = 100
    
    call WPDy_new(2, nx, 0.1d0)

    do ix = 1, nx
       f = exp(-(xs_(ix)-x0)**2 + ii*p0*(xs_(ix)-x0))
       frs_(1,2*(ix-1))   = real(f)
       frs_(1,2*(ix-1)+1) = aimag(f)
       vs_(1,1,ix) = xs_(ix)**2
    end do
    call WPDy_SplitOp_setup

    norm2 = WPDy_rn(0)
    write(*,*) norm2

    call WPDy_SplitOp_inte((0.1d0, 0.0d0))
    write(*,*) norm2
    
  end subroutine Test1
end module Mod_TestWpdy

program main
  use Mod_Utest
  use Mod_UtestCheck
  use Mod_TestWpdy  

  call utest_new

  call TestWpdy_run

  !  call Timer_result
  call utest_delete
  
end program main
