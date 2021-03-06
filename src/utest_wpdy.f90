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
    integer ix, it
    double precision :: x0, p0, norm2
    complex(kind(0d0)) :: f
    
    x0 = 1.0d0
    p0 = 1.0d0
    call WPDy_new(1, -10.0d0, 20.0d0/(2*256), 256*2); check_err()
    do ix = 1, nx_
       f = exp(-(xs_(ix)-x0)**2 + ii*p0*(xs_(ix)-x0))
       frs_(1,2*(ix-1))   = real(f)
       frs_(1,2*(ix-1)+1) = aimag(f)
       vs_(1,1,ix) = xs_(ix)**2
    end do
    call WPDy_SplitOp_setup
    norm2 = WPDy_rn(0)
    call expect_eq(1.0d0, norm2)
    do it = 1, 2
       call WPDy_SplitOp_inte((0.1d0, 0.0d0))
       write(*,*) frs_(1,1:10)
    end do
    norm2 = WPDy_rn(0)    
    call expect_eq(1.0d0, norm2)
    
  end subroutine Test1
  subroutine Test2
    use Mod_const, only : ii
    integer ix, it
    double precision :: x0, p0, norm2
    complex(kind(0d0)) :: f
    
    x0 = 1.0d0
    p0 = 1.0d0
    call WPDy_new(2, -5.0d0, 0.1d0, 100); check_err()
    do ix = 1, nx_
       f = exp(-(xs_(ix)-x0)**2 + ii*p0*(xs_(ix)-x0))
       frs_(1,2*(ix-1))   = real(f)
       frs_(1,2*(ix-1)+1) = aimag(f)
       vs_(1,1,ix) = xs_(ix)**2
       vs_(2,2,ix) = xs_(ix)**2 + 1.0d0
       vs_(1,2,ix) = exp(-xs_(ix)**2)
       vs_(2,1,ix) = exp(-xs_(ix)**2)
    end do
    call WPDy_SplitOp_setup
    norm2 = WPDy_rn(0)    
    do it = 1, 10
       call WPDy_SplitOp_inte((0.1d0, 0.0d0))
    end do
    call expect_eq(1.0d0, norm2)
    
  end subroutine Test2
end module Mod_TestWpdy

subroutine Mod_TestDVR
  
end subroutine Mod_TestDVR

program main
  use Mod_Utest
  use Mod_UtestCheck
  use Mod_TestWpdy  

  call utest_new

  call TestWpdy_run

  !  call Timer_result
  call utest_delete
  
end program main
