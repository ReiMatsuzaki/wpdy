module Mod_TestWpdy
  use Mod_utest_check
  use Mod_WPDy_SplitOp  
  implicit none
contains
  subroutine Test1
    call expect_eq(2.0d0, 1.0d0+1.0d0)
  end subroutine Test1
end module Mod_TestWpdy
