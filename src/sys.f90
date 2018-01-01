#include "macros.fpp"

module mod_sys
  implicit none
contains
  subroutine mkdir(path)
    character(*), intent(in) :: path
    character(100) cmd
    write(cmd, "( 'mkdir ', A )") trim(path)
    call system(cmd)
  end subroutine mkdir
  subroutine mkdir_if_not(path)
    character(*), intent(in) :: path
    character(1000) cmd
    write(cmd, "('ls ', A, ' > /dev/null 2>&1 || mkdir ', A )") trim(path), trim(path)
    call system(cmd)
  end subroutine mkdir_if_not
  subroutine mkdirp_if_not(path)
    character(*), intent(in) :: path
    character(1000) cmd
    write(cmd, "('ls ', A, ' > /dev/null 2>&1 || mkdir -p ', A )") trim(path), trim(path)
    call system(cmd)
  end subroutine mkdirp_if_not
end module mod_sys

