#include "macros.fpp"

module Mod_ArgParser
  use Mod_ErrHandle  
  implicit none
contains
  subroutine throw_err_failed_find_name(name)
    character(*), intent(in) :: name
    begin_err(1)
    write(0,*) "failed to find option"
    write(0,*) "name:", name
    end_err()
  end subroutine throw_err_failed_find_name
  subroutine arg_parse_i(name, x)
    use Mod_StrUtil, only : convert_i
    character(*), intent(in) :: name
    integer, intent(out) :: x
    integer i, num
    character(100) :: ele
    num = iargc()
    do i = 1, num
       call getarg(i, ele)       
       if(name.eq.ele) then
          call getarg(i+1, ele)
          call convert_i(ele, x)
          return
       end if
    end do
    call throw_err_failed_find_name(name) ; check_err()
  end subroutine arg_parse_i
  subroutine arg_parse_d(name, x)
    use Mod_StrUtil, only : convert_d
    character(*), intent(in) :: name
    double precision, intent(out) :: x
    integer i, num
    character(100) :: ele
    num = iargc()
    do i = 1, num
       call getarg(i, ele)
       if(name.eq.ele) then
          call getarg(i+1, ele)
          call convert_d(ele, x)
          return
       end if
    end do
    call throw_err_failed_find_name(name)
  end subroutine arg_parse_d
  subroutine arg_parse_s(name, x)
    character(*), intent(in) :: name
    character(*), intent(out) :: x
    integer i, num
    character(100) :: ele
    num = iargc()
    do i = 1, num
       call getarg(i, ele)
       if(name.eq.ele) then
          call getarg(i+1, ele)
          x = ele
          return
       end if
    end do
    call throw_err_failed_find_name(name)
  end subroutine arg_parse_s
  subroutine arg_parse_dvec(name, xs)
    use Mod_StrUtil, only : convert_d
    character(*), intent(in) :: name
    double precision, intent(out) :: xs(:)
    integer i, num, j
    character(100) :: ele

    num = iargc()
    do i = 1, num
       call getarg(i, ele)
       if(name.eq.ele) then
          do j = 1, num-i
             call getarg(i+j, ele)
             !if(ele(1:1).eq."-") then
             !   return
             !end if
             call convert_d(ele, xs(j))
          end do
          return
       end if
    end do
    call throw_err_failed_find_name(name)   
  end subroutine arg_parse_dvec
  function arg_parse_exist(name) result(res)
    character(*), intent(in) :: name
    logical :: res
    integer i, num
    character(100) :: ele
    
    num = iargc()    
    do i = 1, num
       call getarg(i, ele)
       if(trim(name).eq.trim(ele)) then
          res = .true.
          return
       end if
    end do

    res = .false.
    
  end function arg_parse_exist
end module Mod_ArgParser
