module Mod_TimeStep
  use Mod_ErrHandle
  implicit none
  double precision  dt_
  integer :: nt_, ntskip_
contains
  subroutine TimeStep_new
    dt_ = 0.1
    nt_ = 100
    ntskip_ = 1
  end subroutine TimeStep_new
  subroutine TimeStep_new_json(o)
    use Mod_fjson
    type(object) :: o
    call object_get_i(o, "nt", nt_)
    call object_get_d(o, "dt", dt_)
    call object_get_i(o, "ntskip", ntskip_)
    write(*,*) "TimeStep_new_json begin"
    write(*,*) "nt:", nt_
    write(*,*) "dt:", dt_
    write(*,*) "redt:", dt_
    write(*,*) "ntskip:", ntskip_
    write(*,*) "TimeStep_new_json end"
  end subroutine TimeStep_new_json
  subroutine TimeStep_dump(ifile)
    use mod_fjson
    integer, intent(in) :: ifile
    type(object) :: o
    call object_set_i(o, "nt", nt_)
    call object_set_i(o, "ntskiip_", ntskip_)
    call object_set_d(o, "redt", dt_)
    call object_dump(o, ifile)
  end subroutine TimeStep_dump
end module Mod_TimeStep
