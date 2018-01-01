#include "macros.fpp"

subroutine run
  use Mod_ErrHandle
  use Mod_fjson
  use Mod_sys
  use Mod_WPDy_SplitOp
  use Mod_TimeStep
  implicit none
  integer nx, it, itt, io, ix, istate, is
  integer :: ifile = 12841
  double precision dx, t, re, im
  type(value) :: v
  complex(kind(0d0)) :: cdt
  character(10) :: out_it

  call ErrHandle_new

  ! ==== load ====
  call loads_json_file("wpdy.in.json", ifile, v)
  if(get_err().ne.0) then
     begin_err(1)
     write(0,*) "failed open file"
     end_err()
  end if

  if(v%type .ne. TYPE_O) then
     throw_err("json value must be object", 1)
  end if  
  nx = 100
  dx = 0.1d0
  call object_get_idx(v%val_o, "wpdy", io); check_err()
  call WPDy_new_json(v%val_o%vals(io)%val_o); check_err()
  call open_r(ifile, "psi0.csv"); check_err()
  read(ifile,*)
  do
     read(ifile, *, end=100) ix, istate, re, im
     frs_(istate+1, 2*ix)   = re
     frs_(istate+1, 2*ix+1) = im
  end do
100 close(ifile)  
  ifile = ifile + 1
  call WPDy_setup ; check_err()
  call WPDy_SplitOp_setup ; check_err()

  call object_get_idx(v%val_o, "timestep", io); check_err()  
  call TimeStep_new_json(v%val_o%vals(io)%val_o); check_err()
  cdt = dt_

  ! ==== dump ====
  call mkdir_if_not("out"); check_err()
  call open_w(ifile, "out/xs.csv"); check_err()
  write(ifile,*) "i,val"
  do ix = 1, nx_
     write(ifile,*) ix-1, xs_(ix)
  end do  

  ! ==== calculation ====
  do it = 0, nt_/ntskip_
     t = (it-1)*dt_     
     
     write(out_it, '("out/", I0)') it
     call mkdir_if_not(out_it); check_err()
     call WPDy_dump_coef(out_it); check_err()

     call open_w(ifile, trim(out_it)//"/norm.csv"); check_err()
     write(ifile,*) "i,val"
     write(ifile,'("1,",f20.10)') sqrt(WPDy_rn(0))
     close(ifile)
     ifile = ifile + 1
     
     call open_w(ifile, trim(out_it)//"/r.csv"); check_err()
     write(ifile,*) "i,val"
     write(ifile,'("1,",f20.10)') WPDy_rn(1)
     close(ifile)
     ifile = ifile + 1

     call open_w(ifile, trim(out_it)//"/r2.csv"); check_err()
     write(ifile,*) "i,val"
     write(ifile,'("1,",f20.10)')  WPDy_rn(2)
     close(ifile)
     ifile = ifile + 1

     call open_w(ifile, trim(out_it)//"/prob.csv"); check_err()
     write(ifile,*) "i,val"
     do is = 1, nstate_
        write(ifile,'(i0,",",f20.10)')  is-1, WPDy_prob(is)
     end do

     do itt = 1, ntskip_
        call WPDy_SplitOp_inte(cdt); check_err()
     end do
  end do

  call ErrHandle_delete
  
end subroutine run

program main
  use Mod_ErrHandle
  write(*,*) "wpdy program start"
  call run
  if(get_err().eq.0) then
     write(*,*) "wpdy calculation succeed"
  else
     write(0,*) "Error in wpdy calculation"
     write(0,*) "stop wpdy program..."
  end if
end program main
