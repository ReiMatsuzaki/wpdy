#include "macros.fpp"

subroutine run
  use Mod_ErrHandle
  use Mod_fjson
  use Mod_sys
  use Mod_WPDy_SplitOp
  use Mod_TimeStep
  implicit none
  integer it, itt, io, ix, istate, jstate
  integer :: ifile = 12841
  double precision t, re, im
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

  call object_get_idx(v%val_o, "wpdy", io); check_err()
  call WPDy_new_json(v%val_o%vals(io)%val_o); check_err()
  
  ! -- psi0 --
  call open_r(ifile, "psi0.idx.csv"); check_err()
  read(ifile,*)
  do
     read(ifile, *, end=100) istate, ix, re, im
     frs_(istate, 2*(ix-1))   = re
     frs_(istate, 2*(ix-1)+1) = im
  end do  
100 close(ifile)
  ifile = ifile + 1
  ! -- V --
  call open_r(ifile, "v.idx.csv"); check_err()
  read(ifile,*)
  do
     read(ifile, *, end=101) istate, jstate, ix, re
     vs_(istate, jstate, ix) = re
  end do
101 close(ifile)
  ifile = ifile + 1
  ! -- WPDy setup --
  call WPDy_setup ; check_err()
  call WPDy_SplitOp_setup ; check_err()

  call object_get_idx(v%val_o, "timestep", io); check_err()  
  call TimeStep_new_json(v%val_o%vals(io)%val_o); check_err()
  cdt = dt_

  ! ==== dump ====
  call mkdir_if_not("out"); check_err()

  call open_w(ifile, "out/xs.csv"); check_err()
  write(ifile,'("val")')
  do ix = 1, nx_
     write(ifile,*) xs_(ix)
  end do
  close(ifile)
  ifile = ifile + 1
  
  call open_w(ifile, "out/ts.csv"); check_err()
  write(ifile,'("val")')
  do it = 0, nt_/ntskip_
     write(ifile,*) it*dt_*ntskip_
  end do
  close(ifile)
  ifile = ifile + 1  

  ! ==== calculation ====
  do it = 0, nt_/ntskip_
     t = (it-1)*dt_     
     
     write(out_it, '("out/", I0)') it
     call mkdir_if_not(out_it); check_err()
     call WPDy_dump_coef(out_it); check_err()
     
     !call open_w(ifile, trim(out_it)//"/norm.csv"); check_err()
     !write(ifile,*) "i,val"
     !write(ifile,'("1,",f20.10)') sqrt(WPDy_rn(0))
     !close(ifile)
     !ifile = ifile + 1
     !
     !call open_w(ifile, trim(out_it)//"/r.csv"); check_err()
     !write(ifile,*) "i,val"
     !write(ifile,'("1,",f20.10)') WPDy_rn(1)
     !close(ifile)
     !ifile = ifile + 1
!
!     call open_w(ifile, trim(out_it)//"/r2.csv"); check_err()
!     write(ifile,*) "i,val"
!     write(ifile,'("1,",f20.10)')  WPDy_rn(2)
!     close(ifile)
!     ifile = ifile + 1
!
!     call open_w(ifile, trim(out_it)//"/prob.csv"); check_err()
!     write(ifile,*) "i,val"
!     do istate = 1, nstate_
!        write(ifile,'(i0,",",f20.10)')  istate-1, WPDy_prob(is)
!     end do

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
