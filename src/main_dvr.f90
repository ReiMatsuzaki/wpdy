#include "macros.fpp"
module Mod_MainDVR
  use Mod_ErrHandle
  use Mod_Timer
  use Mod_TimeInteDiag
  use Mod_TimeInteKrylov
  implicit none
  type(Obj_Timer) :: timer_
  double precision :: dt_
  integer :: nstate_, nt_, ntskip_
  complex(kind(0d0)), allocatable :: c_(:)
  character(10) :: inte_
contains
  subroutine MainDVR_run
    write(*,*)
    write(*,*) "WPDY_DVR program begin"
    write(*,*)

    call new
    call calc
    call delete
    
    write(*,*) 
    write(*,*) "WPDY_DVR program end"
    write(*,*) 
  end subroutine MainDVR_run
  subroutine new
    use Mod_ElNuc
    use Mod_ArgParser
    use Mod_sys
    integer :: n, nstate
    double precision :: x0, x1
    character(100) :: fn_hel, fn_xij, fn_psi0
    integer, parameter :: ifile1 = 2511
    integer, parameter :: ifile2 = 2512
    integer, parameter :: ifile3 = 2513
    double precision :: re, im, mass
    complex(kind(0d0)), allocatable :: h(:,:)
    integer :: i,j,k,a,kn, nn
    
    call ErrHandle_new
    call Timer_new(timer_, "WPDY_DVR", .false.)
    
    call arg_parse_i("-dvr_n", n); check_err()
    call arg_parse_d("-dvr_x0", x0); check_err()
    call arg_parse_d("-dvr_x1", x1); check_err()

    call arg_parse_s("-fn_psi0", fn_psi0); check_err()
    call arg_parse_s("-fn_hel", fn_hel); check_err()
    call arg_parse_s("-fn_xij", fn_xij); check_err()

    call arg_parse_d("-mass", mass); check_err()
    call arg_parse_i("-nstate", nstate); check_err()

    call arg_parse_d("-dt", dt_); check_err()
    call arg_parse_i("-nt", nt_); check_err()
    call arg_parse_i("-ntskip", ntskip_); check_err()

    call arg_parse_s("-inte", inte_); check_err()    

    call ExpDVR_new(n, x0, x1); check_err()
    call ElNuc_new(mass, nstate); check_err()
    allocate(c_(nstate_*num_))

    call open_r(ifile3, fn_psi0); check_err()
    read(ifile3)
    do
       read(ifile3, *, end=102) a, i, re, im
       ! see dvr.f90:ExpDVR_fit
       c_(ElNuc_idx(a, i)) = dcmplx(re, im) * sqrt(ws_(a))
    end do
102 close(ifile3)
    
    call open_r(ifile1, fn_hel); check_err()
    read(ifile1)
    do
       read(ifile1, *, end=100) i, j, k, re, im
       Hel_(i,j,k) = dcmplx(re, im)
    end do
100 close(ifile1)
    
    call open_r(ifile2, fn_xij); check_err()
    read(ifile2)
    do
       read(ifile2, *, end=101) i, j, k, re, im
       Xij_(i,j,k) = dcmplx(re, im)
    end do
101 close(ifile2)

    if(inte_.eq."diag") then
       nn = num_*nstate_
       allocate(h(nn,nn))
       call ElNuc_h(h); check_err()
       call TimeInteDiag_new(h); check_err()
       deallocate(h)
    else if(inte_.eq."krylov") then
       call arg_parse_i("-krylov_num", kn); check_err()
       call TimeInteKrylov_new(nstate_*num_, kn); check_err()
    else
       throw_err("invalid inte", 1)
    end if
    
  end subroutine new
  subroutine calc
    use Mod_ElNuc
    use Mod_sys
    integer :: it, itt
    character(100) :: out_it
    complex(kind(0d0))  :: dt
    do it = 1, nt_/ntskip_
       
       dt = (it-1) * ntskip_ * dt
       write(out_it, '("out/", I0)') it       
       call mkdirp_if_not(out_it); check_err()

       if(inte_.eq."diag") then
          do itt = 1, ntskip_
             call TimeInteDiag_calc(dt, c_(:)); check_err()
          end do
       else if(inte_.eq."krylov") then
          do itt = 1, ntskip_
             call TimeInteKrylov_calc(ElNuc_hc, dt, c_(:)); check_err()
          end do
       end if
       
    end do
  end subroutine calc
  subroutine delete
    if(inte_.eq."diag") then
       call TimeInteDiag_delete
    else if(inte_.eq."krylov") then
       call TimeInteKrylov_delete
    else
       throw_err("unsupported inte", 1)
    end if
    call ErrHandle_delete
    call Timer_delete(timer_)
  end subroutine delete
end module Mod_MainDVR
program main
  use Mod_MainDVR
  call MainDVR_run
end program main
