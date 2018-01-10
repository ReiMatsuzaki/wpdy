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

    call ErrHandle_new
    call Timer_new(timer_, "WPDY_DVR", .false.)
    
    call new
    call calc
    call delete

    call ErrHandle_delete
    call Timer_delete(timer_)
    
    write(*,*) 
    write(*,*) "WPDY_DVR program end"
    write(*,*) 
  end subroutine MainDVR_run
  subroutine new
    use Mod_ElNuc
    use Mod_ArgParser
    use Mod_sys
    integer :: n, nstate
    double precision :: x0, xN
    character(100) :: fn_hel, fn_xij, fn_psi0
    double precision :: mass
    complex(kind(0d0)), allocatable :: h(:,:)
    integer :: kn, nn
        
    call arg_parse_i("-dvr_n", n); check_err()
    call arg_parse_d("-dvr_x0", x0); check_err()
    call arg_parse_d("-dvr_xN", xN); check_err()

    call arg_parse_s("-fn_psi0", fn_psi0); check_err()
    call arg_parse_s("-fn_hel", fn_hel); check_err()
    call arg_parse_s("-fn_xij", fn_xij); check_err()

    call arg_parse_d("-mass", mass); check_err()
    call arg_parse_i("-nstate", nstate); check_err()

    call arg_parse_d("-dt", dt_); check_err()
    call arg_parse_i("-nt", nt_); check_err()
    call arg_parse_i("-ntskip", ntskip_); check_err()

    call arg_parse_s("-inte", inte_); check_err()    

    ! -- initialization --
    call ExpDVR_new(n, x0, xN); check_err()
    call ElNuc_new(mass, nstate); check_err()
    allocate(c_(nstate_*num_))

    ! -- read data and dump --
    call new_read(fn_psi0, fn_hel, fn_xij)
    call new_dump

    ! -- normalization --
    c_(:) = c_(:) / sqrt(sum(abs(c_(:))**2))

    ! -- precalculation --
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
  subroutine new_dump
    use Mod_ExpDVR
    use Mod_Sys
    integer :: ifile = 23433
    integer a
    
    call mkdirp_if_not("out"); check_err()
    
    call open_w(ifile, "out/ws.csv")
    write(ifile, '(A)') "val"
    do a = 1, num_
       write(ifile, '(F20.10)') ws_(a)
    end do
    ifile = ifile + 1

    call open_w(ifile, "out/xs.csv")
    write(ifile, '(A)') "val"
    do a = 1, num_
       write(ifile, '(F20.10)') xs_(a)
    end do
    ifile = ifile + 1
    
  end subroutine new_dump
  subroutine new_read( fn_psi0, fn_hel, fn_xij)
    use Mod_ElNuc
    integer :: ifile = 23421
    character(100), intent(in) :: fn_hel, fn_xij, fn_psi0
    integer a, i, j, k
    double precision re, im
    
    call open_r(ifile, fn_psi0); check_err()
    read(ifile, '()')
    do
       read(ifile, *, end=102) a, i, re, im
       ! see dvr.f90:ExpDVR_fit
       c_(ElNuc_idx(a, i)) = dcmplx(re, im) * sqrt(ws_(a))
    end do
102 close(ifile)
    ifile = ifile + 1
    
    call open_r(ifile, fn_hel); check_err()
    read(ifile, '()')
    do
       read(ifile, *, end=100) i, j, k, re, im
       Hel_(i,j,k) = dcmplx(re, im)
    end do
100 close(ifile)
    ifile = ifile + 1
    
    call open_r(ifile, fn_xij); check_err()
    read(ifile, '()')
    do
       read(ifile, *, end=101) i, j, k, re, im
       Xij_(i,j,k) = dcmplx(re, im)
    end do
101 close(ifile)
    ifile = ifile + 1
    
  end subroutine new_read
  subroutine calc
    use Mod_ElNuc
    use Mod_sys
    integer :: it, itt
    character(100) :: out_it, fn_coef
    complex(kind(0d0))  :: dt
    integer :: ifile = 23412
    integer :: i
    do it = 0, nt_/ntskip_
       
       dt = it * ntskip_ * dt
       write(out_it, '("out/", I0)') it       
       call mkdirp_if_not(out_it); check_err()

       fn_coef = trim(out_it) // "/coef.csv"
       call open_w(ifile, fn_coef)
       write(ifile, '(A)') "re,im"
       do i = 1, size(c_(:))
          write(ifile, '(F20.10, ",", F20.10)') real(c_(i)), aimag(c_(i))
       end do
       close(ifile)
       ifile = ifile + 1

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
  end subroutine delete
end module Mod_MainDVR
program main
  use Mod_MainDVR
  call MainDVR_run
end program main
