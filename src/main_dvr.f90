#include "macros.fpp"

module Mod_MainDVR
  use Mod_ErrHandle
  use Mod_Timer
  implicit none
  type(Obj_Timer) :: timer_
  double precision :: dt_
  complex(kind(0d0)) :: cdt_
  integer :: nt_, ntskip_
  complex(kind(0d0)), allocatable :: c_(:)
  character(10) :: inte_
contains
  subroutine MainDVR_run
    use Mod_ArgParser
    character(10) :: runtype
    write(*,*)
    write(*,*) "WPDY_DVR program begin"
    write(*,*)

    call ErrHandle_new
    call Timer_new(timer_, "WPDY_DVR", .true.)

    if(arg_parse_exist("-runtype")) then
       call arg_parse_s("-runtype", runtype)
    else
       runtype = "dynamics"
    end if
    write(*,*) "runtype: ", runtype
    if(runtype.eq."dynamics") then
       call new
       call calc
       call delete
    else if(runtype.eq."check_hc") then
       call new
       call check_hc
    else
       throw_err("invalid runtype", 1)
    end if

    call Timer_result(timer_)
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
    use Mod_TimeInteDiag, only : TimeInteDiag_new, TimeInteDiag_set_print_level
    use Mod_TimeInteKrylov, only : TimeInteKrylov_new, TimeInteKrylov_set_print_level

    integer :: n, kn
    double precision :: x0, xN
    character(100) :: fn_hel, fn_xij, fn_psi0
    double precision :: mass
    integer print_level

    call Timer_begin(timer_, "parse_arg")
    call arg_parse_i("-dvr_n", n); check_err()
    call arg_parse_d("-dvr_x0", x0); check_err()
    call arg_parse_d("-dvr_xN", xN); check_err()

    call arg_parse_s("-fn_psi0", fn_psi0); check_err()
    call arg_parse_s("-fn_hel", fn_hel); check_err()
    call arg_parse_s("-fn_xij", fn_xij); check_err()

    call arg_parse_d("-mass", mass); check_err()
    call arg_parse_i("-nstate", nstate_); check_err()

    call arg_parse_d("-dt", dt_); check_err()
    cdt_ = dt_
    call arg_parse_i("-nt", nt_); check_err()
    call arg_parse_i("-ntskip", ntskip_); check_err()
    write(*,*) "dt:", cdt_
    write(*,*) "nt:", nt_
    write(*,*) "ntskip:", ntskip_
    write(*,*) "mass:", mass

    call arg_parse_s("-inte", inte_); check_err()
    if(arg_parse_exist("-print_level")) then
       call arg_parse_i("-print_level", print_level)
    end if
    
    call Timer_end(timer_, "parse_arg")

    ! -- initialization --
    call Timer_begin(timer_, "initialize")
    call ExpDVR_new(n, x0, xN); check_err()
    call ElNuc_new(mass, nstate_); check_err()
    call ElNuc_set_print_level(print_level)
    allocate(c_(nstate_*num_))
    call Timer_end(timer_, "initialize")

    ! -- read data and dump --    
    call new_read(fn_psi0, fn_hel, fn_xij)
    call new_dump

    ! -- normalization --
    c_(:) = c_(:) / sqrt(sum(abs(c_(:))**2))

    ! -- time integrator --    
    if(inte_.eq."diag") then
       call TimeInteDiag_new(nstate_*num_); check_err()
       call TimeInteDiag_set_print_level(print_level)
    else if(inte_.eq."krylov") then
       call arg_parse_i("-krylov_num", kn); check_err()
       call TimeInteKrylov_new(nstate_*num_, kn); check_err()
       call TimeInteKrylov_set_print_level(print_level)
    else
       throw_err("invalid inte", 1)
    end if
    
  end subroutine new
  subroutine new_read( fn_psi0, fn_hel, fn_xij)
    use Mod_ElNuc
    integer :: ifile = 23421
    character(100), intent(in) :: fn_hel, fn_xij, fn_psi0
    integer a, i, j, k
    double precision re, im

    call Timer_begin(timer_, "new_read")
    
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

    call Timer_end(timer_, "new_read")
    
  end subroutine new_read
  subroutine new_dump
    use Mod_ElNuc, only : nstate_, m_
    use Mod_ExpDVR, only : n_, x0_, xN_, xs_, ws_, num_
    use Mod_Sys, only : mkdirp_if_not    
    integer :: ifile = 23433
    integer a

    call Timer_begin(timer_, "new_read")
    
    call mkdirp_if_not("out"); check_err()
    
    call open_w(ifile, "out/ws.csv")
    write(ifile, '(A)') "val"
    do a = 1, num_
       write(ifile, '(F20.10)') ws_(a)
    end do
    close(ifile)
    ifile = ifile + 1

    call open_w(ifile, "out/xs.csv")
    write(ifile, '(A)') "val"
    do a = 1, num_
       write(ifile, '(F20.10)') xs_(a)
    end do
    close(ifile)
    ifile = ifile + 1

    call open_w(ifile, "out/ts.csv")
    write(ifile, '(A)') "val"
    do a = 0, nt_/ntskip_
       write(ifile, '(F20.10)') a*dt_*ntskip_
    end do
    ifile = ifile + 1
    close(ifile)
    
    call open_w(ifile, "out/params.json")
    write(ifile, '(A)') "{"
    write(ifile, '(A, i0, A)')     '   "nstate": ', nstate_, ","
    write(ifile, '(A, f20.10, A)') '   "mass": '  , m_,      ","
    write(ifile, '(A, i0, A)')     '   "dvr_n":  ', n_,      ","
    write(ifile, '(A, f20.10, A)') '   "dvr_x0": ', x0_,     ","
    write(ifile, '(A, f20.10)')    '   "dvr_xN": ', xN_
    write(ifile, '(A)') "}"
    close(ifile)
    ifile = ifile + 1

    call open_w(ifile, "out/dvr.json")
    write(ifile,'(A)') "{"
    write(ifile,'(A)') "}"
    close(ifile)
    call Timer_end(timer_, "new_read")
  end subroutine new_dump
  subroutine calc
    use Mod_ElNuc
    use Mod_sys
    use Mod_TimeInteDiag, only : TimeInteDiag_precalc, TimeInteDiag_calc
    use Mod_TimeInteKrylov, only : TimeInteKrylov_calc
    integer :: it, itt
    character(100) :: out_it, fn_coef
    complex(kind(0d0))  :: t
    integer :: ifile = 23412
    integer :: i, nn
    complex(kind(0d0)), allocatable :: h(:,:)

    ! -- precalc --
    call Timer_begin(timer_, "precalc")    
    if(inte_.eq."diag") then
       nn = num_*nstate_
       allocate(h(nn,nn))
       call ElNuc_h(h); check_err()
       call TimeInteDiag_precalc(h); check_err()
       deallocate(h)
    end if
    call Timer_end(timer_, "precalc")
    
    ! -- time loop --
    do it = 0, nt_/ntskip_

       write(*,*) "wpdy_dvr ", it, "/", nt_/ntskip_
       call Timer_begin(timer_, "main_calc")
       t = it * ntskip_ * dt_
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

       write(*,*) "integrate t"
       if(inte_.eq."diag") then
          do itt = 1, ntskip_
             call TimeInteDiag_calc(cdt_, c_(:)); check_err()
          end do
       else if(inte_.eq."krylov") then
          do itt = 1, ntskip_
             call TimeInteKrylov_calc(ElNuc_hc, cdt_, c_(:)); check_err()
          end do
       end if
       call Timer_end(timer_, "main_calc")
    end do
        
  end subroutine calc
  subroutine check_hc
    use Mod_ElNuc, only : nstate_, ElNuc_h, ElNuc_hc
    use Mod_ExpDVR, only : num_
    complex(kind(0d0)), allocatable :: h(:,:), c0(:), c1(:)
    integer :: nn
    
    call Timer_begin(timer_, "check_hc")
    
    nn = num_*nstate_
    allocate(h(nn,nn), c0(nn), c1(nn))
    
    call ElNuc_h(h(:,:)); check_err()
    c0(:) = matmul(h(:,:), c_(:))
    call ElNuc_hc(c_(:), c1(:))

    write(*,*) "diff:", sum(abs(c1-c0))/size(c0)

    call Timer_end(timer_, "check_hc")
  end subroutine check_hc
  subroutine delete
    use Mod_TimeInteDiag, only : TimeInteDiag_delete
    use Mod_TimeInteKrylov, only : TimeInteKrylov_delete
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
