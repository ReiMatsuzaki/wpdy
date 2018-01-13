#include "macros.fpp"

module Mod_main
  use Mod_ErrHandle
  implicit none
contains
  subroutine run
    use Mod_UtestCheck
    !$ use omp_lib
    !$omp parallel
    if(omp_get_thread_num().eq.0) then
       write(*,*) "thread number:", omp_get_num_threads()
    end if
    !$omp end parallel    

    call Utest_sub_begin("hc_time0")
    call test_hc_time(0)
    call Utest_sub_end

    call Utest_sub_begin("hc_time3")
    call test_hc_time(3)
    call Utest_sub_end    

    call Utest_sub_begin("krylov")
    call test_krylov
    call Utest_sub_end    
    
  end subroutine run
  subroutine test_hc_time(hc_method)
    use Mod_ElNuc
    use Mod_ExpDVR
    integer :: hc_method
    integer, parameter :: n = 128
    integer, parameter :: num = 2*n+1
    integer, parameter :: nstate = 50
    integer, parameter :: nn = num*nstate
    double precision, parameter :: m = 1.2d0
    double precision, parameter :: w = 1.0d0
    double precision, parameter :: a = m*w/2
    double precision, parameter :: x0 = 0.0
    double precision, parameter :: p0 = 0.0    
    complex(kind(0d0)) :: g0(num), cg0(num), c0(nn), hc0(nn)
    integer i, j

    ! == results at athena49.cluster ==
    !         |   -O0  | -O3   |
    ! core=1  |  9.0s  | 1.6s  |
    ! core=12 |  1.6s  | 0.6s,0.564  |

    ! -- initialize --
    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call ElNuc_new(m, nstate)    

    ! -- Matrix --
    do i = 1, nstate       
       do j = 1, nstate
          Hel_(:,i,j) = m*w*w/2 *xs_(:)**2 + (i+j)/100
          Xij_(:,i,j) = exp(-(xs_(:)-1)**2) * (i-j)/100
       end do
    end do

    ! -- coefficient --
    g0(:) = exp(-a*(xs_-x0)**2 + ii*p0*(xs_-x0))
    call ExpDVR_fit(g0(:), cg0(:))
    cg0(:) = cg0(:) / sqrt(real(dot_product(cg0, cg0)))
    c0(:) = 0
    do i = 1, num
       c0(2*i-1) = cg0(i)
    end do
    
    ! -- Hc by direct --
    hc_method_ = hc_method
    call ElNuc_hc(c0(:), hc0(:))
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete
        
  end subroutine test_hc_time
  subroutine test_krylov
    use Mod_TimeInteKrylov
    use Mod_ElNuc
    use Mod_Timer
    integer, parameter :: n = 128    
    integer, parameter :: num = 2*n+1
    integer, parameter :: nstate = 50
    double precision, parameter :: x0 = 1.0
    double precision, parameter :: p0 = 0.0
    complex(kind(0d0)) :: dt = 1.0d0
    integer, parameter :: nn=num*nstate
    complex(kind(0d0)) :: c(nn)
    complex(kind(0d0)) :: h(nn,nn)
    integer, parameter :: nt = 1
    type(Obj_Timer) :: timer
    integer i, j

    ! -- initialize --
    call ExpDVR_new(n, -5.0d0, 5.0d0)
    call ElNuc_new(2.1d0, nstate)
    call Timer_new(timer, "time_inte2", .false.)

    ! -- Matrix --
    do i = 1, nstate
       do j = 1, nstate
          Hel_(:,i,j) = xs_(:)**2 + (i+j)/100
          Xij_(:,i,j) = exp(-(xs_(:)-1)**2) * (i-j)/100
       end do
    end do

    ! -- coefficient --
    c(:) = 1.0d0
    
    ! -- time integration by Krylov --
    call Timer_begin(timer, "inte_krylov")
    TimeInteKrylov_use_timer_ = .true.
    call TimeInteKrylov_new(nn, 10)
    call TimeInteKrylov_calc(ElNuc_hc, dt, c(:))
    !    call TimeInteKrylov_calc(dt, c(:))
    call TimeInteKrylov_delete
    call Timer_end(timer, "inte_krylov")
    
    ! -- finalize --
    call ExpDVR_delete
    call ElNuc_delete
    
    call Timer_result(timer)
    call Timer_delete(timer)

  end subroutine test_krylov
end module Mod_main

program main
  use Mod_Utest
  use Mod_UtestCheck
  use Mod_main

  call ErrHandle_new
  call Utest_new
  
  call run

  call Utest_delete
  call ErrHandle_delete
  
end program main

