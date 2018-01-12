program main
  !$ use omp_lib
  implicit none
  integer i
  write(*,*) "begin"
  
  !$omp parallel
  write(*,*) "N=", omp_get_num_threads(), ",", omp_get_thread_num()
  !$omp end parallel

  !$omp parallel do
  do i = 1, 5
     write(*,*) i
  end do
  !$omp end parallel do
  
  write(*,*) "end"
end program main
