program machine_zero
  implicit none
  real :: Z, two_times_Z   
  integer :: iterations     


  Z = 1.0
  iterations = 0

  do
    two_times_Z = 2.0 * Z   

 
    if (two_times_Z > Z) then
      Z = Z / 2.0
      iterations = iterations + 1   
    else
      exit    
    end if
  end do




  print *, "Machine zero is: ", Z
  print *, "Number of iterations: ", iterations

end program machine_zero
