program machine_infinity
  implicit none
  real :: I, two_times_I  
  integer :: iterations   

  I = 1.0
  iterations = 0

  do
    two_times_I = 2.0 * I 

    if (two_times_I > I) then
      I = two_times_I     
      iterations = iterations + 1
    else
      exit    
    end if
  end do

  print *, "Machine infinity is approximately: ", I
  print *, "Number of iterations to reach infinity: ", iterations


end program machine_infinity
