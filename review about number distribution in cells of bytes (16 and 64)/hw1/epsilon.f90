program machine_epsilon
  implicit none
  real :: epsilon, one_plus_epsilon  
  integer :: iterations              


  epsilon = 1.0
  iterations = 0                   

  do
    one_plus_epsilon = 1.0 + epsilon  


    if (one_plus_epsilon > 1.0) then
      epsilon = epsilon / 2.0       
      iterations = iterations + 1     
    else
      exit                          
    end if
  end do


  print *, "Machine epsilon is: ", epsilon
  print *, "Number of iterations: ", iterations
  print *, "This means the smallest epsilon such that 1 + epsilon > 1 is: ", epsilon
  
end program machine_epsilon
