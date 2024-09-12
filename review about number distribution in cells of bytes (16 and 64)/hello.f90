program machine_epsilon
  implicit none
  real :: epsilon, one_plus_epsilon  ! Define variables for epsilon and (1 + epsilon)
  integer :: iterations              ! Variable to track the number of iterations

  ! Initialize epsilon to 1.0 (start with the assumption epsilon = 1)
  epsilon = 1.0
  iterations = 0                     ! Initialize the iteration counter

  ! Loop to find the smallest epsilon such that (1 + epsilon) is greater than 1
  do
    one_plus_epsilon = 1.0 + epsilon   ! Calculate (1 + epsilon)

    ! Check if (1 + epsilon) is still greater than 1
    if (one_plus_epsilon > 1.0) then
      epsilon = epsilon / 2.0          ! If true, halve epsilon
      iterations = iterations + 1      ! Increment the iteration counter
    else
      exit                             ! Exit the loop when (1 + epsilon) is not greater than 1
    end if
  end do

  ! Since the loop overshoots by one step, double epsilon to get the correct value
  epsilon = epsilon * 2.0

  ! Print the final result
  print *, "Machine epsilon is: ", epsilon
  print *, "Number of iterations: ", iterations
  print *, "This means the smallest epsilon such that 1 + epsilon > 1 is: ", epsilon

  ! Explain further the significance of the result
  print *, "Machine epsilon represents the precision limit of floating-point arithmetic."
  print *, "Numbers smaller than epsilon cannot be represented accurately."
  
end program machine_epsilon
