program machine_zero
  implicit none
  real :: Z, two_times_Z   ! Variables for machine zero and 2 * Z
  integer :: iterations     ! Counter for the number of iterations

  ! Initialize Z to 1.0 and the iteration counter
  Z = 1.0
  iterations = 0

  ! Loop to find machine zero (smallest Z such that 2 * Z > Z)
  do
    two_times_Z = 2.0 * Z   ! Calculate 2 * Z

    ! If 2 * Z is still greater than Z, continue halving Z
    if (two_times_Z > Z) then
      Z = Z / 2.0
      iterations = iterations + 1   ! Increment the iteration counter
    else
      exit    ! Exit the loop when 2 * Z is no longer greater than Z
    end if
  end do

  ! Multiply Z by 2 to correct the overshoot
  Z = Z * 2.0

  ! Print the machine zero value and number of iterations
  print *, "Machine zero is: ", Z
  print *, "Number of iterations: ", iterations
  print *, "This means that 2 * Z > Z for this value of Z."

end program machine_zero
