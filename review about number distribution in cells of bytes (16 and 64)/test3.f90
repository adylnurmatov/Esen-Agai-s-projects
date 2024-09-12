program machine_infinity
  implicit none
  real :: I, two_times_I  ! Variables for machine infinity and 2 * I
  integer :: iterations   ! Counter for the number of iterations

  ! Initialize I to a large value and the iteration counter to 0
  I = 1.0
  iterations = 0

  ! Loop to find the point where 2 * I results in infinity
  do
    two_times_I = 2.0 * I   ! Calculate 2 * I

    ! If 2 * I is still greater than I, keep doubling I
    if (two_times_I > I) then
      I = two_times_I       ! Update I to the new value
      iterations = iterations + 1  ! Increment the iteration counter
    else
      exit    ! Exit the loop when 2 * I is no longer greater than I (infinity)
    end if
  end do

  ! Print the final value of I before it reaches infinity
  print *, "Machine infinity is approximately: ", I
  print *, "Number of iterations to reach infinity: ", iterations
  print *, "This means that for larger values, the system overflows to infinity."

end program machine_infinity
