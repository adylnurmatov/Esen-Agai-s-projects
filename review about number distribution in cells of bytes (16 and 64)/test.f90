program machine_epsilon_multiple_precisions
  implicit none
  integer, parameter :: sp = selected_real_kind(6, 30)    ! Single precision (float32)
  integer, parameter :: dp = selected_real_kind(15, 307)  ! Double precision (float64)
  integer, parameter :: hp = selected_real_kind(3, 15)    ! Half precision (float16)
  
  real(kind=hp) :: epsilon_hp, one_plus_epsilon_hp   ! Variables for half precision
  real(kind=sp) :: epsilon_sp, one_plus_epsilon_sp   ! Variables for single precision
  real(kind=dp) :: epsilon_dp, one_plus_epsilon_dp   ! Variables for double precision
  integer :: iterations_hp, iterations_sp, iterations_dp   ! Iteration counters

  ! Initialize iteration counters to zero
  iterations_hp = 0
  iterations_sp = 0
  iterations_dp = 0

  ! Calculate epsilon for half precision (float16)
  epsilon_hp = 1.0_hp
  do
    one_plus_epsilon_hp = 1.0_hp + epsilon_hp
    if (one_plus_epsilon_hp > 1.0_hp) then
      epsilon_hp = epsilon_hp / 2.0_hp
      iterations_hp = iterations_hp + 1
    else
      exit
    end if
  end do
  epsilon_hp = epsilon_hp * 2.0_hp  ! Correct the overshoot

  ! Calculate epsilon for single precision (float32)
  epsilon_sp = 1.0_sp
  do
    one_plus_epsilon_sp = 1.0_sp + epsilon_sp
    if (one_plus_epsilon_sp > 1.0_sp) then
      epsilon_sp = epsilon_sp / 2.0_sp
      iterations_sp = iterations_sp + 1
    else
      exit
    end if
  end do
  epsilon_sp = epsilon_sp * 2.0_sp  ! Correct the overshoot

  ! Calculate epsilon for double precision (float64)
  epsilon_dp = 1.0_dp
  do
    one_plus_epsilon_dp = 1.0_dp + epsilon_dp
    if (one_plus_epsilon_dp > 1.0_dp) then
      epsilon_dp = epsilon_dp / 2.0_dp
      iterations_dp = iterations_dp + 1
    else
      exit
    end if
  end do
  epsilon_dp = epsilon_dp * 2.0_dp  ! Correct the overshoot

  ! Print the results
  print *, "Machine epsilon for half precision (float16): ", epsilon_hp
  print *, "Iterations for float16: ", iterations_hp
  print *, "Machine epsilon for single precision (float32): ", epsilon_sp
  print *, "Iterations for float32: ", iterations_sp
  print *, "Machine epsilon for double precision (float64): ", epsilon_dp
  print *, "Iterations for float64: ", iterations_dp

end program machine_epsilon_multiple_precisions
