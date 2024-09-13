program machine_epsilon_multiple_precisions
  implicit none
  integer, parameter :: sp = selected_real_kind(6, 30)   
  integer, parameter :: dp = selected_real_kind(15, 307)  
  integer, parameter :: hp = selected_real_kind(3, 15)    

  real(kind=hp) :: epsilon_hp, one_plus_epsilon_hp  
  real(kind=sp) :: epsilon_sp, one_plus_epsilon_sp  
  real(kind=dp) :: epsilon_dp, one_plus_epsilon_dp   
  integer :: iterations_hp, iterations_sp, iterations_dp

  iterations_hp = 0
  iterations_sp = 0
  iterations_dp = 0

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
  epsilon_hp = epsilon_hp * 2.0_hp  

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
  epsilon_sp = epsilon_sp * 2.0_sp


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
  epsilon_dp = epsilon_dp * 2.0_dp 

  print *, "Машинное эпсилон для половинной точности (float16): ", epsilon_hp, " Итераций: ", iterations_hp
  print *, "Машинное эпсилон для одинарной точности (float32): ", epsilon_sp, " Итераций: ", iterations_sp
  print *, "Машинное эпсилон для двойной точности (float64): ", epsilon_dp, " Итераций: ", iterations_dp

end program machine_epsilon_multiple_precisions
