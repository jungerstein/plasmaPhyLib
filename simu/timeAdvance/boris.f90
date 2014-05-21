! vim : tw=72 ts=2 sw=2
! boris.f90
! Implements for Boris algorithm, for single particle in electric and
!   magnetic field. 
! Coded by jungerstein. 
! Except for the licence, it is kind to contact me
! if the codes are involved in your original research.

elemental function borisOneStep(vOld, B, E, q, m, c, deltaT) result (vNew)
  use vec3d
  implicit none

  type(vec3), intent (in) :: vOld, B, E
  double precision, intent (in) :: q, m, c, deltaT
  type(vec3) :: vNew

  type(vec3) :: vMinus, vPlus, bigC

  vMinus = vOld + (q * deltaT / 2 / m) * E
  bigC = vMinus + (q / 2 / m / c) * (vMinus .x. B) 
  vPlus = (deltaT * bigC + &
           (q * deltaT**2 / (2 * m * c)) * (bigC .x. B) + &
           (q * deltaT**2 / (2 * m * c) * (bigC .dot. B)) * B) &
          / &
          (1 + q**2 * (B .dot. B) * deltaT**2i / (4 * m**2 * c**2))
  vNew = vPlus + (q * deltaT / 2 / m) * E
end function borisOneStep
