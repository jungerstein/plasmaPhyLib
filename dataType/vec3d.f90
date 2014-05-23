! vim : tw=72 ts=2 sw=2
! vec3d.f90
! Implements for 3D vectors. 
! Coded by jungerstein. 
! Except for the licence, it is kind to contact me
! if the codes are involved in your original research.

module vec3d
  type vec3
    double precision :: x
    double precision :: y
    double precision :: z
  end type vec3

  ! TODO Add definitions of cylinder / spheric coordinates here. 

  interface operator (+)
    module procedure :: vecAdd
  end interface

  interface operator (-)
    module procedure :: oppositeVec, vecMinus
  end interface

  interface operator (*)
    module procedure :: constTimesVec
  end interface

  interface operator (/)
    module procedure :: vecDivConst
  end interface

  interface operator (.dot.)
    module procedure :: vecDotVec
  end interface

  interface operator (.x.)
    module procedure :: vecCrossVec
  end interface

  contains 

  elemental function constTimesVec(alpha, a) result (b)
    implicit none
    double precision, intent (in) :: alpha
    type(vec3), intent (in) :: a
    type(vec3) :: b

    b%x = alpha * a%x
    b%y = alpha * a%y
    b%z = alpha * a%z
  end function constTimesVec
  
  elemental function oppositeVec(a) result (b)
    implicit none
    type(vec3), intent (in) :: a
    type(vec3) :: b

    b = (-1.0D0) * a
  end function oppositeVec

  elemental function vecAdd(a, b) result (c)
    implicit none
    type(vec3), intent (in) :: a, b
    type(vec3) :: c

    c % x = a % x + b % x
    c % y = a % y + b % y
    c % z = a % z + b % z
  end function vecAdd

  elemental function vecCrossVec(a, b) result (c)
    implicit none
    type(vec3), intent (in) :: a, b
    type(vec3) :: c

    c % x = a % y * b % z - a % z * b % y
    c % y = a % z * b % x - a % x * b % z
    c % z = a % x * b % y - a % y * b % x
  end function vecCrossVec

  elemental function vecDivConst(a, alpha) result (b)
    implicit none
    type(vec3), intent (in) :: a
    double precision, intent (in) :: alpha
    type(vec3) :: b

    b = (1 / alpha) * a
  end function vecDivConst

  elemental function vecDotVec(a, b) result (c)
    implicit none
    type(vec3), intent (in) :: a, b
    double precision :: c

    c = a % x * b % x + a % y * b % y + a % z * b % z
  end function vecDotVec

  elemental function vecMinus(a, b) result (c)
    implicit none
    type(vec3), intent (in) :: a, b
    type(vec3) :: c

    c = a + (-b)
  end function vecMinus
end module vec3d
