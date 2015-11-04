! vim : tw=72 ts=2 sw=2
! sphericalvec.f90
! Implements for 3D vectors in spherical coordinates. 
! Coded by jungerstein. 
! Except for the licence, it is kind to contact me
! if the codes are involved in your original research.

module sphericalvec3d
  type sphvec3
    ! Please note that there is NO POINT using arrays to store components. 
    ! The elements simply refer to different things. 
    double precision :: r
    double precision :: th
    double precision :: ph 
    double precision :: rPos     ! For vector positions
    double precision :: thPos
    double precision :: phPos
  end type sphvec3

  interface operator (.len.)
    module procedure :: sphericalVectorLength
  end interface

  interface operator (.ein.)
    module procedure :: sphericalUnityVector
  end interface

  contains 

  elemental function sphericalVectorLength(A) result (l)
    implicit none 

    type (sphvec3), intent (in) :: A
    double precision :: l

    ! TODO In some systems there are built-in procedures to compute this. 
    ! Try to utilise them. 
    l = sqrt(A%r ** 2 + A%th ** 2 + A%ph ** 2) 
  end function sphericalVectorLength

  elemental function sphericalUnityVector(A) result (e)
    implicit none

    type (sphvec3), intent (in) :: A
    type (sphvec3) :: e

    double precision :: l

    e%rPos = A%rPos
    e%thPos = A%thPos
    e%phPos = A%phPos

    l = .len. A

    e%r = A%r / l
    e%th = A%th / l
    e%ph = A%ph / l
  end function sphericalUnityVector

end module sphericalvec3d
