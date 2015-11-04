! vim : tw=72 ts=2 sw=2
! sphericalcalculus.f90
! Implements for 3D vector calculus in spherical coordinates. 
! Coded by jungerstein. 
! Except for the licence, it is kind to contact me
! if the codes are involved in your original research.

module sphericalcalculus
  use sphericalvec3d
  implicit none

  type neighbourCentreDiff
    type (sphvec3) :: rPrev, rNext, thPrev, thNext, phPrev, phNext
  end type neighbourCentreDiff

  interface operator (.div.)
    module procedure :: sphericalVectorDivCentreDiff
  end interface

  contains 

  ! Use central differentation. 
  ! FIXME Check values at zero and polars. 
  elemental function sphericalVectorDivCentreDiff &
      (theVec, neighbour) &
      result (div)
    implicit none 

    type (sphvec3), intent (in) :: theVec
    type (neighbourCentreDiff), intent (in) :: neighbour
    double precision :: div

    double precision :: rContrib, thContrib, phContrib
    type (sphvec3) :: atRPrev, atRNext, atThPrev, atThNext, atPhPrev, atPhNext
    atRPrev = neighbour%rPrev
    atRNext = neighbour%rNext
    atThPrev = neighbour%thPrev
    atThNext = neighbour%thNext
    atPhPrev = neighbour%phrPrev
    atPhNext = neighbour%phrNext

    ! FIXME Mind catastrophic cancellation. 

    ! $\frac{1}{r^2} \left(r^2 A_r\right)_{,r}$
    rContrib = 1/(theVec%rPos**2) &
      * (atRNext%rPos ** 2 * atRNext%r - atRPrev%rPos ** 2 * atRPrev%r) &
      / (atRNext%rPos - atRPrev%rPos)

    ! $\frac{1}{\sin \theta} \left(B_\theta \sin\theta \right)_{,\theta}$
    thContrib = 1/(theVec%rPos * sin(theVec%thPos)) &
      * (atThNext%th * sin(atThNext%thPos) - atThPrev%th * sin(atThPrev%thPos)) &
      / (atThNext%thPos - atThPrev%thPos)

    ! $\frac{1}{\sin \theta} B_\phi_{,phi}$
    phContrib = 1/(theVec%rPos * sin(theVec%thPos)) &
      * (atPhNext%ph - atPhNext%th) &
      / (atPhNext%phPos - atPhPrev%phPos)

    div = rContrib + thContrib + phContrib
  end function sphericalVectorDiv
end module sphericalvec3d
