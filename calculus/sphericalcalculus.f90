! vim : tw=72 ts=2 sw=2
! sphericalcalculus.f90
! Implements for 3D vector calculus in spherical coordinates. 
! Coded by jungerstein. 
! Except for the licence, it is kind to contact me
! if the codes are involved in your original research.

module sphericalcalculus
  use sphericalvec3d
  interface operator (.div.)
    module procedure :: sphericalVectorDiv
  end interface

  contains 

  ! Use central differentation. 
  ! FIXME Check values at zero and polars. 
  elemental function sphericalVectorDiv &
      (theVec, atRPrev, atRNext, atThPrev, atThNext, atPhPrev, atPhNext) &
      result (div)
    implicit none 

    type (sphvec3), intent (in) :: theVec, atRPrev, atRNext, &
      atThPrev, atThNext, atPhPrev, atPhNext
    double precision :: div

    double precision :: rContrib, thContrib, phContrib

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
