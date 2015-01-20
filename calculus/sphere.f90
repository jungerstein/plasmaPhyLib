! sphere.f90
! Module for calculus in sphere geometrics. 
! 
module spherCalculus
  type vecSphere
    double precision :: r
    double precision :: theta
    double precision :: phi
  end type vecSphere

  type meshSphere
    double precision :: rMin, rMax
    double precision :: thetaMin, thetaMax
    double precision :: phiMin, phiMax
    integer :: nR, nTheta, nPhi
  end type meshSphere

  type meshSphereEquator
    double precision :: rMin, rMax
    double precision :: phiMin, phiMax
    integer :: nR, nPhi
  end type meshSphereEquator
contains
  function calcDr(meshEquator)
    implicit none
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision :: calcDr

    if (meshEquator%nR .eq. 1) return 0
    calcDr = (meshEquator%rMax - meshEquator%rMin) / (meshEquator%nR - 1)
  end function calcDr

  function calcDphi(meshEquator)
    implicit none
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision :: calcDphi

    if (meshEquator%nPhi .eq. 1) return 0
    calcDphi = (meshEquator%phiMax - meshEquator%phiMin) &
             / (meshEquator%nPhi - 1)
  end function calcDphi

  function calcR(i, meshEquator)
    implicit none
    integer, intent (in) :: i
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision :: calcR

    double precision :: dr

    dr = calcDr(meshEquator)
    calcR = (i - 0.5) * dr + meshEquator%rMin
  end function calcR

  ! A fast version. 
  function calcR(i, dr, meshEquator)
    implicit none
    integer, intent (in) :: i
    double precision, intent (in) :: dr
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision :: calcR

    calcR = (i - 0.5) * dr + meshEquator%rMin
  end function calcR

  function calcPhi(i, meshEquator)
    implicit none
    integer, intent (in) :: i
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision :: calcPhi

    double precision :: dPhi

    dPhi = calcDphi(meshEquator)
    calcPhi = (i - 0.5) * dPhi + meshEquator%phiMin
  end function calcPhi

  ! A fast version. 
  function calcPhi(i, dPhi, meshEquator)
    implicit none
    integer, intent (in) :: i
    double precision, intent (in) :: dPhi
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision :: calcPhi

    calcPhi = (i - 0.5) * dPhi + meshEquator%phiMin
  end function calcPhi

  subroutine doGrad(res, vals, meshEquator)
    implicit none
                                           !  r, phi
    double precision, intent (in), dimension (:, :) :: vals 
    type (meshSphereEquator), intent (in) :: meshEquator

  end subroutine doGrad
end module spherCalculus
