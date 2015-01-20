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

  ! This is for 2.5D case. 
  subroutine doGrad(res, vals, meshEquator)
    implicit none
                                           !  r, phi
    double precision, intent (in), dimension (:, :) :: vals 
    type (meshSphereEquator), intent (in) :: meshEquator
    type (vecSphere), intent (out), &
      dimension (size(vals, 1) - 1, size(vals, 2) - 1) :: res

    integer :: iR, iPhi, nR, nPhi
    double precision :: r, dr, dPhi

    nR = size(vals, 1)
    nPhi = size(vals, 2)

    checkGrid(meshEquator%nR, nR, 'r mesh')
    checkGrid(meshEquator%nPhi, nPhi, 'phi mesh')

    dr = calcDr(meshEquator)
    dPhi = calcDphi(meshEquator)

    do iPhi = 1, nPhi - 1
      do iR = 1, nR - 1
        r = calcR(iR, dr, meshEquator) 
        ! grad_r u = partial u / partial r
        res(iR, iPhi) % r = (u(iR + 1, iPhi) - u(iR, iPhi)) / dr
        ! grad_theta u = 0
        res(iR, iPhi) % theta = 0
        ! grad_phi u = (1/r) (partial u / partial phi)
        res(iR, iPhi) % phi = 1/r * (u(iR, iPhi + 1) - u(iR, iPhi)) / dPhi
      end do
    end do
  end subroutine doGrad

  ! Special as to only A_theta is parsed. 
  subroutine doCurl(res, Atheta, meshEquator)
    implicit none
                                           !  r, phi
    double precision, intent (in), dimension (:, :) :: Atheta
    type (meshSphereEquator), intent (in) :: meshEquator
    type (vecSphere), intent (out), &
      dimension (size(vals, 1) - 1, size(vals, 2) - 1) :: res

    integer :: iR, iPhi, nR, nPhi
    double precision :: r, dr, dPhi

    nR = size(vals, 1)
    nPhi = size(vals, 2)

    checkGrid(meshEquator%nR, nR, 'r mesh')
    checkGrid(meshEquator%nPhi, nPhi, 'phi mesh')

    dr = calcDr(meshEquator)
    dPhi = calcDphi(meshEquator)

    do iPhi = 1, nPhi - 1
      do iR = 1, nR - 1
        r = calcR(iR, dr, meshEquator) 
        ! curl A_r = - 1 / r * (partial Atheta / partial phi) 
        res(iR, iPhi) % r = - 1 / r * &
          (Atheta(iR, iPhi + 1) - Atheta(iR, iPhi)) / dPhi
        ! grad_theta u = 0
        res(iR, iPhi) % theta = 0
        ! grad_phi u = (1/r) (partial (r Atheta) / partial r)
        res(iR, iPhi) % phi = 1 / r * &
          ( &
              (r + dr / 2) * Atheta(iR + 1, iPhi) &
            - (r - dr / 2) * Atheta(iR,     iPhi) &
          ) / dr
      end do
    end do
  end subroutine doCurl

  subroutine checkGrid(a, b, namen)
    implicit none
    integer, intent (in) :: a, b
    character (len = *), intent (in) :: namen

    if (a .ne. b) then
      ! TODO to stderr. 
      print *, 'Size of ', namen, ' should be ', b, ', but set as ', a, '. '
      print *, 'Please correct it. The program is bewildered and stops. '
      stop
    end if
  end subroutine checkGrid
end module spherCalculus
