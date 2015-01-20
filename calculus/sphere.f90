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

    nR = size(Atheta, 1)
    nPhi = size(Atheta, 2)

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

  subroutine doDiv(res, Ar, Aphi, meshEquator)
    implicit none
                                           !  r, phi
    double precision, intent (in), dimension (:, :) :: Ar, Aphi
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision, intent (out), &
      dimension (size(vals, 1) - 1, size(vals, 2) - 1) :: res

    integer :: iR, iPhi, nR, nPhi, nR2, nPhi2
    double precision :: r, dr, dPhi

    nR = size(Ar, 1)
    nPhi = size(Ar, 2)
    nR2 = size(Aphi, 1)
    nPhi2 = size(Aphi, 2)

    checkGrid(meshEquator%nR, nR, 'r mesh for Ar')
    checkGrid(meshEquator%nPhi, nPhi, 'phi mesh for Ar')
    checkGrid(meshEquator%nR, nR2, 'r mesh for Aphi')
    checkGrid(meshEquator%nPhi, nPhi2, 'phi mesh for Aphi')

    dr = calcDr(meshEquator)
    dPhi = calcDphi(meshEquator)

    do iPhi = 1, nPhi - 1
      do iR = 1, nR - 1
        r = calcR(iR, dr, meshEquator) 
        ! div A       = 1/(r^2) (partial (r^2 Ar) / partial r) &
        res(iR, iPhi) = 1 / (r ** 2) * ( &
                            (r + dr/2)**2 * Ar(iR + 1, iPhi) &
                          - (r - dr/2)**2 * Ar(iR,     iPhi) &
                        ) / dr &
        !             + 1/r * (partial Aphi / partial phi)
                      + 1 / r * ( &
                          Aphi(iR, iPhi + 1) - Aphi(iR, iPhi)
                        ) / dPhi
      end do
    end do
  end subroutine doDiv

  subroutine doLaplace(res, vals, meshEquator)
    implicit none
                                           !  r, phi
    double precision, intent (in), dimension (:, :) :: vals
    type (meshSphereEquator), intent (in) :: meshEquator
    double precision, intent (out), &
      dimension (size(vals, 1) - 1, size(vals, 2) - 1) :: res

    integer :: iR, iPhi, nR, nPhi
    double precision :: r, dr, dPhi

    nR = size(vals, 1)
    nPhi = size(vals, 2)

    checkGrid(meshEquator%nR, nR, 'r mesh for Ar')
    checkGrid(meshEquator%nPhi, nPhi, 'phi mesh for Ar')

    dr = calcDr(meshEquator)
    dPhi = calcDphi(meshEquator)

    do iPhi = 2, nPhi - 2
      do iR = 2, nR - 2
        ! Now defined at the grid point. 
        r = calcR(iR, dr, meshEquator) - dr / 2
        res(iR - 1, iPhi - 1) = &
          !   1/(r^2) (r^2 f,r),r &
          1 / (r ** 2) * ( &
            ! To compute (r^2 f,r) at outer grid
              (r + dr / 2) ** 2 * &
                (vals(iR + 1, iPhi) - vals(iR, iPhi)) / dr &
            ! To compute (r^2 f,r) at inner grid
            - (r - dr / 2) ** 2 * &
                (vals(iR, iPhi) - vals(iR - 1, iPhi)) / dr &
          ) / dr &
          ! + 1/(r^2) f,PhiPhi 
          + 1 / (r ** 2) * &
            (vals(iR, iPhi + 1) + vals(iR, iPhi - 1) - 2 * vals(iR, iPhi)) &
            / (dPhi ** 2)
      end do
    end do
  end subroutine doLaplace

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
