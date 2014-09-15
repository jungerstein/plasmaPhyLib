! vim : tw=72 ts=2 sw=2
! velocityHT.f90
! Finds the velocity of deHoffman-Teller frame. 
! Coded by jungerstein. 
! Except for the licence, it is kind and proper to contact me
! if the codes are involved in your original research. 
!
! EXTERNAL LIBRARY
! * mkl
! OTHERWISE
! 1. Can be slightly modified to use general lapack. 

module vht
  use vec3d
  implicit none

  contains

  ! Encapsuled function to find the inverse of a 3x3 matrix. 
  ! But the matrix must be symmetric.
  subroutine invSymMat(inv, mat, isSingularExpected)
    use mkl95_precision, only: wp=>dp
    use mkl95_lapack, only: getrf, getri
    implicit none

    double precision, dimension (3, 3), intent (out) :: inv
    double precision, dimension (3, 3), intent (in) :: mat
    logical, intent (in), optional :: isSingularExpected

    double precision, dimension (3, 3) :: working
    double precision, dimension (3) :: ipiv
    logical :: isNotComplainAboutSingular

    integer :: info

    working = mat
    isNotComplainAboutSingular = .false. 
    if (present(isSingularExpected)) then
      isNotComplainAboutSingular = isSingularExpected
    end if

    call getrf(working, ipiv, info)

    if ((info .ne. 0) .and. (.not. isSingularExpected)) then
      print *, 'Singular matrix. '
      stop 
    end if

    call getri(working, ipiv)
    inv = working
  end subroutine invMat

  subroutine calcVHT(vHT, Bx, By, Bz, vx, vy, vz)
    implicit none
    type (vec3), intent (out) :: vHT
    double precision, dimension (:, :, :), intent (in) :: Bx, By, Bz, &
      vx, vy, vz

    integer :: nx, ny, nz, i, j, k
    double precision, dimension (3, 3) :: Km, sumKm, KZero, invKZero
    double precision, dimension (3) :: Km_dotVm, sumKm_dotVm, vm, res
    double precision :: BSq
    integer :: nGrid

    ! TODO Check input dimension. 
    nx = size(Bx, 1)
    ny = size(Bx, 2)
    nz = size(Bx, 3)
    sumKm = 0
    sumKm_dotVm = 0
    do k = 1, nz
    do j = 1, ny
    do i = 1, nx
      BSq = Bx(i, j, k) ** 2 + By(i, j, k) ** 2 + Bz(i, j, k) ** 2
      Km(1, 1) = BSq - Bx(i, j, k) ** 2
      Km(2, 2) = BSq - By(i, j, k) ** 2
      Km(3, 3) = BSq - Bz(i, j, k) ** 2
      Km(1, 2) = -Bx(i, j, k) * By(i, j, k)
      Km(1, 3) = -Bx(i, j, k) * Bz(i, j, k)
      Km(2, 3) = -By(i, j, k) * Bz(i, j, k)
      Km(2, 1) = Km(1, 2)
      Km(3, 1) = Km(1, 3)
      Km(3, 2) = Km(2, 3)
      sumKm = sumKm + Km
      vm(1) = vx(i, j, k)
      vm(2) = vy(i, j, k)
      vm(3) = vz(i, j, k)
      Km_dotVm = matmul(Km, vm)
      sumKm_dotVm = sumKm_dotVm + Km_dotVm
    end do
    end do
    end do
    sumKm_dotVm = sumKm_dotVm / nGrid
    KZero = sumKm_dotVm / nGrid
    call invSymMat(invKZero, KZero, .true.)
    res = matmul(invKZero, sumKm_dotVm)

    vHT % x = res(1)
    vHT % y = res(2)
    vHT % z = res(3)
  end subroutine calcVHT
end module vht

