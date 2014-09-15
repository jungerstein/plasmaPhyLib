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
    use mkl95_lapack, only: 
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
end module vht

