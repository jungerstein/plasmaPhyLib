! binner.f90
! Module of bin summary. 

module bin
  type countBin
    double precision, dimension (:), allocatable :: binBoundaries
    integer, dimension (:), allocatable :: binCounts
    integer :: n
    logical :: isLog
  end type countBin
contains
! Allocate memory for a bin. Must be deallocated. 
subroutine allocateCountBin(binToProc, nBins, valMin, valMax)
  implicit none
! Neccessary inout. 
  type (countBin), intent (inout) :: binToProc
  integer, intent (in) :: nBins
  double precision, intent (in) :: valMin, valMax

  double precision :: minAct, maxAct, dx
  integer :: i
  logical :: isLogBoundNegative, isUseLog

  if (nBins .lt. 1) return
  minAct = min(valMin, valMax)
  maxAct = max(valMin, valMax)

  isLogBoundNegative = .false.
  isUseLog = .false.

  if (binToProc % isLog) then
    ! Same sign. 
    if ((minAct * maxAct) .gt. 0) then
      isUseLog = .true.
      ! Both minus
      if (minAct .lt. 0) then
        minAct = -minAct
        maxAct = -maxAct
        isLogBoundNegative = .true.
      end if
      minAct = log10(minAct)
      maxAct = log10(maxAct)
    end if
  end if

  dx = (maxAct - minAct) / nBins
  binToProc % n = nBins

  allocate(binToProc % binBoundaries(nBins + 1))
  allocate(binToProc % binCounts(nBins))

  binToProc % binCounts = 0
  binToProc % binBoundaries = (/ (minAct + dx * i, i=1, nBins) /)

  if (isUseLog) then
    binToProc % binBoundaries = 10 ** (binToProc % binBoundaries)
    if (isLogBoundNegative) then
      binToProc % binBoundaries = -1 * binToProc % binBoundaries
    end if
  end if
  
end subroutine allocateCountBin

subroutine deallocateCountBin(binToProc)
  implicit none
! Neccessary inout. 
  type (countBin), intent (inout) :: binToProc

  deallocate(binToProc % binCounts)
  deallocate(binToProc % binBoundaries)
end subroutine deallocateCountBin

! Locate the bin where minBin <= x < maxBin. 
! May return 0 or nBins + 1. 
! Use linear search. 
! TODO Init binary search when nBins > a threshold (typical 128? )
function searchBin(binToProc, x)
  implicit none
  type (countBin), intent (in) :: binToProc
  double precision, intent (in) :: x
  integer :: searchBin

  integer :: i

  if (x .lt. binToProc % binBoundaries(1)) then
    searchBin = 0
  else if (x .gt. binToProc%binBoundaries(binToProc%n+1)) then
    searchBin = binToProc%n + 1
  else
    do i = 1, binToProc % n
      if (x .ge. binToProc % binBoundaries(i)) then
        searchBin = i
        exit
      end if
    end do
  end if
end function searchBin

! TODO Overload these functions. 
subroutine sumBin1d(binToProc, x)
  implicit none
  type (countBin), intent (inout) :: binToProc
  double precision, intent (in), dimension (:) :: x

  integer :: i, n, ind
  n = size(x)
  do i = 1, n
    ind = searchBin(binToProc, x(i)) 
    if ((ind .ge. 1) .and. (ind .le. binToProc%n)) then
      binToProc % binCounts(ind) = binToProc % binCounts(ind) + 1
    end if
  end do
end subroutine sumBin1d

subroutine sumBin2d(binToProc, x)
  implicit none
  type (countBin), intent (inout) :: binToProc
  double precision, intent (in), dimension (:, :) :: x

  integer :: i, j, nx, ny, ind
  nx = size(x, 1)
  ny = size(x, 2)
  do j = 1, ny
  do i = 1, nx
    ind = searchBin(binToProc, x(i, j)) 
    if ((ind .ge. 1) .and. (ind .le. binToProc%n)) then
      binToProc % binCounts(ind) = binToProc % binCounts(ind) + 1
    end if
  end do
  end do
end subroutine sumBin2d

subroutine sumBin3d(binToProc, x)
  implicit none
  type (countBin), intent (inout) :: binToProc
  double precision, intent (in), dimension (:, :, :) :: x

  integer :: i, j, k, nx, ny, nz, ind
  nx = size(x, 1)
  ny = size(x, 2)
  nz = size(x, 3)
  do k = 1, nz
  do j = 1, ny
  do i = 1, nx
    ind = searchBin(binToProc, x(i, j, k)) 
    if ((ind .ge. 1) .and. (ind .le. binToProc%n)) then
      binToProc % binCounts(ind) = binToProc % binCounts(ind) + 1
    end if
  end do
  end do 
  end do 
end subroutine sumBin3d

end module bin
