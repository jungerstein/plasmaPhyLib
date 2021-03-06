! dataarrange.f90
! Arranges data in parallel output binary dump. 

module dataarrange
contains
subroutine arrangeData(res, dataIn, coordCentres)
  use segment, only: whichSegment
  implicit none
  
  double precision, intent (in), dimension (:, :, :) :: dataIn
  double precision, intent (out), dimension (:, :, :) :: res
  double precision, intent (in), dimension (:, :, :, :) :: coordCentres

  double precision :: minX, maxX, minY, maxY, minZ, maxZ
  integer :: nx, ny, nz, nBlockX, nBlockY, nBlockZ, nDim
  integer :: nGridX, nGridY, nGridZ
  integer :: i, j, k, minI, minJ, minK, maxI, maxJ, maxK, iCoor, jCoor, kCoor
  integer :: minIB, minJB, minKB, maxIB, maxJB, maxKB

  ! FIXME Should the procession die? 
  nDim = size(coordCentres, 1)
  if (nDim .ne. 3) return

  minX = minval(coordCentres(1, :, :, :))
  maxX = maxval(coordCentres(1, :, :, :))
  minY = minval(coordCentres(2, :, :, :))
  maxY = maxval(coordCentres(2, :, :, :))
  minZ = minval(coordCentres(3, :, :, :))
  maxZ = maxval(coordCentres(3, :, :, :))
  nx = size(dataIn, 1)
  ny = size(dataIn, 2)
  nz = size(dataIn, 3)
  nBlockX = size(coordCentres, 2)
  nBlockY = size(coordCentres, 3)
  nBlockZ = size(coordCentres, 4)
  ! TODO Add checks here: if dividable. 
  nGridX = nx / nBlockX
  nGridY = ny / nBlockY
  nGridZ = nz / nBlockZ
  do k = 1, nBlockZ
    minKB = (k - 1) * nGridZ + 1
    maxKB = k * nGridZ
  do j = 1, nBlockY
    minJB = (j - 1) * nGridY + 1
    maxJB = j * nGridY
  do i = 1, nBlockX
    minIB = (i - 1) * nGridX + 1
    maxIB = i * nGridX
    iCoor = whichSegment(coordCentres(1,i,j,k), minX, maxX, 1, nBlockX)
    jCoor = whichSegment(coordCentres(2,i,j,k), minY, maxY, 1, nBlockY)
    kCoor = whichSegment(coordCentres(3,i,j,k), minZ, maxZ, 1, nBlockZ)
    minI = (iCoor - 1) * nGridX + 1
    minJ = (jCoor - 1) * nGridY + 1
    minK = (kCoor - 1) * nGridZ + 1
    maxI = iCoor * nGridX
    maxJ = jCoor * nGridY
    maxK = kCoor * nGridZ
    res(minI:maxI, minJ:maxJ, minK:maxK) &
      = dataIn(minIB:maxIB, minJB:maxJB, minKB:maxKB)
  end do
  end do
  end do

end subroutine arrangeData

subroutine deblock(res, dataIn, sizeBlock)
  implicit none
  
  double precision, intent (in), dimension (:, :, :) :: dataIn
  double precision, intent (out), dimension (:, :, :) :: res
  integer, intent (in) :: sizeBlock

  integer :: nx, ny, nz, nBlockX, nBlockY, nBlockZ
  integer :: nGridX, nGridY, nGridZ
  integer :: i, j, k, iB, jB, kB, iBig, jBig, kBig, iRead, jRead, kRead

  nx = size(dataIn, 1)
  ny = size(dataIn, 2)
  nz = size(dataIn, 3)
  nBlockX = nx / sizeBlock
  nBlockY = ny / sizeBlock
  nBlockZ = nz / sizeBlock
  nGridX = sizeBlock
  nGridY = sizeBlock
  nGridZ = sizeBlock
  iRead = 1
  jRead = 1
  kRead = 1
  do kB = 1, nBlockZ
  do jB = 1, nBlockY
  do iB = 1, nBlockX
    do k = 1, nGridZ
      kBig = (kB - 1) * nGridZ + k
    do j = 1, nGridY
      jBig = (jB - 1) * nGridY + j
    do i = 1, nGridX
      iBig = (iB - 1) * nGridX + i
      res(iBig, jBig, kBig) = dataIn(iRead, jRead, kRead)
      iRead = iRead + 1
      if (iRead .eq. nx + 1) then
        iRead = 1
        jRead = jRead + 1
        if (jRead .eq. ny + 1) then
          jRead = 1
          kRead = kRead + 1
        end if
      end if
    end do
    end do
    end do
  end do
  end do
  end do

end subroutine deblock
end module dataarrange
