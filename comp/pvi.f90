! pvi.f90
! computes pvi. 
! Should be used with f95 and later. 

module pvi
contains

subroutine compPVI(res, compX, compY, compZ)
  implicit none

  double precision, intent (in), dimension (:, :, :) :: compX
  double precision, intent (in), dimension (:, :, :) :: compY, compZ
  double precision, intent (out), &
    dimension (size(compX, 1), size(compX, 2), size(compX, 3)) :: res

  double precision, dimension(size(compX,1), size(compX,2), size(compX,3))&
    :: advX, advY, advZ, advXX, advXY, advXZ, advYX, advYY, advYZ, &
    advZX, advZY, advZZ, pviDenom
  ! FIXME When 2048^3, it will not suffit! 
  integer :: nx, ny, nz

  nx = size(compX, 1)
  ny = size(compX, 2)
  nz = size(compX, 3)
  advXX = cshift(compX, 1, 1)
  advXY = cshift(compX, 1, 2)
  advXZ = cshift(compX, 1, 3)
  advYX = cshift(compY, 2, 1)
  advYY = cshift(compY, 2, 2)
  advYZ = cshift(compY, 2, 3)
  advZX = cshift(compZ, 3, 1)
  advZY = cshift(compZ, 3, 2)
  advZZ = cshift(compZ, 3, 3)

  advX = ((advXX-compX)**2 + (advYX-compY)**2 + (advZX-compZ)**2)
  advY = ((advXY-compX)**2 + (advYY-compY)**2 + (advZY-compZ)**2)
  advZ = ((advXZ-compX)**2 + (advYZ-compY)**2 + (advZZ-compZ)**2)
  pviDenom = advX + advY + advZ
  res = pviDenom / (sum(pviDenom) / (nx * ny * nz))
end subroutine compPVI

end module pvi
