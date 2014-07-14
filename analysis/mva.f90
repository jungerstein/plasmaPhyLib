
module mva
contains

subroutine calcDiv(aus, Bx, By, Bz, i, j, k, width)
  implicit none
  double precision, dimension (3, 3), intent (out) :: aus
  double precision, dimension (:, :, :), intent (in) :: Bx, By, Bz
  integer, intent (in) :: i, j, k
  integer, intent (in) :: width

  double precision, dimension (:, :, :), allocatable :: dBx, dBy, dBz
  integer :: nx, ny, nz

  nx = size(Bx, 1)
  ny = size(Bx, 2)
  nz = size(Bx, 3)
    
  allocate(dBx(width, width, width))
  allocate(dBy(width, width, width))
  allocate(dBz(width, width, width))
  call takeData(dBx, Bx, i, j, k, width)
  call takeData(dBy, By, i, j, k, width)
  call takeData(dBz, Bz, i, j, k, width)
  dBx = dBx - sum(dBx) / (width ** 3)
  dBy = dBy - sum(dBy) / (width ** 3)
  dBz = dBz - sum(dBz) / (width ** 3)

  aus(1, 1) = sum(dBx * dBx) / (width ** 3)
  aus(1, 2) = sum(dBx * dBy) / (width ** 3)
  aus(1, 3) = sum(dBx * dBz) / (width ** 3)
  aus(2, 1) = aus(1, 2)
  aus(2, 2) = sum(dBy * dBy) / (width ** 3)
  aus(2, 3) = sum(dBy * dBz) / (width ** 3)
  aus(3, 1) = aus(1, 3)
  aus(3, 2) = aus(2, 3)
  aus(3, 3) = sum(dBz * dBz) / (width ** 3)

  deallocate(dBx, dBy, dBz)
end subroutine calcDiv

subroutine eigene(vals, vecs, A)
  use mkl95_precision, only: wp=>dp
  use mkl95_lapack, only: syevd
  implicit none
  double precision, dimension(3), intent (out) :: vals
  double precision, dimension(3, 3), intent (out) :: vecs
  double precision, dimension(3, 3), intent (in) :: A

  double precision, dimension(3, 3) :: working
  integer :: info

  working = A

  call syevd(working, vals, 'V', 'L', info)
  vecs = working
end subroutine eigene

subroutine goMVA(vals, vecs, Bx, By, Bz, i, j, k, width)
  implicit none
  double precision, dimension(3), intent (out) :: vals
  double precision, dimension(3, 3), intent (out) :: vecs
  double precision, dimension (:, :, :), intent (in) :: Bx, By, Bz
  integer, intent (in) :: i, j, k
  integer, intent (in) :: width

  double precision, dimension(3, 3) :: bigM

  call calcDiv(bigM, Bx, By, Bz, i, j, k, width)
  call eigene(vals, vecs, bigM)
end subroutine goMVA

subroutine printMVA(vals, vecs)
  implicit none

  double precision, dimension(3), intent (in) :: vals
  double precision, dimension(3, 3), intent (in) :: vecs

  print *, 'L: val ', vals(3), 'vec ', vecs(:, 3)
  print *, 'M: val ', vals(2), 'vec ', vecs(:, 2)
  print *, 'N: val ', vals(1), 'vec ', vecs(:, 1)
  print *, 'lambda L / lambda M should > 3, act''ly: ', vals(3) / vals(2)
  print *, 'lambda M / lambda N should > 3, act''ly: ', vals(2) / vals(1)
end subroutine printMVA

subroutine takeData(aus, ein, i, j, k, width)
  implicit none
  integer, intent (in) :: width
  double precision, dimension (width, width, width), intent (out) :: aus
  double precision, dimension (:, :, :), intent (in) :: ein
  integer, intent (in) :: i, j, k

  integer :: nx, ny, nz, nWing, iLoop, jLoop, kLoop, iAct, jAct, kAct
 
  ! TODO Improve error handling. 
  if (mod(width, 2) .eq. 0) then 
    print *, 'mva:takeData: Width should be odd. '
    stop 100
  end if
  nWing = (width - 1) / 2
  nx = size(ein, 1)
  ny = size(ein, 2)
  nz = size(ein, 3)

  do kLoop = -nWing, nWing
    kAct = mod(k+kLoop+nz-1, nz) + 1
  do jLoop = -nWing, nWing
    jAct = mod(j+jLoop+ny-1, ny) + 1
  do iLoop = -nWing, nWing
    iAct = mod(i+iLoop+nx-1, nx) + 1
    aus(iLoop+nWing+1,jLoop+nWing+1,kLoop+nWing+1) = ein(iAct, jAct, kAct)
  end do
  end do
  end do
end subroutine takeData

end module mva
