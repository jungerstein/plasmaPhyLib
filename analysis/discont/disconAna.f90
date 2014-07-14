module discont
contains
subroutine judgeDisc(Bx, By, Bz, i, j, k, width)
  use mva
  implicit none
  double precision, dimension (:, :, :), intent (in) :: Bx, By, Bz
  integer, intent (in) :: i, j, k, width
  integer :: n
  integer :: n1, n2
  double precision :: sBx1, sBx2, sBy1, sBy2, sBz1, sBz2
  double precision :: Bx1, Bx2, By1, By2, Bz1, Bz2
  double precision :: Bn, deltaB, B1, B2, Bn1, Bn2
  double precision :: par1, par2

  double precision, dimension(width,width,width) :: BxTake, ByTake, BzTake
  double precision, dimension(3, 3) :: vecs, bigM
  double precision, dimension(3) :: vals

  if (mod(width, 2) .eq. 0) then
    print *, 'discont:judgeDisc: width should be odd. '
    stop 
  end if

  n = (width + 1) / 2

  call takeData(BxTake, Bx, i, j, k, width)
  call takeData(ByTake, By, i, j, k, width)
  call takeData(BzTake, Bz, i, j, k, width)

  call calcDiv(bigM, Bx, By, Bz, i, j, k, width)
  call eigene(vals, vecs, bigM)

  call throwSum(n2, sBx2, n1, sBx1, BxTake, vecs(:, 1))
  call throwSum(n2, sBy2, n1, sBy1, ByTake, vecs(:, 1))
  call throwSum(n2, sBz2, n1, sBz1, BzTake, vecs(:, 1))

  Bx1 = sBx1 / n1
  By1 = sBy1 / n1
  Bz1 = sBz1 / n1
  Bx2 = sBx2 / n2
  By2 = sBy2 / n2
  Bz2 = sBz2 / n2

  B1 = sqrt(Bx1**2 + By1**2 + Bz1**2)
  B2 = sqrt(Bx2**2 + By2**2 + Bz2**2)
  Bn1 = Bx1 * vecs(1, 1) + By1 * vecs(2, 1) + Bz1 * vecs(3, 1)
  Bn2 = Bx2 * vecs(1, 1) + By2 * vecs(2, 1) + Bz2 * vecs(3, 1)
  Bn = abs(Bn1 * n1 + Bn2 * n2) / (n1 + n2)

  par1 = Bn / max(B1, B2)
  par2 = abs(B1 - B2) / max(B1, B2)

  print *, '1B:', Bx1, By1, Bz1, n1, sBx1, sBy1, sBz1
  print *, '2B:', Bx2, By2, Bz2, n2, sBx2, sBy2, sBz2
  print *, 'Bn1 = ', Bn1
  print *, 'Bn2 = ', Bn2
  print *, 'B1 = ', B1
  print *, 'B2 = ', B2
  print *, '|Bn|      / |B|L = ', par1
  print *, 'delta |B| / |B|L = ', par2
  if (par2 .lt. 0.2) then
    if (par1 .lt. 0.4) then
      print *, 'ED'
    else
      print *, 'RD'
    end if
  else
    if (par1 .lt. 0.2) then
      print *, 'TD'
    else
      print *, 'ND'
    end if
  end if
end subroutine judgeDisc

subroutine throwSum(nMinus, totalMinus, nPlus, totalPlus, ein, ijk)
  implicit none
  double precision, intent (in), dimension (:, :, :) :: ein
  double precision, intent (in), dimension (3) :: ijk
  double precision, intent (out) :: totalMinus, totalPlus
  integer, intent (out) :: nMinus, nPlus

  ! Assume that size of ein is odd, and cubic. 
  integer :: n
  integer :: i, j, k
  integer :: nCenter
  double precision :: dot

  nMinus = 0
  nPlus = 0
  totalPlus = 0
  totalMinus = 0
  n = size(ein, 1)
  nCenter = (n + 1) / 2
  do k = 1, n
  do j = 1, n
  do i = 1, n
    dot = (i-nCenter) * ijk(1) + (j-nCenter) * ijk(2) + (k-nCenter) * ijk(3)
    if (dot .gt. 0.0) then
      nPlus = nPlus + 1
      totalPlus = totalPlus + ein(i, j, k)
    else 
      if (dot .lt. 0.0) then
        nMinus = nMinus + 1
        totalMinus = totalMinus + ein(i, j, k)
      end if
    end if
  end do
  end do
  end do
end subroutine throwSum

end module discont
