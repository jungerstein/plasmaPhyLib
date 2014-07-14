! testEig.f90
! tester of eigen values. 

program testMVA
  use mva, only : goMVA
  implicit none

  double precision, dimension(64, 64, 64) :: Bx, By, Bz
  double precision, dimension(3) :: vals
  double precision, dimension(3, 3) :: vecs

  Bz = 1.0
  By = 1.0
  Bx(:, :, 1:31) = 1.0
  Bx(:, :, 32:64) = -1.0
  call goMVA(vals, vecs, Bx, By, Bz, 55, 5, 32, 13)

  print *, '1st eig: val ', vals(1), 'vec ', vecs(:, 1)
  print *, '2nd eig: val ', vals(2), 'vec ', vecs(:, 2)
  print *, '3rd eig: val ', vals(3), 'vec ', vecs(:, 3)

end program testMVA
