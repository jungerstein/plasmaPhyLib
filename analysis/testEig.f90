! testEig.f90
! tester of eigen values. 

program testEig
  use mva, only : eigene
  implicit none

  double precision, dimension(3, 3) :: A, vecs
  double precision, dimension(3) :: vals

  A = 0.0
  A(1, 1) = 4.0
  A(2, 2) = 4.0
  A(2, 1) = 3.0
  A(1, 2) = 3.0
  A(3, 3) = 1.0
  call eigene(vals, vecs, A)

  print *, '1st eig: val ', vals(1), 'vec ', vecs(:, 1)
  print *, '2nd eig: val ', vals(2), 'vec ', vecs(:, 2)
  print *, '3rd eig: val ', vals(3), 'vec ', vecs(:, 3)

end program testEig
