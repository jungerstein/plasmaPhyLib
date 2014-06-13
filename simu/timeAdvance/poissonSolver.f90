! vim : tw=76 ts=2 sw=2
! poissonSolver.f90
! Solves Poisson eq with dynamic relaxation. 
! For research purpose, contact me before submitting your article. 

! Boundary condition: i = 1 or nx; j = 1 or ny. 
! Solve div grad phi = f(phi). 
module poissonSolver
contains
subroutine poissonDynRelaxOneStep(grid_np, grid_n, grid_nm, f, dx)
implicit none

double precision, intent (in), dimension (:, :) :: grid_n
double precision, intent (in), dimension(size(grid_n,1),size(grid_n,2)) :: &
  grid_nm
double precision, intent (out), dimension(size(grid_n,1),size(grid_n,2)) :: &
  grid_np
double precision, intent (in) :: dx
double precision :: f

double precision, parameter :: sqrt_2pi_Inv = 0.39894228040143267794D0 

integer :: nx, ny, i, j
double precision :: L, tau
double precision :: n, nw, ne, ns, nn, nm1, fval, bigH, bigD

nx = size(grid_n, 1)
ny = size(grid_n, 2)
L = max(nx, ny) * dx
tau = L * sqrt_2pi_Inv
bigH = 1 - sqrt(0.5d0) * dx / tau
bigD = 1 + sqrt(0.5d0) * dx / tau

grid_np(1, :) = grid_n(1, :)
grid_np(nx, :) = grid_n(nx, :)
grid_np(:, 1) = grid_n(:, 1)
grid_np(:, ny) = grid_n(:, ny)

goOverGrid: do i = 2, nx-1
do j = 2, ny-1
  n = grid_n(i, j)
  nw = grid_n(i-1, j)
  ne = grid_n(i+1, j)
  ns = grid_n(i, j-1)
  nn = grid_n(i, j+1)
  nm1 = grid_nm(i, j)
  fval = f(n)

  grid_np(i, j) = -bigH/bigD * nm1 + &
                  (nw + ne + ns + nn) / (2 * bigD) - dx**2/(2*bigD) * fval
end do
end do goOverGrid

end subroutine poissonDynRelaxOneStep
end module poissonSolver
