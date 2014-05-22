! vim : tw=72 sw=2 ts=2
! gs.f90
! Solver (e.g. GS Reconstruction) with fixed-value boundary conditions. 
! Coded by jungerstein. 

module gsRecon
! Solve div grad A = f(A)
! Old must have four boundaries already written. 
function gsStep(old, boundary, isBoundary, dx, dy, f, omega, tol) result (new)
  implicit none
  double precision, dimension (:, :), intent (in) :: old, boundary
  locigal, dimension(size(old, 1), size(old, 2), intent (in) :: isBoundary
  double precision, intent (in) :: dx, dy
  double precision :: f
  double precision, intent (in) :: omega, tol
  double precision, dimension (size(old, 1), size(old, 2)) :: new

  integer :: i, j, nx, ny
  nx = size(old, 1)
  ny = size(old, 2)
  new(1, :) = old(1, :)
  new(nx, :) = old(nx, :)
  new(:, 1) = old(:, 1)
  new(:, ny) = old(:, ny)

  jLoop: do j = 2, ny - 1
    iLoop: do i = 2, nx - 1
      isBord: if (isBoundary(i, j)) then
        new(i, j) = boundary(i, j)
      else
        new(i, j) = (1 - omega) * old(i, j) &
                  + omega * &
                  (  (new(i-1,j) + old(i+1,j)) / (dx**2) &
                   + (new(i,j-1) + old(i,j+1)) / (dy**2) &
                   - f(old(i,j))) &
                  / (2 / (dx**2) + 2 / (dy**2))
      end if isBord
    end do iLoop
  end do jLoop
end function gsStep
end module gsRocon
