! Segment.f90
! Operations about segments. 

module segment
contains
elemental function whichSegment(x, xLower,xUpper, iLower,iUpper) result (i)
  implicit none
  double presicion, intent (in) :: x, xLower, xUpper
  integer, intent (in) :: iLower, iUpper

  integer :: i

  i = iLower + floor(0.5 + (x-xLower) / (xUpper-xLower) * (iUpper-iLower))
end function whichSegment

end module segment
