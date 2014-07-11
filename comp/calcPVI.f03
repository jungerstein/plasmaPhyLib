! calcPVI.f03
! Main program (CUI) for computing PVI. 
! Usage: 
!  calcPVI aus.bin x.bin y.bin z.bin 128
! Need fortran 2003

program calcPVI
  use pvi
  implicit none

  integer :: n, lenIO
  double precision, allocatable, dimension(:, :, :) :: aus, x, y, z
  character (len = 1024) :: nameX, nameY, nameZ, nameAus, charN

  showUsage: if (command_argument_count() .lt. 6) then
    ! FIXME Redirect to stderr. 
    print *, 'Usage: calcPVI aus.bin x.bin y.bin z.bin n'
    print *, 'Where *.bin should be DOUBLE PRECISION raw binary format'
    ! FIXME Redefine error codes. 
    stop 1
  end if

  ! FIXME Error handling. 
  call get_command_argument(1, nameAus, 1023)
  call get_command_argument(2, nameX, 1023)
  call get_command_argument(3, nameY, 1023)
  call get_command_argument(4, nameZ, 1023)
  call get_command_argument(5, charN, 1023)

  read(unit = charN, fmt=*) n
  ! DEBUG CODE, 8 lines
  ! VVVVVVVVVV
  print *, nameAus
  print *, nameX
  print *, nameY
  print *, nameZ
  print *, n
  ! ^^^^^^^^^^
  allocate(x(n, n, n), y(n, n, n), z(n, n, n), aus(n, n, n))
  inquire(iolength=lenIO) x
  open(32, file=nameX, form='unformatted', access='direct', recl=rec_len, &
    action='read') 
  open(33, file=nameY, form='unformatted', access='direct', recl=rec_len, &
    action='read') 
  open(34, file=nameZ, form='unformatted', access='direct', recl=rec_len, &
    action='read') 
  open(35, file=nameAus, form='unformatted', access='direct', recl=rec_len) 
    read(32, rec=1) x
    read(33, rec=1) y
    read(34, rec=1) z
  close(34)
  close(33)
  close(32)
    call compPVI(aus, x, y, z)
    write(35, rec=1) aus
  close(35)
end program calcPVI
