! deblocker.f03
! Make blocks straight. 

program deblocker
  use dataarrange, only: deblock
  implicit none
  
  character (len = 1024) :: charGrid, charBlock, nameFile, nameAus
  integer :: nGrid, sizeBlock
  integer :: lenIOData
  double precision, allocatable, dimension (:, :, :) :: aus, ein

  showUsage: if (command_argument_count() .ne. 4) then
    ! FIXME Redirect to stderr. 
    print *, 'Usage: deblocker aus.bin x.bin nGrid sizeBlock'
    print *, 'Where *.bin should be DOUBLE PRECISION raw binary format'
    ! FIXME Redefine error codes. 
    stop 1
  end if showUsage

  ! FIXME Error handling. 
  call get_command_argument(1, nameAus)
  call get_command_argument(2, nameFile)
  call get_command_argument(3, charGrid)
  call get_command_argument(4, charBlock)

  read(unit = charGrid, fmt = *) nGrid
  read(unit = charBlock, fmt = *) sizeBlock

  if (mod(nGrid, sizeBlock) .ne. 0) then
    print *, 'nGrid (3rd parameter) should be dividable by sizeBlock(4th). '
    ! FIXME Redefine error codes. 
    stop 2
  end if

  allocate(ein(nGrid, nGrid, nGrid), aus(nGrid, nGrid, nGrid))
  inquire(iolength = lenIOData) ein
  
  open(32, file=nameFile, form='unformatted', access='direct', &
    recl=lenIOData, action='read')
  open(33, file=nameAus, form='unformatted', access='direct', &
    recl=lenIOData)
    read(32, rec=1) ein
  close(32)
    call deblock(aus, ein, sizeBlock)
    write(33, rec=1) aus
  close(33)

end program deblocker
