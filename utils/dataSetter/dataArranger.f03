! dataArranger.f03
! Main program for arrange data. 

program dataArranger
  use dataarrange, only: arrangeData
  implicit none
  
  character (len = 1024) :: charGrid, charBlock, nameFile, nameAus, nameCoor
  integer :: nGrid, nBlock
  integer :: lenIOData, lenIOCoor
  double precision, allocatable, dimension (:, :, :) :: aus, ein
  double precision, allocatable, dimension (:, :, :, :) :: coord

  showUsage: if (command_argument_count() .ne. 5) then
    ! FIXME Redirect to stderr. 
    print *, 'Usage: dataArranger aus.bin x.bin coor.bin nGrid nBlock'
    print *, 'Where *.bin should be DOUBLE PRECISION raw binary format'
    ! FIXME Redefine error codes. 
    stop 1
  end if showUsage

  ! FIXME Error handling. 
  call get_command_argument(1, nameAus)
  call get_command_argument(2, nameFile)
  call get_command_argument(3, nameCoor)
  call get_command_argument(4, charGrid)
  call get_command_argument(5, charBlock)

  read(unit = charGrid, fmt = *) nGrid
  read(unit = charBlock, fmt = *) nBlock

  if (mod(nGrid, nBlock) .ne. 0) then
    print *, 'nGrid (4th parameter) should be dividable by nBlock(5th). '
    ! FIXME Redefine error codes. 
    stop 2
  end if

  allocate(ein(nGrid, nGrid, nGrid), aus(nGrid, nGrid, nGrid))
  allocate(coord(3, nBlock, nBlock, nBlock))
  inquire(iolength = lenIOData) ein
  inquire(iolength = lenIOCoor) coord
  
  open(32, file=nameFile, form='unformatted', access='direct', &
    recl=lenIOData, action='read')
  open(33, file=nameCoor, form='unformatted', access='direct', &
    recl=lenIOCoor, action='read')
  open(34, file=nameAus, form='unformatted', access='direct', &
    recl=lenIOData)
    read(32, rec=1) ein
    read(33, rec=1) coord
  close(33)
  close(32)
    call arrangeData(aus, ein, coord)
    write(34, rec=1) aus
  close(34)

end program dataArranger
