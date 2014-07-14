program anaMva
  use discont, only:judgeDisc
  use mva, only:goMVA, printMVA
  implicit none
  
  character (len = 1024) :: charGrid, nameFile, charWidth, charI, charJ, charK
  integer :: nGrid
  integer :: width
  integer :: lenIOData, i, j, k
  double precision, allocatable, dimension (:, :, :) :: Bx, By, Bz
  double precision, dimension (3) :: vals
  double precision, dimension (3, 3) :: vecs

  showUsage: if (command_argument_count() .ne. 6) then
    ! FIXME Redirect to stderr. 
    print *, 'Usage: mva time nGrid i j k width'
    print *, 'Where *.bin should be DOUBLE PRECISION raw binary format'
    ! FIXME Redefine error codes. 
    stop 1
  end if showUsage

  ! FIXME Error handling. 
  call get_command_argument(1, nameFile)
  call get_command_argument(2, charGrid)
  call get_command_argument(4, charI)
  call get_command_argument(5, charJ)
  call get_command_argument(6, charK)
  call get_command_argument(6, charWidth)

  read(unit = charGrid, fmt = *) nGrid
  read(unit = charWidth, fmt = *) width
  read(unit = charI, fmt = *) i
  read(unit = charJ, fmt = *) j
  read(unit = charK, fmt = *) k

  allocate(Bx(nGrid, nGrid, nGrid))
  allocate(By(nGrid, nGrid, nGrid))
  allocate(Bz(nGrid, nGrid, nGrid))
  inquire(iolength = lenIOData) Bx
  
  open(32, file='bx'//nameFile, form='unformatted', access='direct', &
    recl=lenIOData, action='read')
  open(33, file='by'//nameFile, form='unformatted', access='direct', &
    recl=lenIOData, action='read')
  open(34, file='bz'//nameFile, form='unformatted', access='direct', &
    recl=lenIOData, action='read')
    read(32, rec=1) Bx
    read(33, rec=1) By
    read(34, rec=1) Bz
  close(34)
  close(33)
  close(32)

  call goMVA(vals, vecs, Bx, By, Bz, i, j, k, width)
  call printMVA(vals, vecs)

  call judgeDisc(Bx, By, Bz, i, j, k, width)

end program  
