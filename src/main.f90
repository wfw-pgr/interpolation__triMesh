program main
  use interpTriMod
  implicit none
  integer, parameter            :: lun  = 50
  integer, parameter            :: cLen = 300
  integer                       :: iL, nNodes, nElems, nPoints, nCmp
  character(cLen)               :: nodeFile   = "dat/nodes.dat"
  character(cLen)               :: elemFile   = "dat/elems.dat"
  character(cLen)               :: pointFile  = "dat/points.dat"
  character(cLen)               :: outFile    = "dat/results.dat"
  character(cLen)               :: cmt
  double precision, allocatable :: points(:,:), nodes(:,:)
  integer         , allocatable :: elems(:,:), results(:,:)
  logical         , parameter   :: flag__numFrom1 = .true.
  
  ! ------------------------------------------------------ !
  ! --- [1] load nodes file                            --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(nodeFile),status="old")
  read(lun,*)
  read(lun,*) cmt, nNodes, nCmp
  read(lun,*) cmt, nNodes, nCmp
  allocate( nodes(nCmp,nNodes) )
  do iL=1, nNodes
     read(lun,*) nodes(:,iL)
  enddo
  close(lun)

  do iL=1, nNodes
     nodes(3,iL) = sqrt( nodes(1,iL)**2 + nodes(2,iL)**2 )
  enddo

  ! ------------------------------------------------------ !
  ! --- [2] load elems File                            --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(elemFile),status="old")
  read(lun,*)
  read(lun,*) cmt, nElems, nCmp
  read(lun,*) cmt, nElems, nCmp
  allocate( elems(nCmp,nElems) )
  do iL=1, nElems
     read(lun,*) elems(:,iL)
  enddo
  close(lun)

  if ( flag__numFrom1 ) then
     elems(:,:) = elems(:,:) + 1
  endif

  ! ------------------------------------------------------ !
  ! --- [3] load point File                            --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(pointFile),status="old")
  read(lun,*)
  read(lun,*) cmt, nPoints, nCmp
  read(lun,*) cmt, nPoints, nCmp
  allocate( points(nCmp,nPoints) )
  do iL=1, nPoints
     read(lun,*) points(:,iL)
  enddo
  close(lun)

  ! ------------------------------------------------------ !
  ! --- [4] test inside__triangle                      --- !
  ! ------------------------------------------------------ !
  allocate( results( 2, nPoints ) )
  call interpolation__triElement( nodes, elems, points, results, nNodes, nElems, nPoints )

  ! ------------------------------------------------------ !
  ! --- [5] save results                               --- !
  ! ------------------------------------------------------ !
  open(lun,file=trim(outFile),status="replace")
  write(lun,"(a)") "# x_ y_ z_ "
  write(lun,"(a,2(i10,1x))") "# ", nPoints, 3
  write(lun,"(a,2(i10,1x))") "# ", nPoints, 3
  do iL=1, nPoints
     write(lun,*) points(:,iL), results(:,iL)
  enddo
  close(lun)

end program main
