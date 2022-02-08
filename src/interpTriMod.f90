module interpTriMod
contains

  ! ====================================================== !
  ! === linear interpolation form a triangle           === !
  ! ====================================================== !
  subroutine interpolation__triElement( nodes, elems, points, results, nNodes, nElems, nPoints )
    implicit none
    integer         , intent(in)    :: nNodes, nElems, nPoints
    integer         , intent(in)    :: elems(3,nElems)
    double precision, intent(inout) :: nodes(3,nNodes), points(3,nPoints)
    integer         , intent(inout) :: results(nPoints)
    integer                         :: iN, iP
    double precision, allocatable   :: node2D(:,:)
    integer         , parameter     :: x_=1, y_=2, z_=3
    integer         , parameter     :: nd1_=1, nd2_=2, nd3_=3
    
    ! make sure that points & data
    ! ------------------------------------------------------ !
    ! --- [1] initialization                             --- !
    ! ------------------------------------------------------ !
    allocate( node2D(3,nNodes) )
    do iN=1, nNodes
       node2D(x_,iN)  = nodes(x_,iN)
       node2D(y_,iN)  = nodes(y_,iN)
       node2D(z_,iN)  = 0.d0
    enddo
    do iP=1, nPoints
       points(z_,iP) = 0.d0
    enddo
    call investigate__inside_triangle( node2D, elems, points, results, nNodes, nElems, nPoints )
    write(6,*) results
    do iP=1, nPoints
       if ( results(iP) > 0 ) then
          write(6,*) nodes(x_:z_,elems(nd1_,results(iP))), &
               & nodes(x_:z_,elems(nd2_,results(iP))), nodes(x_:z_,elems(nd3_,results(iP))), points(:,iP)
          call barycentric__interpolate( nodes(x_:z_,elems(nd1_,results(iP))), &
               & nodes(x_:z_,elems(nd2_,results(iP))), nodes(x_:z_,elems(nd3_,results(iP))), points(:,iP) )
       else
          ! call near_edge_interpolation
       endif
    enddo
    
    return
  end subroutine interpolation__triElement


  ! ====================================================== !
  ! === investigate__inside_triangle                   === !
  ! ====================================================== !
  subroutine investigate__inside_triangle( nodes, elems, points, results, nNodes, nElems, nPoints )
    implicit none
    integer         , intent(in)    :: nNodes, nElems, nPoints
    integer         , intent(in)    :: elems(3,nElems)
    double precision, intent(in)    :: nodes(3,nNodes), points(3,nPoints)
    integer         , intent(inout) :: results(nPoints)
    integer                         :: iP, iE, iv
    double precision                :: norm_length, inner21, inner31, dist
    double precision                :: edge(3,3), VtoP(3,3), cross(3,3), normc(3), cog(3)
    double precision, allocatable   :: distance(:)
    integer         , parameter     :: x_  =1, y_  =2, z_  =3
    integer         , parameter     :: e1_ =1, e2_ =2, e3_ =3
    integer         , parameter     :: v1_ =1, v2_ =2, v3_ =3
    integer         , parameter     :: nd1_=1, nd2_=2, nd3_=3
    double precision, parameter     :: eps         = 1.d-10
    double precision, parameter     :: onethird    = 1.d0 / 3.d0
    double precision, parameter     :: LARGE_VALUE = 1.d20

    allocate( distance(nPoints) )
    distance(:) = LARGE_VALUE
    
    do iP=1, nPoints

       results(iP)  = -1
       distance(iP) = LARGE_VALUE
       
       do iE=1, nElems

          ! ------------------------------------------------------ !
          ! --- [1] edge vector                                --- !
          ! ------------------------------------------------------ !
          edge(x_:z_,e1_) = nodes(x_:z_,elems(nd1_,iE)) - nodes(x_:z_,elems(nd3_,iE) )
          edge(x_:z_,e2_) = nodes(x_:z_,elems(nd2_,iE)) - nodes(x_:z_,elems(nd1_,iE) )
          edge(x_:z_,e3_) = nodes(x_:z_,elems(nd3_,iE)) - nodes(x_:z_,elems(nd2_,iE) )

          ! ------------------------------------------------------ !
          ! --- [2] vector from a vertex to the Point          --- !
          ! ------------------------------------------------------ !
          VtoP(x_:z_,v1_)  = points(x_:z_,iP) - nodes(x_:z_,elems(nd1_,iE) )
          VtoP(x_:z_,v2_)  = points(x_:z_,iP) - nodes(x_:z_,elems(nd2_,iE) )
          VtoP(x_:z_,v3_)  = points(x_:z_,iP) - nodes(x_:z_,elems(nd3_,iE) )

          ! ------------------------------------------------------ !
          ! --- [3] cross product                              --- !
          ! ------------------------------------------------------ !
          do iv=v1_, v3_
             cross(x_,iv) = edge(y_,iv)*VtoP(z_,iv) - edge(z_,iv)*VtoP(y_,iv)
             cross(y_,iv) = edge(z_,iv)*VtoP(x_,iv) - edge(x_,iv)*VtoP(z_,iv)
             cross(z_,iv) = edge(x_,iv)*VtoP(y_,iv) - edge(y_,iv)*VtoP(x_,iv)
             normc   (iv) = sqrt( cross(x_,iv)**2 + cross(y_,iv)**2 + cross(z_,iv)**2 )
          enddo
          
          ! ------------------------------------------------------ !
          ! --- [4] check on edge / on vertex                  --- !
          ! ------------------------------------------------------ !
          do iv=v1_, v3_
             if ( normc(iv) <= eps ) then
                norm_length = ( sqrt( sum( VtoP(:,iv)**2 ) ) ) / sqrt( sum( edge(:,iv)**2 ) )
                if ( ( norm_length >= 0.d0 ).and.( norm_length <= 1.d0 ) ) then
                   ! -- [FOUND] :: exit -- !
                   results(iP) = iE
                   exit
                endif
             endif
          enddo
          ! ------------------------------------------------------ !
          ! --- [5] check inside / outside                     --- !
          ! ------------------------------------------------------ !
          inner21 = sum( cross(:,v2_)*cross(:,v1_) )
          inner31 = sum( cross(:,v3_)*cross(:,v1_) )
          if ( ( inner21 > 0.d0 ).and.( inner31 > 0.d0 ) ) then
             ! -- [FOUND] :: exit -- !
             results(iP) = iE
             exit
          else
             ! -- inquire most closest edge -- !
             cog(x_:z_)    = onethird*( + nodes(x_:z_,elems(nd1_,iE)) &
                  &                     + nodes(x_:z_,elems(nd2_,iE)) &
                  &                     + nodes(x_:z_,elems(nd3_,iE)) )
             dist          = sqrt( sum( ( points(:,iP) - cog(:) )**2 ) )
             if ( dist < distance(iP) ) then
                distance(iP) = min( distance(iP), dist )
                results(iP)  = - iE
             endif
          endif
          
       enddo
       
    enddo
    
    return
  end subroutine investigate__inside_triangle

  
  ! ====================================================== !
  ! === interpolation in barycentric coordinate        === !
  ! ====================================================== !
  subroutine barycentric__interpolate( v1, v2, v3, xp )
    implicit none
    double precision, intent(in)    :: v1(3), v2(3), v3(3)
    double precision, intent(inout) :: xp(3)
    double precision                :: denom, pI, w1, w2, w3
    integer         , parameter     :: x_=1, y_=2, z_=3

    denom = ( ( v2(y_)-v3(y_) )*( v1(x_)-v3(x_) ) + ( v3(x_)-v2(x_) )*( v1(y_)-v3(y_) ) )
    if ( denom.eq.0.d0 ) then
       pI = 0.d0
    else
       pI = 1.d0 / denom
    endif
    w1    = pI * ( ( v2(y_)-v3(y_) )*( xp(x_)-v3(x_) ) + ( v3(x_)-v2(x_) )*( xp(y_)-v3(y_) ) )
    w2    = pI * ( ( v3(y_)-v1(y_) )*( xp(x_)-v3(x_) ) + ( v1(x_)-v3(x_) )*( xp(y_)-v3(y_) ) )
    w3    = 1.d0  - w1 - w2

    xp(z_) = w1*v1(z_) + w2*v2(z_) + w3*v3(z_)
    
    return
  end subroutine barycentric__interpolate
  
  
end module interpTriMod
