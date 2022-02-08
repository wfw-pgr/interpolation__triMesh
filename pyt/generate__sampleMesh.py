import numpy as np
import os, sys
import gmsh

# ========================================================= #
# ===  geometry                                    === #
# ========================================================= #

def make__geometry():

    import nkGmshRoutines.generate__sector180 as sec
    ret = sec.generate__sector180( r2=1.0, defineSurf=True, side="+" )
    ret = sec.generate__sector180( r2=1.0, defineSurf=True, side="-" )
    return()


# ========================================================= #
# ===   実行部                                          === #
# ========================================================= #

if ( __name__=="__main__" ):
    

    # ------------------------------------------------- #
    # --- [1] initialization of the gmsh            --- #
    # ------------------------------------------------- #
    gmsh.initialize()
    gmsh.option.setNumber( "General.Terminal", 1 )
    gmsh.option.setNumber( "Mesh.Algorithm"  , 5 )
    gmsh.option.setNumber( "Mesh.Algorithm3D", 4 )
    gmsh.option.setNumber( "Mesh.SubdivisionAlgorithm", 0 )
    gmsh.model.add( "model" )
    
    
    # ------------------------------------------------- #
    # --- [2] Modeling                              --- #
    # ------------------------------------------------- #

    make__geometry()
    
    gmsh.model.occ.synchronize()
    gmsh.model.occ.removeAllDuplicates()
    gmsh.model.occ.synchronize()


    # ------------------------------------------------- #
    # --- [3] Mesh settings                         --- #
    # ------------------------------------------------- #
    
    # meshFile = "dat/mesh.conf"
    # physFile = "dat/phys.conf"
    # import nkGmshRoutines.assign__meshsize as ams
    # meshes = ams.assign__meshsize( meshFile=meshFile, physFile=physFile )
    
    gmsh.option.setNumber( "Mesh.CharacteristicLengthMin", 0.1 )
    gmsh.option.setNumber( "Mesh.CharacteristicLengthMax", 0.1 )

    # ------------------------------------------------- #
    # --- [4] post process                          --- #
    # ------------------------------------------------- #
    gmsh.model.occ.synchronize()
    gmsh.model.mesh.generate(3)
    gmsh.write( "msh/model.msh" )
    gmsh.finalize()

    # ------------------------------------------------- #
    # --- [5] nodes.dat elems.dat                   --- #
    # ------------------------------------------------- #
    import nkMeshRoutines.load__meshio as lms
    elems, nodes = lms.load__meshio( mshFile="msh/model.msh", elementType="triangle", \
                                     returnType="elem-node" )
    print( elems )
    print( nodes )
    print( elems.shape )
    print( nodes.shape )
    
    import nkUtilities.save__pointFile as spf
    spf.save__pointFile( outFile="dat/nodes.dat", Data=nodes, fmt="%15.8e" )
    spf.save__pointFile( outFile="dat/elems.dat", Data=elems, fmt="%12d"   )
