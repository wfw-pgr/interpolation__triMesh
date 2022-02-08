import sys
import numpy                      as np
import nkUtilities.load__config   as lcf
import nkUtilities.plot1D         as pl1
import nkUtilities.configSettings as cfs


# ========================================================= #
# ===  display                                          === #
# ========================================================= #
def display():

    x_,y_ = 0, 1

    index = 155

    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config  = lcf.load__config()
    elmFile = "dat/elems.dat"
    nodFile = "dat/nodes.dat"
    poiFile = "dat/points.dat"
    pngFile = "png/results.png"

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    nodes  = lpf.load__pointFile( inpFile=nodFile, returnType="point" )
    elems  = lpf.load__pointFile( inpFile=elmFile, returnType="point" )
    points = lpf.load__pointFile( inpFile=poiFile, returnType="point" )
    elems  = np.array( elems, dtype=np.int64 )

    edge   = elems[index-1,:]
    print( edge.shape )
    edge   = edge[ [0,2] ]
    print( edge )
    edge   = nodes[edge,:]
    print( edge )
    
    element = nodes[ elems[:,:],: ]
    element = np.concatenate( [ element[:,:,:], np.reshape( element[:,0,:], (-1,1,3) ) ], axis=1 )
    
    # ------------------------------------------------- #
    # --- [3] config Settings                       --- #
    # ------------------------------------------------- #
    cfs.configSettings( configType="plot1D_def", config=config )
    config["xTitle"]         = "X (m)"
    config["yTitle"]         = "Y (m)"
    config["plt_xAutoRange"] = True
    config["plt_yAutoRange"] = True
    config["plt_xRange"]     = [-5.0,+5.0]
    config["plt_yRange"]     = [-5.0,+5.0]
    config["plt_linewidth"]  = 1.0
    config["xMajor_Nticks"]  = 5
    config["yMajor_Nticks"]  = 5

    # ------------------------------------------------- #
    # --- [4] plot Figure                           --- #
    # ------------------------------------------------- #
    fig = pl1.plot1D( config=config, pngFile=pngFile )
    for ik,el in enumerate( element ):
        fig.add__plot( xAxis=el[:,x_], yAxis=el[:,y_]    , marker="o", linestyle="--"  , color="grey" )
    fig.add__plot( xAxis=points[:,x_], yAxis=points[:,y_], marker="o", linestyle="none", color="red"  )
    fig.add__plot( xAxis=edge[:,x_], yAxis=edge[:,y_], linestyle="-", color="red"  )
    fig.set__axis()
    fig.save__figure()


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display()

