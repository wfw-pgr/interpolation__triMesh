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

    # ------------------------------------------------- #
    # --- [1] Arguments                             --- #
    # ------------------------------------------------- #
    config  = lcf.load__config()
    datFile = "dat/results.dat"
    poiFile = "dat/points.dat"
    pngFile = "png/results.png"

    # ------------------------------------------------- #
    # --- [2] Fetch Data                            --- #
    # ------------------------------------------------- #
    import nkUtilities.load__pointFile as lpf
    Data   = lpf.load__pointFile( inpFile=datFile, returnType="point" )
    points = lpf.load__pointFile( inpFile=poiFile, returnType="point" )

    xp1,xp2,xp3  = Data[:,4], Data[:,7], Data[:,10]
    yp1,yp2,yp3  = Data[:,5], Data[:,8], Data[:,11]
    xAxis        = np.concatenate( [xp1[:,None],xp2[:,None],xp3[:,None],xp1[:,None]], axis=1 )
    yAxis        = np.concatenate( [yp1[:,None],yp2[:,None],yp3[:,None],yp1[:,None]], axis=1 )
    
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
    for ip in range( points.shape[0] ):
        fig.add__plot( xAxis=xAxis[ip,:] , yAxis=yAxis[ip,:] , color="royalBlue", linewidth=1.0 )
    fig.add__plot( xAxis=points[:,x_], yAxis=points[:,y_], marker="o", linestyle="none" )
    fig.set__axis()
    fig.save__figure()


# ======================================== #
# ===  実行部                          === #
# ======================================== #
if ( __name__=="__main__" ):
    display()

