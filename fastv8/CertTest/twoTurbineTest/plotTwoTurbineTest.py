import matplotlib
matplotlib.use('AGG')
matplotlib.rc_file('twoColumnReport.rc')
import fast_io
import numpy as np
import sys, os, palettable
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.axes_divider import make_axes_area_auto_adjustable

def plotTwoTurbineOutput(case1,d1x,d1y,case2,d2x,d2y,chanName,chanUnit=''):
    
    fig = plt.figure()
    ax = plt.axes([0,0,1,1])
    ax.set_color_cycle(palettable.colorbrewer.qualitative.Dark2_8.mpl_colors)
    plt.plot(d1x,d1y,label=case1)
    plt.plot(d2x,d2y,label=case2)
    plt.xlabel('Time (s)')
    plt.ylabel('{} ({})'.format(chanName,chanUnit))
    make_axes_area_auto_adjustable(ax)
    plt.savefig('compare_{}_{}_{}.png'.format(case1,case2,chanName))
    plt.close(fig)

def compareTwoTurbineOutput(case1, case2):

    try:
        d1, i1 = fast_io.load_binary_output(case1+'.outb')
    except:
        print "Problem reading file {}.outb".format(case1)

    try:
        d2, i2 = fast_io.load_binary_output(case2+'.outb')
    except:
        print "Problem reading file {}.outb".format(case2)
    
    nChannels = np.size(d1,1)

    if (np.size(d2,1) != nChannels):
        print "The number of output channels in the two files are not the same."
        print "{} in {} and {} in {}".format(nChannels, case1, np.size(d2,1))
        return 
    
    for j in range(1,nChannels):
        plotTwoTurbineOutput(case1,d1[:,0],d1[:,j],case2,d2[:,0],d2[:,j],i1['attribute_names'][j])



if __name__=="__main__":
    
    compareTwoTurbineOutput('t1_test18','t2_test18')
