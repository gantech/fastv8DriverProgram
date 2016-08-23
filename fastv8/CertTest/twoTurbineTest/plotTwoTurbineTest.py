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


def plotFourTurbineOutput(case1,d1x,d1y,d1xG,d1yG,case2,d2x,d2y,d2xG,d2yG,chanName,chanUnit=''):

    colors = palettable.colorbrewer.qualitative.Dark2_8.mpl_colors
    fig = plt.figure()
    ax = plt.axes([0,0,1,1])
    plt.locator_params(nbins=5)
    plt.plot(d1x,d1y,'-',color=colors[0],label='Std.')
    plt.plot(d1xG[::80],d1yG[::80],'--^',color=colors[0])    
    plt.plot(d2x,d2y,'-',color=colors[1],label='Mod')
    plt.plot(d2xG[::80],d2yG[::80],'--^',color=colors[1])    
    plt.xlabel('Time (s)')
    plt.ylabel('{} ({})'.format(chanName,chanUnit))
    make_axes_area_auto_adjustable(ax)
    # Shrink current axis's height by 10% on the bottom
    box = ax.get_position()
    ax.set_position([box.x0, box.y0,
                 box.width, box.height * 0.93])
    plt.legend(loc='upper center', bbox_to_anchor=(0.5, 1.3),ncol=2,frameon=False)

    plt.savefig('compare_{}_{}_{}.png'.format(case1,case2,chanName))
    plt.close(fig)

def compareFourTurbineOutput(case1, case1Gold, case2, case2Gold):

    try:
        d1, i1 = fast_io.load_binary_output(case1+'.outb')
    except:
        print "Problem reading file {}.outb".format(case1)

    try:
        d1g, i1g = fast_io.load_binary_output(case1Gold+'.outb')
    except:
        print "Problem reading file {}.outb".format(case1Gold)

    try:
        d2, i2 = fast_io.load_binary_output(case2+'.outb')
    except:
        print "Problem reading file {}.outb".format(case2)
    
    try:
        d2g, i2g = fast_io.load_binary_output(case2Gold+'.outb')
    except:
        print "Problem reading file {}.outb".format(case2Gold)

    nChannels = np.size(d1,1)

    print d2g - d1g
    print d2 - d2g
    
    if (np.size(d2,1) != nChannels):
        print "The number of output channels in the two files are not the same."
        print "{} in {} and {} in {}".format(nChannels, case1, np.size(d2,1))
        return 
    
    for j in range(1,nChannels):
        try:
            mystring = i1['attribute_units'][j]
            print mystring.decode('utf8')
            plotFourTurbineOutput(case1,d1[:,0],d1[:,j],d1g[:,0],d1g[:,j],case2,d2[:,0],d2[:,j],d2g[:,0],d2g[:,j],i1['attribute_names'][j],mystring.decode('utf8'))
        except UnicodeDecodeError:
            print "it was not a ut8-encoded unicode string"
            plotFourTurbineOutput(case1,d1[:,0],d1[:,j],d1g[:,0],d1g[:,j],case2,d2[:,0],d2[:,j],d2g[:,0],d2g[:,j],i1['attribute_names'][j])        
    

if __name__=="__main__":
    
#    compareTwoTurbineOutput('t1_test18','t2_test18')
    compareFourTurbineOutput('t1_test18','t1_test18SingleTurbine','t2_test18','t2_test18SingleTurbine')    
