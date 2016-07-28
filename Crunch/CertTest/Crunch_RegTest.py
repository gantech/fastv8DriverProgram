#!/usr/bin/python

# This program runs the certification test for Crunch.
# Created for NREL/NWTC by Marshall Buhl on 28-Dec-2012.

# You will need Python installed on your system to run this script.
# Although I used the 64-bit version of Python on my 64-bit Windows 7 computer,
# the 32-bit version should work fine.  I used Python 2.7.2 to test this script.

# You will also need matplotlib with the appropriate 32/64-bit option.  There is no
# "official" 64-bit distribution, so you may want to condider going all 32-bit to
# avoid the hassle.

import difflib
import os
import platform
import pylab
import sys
import time

#======================================================================
def CompFiles( NewFile, SavedSuff ):


      # Get the file names set correctly.

   SavedName = 'Test' + TestStr + SavedSuff
   SavedFile = os.sep.join( ( 'TstFiles', SavedName ) )

   if ( NewFile != SavedName ):
      if ( os.path.isfile( SavedName ) ):
         os.remove( SavedName )
      os.rename( NewFile, SavedName )


      # Make sure they are there.

   if ( not os.path.isfile( SavedName ) ):
      print( '\n File "' + SavedName + '" does not exist for comparison.  Aborting ' + RegProgram + '.' )
      sys.exit()

   if ( not os.path.isfile( SavedFile ) ):
      print( '\n File "' + SavedFile + '" does not exist for comparison.  Aborting ' + RegProgram + '.' )
      sys.exit()


      # Read the two files.

   NewDesc   = open( SavedName, 'r' )
   SavedDesc = open( SavedFile, 'r' )

   NewText   =   NewDesc.readlines();   NewDesc.close
   SavedText = SavedDesc.readlines(); SavedDesc.close


      # Strip the carriage returns for Luinux compatibility.

   NewText_NoCR   = list( '' )
   SavedText_NoCR = list( '' )

   for line in NewText:
      line = line.rstrip( '\n' )
      line = line.rstrip( '\r' )
      NewText_NoCR.append( line + '\n' )

   for line in SavedText:
      line = line.rstrip( '\n' )
      line = line.rstrip( '\r' )
      SavedText_NoCR.append( line + '\n' )


      # Compare the lines in the two files and output differences to the comparison file.

   CompareFileObj.write( '------\nFile 1: "' +   NewFile + '",  File 2: "' + SavedFile + '"\n' )

   BlkDiffs1 = 0
   NumDiffs1 = 0
   NumDiffs2 = 0
   LastWas2  = True

   for line in difflib.unified_diff( NewText_NoCR, SavedText_NoCR, fromfile='IGNORE', tofile='IGNORE', n=0 ):
      if ( ( line[:10] != '--- IGNORE' ) and ( line[:10] != '+++ IGNORE' ) ):
         if ( line[:1] == '-' ):
            if ( LastWas2 ):
               if ( ( NumDiffs1 == MaxDiffs ) or ( NumDiffs2 == MaxDiffs ) ): break
               CompareFileObj.write( '-\n' )
            LastWas2   = False
            BlkDiffs2  = 0
            CompareFileObj.write( '   1: ' + line[1:].rstrip( '\n' ) + '\n' )
            BlkDiffs1 += 1
            NumDiffs1 += 1
         elif ( line[:1] == '+' ):
            CompareFileObj.write( '   2: ' + line[1:].rstrip( '\n' ) + '\n' )
            NumDiffs2 += 1
            if ( NumDiffs2 == MaxDiffs ): break
            LastWas2 = True

#======================================================================
def LinePlot( NewFile, SavedSuff, WinTitle, PlotTitle, PlotFileName, FirstLine, NumPts, Xcol, Xtype, Xlab, Ycol, Ytype, Ylab ):


      # Read the data.

   SavedFile = os.sep.join( ( 'TstFiles', 'Test' ) ) + TestStr + SavedSuff
   NewDesc   = open( NewFile  , 'r' )
   SavedDesc = open( SavedFile, 'r' )

   Xnew = [0.0] * NumPts
   Ynew = [0.0] * NumPts
   Xold = [0.0] * NumPts
   Yold = [0.0] * NumPts

   Ind = 0

   for Line in NewDesc.readlines()[FirstLine-1:]:
      MyWords   = Line.split()
      Xnew[Ind] = float( MyWords[Xcol-1] )
      Ynew[Ind] = float( MyWords[Ycol-1] )
      Ind      += 1

   NewDesc.close

   Ind = 0

   for Line in SavedDesc.readlines()[FirstLine-1:]:
      MyWords   = Line.split()
      Xold[Ind] = float( MyWords[Xcol-1] )
      Yold[Ind] = float( MyWords[Ycol-1] )
      Ind      += 1

   SavedDesc.close


      # Create the plot and save it in a file.

   fig = pylab.figure()
   pylab.plot( Xnew, Ynew, color='b', linewidth=2 )
   pylab.plot( Xold, Yold, color='r', linewidth=2, linestyle='--' )
   pylab.xscale( Xtype )
   pylab.yscale( Ytype )

   pylab.xlabel( Xlab)
   pylab.ylabel( Ylab )
   pylab.title( PlotTitle )
   pylab.savefig( PlotFileName )

   fig.canvas.set_window_title( WinTitle )

   pylab.draw()


#======================================================================
def RunCase():

   Str     = TestedProg + ' Test #' + TestStr + ':  ' + TestTitle[Test]
   print( '   ' + Str )

   Sttus = os.system( TestedExec + ' Test' + TestStr + ProgExt + ' > ' + NullDev )

   if Sttus > 0:
      print( '\n ' + TestedProg + ' Test' + TestStr + ' failed.  Aborting ' + RegProgram + '.' )
      sys.exit()

   CompareFileObj.write( '\n\n==========================================================================================================\n' )
   CompareFileObj.write( Str + '\n' )
#======================================================================
# Main Program.

   # Set up some parameters.

MaxDiffs   = 30
NullDev    = os.path.devnull
ProgExt    = '.cru'
RegProgram = 'Crunch_RegTest (v1.01.00a-mlb, 25-January-2013)'
TestedExec = os.sep.join( ('..', 'Crunch' ) )
TestedProg = 'Crunch'
pylab.ion()


   # Get the command-line argument to specify which executable to use.
   # Print out the syntax if one of the required exectuable types is not listed.

if ( len( sys.argv ) == 1 ):
   CLarg = ' '
else:
   CLarg = sys.argv[1]

if sys.platform == 'win32':
   if ( ( CLarg != 'win32' ) and ( CLarg != 'win64' ) and ( CLarg != 'gwin32' ) and ( CLarg != 'gwin64' ) ):
      print('\n Syntax is:\n')
      print('    ' + TestedProg + '_RegTest.py <ExecType>\n')
      print(' where <ExecType> is one of: win32, win64, gwin32, or gwin64.\n')
      print(' Example:\n')
      print('    ' + TestedProg + '_RegTest.py win32\n')
      print(' will execute ' + TestedProg + '_win32.exe.')
      sys.exit()
   TestedExec += '_' + CLarg + '.exe'
elif sys.platform.startswith('linux'):
   if ( ( CLarg != 'glin32' ) and ( CLarg != 'glin64' ) ):
      print('\n Syntax is:\n')
      print('    python ' + TestedProg + '_RegTest.py <ExecType>\n')
      print(' where <ExecType> is one of: glin32 or glin64.\n')
      print(' Example:\n')
      print('    python ' + TestedProg + '_RegTest.py glin32\n')
      print(' will execute ' + TestedProg + '_glin32.\n')
      sys.exit()
   TestedExec += '_' + CLarg


if sys.platform == 'win32':
   Editor      = 'NotePad'
   Redirect    = ' > ' + NullDev
elif sys.platform.startswith('linux'):
   Editor      = 'vi'
   Redirect    = ''

print('\n Regression testing ' + TestedExec + '.\n')

TestTitle = [ '' ]
TestTitle.append( 'Validation test using sine waves.  Includes slope-change peak-listing.'                                                              )
TestTitle.append( 'Statistical summaries and extrapolated values for individual files.'                                                                 )
TestTitle.append( 'Analyses of aggregate files including normalized 1-D RF cycle counting.'                                                             )
TestTitle.append( 'Analysis of aggregate files for unnormalized 1-D RF cycle counting.'                                                                 )
TestTitle.append( 'Analyses of individual files including normalized 2-D RF cycle counting.'                                                            )
TestTitle.append( 'Analysis of an individual file for unnormalized 2-D RF cycle counting.  Filtered data.'                                              )
TestTitle.append( 'Analysis of an individual file for generating raw RF cycles.'                                                                        )
TestTitle.append( 'Analysis of an individual file for generating low-pass filtered output.'                                                             )
TestTitle.append( 'Analysis of an individual file for generating moving averages, load roses, and extreme events.'                                      )
TestTitle.append( 'Analysis of an individual file for generating calculated channels, load roses, and aggregate extreme events.'                        )
TestTitle.append( 'Validation test using sine waves.  Includes threshhold peak-listing.  Autodetecting format.'                                         )
TestTitle.append( 'Statistical summaries and extrapolated values for individual files.  Autodetecting format.'                                          )
TestTitle.append( 'Analyses of aggregate files including normalized 1-D RF cycle counting.  Autodetecting format.'                                      )
TestTitle.append( 'Analysis of aggregate files for unnormalized 1-D RF cycle counting.  Autodetecting format.'                                          )
TestTitle.append( 'Analyses of individual files including normalized 2-D RF cycle counting.  Autodetecting format.'                                     )
TestTitle.append( 'Analysis of an individual file for unnormalized 2-D RF cycle counting.  Filtered data.  Autodetecting format.'                       )
TestTitle.append( 'Analysis of an individual file for generating raw RF cycles.  Autodetecting format.'                                                 )
TestTitle.append( 'Analysis of an individual file for generating low-pass filtered output.  Autodetecting format.'                                      )
TestTitle.append( 'Analysis of an individual file for generating moving averages, load roses, and extreme events.  Autodetecting format.'               )
TestTitle.append( 'Analysis of an individual file for generating calculated channels, load roses, and aggregate extreme events.  Autodetecting format.' )
TestTitle.append( 'Aggregate analysis of two binary files for generating calculated channels, probability mass, and statistics.'                        )
TestTitle.append( 'Aggregate analysis of two binary files for generating calculated channels, probability mass, and statistics.  NumCols set to 0.'     )

#======================================================================


   # Open the comparisons file and add the header.

CompareFile    = 'Crunch_RegTest.comp'
CompareFileObj = open( CompareFile, 'w' )

CompareFileObj.write( '\nThis regression test was generated by ' + RegProgram + ' on ' + time.asctime( time.localtime(time.time()) ) + '.\n' )

CompareFileObj.write( '\n##########################################################################################################\n' )
CompareFileObj.write(   '# Inspect this file for any differences between your results and the saved results.  Any differing lines #\n' )
CompareFileObj.write(   '# will be listed.  The only differences should be the time stamps near the beginnong of each file.       #\n' )
CompareFileObj.write(   '#                                                                                                        #\n' )
CompareFileObj.write(   '# However, you may see some differences in the numbers depending on the compiler and operating system.   #\n' )
CompareFileObj.write(   '##########################################################################################################\n' )

CompareFileObj.write('\nRegression testing ' + TestedExec + '.\n')


#******************************************************
Test    = 1
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Validate.azi'        , '.azi'  )
CompFiles( 'Validate.pmf'        , '.pmf'  )
CompFiles( 'Validate.rcc'        , '.rcc'  )
CompFiles( 'Validate.sts'        , '.sts'  )
CompFiles( 'Validate_1P_Sine.pek', 'a.pek' )
CompFiles( 'Validate_2P_Sine.pek', 'b.pek' )
CompFiles( 'Validate_3P_Sine.pek', 'c.pek' )
CompFiles( 'Validate_4P_Sine.pek', 'd.pek' )


#******************************************************
Test    = 2
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'WindSpd.sum' , 'a.sum'  )
CompFiles( 'B1_Moop.sum' , 'b.sum'  )
CompFiles( 'B1_Moop.ext' ,  '.ext'  )


#******************************************************
Test    = 3
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Test'+TestStr+'.azi'        , '.azi'  )
CompFiles( 'Test'+TestStr+'.pmf'        , '.pmf'  )
CompFiles( 'Test'+TestStr+'.rcc'        , '.rcc'  )
CompFiles( 'Test'+TestStr+'.sts'        , '.sts'  )
CompFiles( 'Test'+TestStr+'_Azimuth.pek', 'a.pek' )
CompFiles( 'Test'+TestStr+'_B1_Mip.pek' , 'b.pek' )
CompFiles( 'Test'+TestStr+'_B1_Moop.pek', 'c.pek' )
CompFiles( 'Test'+TestStr+'_YB_Fdw.pek' , 'd.pek' )

LinePlot( 'Test'+TestStr+'.azi', '.azi',
            WinTitle='Test03 - Azimuth Average', PlotTitle='Test03 - Azimuth Average', PlotFileName='Test03_AZI', FirstLine=11, NumPts=36,
            Xcol=1, Xtype='linear', Xlab='Azimuth, deg',
            Ycol=3, Ytype='linear', Ylab='Blade 1 In-Plane Moment, kN-m' )
LinePlot( 'Test'+TestStr+'.pmf', '.pmf',
            WinTitle='Test03 - Probability Density', PlotTitle='Crunch Test03', PlotFileName='Test03_PMF', FirstLine=12, NumPts=20,
            Xcol=3, Xtype='linear', Xlab='Blade 1 Out-of-Plane Moment, kN-m',
            Ycol=4, Ytype='linear', Ylab='Probability Density, 1/kN-m' )


#******************************************************
Test    = 4
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Test'+TestStr+'.rcc', '.rcc'  )


#******************************************************
Test    = 5
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.azi', 'a.azi' )
CompFiles( '14u001.pmf', 'a.pmf' )
CompFiles( '14u001.rcc', 'a.rcc' )
CompFiles( '14u001.sts', 'a.sts' )
CompFiles( '14u002.azi', 'b.azi' )
CompFiles( '14u002.pmf', 'b.pmf' )
CompFiles( '14u002.rcc', 'b.rcc' )
CompFiles( '14u002.sts', 'b.sts' )


#******************************************************
Test    = 6
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.rcc', '.rcc' )


#******************************************************
Test    = 7
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001_B1_Moop.rcc', '.rcc' )


#******************************************************
Test    = 8
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.mod', '.mod' )


#******************************************************
Test    = 9
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.eev', '.eev' )


#******************************************************
Test    = 10
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Aggregate.eev', '.eev' )


#******************************************************
Test    = 11
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Validate.azi'        , '.azi'  )
CompFiles( 'Validate.pmf'        , '.pmf'  )
CompFiles( 'Validate.rcc'        , '.rcc'  )
CompFiles( 'Validate.sts'        , '.sts'  )
CompFiles( 'Validate_1P_Sine.pek', 'a.pek' )
CompFiles( 'Validate_2P_Sine.pek', 'b.pek' )
CompFiles( 'Validate_3P_Sine.pek', 'c.pek' )
CompFiles( 'Validate_4P_Sine.pek', 'd.pek' )


#******************************************************
Test    = 12
TestStr = '%02d' % ( Test )

RunCase()

CompFiles(  'aV_hub.sum', 'a.sum' )
CompFiles( 'aFlapM1.sum', 'b.sum' )
CompFiles( 'aFlapM1.ext', '.ext'  )


#******************************************************
Test    = 13
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Test'+TestStr+           '.azi', '.azi'  )
CompFiles( 'Test'+TestStr+           '.pmf', '.pmf'  )
CompFiles( 'Test'+TestStr+           '.rcc', '.rcc'  )
CompFiles( 'Test'+TestStr+           '.sts', '.sts'  )
CompFiles( 'Test'+TestStr+'_aAzimuthde.pek', 'a.pek' )
CompFiles( 'Test'+TestStr+   '_aEdgeM1.pek', 'b.pek' )
CompFiles( 'Test'+TestStr+   '_aFlapM1.pek', 'c.pek' )
CompFiles( 'Test'+TestStr+   '_aYaw_FX.pek', 'd.pek' )


#******************************************************
Test    = 14
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Test'+TestStr+'.rcc', '.rcc'  )


#******************************************************
Test    = 15
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.azi', 'a.azi' )
CompFiles( '14u001.pmf', 'a.pmf' )
CompFiles( '14u001.rcc', 'a.rcc' )
CompFiles( '14u001.sts', 'a.sts' )
CompFiles( '14u002.azi', 'b.azi' )
CompFiles( '14u002.pmf', 'b.pmf' )
CompFiles( '14u002.rcc', 'b.rcc' )
CompFiles( '14u002.sts', 'b.sts' )


#******************************************************
Test    = 16
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.rcc', '.rcc' )


#******************************************************
Test    = 17
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001_aFlapM1.rcc', '.rcc' )


#******************************************************
Test    = 18
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.mod', '.mod' )


#******************************************************
Test    = 19
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( '14u001.eev', '.eev' )


#******************************************************
Test    = 20
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Aggregate.eev', '.eev' )


#******************************************************
Test    = 21
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Test'+TestStr+'.pmf', '.pmf'  )
CompFiles( 'Test'+TestStr+'.sts', '.sts'  )

LinePlot( 'Test'+TestStr+'.pmf', '.pmf',
            WinTitle='Test21 - Probability Density', PlotTitle='Crunch Test21', PlotFileName='Test21_PMF', FirstLine=10, NumPts=20,
            Xcol=1, Xtype='linear', Xlab='2*RootMxc1, kN-m',
            Ycol=2, Ytype='log'   , Ylab='Probability Density, 1/kN-m' )


#******************************************************
Test    = 22
TestStr = '%02d' % ( Test )

RunCase()

CompFiles( 'Test'+TestStr+'.pmf', '.pmf'  )
CompFiles( 'Test'+TestStr+'.sts', '.sts'  )


   # Close the file and view it.

CompareFileObj.close

raw_input( '\n   Press <Enter> to close the figures and exit.' )

os.system( Editor + ' ' + CompareFile + Redirect )


   # We be done.

print('\n Regression testing complete.\n')


