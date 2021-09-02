c*--------------------------------------------------------------------------
c*    lrss2rf.f
c*
c*
c*    REF/DIF 1 requires two principle input files for data if only the
c*    basic option of running an initially monochromatic, long-crested
c*    wave component is chosen.  The structure of these files is somewhat at
c*    odds with the requirements for input data file structure in LRSS.
c*    The present program is provided to carry out the necessary conversion.
c*
c*    This program takes the single namelist group input file provided by
c*    LRSS, together with the HDF formatted bathymetry file, and generates an 
c*    indat.dat file for REF/DIF 1.  This intermediate
c*    step is taken because REF/DIF 1 can generally be initialized in several
c*    different ways, only one of which (running a single monochromatic
c*    component with only wave height, period and direction specified at the 
c*    offshore boundary) is provided as an option for LRSS at this point.  The
c*    standard indat.dat file thus has several possible configurations of 
c*    namelist groups, only one of which will be utilized here.
c*
c*    It is assumed that: 
c*
c*    (1)  The dimensions of the bathymetry grid in the HDF input file
c*         are to be the same as the reference grid dimensions in the
c*         output ascii file refdat.dat and as specified in the output
c*         indat.dat. 
c*
c*    (2)  Aside from bathymetry, the user is specifying the wave period,
c*         wave angle, wave height and tidal offset relative to grid datum
c*         for a single long crested monochromatic wave.  Spectral 
c*         simulations are handled by the related program REF/DIF S.
c*
c*    (3)  The user tells REF/DIF 1 to generate the data for an image of
c*         the water surface at the computational resolution by setting
c*         the parameter isurface=1.  The user requests data on bottom 
c*         velocities at the bathymetry grid resolution by setting the
c*         parameter ibottom=1.
c*
c*    (3)  The program will attempt to determine its own computational 
c*         subgridding based on resolution requirements computed in this
c*         program.
c*
c*    (4)  The program will assume that:
c*
c*         - input data is in MKS units.
c*         - the lateral model boundaries will be open and transmitting.
c*         - the composite nonlinear model described in the user's manual
c*           is used.
c*         - frictional dissipation is ignored.
c*         - there will be no user specified subgridding.
c*         - wave-current interaction effects are being neglected.  
c*           (otherwise, the system will need to specify a u,v current
c*           field at the bathymetry grid resolution.)
c*
c*         Deviations from any of these assumptions will require the user
c*         to construct the desired datafileout (=indat.dat) data file 
c*         following the instructions in the user's manual.
c*
c*--------------------------------------------------------------------------
c*
c*    Running the program.
c*
c*    The program is to be run with a command line file specifications:
c*
c*    lrss2rf datafilein 
c*
c*    Runtime messages are stored in lrss2rf.log.  REF/DIF 1 is then
c*    started using the command line:
c*
c*    refdif1 filein
c*
c*    where 'filein' is a namelist file containing the file name 'datafileout'
c*    in the namelist group 'lrss_name'. (See the code fragment 'infile2.f').  
c*
c*    'gridfilein' is the name of the HDF bathymetry file.
c*
c*    'datafilein' is the name of the single namelist group input data file.  
c*    A sample of this file follows.
c*
c*--------------------------------------------------------------------------
c*
c*    Sample data file:
c*
c* $input
c* height=1.
c* period=10.
c* angle=0.
c* tideoffset=0.
c* gridfilein='camppend.hdf'
c* datafileout='indat.dat'
c* logfile='lrss2rf.log'
c* fname1='refdat.dat'
c* fname2='outdat.dat'
c* fname3='subdat.dat'
c* fname6='surface.dat'
c* fname7='bottomu.dat'
c* fname8='angle.dat'
c* fname9=' '
c* fname10='refdif1.log'
c* fname11='height.dat'
c* fname12=' '
c* fname13=' '
c* fname14=' '
c* fname15='depth.dat'
c* $end
c*
c*    The standard names for REF/DIF 1 input files would correspond to:
c*
c*    datafileout = indat.dat
c*    fname1= refdat.dat
c*
c*    but any arbitrary file name may be specified.  Runtime messages are
c*    stored in lrss2rf.log.
c*
c*
c*--------------------------------------------------------------------------
c*
c*
c*    James T. Kirby
c*    Center for Applied Coastal Research
c*    University of Delaware
c*    Newark, DE 19716
c*
c*    (302) 831-2438, FAX (302) 831-1228, kirby@coastal.udel.edu
c*
c*    Last revision 12/21/94.
c*
c*---------------------------------------------------------------------------

      program lrss2rf

      include 'param.h'

      common/indat/ height,period,angle,tideoffset, mr, nr, dxr, dyr
      common/refdat/ bath,nd,dt
      common/fname_s/fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15

c  Information about hdf file.

      integer i,j,k,m,n,iswap,n3,n4
      integer iret, iwrite, iout, error_write, error_exit
      external itegarg
      real bathflip(ixr,iyr),bath(ixr,iyr),hdfarray(ixr,iyr)
      real x(ixr),y(iyr),dx,dy
      character*255 infile, fileout
      character*255 coordsys
      character*255 labelx,labely,labelf,unitx,unity,unitf
      character*255 formatx,formaty,formatf,filein
      character*255 datafilein, datafileout, gridfilein, logfile
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15
      integer idimsizes(2)

c*----------------------------------------------------------------------------
c*   First, set up the ability to read the input file and output file
c*   names from the command line.
c*----------------------------------------------------------------------------

      integer igetarg
      external igetarg

c*------------------------------------------------------------------------
c*    Namelist structure for datafilein
c*------------------------------------------------------------------------

      namelist/input/height,period,angle,tideoffset,isurface,ibottom,
     1               gridfilein,datafileout,logfile,
     1              fname1,fname2,fname3,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15

c*------------------------------------------------------------------------
c*   add this call to work around SGI bug (not needed if program being
c*   used as a free-standing program).
c*------------------------------------------------------------------------

c      call lrss_setup

c*------------------------------------------------------------------------
c*   read the filename, check that we got it.
c*------------------------------------------------------------------------

      iret=igetarg(1,datafilein, 255)

      if(iret1.eq.-1) then
        iwrite = error_write
     1       ('no filename for datafilein specified on command line')
        call exit(1)
      endif

c*-----------------------------------------------------------------------
c*    Enter the namelist data from datafilein
c*-----------------------------------------------------------------------

      open(5,file=datafilein)
      read(5,nml=input)
      close(5)

      open(10,file=logfile)
c*----------------------------------------------------------------------
c*   First, we will read in information about the grid from the LRSS
c*   HDF formatted file (specified by gridfilein) and generate the ASCII
c*   file refdat.dat usually used by REF/DIF 1.  It is assumed that the
c*   REF/DIF 1 parameters ixr and iyr are set large enough (in param.h)
c*   to contain the bathymetry grid being read in.
c*----------------------------------------------------------------------

      write(10,*) ' begin r_hdf'

      call r_hdf(gridfilein,bath,ixr,iyr,mr,nr,x,y,hdfarray,
     $      labelx,formatx,unitx,labely,formaty,unity,
     $      labelf,formatf,unitf,coordsys,iswap)

      write(10,*) ' end of r_hdf'

c*-----------------------------------------------------------------------
c*    Dimensions mr and nr of actual grid
c*-----------------------------------------------------------------------

      write(10,*) 'mr=',mr
      write(10,*) 'nr=',nr

      dxr=abs(x(2)-x(1))
      dyr=abs(y(1)-y(2))

c*-----------------------------------------------------------------------
c*    Generate the (fname1=refdat.dat) data file. 
c*-----------------------------------------------------------------------

      open(8,file=fname1)
      do 10 i=1,mr
      write(8,20) (bath(i,j),j=nr,1,-1)
 10   continue
      close(8)
 20   format(20f10.4)

c*-----------------------------------------------------------------------
c*    Now we must inspect the depth grid to get an estimate of what the
c*    level of automatic grid subdivision should be.  This is done as in
c*    REF/DIF 1 itself.
c*-----------------------------------------------------------------------

      call con

c*-----------------------------------------------------------------------
c*    Now we need to generate the namelist file (usually indat.dat) for 
c*    REF/DIF 1.
c*-----------------------------------------------------------------------

      call indatgen(datafileout)

c*-----------------------------------------------------------------------
c*    All done.
c*-----------------------------------------------------------------------

      call exit(0)
      end

c*-----------------------------------------------------------------------
c*    indatgen
c*
c*    Generate the ascii namelist file indat.dat for input into REF/DIF 1
c*
c*-----------------------------------------------------------------------

      subroutine indatgen(datafileout)

      include 'param.h'

c*-----------------------------------------------------------------------
c*    Information from main program.
c*-----------------------------------------------------------------------

      common/indat/height,period,angle,tideoffset,mr,nr,dxr,dyr
      common/refdat/bath,nd,dt
      common/fname_s/fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15

      character*255 datafileout

c*-----------------------------------------------------------------------
c*    information for datafileout ( =indat.dat).
c*-----------------------------------------------------------------------

      dimension iff(3)
      dimension bath(ixr,iyr)
      dimension freqs(ncomp), nwavs(ncomp)
      dimension amp(ncomp,ncomp), dir(ncomp,ncomp), tide(ncomp)

      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15

c*----------------------------------------------------------------------
c*   The following files correspond to a portion of REF/DIF 1 which is 
c*   never used by LRSS, but are required for input into REF/DIF 1.
c*   Names are specified here in order to avoid having the system or user
c*   invent an arbitrary name for a non-existant file.
c*-----------------------------------------------------------------------

      data fname4/'fname4.dat'/,fname5/'fname5.dat'/


      namelist /ingrid/ mr, nr, iu, ntype, icur, ibc, dxr, dyr, dt, 
     1                  ispace, nd, iff, isp, iinput, ioutput
     1         /fnames/ fname1,fname2,fname3,fname4,fname5,fname6,
     1                  fname7,fname8,fname9,fname10,fname11,fname12,
     1                  fname13,fname14,fname15
     1         /waves1a/ iwave, nfreqs
     1         /waves1b/ freqs, tide, nwavs, amp, dir

c*-----------------------------------------------------------------------
c*    Define any variables not yet specified
c*-----------------------------------------------------------------------

      iu=1
      ntype=1
      icur=0
      ibc=1
      ispace=0
      iff(1)=0
      iff(2)=0
      iff(3)=0
      isp=0
      iinput=1
      ioutput=1

      iwave=1
      nfreqs=1
      freqs(1)=period
      tide(1)=tideoffset
      nwavs(1)=1
      amp(1,1)=height/2.
      dir(1,1)=angle
      

c*-----------------------------------------------------------------------
c*    Open the data file and write the namelists.
c*-----------------------------------------------------------------------
  
      open(7,file=datafileout)

      write(7,nml=fnames)
      write(7,nml=ingrid)
      write(7,nml=waves1a)
      write(7,nml=waves1b)

      close(7)

c*-----------------------------------------------------------------------
c*    datafileout (=indat.dat) all done.
c*-----------------------------------------------------------------------

      return
      end

c*-----------------------------------------------------------------------
c*    con
c*
c*    Compute wavenumbers and number of waves per grid space in order to
c*    estimate the required level of subdivision.
c*
c*-----------------------------------------------------------------------

      subroutine con

      include 'param.h'

      common/indat/ height,period,angle,tideoffset, mr, nr, dxr, dyr
      common/refdat/ bath,nd,dt

      dimension bath(ixr,iyr),md(ixr)
      real k(ixr,iyr),kb(ixr)

      mdmax=1
      pi=3.1415927
      eps=1.0e-05
      omega=2.*pi/period

c*-----------------------------------------------------------------------
c*    Compute x-direction subdivisions just as in REF/DIF 1
c*-----------------------------------------------------------------------

      depthmin=0.
      depthmax=0.

      do 100 i=1,mr-1

      npts=0
      sumk=0.
  
      do 13 j=1,nr
      dref=bath(i,j)+tideoffset
      if(dref.gt.depthmax)depthmax=dref
      if(dref.lt.0.001) dref=0.001
      call wvnum(dref,0.0,omega,k(i,j),eps,icdw,i,j)
      if(dref.gt.0.05) then
        sumk=sumk+k(i,j)
        npts=npts+1
      endif
 13   continue
      if(npts.eq.0)then
      kb(i)=k(i,1)
      else
      kb(i)=sumk/float(npts)
      endif
      alw=2.*pi/kb(i)
      anw=dxr/alw
      np=ifix(5.*anw)
      if(np.lt.1) np=1

      if(npts.gt.0) then
      md(i)=min((ix-1),np)
      if(np.gt.(ix-1)) write(10,200) i
      else
      md(i)=md(i-1)
      endif

 100  continue

      dt=depthmax

c*----------------------------------------------------------------------
c*    Find the biggest one, compute the corresponding dx and dy, and
c*    compute the resulting nd.
c*----------------------------------------------------------------------

      do 101 i=1,mr-1
      if(md(i).gt.mdmax) mdmax=md(i)
 101  continue

      write(10,*) ' md(ir) max = ', mdmax

      dx=dxr/float(mdmax)

      write(10,*) ' dxmin = ', dx
      dy=dx
      nd=ifix(dyr/dy)
    
      return

 200  format(' model tried to put more spaces than allowed in',
     1' grid block ',i3)

      end 

c*-----------------------------------------------------------------------
c*   wvnum
c*
c*   Compute wavenumbers.
c*
c*-----------------------------------------------------------------------     

      subroutine wvnum(d,u,s,k,eps,icdw,i,j)  

      include 'param.h'

      real k,kn

c  Constants.

      g=9.806
      pi=3.1415927
      k=s*s/(g*sqrt(tanh(s*s*d/g)))

c  Newton-Raphson iteration.

      do 1 ii=1,20
      f=s*s-2.*s*k*u+k*k*u*u-g*k*tanh(k*d)
      fp=-2.*s*u+2.*k*u*u-g*tanh(k*d)-g*k*d/(cosh(k*d)**2.)
      kn=k-f/fp
      if((abs(kn-k)/kn).lt.eps)go to 2
      k=kn
 1    continue
      t=2.*pi/(sqrt(g*k*tanh(k*d))+k*u)
      write(10,100)i,j,k,u,d,f,t
      icdw=1

      return

 2    k=kn

      return

 100  format(' wavenumber iter. failed to converge on row',i10,
     1'  column',i10/
     1'      k=',f15.8,'      u=',f15.8/
     1'      d=',f15.8,'      f=',f15.8/
     1'      t=',f15.8)

      end
