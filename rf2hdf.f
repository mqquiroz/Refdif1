c*--------------------------------------------------------------------------
c*
c*    rf2hdf.f
c*
c*    This program converts selected refdif1 files at the reference
c*    grid spacing to hdf format, for use by other LRSS algorithms and
c*    by the Matlab Image Processing Toolbox.
c*
c*    James T. Kirby
c*    Center for Applied Coastal Research
c*    University of Delaware
c*    Newark, DE 19716
c*
c*    kirby@coastal.udel.edu, (302) 831-2438, FAX (302) 831-1228.
c*
c*    Last revision 12/22/94. 
c*
c*---------------------------------------------------------------------------

      program rf2hdf

      include 'param.h'

      dimension iff(3)

      common/surf/surface(iy,iy)

      integer nx,ny, iwrite
      integer iret, igetarg, error_write
      external itegarg
      real out_array(ixr,iyr),hdfarray(ixr,iyr)
      real x(ixr),y(iyr),dxr,dyr

      character*255 fname1,fname2,fname3,fname4,fname5,fname6,
     1              fname7,fname8,fname9,fname10,fname11,fname12,
     1              fname13,fname14,fname15,fnamein
      character*255 coordsys
      integer iswap

      namelist /ingrid/ mr, nr, iu, ntype, icur, ibc, dxr, dyr, dt, 
     1                  ispace, nd, iff, isp, iinput, ioutput
     1         /inmd/ md
     1         /fnames/ fname1,fname2,fname3,fname4,fname5,fname6,
     1                  fname7,fname8,fname9,fname10,fname11,fname12,
     1                  fname13,fname14,fname15
     1         /waves1a/ iwave, nfreqs
     1         /waves1b/ freqs, tide, nwavs, amp, dir
     1         /waves1c/ thet0, freqs, tide, edens, nwavs, nseed    
     1         /waves2/ freqin, tidein

      open(6,file='rf2hdf.log')


      call infile(fnamein)

      open(8,file=fnamein)

      read(8,nml=fnames)
      read(8,nml=ingrid)

c  Compute grid spacing data.

      do 1 i=1,mr
      x(i)=float(i-1)*dxr
 1    continue

      do 2 j=1,nr
      y(j)=float(j-1)*dyr
 2    continue

c*-----------------------------------------------------------------------
c    values used for labels, formats, units are your choice
c    coordsys is usually just null string = ''
c    iswap is usually 0
c*-----------------------------------------------------------------------
      coordsys=' '
      iswap=0

c*-----------------------------------------------------------------------
c   if output file names are not null strings, then 
c   write out data in hdf format
c*-----------------------------------------------------------------------
c   angle file output
c*-----------------------------------------------------------------------

      if (fname8.ne.' ') then
        write(6,*) 'in angleout w_hdf'
	write(6,*) 'iyr,mr,nr=',iyr,mr,nr	
        open(10,file=fname8)
        do 10 i=1,mr
        read(10,*) (out_array(i,j),j=nr,1,-1)
 10     continue
        close(10)
        print*, 'angle=',out_array(1,1),out_array(100,100),
     1           out_array(mr,nr)


        call w_hdf(fname8,out_array,ixr,iyr,mr,nr,x,y,hdfarray,
     $    'x axis','f8.1','meters','y axis','f8.1','meters',
     $    'reference_grid_angles','e10.3','degrees',coordsys,iswap)

      endif

c*-----------------------------------------------------------------------
c   height file output
c*----------------------------------------------------------------------

      if (fname11.ne.' ') then
	
        open(10,file=fname11)

        do 20 i=1,mr
        read(10,*) (out_array(i,j),j=nr,1,-1)
 20     continue
        close(10)
        write(6,*) 'in height w_hdf'
	write(6,*) 'iyr,mr,nr=',iyr,mr,nr	
        write(6,*) 'height=',out_array(1,1),out_array(100,100),
     1           out_array(mr,nr)

        call w_hdf(fname11,out_array,ixr,iyr,mr,nr,x,y,hdfarray,
     $    'x axis','f8.1','meters','y axis','f8.1','meters',
     $    'wave height','e10.3','meters',coordsys,iswap)

      endif

c*----------------------------------------------------------------------
c  Read radiation stress data.
c*----------------------------------------------------------------------

      if (fname12.ne.' '.and.fname13.ne.' '.and.fname14.ne.' ') then
        write(6,*) 'in stress out'
        open(10,file=fname12)

        do 30 i=1,mr
        read(10,*) (out_array(i,j),j=nr,1,-1)
 30     continue
        close(10)

        call w_hdf(fname12,out_array,ixr,iyr,mr,nr,x,y,hdfarray,
     $    'x axis','f8.1','meters','y axis','f8.1','meters',
     $    'radiation stress sxx','e10.3','kg/sec^2',coordsys,iswap)

        open(10,file=fname13)
        do 40 i=1,mr
        read(10,*) (out_array(i,j),j=nr,1,-1)
 40     continue
        close(10)

        call w_hdf(fname13,out_array,ixr,iyr,mr,nr,x,y,hdfarray,
     $    'x axis','f8.1','meters','y axis','f8.1','meters',
     $    'radiation stress sxy','e10.3','kg/sec^2',coordsys,iswap)

        open(10,file=fname14)
        do 50 i=1,mr
        read(10,*) (out_array(i,j),j=nr,1,-1)
 50     continue
        close(10)

        call w_hdf(fname14,out_array,ixr,iyr,mr,nr,x,y,hdfarray,
     $    'x axis','f8.1','meters','y axis','f8.1','meters',
     $    'radiation stress syy','e10.3','kg/sec^2',coordsys,iswap)
       endif

c*-------------------------------------------------------------------------
c*   surface data.
c*-------------------------------------------------------------------------

      if (fname6.ne.' ') then
	write(6,*) 'in surface out'
	call surface2hdf(fname6,nx,ny,logfileout)
        call w_hdf(fname6,surface,ixr,iyr,nx,ny,x,y,hdfarray,
     $    'x axis','f8.1','meters','y axis','f8.1','meters',
     $    'surface_image','e10.3','kg/sec^2',coordsys,iswap)
      endif
      call exit(0)
      end

c*------------------------------------------------------------------------
c*    surface
c*
c*   Interpolate the surface image onto a regular grid.
c*------------------------------------------------------------------------

      subroutine surface2hdf(fname6,nx,ny,logfileout)

      include 'param.h'

      common/surf/surface(iy,iy)

      integer i,j,m,nx,ny
      real x(iy),y(iy),dx,dy,xold(iy),surfold(iy,iy)

      character*255 fname6, logfileout

      open(10,file=fname6)

c  Enter output file name.

c  Read number of y-direction points from surface.dat

      read(10,*) ny
      read(10,*) (y(j),j=1,ny)

      write(6,*) ' number of y points = ', ny
      write(6,*) ' maximum y = ', y(ny)

c  Read surface data.

      do 10 i=1,100000

      read(10,*) xold(i)

      if (xold(i).lt.0) go to 20

      read(10,*) (surfold(i,j),j=1,ny)

 10   continue

 20   continue
      close(10)
      m=i-1

      write(6,*) ' number of x points in file = ', m
      write(6,*) ' maximum x = ', xold(m)

      dy=y(2)-y(1)
      dx=dy

      write(6,*) ' grid spacing (x and y) in new image = ', dy

      nx=int(xold(m)/dx)+1

      write(6,*) ' number of x points in interpolated image = ', nx

      do 25 j=1,ny
      jj=(ny-j)+1
      surface(1,j)=surfold(1,jj)
 25   continue

      x(1)=0.

      do 40 i=2,nx-1
      x(i)=float(i-1)*dx
      do 35 ii=1,m-1
      if((xold(ii).le.x(i)).and.(xold(ii+1).gt.x(i))) then
          fac=(x(i)-xold(ii))/(xold(ii+1)-xold(ii))
          do 30 j=1,ny
          jj=(ny-j)+1
          surface(i,j)=(1.-fac)*surfold(ii,jj)+fac*surfold(ii+1,jj)
 30       continue
      endif
 35   continue
 40   continue

      do 45 j=1,ny
      jj=(ny-j)+1
      surface(nx,j)=surfold(m,jj)
 45   continue

       close(11)
       return
       end

