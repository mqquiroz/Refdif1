c*--------------------------------------------------------------------------
c*
c*    surface.f
c*
c*    This program converts the file (usually surface.dat) containing
c*    an instantaneous snapshot of the water surface at the computational
c*    grid spacing to a regularly spaced ascii file, suitable for directly
c*    reading into Matlab format.
c*
c*    James T. Kirby
c*    Center for Applied Coastal Research
c*    University of Delaware
c*    Newark, DE 19716
c*    kirby@coastal.udel.edu, (302) 831-2438, FAX (302) 831-1228
c*
c*    Last revision 12/22/94. 
c*
c*---------------------------------------------------------------------------

      program surf

      include 'param.h'

      integer i,j,k,m,n,nx,ny,iswap
      integer iret, iout 
      real surface(iy,iy)
      real x(iy),y(iy),dx,dy,xold(iy),surfold(iy,iy)
      character*255 fileout
      integer idimsizes(2)

      character*255 fname1,fname2,fname3,fname4,fname5,fname6,
     1              fname7,fname8,fname9,fname10,fname11,fname12,
     1              fname13,fname14,fname15,fnamein

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

      open(8,file='indat.dat')
      read(8,nml=fnames)

      open(10,file=fname6)

c  Enter output file name.

      write(*,*) 'enter output file name in single quotes'
      read(*,*) fileout

c  Read number of y-direction points from surface.dat

      read(10,*) ny
      read(10,*) (y(j),j=1,ny)

      write(*,*) ' number of y points = ', ny
      write(*,*) ' maximum y = ', y(ny)

c  Read surface data.

      do 10 i=1,100000

      read(10,*) xold(i)

      if (xold(i).lt.0) go to 20

      read(10,*) (surfold(i,j),j=1,ny)

 10   continue

 20   continue

      m=i-1

      write(*,*) ' number of x points in file = ', m
      write(*,*) ' maximum x = ', xold(m)

      dy=y(2)-y(1)
      dx=dy

      write(*,*) ' grid spacing (x and y) in new image = ', dy

      nx=int(xold(m)/dx)+1

      write(*,*) ' number of x points in interpolated image = ', nx

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
      surface(nx,j)=surfold(m,j)
 45   continue

      open(11,file=fileout)

      do 50 i=1,nx
      write(11,51)(surface(i,j),j=1,ny)
 50   continue

      close(11)

      stop

 51   format(500(f10.4))

      end

