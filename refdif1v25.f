
C FTANGLE v1.30, created with UNIX on "Thursday, June 17, 1993 at 10:46."
C COMMAND LINE: "ftangle -v refdif1v25"
C RUN TIME: "Thursday, April 13, 1995 at 10:13."
C WEB FILE:    "refdif1v25.web"
C CHANGE FILE: (none)
C* 1: *
*line 120 "refdif1v25.web"

      program refdif1
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 126 "refdif1v25.web"

C  Constants which provide for conversion between MKS and English units       on
C input and output.
      dconv(1)=1.
      dconv(2)=0.30488

C  read control parameters and reference grid data
      call inref

C  read control parameters and initializing wave data
      call inwave

C   Pass program control to subroutine |model|.

C   For each frequency component specified in |inwave|, |model|        executes
Cthe model throughout the entire grid and then          reinitializes the model
Cfor the next frequency.
      call model

C  All done.  Close output data files if |open| and |close| statements are
C  being used.

C |outdat|.
      close(iun(3))

C  |surface|.
      if(fname6.NE.'')close(8)

C  |angle|.
      close(9)

C  |refdif1.log|.
      close(10)

C  |height|.
      close(12)

C  |sxx|.
      if(fname12.NE.' ')close(13)

C  |sxy|.
      if(fname13.NE.' ')close(14)

C  |syy|.
      if(fname14.NE.' ')close(15)

C  |depth|.
      close(16)

C  |bottomu|.
      if(fname7.NE.' ')close(17)

C  |owave|.
      if(ioutput.EQ.2)close(33)
      stop
      end
C* :1 *
C* 2: *
*line 307 "refdif1v25.web"
      subroutine inref
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 313 "refdif1v25.web"

C  Standard file name choices:

C  |fname1| = |refdat.dat|, reference grid data file.

C  |fname2| = |outdat.dat|, standard output data file.

C  |fname3| = |subdat.dat|, user-specified subgrids.

C  |fname4| = |wave.dat|, user-specified complex amplitude on row 1 (for |iinput
C|       =2).

C  |fname5| = |owave.dat|, complex amplitude on last row (for |ioutput| = 2).

C  |fname6| = |surface.dat|, instantaneous water surface at computational
Cresolution.

C  |fname7| = |bottomu.dat|, magnitude of bottom velocity at reference grid
C  points.

C  |fname8| = |angle.dat|, wave directions at reference grid points.

C  |fname9| = not used yet.

C  |fname10| = |refdif1.log|, run log for |refdif1| program.

C  |fname11| = |height.dat|, wave heights at reference grid locations.

C  |fname12| = |sxx.dat|, Sxx components at reference grid locations.

C  |fname13| = |sxy.dat|, Sxy components at reference grid locations.

C  |fname14| = |syy.dat|, Syy components at reference grid locations.

C  |fname15| = |depth.dat|, tide-corrected depths at reference grid locations.
C

C  |fnamein| = |indat.dat|, input namelist file.
      namelist/ingrid/mr,nr,iu,ntype,icur,ibc,dxr,dyr,dt,ispace,nd,iff,i
     &sp,iinput,ioutput/inmd/md/fnames/fname1,fname2,fname3,fname4,fname
     &5,fname6,fname7,fname8,fname9,fname10,fname11,fname12,fname13,fnam
     &e14,fname15

C  Constants.
      g=9.80621
C* :2 *
C* 3: *
*line 403 "refdif1v25.web"

      call infile(fnamein)
      iun(5)=5
      open(unit=iun(5),file=fnamein,status='old')
C* :3 *
C* 4: *
*line 412 "refdif1v25.web"


      iun(1)=1
      iun(2)=2
      iun(3)=3
      read(iun(5),nml=fnames)
      open(unit=iun(1),file=fname1,status='old')
      open(unit=iun(3),file=fname2)
      open(9,file=fname8)
      open(10,file=fname10)
      open(12,file=fname11)
      if(fname12.NE.' '.AND.fname13.NE.' '.AND.fname14.NE.' ')then
      open(13,file=fname12)
      open(14,file=fname13)
      open(15,file=fname14)
      endif
      open(16,file=fname15)
      if(fname7.NE.' ')open(17,file=fname7)
      if(fname6.NE.' ')open(8,file=fname6)

C  print headers on output
      write(10,120)
      write(10,106)

C  Read control data from unit |iun(5)|.
      read(iun(5),nml=ingrid)
      if(ispace.EQ.1)read(iun(5),nml=inmd)
      write(10,107)mr,nr,dxr,dyr
      if(iu.EQ.1)write(10,114)iu
      if(iu.EQ.2)write(10,115)iu
      if(icur.EQ.0)write(10,200)
      if(icur.EQ.1)write(10,201)
      if(ibc.EQ.0)write(10,202)
      if(ibc.EQ.1)write(10,203)
      if(ispace.EQ.0)write(10,108)
      if(ispace.EQ.1)write(10,109)
      write(10,119)nd
      if(ntype.EQ.0)write(10,110)
      if(ntype.EQ.1)write(10,111)
      if(ntype.EQ.2)write(10,112)

C  Check input from unit |iun(5)|.
      if((mr.GT.ixr).OR.(nr.GT.iyr))then
      write(10,*)'dimensions for reference grid too large, stopping'
      call exit(1)
      end if
      if((iu.NE.1).AND.(iu.NE.2))iu=1
      dt=dt*dconv(iu)
      dxr=dxr*dconv(iu)
      dyr=dyr*dconv(iu)
      if(dt.EQ.0.)dt=2.
      if(nd.GT.ifix(float(iy-1)/float(nr-1)))then
      write(10,102)
      call exit(1)
      endif
      if(ispace.EQ.1)then
      test=0.
      do 1 i=1,mr-1
      if(md(i).GT.(ix-1))then
      write(10,103)i
      test=1.
      endif
1     continue
      if(test.EQ.1.)call exit(1)
      endif

C     read depth grid and velocities from unit iun(1)
      do 2 i=1,mr
      read(iun(1),101)(dr(i,j),j=1,nr)
2     continue
      if(icur.EQ.1)then
      do 3 i=1,mr
      read(iun(1),101)(ur(i,j),j=1,nr)
3     continue
      do 4 i=1,mr
      read(iun(1),101)(vr(i,j),j=1,nr)
4     continue
      endif

C     convert depth and currents
      do 5 i=1,mr
      do 5 j=1,nr
      dr(i,j)=dr(i,j)*dconv(iu)
5     continue
      if(icur.EQ.1)then
      do 55 i=1,mr
      do 55 j=1,nr
      ur(i,j)=ur(i,j)*dconv(iu)
      vr(i,j)=vr(i,j)*dconv(iu)
55    continue
      endif

C     check for large depth changes and large currents in reference grid data.
C
      do 6 i=2,mr-1
      do 6 j=2,nr-1
      dcheck=(dr(i+1,j)+dr(i-1,j)+dr(i,j-1)+dr(i,j+1))/4.
      if(abs(dcheck-dr(i,j)).GT.dt)write(10,104)dr(i,j),i,j,dt
6     continue
      if(icur.EQ.1)then
      do 7 i=1,mr
      do 7 j=1,nr
      if(dr(i,j).LE.0.0)go to 7
      fr=(ur(i,j)*ur(i,j)+vr(i,j)*vr(i,j))/(g*dr(i,j))
      if(fr.GT.1.)write(10,105)i,j,fr
7     continue
      endif

C  Establish coordinates for reference grid.
      do 8 ir=1,mr
      xr(ir)=float(ir-1)*dxr
8     continue
      do 9 jr=1,nr
      yr(jr)=float(jr-1)*dyr
9     continue

C  Establish |y| coordinates for interpolated grid.
      n=nd*(nr-1)+1
      dy=dyr/float(nd)
      do 10 j=1,n
      y(j)=float(j-1)*dy
10    continue

C  Write grid information on output unit |iun(3)|.
      write(iun(3),*)nr,mr
      write(iun(3),*)(yr(jr)/dconv(iu),jr=1,nr)

C  Check friction values.

C         |iff(1)=1|, turbulent boundary layer damping everywhere

C         |iff(2)=1|, porous bottom damping everywhere

C         |iff(3)=1|, laminar boundary layer damping everywhere
      do 11 i=1,3
      if((iff(i).NE.0).AND.(iff(i).NE.1))iff(i)=0
11    continue
      write(10,116)(iff(i),i=1,3)

C   Specify whether or not user specified subgrids are to be read in during
C  model operation.

C     |isp=0|, no subgrids specified

C     |isp=1|, subgrids to be read in later from unit |iun(2)|
      if(isp.EQ.0)write(10,117)
      if(isp.EQ.1)then
      write(10,118)
      open(unit=iun(2),file=fname3,status='old')
      endif
      if((isp.EQ.1).AND.(ispace.EQ.0))write(10,113)
      if(isp.EQ.0)then
      do 14 ir=1,mr
      do 14 jr=1,nr
      isd(ir,jr)=0
14    continue
      else
      do 15 ir=1,mr-1
      read(iun(2),100)(isd(ir,jr),jr=1,nr-1)
15    continue
      endif

C  Input done, return to main program.
      return
100   format(15i4)
101   format(20f10.4)
102   format(' y-direction subdivision too fine.'/' maximum number of y
     &grid points will be exceeded.'/' execution terminating.')
103   format(' x-direction subdivision too fine on grid block',2x,i3/' e
     &xecution terminating')
104   format(' depth',2x,f7.2,'(m) at reference grid location',2(2x,i3)/
     &' differs from the average of its neighbors by',' more than',2x,f7
     &.2,'(m).'/' execution continuing')
105   format(' ambient current at reference grid location',2(2x,i3),' is
     & supercritical with froude number =',f7.4/' execution continuing')
106   format('0'///20x,'input section, reference grid values'///)
107   format(' reference grid dimensions  mr=',i3/'
     &       nr=',i3///' reference grid spacings   dxr=',f8.4/'
     &                  dyr=',f8.4)
108   format(' '/' ispace =0 chosen, program will attempt its own ','ref
     &erence grid subdivisions')
109   format(' '/' ispace =1 chosen, subdivision spacings will be',' inp
     &ut as data')
110   format(' '/' ntype = 0, linear model')
111   format(' '/' ntype = 1, stokes model matched to hedges model')
112   format(' '/' ntype = 2, stokes model')
113   format(' warning: input specifies that user will be supplying',' s
     &pecified subgrids (isp=1),'/' while program has been told to gener
     &ate its own subgrid',' spacings (ispace=0).'/' possible incompatib
     &ility in any or all subgrid blocks')
114   format(' '/' physical unit switch iu=',i1,',  input in mks units')
115   format(' '/' physical unit switch iu=',i1,',  input in english uni
     &ts')
116   format(' '//'   switches for dissipation terms'//' ',i1,'   turbul
     &ent boundary layer'/' ',i1,'   porous bottom'/' ',i1,'   laminar b
     &oundary layer')
120   format(//////20x,'Refraction-Diffraction Model for'/20x,'Weakly No
     &nlinear Surface Water Waves'///20x,'REF/DIF 1,  Version 2.5'///20x
     &,'Center for Applied Coastal Research'/20x,'Department of Civil En
     &gineering'/20x,'University of Delaware'/20x,'Newark, Delaware 1971
     &6'///10x,'James T. Kirby and Robert A. Dalrymple, November 1994')
117   format(' '/' isp=0, no user defined subgrids')
118   format(' '/' isp=1, user defined subgrids to be read')
119   format(' '/' y-direction subdivision according to nd=',i3)
200   format(' '/' icur=0, no current values read from input files')
201   format(' '/' icur=1, current values read from data files')
202   format(' '/' ibc=0, closed (reflective) lateral boundaries')
203   format(' '/' ibc=1, open lateral boundaries')
      end
C* :4 *
C* 5: *
*line 756 "refdif1v25.web"

      subroutine inwave
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 762 "refdif1v25.web"

      namelist/waves1a/iwave,nfreqs/waves1b/freqs,tide,nwavs,amp,dir/wav
     &es1c/thet0,freqs,tide,edens,nwavs,nseed/waves2/freqin,tidein
      pi=3.1415927

C  Values of |iinput|, |ioutput| already entered in namelist statement in
C|inref|.
      if((iinput.NE.1).AND.(iinput.NE.2))then
      write(10,*)' invalid value chosen for iinput, check indat.dat'
      stop
      endif
      if((ioutput.NE.1).AND.(ioutput.NE.2))then
      write(10,*)' invalid value chosen for ioutput, check indat.dat'
      stop
      endif
      if(ioutput.EQ.2)then
      open(33,file=fname5)
      endif
      if(iinput.EQ.1)then
      write(10,*)' iinput = 1, program specifies initial row of a'

C  Enter |iwave|, |nfreqs| for |iinput = 1|.
      read(iun(5),nml=waves1a)
      write(10,102)

C  Enter data for case of |iinput=1, iwave=1|.
      if(iwave.EQ.1)then
      read(iun(5),nml=waves1b)
      write(10,103)
      endif

C  Enter data for case of |iinput=1, iwave=2|.
      if(iwave.EQ.2)then
      read(iun(5),nml=waves1c)
      write(10,104)
      endif
      write(10,105)nfreqs
      if(iwave.EQ.2)then
      thet0=thet0*pi/180.
      endif

C  For each frequency, enter the wave period and tidal offset.
      do 3 ifreq=1,nfreqs
      write(10,107)ifreq,freqs(ifreq),tide(ifreq)

C  Convert angles to radians.
      freqs(ifreq)=2.*pi/freqs(ifreq)
      tide(ifreq)=tide(ifreq)*dconv(iu)

C  If |iwave = 1|, read the number of discrete components.
      if(iwave.EQ.1)then
      do 1 iwavs=1,nwavs(ifreq)
      write(10,106)iwavs,amp(ifreq,iwavs),dir(ifreq,iwavs)
      dir(ifreq,iwavs)=dir(ifreq,iwavs)*pi/180.
      amp(ifreq,iwavs)=amp(ifreq,iwavs)*dconv(iu)
1     continue
      endif

C  If |iwave = 2|, read the parameters for each frequency.
      if(iwave.EQ.2)then
      seed=float(nseed)/9999.
      write(10,108)edens(ifreq),nwavs(ifreq),nseed
      dir(ifreq,1)=thet0
      edens(ifreq)=edens(ifreq)*(dconv(iu)**2.)
      endif
3     continue
      endif

C  If |iinput = 2|, read in wave period and tidal offset.
      if(iinput.EQ.2)then
      read(iun(5),nml=waves2)
      freqs(1)=freqin
      tide(1)=tidein
      write(10,*)' iinput = 2, user specifies a in wave.dat'
      nfreqs=1
      write(10,102)
      write(10,*)' wave period =',freqs(1),' sec.'
      write(10,*)' tidal offset=',tide(1)
      freqs(1)=2.*pi/freqs(1)
      tide(1)=tide(1)*dconv(iu)
      endif
      return
100   format(15i4)
101   format(20f10.4)
102   format('1'///20x,' input section, wave data values'///)
103   format(' '///' iwave=1, discrete wave amps and directions')
104   format(' '///' iwave=2, directional spreading model chosen')
105   format(' '///' the model is to be run for',i3,' separate',' freque
     &ncy components')
106   format(' '/' wave component ',i2,', amplitude =',f8.4,', direction
     &=',f8.4)
107   format(' '//' frequency component ',i2//' wave period=',f8.4,'sec.
     &, tidal offset=',f8.4)
108   format(' '/' total variance density =',f8.4,', spreading factor
     &       n=',i2,' seed number =',i5)
      end
C* :5 *
C* 6: *
*line 937 "refdif1v25.web"

      subroutine model
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 943 "refdif1v25.web"

      dimension dthi(31),thi(31),thet(iy)

      integer seed

C  Constants.
      g=9.80621
      rho=1000.
      pi=3.1415927
      eps=1.0e-05

C  Execute model once for each frequency.

C     |ifreq| is the controlling index value.
      do 200 ifreq=1,nfreqs
      psibar=0.
      write(10,203)ifreq

C  Specify initial nonlinear parameters for each run.
      if(ntype.EQ.0)an=0.
      if(ntype.NE.0)an=1.
      if(ntype.NE.2)anl=0.
      if(ntype.EQ.2)anl=1.

C   Calculate the mean |kb| on the first row, for use in specifying initial cond
Citions.
      npts=0
      sumk=0.
      do 10 jr=1,nr
      d(1,jr)=dr(1,jr)+tide(ifreq)
      call wvnum(d(1,jr),ur(1,jr),freqs(ifreq),k(1,jr),eps,icdw,1,1)
      if(d(1,jr).GT.0.05)then
      sumk=sumk+k(1,jr)
      npts=npts+1
      endif
10    continue
      kb(1)=sumk/float(npts)

C  Establish initial wave conditions for the |ifreq| frequency
      if(iinput.EQ.1)then

C  Compute wave from data given in |indat.dat|.
      if(iwave.EQ.1)then

C  |iwave.eq.1|, discrete components specified.
      do 3 j=1,n
      a(1,j)=cmplx(0.,0.)
      do 2 iwavs=1,nwavs(ifreq)
      thet(j)=dir(ifreq,iwavs)*180./pi
      a(1,j)=a(1,j)+amp(ifreq,iwavs)*cexp(cmplx(0.,kb(1)*sin(dir(ifreq,i
     &wavs))*y(j)))
2     continue
3     continue
      write(9,202)(thet(j),j=1,n,nd)
      else

C  |iwave.eq.2|, directional spreading model.
      sp=float(nwavs(ifreq))
      nsp=nwavs(ifreq)
      thmax=pi/4.
      call acalc(thmax,nsp,a1)
      edens(ifreq)=sqrt(edens(ifreq)/a1)
      nn=31
      ii=(nn-1)/2+1
      seed=rand(seed)

C  Compute randomly distributed $\Delta\theta$'s.
      sum0=0.
      do 12 i=1,nn
      seed=rand(seed)
      dthi(i)=seed
      sum0=sum0+seed
12    continue
      xnorm=2.*thmax/sum0
      do 101 i=1,nn
      dthi(i)=dthi(i)*xnorm
101   continue
      thi0=-thmax
      do 4 i=1,nn
      thi0=thi0+dthi(i)
      thi(i)=thi0-dthi(i)/2.
      dth=dthi(i)
      amp(ifreq,i)=edens(ifreq)*sqrt(dth)*sqrt(cos(thi(i)+dth/2.)**(2*ns
     &p)+cos(thi(i)-dth/2.)**(2*nsp))
4     continue
      do 5 i=1,nn
      ip1=i+1
      seed=rand(seed)
      dir(ifreq,ip1)=2.*pi*seed/100.
5     continue
      do 7 j=1,n
      a(1,j)=cmplx(0.,0.)
      do 6 i=1,nn
      a(1,j)=a(1,j)+amp(ifreq,i)*cexp(cmplx(0.,kb(1)*sin(thi(i)-thet0)*y
     &(j)+dir(ifreq,i+1)))*2.
6     continue
7     continue
      endif
      endif

C  If |iinput=2|, read |a| from data file |fname4|.
      if(iinput.EQ.2)then
      open(11,file=fname4)
      read(11,*)(a(1,j),j=1,n)
      close(11)
      endif

C  Store first row of wave heights on unit 12.
      write(12,202)(2*cabs(a(1,j))/dconv(iu),j=1,n,nd)

C  If |fname6| not null, store surface on file |fname6|.
      x(1)=0
      if(fname6.NE.' ')then
      write(8,*)n
      write(8,*)(y(j),j=1,n)
      write(8,*)x(1)
      write(8,*)(real(a(1,j)),j=1,n)
      endif

C  Now execute model for the |ifreq| frequency over each of |mr|       grid bloc
Cks.  |ir| is the controlling index value.
      do 100 ir=1,(mr-1)

C  Establish interpolated grid block for segment |ir|.
      call grid(ifreq,ir)

C  If |ir=1| write initial values on |iun(3)|.
      if(ir.EQ.1)then
      write(10,201)x(1)/dconv(iu),psibar
      write(iun(3),*)x(1)/dconv(iu),psibar
      write(iun(3),*)(a(1,j)/dconv(iu),j=1,n,nd)
      write(16,202)(d(1,j)/dconv(iu),j=1,n,nd)
      endif

C  Calculate constants for each grid block.
      call con(ifreq,ir)

C  Perform finite difference calculations.
      call fdcalc(ifreq,ir)

C  Grid block |ir| done, print output and go to next grid.
100   continue
      if(ioutput.EQ.2)then
      write(33,*)(a(m,j),j=1,n)
      endif

C  Termination for the |surface.dat| file.
      if(fname6.NE.' ')then
      x(1)=-100.
      write(8,*)x(1)
      endif

C  Model complete for the |ifreq| frequency component, go to the next frequency
C      component.
200   continue

C   Runs completed for all frequencies.  Return to end of main program.
      return
201   format(' x=',f10.2,'  psibar=',f20.4)
202   format(' ',200(f10.4))
203   format('1',20x,'model execution, frequency',' component',i4//)
      end
C* :6 *
C* 7: *
*line 1178 "refdif1v25.web"

      subroutine grid(ifreq,ir)
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 1184 "refdif1v25.web"

C  Constants.
      pi=3.1415927
      eps=1.0e-05

C  Perform $y$-interpolation on reference grid.

C  Interpolate first row.
      do 10 j=1,n,nd
      d(1,j)=dr(ir,((j-1)/nd+1))
      u(1,j)=ur(ir,((j-1)/nd+1))
      v(1,j)=vr(ir,((j-1)/nd+1))
10    continue
      do 12 jj=2,nr
      do 11 j=1,(nd-1)
      jjj=nd*(jj-2)+(j+1)
      d(1,jjj)=(dr(ir,jj)-dr(ir,jj-1))*y(jjj)/dyr+(yr(jj)*dr(ir,jj-1)-yr
     &(jj-1)*dr(ir,jj))/dyr
      u(1,jjj)=(ur(ir,jj)-ur(ir,jj-1))*y(jjj)/dyr+(yr(jj)*ur(ir,jj-1)-yr
     &(jj-1)*ur(ir,jj))/dyr
      v(1,jjj)=(vr(ir,jj)-vr(ir,jj-1))*y(jjj)/dyr+(yr(jj)*vr(ir,jj-1)-yr
     &(jj-1)*vr(ir,jj))/dyr
11    continue
12    continue

C  Set number of $x$ points and define $x$ values.
      if(ispace.EQ.0)then

C  |ispace=0|, program sets subdivisions.
      do 13 j=1,n
      dref=d(1,j)+tide(ifreq)
      if(dref.LT.0.001)dref=0.001
      call wvnum(dref,u(1,j),freqs(ifreq),k(1,j),eps,icdw,1,j)
13    continue
      npts=0
      sumk=0.
      do 14 j=1,n
      if(d(1,j).GT.0.05)then
      sumk=sumk+k(1,j)
      npts=npts+1
      endif
14    continue
      kb(1)=sumk/float(npts)
      alw=2.*pi/kb(1)
      anw=dxr/alw
      np=ifix(5.*anw)
      if(np.LT.1)np=1
      md(ir)=min((ix-1),np)
      if(np.GT.(ix-1))write(10,100)ir
      endif

C     |ispace=1|, user specified subdivision.
      m=md(ir)+1
      dx=dxr/float(md(ir))
      do 15 i=1,m
      x(i)=xr(ir)+float(i-1)*dx
15    continue

C     interpolate values on |m| row.
      do 16 j=1,n,nd
      d(m,j)=dr(ir+1,((j-1)/nd+1))
      u(m,j)=ur(ir+1,((j-1)/nd+1))
      v(m,j)=vr(ir+1,((j-1)/nd+1))
16    continue
      do 18 jj=2,nr
      do 17 j=1,(nd-1)
      jjj=nd*(jj-2)+(j+1)
      d(m,jjj)=(dr(ir+1,jj)-dr(ir+1,jj-1))*y(jjj)/dyr+(yr(jj)*dr(ir+1,jj
     &-1)-yr(jj-1)*dr(ir+1,jj))/dyr
      u(m,jjj)=(ur(ir+1,jj)-ur(ir+1,jj-1))*y(jjj)/dyr+(yr(jj)*ur(ir+1,jj
     &-1)-yr(jj-1)*ur(ir+1,jj))/dyr
      v(m,jjj)=(vr(ir+1,jj)-vr(ir+1,jj-1))*y(jjj)/dyr+(yr(jj)*vr(ir+1,jj
     &-1)-yr(jj-1)*vr(ir+1,jj))/dyr
17    continue
18    continue

C     interpolate values in |x|-direction
      do 19 i=2,m-1
      do 19 j=1,n
      d(i,j)=(d(m,j)-d(1,j))*x(i)/dxr+(x(m)*d(1,j)-x(1)*d(m,j))/dxr
      u(i,j)=(u(m,j)-u(1,j))*x(i)/dxr+(x(m)*u(1,j)-x(1)*u(m,j))/dxr
      v(i,j)=(v(m,j)-v(1,j))*x(i)/dxr+(x(m)*v(1,j)-x(1)*v(m,j))/dxr
19    continue

C  Add in user specified grid subdivisions (read from unit |iun(2)|).
      do 30 jr=1,nr-1
      if(isd(ir,jr).EQ.1)then
      js=nd*jr+(1-nd)
      jf=js+nd
      read(iun(2),101)((d(i,j),j=js,jf),i=1,m)
      if(icur.EQ.1)then
      read(iun(2),101)((u(i,j),j=js,jf),i=1,m)
      read(iun(2),101)((v(i,j),j=js,jf),i=1,m)
      endif
      do 31 i=1,m
      do 31 j=js,jf
      d(i,j)=d(i,j)*dconv(iu)
      u(i,j)=u(i,j)*dconv(iu)
      v(i,j)=v(i,j)*dconv(iu)
31    continue
      end if
30    continue

C  Add tidal offset to all rows and establish thin film.
      do 20 i=1,m
      do 20 j=1,n
      d(i,j)=d(i,j)+tide(ifreq)
      if(d(i,j).LT.0.001)d(i,j)=0.001
20    continue

C  Interpolation complete, return to |model|.
      return
100   format(' model tried to put more spaces than allowed in',' grid bl
     &ock ',i3)
101   format(20f10.4)
      end
C* :7 *
C* 8: *
*line 1350 "refdif1v25.web"
      subroutine con(ifreq,ir)
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 1355 "refdif1v25.web"

C  Constants.
      eps=1.0e-05
      g=9.80621

C  Calculate constants.
      do 1 i=1,m
      do 1 j=1,n
      call wvnum(d(i,j),u(i,j),freqs(ifreq),k(i,j),eps,icdw,i,j)
      sig(i,j)=freqs(ifreq)-k(i,j)*u(i,j)
      akd=k(i,j)*d(i,j)
      q(i,j)=(1.+akd/(sinh(akd)*cosh(akd)))/2.
      p(i,j)=q(i,j)*g*tanh(akd)/k(i,j)
      dd(i,j)=(cosh(4.*akd)+8.-2.*(tanh(akd)**2))/(8.*(sinh(akd)**4.))
      bottomu(i,j)=g*k(i,j)/(2*freqs(ifreq)*cosh(akd))
1     continue

C  Calculate the dissipation term |w|.
      call diss

C  Calculate the mean |kb| on each row.
      do 11 i=1,m
      npts=0
      sumk=0.
      do 10 j=1,n
      if(d(i,j).GT.0.05)then
      sumk=sumk+k(i,j)
      npts=npts+1
      endif
10    continue
      if(npts.EQ.0)then
      kb(i)=k(i,1)
      else
      kb(i)=sumk/float(npts)
      endif
11    continue
      return
      end
C* :8 *
C* 9: *
*line 1448 "refdif1v25.web"

      subroutine fdcalc(ifreq,ir)
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 1454 "refdif1v25.web"

      real kap,ksth1,ksth2
      complex c1,c2,c3,cp1,cp2,cp3,ci,damp
      complex ac(iy),bc(iy),cc(iy),rhs(iy),sol(iy)
      dimension thet(iy),urs(iy)
C* 10: *
*line 1800 "refdif1v25.web"

      cg(i,j)=sqrt(p(i,j)*q(i,j))
      pv(i,j)=p(i,j)-v(i,j)*v(i,j)
      bet(i,j)=-4.*(k(i+1,j)-k(i,j))/(dx*((k(i+1,j)+k(i,j))**2))-4.*(k(i
     &+1,j)*(p(i+1,j)-u(i+1,j)**2)-k(i,j)*(p(i,j)-u(i,j)**2))/(dx*((k(i+
     &1,j)+k(i,j))**2.)*(p(i+1,j)+p(i,j)-(u(i+1,j)**2+u(i,j)**2)))
      dv(i,j)=(cg(i+1,j)+u(i+1,j))/sig(i+1,j)-(cg(i,j)+u(i,j))/sig(i,j)-
     &delta1*dx*((v(i+1,j+1)/sig(i+1,j+1))+(v(i,j+1)/sig(i,j+1))-(v(i+1,
     &j-1)/sig(i+1,j-1))-(v(i,j-1)/sig(i,j-1)))/(2.*dy)
      damp(i,j)=2.*ci*cdamp*((cg(i+1,j)+u(i+1,j))+(cg(i,j)+u(i,j)))/(dy*
     &dy*(k(i+1,j)**2+k(i,j)**2))
      deltap(i,j)=a1-b1*kb(i)/k(i,j)
      cp1(i,j)=(cg(i+1,j)+u(i+1,j))*cmplx(1.,dx*(kb(i+1)-a0*k(i+1,j)))+c
     &mplx(1.,0.)*(cg(i,j)+u(i,j)+dv(i,j)*(sig(i+1,j)+sig(i,j))/4.)+2.*o
     &meg*cmplx(0.,1.)*(-b1)*bet(i,j)*(u(i+1,j)+u(i,j))/sig(i+1,j)+4.*om
     &eg*(-b1)*cmplx(0.,1.)*(3.*(u(i+1,j)-u(i,j))/dx+(v(i+1,j+1)+v(i,j+1
     &)-v(i+1,j-1)-v(i,j-1))/(4.*dy))/(sig(i+1,j)*(k(i+1,j)+k(i,j)))+cmp
     &lx(-2.*(-b1)/(dy*dy*(k(i+1,j)+k(i,j)))+b1*bet(i,j)*dx/(2.*dy*dy),-
     &deltap(i,j)*dx/(2.*dy*dy))*(pv(i+1,j+1)+2.*pv(i+1,j)+pv(i+1,j-1))/
     &sig(i+1,j)-cmplx(1.,0.)*omeg*delta2*(3.*u(i+1,j)+u(i,j))/(2.*sig(i
     &+1,j))+ci*omeg*(a0-1.)*k(i+1,j)*u(i+1,j)*dx/sig(i+1,j)+2.*ifilt*da
     &mp(i,j)+cmplx(1.,0.)*alphn*dx
      cp2(i,j)=cmplx((-delta1*dx)*(v(i+1,j)+v(i,j))/(2.*dy)+b1*u2*bet(i,
     &j)*(u(i+1,j)*v(i+1,j)+u(i,j)*v(i,j))/(dy*sig(i+1,j+1)),(-delta1*u2
     &)*(u(i+1,j+1)*v(i+1,j+1)+u(i,j+1)*v(i,j+1)+2.*u(i+1,j)*v(i+1,j))/(
     &2.*dy*sig(i+1,j+1))+dx*(-b1)*bet(i,j)*(sig(i+1,j)*v(i+1,j)+sig(i,j
     &)*v(i,j))/(2.*dy*sig(i+1,j+1)))+cmplx(2.*(-b1)/(dy*dy*(k(i+1,j)+k(
     &i,j)))+(-b1)*bet(i,j)*dx/(2.*dy*dy),+deltap(i,j)*dx/(2.*dy*dy))*(p
     &v(i+1,j+1)+pv(i+1,j))/sig(i+1,j+1)+4.*cmplx(0.,1.)*(-b1)*sig(i+1,j
     &)*v(i+1,j)/(dy*sig(i+1,j+1)*(k(i+1,j)+k(i,j)))-ifilt*damp(i,j)
      cp3(i,j)=cmplx(-(-delta1*dx)*(v(i+1,j)+v(i,j))/(2.*dy)+(-b1)*u2*be
     &t(i,j)*(u(i+1,j)*v(i+1,j)+u(i,j)*v(i,j))/(dy*sig(i+1,j-1)),-(-delt
     &a1*u2)*(u(i+1,j-1)*v(i+1,j-1)+u(i,j-1)*v(i,j-1)+2.*u(i+1,j)*v(i+1,
     &j))/(2.*dy*sig(i+1,j-1))-dx*(-b1)*bet(i,j)*(sig(i+1,j)*v(i+1,j)+si
     &g(i,j)*v(i,j))/(2.*dy*sig(i+1,j-1)))+cmplx(2.*(-b1)/(dy*dy*(k(i+1,
     &j)+k(i,j)))+(-b1)*bet(i,j)*dx/(2.*dy*dy),-(-deltap(i,j)*dx)/(2.*dy
     &*dy))*(pv(i+1,j)+pv(i+1,j-1))/sig(i+1,j-1)-4.*cmplx(0.,1.)*(-b1)*s
     &ig(i+1,j)*v(i+1,j)/(dy*sig(i+1,j-1)*(k(i+1,j)+k(i,j)))-ifilt*damp(
     &i,j)
      c1(i,j)=cmplx(cg(i+1,j)+u(i+1,j)-dv(i,j)*(sig(i+1,j)+sig(i,j))/4.,
     &0.)+cmplx(1.,-dx*(kb(i)-a0*k(i,j)))*(cg(i,j)+u(i,j))+2.*cmplx(0.,1
     &.)*omeg*(-b1)*bet(i,j)*(u(i+1,j)+u(i,j))/sig(i,j)+4.*cmplx(0.,1.)*
     &omeg*(-b1)*(3.*(u(i+1,j)-u(i,j))/dx+(v(i+1,j+1)+v(i,j+1)-v(i+1,j-1
     &)-v(i,j-1))/(4.*dy))/(sig(i,j)*(k(i+1,j)+k(i,j)))+cmplx(2.*b1/(dy*
     &dy*(k(i+1,j)+k(i,j)))-b1*bet(i,j)*dx/(2.*dy*dy),+deltap(i,j)*dx/(2
     &.*dy*dy))*(pv(i,j+1)+2.*pv(i,j)+pv(i,j-1))/sig(i,j)-cmplx(1.,0.)*o
     &meg*delta2*(3.*u(i+1,j)+u(i,j))/(2.*sig(i,j))-ci*omeg*(a0-1.)*k(i,
     &j)*u(i,j)*dx/sig(i,j)+2.*ifilt*damp(i,j)-cmplx(1.,0.)*alphn*dx
      c2(i,j)=cmplx(delta1*dx*(v(i+1,j)+v(i,j))/(2.*dy)+b1*u2*bet(i,j)*(
     &u(i+1,j)*v(i+1,j)+u(i,j)*v(i,j))/(dy*sig(i,j+1)),(-delta1*u2)*(u(i
     &+1,j+1)*v(i+1,j+1)+u(i,j+1)*v(i,j+1)+2.*u(i,j)*v(i,j))/(2.*dy*sig(
     &i,j+1))+4.*(-b1)*sig(i,j)*v(i,j)/(dy*(k(i+1,j)+k(i,j))*sig(i,j+1))
     &-dx*(-b1)*bet(i,j)*(sig(i+1,j)*v(i+1,j)+sig(i,j)*v(i,j))/(2.*dy*si
     &g(i,j+1)))+cmplx(2.*(-b1)/(dy*dy*(k(i+1,j)+k(i,j)))+b1*bet(i,j)*dx
     &/(2.*dy*dy),(-deltap(i,j)*dx)/(2.*dy*dy))*(pv(i,j+1)+pv(i,j))/sig(
     &i,j+1)-ifilt*damp(i,j)
      c3(i,j)=cmplx((-delta1*dx)*(v(i+1,j)+v(i,j))/(2.*dy)-b1*u2*bet(i,j
     &)*(u(i+1,j)*v(i+1,j)+u(i,j)*v(i,j))/(dy*sig(i,j-1)),(delta1*u2)*(u
     &(i+1,j-1)*v(i+1,j-1)+u(i,j-1)*v(i,j-1)+2.*u(i,j)*v(i,j))/(2.*dy*si
     &g(i,j-1))-4.*(-b1)*sig(i,j)*v(i,j)/(dy*(k(i+1,j)+k(i,j))*sig(i,j-1
     &))+dx*(-b1)*bet(i,j)*(sig(i+1,j)*v(i+1,j)+sig(i,j)*v(i,j))/(2.*dy*
     &sig(i,j-1)))+cmplx(-2.*b1/(dy*dy*(k(i+1,j)+k(i,j)))+b1*bet(i,j)*dx
     &/(2.*dy*dy),(-deltap(i,j)*dx)/(2.*dy*dy))*(pv(i,j)+pv(i,j-1))/sig(
     &i,j-1)-ifilt*damp(i,j)
      f1(i,j)=tanh(k(i,j)*d(i,j))**5.
      f2(i,j)=(k(i,j)*d(i,j)/sinh(k(i,j)*d(i,j)))**4.
C* :10 *
*line 1461 "refdif1v25.web"


C  Constants.

C  70 degree minimax coefficients.

C      a0=0.994733           a1=-0.890065           b1=-0.451641

C  Pad\'{e} coefficients.
      a0=1.0
      a1=-0.75
      b1=-0.25

C  Additional constants.
      u2=1.0
      kap=0.78
      gam=0.4
      omeg=freqs(ifreq)
      pi=3.1415927
      ci=cmplx(0.,1.)
      cdamp=0.00025
      alphn=0.
      delta1=a1-b1
      delta2=1+2.*a1-2.*b1

C  Initialize breaking index if |ir = 1|.
      if(ir.EQ.1)then
      ifilt=0
      do 100 j=1,n
      ibr(j)=0
      wb(1,j)=cmplx(0.,0.)
100   continue
      endif

C  Solution for |m| grid blocks in reference block |ir|.
      do 200 i=1,(m-1)
      ip1=i+1
      it=1
      ih=1

C  r.h.s. of matrix equation.
      rhs(1)=cmplx(0.,0.)
      do 1 j=2,(n-1)
      rhs(j)=c1(i,j)*a(i,j)+c2(i,j)*a(i,j+1)+c3(i,j)*a(i,j-1)-dx*(w(i,j)
     &+wb(1,j))*a(i,j)/2.-dx*cmplx(0.,1.)*an*anl*sig(i,j)*k(i,j)*k(i,j)*
     &dd(i,j)*(1.-float(ibr(j)))*(cabs(a(i,j))**2.)*a(i,j)/2.-dx*cmplx(0
     &.,1.)*(1.-float(ibr(j)))*an*(1.-anl)*sig(i,j)*((1.+f1(i,j)*k(i,j)*
     &k(i,j)*(cabs(a(i,j))**2.)*dd(i,j))*tanh(k(i,j)*d(i,j)+f2(i,j)*k(i,
     &j)*cabs(a(i,j)))/tanh(k(i,j)*d(i,j))-1.)*a(i,j)/2.
1     continue
      rhs(n)=cmplx(0.,0.)

C  Return here for iterations.
2     if(it.EQ.1)ii=i
      if(it.EQ.2)ii=ip1

C  Establish boundary conditions.
      if(ibc.EQ.1)then
      ksth1=real((2.*(a(i,2)-a(i,1))/((a(i,2)+a(i,1))*dy))*cmplx(0.,-1.)
     &)
      ksth2=real((2.*(a(i,n)-a(i,n-1))/((a(i,n)+a(i,n-1))*dy))*cmplx(0.,
     &-1.))
      bc(1)=cmplx(1.,ksth1*dy/2.)
      cc(1)=-cmplx(1.,-ksth1*dy/2.)
      bc(n)=-cmplx(1.,-ksth2*dy/2.)
      ac(n)=cmplx(1.,ksth2*dy/2.)
      else
      bc(1)=cmplx(1.,0.)
      cc(1)=-bc(1)
      bc(n)=cmplx(1.,0.)
      ac(n)=-bc(n)
      endif

C  Calculate dissipation in rows where breaking occurs.
      do 3 j=1,n
      if(ibr(j).EQ.1)wb(2,j)=cmplx(1.,0.)*0.15*cg(ip1,j)*(1.-(gam*d(ip1,
     &j)/(2.*cabs(a(ii,j))))**2.)/d(ip1,j)
      if(ibr(j).EQ.0)wb(2,j)=cmplx(0.,0.)
3     continue

C  Coefficients for forward row.
      do 4 j=2,(n-1)
      ac(j)=cp3(i,j)
      bc(j)=cp1(i,j)+(dx/2.)*(w(i+1,j)+wb(2,j))+cmplx(0.,an*anl)*sig(i+1
     &,j)*k(i+1,j)*k(i+1,j)*dd(i+1,j)*(cabs(a(ii,j))**2.)*(dx/2.)+cmplx(
     &0.,an*(1.-anl))*sig(i+1,j)*(dx/2.)*((1.+f1(i+1,j)*k(i+1,j)*k(i+1,j
     &)*(cabs(a(ii,j))**2.)*dd(i+1,j))*tanh(k(i+1,j)*d(i+1,j)+f2(i+1,j)*
     &k(i+1,j)*cabs(a(ii,j)))/tanh(k(i+1,j)*d(i+1,j))-1.)
      cc(j)=cp2(i,j)
4     continue

C  Update solution one step.
      call ctrida(1,n,ac,bc,cc,rhs,sol)
      do 5 j=1,n
      a(ip1,j)=sol(j)
      sol(j)=cmplx(0.,0.)
5     continue
      if((it.EQ.2).OR.(ih.EQ.2))go to 8

C  Check for start or stop of breaking in each row.
      do 6 j=1,n
      urs(j)=2.*cabs(a(ip1,j))
6     continue
      isave1=0
      isave2=0
      do 7 j=1,n
      iset=0
      ireset=0
      if(((urs(j)/d(ip1,j)).GT.kap).AND.(ibr(j).EQ.0))iset=1
      if(iset.EQ.1)then
      ibr(j)=1
      isave1=1
      end if
      if(((urs(j)/d(ip1,j)).LT.gam).AND.(ibr(j).EQ.1))ireset=1
      if(ireset.EQ.1)then
      ibr(j)=0
      isave2=1
      end if
7     continue
      ih=2

C  Redo initial calculation if breaking status changes.
      if((isave1.EQ.1).OR.(isave2.EQ.1))go to 2
8     continue
      if(it.EQ.2)go to 9
      it=2
      go to 2
9     continue

C  For Stokes model alone (|ntype.eq.2|), test to see whether Ursell       param
Ceter is too large.
      if(ntype.EQ.2)then
      do 11 j=1,n
      urs(j)=(cabs(a(ip1,j))/d(ip1,j))/((k(ip1,j)*d(ip1,j))**2)
      if(urs(j).GT.0.5)write(10,204)urs(j),i,j
11    continue
      end if

C  Roll back breaking dissipation coefficient at each row.
      do 12 j=1,n
      wb(1,j)=wb(2,j)
12    continue

C  Calculate reference phase function for surface plotting.
      psibar=psibar+(kb(ip1)+kb(i))*dx/2.

C  Store plotted surface if requested.
      if(fname6.NE.' ')then
      write(8,*)x(ip1)
      write(8,*)(real(a(ip1,j)*cexp(cmplx(0.,psibar))),j=1,n)
      endif

C  Start filter if breaking is occuring.
      do 13 j=1,n
      if(ibr(j).EQ.1)ifilt=1
13    continue
200   continue

C Calculate wave angles at reference grid rows.   Note:  angles are not well
C  defined in a directional, multicomponent sea, or where waves become short
C  crested.  This routine was heavily modified by Raul Medina, University of
C  Cantabria.
      do 15 j=1,n
      if(a(m,j).EQ.(0.,0.))then
      akx2=0.
      else
      akx2=aimag(clog(a(m,j)))
      endif
      if(a(m-1,j).EQ.(0.,0.))then
      akx1=0.
      else
      akx1=aimag(clog(a(m-1,j)))
      endif
      if(abs(akx2-akx1).GT.pi)then
      akx=sign((2.*pi-(abs(akx1)+abs(akx2)))/dx,akx1)
      else
      akx=(akx2-akx1)/dx
      endif
      if(j.NE.n)then
      if(a(m,j+1).EQ.(0.,0.))then
      aky2=0.
      else
      aky2=aimag(clog(a(m,j+1)))
      endif
      if(a(m,j).EQ.(0.,0.))then
      aky1=0.
      else
      aky1=aimag(clog(a(m,j)))
      endif
      else
      if(a(m,j).EQ.(0.,0.))then
      aky2=0.
      else
      aky2=aimag(clog(a(m,j)))
      endif
      if(a(m,j-1).EQ.(0.,0.))then
      aky1=0.
      else
      aky1=aimag(clog(a(m,j-1)))
      endif
      endif
      if(abs(aky2-aky1).GT.pi)then
      aky=sign((2.*pi-(abs(aky1)+abs(aky2)))/dy,aky1)
      else
      aky=(aky2-aky1)/dy
      endif
      thet(j)=atan2(aky,(akx+kb(m)))
      thet(j)=180.*thet(j)/pi
15    continue

C  Print out |abs(a)| at grid reference points on unit |iun(4)|.
      mm1=m-1
      write(10,205)(ir+1),mm1
      write(10,202)x(m)/dconv(iu),psibar

C  Wave heights on |height.dat|.
      write(12,203)(2.*cabs(a(m,j))/dconv(iu),j=1,n,nd)

C  Wave angles on |angle.dat|.
      write(9,203)(thet(j),j=1,n,nd)

C  Water depths on |depth.dat|.
      write(16,203)(d(m,j)/dconv(iu),j=1,n,nd)

C  Bottom velocities on |bottomu.dat|.
      if(fname7.NE.' ')then
      do 16 j=1,n,nd
      bottomu(m,j)=bottomu(m,j)*cabs(a(m,j))
16    continue
      write(17,203)(bottomu(m,j)/dconv(iu),j=1,n,nd)
      endif

C  Write out reference grid data on disk file |iun(3)|.
      write(iun(3),*)x(m)/dconv(iu),psibar
      write(iun(3),*)(a(m,j)/dconv(iu),j=1,n,nd)

C  Roll back solution to first grid level.
      do 201 j=1,n
      a(1,j)=a(m,j)
201   continue
      return
202   format(' x=',f10.2,'    reference phase psibar=',f20.4)
203   format(' ',200(f10.4))
204   format(' '//' warning: Ursell number =',f10.4,' encountered at','g
     &rid location',i6,',',i6/' should be using Stokes-Hedges model (nty
     &pe=1) due to shallow','water')
205   format(' grid row ir=',i3,',  ',i3,' x-direction subdivisions',' u
     &sed')
      end
C* :9 *
C* 11: *
*line 1913 "refdif1v25.web"

      subroutine ctrida(ii,l,a,b,c,d,v)
      include 'param.h'
      complex a(iy),b(iy),c(iy),d(iy),v(iy),beta(iy),gamma(iy)

C  Compute intermediate vectors |beta| and |gamma|.
      beta(ii)=b(ii)
      gamma(ii)=d(ii)/beta(ii)
      iip1=ii+1
      do 1 i=iip1,l
      beta(i)=b(i)-a(i)*c(i-1)/beta(i-1)
      gamma(i)=(d(i)-a(i)*gamma(i-1))/beta(i)
1     continue

C  Compute solution vector |v|.
      v(l)=gamma(l)
      last=l-ii
      do 2 k=1,last
      i=l-k
      v(i)=gamma(i)-c(i)*v(i+1)/beta(i)
2     continue
      return
      end
C* :11 *
C* 12: *
*line 1964 "refdif1v25.web"

      subroutine diss
      include 'param.h'
C* 18: *
*line 2222 "refdif1v25.web"

      common/ref1/mr,nr,ispace,nd,md(ixr),iu,dconv(2),iff(3),icur,ibc
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      common/ref3/dxr,dyr,xr(ixr),yr(iyr),x(ix),y(iy)
      common/ref4/isd(ixr,iyr)
      common/block1/d(ix,iy),u(ix,iy),v(ix,iy),m,n,dx,dy,ibr(iy)
      common/con1/q(ix,iy),p(ix,iy),sig(ix,iy),bottomu(ix,iy)
      common/con2/k(ix,iy),kb(ix),w(ix,iy),dd(ix,iy),wb(2,iy)
      common/nlin/an,anl,ntype
      common/wav1/iwave,nfreqs,freqs(ncomp),edens(ncomp),nwavs(ncomp)
      common/wav2/amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),seed,the
     &t0
      common/comp/a(ix,iy),psibar,ifilt
      common/names/fname1,fname2,fname3,fname4,fname5,fname6,fname7,fnam
     &e8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
      real k,kb
      complex w,a,wb
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,fna
     &me8,fname9,fname10,fname11,fname12,fname13,fname14,fname15,fnamein
C* :18 *
*line 1970 "refdif1v25.web"

      real nu,cp,kd

C  Statement function.
      sq(i,j)=sqrt(nu/(2.*sig(i,j)))

C  Constants.
      nu=1.3e-06
      cp=4.5e-11
      g=9.80621
      pi=3.1415927

C     Value of |f| here is value assuming $\tau=(f/8)u^{2}$.

C     $f=4 f_w$ ; $f_w$ is the wave friction factor
      f=0.01*4.0
      do 1 j=1,n
      do 1 i=1,m
      w(i,j)=cmplx(0.,0.)
      kd=k(i,j)*d(i,j)

C     If |iff(1) = 1|, use turbulent boundary layer damping.
      if(iff(1).EQ.1)w(i,j)=2.*f*cabs(a(1,j))*sig(i,j)*k(i,j)/(sinh(2.*k
     &d)*sinh(kd)*3.*pi)

C  If |iff(2) = 1|, add porous bottom damping.
      if(iff(2).EQ.1)w(i,j)=w(i,j)+(g*k(i,j)*cp/(nu*(cosh(kd)**2)))*cmpl
     &x(1.,0.)

C   If |iff(3) = 1|, add boundary layer damping.
      if(iff(3).EQ.1)w(i,j)=w(i,j)+2.*k(i,j)*sig(i,j)*sq(i,j)*(1.+(cosh(
     &kd)**2))*cmplx(1.,-1.)/sinh(2.*kd)
1     continue
      return
      end
C* :12 *
C* 13: *
*line 2038 "refdif1v25.web"
      function rand(x)
      ix=ifix(32767.*x)
      irand=mod(4573*ix+6923,32767)
      rand=float(irand)/32767.
      return
      end
C* :13 *
C* 14: *
*line 2062 "refdif1v25.web"

      function fact(xi)
      prod=1.
      if(xi.GT.1.)then
      do 17 ii=2,int(xi)
      prod=prod*float(ii)
17    continue
      endif
      fact=prod
      return
      end
C* :14 *
C* 15: *
*line 2096 "refdif1v25.web"
      subroutine bnum(in,n,bn)
      xin=in
      xt=fact(xin)
      xb=fact(float(n))*fact(float(in-n))
      bn=xt/xb
      return
      end
C* :15 *
C* 16: *
*line 2123 "refdif1v25.web"

      subroutine acalc(thmax,nsp,a)
      itn=2*nsp
      call bnum(itn,nsp,bn)
      a=thmax*bn/(2.**(itn-1))
      sum=0.
      do 10 ik=1,nsp
      ki=ik-1
      call bnum(itn,ki,bn)
10    sum=sum+bn*sin(float(nsp-ki)*thmax)/float(nsp-ki)
      a=a+sum/(2.**(itn-2))
      return
      end
C* :16 *
C* 17: *
*line 2176 "refdif1v25.web"
      subroutine wvnum(d,u,s,k,eps,icdw,i,j)
      include 'param.h'
      common/ref2/dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),iun(8),iinput,iout
     &put
      real k,kn

C  Constants.
      g=9.806
      pi=3.1415927
      k=s*s/(g*sqrt(tanh(s*s*d/g)))

C  Newton-Raphson iteration.
      do 1 ii=1,20
      f=s*s-2.*s*k*u+k*k*u*u-g*k*tanh(k*d)
      fp=-2.*s*u+2.*k*u*u-g*tanh(k*d)-g*k*d/(cosh(k*d)**2.)
      kn=k-f/fp
      if((abs(kn-k)/kn).LT.eps)go to 2
      k=kn
1     continue
      t=2.*pi/(sqrt(g*k*tanh(k*d))+k*u)
      write(10,100)i,j,k,u,d,f,t
      icdw=1
      return
2     k=kn
      return
100   format(' wavenumber iter. failed to converge on row',i10,'  column
     &',i10/'      k=',f15.8,'      u=',f15.8/'      d=',f15.8,'      f=
     &',f15.8/'      t=',f15.8)
      end
C* :17 *
