C*-------------------------------------------------------------------
C*
C*    datgenv25.f
C*
C*    This program generates input data files for several example
c*    applications of REF/DIF 1.  In particular, the first three cases
c*    listed here correspond to the three test cases shown in the
c*    User's Manual.
c*
c*    James T. Kirby
c*
c*    kirby@coastal.udel.edu, (302) 831-2438, FAX (302) 831-1228
c*
c*    Center for Applied Coastal Research
c*    Department of Civil Engineering
c*    University of Delaware
c*    Newark, DE 19716
C*
C*    January 1991, revised July 1994 for REF/DIF 1 version 2.5.
c*
c*    Last revision 12/22/94.
C*
C*-------------------------------------------------------------------

      include 'param.h'

      dimension iun(4), md(ixr)

      common/ref/ dr(ixr,iyr),ur(ixr,iyr),vr(ixr,iyr),mr,nr,dxr,dyr,
     1itype
      common/ind/ iu,ntype,icur,ibc,ispace,nd,iff,isp,iinput,
     1iwave,nfreqs,freqs,tide,nwavers,amp,dir,edens
      common/dims/ x(ixr),y(iyr)
      dimension iff(3)
      dimension amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),
     1          freqs(ncomp),edens(ncomp),nwavs(ncomp)
      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15,fnamein

      data fname1/'refdat.dat'/, fname2/'outdat.dat'/,
     1     fname3/'subdat.dat'/, fname4/'wave.dat'/,
     1     fname5/'owave.dat'/, fname6/'surface.dat'/,
     1     fname7/'bottomu.dat'/,fname8/'angle.dat'/,
     1     fname9/' '/,fname10/'refdif1.log'/,
     1     fname11/'height.dat'/,fname12/'sxx.dat'/,
     1     fname13/'sxy.dat'/,fname14/'syy.dat'/,
     1     fname15/'depth.dat'/


      namelist /ingrid/ mr, nr, iu, ntype, icur, ibc, dxr, dyr, dt,
     1                  ispace,nd,iff,isp,iinput,ioutput
     1         /inmd/ md
     1         /fnames/ fname1,fname2,fname3,fname4,fname5,fname6,
     1                  fname7,fname8,fname9,fname10,fname11,fname12,
     1                  fname13,fname14,fname15
     1         /waves1a/ iwave, nfreqs
     1         /waves1b/ freqs, tide, nwavs, amp, dir
     1         /waves1c/ thet0, freqs, tide, edens, nwavs, nseed
     1         /waves2/ freqin, tidein
c*------------------------------------------------------------------
c*    setup the logical devices for input:
c*          *=keyboard input
c*          iun(2)=output file "refdat.dat"
c*          iun(3)=screen output (use 0 for sun, 3 for pc)
c*          iun(4)=output file "indat.dat"
c*-------------------------------------------------------------------
      iun(2)=20
      iun(3)=0

      call infile(fnamein)

      iun(4)=24
      open(iun(4),file=fnamein)

c*-------------------------------------------------------------------
c*    initialize all entries for indat.dat prior to generating the
c*    depth grid.
c*-------------------------------------------------------------------
      iu=0
      ntype=0
      icur=0
      ibc=0
      ispace=0
      nd=1
      if1=0
      if2=0
      if3=0
      isp=0
      iinput=0
      iwave=0
      nfreqs=0

      nwaves=0

c*-------------------------------------------------------------------
c*    open file for reference grid data
c*-------------------------------------------------------------------

      open(iun(2),file=fname1)

c*-------------------------------------------------------------------
c*    establish depth grid
c*-------------------------------------------------------------------

      call depth(iun)

c*-------------------------------------------------------------------
c*    calculate constants
c*-------------------------------------------------------------------
      dt=10.
      call con
c*-------------------------------------------------------------------
c*    write reference grid data
c*-------------------------------------------------------------------
      do 1 i=1,mr
      write(iun(2),100)(dr(i,j),j=1,nr)
 1    continue

      if(icur.eq.1)then

      do 2 i=1,mr
      write(iun(2),100)(ur(i,j),j=1,nr)
 2    continue

      do 3 i=1,mr
      write(iun(2),100)(vr(i,j),j=1,nr)
 3    continue

      endif

      close(iun(2))

 100  format(20f10.4)

c*---------------------------------------------------------------------
c*    generation of file indat.dat
c*---------------------------------------------------------------------

      write(*,*) ' do you want to create indat.dat? yes=1'
      read(*,*) ians

      if(ians.eq.1) then
      open(iun(4),file='indat.dat')

c*---------------------------------------------------------------------
c*    write fnames portion of namelist file
c*---------------------------------------------------------------------

      write(iun(4),nml=fnames)

c*---------------------------------------------------------------------
c*    write ingrid portion of namelist file
c*---------------------------------------------------------------------

      if(iu.eq.0)then
      write(iun(3),*)' input iu: 1=mks, 2=english'
      read(*,*) iu
      endif
      write(iun(3),*)' input dispersion relationship; ntype: 0=linear,'
      write(iun(3),*)'                          1=composite, 2=stokes'
      read(*,*)ntype
      write(iun(3),*)' input lateral boundary condition; ibc: 0=closed'
      write(iun(3),*)'                                        1=open'
      read(*,*) ibc

      write(iun(3),102)
 102  format(' input ispace (0=program picks x spacing, 1=user choses)')
      read(*,*) ispace
      write(*,*)' input nd (# y divisions, 1 is minimum)'
      read (*,*) nd

      if(ispace.eq.0) go to 105
      write(iun(3),*)' constant or variable x spacing?(0 for constant)'
      read(*,*) ians1
      if(ians1.eq.0) then
      write(*,*) ' input constant md'
      read(*,*) mdc
      do 103 iko=1,mr-1
      md(iko)=mdc
 103  continue
      else
      write(iun(3),104)
 104  format(' input md(i) for i=1 to mr-1')
      read(*,*) (md(i),i=1,mr-1)
      endif

 105  write(iun(3),106)
 106  format(' input if(1) turbulent, if(2) porous, if(3) laminar')
      write(iun(3),*) '  standard  choice:  1, 0, 0'
      read(*,*) iff(1), iff(2), iff(3)

      write(iun(3),107)
 107  format(' input isp (subgrid features) :standard  0')
      read(*,*) isp

      write(iun(3),108)
 108  format(' input values of iinput, ioutput:'/
     1' iinput: 1 standard, i.e., not starting from previous run'/
     1'         2 if starting from previous run'/
     1' ioutput: 1 standard, not saving restart data'/
     1'          2 if saving restart data')
      read(*,*)iinput,ioutput

      write(iun(3),115)
 115  format(' input value of isurface:'/
     1'  isurface = 0:   no surface picture generated'/
     1'  isurface = 1:   surface picture generated')
      read(*,*) isurface
      if(isurface.eq.0) fname6 = ' '

      write(iun(4),nml=ingrid)

      if(ispace.eq.1) write(iun(4),nml=inmd)

      if(iinput.eq.1) then
c*----------------------------------------------------------------------
c*    write waves1 portion of indat.dat
c*----------------------------------------------------------------------
      write(iun(3),*)' input iwave (1 discrete, 2 directional spread)'
      read(*,*) iwave
      write(iun(3),*) ' input nfreq (# of frequencies)'
      read(*,*) nfreqs

      write(iun(4), nml=waves1a)

      if (iwave.eq.2) then
      write(*,*)' enter central direction thet0'
      read(*,*) thet0
      endif

      do 113 ifreq=1,nfreqs
c*----------------------------------------------------------------------
c*    line 10, iinput=1
c*----------------------------------------------------------------------
      write(iun(3),109)
 109  format(' input wave period and tide stage')
      read(*,*) freqs(ifreq), tide(ifreq)

c*----------------------------------------------------------------------
c*    line 11, iwave=1, iinput=1
c*----------------------------------------------------------------------
      if(iwave.eq.1) then

      write(iun(3),110)
 110  format(' input # of waves per frequency, nwavs')
      read(*,*) nwavs(ifreq)

c*----------------------------------------------------------------------
c*    line 12, iwave=1, iinput=1
c*----------------------------------------------------------------------
      do 111 iwavs=1,nwavs(ifreq)
      write(iun(3),*)' input amplitude and direction'
      read(*,*) amp(ifreq,iwavs), dir(ifreq,iwavs)

 111  continue

      else

c*---------------------------------------------------------------------
c*    iwave=2, iinput=1
c*---------------------------------------------------------------------
      write(iun(3),112)
 112  format('input en. density and on next line, directional',
     1'  spreading factor')
      read(*,*) edens(ifreq)
      read(*,*) nwavs(ifreq)

      nseed=500

      endif
 113  continue

      if (iwave .eq. 1) write(iun(4), nml=waves1b)
      if (iwave .eq. 2) write(iun(4), nml=waves1c)

      endif

      if ( iinput .eq. 2) then
c*---------------------------------------------------------------------
c*    line 9, iinput=2
c*---------------------------------------------------------------------
      write(iun(3),*)' input wave period and tide stage'
      read(*,*) freqin ,tidein

      write(iun(4), nml=waves2)

      endif

      close(iun(4))

      endif

      stop
      end

c*---------------------------------------------------------------------
      subroutine con
c*---------------------------------------------------------------------

      include 'param.h'

      common/ref/ d(ixr,iyr),u(ixr,iyr),v(ixr,iyr),m,n,dx,dy,itype
      common/ind/ iu,ntype,icur,ibc,ispace,nd,iff,isp,iinput,
     1iwave,nfreqs,freqs,tide,nwavers,amp,dir,edens
      common/dims/ x(ixr),y(iyr)
      dimension iff(3)
      dimension amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),
     1          freqs(ncomp),edens(ncomp),nwavs(ncomp)

      do 1 i=1,m
      do 1 j=1,n
      if((icur.eq.1).and.(itype.eq.3))then
      xp=x(m)-x(i)
      u(i,j)=-0.02295*exp(-((xp/76.2)**2)/2.)*exp(-((y(j)/7.62)**2)
     1/2.)*xp
      v(i,j)=-0.2188*(2.-(xp/76.2)**2)*exp(-((xp/76.2)**2)/2.)*
     1erfjk(abs(y(j))/107.76)*y(j)/abs(y(j))
      else
      u(i,j)=0.
      v(i,j)=0.
      endif
 1    continue

      return
      end

c*---------------------------------------------------------------------
      subroutine depth(iun)
c*---------------------------------------------------------------------

      include 'param.h'

      common/ref/ d(ixr,iyr),u(ixr,iyr),v(ixr,iyr),m,n,dx,dy,itype
      common/ind/ iu,ntype,icur,ibc,ispace,nd,iff(3),isp,iinput,
     1iwave,nfreqs,freqs,tide,nwavers,amp,dir,edens
      common/dims/ x(ixr),y(iyr)
      dimension iun(3)
      dimension amp(ncomp,ncomp),dir(ncomp,ncomp),tide(ncomp),
     1          freqs(ncomp),edens(ncomp),nwavs(ncomp)


      write(iun(3),1)
  1   format(' ***********  parabolic model in rectangular',
     1 ' grid  **********'//
     2 ' input type of bottom desired'//
     3 ' 1=surface piercing island'/
     4 ' 2=bbr, submerged shoal'/
     5 ' 3=arthur rip current'/
     6 ' 4=test case, planar bottom'/
     7 ' 5=radder(1979), configuration 2'/
     8 ' 6=grazing incidence on linear caustic'/
     9 ' 7=whalin''s channel'/
     1 ' 8=surface piercing breakwater'/
     2 ' 9=channel'/
     3 '10=breakwater')
      read (*,*) itype

      if ( itype .eq. 1) then
c*-----------------------------------------------------------------
c*    surface piercing island
c*-----------------------------------------------------------------
      write(iun(3),101)
 101  format(' surface piercing island')
      write(iun(3),102)
 102  format(' input m,n,dx,dy,depth,period')
      read (*,*) m,n,dx,dy,dep,t
      write(iun(3),*)m,n,dx,dy,dep,t
      write(iun(3),103)
 103  format(' input crest height, x semiaxis, y semiaxis')
      read (*,*) hb, xa, ya
      write (iun(3),*)hb,xa,ya
      sig=2.*3.1415927/t
      do 104 i=1,m
      x(i)=float(i-1)*dx
 104  continue
      do 105 j=1,n
      y(j)=(float(j-1)-0.5)*dy
 105  continue
      xc=xa+3.*dx
      do 106 i=1,m
      do 106 j=1,n
      d(i,j)=dep-(1.-sqrt((((x(i)-xc)/xa)**2)+((y(j)/ya)**2)
     1))*hb
      if(d(i,j).gt.dep)d(i,j)=dep
 106  continue
      end if

      if ( itype .eq. 2) then
c*-------------------------------------------------------------------
c*    bbr, submerged shoal
c*-------------------------------------------------------------------
      iu=1
      m=100
      n=100
      c20=cos(20.*3.1415927/180.)
      s20=sin(20.*3.1415927/180.)
      dx=0.25
      dy=0.25
      do 151 i=1,m
 151  x(i)=float(i-1)*dx
      do 152 j=1,n
 152  y(j)=float(j-1)*dy
      do 154 i=1,m
      do 154 j=1,n
      xp=(x(i)-10.5)*c20-(y(j)-10.)*s20
      yp=(x(i)-10.5)*s20+(y(j)-10.)*c20
      test=((yp/4.)**2)+((xp/3.)**2)
      if(xp.lt.(-5.84))d(i,j)=0.45
      if(xp.ge.(-5.84))d(i,j)=0.45-0.02*(xp+5.84)
      if(test.gt.1) go to 153
      d(i,j)=d(i,j)-(0.5*sqrt(1.-((yp/5.)**2)-((xp/3.75)**2))
     1-0.3)
 153  continue
 154  continue
      end if

      if ( itype .eq. 3) then
c*------------------------------------------------------------------
c*    arthur (1952) rip current
c*------------------------------------------------------------------
      iu=1
      icur=1
      dx=5.0
      dy=5.0
      slope=0.02
      m=100
      n=100
      do 201 i=1,m
      x(i)=float(i-1)*dx
 201  continue
      do 202 j=1,n
      y(j)=float(2*j-n-1)*dy/2.
 202  continue
      xm=x(m)
      do 203 j=1,n
      do 203 i=1,m
      d(i,j)=(xm-x(i)+dx)*slope
 203  continue
      endif

      if ( itype .eq. 4) then
c*-------------------------------------------------------------------
c*    planar bottom test case
c*-------------------------------------------------------------------
      write(iun(3),*)' input m,n,dx,dy,depth,period'
      read (*,*) m,n,dx,dy,dep,period
      write(iun(3),*) ' input bottom slope'
      read(*,*) xm
      sig=2.*3.1415927/period
      do 251 i=1,m
      do 251 j=1,n
      d(i,j)=dep-xm*float(i-1)*dx
 251  continue
      do 252 i=1,m
 252  x(i)=float(i-1)*dx
      do 253 j=1,n
 253  y(j)=float(j-1)*dy
      endif

      if ( itype .eq. 5) then
c*------------------------------------------------------------------
c*    radder(1979), configuration 2
c*------------------------------------------------------------------
      write(iun(3),*)' input m,n,dx,dy,depth'
      read (*,*)m,n,dx,dy,dep
      iu=1
      rad=dep/0.116
      dm=0.1379*dep
      e0=(dep-dm)/rad
      ak0=2.*3.1415927/(0.288*rad)
      sig2=9.806*ak0*tanh(ak0*dep)
      sig=sqrt(sig2)
      do 301 i=1,m
      x(i)=float(i-1)*dx
 301  continue
      do 302 j=1,n
      y(j)=float(j-1)*dx
 302  continue
      do 303 i=1,m
      do 303 j=1,n
      r=sqrt(((x(i)-x(ifix(rad/dx)+1))**2.)+((y(j)-y((n+1)/2)
     1)**2.))
      if(r.gt.rad)d(i,j)=dep
c*    if(r.le.rad)d(i,j)=dm+e0*r*r
      if(r.le.rad)d(i,j)=dm+e0*r
 303  continue
      endif

      if ( itype .eq. 6) then
c*-------------------------------------------------------------------
c*    grazing incidence on caustic (kirby and dalrymple, 1983)
c*-------------------------------------------------------------------
      write(iun(3),*)' input m,n,dx,dy,depth,period'
      read(*,*)m,n,dx,dy,dep,per
      pi=3.1415927
      sig=2.*pi/per
      d2=2.*dep
      alph=atan(0.02)
      thet=25.*pi/180.
      b=(d2-dep)/tan(alph)
      tt=tan(thet)
      do 351 j=1,n
      y(j)=float(j-1)*dy
 351  continue
      do 352 i=1,m
      x(i)=float(i-1)*dx
 352  continue
      do 353 i=1,m
      do 353 j=1,n
      if(y(j).lt.(y(n)-x(i)*tt))d(i,j)=dep
      if(y(j).ge.(y(n)-x(i)*tt))d(i,j)=dep+cos(thet)*tan(alph)
     1*(x(i)*tt+y(j)-y(n))
      if(y(j).gt.(y(n)-x(i)*tt+b/cos(thet)))d(i,j)=d2
 353  continue
      endif

      if ( itype .eq. 7) then
c*------------------------------------------------------------------
c*    whalin's channel (1971)
c*------------------------------------------------------------------
      write(iun(3),401)
 401  format(' whalins channel, input wave period')
      read (*,*)period
      write (iun(3),*)period
      m=100
      n=74
      iu=1
      dx=.242424242
      dy=.33866666/4.
      pi=3.1415927
      sig=2.*pi/period
      do 402 i=1,m
 402  x(i)=float(i-1)*dx
      do 403 j=1,n
 403  y(j)=float(j-1)*dy-dy/2.
      do 404 j=2,n-1
      g=sqrt(y(j)*(6.096-y(j)))
      do 404 i=1,m
      if(x(i).lt.(10.67-g))d(i,j)=0.4572
      if((x(i).ge.(10.67-g)).and.(x(i).le.(18.29-g)))d(i,j)=
     10.4572+(10.67-g-x(i))/25.
      if(x(i).gt.(18.29-g))d(i,j)=0.1524
 404  continue
      do 405 i=1,m
      d(i,1)=d(i,2)
      d(i,n)=d(i,n-1)
 405  continue
      endif

      if ( itype .eq. 8) then
c*------------------------------------------------------------------
c*    surface piercing breakwater
c*------------------------------------------------------------------
      write(*,*)' input m,n,dx,dy'
      read(*,*) m,n,dx,dy
c*    breakwater tip radius
      xt=50.  !1.5
      yt=50. !4.
      do 451 j=1,n
      y(j)=float(j-1)*dy
 451  continue
      do 452 i=1,m
      x(i)=float(i-1)*dx
      do 452 j=1,n
      if (x(i).lt.xt) then
        r=sqrt((x(i)-xt)**2+(y(j)-yt)**2)
        dep=.66*r-.37
      else
        dep=.66*abs(y(j)-yt)-.37
      endif
      if(dep.gt..36) dep=.36
      d(i,j)=dep
 452  continue
      endif

      if ( itype .eq. 9) then
c*-----------------------------------------------------------------
c*    generate a pair of breakwaters with rounded heads on each
c*    side of a channel.
c*    xt= beginning location for trunk of breakwater
c*    sl= slope of the sides of the breakwater
c*-----------------------------------------------------------------
      write(*,*) ' input m,n,dx,dy,xt,sl,do'
      read(*,*) m,n,dx,dy,xt,sl,do
      w=float(n-1)*dy
      do 503 i=1,m
         x(i)=float(i-1)*dx
         if (x(i).le.xt) then
            do 501 j=1,n
               y(j)=float(j-1)*dy
               if (j.lt.n/2) then
                 r=sqrt((x(i)-xt)**2+y(j)**2)
                 if(r.eq.0.) then
                   cr=0.
                 else
                   cr=abs((x(i)-xt))/r
                 endif
               dep=sl*r+.05
c*    add bulbous head
c               dep=dep-sl*sqrt(r)*cr
               else
                 r=sqrt((x(i)-xt)**2+(y(j)-w)**2)
                 if(r.eq.0.) then
                     cr=0.
                 else
                     cr=abs((x(i)-xt))/r
                 endif
               dep=sl*r+.05
c    add bulbous head
c              dep=dep  -sl*sqrt(r)*cr
               endif
               if (dep.gt.do)dep = do
               if (dep.lt..05)dep = .05
               d(i,j)=dep
 501      continue
        else
          do 502 j=1,n
              if (j.lt.n/2) then
                 yt=0.
              else
                 yt=w
              endif
            dep=sl*abs(y(j)-yt)+.05
            if (dep.gt.do)dep = do
            d(i,j)=dep
 502      continue
        end if
 503  continue
      endif

      if ( itype .eq. 10) then
c*-----------------------------------------------------------------
c*    generate a breakwater with rounded head
c*    with an orientation alpha  (degrees) to the x axis
c*    xh, yh= locus of breakwater head;
c*    sl= slope of the sides of the breakwater;
c*    do= constant depth section.
c*-----------------------------------------------------------------
      write(*,*) ' input m,n,dx,dy,xh, yh,alpha,sl,do'
      read(*,*) m,n,dx,dy,xh,yh,alpha,sl,do
      al=alpha*3.1415927/180.
      write(*,*)' alpha =',al
      co=cos(al)
      si=sin(al)
      do 551 i=1,m
        x(i)=float(i-1)*dx
           do 551 j=1,n
             y(i)=float(j-1)*dy
             xp=(x(i)-xh)*co+(y(i)-yh)*si
             yp=-(x(i)-xh)*si +(y(i)-yh)*co
c            see if we are in front of the trunk
             if(xp.lt.0.0) then
                r=sqrt(xp*xp+yp*yp)
                dep=sl*r-10.
             else
                dep=sl*abs(yp)-10.
             endif
             if (dep.gt.do) dep=do
             d(i,j)=dep
 551  continue
      endif

      return
      end


c*--------------------------------------------------------------------
c*    error function, hasting's method
c*--------------------------------------------------------------------
      function erfjk(x)
      dimension a(5)
      a(1)=0.254830
      a(2)=-0.284497
      a(3)=1.421414
      a(4)=-1.453152
      a(5)=1.061405
      t=1./(1.+0.327591*x)
      erfjk=1.-exp(-(x**2))*t*(a(1)+t*(a(2)+t*(a(3)+t*(a(4)+t*a(5)))))
      return
      end
