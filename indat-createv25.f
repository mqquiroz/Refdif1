c*----------------------------------------------------------------------------
c*    indat-createv25.f
c*
c*    This program generates an indat.dat file by asking the operator a
c*    series of questions.  This file is intended to make life a little 
c*    easier - its function is just as easily carried out manually if you
c*    are used to the form of the indat.dat file.
c*
c*    James T. Kirby
c*    Center for Applied Coastal Research
c*    University of Delaware
c*    Newark, DE 19716
c*
c*    (302) 831-2438, FAX (302) 831-1228, kirby@coastal.udel.edu
c*
c*    Last revision 12/22/94.
c*
c*----------------------------------------------------------------------------

      program indatcreate

      include 'param.h'

      dimension md(ixr), dconv(2), iff(3)
      dimension freqs(ncomp), edens(ncomp), nwavs(ncomp)
      dimension amp(ncomp,ncomp), dir(ncomp,ncomp), tide(ncomp)

      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1             fname8,fname9,fname10,fname11,fname12,fname13,
     1              fname14,fname15,fnamein

      data fname1 /'refdat.dat'/, fname2 /'outdat.dat'/,
     1     fname3/'subdat.dat'/,fname4/'wave.dat'/,
     1     fname5/'owave.dat'/,fname6/'surface.dat'/,
     1     fname7/'bottomu.dat'/,fname8/'angle.dat'/,
     1     fname9/' '/,fname10/'refdif1.log'/,
     1     fname11/'height.dat'/,fname12/'sxx.dat'/,
     1     fname13/'sxy.dat'/,fname14/'syy.dat'/,
     1     fname15/'depth.dat'/


      namelist /ingrid/ mr, nr, iu, ntype, icur, ibc, dxr, dyr, dt, 
     1                  ispace, nd, iff, isp, iinput, ioutput
     1         /inmd/  md
     1         /fnames/ fname1,fname2,fname3,fname4,fname5,fname6,
     1                  fname7,fname8,fname9,fname10,fname11,fname12,
     1                  fname13,fname14,fname15
     1         /waves1a/ iwave, nfreqs
     1         /waves1b/ freqs, tide, nwavs, amp, dir
     1         /waves1c/ thet0, freqs, tide, edens, nwavs, nseed    
     1         /waves2/ freqin, tidein
  

      call infile(fnamein)

      open(unit=10,file=fnamein)

      write(*,*)' enter name for .dat file containing reference grid in',
     1' single quotes'
      read(*,*) fname1

      write(*,*)' enter name for ouput data file'
      read(*,*) fname2

      write(10,nml=fnames)
       
c  Enter control data.

      write(*,*)' enter grid dimensions mr, nr'
      read(*,*) mr,nr

      write(*,*)' enter grid spacings dxr, dyr and depth tolerance dt'
      read(*,*) dxr, dyr, dt

      write(*,*)' input iu: 1=mks, 2=english' 
      read(*,*) iu
      
      write(*,*)' input dispersion relationship; ntype: 0=linear,'
      write(*,*)'                          1=composite, 2=stokes'
      read(*,*)ntype

      write(*,*)' input lateral boundary condition; ibc: 0=closed'
      write(*,*)'                                        1=open'
      read(*,*) ibc	

      write(*,*)' input ispace (0=program picks x spacing,',
     1'  1=user chooses)'
      read(*,*) ispace

      write(*,*)' input nd (# y divisions, 1 is minimum)'
      read (*,*) nd

      if(ispace.eq.0) go to 105

      write(*,*)' constant or variable x spacing?(0 for constant)'
      read(*,*) ians1

      if(ians1.eq.0) then
      write(*,*) ' input constant md'
      read(*,*) mdc
      do 103 iko=1,mr-1
      md(iko)=mdc
 103  continue
      else
      write(*,104)
 104  format(' input md(i) for i=1 to mr-1')
      read(*,*) (md(i),i=1,mr-1)
      endif

 105  write(*,106)
 106  format(' input if(1) turbulent, if(2) porous, if(3) laminar')
      write(*,*) '  standard  choice:  1, 0, 0'
      read(*,*) iff(1), iff(2), iff(3)

      write(*,*)' input isp (subgrid features) :standard  0'  
      read(*,*) isp

      write(*,108)
 108  format(' input values of iinput, ioutput:'/
     1' iinput: 1 standard, i.e., not starting from previous run'/
     1'         2 if starting from previous run'/
     1' ioutput: 1 standard, not saving restart data'/
     1'          2 if saving restart data')
      read(*,*)iinput,ioutput

      write(*,115)
 115  format(' input value of isurface:'/
     1'  isurface = 0:   no surface picture generated'/
     1'  isurface = 1:   surface picture generated')
      read(*,*) isurface

      if(isurface.eq.0) fname6 = ' '

      write(10,nml=ingrid)

      if(ispace.eq.1) write(10,nml=inmd)

      if(iinput.eq.1) then
c*----------------------------------------------------------------------
c*    write waves1 portion of indat.dat
c*----------------------------------------------------------------------
      write(*,*)' input iwave (1 discrete, 2 directional spread)'
      read(*,*) iwave

      write(*,*) ' input nfreq (# of frequencies)' 
      read(*,*) nfreqs

      write(10, nml=waves1a)

      if (iwave.eq.2) then
      write(*,*)' enter central direction thet0'
      read(*,*) thet0
      endif

      do 113 ifreq=1,nfreqs
c*----------------------------------------------------------------------
c*    line 10, iinput=1
c*----------------------------------------------------------------------
      write(*,109)
 109  format(' input wave period and tide stage')
      read(*,*) freqs(ifreq), tide(ifreq)

c*----------------------------------------------------------------------
c*    line 11, iwave=1, iinput=1
c*----------------------------------------------------------------------
      if(iwave.eq.1) then

      write(*,110)
 110  format(' input # of waves per frequency, nwavs')
      read(*,*) nwavs(ifreq)

c*----------------------------------------------------------------------
c*    line 12, iwave=1, iinput=1
c*----------------------------------------------------------------------
      do 111 iwavs=1,nwavs(ifreq)
      write(*,*)' input amplitude and direction'
      read(*,*) amp(ifreq,iwavs), dir(ifreq,iwavs)

 111  continue

      else

c*---------------------------------------------------------------------
c*    iwave=2, iinput=1
c*---------------------------------------------------------------------
      write(*,112)
 112  format('input en. density and on next line, directional',
     1'  spreading factor')
      read(*,*) edens(ifreq)
      read(*,*) nwavs(ifreq)

      nseed=500

      endif
 113  continue 

      if (iwave .eq. 1) write(10, nml=waves1b)
      if (iwave .eq. 2) write(10, nml=waves1c) 

      endif

      if ( iinput .eq. 2) then
c*---------------------------------------------------------------------
c*    line 9, iinput=2
c*---------------------------------------------------------------------
      write(*,*)' input wave period and tide stage'
      read(*,*) freqin ,tidein

      write(10, nml=waves2)

      endif

      close(10)
  
      stop
      end
  
