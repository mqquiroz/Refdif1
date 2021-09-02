c*----------------------------------------------------------------------------
c*    indat-convertv25.f
c*
c*    This program reads an old (version 2.4 or earlier) indat.dat file
c*    for ref/dif 1 and converts it to a new (version 2.5 or later) file
c*    with the temporary file name indat.new.  The new file should be
c*    renamed to indat.dat.
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

      include 'param.h'

      dimension md(ixr), dconv(2), iff(3), iun(8)
      dimension freqs(ncomp), edens(ncomp), nwavs(ncomp)
      dimension amp(ncomp,ncomp), dir(ncomp,ncomp), tide(ncomp)

      character*255 fname1,fname2,fname3,fname4,fname5,fname6,fname7,
     1              fname8,fname9,fname10,fname11,fname12,fname13,
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
  
c  Define device number |iun(5)| for reference grid input.  
c  This line must be set during program installation, prior to compilation.   
c  Also construct an |open| statement if needed.

      call infile(fnamein)

      iun(5)=5
      iun(6)=6
      open(unit=iun(5),file=fnamein,status='old')
      open(unit=iun(6),file='indat.new')

      write(iun(6),nml=fnames)
    
c  Read unit numbers from indat.dat.  These are not used in indat.new.
    
      read(iun(5),*)(iun(i),i=1,3)
   
c  Read control data from unit |iun(5)|.
  
      read(iun(5),*) mr,nr
      read(iun(5),*) iu, ntype, icur, ibc
      read(iun(5),*) dxr, dyr, dt
      read(iun(5),*) ispace, nd

      if(ispace.eq.1) then
      read(iun(5),*) (md(i),i=1,mr-1)
      endif

      read(iun(5),*) (iff(i),i=1,3)
    
      read(iun(5),*) isp

      read(iun(5),*) iinput,ioutput

      if (iinput .eq. 1) then

c  Read |iwave|, |nfreqs|.
    
      read(iun(5),*) iwave, nfreqs

      if(iwave.eq.2) then
      read(iun(5),*)thet0
      endif

c  For each frequency, enter the wave period and tidal offset.
    
      do 3 ifreq=1,nfreqs
      read(iun(5),*) freqs(ifreq), tide(ifreq)

    
c  If |iwave = 1|, read the number of discrete components.
    
      if(iwave.eq.1) then
      read(iun(5),*) nwavs(ifreq)
      do 1 iwavs=1,nwavs(ifreq)
      read(iun(5),*) amp(ifreq,iwavs), dir(ifreq,iwavs)
 1    continue
      endif
    
c  If |iwave = 2|, read the parameters for each frequency.
    
      if(iwave.eq.2)then
      read(iun(5),*) edens(ifreq)
      read(iun(5),*)  nwavs(ifreq),nseed
      endif

 3    continue

      endif
    
c  If |iinput = 2|, read in wave period and tidal offset.
    
      if(iinput.eq.2)then
      nfreqs=1
      read(iun(5),*) freqin, tidein
      endif

      write(iun(6),nml=ingrid)

      if (ispace.eq.1) write(iun(6),nml=inmd)

      if (iinput .eq. 1) then

        write(iun(6),nml=waves1a)
        if (iwave .eq. 1) write (iun(6), nml = waves1b)
        if (iwave .eq. 2) write (iun(6), nml = waves1c)

      endif

      if (iinput .eq. 2) write (iun(6), nml = waves2)

      close(iun(6))



      stop


      end
  
