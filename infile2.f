c*---------------------------------------------------------------------------
c*  infile2.f
c*
c*  Provide code for reading the namelist file name from the command line
c*  for refdif 1 or refdifs.
c*
c*  igetarg provided on SGI by the link to liblrss, provided by SAIC.
c*
c*  James T. Kirby, October 10, 1994.
c*
c*  LRSS mods made November 22, 1994 by Kurt Schmitt at SAIC 
c*
c*  Last revision 11/26/94
c*---------------------------------------------------------------------------

      subroutine infile(fnamein)
      character*255 fnamein2,fnamein
      integer igetarg
      external igetarg
      character*255 input_namelist
      namelist/lrss_name/fnamein2

c      call lrss_setup

c*   read the filename

      iret=igetarg(1,input_namelist,255)

      if(iret.eq.-1) then
          iret =
     1    error_write('no namelist filename specified on command line')
          call exit(1)
      endif

       open(unit=10,file=input_namelist)
       read(10,lrss_name)
       close(10)
       fnamein=fnamein2
      return
      end

