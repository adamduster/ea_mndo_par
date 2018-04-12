c#######################################################################
c FILE: INITVAR.F
c#######################################################################
C     This source code file contains subroutines for initializing the
C     program variables
C
c#######################################################################
c SUBROUTINE: INITVARS
      subroutine InitVars
c
c     Created by Adam Duster June 1 2016
c     Purpose : Initialize all of the modules with their initial values
c
      implicit none

      call InitIvar()

      end subroutine InitVars
c#######################################################################
c SUBROUTINE: INITIVAR
      subroutine InitIvar
c
c     Created by Adam Duster June 1 2016
c     Purpose : Initialize the ivar module with default values
c
      use ivar
      use ifport
      implicit none

      integer ierr

      npar = 0

      ngen = 10
      npop = 6

      StatTyp = 0
      StatEl = 0
      StatPar = 0.0d0
      StatNum = 0

      OptTyp = 0
      OptEl = 0
      OptPar = 0.0d0
      OptNum = 0

      MNDOPath = ""

      HoFWeight = 0.0d0
      BondWeight = 0.0d0
      AngWeight = 0.0d0
      DihedWeight = 0.0d0

      ierr =  GetCWD(MainCWD)

      end subroutine InitIvar
c#######################################################################
c SUBROUTINE: INITPERCENT
      subroutine InitPercent(ctx)
c
c     Created by Adam Duster June 1 2016
c     Purpose : Initialize the ctx variable to generate population
c               based on gaussian distribution about a the parameter
c               range
c
      use ivar
      implicit none
      include 'pgapackf.h'
      include 'mpif.h'
      integer ctx, strLen, i
      double precision :: median(OptNum), percent(OptNum)

      strLen = PGAGetStringLength(ctx)
      if (OptNum .ne. strLen) then
         goto 1000
      endif

      do i=1, strLen
         median(i) = OptPar(i)
         percent(i) = OptPer(i)
      enddo

      call PGASetRealInitPercent(ctx, median, percent)

c     Successful Termination of Subroutine
      return
 1000 print *, 'Fatal Error. OptPar =! strLen of a Population Member'
      stop 'InitPercent 1'
      end subroutine InitPercent
