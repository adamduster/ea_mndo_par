c#######################################################################
c FILE: INPUT.F
c#######################################################################
C     This source code file contains subroutines for getting input from
C     the user
C
c#######################################################################
c SUBROUTINE: GETUSERINPUT
      subroutine GetUserInput(taskid)
c
c     Written by Adam Duster June 1 2016
c
c     Purpose: Get the user input from the supplied input file
c

      use ivar
      use files
      use String_Utility
      implicit none
      include 'mpif.h'
      integer taskid

      integer istat, i, j
      character*80 :: words(10)
      character*120 :: line

c     temp variables for holding read data
      integer ttyp, tel, topt
      double precision tpar, tper
c     integer ierr, fh, info
c
c initialize variables
c
      istat = 0
c
c begin program
c
c      fh = ifhnd
c      info = MPI_INFO_NULL
c      call MPI_FILE_open(MPI_COMM_WORLD, ifpath, MPI_MODE_RDONLY,
c     *                   info, fh, ierr)
c      if (taskid .eq. 0) then
c     READ THE FIXED LINES INTO THE INPUT FILE
c       The fixed lines must be in this order
c             1. mndopath
c             2. hofweight
c             3. bondweight
c             4. angleweight
c             5. dihedweight
      open(unit=ifhnd, name=ifpath, status='old', err=4000)
      read(ifhnd, '(a)', err=2000) MNDOPath
      read(ifhnd, '(a)', err=2000) ScrPath

      do while (istat .eq. 0)
          read(ifhnd, '(a)') line
          read(line, *, err=2000) words(1)
          words(1) = StrLowCase(words(1))
          if(words(1)(1:9) .eq. 'hofweight') then
              read(line,*) words(1), HoFWeight
          elseif(words(1)(1:10) .eq. 'bondweight') then
              read(line,*)  words(1), BondWeight
          elseif(words(1)(1:9) .eq. 'angweight') then
              read(line,*)  words(1), AngWeight
          elseif(words(1)(1:9) .eq. 'dihweight') then
              read(line,*)  words(1), DihedWeight
          elseif(words(1)(1:4) .eq. 'seed') then
              read(line,*)  words(1), rSeed
          elseif(words(1)(1:4) .eq. 'ngen') then
              read(line,*) words(1), nGen
          elseif(words(1)(1:4) .eq. 'npop') then
              read(line,*) words(1), nPop

c     READ THE PARAMETERS INTO THE INPUT FILE
c
c     NO MORE INPUT WILL BE GATHERED AFTER THESE LINES
c
c     i is counter for the parameters to be optimized
c     j is counter for the parameters to be staic
c
          elseif(words(1)(1:10) .eq. 'parameters') then
              i = 1
              j = 1
              read(ifhnd, FMT='(A)', IOSTAT=istat) line
              do while (istat .eq. 0)
                  read(line,*) ttyp, tel, topt, tpar, tper
                  if (topt .eq. 1) then
                      OptTyp(i) = ttyp
                      OptEl(i) = tel
                      OptPar(i) = tpar
                      OptNum = OptNum + 1
                      OptPer(i) = tper / 100.0d0
                      i = i + 1
                  elseif (topt .eq. 0) then
                      StatTyp(j) = ttyp
                      StatEl(j) = tel
                      StatPar(j) = tpar
                      StatNum = StatNum + 1
                      j = j + 1
                  endif
                  read(ifhnd, FMT='(A)', IOSTAT=istat) line
              enddo
          elseif (words(1) .eq. "") then
              cycle
c
c The input is not recognized
c
          else
              goto 3000
          endif
      enddo
      npar = OptNum + Stat Num
c      endif
c      call MPI_FILE_CLOSE(fh, ierr)

c     Successful termination of the subroutine :)
      return
 1000 print *, 'Error opening input file'
      stop 'getuserinput 1'
 2000 print *, 'Error reading line to words variable'
      stop 'getuserinput 2'
 3000 print *, 'Error reading variable ', words(1)
      stop 'getuserinput 3'
 4000 print *, 'Error opening input file'
      stop 'getuserinput 4'
      end subroutine GetUserInput
c#######################################################################
c SUBROUTINE: BcastINput
      subroutine BcastInput
c
c     Written by Adam Duster June 1 2016
c
c     Purpose: Broadcast input variables to MPI threads
c
      use ivar
      implicit none
      include 'mpif.h'
c     Brodcast the information
      call MPI_Bcast(mndopath, maxpath, MPI_CHARACTER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(hofweight, 1, MPI_DOUBLE_PRECISION, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(bondweight, 1, MPI_DOUBLE_PRECISION, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(angweight, 1, MPI_DOUBLE_PRECISION, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(dihedweight, 1, MPI_DOUBLE_PRECISION, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(dihedweight, 1, MPI_DOUBLE_PRECISION, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(rseed, 1, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(OptTyp, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(OptEl, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(OptPar, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(OptPer, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(OptNum, 1, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(StatTyp, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(StatEl, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(StatPar, maxpar, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(StatNum, 1, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      call MPI_Bcast(npar, 1, MPI_INTEGER, 0,
     *               MPI_COMM_WORLD)
      end subroutine BcastInput
