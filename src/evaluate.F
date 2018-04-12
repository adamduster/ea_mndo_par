c#######################################################################
c FILE: INPUT.F
c#######################################################################
C     This source code file contains subroutines for getting evaluating
c     the parameters
C
c#######################################################################
c#######################################################################
c SUBROUTINE: CREATEEVALENVIRONMENT
      subroutine CreateEvalEnvironment(taskid)
c
c     Created by Adam Duster June 1 2016
c     Purpose : Each task should have its own copy of the MNDO Binary
c     for HPC purposes
c
c     Additionally, each task needs its own folder for calculations
c     and its own mndo input file for evaluations
c
      use ifport
      use ivar
      use files
      implicit none
c      include 'mpif.h'
      integer taskid, ierr

      character*50 folder
      character*5 tid
      character*200 :: cwd
      character*400 syscall
      character*300 :: cppath

      ierr = 0
      write(folder, '(A1,I0.5)') 'p', taskid
      ierr = chdir(trim(SCRPath))
      if (ierr .ne. 0) goto 2000
      ierr = makedirqq(trim(folder))
      ierr = chdir(trim(folder))
      ierr = GetCWD(cwd)
      if (ierr .ne. 0) then
         goto 1000
      endif
      cppath = trim(MainCWD) // '/' // refpath
      syscall = 'cp -f ' // cppath // ' ' // trim(cwd)
      call execute_command_line(syscall)
      syscall = 'cp -f ' // trim(mndopath) // ' ' // trim(cwd)
      call execute_command_line(syscall)

      return
 1000 print *, 'Failure for thread to enter directory'
      stop 'CreateEvalEnvironment 1'
 2000 print *, 'Error accessing temporary directory'
      stop 'CreateEvalEnvironment 2'
      end subroutine CreateEvalEnvironment
c#######################################################################
c FUNCTION: WRITEMNDOPAR
      subroutine WriteMNDOPar(ctx, p, pop, iname, ihnd)
c
c     Created by Adam Duster June 3 2016
c     Purpose :  Write the fort.14 file with the set of parameters
c                passed to this function
c
c
      use files
      use ivar
      implicit none
      include 'pgapackf.h'

      integer ctx, p, pop
      integer ihnd
      character*(*) :: iname

      integer i
      double precision allele

      open(file=iname, unit=ihnd, status='replace')

c
c  Write the parameters from the genetic algorithm
c
      do i=1, OptNum
         allele = PGAGetRealAllele (ctx, p, pop, i)
         write(ihnd, 100) OptTyp(i), OptEl(i), allele
  100 format(I3,x,I2,x,f17.10)
      enddo

c
c  Write the parameters that are not to be optimized
c
      do i=1, StatNum
         write(ihnd, 100) StatTyp(i), StatEl(i), StatPar(i)
      enddo
      close(ihnd)


      end subroutine WriteMNDOPar
c#######################################################################
c FUNCTION: RUNMDNO
      subroutine RunMNDO
c
c     Created by Adam Duster June 3 2016
c     Purpose : Run MNDO
c
      implicit none

      call execute_command_line('./mndo99 < mndo.inp > mndo.out')
      end subroutine RunMNDO
c#######################################################################
c FUNCTION: READMNDO(fitness)
      subroutine ReadMNDO(fitness, fail)
c
c     Created by Adam Duster June 3 2016
c     Purpose : Read the mndo file nb.50 and get the error from each
c               line
c
c               Based on the error, multiply it by the proper weighting
c               factor
c
c               Return the final Sum of Square Errors as the fintess
c               function
c
      use ivar
      use files
      implicit none
c
c I/O Variables
c
      double precision :: fitness
      logical :: fail
c
c Local Variables
c
      integer ioerr
c
c THESE VARIABLES MUST BE THE SAME AS IN THE FORT.50 MNDO FILE
c
c   ID 1 tells if HOF, BOND, ANGLE, DIHEDRAL,
c   ID 2
c   Comment is the comment at the end of the mndo input
c   Difference is the sum of square errors difference
      integer(kind=4) id, id2
      character(len=30) :: comment
      real(kind=8) :: w, diff
c
c Begin program
c
      fitness = 0.0d0
      ioerr = 0
      fail = .false.

      open(file=fitpath, unit=fithnd, form="unformatted", status='old')

c
c This is adapted from v1 of Xin Wu's PPP program
c
      read(fithnd,iostat=ioerr) id, id2, w, diff, comment
      do while (ioerr .eq. 0)
c
c This indicates failure for SCF to converge
c
         if (id2.le.-91) then
           fail=.true.
           return
         endif
c
c Multiply the difference by the individual weight
c
         diff = w * diff
c
c Add the resulting weight to the Fitness
c
         if (id .eq. HoFId) then
            fitness = (diff * HoFWeight) ** 2 + fitness
         elseif (id .eq. BndId) then
            fitness = (diff * BondWeight) ** 2 + fitness
         elseif (id .eq. AngId) then
            fitness = (diff * AngWeight) ** 2 + fitness
         elseif (id .eq. DihId) then
            fitness = (diff * DihedWeight) **2 + fitness
         elseif (id .eq. CNormId) then
            continue
         else
            goto 1000
         endif
         read(fithnd,iostat=ioerr) id, id2, w, diff, comment
      enddo
      close(fithnd)

      return
 1000 print *, 'parameter not supported'
      stop 'ReadMNDO 1'
      end subroutine ReadMNDO
c#######################################################################
c FUNCTION: EVALUATE:
      double precision function evaluate(ctx, p, pop)
c
c     Created by Adam Duster June 3 2016
c     Purpose : This is the major function of the program.
c
c     Run an MNDO calculation with an input parameter set and read the
c     output to find the error
c
c     Weight the error
c
c     Return the sum of squared errors

      use files
      implicit none
      include  'pgapackf.h'

      integer ctx, p, pop
      double precision fitness
      logical fail

c     Write the fort.14 file
      call WriteMNDOPar(ctx, p, pop, parpath, parhnd)

      call RunMNDO

      call ReadMNDO(fitness, fail)

      if (fail .eqv. .false.) then
          evaluate = fitness
      else
          evaluate = huge(evaluate)
      endif
      open(unit=66,file='test')
         write(66, *) evaluate
      close(66)
      end function evaluate
