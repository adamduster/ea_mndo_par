c#######################################################################
c FILE: OUTPUT.F
c#######################################################################
C     Output useful information to the user
C
c#######################################################################
c SUBROUTINE: SUMINPUT
      subroutine suminput(taskid)
c
c     Created by Adam Duster June 1 2016
c
c     Purpose: Summarize the input in the output file
c
      use ivar
      use files
      implicit none

      integer taskid

      integer i

      i = 1

      if (taskid .eq. 0) then
      open(unit=ofhnd, file=ofpath, status="unknown", err=1000)
      write(ofhnd, *) MNDOPath(1:78)
      write(ofhnd, *) 'Hof Weight:', HoFWeight
      write(ofhnd, *) 'Bond Weight: ', BondWeight
      write(ofhnd, *) 'Angle Weight: ', AngWeight
      write(ofhnd, *) 'Dihedral Weight: ', DihedWeight
      write(ofhnd, *) 'Population Size: ' , nPop
      write(ofhnd, *) 'Number of Generations: ', nGen

      write(ofhnd, *) 'The input parameters:'
      write(ofhnd, 48) 'TYP', 'EL', 'OPT','PAR VALUE', 'PERCENT VARIED'
   48 format(A3,' ', A2, ' ', A3, ' ', A17, '   ', A14)
      do i=1, OptNum, 1
         write(ofhnd, 50) OptTyp(i), OptEl(i), 1, OptPar(i),
     *                   OptPer(i) * 100
      enddo

      do i=1, StatNum, 1
         write(ofhnd, *) StatTyp(i), StatEl(i), 0, StatPar(i),
     *                   0
      enddo
   50 format(I3,' ',I2,' ',I3, ' ', f17.10, '   ', f5.2,'%')
      write(ofhnd, 100)
      close(ofhnd)
  100 format(/,80('-'),/)
      endif
      return
 1000 print *, 'Could not open output file'
      stop 'suminput 1'
      end subroutine suminput
c#######################################################################
c SUBROUTINE: OUTPUTBESTSTRING
      subroutine OutputBestString(ctx, taskid)
c
c     Created by Adam Duster June 1 2016
c
c     Purpose: Output the best string file for the parametrization run
c
      use files
      implicit none
      include 'pgapackf.h'

      integer ctx, taskid

      integer BestString

      if (taskid .eq. 0) then
          BestString = PGAGetBestIndex(ctx, PGA_OLDPOP)
          call WriteMNDOPar(ctx, BestString, PGA_OLDPOP,
     *                      bestpath, besthnd)
      endif

      end subroutine OutputBestString
