c#######################################################################
c#
c#    Semi-empirical genetic algorithm parallelization program
c#
c#    Written by Adam Duster - June 1 2016
c#
c#    Find optimal parameters for Semi-empirical methods using MNDO and
c#    the PGAPack Parallel Genetic Algorithm Library
c#
c#######################################################################

c#######################################################################
c     PROGRAM SEGAPP
c#######################################################################
      program segapp
c
c  Created by: Adam Duster
c
c  Usage: This is the main driver for the program
c
      use ivar
      use files
      use ifport
      implicit none
      include 'pgapackf.h'
      include 'mpif.h'
      integer ctx, ierror, locseed
      integer numtasks, taskid

c     functions
      double precision, external :: evaluate

c Initialize MPI
      call MPI_INIT(ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD, taskid, ierror)
c Initialize module variables to their default values
      call InitVars()
c Read the user input file
      call GetUserInput(taskid)
c Print the user input file to the output file
      call SumInput(taskid)
      call MPI_BARRIER(MPI_COMM_WORLD, ierror)
c Create the 'context' variable
      ctx = PGACreate(PGA_DATATYPE_REAL, OptNum, PGA_MINIMIZE)
c Set the Random seed
      call PGASetRandomSeed(ctx, rseed)
c Set the probability of winning the tournament
      call PGASetPTournamentProb(ctx, 0.7)
c Now we wish to set the parameters with ranges specified by their type
      call InitPercent(ctx)
c Set the population size to 1 for debugging
      call PGASetPopSize(ctx, nPop)
c Set stopping Criteria for the simulation
      call PGASetMaxGAIterValue(ctx, nGen)
c Stop now after 1 generation for debugging purposes
      call PGASetStoppingRuleType(ctx,PGA_STOP_MAXITER)
c Set the frequency value for printing output
      call PGASetPrintOptions(ctx, PGA_REPORT_STRING, PGA_TRUE)
c Each thread needs its own folder, copy of binary, etc...
c      note the cwd of each process is now in the tid000x folder
      call CreateEvalEnvironment(taskid)
c Finalize the PGA setup
      call PGASetup(ctx)
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      call PGAEvaluate(ctx, PGA_OLDPOP, evaluate, NULL)
      call PGARun(ctx, evaluate)
      call MPI_BARRIER(MPI_COMM_WORLD, taskid, ierror)
c      if (taskid .eq. 0) then
c         ierror = chdir(trim(MainCWD))
c         call OutputBestString(ctx, taskid)
c      endif
      call MPI_BARRIER(MPI_COMM_WORLD, taskid, ierror)
      call PGADestroy(ctx)
      call MPI_FINALIZE(ierror)
      goto 9999

 9999 continue
      end program segapp
