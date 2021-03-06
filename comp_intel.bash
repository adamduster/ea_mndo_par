#!/usr/bin/env bash
##############################################################################
# Make the program
##############################################################################


FC=mpiifort
OBJDIR=`pwd`/obj
MODDIR=`pwd`/mod
SRCDIR=`pwd`/src
EXEDIR=`pwd`/exe

cd src

CF="$FC -i8 -g -check all -traceback -O0 -c"
OF="$FC -i8 -g -O0 -check all -L/home/aduster/apps/pgapack-1.1.1/lib/linux -lpgaO -o"

echo "Successfuly compiled all Files"

$CF module.F
$CF main.F
$CF input.F
$CF initvar.F
$CF output.F
$CF evaluate.F
#$CF String_Utility.f90

mv *.o $OBJDIR
mv *.mod $MODDIR

cd $OBJDIR

#$OF segapp String_Utility.o module.o main.o input.o initvar.o output.o evaluate.o /home/aduster/apps/pgapack-1.1.1/lib/linux/*.o
$OF segapp module.o main.o input.o initvar.o output.o evaluate.o /home/aduster/apps/pgapack-1.1.1/lib/linux/*.o

echo "Successfuly linked all files"
mv segapp ../exe/segapp
