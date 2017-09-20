#!/bin/sh

PROGRAM=trj
GUI=trjwgui

SRCPATH=f90trj

rm $SRCPATH/*.o
rm $SRCPATH/*.mod

#if ["$1" == 'gui']; then 
#    make -f f90trj/Makefile.gefortran
#    mv $SRCPATH/$GUI .
#elif ["$1" == 'gfortran']; then
#    make -f r90trj/Makefile.congui
#    mv $SRCPATH/$PROGRAM .
#else 
    make -C f90trj
    mv $SRCPATH/$GUI .
    mv $SRCPATH/$PROGRAM .
#fi
