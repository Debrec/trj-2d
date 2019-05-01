#!/bin/sh

export LD_LIBRARY_PATH=/home/hernan/netcdf/lib

PROGRAM=trj
GUIGTK=trjgtk

SRCPATH=f90trj

rm $SRCPATH/*.o
rm $SRCPATH/*.mod

make -C f90trj
mv $SRCPATH/$GUIGTK .
mv $SRCPATH/$PROGRAM .

