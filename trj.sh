#!/bin/sh

PROGRAM=trj
GUIGTK=trjgtk
GUIWINDOWS=trjwgui

SRCPATH=f90trj

rm $SRCPATH/*.o
rm $SRCPATH/*.mod

make -C f90trj
#mv $SRCPATH/$GUIGTK .
mv $SRCPATH/$PROGRAM .
mv $SRCPATH/$GUIWINDOWS .

