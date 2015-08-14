#!/bin/bash
#
#mkdir temp
#cd temp
#rm *
#f90split ../ranlib.f90
#
for FILE in `ls -1 *.f90`;
do
  gfortran -c $FILE
  if [ $? -ne 0 ]; then
    echo "Errors compiling " $FILE
    exit
  fi
done
#rm *.f90
#
ar qc libranlib.a *.o
rm *.o
#
mv libranlib.a ../../lib/
#cd ..
#rmdir temp
#
#echo "Library installed as ~/lib/$ARCH/libranlib.a."
