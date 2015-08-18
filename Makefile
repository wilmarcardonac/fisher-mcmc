FC     	= gfortran
LC     	= $(FC)
EXE    	= fisher
FITSDIR = #/home/wilmar/usr/local/lib/lib
LIBFITS = #cfitsio
INCDIR	= #/home/wilmar/additional-software/Healpix_3.00/includef90
IDIR	= #/home/wilmar/additional-software/Healpix_3.00/include
LIBDIR	= ./lapack-3.5.0	
LDIR	= ./ranlib/lib
F_FL   	= -O3 -I$(INCDIR) -I$(IDIR) -DGFORTRAN -fno-second-underscore -fopenmp -fPIC -g
LIB_FL 	= -L$(LIBDIR) -llapack -lblas -L$(LDIR) -lranlib -lrnglib # -lhpxgif -l$(LIBFITS) -Wl,-R$(FITSDIR)
#####################
OBJ   =  arrays.o fiducial.o functions.o fisher.o

def:	$(OBJ) $(OBJNR) $(OBJODE)
	$(LC) $(F_FL) $(OBJ) $(OBJNR) $(OBJODE) -o $(EXE)  $(LIB_FL)

%.o:	%.f90
	$(FC) $(F_FL) -c $<

%.o:	%.F90
	$(FC) $(F_FL) -c $<

%.o:	%.f
	$(FC) $(F_FL) -c $<

clean :
	rm -f *.o *.mod *.ini *~  fort.* slurm*.out $(EXE)

### put dependencies here ###

fisher.o :	arrays.o functions.o fiducial.o