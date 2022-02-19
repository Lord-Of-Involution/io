######################################################
#Makefile for
# drive_SRF
#
#Variables:
# fc= Fortran compiler
# flags = general flags for compilation
# libs = libraries used during linking
# name = root name of the code
# code = Main fortran code
# exec = Binary executable (final output)
# objects = list of subroutines and functions used
#
# Use 'make' to compile everything
# Use 'make clean' to erase all the objects
# and 'make cleanout' to erase OUTPUT files.
#
#fc=ifort
#flags=-O3 -parallel -no-prec-div -shared-intel -xHost -fp-model source -mcmodel=large
#
# This worked on yorp, chandra, etc (UMD)
#flags=-O3 -parallel -no-prec-div -static-intel -xHost -fp-model source -mcmodel=large
#libs=$(HEADAS)/lib/libcfitsio_3.27.so
#
#flags= -O3 -fp-model source
#flags= -O3 -check bounds -fp-model source
#
fc=gfortran
flags=-O3
#libs = -L/Users/gullo/Software/heasoft-6.28/x86_64-apple-darwin18.7.0/lib/ \
#       -lXSFunctions -lcfitsio -lXSModel -lXSUtil -lXS
libs = -L/usr/local/lib/ \
		-lcfitsio
#flags=-march=native -ffast-math -funroll-loops -O3 -finline-limit=600
#flags= -O3 -fbounds-check
#
libs= -lcfitsio
name=drive_read
code=$(name).f
exec=$(name).x
mysrc=my_routines

myobjts= $(mysrc)/read_fits.o                      \
         $(mysrc)/write_fits.o            \


# Compile xstar with all subroutines
$(exec): $(myobjts)
	$(fc) $(flags) $(code) $(myobjts) $(libs) -o $(exec)

# Compile and create objects (XILLVER)
$(myobjts): %.o: %.f
	$(fc) $(flags) -c $< -o $@

# Clean all objects
clean:
	rm -rf $(mysrc)/*.o $(mysrc)/*~
	rm -rf $(exec)

all:
	rm -rf $(exec)
	make $(exec)

# Tue Dec 18 14:13:06 EST 2007
# Javier Garcia
# Modified Dec 28 T. Kallman
