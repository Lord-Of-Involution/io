fc=gfortran
flags=-O3
libs= -lcfitsio
name=drive_read


myobjts = drive_read.o read_fits.o write_fits.o
mymods=  structure.mod

drive_read.x: drive_read.o read_fits.o write_fits.o
	$(fc) $(flags) $(libs) -o $@ $^

structure.mod: structure.f90 
	$(fc) $(flags) -c structure.f90

read_fits.o: read_fits.f90 structure.mod
	$(fc) $(flags) $(libs) -c -o $@ read_fits.f90

write_fits.o: write_fits.f90 structure.mod
	$(fc) $(flags) $(libs) -c -o $@ write_fits.f90

drive_read.o: drive_read.f90 write_fits.f90 read_fits.f90 structure.mod
	$(fc) $(flags) $(libs) -c -o $@ drive_read.f90

.PHONY: clean

clean:
	-rm -f *.o *.mod *~