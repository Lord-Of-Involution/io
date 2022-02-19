      program drive_read
c........
      implicit none
      integer tempdim, endim
      parameter (tempdim=70, endim=500)
      integer points(endim, tempdim)

      double precision tini, tfin, tcpu
      double precision temp(tempdim), en(endim)
      double precision kn_cross(endim, tempdim)

      type :: arrayptr
        double precision,pointer :: prob_data(:)
      end type arrayptr
      
      type :: indiceptr
        integer,pointer :: indice_data(:)
      end type indiceptr
      
      type(arrayptr),dimension(endim) :: prob_in_temp
      type(indiceptr),dimension(endim) :: indice_in_temp
      
      type(arrayptr), dimension(endim,tempdim) :: srf
      type(indiceptr), dimension(endim,tempdim) :: indices
      
      
      call cpu_time(tini)
      call read_fits('table.fits', tempdim,endim,temp,en,
     1                     points,kn_cross,indices,srf)
     

      call write_fits(tempdim, temp, endim, en, srf,kn_cross,  !inp
     1                     indices,points)                     
      call cpu_time(tfin)
      tcpu=tfin-tini
      print *, ' '
      print *, 'CPU time (s) =',real(tcpu)
c
      end program drive_read
