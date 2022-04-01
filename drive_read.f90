      program drive_read
      
      USE structure
      
      implicit none
      
      integer temp_dim, en_dim,jj,iz,kk,unit,rownum,np
      parameter (temp_dim=70, en_dim=2500)
      integer points(en_dim),indice_in_temp(en_dim)

      double precision tini, tfin, tcpu
      double precision temp(temp_dim), en(en_dim)
      double precision kn_cross(en_dim)


      
      type(arrayptr),dimension(en_dim) :: prob_in_temp
!      type(indiceptr),dimension(en_dim) :: indice_in_temp
      
100   format(2i8)
101   format(i8,ES16.8E3)
102   format(i8,ES16.8E3,i8,ES16.8E3)
103   format(2i8,2ES16.8E3)
      
      call cpu_time(tini)
      
      call read_HDU2('table.fits', temp_dim, en_dim, temp, en)

      call open_fits('table.fits', unit)
      open(10, status = 'unknown', file = 'srf.txt')    
      temp=temp*5.11d5*1.16d4
      
      rownum = 0
      write(10,100)temp_dim,en_dim
      do iz = 1, temp_dim
      
         write(10,101)iz,temp(iz)
      	 call read_temp(rownum,unit, en_dim, points, kn_cross, indice_in_temp, prob_in_temp)
      	 call write_temp(en_dim,en,points,kn_cross,indice_in_temp,prob_in_temp)
      
      enddo
      
      close(10)
!      print *, 'In read: points',np,iz,points(1,1)

      call close_fits(unit)

                         
      call cpu_time(tfin)
      tcpu=tfin-tini
      print *, ' '
      print *, 'CPU time (s) =',real(tcpu)
      end program drive_read
