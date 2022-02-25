      subroutine write_fits(temp_dim, temp, en_dim, en, srf,skn,indices,points)
      USE structure
      
      implicit none

      integer temp_dim, en_dim, mgi, np
      double precision temp(temp_dim), en(en_dim)
      double precision skn(en_dim,en_dim)
      integer iz,jj,kk, points(en_dim, temp_dim)

      
      type(arrayptr), dimension(en_dim,temp_dim) :: srf
      type(indiceptr), dimension(en_dim,temp_dim) :: indices
      
      
      
100   format(2i8)
101   format(i8,ES16.8E3)
102   format(i8,ES16.8E3,i8,ES16.8E3)
103   format(2i8,2ES16.8E3)

      open(10, status = 'unknown', file = 'srf.txt')
      write(10,100)temp_dim,en_dim
      do iz = 1, temp_dim
         print *,iz
         write(10,101)iz,temp(iz)*5.11d5*1.16d4
         do jj = 1, en_dim
             write(10,102)jj,en(jj),points(jj,iz),skn(jj,iz)
             do kk = 1, points(jj,iz)
!             	print *,SIZE(indices(jj,kk)%indice_data),points(jj,iz)
               np=indices(jj,iz)%indice_data(kk)
!               print *,'iz=',iz,'jj=',jj,'kk=',kk
!               print *,kk,np
               write(10,103)kk,np,en(np),srf(jj,iz)%prob_data(kk)
!               print *,'written!'
             enddo
         enddo

1001  enddo

      close(10)
      return
      end subroutine
