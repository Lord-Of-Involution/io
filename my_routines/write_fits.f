      subroutine write_fits(temp_dim, temp, en_dim, en, srf,skn, indices,points  !inp
     1                            )                     !inp
c
c
      implicit none
      integer temp_dim, en_dim, mgi, np
      double precision temp(temp_dim), en(en_dim)
      double precision skn(en_dim,en_dim)
      integer iz,jj,kk, points(en_dim, temp_dim)
c

      type :: arrayptr
        double precision,pointer :: prob_data(:)
      end type arrayptr
      
      type :: indiceptr
        integer,pointer :: indice_data(:)
      end type indiceptr
      
      type(arrayptr), dimension(en_dim,temp_dim) :: srf
      type(indiceptr), dimension(en_dim,temp_dim) :: indices
      
      
      
100   format(2i8)
101   format(i8,ES16.8E3)
102   format(i8,ES16.8E3,i8,ES16.8E3)
103   format(2i8,2ES16.8E3)

      open(10, status = 'unknown', file = 'srf.txt')
      write(10,100)temp_dim, en_dim

      do 1001 iz = 1, temp_dim
         write(10,101)iz,temp
         do jj = 1, en_dim
             write(10,102)jj,en(jj),points(jj,iz),skn(jj,iz)
             do kk = 1, points(jj,iz)
               np=indices(jj,kk)%indice_data(kk)
               write(10,103)kk,np,en(np),srf(jj,iz)%prob_data(kk)
             enddo
         enddo
c
1001  continue
c
      close(10)
      return
      end subroutine
