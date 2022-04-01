      subroutine write_temp(en_dim,en,points,kn_cross,indice_in_temp,prob_in_temp)
      USE structure
      
      implicit none

      integer en_dim,np,jj,kk
      double precision en(en_dim),kn_cross(en_dim)
      integer points(en_dim),indice_in_temp(en_dim)
      
      type(arrayptr), dimension(en_dim) :: prob_in_temp
      
      
102   format(i8,ES16.8E3,i8,ES16.8E3)
103   format(2i8,2ES16.8E3)


      	 do jj = 1, en_dim
             write(10,102)jj,en(jj),points(jj),kn_cross(jj)
             np=indice_in_temp(jj)
             do kk = 1, points(jj)
               write(10,103)kk,np,en(np),prob_in_temp(jj)%prob_data(kk)
               np=np+1
             enddo
             deallocate(prob_in_temp(jj)%prob_data)
         enddo

      return
      end subroutine
!--------------------------------------------------------------------------

