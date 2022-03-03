MODULE structure
    implicit none
    
      type :: arrayptr
        double precision,pointer :: prob_data(:)
      end type arrayptr
      
      
      type :: indiceptr
        integer,pointer :: indice_data(:)
      end type indiceptr
      
END MODULE structure
