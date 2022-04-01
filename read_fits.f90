subroutine read_temp(rownum, unit, en_dim, points, kn_cross, indice_in_temp, prob_in_temp)

      USE structure
      
      implicit none
      
      integer np, out_dim, out_ind, rownum, en_dim, unit, points(en_dim),indice_in_temp(en_dim)
      double precision kn_cross(en_dim)

      type(arrayptr),dimension(en_dim) :: prob_in_temp
      
      
      do np = 1, en_dim

            rownum = rownum + 1


            call read_HDU3_col(unit, rownum, kn_cross(np), out_ind, out_dim)

            points(np) = out_dim
            indice_in_temp(np) = out_ind
            
            allocate(prob_in_temp(np)%prob_data(out_dim))
            call read_HDU3_srf(unit, rownum, out_dim, prob_in_temp(np)%prob_data)
      
      enddo
end subroutine       
!-----------------------------------------------------------------------
      subroutine read_HDU2(filename, temp_dim, en_dim, temp, en)
      implicit none
      integer           :: temp_dim, en_dim
      double precision  :: temp(temp_dim), en(en_dim)
      character (len=200) filename

      logical :: anynul
      integer :: status,readwrite,blocksize,unit,chdu,hdutype,colnum,rownum,felem
      
! Initialize status
      status    = 0
      readwrite = 0
      blocksize = 1
! Get an unused Logical Unit Number to use to open the FITS file.
      call ftgiou(unit,status)
! Open the FITS file
      call ftopen(unit,filename,readwrite,blocksize,status)
! Go to the extension with the parameters
      chdu = 2
      call FTMAHD(unit,chdu,hdutype,status)

! Read in the function 
      felem  = 1
      rownum = 1
      colnum = 1
      call ftgcvd(unit, colnum, rownum, felem, temp_dim, 0.0,temp, anynul, status)
      colnum = 2
      call ftgcvd(unit, colnum, rownum, felem, en_dim, 0.0,en, anynul, status)

! The FITS file must always be closed before exiting the program. 
! Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
      call ftclos(unit, status)
      call ftfiou(unit, status)
! Check for any error, and if so print out error messages.
      if (status .gt. 0) call printerror(status)

      return
      end 
!-----------------------------------------------------------------------

      
!-----------------------------------------------------------------------
      Subroutine read_HDU3_col(unit, rownum, kncross, out_ind, out_dim)
      implicit none
      integer           :: unit, rownum, out_ind, out_dim 
      double precision  :: kncross(1)

      logical :: anynul
      integer :: status,  chdu, hdutype,colnum, felem, nelem
      
! Initialize status
      status    = 0
! Go to the extension 3
      chdu = 3
      call FTMAHD(unit,chdu,hdutype,status)

! Read in the function 
      felem  = 1
      nelem  = 1
      colnum = 1
      call ftgcvd(unit, colnum, rownum, felem, nelem, 0.0,kncross, anynul, status)
      if (status .gt. 0) call printerror(status)

      colnum = 2
      call ftgcvj(unit, colnum, rownum, felem, nelem, 0.0,out_ind, anynul, status)
      if (status .gt. 0) call printerror(status)

      colnum = 3
      call ftgcvj(unit, colnum, rownum, felem, nelem, 0.0,out_dim, anynul, status)
! Check for any error, and if so print out error messages.
      if (status .gt. 0) call printerror(status)

      return
      end 
!-----------------------------------------------------------------------
      
!-----------------------------------------------------------------------
      subroutine read_HDU3_srf(unit, rownum, out_dim, en)
      implicit none
      integer           :: unit, rownum, out_dim
      double precision  :: en(out_dim)

      logical :: anynul
      integer :: status, chdu, hdutype, colnum, felem
      
! Initialize status
      status    = 0
! Go to the extension 3
      chdu = 3
      call FTMAHD(unit,chdu,hdutype,status)
      if (status .gt. 0) call printerror(status)

! Read in the function 
      felem  = 1
      colnum = 4
      call ftgcvd(unit, colnum, rownum, felem, out_dim, 0.0,en, anynul, status)

! Check for any error, and if so print out error messages.
      if (status .gt. 0) call printerror(status)

      return
      end 
!-----------------------------------------------------------------------
      
      
!-----------------------------------------------------------------------
      subroutine open_fits(filename, unit)
      implicit none
      integer :: status, unit
      character* (*) filename

      integer :: readwrite, blocksize
      
! Initialize status
      status    = 0
      readwrite = 0
      blocksize = 1
! Get an unused Logical Unit Number to use to open the FITS file.
      call ftgiou(unit,status)
! Open the FITS file
      call ftopen(unit,filename,readwrite,blocksize,status)


! Check for any error, and if so print out error messages.
      if (status .gt. 0) call printerror(status)

      return
      end 
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
      subroutine close_fits(unit)
      implicit none
      integer :: unit, status

! Initialize status
      status    = 0
! The FITS file must always be closed before exiting the program. 
! Any unit numbers allocated with FTGIOU must be freed with FTFIOU.
      call ftclos(unit, status)
      call ftfiou(unit, status)
! Check for any error, and if so print out error messages.
      if (status .gt. 0) call printerror(status)

      return
      end 
!-----------------------------------------------------------------------



      subroutine printerror(status)
      integer status
      character errtext*30,errmessage*80

      if (status .le. 0)return
!C  The FTGERR subroutine returns a descriptive 30-character text string that
!C  corresponds to the integer error status number.  A complete list of all
!C  the error numbers can be found in the back of the FITSIO User's Guide.
      call ftgerr(status,errtext)
      print *,'FITSIO Error Status =',status,': ',errtext
!C  FITSIO usually generates an internal stack of error messages whenever
!C  an error occurs.  These messages provide much more information on the
!C  cause of the problem than can be provided by the single integer error
!C  status value.  The FTGMSG subroutine retrieves the oldest message from
!C  the stack and shifts any remaining messages on the stack down one
!C  position.  FTGMSG is called repeatedly until a blank message is
!C  returned, which indicates that the stack is empty.  Each error message
!C  may be up to 80 characters in length.  Another subroutine, called
!C  FTCMSG, is available to simply clear the whole error message stack in
!C  cases where one is not interested in the contents.
      call ftgmsg(errmessage)
      do while (errmessage .ne. ' ')
          print *,errmessage
          call ftgmsg(errmessage)
      end do
      end
