program main
   use iso_c_binding
   implicit none
   character(len=*), parameter    :: filename="test.npy"
   character(len=:), allocatable  :: header, descr
   integer, allocatable           :: dims(:)
   integer                        :: stat
   logical                        :: fortran_order

   open(unit=17,file=filename, access="stream", form="unformatted", iostat=stat)
   write (*,*) "status =", stat, __LINE__

   header = get_header(17)
   call parse_header(header, descr, dims, fortran_order)
   close(17)
contains
   function get_header(unitnum) result(header)
      implicit none
      integer, intent(in)             :: unitnum
      character(len=6)                :: magic
      character(len=1)                :: byte_tmp
      character(len=:), allocatable   :: header
      integer                         :: i, major, minor, header_size
      integer(kind=c_int16_t)         :: int16
      integer(kind=c_int32_t)         :: int32

      read(17, iostat=stat) magic
      call check_stat(stat,__LINE__)

      if(magic /= achar(147) // "NUMPY") then
         write (*,*) "not a .npy file"
         stop 
      endif
   
      read(17, iostat=stat) byte_tmp
      call check_stat(stat,__LINE__)
      major = ichar(byte_tmp)
      read(17, iostat=stat) byte_tmp
      call check_stat(stat,__LINE__)
      minor = ichar(byte_tmp)

      if(major == 1) then
         read(17, iostat=stat) int16
         call check_stat(stat,__LINE__)
         header_size = int16
      elseif(major == 2) then
         read(17, iostat=stat) int32
         call check_stat(stat,__LINE__)
         header_size = int32
      endif
      
      allocate(character(header_size) :: header)
      read(17, iostat=stat) header
      call check_stat(stat,__LINE__)
   end function get_header

   subroutine parse_header(header_str, descr, dims, fortran_order)
      implicit none
      character(len=*), intent(in)     :: header_str
      character(len=:), allocatable    :: descr, substr
      integer, allocatable             :: dims(:)
      logical, intent(out)             :: fortran_order

      integer                          :: comma_idx, colon_idx, old_idx
   
      write (*,*) header_str
      comma_idx = index(header_str, ",") - 1
      substr = header_str(2:comma_idx)

      write (*,*) substr
      
      colon_idx = index(substr, ":") + 1
      descr = adjustl(trim(substr(colon_idx:)))
      descr = descr(2:len(descr)-2)

      write (*,*) header_str(comma_idx+2:)
      old_idx = comma_idx+1
      comma_idx = comma_idx + index(header_str(comma_idx+2:), ",") +2
      write (*,*) "comma_idx = ", comma_idx 
      write (*,*) header_str(old_idx:comma_idx)

   end subroutine parse_header

   subroutine check_stat(stat, line)
      implicit none
      integer, intent(in) :: stat, line
      if(stat /= 0) then
         write (*,"('iostat =',I4,' at line ',I4)") stat, line
         stop 7
      endif
   end subroutine

end program main
