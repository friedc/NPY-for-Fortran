program main
   use iso_c_binding
   implicit none
   character(len=*), parameter    :: filename="test.npy"
   character(len=:), allocatable  :: header, descr
   integer, allocatable           :: dims(:)
   logical                        :: fortran_order

   open(unit=17,file=filename, access="stream", form="unformatted")

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

      read(17) magic

      if(magic /= achar(147) // "NUMPY") then
         write (*,*) "not a .npy file"
         stop 
      endif
   
      read(17) byte_tmp
      major = ichar(byte_tmp)
      read(17) byte_tmp
      minor = ichar(byte_tmp)

      if(major == 1) then
         read(17) int16
         header_size = int16
      elseif(major == 2) then
         read(17) int32
         header_size = int32
      endif
      
      allocate(character(header_size) :: header)
      read(17) header
   end function get_header

   subroutine parse_header(header_str, descr, dims, fortran_order)
      implicit none
      character(len=*), intent(in)     :: header_str
      character(len=:), allocatable    :: descr, substr
      integer, allocatable             :: dims(:)
      logical, intent(out)             :: fortran_order

      integer                          :: idx

      idx = index(header_str, ",") - 1
      substr = header_str(2:idx)
      write (*,*) substr
      idx = index(substr, ":") + 1
      descr = adjustl(trim(substr(idx:)))
      descr = descr(2:len(descr)-2)
      write (*,*) "b" // descr // "e"

   end subroutine parse_header
end program main
