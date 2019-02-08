program main
   use iso_c_binding
   implicit none
   character(len=*), parameter    :: filename = "test.npy"
   character(len=:), allocatable  :: header, descr
   integer, allocatable           :: dims(:)
   integer                        :: stat
   logical                        :: fortran_order

   open (unit=17, file=filename, access="stream", form="unformatted", iostat=stat)

   header = get_header(17)
   call parse_header(header, descr, dims, fortran_order)
   close (17)
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

      read (17, iostat=stat) magic
      call check_stat(stat, __LINE__)

      if (magic /= achar(147)//"NUMPY") then
         write (*, *) "not a .npy file"
         stop
      endif

      read (17, iostat=stat) byte_tmp
      call check_stat(stat, __LINE__)
      major = ichar(byte_tmp)
      read (17, iostat=stat) byte_tmp
      call check_stat(stat, __LINE__)
      minor = ichar(byte_tmp)

      if (major == 1) then
         read (17, iostat=stat) int16
         call check_stat(stat, __LINE__)
         header_size = int16
      elseif (major == 2) then
         read (17, iostat=stat) int32
         call check_stat(stat, __LINE__)
         header_size = int32
      endif

      allocate (character(header_size) :: header)
      read (17, iostat=stat) header
      call check_stat(stat, __LINE__)
   end function get_header

   subroutine parse_header(header_str, descr, dims, fortran_order)
      implicit none
      character(len=*), intent(in)     :: header_str
      character(len=:), allocatable    :: descr_str, fort_str, shape_str, descr
      integer, allocatable             :: dims(:)
      logical, intent(out)             :: fortran_order

      integer                          :: descr_idx, fort_idx, shape_idx, n_entries

      descr_idx = index(header_str, "'descr':")
      fort_idx = index(header_str, "'fortran_order':")
      shape_idx = index(header_str, "'shape':")

      descr_str = get_keystr(header_str(descr_idx:fort_idx - 1))
      descr_str = descr_str(2:len(descr_str) - 1)

      fort_str = get_keystr(header_str(fort_idx:shape_idx - 1))
      if (fort_str == "True") then
         fortran_order = .True.
      elseif (fort_str == "False") then
         fortran_order = .False.
      else
         stop 8
      endif

      dims = interp_shape(get_keystr(header_str(shape_idx:)))
   end subroutine parse_header

   subroutine check_stat(stat, line)
      implicit none
      integer, intent(in) :: stat, line
      if (stat /= 0) then
         write (*, "('iostat =',I4,' at line ',I4)") stat, line
         stop 7
      endif
   end subroutine

   function get_keystr(input_str) result(out_str)
      implicit none
      character(len=*), intent(in)    :: input_str
      character(len=:), allocatable   :: out_str
      integer                         :: col, last_comma

      col = index(input_str, ":") + 1
      last_comma = index(input_str, ",", back=.True.)

      out_str = adjustl(trim(input_str(col:last_comma)))
      out_str = out_str(1:len(out_str) - 2)
   end function get_keystr

   function interp_shape(shape_str) result(dims)
      implicit none
      character(len=*), intent(in)   :: shape_str
      character(len=:), allocatable  :: work_str, num_str
      integer, allocatable           :: dims(:)
      integer                        :: idx

      work_str = shape_str(2:len(shape_str))
      allocate(dims(0))
      idx = -1

      do while (idx /= 0)
         idx = index(work_str, ",")
         num_str = work_str(1:idx - 1)
         if(len(num_str) > 0) dims = [dims, str2int(num_str)]
         work_str = work_str(idx + 1:len(work_str))
      enddo
   end function interp_shape

   function str2int(str) result(int)
      implicit none
      character(len=*), intent(in) :: str
      integer                      :: int
      integer                      :: stat

      read (str, *, iostat=stat) int
      if (stat /= 0) then
         write (*,*) "str reading failed", str
         stop 9
      endif
   end function str2int

end program main
