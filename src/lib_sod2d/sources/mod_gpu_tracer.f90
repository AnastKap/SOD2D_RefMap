#ifdef _USE_NVTX
#  define C_RANGE_PUSH        "nvtxRangePushA"
#  define C_RANGE_PUSH_EX     "nvtxRangePushEx"
#  define C_RANGE_POP         "nvtxRangePop"
#elif defined(_USE_ROCTX)
#  define C_RANGE_PUSH        "rctxRangePushA"
#  define C_RANGE_PUSH_EX     "roctxRangePushEx"
#  define C_RANGE_POP         "roctxRangePop"
#elif defined(_USE_ROCTX)
#endif



#if !(defined(NOACC) || defined(NOOPENMP))
module mod_gpu_tracer

   use iso_c_binding
   use cudafor
   implicit none

   integer, private :: col(7) = [Z'0000ff00', Z'000000ff', Z'00ffff00', Z'00ff00ff', Z'0000ffff', &
                                 Z'00ff0000', Z'00ffffff']
   character(len=256), private :: tempName

   logical, save :: isopen = .false.

   type, bind(C) :: EventAttributes
      integer(C_INT16_T) :: version = 1
      integer(C_INT16_T) :: size = 48 !
      integer(C_INT) :: category = 0
      integer(C_INT) :: colorType = 1 ! COLOR_ARGB = 1
      integer(C_INT) :: color
      integer(C_INT) :: payloadType = 0 ! PAYLOAD_UNKNOWN = 0
      integer(C_INT) :: reserve_rp
      integer(C_INT64_T) :: payload   ! union uint,int,double
      integer(C_INT) :: messageType = 1  ! MESSAGE_TYPE_ASCII     = 1
      type(C_PTR) :: message  ! ascii char
   end type EventAttributes

#if defined(_USE_NVTX) || defined(_USE_ROCTX)
   interface RangePush
      ! push range with custom label and standard color
      subroutine RangePushA(name) bind(C, name=C_RANGE_PUSH)
         use iso_c_binding
         character(kind=C_CHAR, len=*) :: name
      end subroutine RangePushA

      ! push range with custom label and custom color
      subroutine RangePushEx(event) bind(C, name=C_RANGE_PUSH_EX)
         use iso_c_binding
         import :: EventAttributes
         type(EventAttributes) :: event
      end subroutine RangePushEx
   end interface RangePush

   interface RangePop
      subroutine RangePop() bind(C, name=C_RANGE_POP)
      end subroutine RangePop
   end interface RangePop
#endif

contains

   subroutine StartRange(name, id)
      character(kind=c_char, len=*) :: name
      integer, optional :: id
#if defined(_USE_NVTX) || defined(_USE_ROCTX)
      type(EventAttributes) :: event
      integer :: istat
      istat = cudaDeviceSynchronize()

      tempName = trim(name)//c_null_char

      if (.not. present(id)) then
         call RangePush(tempName)
      else
         event%color = col(mod(id, 7) + 1)
         event%message = c_loc(tempName)
         call RangePushEx(event)
      end if
#endif
   end subroutine StartRange

   subroutine EndRange
#if defined(_USE_NVTX) || defined(_USE_ROCTX)
      integer :: istat
      istat = cudaDeviceSynchronize()
      call RangePop
#endif
   end subroutine EndRange

   subroutine StartRangeAsync(name, id)
      character(kind=c_char, len=*) :: name
      integer, optional :: id
#if defined(_USE_NVTX) || defined(_USE_ROCTX)
      type(EventAttributes) :: event
      tempName = trim(name)//c_null_char
      if (.not. present(id)) then
         call RangePush(tempName)
      else
         event%color = col(mod(id, 7) + 1)
         event%message = c_loc(tempName)
         call RangePushEx(event)
      end if
#endif
   end subroutine StartRangeAsync

   subroutine EndRangeAsync
#if defined(_USE_NVTX) || defined(_USE_ROCTX)
      call RangePop
#endif
   end subroutine EndRangeAsync

end module mod_gpu_tracer

#else



module mod_gpu_tracer
   use iso_c_binding
   implicit none
contains

   subroutine StartRange(name, id)
      character(kind=c_char, len=*) :: name
      integer, optional :: id
   end subroutine StartRange

   subroutine EndRange
   end subroutine EndRange

   subroutine StartRangeAsync(name, id)
      character(kind=c_char, len=*) :: name
      integer, optional :: id
   end subroutine StartRangeAsync

   subroutine EndRangeAsync
   end subroutine EndRangeAsync

end module mod_gpu_tracer
#endif
