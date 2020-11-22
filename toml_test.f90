module a_abstract
   implicit none
   private
   public :: a_abc

   type, abstract :: a_abc
   contains
      procedure(destroy), deferred :: destroy
   end type a_abc

   abstract interface
      recursive subroutine destroy(self)
         import a_abc
         class(a_abc), intent(inout) :: self
      end subroutine destroy
   end interface

end module a_abstract


module b_abstract
   use a_abstract, only : a_abc
   implicit none
   private
   public :: b1_abc, b2_abc

   type, abstract :: b1_abc
   contains
      procedure(destroy), deferred :: destroy
   end type b1_abc

   type, abstract, extends(b1_abc) :: b2_abc
   contains
      procedure(get_len), deferred :: get_len
   end type b2_abc

   abstract interface
      pure function get_len(self) result(length)
         import :: b2_abc
         class(b2_abc), intent(in), target :: self
         integer :: length
      end function get_len
      recursive subroutine destroy(self)
         import :: b1_abc
         class(b1_abc), intent(inout), target :: self
      end subroutine destroy
   end interface

end module b_abstract


module b_impl
   use b_abstract, only : b2_abc
   use a_abstract, only : a_abc
   implicit none
   private
   public :: b_type, new_b_impl

   type :: a_node
      class(a_abc), allocatable :: val
   end type a_node

   type, extends(b2_abc) :: b_type
      integer :: n = 0
      type(a_node), allocatable :: lst(:)
   contains
      procedure :: get_len
      procedure :: destroy
   end type b_type

   integer, parameter :: initial_size = 16

contains

subroutine new_b_impl(self, n)
   type(b_type), intent(out) :: self
   integer, intent(in), optional :: n
   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if
end subroutine new_b_impl

pure function get_len(self) result(length)
   class(b_type), intent(in), target :: self
   integer :: length
   length = self%n
end function get_len

recursive subroutine destroy(self)
   class(b_type), intent(inout), target :: self
   integer :: i
   print'(a)', "entered: self%list%destroy"
   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do
   deallocate(self%lst)
   self%n = 0
end subroutine destroy

end module b_impl


module b_proxy
   use b_abstract, only : b1_abc, b2_abc
   use b_impl, only : b_type, new_b_impl
   implicit none
   private
   public :: b1_abc, b2_abc
   public :: new_b1, new_b2
   public :: len

   interface len
      module procedure :: get_len
   end interface

contains

subroutine new_b1(self)
   class(b1_abc), allocatable, intent(out) :: self
   type(b_type), allocatable :: vect
   allocate(vect)
   call new_b_impl(vect)
   call move_alloc(vect, self)
end subroutine new_b1

subroutine new_b2(self)
   class(b2_abc), allocatable, intent(out) :: self
   type(b_type), allocatable :: vect
   allocate(vect)
   call new_b_impl(vect)
   call move_alloc(vect, self)
end subroutine new_b2

pure function get_len(self) result(length)
   class(b2_abc), intent(in) :: self
   integer :: length
   length = self%get_len()
end function get_len

end module b_proxy


module a_impl
   use a_abstract, only : a_abc
   use b_proxy, only : b1_abc, new_b1
   implicit none
   private
   public :: a_type, new_a, new

   type, extends(a_abc) :: a_type
      class(b1_abc), allocatable :: list
   contains
      procedure :: destroy
   end type a_type

   interface a_type
      module procedure :: new_a_func
   end interface a_type

   interface new
      module procedure :: new_a
   end interface

contains

subroutine new_a(self)
   type(a_type), intent(out) :: self
   call new_b1(self%list)
end subroutine new_a

function new_a_func() result(self)
   type(a_type) :: self
   call new_a(self)
end function new_a_func

recursive subroutine destroy(self)
   class(a_type), intent(inout) :: self
   if (allocated(self%list)) then
      print'(a)', "enter: self%list%destroy"
      call self%list%destroy
      deallocate(self%list)
   end if
end subroutine destroy

end module a_impl


program mwe
   use a_impl
   implicit none
   type(a_type), allocatable :: table

   allocate(table)
   table = a_type()
   call table%destroy
   print'(a)', "Done!"

end program mwe
