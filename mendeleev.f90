program mendeleev

   implicit none

   character(len=2), dimension(0:118), parameter :: ELEMENTS = (/ "? ", &
     "Ac", "Ag", "Al", "Am", "Ar", "As", "At", "Au", "B ", "Ba", "Be", "Bh", &
     "Bi", "Bk", "Br", "C ", "Ca", "Cd", "Ce", "Cf", "Cl", "Cm", "Cn", "Co", &
     "Cr", "Cs", "Cu", "Db", "Ds", "Dy", "Er", "Es", "Eu", "F ", "Fe", "Fl", &
     "Fm", "Fr", "Ga", "Gd", "Ge", "H ", "He", "Hf", "Hg", "Ho", "Hs", "I ", &
     "In", "Ir", "K ", "Kr", "La", "Li", "Lr", "Lu", "Lv", "Mc", "Md", "Mg", &
     "Mn", "Mo", "Mt", "N ", "Na", "Nb", "Nd", "Ne", "Nh", "Ni", "No", "Np", &
     "O ", "Og", "Os", "P ", "Pa", "Pb", "Pd", "Pm", "Po", "Pr", "Pt", "Pu", &
     "Ra", "Rb", "Re", "Rf", "Rg", "Rh", "Rn", "Ru", "S ", "Sb", "Sc", "Se", &
     "Sg", "Si", "Sm", "Sn", "Sr", "Ta", "Tb", "Tc", "Te", "Th", "Ti", "Tl", &
     "Tm", "Ts", "U ", "V ", "W ", "Xe", "Y ", "Yb", "Zn", "Zr" /)

   type :: element_t
      integer :: eid = 0
      character(len=:), pointer :: tail => null()
      type(element_t), pointer :: sibs => null()
      type(element_t), pointer :: next => null()
   end type element_t

   type(element_t), pointer :: root
   character(len=:), pointer :: word
   integer, dimension(:), allocatable :: formula
   integer :: length, argc, i

   argc = command_argument_count()
   do i = 1, argc
      call get_command_argument(i, length=length)
      allocate(character(len=length) :: word)
      call get_command_argument(i, value=word)
      write (*, "(A, ':')") word

      if (length > 0) then
         root => explode(word)
         allocate(formula(length))
         call print_plain(root, formula, 1)
         deallocate(formula)
         call free_elements(root)
      end if

      deallocate(word)
   end do


contains

   pure recursive subroutine free_elements(root)
      type(element_t), pointer, intent(in out) :: root
      type(element_t), pointer :: sibs

      do while (associated(root))
         if (associated(root%next)) call free_elements(root%next)
         sibs => root%sibs
         deallocate(root)
         root => sibs
      end do
   end subroutine free_elements


   pure integer function tolower(c)
      character(len=1), intent(in) :: c

      tolower = ior(32, iachar(c))
   end function tolower


   pure subroutine search(start, end, sh, c)
      integer, intent(in out) :: start, end
      integer, intent(in) :: sh
      character(len=1), intent(in) :: c
      integer :: l, m, u, c_

      c_ = tolower(c)

      u = end
      l = start
      do while (l < u)
         m = (u + l) / 2
         if (tolower(ELEMENTS(m)(sh:sh)) < c_) then
           l = m + 1
         else
           u = m
        endif
      end do

      if (l == end) then
        end = 0
        return
      end if

      if (tolower(ELEMENTS(l)(sh:sh)) /= c_) then
         end = 0
        return
      end if

      u = end
      start = l
      do while (l < u)
         m = (u + l) / 2
         if (c_ < tolower(ELEMENTS(m)(sh:sh))) then
           u = m
         else
           l = m + 1
        endif
      end do

      end = u
   end subroutine search


   function split(tail) result(head)
      character(len=:), pointer, intent(in) :: tail
      type(element_t), pointer :: head, last, el
      integer :: start, end, sh

      head => null()
      last => null()

      start = 1
      end = ubound(ELEMENTS, 1) + 1
      do sh = 1, len(tail)
         call search(start, end, sh, tail(sh:sh))
         if (start >= end) exit

         if (sh == len_trim(ELEMENTS(start))) then
            allocate(el)
            if (associated(last)) then
               last%sibs => el
            else
               head => el
            end if

            last => el
            last%eid = start
            last%tail => tail(sh+1:)

            start = start + 1
         end if
      end do

      if (.not. associated(head)) then
         allocate(head)
         head%tail => tail(2:)
      end if
   end function split


   recursive function explode(tail) result(root)
      character(len=:), pointer, intent(in) :: tail
      type(element_t), pointer :: root, el

      root => split(tail)
      el => root
      do while (associated(el))
         if (0 < len(el%tail)) el%next => explode(el%tail)
         el => el%sibs
      end do
   end function explode


   recursive subroutine print_plain(tree, formula, n)
      type(element_t), pointer, intent(in) :: tree
      integer, dimension(:), intent(in out) :: formula
      integer, value :: n
      type(element_t), pointer :: el
      integer :: i

      el => tree
      do while (associated(el))
         formula(n) = el%eid
         if (associated(el%next)) then
            call print_plain(el%next, formula, n+1)
         else
            do i = 1, n
               write (*, "(' ', A)", advance="no") trim(ELEMENTS(formula(i)))
            end do
            write (*, "()")
         end if
         el => el%sibs
      end do
   end subroutine print_plain

end program mendeleev

