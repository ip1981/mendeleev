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

   type :: formula_t
      integer :: tail = 1
      integer :: n = 0
      integer, dimension(:), allocatable :: elements
      type(formula_t), pointer :: next => null()
   end type formula_t

   type(formula_t), pointer :: formula, f
   character(len=:), allocatable :: word
   integer :: length, argc, i, j

   argc = command_argument_count()
   do i = 1, argc
      call get_command_argument(i, length=length)
      allocate(character(len=length) :: word)
      call get_command_argument(i, value=word)
      write (*, "(A, ':')") word

      formula => explode(word)

      f => formula
      do while (associated(f))
         if (f%n > 0) then
            do j = 1, f%n
               write (*, "(' ', A)", advance="no") trim(ELEMENTS(f%elements(j)))
            end do
            write (*, "()")
         end if
         f => f%next
      end do

      call free_formula(formula)
      deallocate(word)
   end do


contains

   pure subroutine free_formula(formula)
      type(formula_t), pointer, intent(in out) :: formula

      type(formula_t), pointer :: next

      do while (associated(formula))
         next => formula%next
         deallocate(formula%elements)
         deallocate(formula)
         formula => next
      end do
   end subroutine free_formula


   pure integer function tolower(c)
      character(len=1), intent(in) :: c

      tolower = ior(32, iachar(c))
   end function tolower


   pure subroutine search(start, length, sh, c)
      integer, intent(in out) :: start, length
      integer, intent(in) :: sh
      character(len=1), intent(in) :: c

      integer :: l, m, u, c_

      c_ = tolower(c)

      u = start + length
      l = start
      do while (l < u)
         m = (u + l) / 2
         if (tolower(ELEMENTS(m)(sh:sh)) < c_) then
           l = m + 1
         else
           u = m
        endif
      end do

      if (l == start + length) then
        length = 0
        return
      end if

      if (tolower(ELEMENTS(l)(sh:sh)) /= c_) then
        length = 0
        return
      end if

      u = start + length
      start = l
      do while (l < u)
         m = (u + l) / 2
         if (c_ < tolower(ELEMENTS(m)(sh:sh))) then
           u = m
         else
           l = m + 1
        endif
      end do

      length = u - start
   end subroutine search


   pure subroutine advance(word, f)
      character(len=*), intent(in) :: word
      type(formula_t), pointer, intent(in out) :: f

      integer :: n, tail
      integer :: start, length, sh, c
      type(formula_t), pointer :: g

      tail = f%tail
      n = f%n

      sh = 0
      start = 1
      length = ubound(ELEMENTS, 1)
      do
         c = tail + sh
         if (len(word) < c) exit

         sh = sh + 1
         call search(start, length, sh, word(c:c))
         if (length == 0) exit

         if (sh == len_trim(ELEMENTS(start))) then
            if (n /= f%n) then
               allocate(g)
               allocate(g%elements(len(word)))
               g%n = n
               g%elements(1:n) = f%elements(1:n)
               g%next => f%next
               f%next => g
               f => g
            end if

            f%n = f%n + 1
            f%elements(f%n) = start
            f%tail = c + 1
            start = start + 1
            length = length - 1
         end if
      end do

      if (n == f%n) then
         f%tail = f%tail + 1
         f%n = f%n + 1
         f%elements(f%n) = 0
      end if
   end subroutine advance


   pure function explode(word) result(formula)
      character(len=*), intent(in) :: word
      type(formula_t), pointer :: formula

      logical :: has_tail
      type(formula_t), pointer :: f

      allocate(formula)
      allocate(formula%elements(len(word)))

      do
         f => formula
         has_tail = .false.
         do while (associated(f))
            if (f%tail <= len(word)) then
               call advance(word, f)
               if (.not. has_tail) has_tail = f%tail <= len(word)
            end if
            f => f%next
         end do
         if (.not. has_tail) exit
      end do
   end function explode
end program mendeleev
