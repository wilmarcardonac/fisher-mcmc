subroutine antithetic_set ( value )

!*****************************************************************************80
!
!! ANTITHETIC_SET sets the antithetic value for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, logical VALUE, is TRUE if generator G is to be antithetic.
!
  implicit none

  integer ( kind = 4 ) i
  logical value

  i = +1
  call antithetic_memory ( i, value )

  return
end
