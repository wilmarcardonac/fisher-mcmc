function antithetic_get ( )

!*****************************************************************************80
!
!! ANTITHETIC_GET queries the antithetic value for a given generator.
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
!    Output, logical ANTITHETIC_GET, is TRUE if generator G is antithetic.
!
  implicit none

  logical antithetic_get
  integer ( kind = 4 ) i
  logical value

  i = -1
  call antithetic_memory ( i, value )

  antithetic_get = value

  return
end
