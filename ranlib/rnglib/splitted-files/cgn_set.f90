subroutine cgn_set ( g )

!*****************************************************************************80
!
!! CGN_SET sets the current generator index.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 March 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) G, the index of the generator.
!    1 <= G <= 32.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = +1
  call cgn_memory ( i, g )

  return
end
