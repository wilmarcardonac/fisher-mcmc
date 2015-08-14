subroutine lg_set ( g, lg1, lg2 )

!*****************************************************************************80
!
!! LG_SET sets the LG values for a given generator.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2013
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
!    Input, integer ( kind = 4 ) LG1, LG2, the LG values for generator G.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg2

  i = +1
  call lg_memory ( i, g, lg1, lg2 )

  return
end
