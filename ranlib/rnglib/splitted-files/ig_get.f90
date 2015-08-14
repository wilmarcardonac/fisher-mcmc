subroutine ig_get ( g, ig1, ig2 )

!*****************************************************************************80
!
!! IG_GET queries the IG values for a given generator.
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
!    Output, integer ( kind = 4 ) IG1, IG2, the IG values for generator G.
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2

  i = -1
  call ig_memory ( i, g, ig1, ig2 )

  return
end
