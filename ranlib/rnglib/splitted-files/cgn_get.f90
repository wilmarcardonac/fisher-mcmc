function cgn_get ( )

!*****************************************************************************80
!
!! CGN_GET gets the current generator index.
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
!    Output, integer ( kind = 4 ) CGN_GET, the current generator index.
!    1 <= CGN_GET <= 32.
!
  implicit none

  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = -1
  call cgn_memory ( i, g )

  cgn_get = g

  return
end
