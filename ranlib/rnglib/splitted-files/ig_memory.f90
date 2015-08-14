subroutine ig_memory ( i, g, ig1, ig2 )

!*****************************************************************************80
!
!! IG_MEMORY stores the IG values for all generators.
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
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) IG1, IG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the IG parameter for generator G.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig1_save(g_max)
  integer ( kind = 4 ) ig2
  integer ( kind = 4 ) ig2_save(g_max)

  save ig1_save
  save ig2_save

  data ig1_save / 32 * 0 /
  data ig2_save / 32 * 0 /

  if ( g < 1 .or. g_max < g ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'IG_MEMORY - Fatal error!'
    write ( *, '(a)' ) '  Input generator index G is out of bounds.'
    stop 1
  end if

  if ( i < 0 ) then
    ig1 = ig1_save(g)
    ig2 = ig2_save(g)
  else if ( i == 0 ) then
    ig1_save(1:g_max) = 0
    ig2_save(1:g_max) = 0
  else if ( 0 < i ) then
    ig1_save(g) = ig1
    ig2_save(g) = ig2
  end if

  return
end
