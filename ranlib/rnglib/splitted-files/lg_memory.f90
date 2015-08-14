subroutine lg_memory ( i, g, lg1, lg2 )

!*****************************************************************************80
!
!! LG_MEMORY stores the LG values for all generators.
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
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input, integer ( kind = 4 ) G, for I = -1 or +1, the index of
!    the generator, with 1 <= G <= 32.
!
!    Input/output, integer ( kind = 4 ) LG1, LG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the LG parameter for generator G.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg1_save(g_max)
  integer ( kind = 4 ) lg2
  integer ( kind = 4 ) lg2_save(g_max)

  save lg1_save
  save lg2_save

  data lg1_save / 32 * 0 /
  data lg2_save / 32 * 0 /

  if ( g < 1 .or. g_max < g ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'LG_MEMORY - Fatal error!'
    write ( *, '(a)' ) '  Input generator index G is out of bounds.'
    stop 1
  end if

  if ( i < 0 ) then
    lg1 = lg1_save(g)
    lg2 = lg2_save(g)
  else if ( i == 0 ) then
    lg1_save(1:g_max) = 0
    lg2_save(1:g_max) = 0
  else if ( 0 < i ) then
    lg1_save(g) = lg1
    lg2_save(g) = lg2
  end if

  return
end
