subroutine cg_memory ( i, g, cg1, cg2 )

!*****************************************************************************80
!
!! CG_MEMORY stores the CG values for all generators.
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
!    Input/output, integer ( kind = 4 ) CG1, CG2.  For I = -1,
!    these are output, for I = +1, these are input, for I = 0,
!    these arguments are ignored.  When used, the arguments are
!    old or new values of the CG parameter for generator G.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg1_save(g_max)
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cg2_save(g_max)
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  save cg1_save
  save cg2_save

  data cg1_save / 32 * 0 /
  data cg2_save / 32 * 0 /

  if ( g < 1 .or. g_max < g ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'CG_MEMORY - Fatal error!'
    write ( *, '(a)' ) '  Input generator index G is out of bounds.'
    stop 1
  end if

  if ( i < 0 ) then
    cg1 = cg1_save(g)
    cg2 = cg2_save(g)
  else if ( i == 0 ) then
    cg1_save(1:g_max) = 0
    cg2_save(1:g_max) = 0
  else if ( 0 < i ) then
    cg1_save(g) = cg1
    cg2_save(g) = cg2
  end if

  return
end
