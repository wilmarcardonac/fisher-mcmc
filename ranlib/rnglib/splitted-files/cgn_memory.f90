subroutine cgn_memory ( i, g )

!*****************************************************************************80
!
!! CGN_MEMORY stores the current generator index.
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
!    -1, get the value.
!    0, initialize the value.
!    1, set the value.
!
!    Input/output, integer ( kind = 4 ) G.  For I = -1 or 0,
!    this is output, for I = +1, this is input.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  integer ( kind = 4 ) g
  integer ( kind = 4 ) g_save
  integer ( kind = 4 ) i

  save g_save

  data g_save / 1 /

  if ( i < 0 ) then

    g = g_save

  else if ( i == 0 ) then

    g_save = 1
    g = g_save

  else if ( 0 < i ) then

    if ( g < 1 .or. g_max < g ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CGN_MEMORY - Fatal error!'
      write ( *, '(a)' ) '  Generator index G is out of bounds.'
      stop 1
    end if

    g_save = g

  end if

  return
end
