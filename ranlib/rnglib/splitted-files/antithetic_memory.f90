subroutine antithetic_memory ( i, value )

!*****************************************************************************80
!
!! ANTITHETIC_MEMORY stores the antithetic value for all generators.
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
!    Input, integer ( kind = 4 ) I, the desired action.
!    -1, get a value.
!    0, initialize all values.
!    1, set a value.
!
!    Input/output, logical VALUE.  For I = -1, VALUE is an output
!    quantity, for I = +1, an input quantity.
!
  implicit none

  integer ( kind = 4 ), parameter :: g_max = 32

  logical a_save(g_max)
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  logical value

  save a_save

  data a_save / 32 * .false. /

  if ( i < 0 ) then
    g = cgn_get ( )
    value = a_save(g)
  else if ( i == 0 ) then
    a_save(1:g_max) = .false.
  else if ( 0 < i ) then
    g = cgn_get ( )
    a_save(g) = value
  end if

  return
end
