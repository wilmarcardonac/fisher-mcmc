subroutine initialized_memory ( i, initialized )

!*****************************************************************************80
!
!! INITIALIZED_MEMORY stores the INITIALIZED value for the package.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 March 2013
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
!    Input/output, logical INITIALIZED.  For I = -1,
!    this is output, for I = +1, this is input, for I = 0,
!    this argument is ignored.
!
  implicit none

  integer ( kind = 4 ) i
  logical initialized
  logical initialized_save

  save initialized_save

  data initialized_save / .false. /

  if ( i < 0 ) then
    initialized = initialized_save
  else if ( i == 0 ) then
    initialized_save = .false.
  else if ( 0 < i ) then
    initialized_save = initialized
  end if

  return
end
