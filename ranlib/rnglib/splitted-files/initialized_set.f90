subroutine initialized_set ( )

!*****************************************************************************80
!
!! INITIALIZED_SET sets the INITIALIZED value true.
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
!    None
!
  implicit none

  integer ( kind = 4 ) i
  logical initialized

  i = +1
  initialized = .true.
  call initialized_memory ( i, initialized )

  return
end
