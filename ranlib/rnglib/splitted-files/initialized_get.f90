function initialized_get ( )

!*****************************************************************************80
!
!! INITIALIZED_GET queries the INITIALIZED value.
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
!    Output, logical INITIALIZED_GET, is TRUE if the package has
!    been initialized.
!
  implicit none

  integer ( kind = 4 ) i
  logical initialized
  logical initialized_get

  i = -1
  call initialized_memory ( i, initialized )

  initialized_get = initialized

  return
end
