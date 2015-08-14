subroutine cg_get ( g, cg1, cg2 )

!*****************************************************************************80
!
!! CG_GET queries the CG values for a given generator.
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
!    Output, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i

  i = -1
  call cg_memory ( i, g, cg1, cg2 )

  return
end
