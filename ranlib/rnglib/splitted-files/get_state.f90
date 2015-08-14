subroutine get_state ( cg1, cg2 )

!*****************************************************************************80
!
!! GET_STATE returns the state of the current generator.
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
!    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Pierre LEcuyer, Serge Cote,
!    Implementing a Random Number Package with Splitting Facilities,
!    ACM Transactions on Mathematical Software,
!    Volume 17, Number 1, March 1991, pages 98-111.
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) CG1, CG2, the CG values for the
!    current generator.
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  logical initialized_get
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'GET_STATE - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )
!
!  Retrieve the seed values for this generator.
!
  call cg_get ( g, cg1, cg2 )

  return
end
