subroutine set_seed ( cg1, cg2 )

!*****************************************************************************80
!
!! SET_SEED resets the initial seed and state of the current generator.
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
!    Input, integer ( kind = 4 ) CG1, CG2, the CG values for generator G.
!    1 <= CG1 < 2147483563
!    1 <= CG2 < 2147483399
!
  implicit none

  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  logical initialized_get
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) t

  if ( cg1 < 1 .or. m1 <= cg1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter CG1 out of bounds.'
    stop 1
  end if

  if ( cg2 < 1 .or. m2 <= cg2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter CG2 out of bounds.'
    stop 1
  end if
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_SEED - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Retrieve the current generator index.
!
  g = cgn_get ( )
!
!  Set the seeds.
!
  call cg_set ( g, cg1, cg2 )
!
!  Initialize the generator.
!
  t = 0
  call init_generator ( t )

  return
end
