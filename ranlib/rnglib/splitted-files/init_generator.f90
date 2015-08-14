subroutine init_generator ( t )

!*****************************************************************************80
!
!! INIT_GENERATOR sets the current generator to initial, last or new seed.
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
!    Input, integer ( kind = 4 ) T, the seed type:
!    0, use the seed chosen at initialization time.
!    1, use the last seed.
!    2, use a new seed set 2^30 values away.
!
  implicit none

  integer ( kind = 4 ), parameter :: a1_w = 1033780774
  integer ( kind = 4 ), parameter :: a2_w = 1494757890
  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2
  logical initialized_get
  integer ( kind = 4 ) lg1
  integer ( kind = 4 ) lg2
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) multmod
  integer ( kind = 4 ) t
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'INIT_GENERATOR - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )
!
!  0: restore the initial seed.
!
  if ( t == 0 ) then

    call ig_get ( g, ig1, ig2 )
    lg1 = ig1
    lg2 = ig2
    call lg_set ( g, lg1, lg2 )
!
!  1: restore the last seed.
!
  else if ( t == 1 ) then

    call lg_get ( g, lg1, lg2 )
!
!  2: advance to a new seed.
!
  else if ( t == 2 ) then

    call lg_get ( g, lg1, lg2 )
    lg1 = multmod ( a1_w, lg1, m1 )
    lg2 = multmod ( a2_w, lg2, m2 )
    call lg_set ( g, lg1, lg2 )

  else

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'INIT_GENERATOR - Fatal error!'
    write ( *, '(a)' ) '  Input parameter T out of bounds.'
    stop 1

  end if
!
!  Store the new seed.
!
  cg1 = lg1
  cg2 = lg2
  call cg_set ( g, cg1, cg2 )

  return
end
