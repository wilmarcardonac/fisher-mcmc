subroutine set_initial_seed ( ig1, ig2 )

!*****************************************************************************80
!
!! SET_INITIAL_SEED resets the initial seed and state for all generators.
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
!    Input, integer ( kind = 4 ) IG1, IG2, the initial seed values
!    for the first generator.
!    1 <= IG1 < 2147483563
!    1 <= IG2 < 2147483399
!
  implicit none

  integer ( kind = 4 ), parameter :: a1_vw = 2082007225
  integer ( kind = 4 ), parameter :: a2_vw = 784306273
  integer ( kind = 4 ) g
  integer ( kind = 4 ), parameter :: g_max = 32
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2
  logical initialized_get
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) multmod
  integer ( kind = 4 ) t

  if ( ig1 < 1 .or. m1 <= ig1 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter IG1 out of bounds.'
    stop 1
  end if

  if ( ig2 < 1 .or. m2 <= ig2 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
    write ( *, '(a)' ) '  Input parameter IG2 out of bounds.'
    stop 1
  end if
!
!  Because INITIALIZE calls SET_INITIAL_SEED, it's not easy to correct
!  the error that arises if SET_INITIAL_SEED is called before INITIALIZE.
!  So don't bother trying.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
    write ( *, '(a)' ) '  The RNGLIB package has not been initialized.'
    stop 1
  end if
!
!  Set the initial seed, then initialize the first generator.
!
  g = 1
  call cgn_set ( g )

  call ig_set ( g, ig1, ig2 )

  t = 0
  call init_generator ( t )
!
!  Now do similar operations for the other generators.
!
  do g = 2, g_max

    call cgn_set ( g )
    ig1 = multmod ( a1_vw, ig1, m1 )
    ig2 = multmod ( a2_vw, ig2, m2 )
    call ig_set ( g, ig1, ig2 )
    call init_generator ( t )

  end do
!
!  Now choose the first generator.
!
  g = 1
  call cgn_set ( g )

  return
end
