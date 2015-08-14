subroutine initialize ( )

!*****************************************************************************80
!
!! INITIALIZE initializes the random number generator library.
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
!    None
!
  implicit none

  integer ( kind = 4 ) g
  integer ( kind = 4 ), parameter :: g_max = 32
  integer ( kind = 4 ) ig1
  integer ( kind = 4 ) ig2
  logical value
!
!  Remember that we have called INITIALIZE().
!
  call initialized_set ( )
!
!  Initialize all generators to have FALSE antithetic value.
!
  value = .false.
  do g = 1, g_max
    call cgn_set ( g )
    call antithetic_set ( value )
  end do
!
!  Set the initial seeds.
!
  ig1 = 1234567890
  ig2 = 123456789
  call set_initial_seed ( ig1, ig2 )
!
!  Initialize the current generator index to the first one.
!
  g = 1
  call cgn_set ( g )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'INITIALIZE - Note:'
  write ( *, '(a)' ) '  The RNGLIB package has been initialized.'

  return
end
