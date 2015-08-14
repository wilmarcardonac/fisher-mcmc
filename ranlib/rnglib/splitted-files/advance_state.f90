subroutine advance_state ( k )

!*****************************************************************************80
!
!! ADVANCE_STATE advances the state of the current generator.
!
!  Discussion:
!
!    This procedure advances the state of the current generator by 2^K
!    values and resets the initial seed to that value.
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
!    Input, integer ( kind = 4 ) K, indicates that the generator is to be
!    advanced by 2^K values.
!    0 <= K.
!
  implicit none

  integer ( kind = 4 ), parameter :: a1 = 40014
  integer ( kind = 4 ), parameter :: a2 = 40692
  integer ( kind = 4 ) b1
  integer ( kind = 4 ) b2
  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i
  logical initialized_get
  integer ( kind = 4 ) k
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  integer ( kind = 4 ) multmod

  if ( k < 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ADVANCE_STATE - Fatal error!'
    write ( *, '(a)' ) '  Input exponent K is out of bounds.'
    stop 1
  end if
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'ADVANCE_STATE - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )

  b1 = a1
  b2 = a2

  do i = 1, k
    b1 = multmod ( b1, b1, m1 )
    b2 = multmod ( b2, b2, m2 )
  end do

  call cg_get ( g, cg1, cg2 )
  cg1 = multmod ( b1, cg1, m1 )
  cg2 = multmod ( b2, cg2, m2 )
  call cg_set ( g, cg1, cg2 )

  return
end
