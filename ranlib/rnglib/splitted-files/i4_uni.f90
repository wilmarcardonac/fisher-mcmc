function i4_uni ( )

!*****************************************************************************80
!
!! I4_UNI generates a random positive integer.
!
!  Discussion:
!
!    This procedure returns a random integer following a uniform distribution
!    over (1, 2147483562) using the current generator.
!
!    The original name of this function was "random()", but this conflicts
!    with a standard library function name in C.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 August 2013
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
!    Output, integer ( kind = 4 ) I4_UNI, the random integer.
!
  implicit none

  integer ( kind = 4 ), parameter :: a1 = 40014
  integer ( kind = 4 ), parameter :: a2 = 40692
  logical antithetic_get
  integer ( kind = 4 ) cg1
  integer ( kind = 4 ) cg2
  integer ( kind = 4 ) cgn_get
  integer ( kind = 4 ) g
  integer ( kind = 4 ) i4_uni
  logical initialized_get
  integer ( kind = 4 ) k
  integer ( kind = 4 ), parameter :: m1 = 2147483563
  integer ( kind = 4 ), parameter :: m2 = 2147483399
  logical value
  integer ( kind = 4 ) z
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'I4_UNI - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get the current generator index.
!
  g = cgn_get ( )
!
!  Retrieve the seeds for the current generator.
!
  call cg_get ( g, cg1, cg2 )
!
!  Update the seeds.
!
  k = cg1 / 53668
  cg1 = a1 * ( cg1 - k * 53668 ) - k * 12211

  if ( cg1 < 0 ) then
    cg1 = cg1 + m1
  end if

  k = cg2 / 52774
  cg2 = a2 * ( cg2 - k * 52774 ) - k * 3791

  if ( cg2 < 0 ) then
    cg2 = cg2 + m2
  end if
!
!  Store the updated seeds.
!
  call cg_set ( g, cg1, cg2 )
!
!  Construct the random integer from the seeds.
!
  z = cg1 - cg2

  if ( z < 1 ) then
    z = z + m1 - 1
  end if
!
!  If the generator is in antithetic mode, we must reflect the value.
!
  value = antithetic_get ( )

  if ( value ) then
    z = m1 - z
  end if

  i4_uni = z

  return
end
