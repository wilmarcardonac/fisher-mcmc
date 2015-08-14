function r8_uni_01 ( )

!*****************************************************************************80
!
!! R8_UNI_01 returns a uniform random double precision number in [0,1].
!
!  Discussion:
!
!    This procedure returns a random floating point number from a uniform
!    distribution over (0,1), not including the endpoint values, using the
!    current random number generator.
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
!    Output, real ( kind = 8 ) R8_UNI_01, a uniform random value in [0,1].
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_uni
  logical initialized_get
  real ( kind = 8 ) r8_uni_01
!
!  Check whether the package must be initialized.
!
  if ( .not. initialized_get ( ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNI_01 - Note:'
    write ( *, '(a)' ) '  Initializing RNGLIB package.'
    call initialize ( )
  end if
!
!  Get a random positive integer.
!
  i = i4_uni ( )
!
!  Scale it to a random real in [0,1].
!
  r8_uni_01 = real ( i, kind = 8 ) * 4.656613057D-10

  return
end
