function multmod ( a, s, m )

!*****************************************************************************80
!
!! MULTMOD carries out modular multiplication.
!
!  Discussion:
!
!    This procedure returns
!
!      ( A * S ) mod M
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
!    Input, integer ( kind = 4 ) A, S, M, the arguments.
!
!    Output, integer ( kind = 4 ) MULTMOD, the value of the product of A and S,
!    modulo M.
!
  implicit none

  integer ( kind = 4 ) a
  integer ( kind = 4 ) a0
  integer ( kind = 4 ) a1
  integer ( kind = 4 ), parameter :: h = 32768
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) multmod
  integer ( kind = 4 ) p
  integer ( kind = 4 ) q
  integer ( kind = 4 ) qh
  integer ( kind = 4 ) rh
  integer ( kind = 4 ) s

  if ( a <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  A <= 0.'
    stop 1
  end if

  if ( m <= a ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  M <= A.'
    stop 1
  end if

  if ( s <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  S <= 0.'
    stop 1
  end if

  if ( m <= s ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'MULTMOD - Fatal error!'
    write ( *, '(a)' ) '  M <= S.'
    stop 1
  end if

  if ( a < h ) then

    a0 = a
    p = 0

  else

    a1 = a / h
    a0 = a - h * a1
    qh = m / h
    rh = m - h * qh

    if ( h <= a1 ) then

      a1 = a1 - h
      k = s / qh
      p = h * ( s - k * qh ) - k * rh

      do while ( p < 0 )
        p = p + m
      end do

    else

      p = 0

    end if

    if ( a1 /= 0 ) then

      q = m / a1
      k = s / q
      p = p - k * ( m - a1 * q )

      if ( 0 < p ) then
        p = p - m
      end if

      p = p + a1 * ( s - k * q )

      do while ( p < 0 )
        p = p + m
      end do

    end if

    k = p / qh
    p = h * ( p - k * qh ) - k * rh

    do while ( p < 0 )
      p = p + m
    end do

  end if

  if ( a0 /= 0 ) then

    q = m / a0
    k = s / q
    p = p - k * ( m - a0 * q )

    if ( 0 < p ) then
      p = p - m
    end if

    p = p + a0 * ( s - k * q )

    do while ( p < 0 )
      p = p + m
    end do

  end if

  multmod = p

  return
end
