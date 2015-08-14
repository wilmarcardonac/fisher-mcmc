      SUBROUTINE setant(qvalue)
!**********************************************************************
!
!      SUBROUTINE SETANT(QVALUE)
!               SET ANTithetic
!
!     Sets whether the current generator produces antithetic values.  If
!     X   is  the value  normally returned  from  a uniform [0,1] random
!     number generator then 1  - X is the antithetic  value. If X is the
!     value  normally  returned  from a   uniform  [0,N]  random  number
!     generator then N - 1 - X is the antithetic value.
!
!     All generators are initialized to NOT generate antithetic values.
!
!     This is a transcription from Pascal to Fortran of routine
!     Set_Antithetic from the paper
!
!     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
!     with Splitting Facilities." ACM Transactions on Mathematical
!     Software, 17:98-111 (1991)
!
!
!                              Arguments
!
!
!     QVALUE -> .TRUE. if generator G is to generating antithetic
!                    values, otherwise .FALSE.
!                                   LOGICAL QVALUE
!
!**********************************************************************
!     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
!     ..
!     .. Scalar Arguments ..
      LOGICAL qvalue
!     ..
!     .. Scalars in Common ..
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2
!     ..
!     .. Arrays in Common ..
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),        &
     &        lg2(numg)
      LOGICAL qanti(numg)
!     ..
!     .. Local Scalars ..
      INTEGER g
!     ..
!     .. External Functions ..
      LOGICAL qrgnin
      EXTERNAL qrgnin
!     ..
!     .. External Subroutines ..
      EXTERNAL getcgn
!     ..
!     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,  &
     &       cg2,qanti
!     ..
!     .. Save statement ..
      SAVE /globe/
!     ..
!     .. Executable Statements ..
!     Abort unless random number generator initialized
      IF (qrgnin()) GO TO 10
      WRITE (*,*) ' SETANT called before random number generator ',     &
     &  ' initialized -- abort!'
      STOP ' SETANT called before random number generator initialized'

   10 CALL getcgn(g)
      qanti(g) = qvalue
      RETURN

      END
