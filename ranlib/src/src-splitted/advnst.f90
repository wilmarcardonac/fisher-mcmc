      SUBROUTINE advnst(k)
!**********************************************************************
!
!     SUBROUTINE ADVNST(K)
!               ADV-a-N-ce ST-ate
!
!     Advances the state  of  the current  generator  by 2^K values  and
!     resets the initial seed to that value.
!
!     This is  a  transcription from   Pascal to  Fortran    of  routine
!     Advance_State from the paper
!
!     L'Ecuyer, P. and  Cote, S. "Implementing  a  Random Number Package
!     with  Splitting   Facilities."  ACM  Transactions  on Mathematical
!     Software, 17:98-111 (1991)
!
!
!                              Arguments
!
!
!     K -> The generator is advanced by2^K values
!                                   INTEGER K
!
!**********************************************************************
!     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
!     ..
!     .. Scalar Arguments ..
      INTEGER k
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
      INTEGER g,i,ib1,ib2
!     ..
!     .. External Functions ..
      INTEGER mltmod
      LOGICAL qrgnin
      EXTERNAL mltmod,qrgnin
!     ..
!     .. External Subroutines ..
      EXTERNAL getcgn,setsd
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
      WRITE (*,*) ' ADVNST called before random number generator ',     &
     &  ' initialized -- abort!'
      STOP ' ADVNST called before random number generator initialized'

   10 CALL getcgn(g)
!
      ib1 = a1
      ib2 = a2
      DO 20,i = 1,k
          ib1 = mltmod(ib1,ib1,m1)
          ib2 = mltmod(ib2,ib2,m2)
   20 END DO
      CALL setsd(mltmod(ib1,cg1(g),m1),mltmod(ib2,cg2(g),m2))
!
!     NOW, IB1 = A1**K AND IB2 = A2**K
!
      RETURN

      END
