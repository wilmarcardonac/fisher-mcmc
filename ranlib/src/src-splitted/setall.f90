      SUBROUTINE setall(iseed1,iseed2)
!**********************************************************************
!
!      SUBROUTINE SETALL(ISEED1,ISEED2)
!               SET ALL random number generators
!
!     Sets the initial seed of generator 1 to ISEED1 and ISEED2. The
!     initial seeds of the other generators are set accordingly, and
!     all generators states are set to these seeds.
!
!     This is a transcription from Pascal to Fortran of routine
!     Set_Initial_Seed from the paper
!
!     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
!     with Splitting Facilities." ACM Transactions on Mathematical
!     Software, 17:98-111 (1991)
!
!
!                              Arguments
!
!
!     ISEED1 -> First of two integer seeds
!                                   INTEGER ISEED1
!
!     ISEED2 -> Second of two integer seeds
!                                   INTEGER ISEED1
!
!**********************************************************************
!     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
!     ..
!     .. Scalar Arguments ..
      INTEGER iseed1,iseed2
      LOGICAL qssd
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
      INTEGER g,ocgn
      LOGICAL qqssd
!     ..
!     .. External Functions ..
      INTEGER mltmod
      LOGICAL qrgnin
      EXTERNAL mltmod,qrgnin
!     ..
!     .. External Subroutines ..
      EXTERNAL getcgn,initgn,inrgcm,setcgn
!     ..
!     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,  &
     &       cg2,qanti
!     ..
!     .. Save statement ..
      SAVE /globe/,qqssd
!     ..
!     .. Data statements ..
      DATA qqssd/.FALSE./
!     ..
!     .. Executable Statements ..
!
!     TELL IGNLGI, THE ACTUAL NUMBER GENERATOR, THAT THIS ROUTINE
!      HAS BEEN CALLED.
!
      qqssd = .TRUE.
      CALL getcgn(ocgn)
!
!     Initialize Common Block if Necessary
!
      IF (.NOT. (qrgnin())) CALL inrgcm()
      ig1(1) = iseed1
      ig2(1) = iseed2
      CALL initgn(-1)
      DO 10,g = 2,numg
          ig1(g) = mltmod(a1vw,ig1(g-1),m1)
          ig2(g) = mltmod(a2vw,ig2(g-1),m2)
          CALL setcgn(g)
          CALL initgn(-1)
   10 END DO
      CALL setcgn(ocgn)
      RETURN

      ENTRY rgnqsd(qssd)
!**********************************************************************
!
!     SUBROUTINE RGNQSD
!                    Random Number Generator Query SeeD set?
!
!     Returns (LOGICAL) QSSD as .TRUE. if SETALL has been invoked,
!     otherwise returns .FALSE.
!
!**********************************************************************
      qssd = qqssd
      RETURN

      END
