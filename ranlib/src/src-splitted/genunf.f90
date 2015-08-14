      REAL FUNCTION genunf(low,high)
!**********************************************************************
!
!     REAL FUNCTION GENUNF( LOW, HIGH )
!
!               GeNerate Uniform Real between LOW and HIGH
!
!
!                              Function
!
!
!     Generates a real uniformly distributed between LOW and HIGH.
!
!
!                              Arguments
!
!
!     LOW --> Low bound (exclusive) on real value to be generated
!                         REAL LOW
!
!     HIGH --> High bound (exclusive) on real value to be generated
!                         REAL HIGH
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL high,low
!     ..
!     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
!     ..
!     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      WRITE (*,*) 'LOW > HIGH in GENUNF: LOW ',low,' HIGH: ',high
      WRITE (*,*) 'Abort'
      STOP 'LOW > High in GENUNF - Abort'

   10 genunf = low + (high-low)*ranf()

      RETURN

      END
