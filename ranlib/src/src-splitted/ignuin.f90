      INTEGER FUNCTION ignuin(low,high)
!**********************************************************************
!
!     INTEGER FUNCTION IGNUIN( LOW, HIGH )
!
!               GeNerate Uniform INteger
!
!
!                              Function
!
!
!     Generates an integer uniformly distributed between LOW and HIGH.
!
!
!                              Arguments
!
!
!     LOW --> Low bound (inclusive) on integer value to be generated
!                         INTEGER LOW
!
!     HIGH --> High bound (inclusive) on integer value to be generated
!                         INTEGER HIGH
!
!
!                              Note
!
!
!     If (HIGH-LOW) > 2,147,483,561 prints error message on * unit and
!     stops the program.
!
!**********************************************************************

!     IGNLGI generates integers between 1 and 2147483562
!     MAXNUM is 1 less than maximum generable value
!     .. Parameters ..
      INTEGER maxnum
      PARAMETER (maxnum=2147483561)
      CHARACTER*(*) err1,err2
      PARAMETER (err1='LOW > HIGH in IGNUIN',                           &
     &          err2=' ( HIGH - LOW ) > 2,147,483,561 in IGNUIN')
!     ..
!     .. Scalar Arguments ..
      INTEGER high,low
!     ..
!     .. Local Scalars ..
      INTEGER err,ign,maxnow,range,ranp1
!     ..
!     .. External Functions ..
      INTEGER ignlgi
      EXTERNAL ignlgi
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC mod
!     ..
!     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      err = 1
!      ABORT-PROGRAM
      GO TO 80

   10 range = high - low
      IF (.NOT. (range.GT.maxnum)) GO TO 20
      err = 2
!      ABORT-PROGRAM
      GO TO 80

   20 IF (.NOT. (low.EQ.high)) GO TO 30
      ignuin = low
      RETURN

      GO TO 70

!     Number to be generated should be in range 0..RANGE
!     Set MAXNOW so that the number of integers in 0..MAXNOW is an
!     integral multiple of the number in 0..RANGE

   30 ranp1 = range + 1
      maxnow = (maxnum/ranp1)*ranp1
   40 ign = ignlgi() - 1
      IF (.NOT. (ign.LE.maxnow)) GO TO 50
      ignuin = low + mod(ign,ranp1)
      RETURN

   50 GO TO 40

   60 CONTINUE
   70 CONTINUE
   80 IF (.NOT. (err.EQ.1)) GO TO 90
      WRITE (*,*) err1
      GO TO 100

!     TO ABORT-PROGRAM
   90 WRITE (*,*) err2
  100 WRITE (*,*) ' LOW: ',low,' HIGH: ',high
      WRITE (*,*) ' Abort on Fatal ERROR'
      IF (.NOT. (err.EQ.1)) GO TO 110
      STOP 'LOW > HIGH in IGNUIN'

      GO TO 120

  110 STOP ' ( HIGH - LOW ) > 2,147,483,561 in IGNUIN'

  120 END
