      INTEGER FUNCTION mltmod(a,s,m)
!**********************************************************************
!
!     INTEGER FUNCTION MLTMOD(A,S,M)
!
!                    Returns (A*S) MOD M
!
!     This is a transcription from Pascal to Fortran of routine
!     MULtMod_Decompos from the paper
!
!     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
!     with Splitting Facilities." ACM Transactions on Mathematical
!     Software, 17:98-111 (1991)
!
!
!                              Arguments
!
!
!     A, S, M  -->
!                         INTEGER A,S,M
!
!**********************************************************************
!     .. Parameters ..
      INTEGER h
      PARAMETER (h=32768)
!     ..
!     .. Scalar Arguments ..
      INTEGER a,m,s
!     ..
!     .. Local Scalars ..
      INTEGER a0,a1,k,p,q,qh,rh
!     ..
!     .. Executable Statements ..
!
!     H = 2**((b-2)/2) where b = 32 because we are using a 32 bit
!      machine. On a different machine recompute H
!
      IF (.NOT. (a.LE.0.OR.a.GE.m.OR.s.LE.0.OR.s.GE.m)) GO TO 10
      WRITE (*,*) ' A, M, S out of order in MLTMOD - ABORT!'
      WRITE (*,*) ' A = ',a,' S = ',s,' M = ',m
      WRITE (*,*) ' MLTMOD requires: 0 < A < M; 0 < S < M'
      STOP ' A, M, S out of order in MLTMOD - ABORT!'

   10 IF (.NOT. (a.LT.h)) GO TO 20
      a0 = a
      p = 0
      GO TO 120

   20 a1 = a/h
      a0 = a - h*a1
      qh = m/h
      rh = m - h*qh
      IF (.NOT. (a1.GE.h)) GO TO 50
      a1 = a1 - h
      k = s/qh
      p = h* (s-k*qh) - k*rh
   30 IF (.NOT. (p.LT.0)) GO TO 40
      p = p + m
      GO TO 30

   40 GO TO 60

   50 p = 0
!
!     P = (A2*S*H)MOD M
!
   60 IF (.NOT. (a1.NE.0)) GO TO 90
      q = m/a1
      k = s/q
      p = p - k* (m-a1*q)
      IF (p.GT.0) p = p - m
      p = p + a1* (s-k*q)
   70 IF (.NOT. (p.LT.0)) GO TO 80
      p = p + m
      GO TO 70

   80 CONTINUE
   90 k = p/qh
!
!     P = ((A2*H + A1)*S)MOD M
!
      p = h* (p-k*qh) - k*rh
  100 IF (.NOT. (p.LT.0)) GO TO 110
      p = p + m
      GO TO 100

  110 CONTINUE
  120 IF (.NOT. (a0.NE.0)) GO TO 150
!
!     P = ((A2*H + A1)*H*S)MOD M
!
      q = m/a0
      k = s/q
      p = p - k* (m-a0*q)
      IF (p.GT.0) p = p - m
      p = p + a0* (s-k*q)
  130 IF (.NOT. (p.LT.0)) GO TO 140
      p = p + m
      GO TO 130

  140 CONTINUE
  150 mltmod = p
!
      RETURN

      END
