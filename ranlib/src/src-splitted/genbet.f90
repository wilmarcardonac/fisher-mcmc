      REAL FUNCTION genbet(aa,bb)
!**********************************************************************
!
!     REAL FUNCTION GENBET( A, B )
!               GeNerate BETa random deviate
!
!
!                              Function
!
!
!     Returns a single random deviate from the beta distribution with
!     parameters A and B.  The density of the beta is
!               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
!
!
!                              Arguments
!
!
!     A --> First parameter of the beta distribution
!                         REAL A
!
!     B --> Second parameter of the beta distribution
!                         REAL B
!
!
!                              Method
!
!
!     R. C. H. Cheng
!     Generating Beta Variatew with Nonintegral Shape Parameters
!     Communications of the ACM, 21:317-322  (1978)
!     (Algorithms BB and BC)
!
!**********************************************************************
!     .. Parameters ..
!     Close to the largest number that can be exponentiated
      REAL expmax
      PARAMETER (expmax=89.0)
!     Close to the largest representable single precision number
      REAL infnty
      PARAMETER (infnty=1.0E38)
!     ..
!     .. Scalar Arguments ..
      REAL aa,bb
!     ..
!     .. Local Scalars ..
      REAL a,alpha,b,beta,delta,gamma,k1,k2,olda,oldb,r,s,t,u1,u2,v,w,y,&
     &     z
      LOGICAL qsame
!     ..
!     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC exp,log,max,min,sqrt
!     ..
!     .. Save statement ..
      SAVE olda,oldb,alpha,beta,gamma,k1,k2
!     ..
!     .. Data statements ..
      DATA olda,oldb/-1,-1/
!     ..
!     .. Executable Statements ..
      qsame = (olda.EQ.aa) .AND. (oldb.EQ.bb)
      IF (qsame) GO TO 20
      IF (.NOT. (aa.LE.0.0.OR.bb.LE.0.0)) GO TO 10
      WRITE (*,*) ' AA or BB <= 0 in GENBET - Abort!'
      WRITE (*,*) ' AA: ',aa,' BB ',bb
      STOP ' AA or BB <= 0 in GENBET - Abort!'

   10 olda = aa
      oldb = bb
   20 IF (.NOT. (min(aa,bb).GT.1.0)) GO TO 100


!     Alborithm BB

!
!     Initialize
!
      IF (qsame) GO TO 30
      a = min(aa,bb)
      b = max(aa,bb)
      alpha = a + b
      beta = sqrt((alpha-2.0)/ (2.0*a*b-alpha))
      gamma = a + 1.0/beta
   30 CONTINUE
   40 u1 = ranf()
!
!     Step 1
!
      u2 = ranf()
      v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 50
      w = infnty
      GO TO 60

   50 w = a*exp(v)
   60 z = u1**2*u2
      r = gamma*v - 1.3862944
      s = a + r - w
!
!     Step 2
!
      IF ((s+2.609438).GE. (5.0*z)) GO TO 70
!
!     Step 3
!
      t = log(z)
      IF (s.GT.t) GO TO 70
!
!     Step 4
!
      IF ((r+alpha*log(alpha/ (b+w))).LT.t) GO TO 40
!
!     Step 5
!
   70 IF (.NOT. (aa.EQ.a)) GO TO 80
      genbet = w/ (b+w)
      GO TO 90

   80 genbet = b/ (b+w)
   90 GO TO 230


!     Algorithm BC

!
!     Initialize
!
  100 IF (qsame) GO TO 110
      a = max(aa,bb)
      b = min(aa,bb)
      alpha = a + b
      beta = 1.0/b
      delta = 1.0 + a - b
      k1 = delta* (0.0138889+0.0416667*b)/ (a*beta-0.777778)
      k2 = 0.25 + (0.5+0.25/delta)*b
  110 CONTINUE
  120 u1 = ranf()
!
!     Step 1
!
      u2 = ranf()
      IF (u1.GE.0.5) GO TO 130
!
!     Step 2
!
      y = u1*u2
      z = u1*y
      IF ((0.25*u2+z-y).GE.k1) GO TO 120
      GO TO 170
!
!     Step 3
!
  130 z = u1**2*u2
      IF (.NOT. (z.LE.0.25)) GO TO 160
      v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 140
      w = infnty
      GO TO 150

  140 w = a*exp(v)
  150 GO TO 200

  160 IF (z.GE.k2) GO TO 120
!
!     Step 4
!
!
!     Step 5
!
  170 v = beta*log(u1/ (1.0-u1))
      IF (.NOT. (v.GT.expmax)) GO TO 180
      w = infnty
      GO TO 190

  180 w = a*exp(v)
  190 IF ((alpha* (log(alpha/ (b+w))+v)-1.3862944).LT.log(z)) GO TO 120
!
!     Step 6
!
  200 IF (.NOT. (a.EQ.aa)) GO TO 210
      genbet = w/ (b+w)
      GO TO 220

  210 genbet = b/ (b+w)
  220 CONTINUE
  230 RETURN

      END
