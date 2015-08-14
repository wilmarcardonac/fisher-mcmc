      INTEGER FUNCTION ignpoi(mu) 
!********************************************************************** 
!                                                                       
!     INTEGER FUNCTION IGNPOI( AV )                                     
!                                                                       
!                    GENerate POIsson random deviate                    
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!     Generates a single random deviate from a Poisson                  
!     distribution with mean AV.                                        
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     AV --> The mean of the Poisson distribution from which            
!            a random deviate is to be generated.                       
!                              REAL AV                                  
!                                                                       
!     GENEXP <-- The random deviate.                                    
!                              REAL GENEXP                              
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Renames KPOIS from TOMS as slightly modified by BWB to use RANF   
!     instead of SUNIF.                                                 
!                                                                       
!     For details see:                                                  
!                                                                       
!               Ahrens, J.H. and Dieter, U.                             
!               Computer Generation of Poisson Deviates                 
!               From Modified Normal Distributions.                     
!               ACM Trans. Math. Software, 8, 2                         
!               (June 1982),163-179                                     
!                                                                       
!********************************************************************** 
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!                                                                      C
!     P O I S S O N  DISTRIBUTION                                      C
!                                                                      C
!                                                                      C
!**********************************************************************C
!**********************************************************************C
!                                                                      C
!     FOR DETAILS SEE:                                                 C
!                                                                      C
!               AHRENS, J.H. AND DIETER, U.                            C
!               COMPUTER GENERATION OF POISSON DEVIATES                C
!               FROM MODIFIED NORMAL DISTRIBUTIONS.                    C
!               ACM TRANS. MATH. SOFTWARE, 8,2 (JUNE 1982), 163 - 179. C
!                                                                      C
!     (SLIGHTLY MODIFIED VERSION OF THE PROGRAM IN THE ABOVE ARTICLE)  C
!                                                                      C
!**********************************************************************C
!                                                                       
!      INTEGER FUNCTION IGNPOI(IR,MU)                                   
!                                                                       
!     INPUT:  IR=CURRENT STATE OF BASIC RANDOM NUMBER GENERATOR         
!             MU=MEAN MU OF THE POISSON DISTRIBUTION                    
!     OUTPUT: IGNPOI=SAMPLE FROM THE POISSON-(MU)-DISTRIBUTION          
!                                                                       
!                                                                       
!                                                                       
!     MUPREV=PREVIOUS MU, MUOLD=MU AT LAST EXECUTION OF STEP P OR B.    
!     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT            
!     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL            
!                                                                       
!                                                                       
!                                                                       
!     SEPARATION OF CASES A AND B                                       
!                                                                       
!     .. Scalar Arguments ..                                            
      REAL mu 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL a0,a1,a2,a3,a4,a5,a6,a7,b1,b2,c,c0,c1,c2,c3,d,del,difmuk,e,  &
     &     fk,fx,fy,g,muold,muprev,omega,p,p0,px,py,q,s,t,u,v,x,xx      
      INTEGER j,k,kflag,l,m 
!     ..                                                                
!     .. Local Arrays ..                                                
      REAL fact(10),pp(35) 
!     ..                                                                
!     .. External Functions ..                                          
      REAL ranf,sexpo,snorm 
      EXTERNAL ranf,sexpo,snorm 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC abs,alog,exp,float,ifix,max0,min0,sign,sqrt 
!     ..                                                                
!     .. Data statements ..                                             
      DATA muprev,muold/0.,0./ 
      DATA a0,a1,a2,a3,a4,a5,a6,a7/-.5,.3333333,-.2500068,.2000118,     &
     &     -.1661269,.1421878,-.1384794,.1250060/                       
      DATA fact/1.,1.,2.,6.,24.,120.,720.,5040.,40320.,362880./ 
!     ..                                                                
!     .. Executable Statements ..                                       
      IF (mu.EQ.muprev) GO TO 10 
      IF (mu.LT.10.0) GO TO 120 
!                                                                       
!     C A S E  A. (RECALCULATION OF S,D,L IF MU HAS CHANGED)            
!                                                                       
      muprev = mu 
      s = sqrt(mu) 
      d = 6.0*mu*mu 
!                                                                       
!             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL   
!             PROBABILITIES FK WHENEVER K >= M(MU). L=IFIX(MU-1.1484)   
!             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .             
!                                                                       
      l = ifix(mu-1.1484) 
!                                                                       
!     STEP N. NORMAL SAMPLE - SNORM(IR) FOR STANDARD NORMAL DEVIATE     
!                                                                       
   10 g = mu + s*snorm() 
      IF (g.LT.0.0) GO TO 20 
      ignpoi = ifix(g) 
!                                                                       
!     STEP I. IMMEDIATE ACCEPTANCE IF IGNPOI IS LARGE ENOUGH            
!                                                                       
      IF (ignpoi.GE.l) RETURN 
!                                                                       
!     STEP S. SQUEEZE ACCEPTANCE - SUNIF(IR) FOR (0,1)-SAMPLE U         
!                                                                       
      fk = float(ignpoi) 
      difmuk = mu - fk 
      u = ranf() 
      IF (d*u.GE.difmuk*difmuk*difmuk) RETURN 
!                                                                       
!     STEP P. PREPARATIONS FOR STEPS Q AND H.                           
!             (RECALCULATIONS OF PARAMETERS IF NECESSARY)               
!             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7. 
!             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE 
!             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.   
!             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION. 
!                                                                       
   20 IF (mu.EQ.muold) GO TO 30 
      muold = mu 
      omega = .3989423/s 
      b1 = .4166667E-1/mu 
      b2 = .3*b1*b1 
      c3 = .1428571*b1*b2 
      c2 = b2 - 15.*c3 
      c1 = b1 - 6.*b2 + 45.*c3 
      c0 = 1. - b1 + 3.*b2 - 15.*c3 
      c = .1069/mu 
   30 IF (g.LT.0.0) GO TO 50 
!                                                                       
!             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)     
!                                                                       
      kflag = 0 
      GO TO 70 
!                                                                       
!     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)                           
!                                                                       
   40 IF (fy-u*fy.LE.py*exp(px-fx)) RETURN 
!                                                                       
!     STEP E. EXPONENTIAL SAMPLE - SEXPO(IR) FOR STANDARD EXPONENTIAL   
!             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'             
!             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)           
!                                                                       
   50 e = sexpo() 
      u = ranf() 
      u = u + u - 1.0 
      t = 1.8 + sign(e,u) 
      IF (t.LE. (-.6744)) GO TO 50 
      ignpoi = ifix(mu+s*t) 
      fk = float(ignpoi) 
      difmuk = mu - fk 
!                                                                       
!             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)     
!                                                                       
      kflag = 1 
      GO TO 70 
!                                                                       
!     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)               
!                                                                       
   60 IF (c*abs(u).GT.py*exp(px+e)-fy*exp(fx+e)) GO TO 50 
      RETURN 
!                                                                       
!     STEP F. 'SUBROUTINE' F. CALCULATION OF PX,PY,FX,FY.               
!             CASE IGNPOI .LT. 10 USES FACTORIALS FROM TABLE FACT       
!                                                                       
   70 IF (ignpoi.GE.10) GO TO 80 
      px = -mu 
      py = mu**ignpoi/fact(ignpoi+1) 
      GO TO 110 
!                                                                       
!             CASE IGNPOI .GE. 10 USES POLYNOMIAL APPROXIMATION         
!             A0-A7 FOR ACCURACY WHEN ADVISABLE                         
!             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)                
!                                                                       
   80 del = .8333333E-1/fk 
      del = del - 4.8*del*del*del 
      v = difmuk/fk 
      IF (abs(v).LE.0.25) GO TO 90 
      px = fk*alog(1.0+v) - difmuk - del 
      GO TO 100 
                                                                        
   90 px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0) -&
     &     del                                                          
  100 py = .3989423/sqrt(fk) 
  110 x = (0.5-difmuk)/s 
      xx = x*x 
      fx = -0.5*xx 
      fy = omega* (((c3*xx+c2)*xx+c1)*xx+c0) 
      IF (kflag) 40,40,60 
!                                                                       
!     C A S E  B. (START NEW TABLE AND CALCULATE P0 IF NECESSARY)       
!                                                                       
  120 muprev = 0.0 
      IF (mu.EQ.muold) GO TO 130 
      muold = mu 
      m = max0(1,ifix(mu)) 
      l = 0 
      p = exp(-mu) 
      q = p 
      p0 = p 
!                                                                       
!     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD                       
!                                                                       
  130 u = ranf() 
      ignpoi = 0 
      IF (u.LE.p0) RETURN 
!                                                                       
!     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE               
!             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES              
!             (0.458=PP(9) FOR MU=10)                                   
!                                                                       
      IF (l.EQ.0) GO TO 150 
      j = 1 
      IF (u.GT.0.458) j = min0(l,m) 
      DO 140 k = j,l 
          IF (u.LE.pp(k)) GO TO 180 
  140 END DO 
      IF (l.EQ.35) GO TO 130 
!                                                                       
!     STEP C. CREATION OF NEW POISSON PROBABILITIES P                   
!             AND THEIR CUMULATIVES Q=PP(K)                             
!                                                                       
  150 l = l + 1 
      DO 160 k = l,35 
          p = p*mu/float(k) 
          q = q + p 
          pp(k) = q 
          IF (u.LE.q) GO TO 170 
  160 END DO 
      l = 35 
      GO TO 130 
                                                                        
  170 l = k 
  180 ignpoi = k 
      RETURN 
                                                                        
      END                                           
