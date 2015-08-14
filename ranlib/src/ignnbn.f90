      INTEGER FUNCTION ignnbn(n,p) 
!********************************************************************** 
!                                                                       
!     INTEGER FUNCTION IGNNBN( N, P )                                   
!                                                                       
!                GENerate Negative BiNomial random deviate              
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!     Generates a single random deviate from a negative binomial        
!     distribution.                                                     
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     N  --> Required number of events.                                 
!                              INTEGER N                                
!                                                                       
!     P  --> The probability of an event during a Bernoulli trial.      
!                              REAL P                                   
!                                                                       
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Algorithm from page 480 of                                        
!                                                                       
!     Devroye, Luc                                                      
!                                                                       
!     Non-Uniform Random Variate Generation.  Springer-Verlag,          
!     New York, 1986.                                                   
!                                                                       
!********************************************************************** 
!     ..                                                                
!     .. Scalar Arguments ..                                            
      REAL p 
      INTEGER n 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL y,a,r 
!     ..                                                                
!     .. External Functions ..                                          
      REAL gengam 
      INTEGER ignpoi 
      EXTERNAL gengam,ignpoi 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC real 
!     ..                                                                
!     .. Executable Statements ..                                       
!     Check Arguments                                                   
      IF (n.LT.0) STOP 'N < 0 in IGNNBN' 
      IF (p.LE.0.0) STOP 'P <= 0 in IGNNBN' 
      IF (p.GE.1.0) STOP 'P >= 1 in IGNNBN' 
                                                                        
!     Generate Y, a random gamma (n,(1-p)/p) variable                   
      r = real(n) 
      a = p/ (1.0-p) 
      y = gengam(a,r) 
                                                                        
!     Generate a random Poisson(y) variable                             
      ignnbn = ignpoi(y) 
      RETURN 
                                                                        
      END                                           
