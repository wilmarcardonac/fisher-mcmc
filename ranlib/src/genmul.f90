      SUBROUTINE genmul(n,p,ncat,ix) 
!********************************************************************** 
!                                                                       
!            SUBROUTINE GENMUL( N, P, NCAT, IX )                        
!     GENerate an observation from the MULtinomial distribution         
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     N --> Number of events that will be classified into one of        
!           the categories 1..NCAT                                      
!                         INTEGER N                                     
!                                                                       
!     P --> Vector of probabilities.  P(i) is the probability that      
!           an event will be classified into category i.  Thus, P(i)    
!           must be [0,1]. Only the first NCAT-1 P(i) must be defined   
!           since P(NCAT) is 1.0 minus the sum of the first             
!           NCAT-1 P(i).                                                
!                         REAL P(NCAT-1)                                
!                                                                       
!     NCAT --> Number of categories.  Length of P and IX.               
!                         INTEGER NCAT                                  
!                                                                       
!     IX <-- Observation from multinomial distribution.  All IX(i)      
!            will be nonnegative and their sum will be N.               
!                         INTEGER IX(NCAT)                              
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Algorithm from page 559 of                                        
!                                                                       
!     Devroye, Luc                                                      
!                                                                       
!     Non-Uniform Random Variate Generation.  Springer-Verlag,          
!     New York, 1986.                                                   
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      INTEGER n,ncat 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL p(*) 
      INTEGER ix(*) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL prob,ptot,sum 
      INTEGER i,icat,ntot 
!     ..                                                                
!     .. External Functions ..                                          
      INTEGER ignbin 
      EXTERNAL ignbin 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC abs 
!     ..                                                                
!     .. Executable Statements ..                                       
                                                                        
!     Check Arguments                                                   
      IF (n.LT.0) STOP 'N < 0 in GENMUL' 
      IF (ncat.LE.1) STOP 'NCAT <= 1 in GENMUL' 
      ptot = 0.0 
      DO 10,i = 1,ncat - 1 
          IF (p(i).LT.0.0) STOP 'Some P(i) < 0 in GENMUL' 
          IF (p(i).GT.1.0) STOP 'Some P(i) > 1 in GENMUL' 
          ptot = ptot + p(i) 
   10 END DO 
      IF (ptot.GT.0.99999) STOP 'Sum of P(i) > 1 in GENMUL' 
                                                                        
!     Initialize variables                                              
      ntot = n 
      sum = 1.0 
      DO 20,i = 1,ncat 
          ix(i) = 0 
   20 END DO 
                                                                        
!     Generate the observation                                          
      DO 30,icat = 1,ncat - 1 
          prob = p(icat)/sum 
          ix(icat) = ignbin(ntot,prob) 
          ntot = ntot - ix(icat) 
          IF (ntot.LE.0) RETURN 
          sum = sum - p(icat) 
   30 END DO 
      ix(ncat) = ntot 
                                                                        
!     Finished                                                          
      RETURN 
                                                                        
      END                                           
