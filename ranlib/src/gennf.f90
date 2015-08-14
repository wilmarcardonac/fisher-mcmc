      REAL FUNCTION gennf(dfn,dfd,xnonc) 
                                                                        
!********************************************************************** 
!                                                                       
!     REAL FUNCTION GENNF( DFN, DFD, XNONC )                            
!           GENerate random deviate from the Noncentral F distribution  
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!     Generates a random deviate from the  noncentral F (variance ratio)
!     distribution with DFN degrees of freedom in the numerator, and DFD
!     degrees of freedom in the denominator, and noncentrality parameter
!     XNONC.                                                            
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     DFN --> Numerator degrees of freedom                              
!             (Must be >= 1.0)                                          
!                              REAL DFN                                 
!      DFD --> Denominator degrees of freedom                           
!             (Must be positive)                                        
!                              REAL DFD                                 
!                                                                       
!     XNONC --> Noncentrality parameter                                 
!               (Must be nonnegative)                                   
!                              REAL XNONC                               
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Directly generates ratio of noncentral numerator chisquare variate
!     to central denominator chisquare variate.                         
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      REAL dfd,dfn,xnonc 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL xden,xnum 
      LOGICAL qcond 
!     ..                                                                
!     .. External Functions ..                                          
      REAL genchi,gennch 
      EXTERNAL genchi,gennch 
!     ..                                                                
!     .. Executable Statements ..                                       
      qcond = dfn .LE. 1.0 .OR. dfd .LE. 0.0 .OR. xnonc .LT. 0.0 
      IF (.NOT. (qcond)) GO TO 10 
      WRITE (*,*) 'In GENNF - Either (1) Numerator DF <= 1.0 or' 
      WRITE (*,*) '(2) Denominator DF < 0.0 or ' 
      WRITE (*,*) '(3) Noncentrality parameter < 0.0' 
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd,'XNONC value: ',  &
     &  xnonc                                                           
      STOP 'Degrees of freedom or noncent param our of range in GENNF' 
                                                                        
   10 xnum = gennch(dfn,xnonc)/dfn 
!      GENNF = ( GENNCH( DFN, XNONC ) / DFN ) / ( GENCHI( DFD ) / DFD ) 
      xden = genchi(dfd)/dfd 
      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 20 
      WRITE (*,*) ' GENNF - generated numbers would cause overflow' 
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden 
      WRITE (*,*) ' GENNF returning 1.0E38' 
      gennf = 1.0E38 
      GO TO 30 
                                                                        
   20 gennf = xnum/xden 
   30 RETURN 
                                                                        
      END                                           
