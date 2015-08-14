      REAL FUNCTION genf(dfn,dfd) 
!********************************************************************** 
!                                                                       
!     REAL FUNCTION GENF( DFN, DFD )                                    
!                GENerate random deviate from the F distribution        
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!     Generates a random deviate from the F (variance ratio)            
!     distribution with DFN degrees of freedom in the numerator         
!     and DFD degrees of freedom in the denominator.                    
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     DFN --> Numerator degrees of freedom                              
!             (Must be positive)                                        
!                              REAL DFN                                 
!      DFD --> Denominator degrees of freedom                           
!             (Must be positive)                                        
!                              REAL DFD                                 
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Directly generates ratio of chisquare variates                    
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      REAL dfd,dfn 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL xden,xnum 
!     ..                                                                
!     .. External Functions ..                                          
      REAL genchi 
      EXTERNAL genchi 
!     ..                                                                
!     .. Executable Statements ..                                       
      IF (.NOT. (dfn.LE.0.0.OR.dfd.LE.0.0)) GO TO 10 
      WRITE (*,*) 'Degrees of freedom nonpositive in GENF - abort!' 
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd 
      STOP 'Degrees of freedom nonpositive in GENF - abort!' 
                                                                        
   10 xnum = genchi(dfn)/dfn 
!      GENF = ( GENCHI( DFN ) / DFN ) / ( GENCHI( DFD ) / DFD )         
      xden = genchi(dfd)/dfd 
      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 20 
      WRITE (*,*) ' GENF - generated numbers would cause overflow' 
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden 
      WRITE (*,*) ' GENF returning 1.0E38' 
      genf = 1.0E38 
      GO TO 30 
                                                                        
   20 genf = xnum/xden 
   30 RETURN 
                                                                        
      END                                           
