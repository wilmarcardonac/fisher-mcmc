      REAL FUNCTION genchi(df) 
!********************************************************************** 
!                                                                       
!     REAL FUNCTION GENCHI( DF )                                        
!                Generate random value of CHIsquare variable            
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!     Generates random deviate from the distribution of a chisquare     
!     with DF degrees of freedom random variable.                       
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     DF --> Degrees of freedom of the chisquare                        
!            (Must be positive)                                         
!                         REAL DF                                       
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Uses relation between chisquare and gamma.                        
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      REAL df 
!     ..                                                                
!     .. External Functions ..                                          
      REAL gengam 
      EXTERNAL gengam 
!     ..                                                                
!     .. Executable Statements ..                                       
      IF (.NOT. (df.LE.0.0)) GO TO 10 
      WRITE (*,*) 'DF <= 0 in GENCHI - ABORT' 
      WRITE (*,*) 'Value of DF: ',df 
      STOP 'DF <= 0 in GENCHI - ABORT' 
                                                                        
   10 genchi = 2.0*gengam(1.0,df/2.0) 
      RETURN 
                                                                        
      END                                           
