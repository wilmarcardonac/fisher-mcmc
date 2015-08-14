      REAL FUNCTION gennch(df,xnonc) 
!********************************************************************** 
!                                                                       
!     REAL FUNCTION GENNCH( DF, XNONC )                                 
!           Generate random value of Noncentral CHIsquare variable      
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
                                                                        
!     Generates random deviate  from the  distribution  of a  noncentral
!     chisquare with DF degrees  of freedom and noncentrality  parameter
!     XNONC.                                                            
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     DF --> Degrees of freedom of the chisquare                        
!            (Must be > 1.0)                                            
!                         REAL DF                                       
!                                                                       
!     XNONC --> Noncentrality parameter of the chisquare                
!               (Must be >= 0.0)                                        
!                         REAL XNONC                                    
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     Uses fact that  noncentral chisquare  is  the  sum of a  chisquare
!     deviate with DF-1  degrees of freedom plus the  square of a normal
!     deviate with mean XNONC and standard deviation 1.                 
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      REAL df,xnonc 
!     ..                                                                
!     .. External Functions ..                                          
      REAL genchi,gennor 
      EXTERNAL genchi,gennor 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC sqrt 
!     ..                                                                
!     .. Executable Statements ..                                       
      IF (.NOT. (df.LE.1.0.OR.xnonc.LT.0.0)) GO TO 10 
      WRITE (*,*) 'DF <= 1 or XNONC < 0 in GENNCH - ABORT' 
      WRITE (*,*) 'Value of DF: ',df,' Value of XNONC',xnonc 
      STOP 'DF <= 1 or XNONC < 0 in GENNCH - ABORT' 
                                                                        
   10 gennch = genchi(df-1.0) + gennor(sqrt(xnonc),1.0)**2 
      RETURN 
                                                                        
      END                                           
