      REAL FUNCTION ranf() 
!********************************************************************** 
!                                                                       
!     REAL FUNCTION RANF()                                              
!                RANDom number generator as a Function                  
!                                                                       
!     Returns a random floating point number from a uniform distribution
!     over 0 - 1 (endpoints of this interval are not returned) using the
!     current generator                                                 
!                                                                       
!     This is a transcription from Pascal to Fortran of routine         
!     Uniform_01 from the paper                                         
!                                                                       
!     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package   
!     with Splitting Facilities." ACM Transactions on Mathematical      
!     Software, 17:98-111 (1991)                                        
!                                                                       
!********************************************************************** 
!     .. External Functions ..                                          
      INTEGER ignlgi 
      EXTERNAL ignlgi 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!     4.656613057E-10 is 1/M1  M1 is set in a data statement in IGNLGI  
!      and is currently 2147483563. If M1 changes, change this also.    
!                                                                       
      ranf = ignlgi()*4.656613057E-10 
      RETURN 
                                                                        
      END                                           
