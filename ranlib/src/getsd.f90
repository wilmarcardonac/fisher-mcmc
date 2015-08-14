      SUBROUTINE getsd(iseed1,iseed2) 
!********************************************************************** 
!                                                                       
!     SUBROUTINE GETSD(G,ISEED1,ISEED2)                                 
!               GET SeeD                                                
!                                                                       
!     Returns the value of two integer seeds of the current generator   
!                                                                       
!     This  is   a  transcription from  Pascal   to  Fortran  of routine
!     Get_State from the paper                                          
!                                                                       
!     L'Ecuyer, P. and  Cote,  S. "Implementing a Random Number  Package
!     with   Splitting Facilities."  ACM  Transactions   on Mathematical
!     Software, 17:98-111 (1991)                                        
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!                                                                       
!     ISEED1 <- First integer seed of generator G                       
!                                   INTEGER ISEED1                      
!                                                                       
!     ISEED2 <- Second integer seed of generator G                      
!                                   INTEGER ISEED1                      
!                                                                       
!********************************************************************** 
!     .. Parameters ..                                                  
      INTEGER numg 
      PARAMETER (numg=32) 
!     ..                                                                
!     .. Scalar Arguments ..                                            
      INTEGER iseed1,iseed2 
!     ..                                                                
!     .. Scalars in Common ..                                           
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2 
!     ..                                                                
!     .. Arrays in Common ..                                            
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),        &
     &        lg2(numg)                                                 
      LOGICAL qanti(numg) 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER g 
!     ..                                                                
!     .. External Functions ..                                          
      LOGICAL qrgnin 
      EXTERNAL qrgnin 
!     ..                                                                
!     .. External Subroutines ..                                        
      EXTERNAL getcgn 
!     ..                                                                
!     .. Common blocks ..                                               
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,  &
     &       cg2,qanti                                                  
!     ..                                                                
!     .. Save statement ..                                              
      SAVE /globe/ 
!     ..                                                                
!     .. Executable Statements ..                                       
!     Abort unless random number generator initialized                  
      IF (qrgnin()) GO TO 10 
      WRITE (*,*) ' GETSD called before random number generator ',      &
     &  ' initialized -- abort!'                                        
      STOP ' GETSD called before random number generator initialized' 
                                                                        
   10 CALL getcgn(g) 
      iseed1 = cg1(g) 
      iseed2 = cg2(g) 
      RETURN 
                                                                        
      END                                           
