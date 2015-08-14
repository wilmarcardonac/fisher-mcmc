      SUBROUTINE initgn(isdtyp) 
!********************************************************************** 
!                                                                       
!     SUBROUTINE INITGN(ISDTYP)                                         
!          INIT-ialize current G-e-N-erator                             
!                                                                       
!     Reinitializes the state of the current generator                  
!                                                                       
!     This is a transcription from Pascal to Fortran of routine         
!     Init_Generator from the paper                                     
!                                                                       
!     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package   
!     with Splitting Facilities." ACM Transactions on Mathematical      
!     Software, 17:98-111 (1991)                                        
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     ISDTYP -> The state to which the generator is to be set           
!                                                                       
!          ISDTYP = -1  => sets the seeds to their initial value        
!          ISDTYP =  0  => sets the seeds to the first value of         
!                          the current block                            
!          ISDTYP =  1  => sets the seeds to the first value of         
!                          the next block                               
!                                                                       
!                                   INTEGER ISDTYP                      
!                                                                       
!********************************************************************** 
!     .. Parameters ..                                                  
      INTEGER numg 
      PARAMETER (numg=32) 
!     ..                                                                
!     .. Scalar Arguments ..                                            
      INTEGER isdtyp 
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
      INTEGER mltmod 
      EXTERNAL qrgnin,mltmod 
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
      WRITE (*,*) ' INITGN called before random number generator ',     &
     &  ' initialized -- abort!'                                        
      STOP ' INITGN called before random number generator initialized' 
                                                                        
   10 CALL getcgn(g) 
      IF ((-1).NE. (isdtyp)) GO TO 20 
      lg1(g) = ig1(g) 
      lg2(g) = ig2(g) 
      GO TO 50 
                                                                        
   20 IF ((0).NE. (isdtyp)) GO TO 30 
      CONTINUE 
      GO TO 50 
!     do nothing                                                        
   30 IF ((1).NE. (isdtyp)) GO TO 40 
      lg1(g) = mltmod(a1w,lg1(g),m1) 
      lg2(g) = mltmod(a2w,lg2(g),m2) 
      GO TO 50 
                                                                        
   40 STOP 'ISDTYP NOT IN RANGE' 
                                                                        
   50 cg1(g) = lg1(g) 
      cg2(g) = lg2(g) 
      RETURN 
                                                                        
      END                                           
