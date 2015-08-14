      INTEGER FUNCTION ignlgi() 
!********************************************************************** 
!                                                                       
!     INTEGER FUNCTION IGNLGI()                                         
!               GeNerate LarGe Integer                                  
!                                                                       
!     Returns a random integer following a uniform distribution over    
!     (1, 2147483562) using the current generator.                      
!                                                                       
!     This is a transcription from Pascal to Fortran of routine         
!     Random from the paper                                             
!                                                                       
!     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package   
!     with Splitting Facilities." ACM Transactions on Mathematical      
!     Software, 17:98-111 (1991)                                        
!                                                                       
!********************************************************************** 
!     .. Parameters ..                                                  
      INTEGER numg 
      PARAMETER (numg=32) 
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
      INTEGER curntg,k,s1,s2,z 
      LOGICAL qqssd 
!     ..                                                                
!     .. External Functions ..                                          
      LOGICAL qrgnin 
      EXTERNAL qrgnin 
!     ..                                                                
!     .. External Subroutines ..                                        
      EXTERNAL getcgn,inrgcm,rgnqsd,setall 
!     ..                                                                
!     .. Common blocks ..                                               
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,  &
     &       cg2,qanti                                                  
!     ..                                                                
!     .. Save statement ..                                              
      SAVE /globe/ 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!     IF THE RANDOM NUMBER PACKAGE HAS NOT BEEN INITIALIZED YET, DO SO. 
!     IT CAN BE INITIALIZED IN ONE OF TWO WAYS : 1) THE FIRST CALL TO   
!     THIS ROUTINE  2) A CALL TO SETALL.                                
!                                                                       
      IF (.NOT. (qrgnin())) CALL inrgcm() 
      CALL rgnqsd(qqssd) 
      IF (.NOT. (qqssd)) CALL setall(1234567890,123456789) 
!                                                                       
!     Get Current Generator                                             
!                                                                       
      CALL getcgn(curntg) 
      s1 = cg1(curntg) 
      s2 = cg2(curntg) 
      k = s1/53668 
      s1 = a1* (s1-k*53668) - k*12211 
      IF (s1.LT.0) s1 = s1 + m1 
      k = s2/52774 
      s2 = a2* (s2-k*52774) - k*3791 
      IF (s2.LT.0) s2 = s2 + m2 
      cg1(curntg) = s1 
      cg2(curntg) = s2 
      z = s1 - s2 
      IF (z.LT.1) z = z + m1 - 1 
      IF (qanti(curntg)) z = m1 - z 
      ignlgi = z 
      RETURN 
                                                                        
      END                                           
