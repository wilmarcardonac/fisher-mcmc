      SUBROUTINE phrtsd(phrase,seed1,seed2) 
!********************************************************************** 
!                                                                       
!     SUBROUTINE PHRTSD( PHRASE, SEED1, SEED2 )                         
!               PHRase To SeeDs                                         
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!     Uses a phrase (character string) to generate two seeds for the RGN
!     random number generator.                                          
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     PHRASE --> Phrase to be used for random number generation         
!                         CHARACTER*(*) PHRASE                          
!                                                                       
!     SEED1 <-- First seed for RGN generator                            
!                         INTEGER SEED1                                 
!                                                                       
!     SEED2 <-- Second seed for RGN generator                           
!                         INTEGER SEED2                                 
!                                                                       
!                                                                       
!                              Note                                     
!                                                                       
!                                                                       
!     Trailing blanks are eliminated before the seeds are generated.    
!                                                                       
!     Generated seed values will fall in the range 1..2^30              
!     (1..1,073,741,824)                                                
!                                                                       
!********************************************************************** 
!     .. Parameters ..                                                  
      CHARACTER*(*) table 
      PARAMETER (table='abcdefghijklmnopqrstuvwxyz'//                   &
     &          'ABCDEFGHIJKLMNOPQRSTUVWXYZ'//'0123456789'//            &
     &          '!@#$%^&*()_+[];:''"<>?,./')                            
      INTEGER twop30 
      PARAMETER (twop30=1073741824) 
!     ..                                                                
!     .. Scalar Arguments ..                                            
      INTEGER seed1,seed2 
      CHARACTER phrase* (*) 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER i,ichr,j,lphr 
!     ..                                                                
!     .. Local Arrays ..                                                
      INTEGER shift(0:4),values(5) 
!     ..                                                                
!     .. External Functions ..                                          
      INTEGER lennob 
      EXTERNAL lennob 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC index,mod 
!     ..                                                                
!     .. Data statements ..                                             
      DATA shift/1,64,4096,262144,16777216/ 
!     ..                                                                
!     .. Executable Statements ..                                       
      seed1 = 1234567890 
      seed2 = 123456789 
      lphr = lennob(phrase) 
      IF (lphr.LT.1) RETURN 
      DO 30,i = 1,lphr 
          ichr = mod(index(table,phrase(i:i)),64) 
          IF (ichr.EQ.0) ichr = 63 
          DO 10,j = 1,5 
              values(j) = ichr - j 
              IF (values(j).LT.1) values(j) = values(j) + 63 
   10     CONTINUE 
          DO 20,j = 1,5 
              seed1 = mod(seed1+shift(j-1)*values(j),twop30) 
              seed2 = mod(seed2+shift(j-1)*values(6-j),twop30) 
   20     CONTINUE 
   30 END DO 
      RETURN 
                                                                        
      END                                           
