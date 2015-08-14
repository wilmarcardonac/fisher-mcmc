!DECK SPOFA                                                             
      SUBROUTINE spofa(a,lda,n,info) 
      INTEGER lda,n,info 
      REAL a(lda,1) 
!                                                                       
!     SPOFA FACTORS A REAL SYMMETRIC POSITIVE DEFINITE MATRIX.          
!                                                                       
!     SPOFA IS USUALLY CALLED BY SPOCO, BUT IT CAN BE CALLED            
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.          
!     (TIME FOR SPOCO) = (1 + 18/N)*(TIME FOR SPOFA) .                  
!                                                                       
!     ON ENTRY                                                          
!                                                                       
!        A       REAL(LDA, N)                                           
!                THE SYMMETRIC MATRIX TO BE FACTORED.  ONLY THE         
!                DIAGONAL AND UPPER TRIANGLE ARE USED.                  
!                                                                       
!        LDA     INTEGER                                                
!                THE LEADING DIMENSION OF THE ARRAY  A .                
!                                                                       
!        N       INTEGER                                                
!                THE ORDER OF THE MATRIX  A .                           
!                                                                       
!     ON RETURN                                                         
!                                                                       
!        A       AN UPPER TRIANGULAR MATRIX  R  SO THAT  A = TRANS(R)*R 
!                WHERE  TRANS(R)  IS THE TRANSPOSE.                     
!                THE STRICT LOWER TRIANGLE IS UNALTERED.                
!                IF  INFO .NE. 0 , THE FACTORIZATION IS NOT COMPLETE.   
!                                                                       
!        INFO    INTEGER                                                
!                = 0  FOR NORMAL RETURN.                                
!                = K  SIGNALS AN ERROR CONDITION.  THE LEADING MINOR    
!                     OF ORDER  K  IS NOT POSITIVE DEFINITE.            
!                                                                       
!     LINPACK.  THIS VERSION DATED 08/14/78 .                           
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.      
!                                                                       
!     SUBROUTINES AND FUNCTIONS                                         
!                                                                       
!     BLAS SDOT                                                         
!     FORTRAN SQRT                                                      
!                                                                       
!     INTERNAL VARIABLES                                                
!                                                                       
      REAL sdot,t 
      REAL s 
      INTEGER j,jm1,k 
!     BEGIN BLOCK WITH ...EXITS TO 40                                   
!                                                                       
!                                                                       
      DO 30 j = 1,n 
          info = j 
          s = 0.0E0 
          jm1 = j - 1 
          IF (jm1.LT.1) GO TO 20 
          DO 10 k = 1,jm1 
              t = a(k,j) - sdot(k-1,a(1,k),1,a(1,j),1) 
              t = t/a(k,k) 
              a(k,j) = t 
              s = s + t*t 
   10     CONTINUE 
   20     CONTINUE 
          s = a(j,j) - s 
!     ......EXIT                                                        
          IF (s.LE.0.0E0) GO TO 40 
          a(j,j) = sqrt(s) 
   30 END DO 
      info = 0 
   40 CONTINUE 
      RETURN 
                                                                        
      END                                           
