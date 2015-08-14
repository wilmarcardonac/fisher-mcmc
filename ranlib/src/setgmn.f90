      SUBROUTINE setgmn(meanv,covm,p,parm) 
!********************************************************************** 
!                                                                       
!     SUBROUTINE SETGMN( MEANV, COVM, P, PARM)                          
!            SET Generate Multivariate Normal random deviate            
!                                                                       
!                                                                       
!                              Function                                 
!                                                                       
!                                                                       
!      Places P, MEANV, and the Cholesky factoriztion of COVM           
!      in GENMN.                                                        
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     MEANV --> Mean vector of multivariate normal distribution.        
!                                        REAL MEANV(P)                  
!                                                                       
!     COVM   <--> (Input) Covariance   matrix    of  the  multivariate  
!                 normal distribution                                   
!                 (Output) Destroyed on output                          
!                                        REAL COVM(P,P)                 
!                                                                       
!     P     --> Dimension of the normal, or length of MEANV.            
!                                        INTEGER P                      
!                                                                       
!     PARM <-- Array of parameters needed to generate multivariate norma
!                deviates (P, MEANV and Cholesky decomposition of       
!                COVM).                                                 
!                1 : 1                - P                               
!                2 : P + 1            - MEANV                           
!                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM  
!                                             REAL PARM(P*(P+3)/2 + 1)  
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      INTEGER p 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL covm(p,p),meanv(p),parm(p* (p+3)/2+1) 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER i,icount,info,j 
!     ..                                                                
!     .. External Subroutines ..                                        
      EXTERNAL spofa 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!                                                                       
!     TEST THE INPUT                                                    
!                                                                       
      IF (.NOT. (p.LE.0)) GO TO 10 
      WRITE (*,*) 'P nonpositive in SETGMN' 
      WRITE (*,*) 'Value of P: ',p 
      STOP 'P nonpositive in SETGMN' 
                                                                        
   10 parm(1) = p 
!                                                                       
!     PUT P AND MEANV INTO PARM                                         
!                                                                       
      DO 20,i = 2,p + 1 
          parm(i) = meanv(i-1) 
   20 END DO 
!                                                                       
!      Cholesky decomposition to find A s.t. trans(A)*(A) = COVM        
!                                                                       
      CALL spofa(covm,p,p,info) 
      IF (.NOT. (info.NE.0)) GO TO 30 
      WRITE (*,*) ' COVM not positive definite in SETGMN' 
      STOP ' COVM not positive definite in SETGMN' 
                                                                        
   30 icount = p + 1 
!                                                                       
!     PUT UPPER HALF OF A, WHICH IS NOW THE CHOLESKY FACTOR, INTO PARM  
!          COVM(1,1) = PARM(P+2)                                        
!          COVM(1,2) = PARM(P+3)                                        
!                    :                                                  
!          COVM(1,P) = PARM(2P+1)                                       
!          COVM(2,2) = PARM(2P+2)  ...                                  
!                                                                       
      DO 50,i = 1,p 
          DO 40,j = i,p 
              icount = icount + 1 
              parm(icount) = covm(i,j) 
   40     CONTINUE 
   50 END DO 
      RETURN 
!                                                                       
      END                                           
