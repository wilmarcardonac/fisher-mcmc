      SUBROUTINE genmn(parm,x,work) 
!********************************************************************** 
!                                                                       
!     SUBROUTINE GENMN(PARM,X,WORK)                                     
!              GENerate Multivariate Normal random deviate              
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     PARM --> Parameters needed to generate multivariate normal        
!               deviates (MEANV and Cholesky decomposition of           
!               COVM). Set by a previous call to SETGMN.                
!               1 : 1                - size of deviate, P               
!               2 : P + 1            - mean vector                      
!               P+2 : P*(P+3)/2 + 1  - upper half of cholesky           
!                                       decomposition of cov matrix     
!                                             REAL PARM(*)              
!                                                                       
!     X    <-- Vector deviate generated.                                
!                                             REAL X(P)                 
!                                                                       
!     WORK <--> Scratch array                                           
!                                             REAL WORK(P)              
!                                                                       
!                                                                       
!                              Method                                   
!                                                                       
!                                                                       
!     1) Generate P independent standard normal deviates - Ei ~ N(0,1)  
!                                                                       
!     2) Using Cholesky decomposition find A s.t. trans(A)*A = COVM     
!                                                                       
!     3) trans(A)E + MEANV ~ N(MEANV,COVM)                              
!                                                                       
!********************************************************************** 
!     .. Array Arguments ..                                             
      REAL parm(*),work(*),x(*) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL ae 
      INTEGER i,icount,j,p 
!     ..                                                                
!     .. External Functions ..                                          
      REAL snorm 
      EXTERNAL snorm 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC int 
!     ..                                                                
!     .. Executable Statements ..                                       
      p = int(parm(1)) 
!                                                                       
!     Generate P independent normal deviates - WORK ~ N(0,1)            
!                                                                       
      DO 10,i = 1,p 
          work(i) = snorm() 
   10 END DO 
      DO 30,i = 1,p 
!                                                                       
!     PARM (P+2 : P*(P+3)/2 + 1) contains A, the Cholesky               
!      decomposition of the desired covariance matrix.                  
!          trans(A)(1,1) = PARM(P+2)                                    
!          trans(A)(2,1) = PARM(P+3)                                    
!          trans(A)(2,2) = PARM(P+2+P)                                  
!          trans(A)(3,1) = PARM(P+4)                                    
!          trans(A)(3,2) = PARM(P+3+P)                                  
!          trans(A)(3,3) = PARM(P+2-1+2P)  ...                          
!                                                                       
!     trans(A)*WORK + MEANV ~ N(MEANV,COVM)                             
!                                                                       
          icount = 0 
          ae = 0.0 
          DO 20,j = 1,i 
              icount = icount + j - 1 
              ae = ae + parm(i+ (j-1)*p-icount+p+1)*work(j) 
   20     CONTINUE 
          x(i) = ae + parm(i+1) 
   30 END DO 
      RETURN 
!                                                                       
      END                                           
