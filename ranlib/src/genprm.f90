      SUBROUTINE genprm(iarray,larray) 
!********************************************************************** 
!                                                                       
!    SUBROUTINE GENPRM( IARRAY, LARRAY )                                
!               GENerate random PeRMutation of iarray                   
!                                                                       
!                                                                       
!                              Arguments                                
!                                                                       
!                                                                       
!     IARRAY <--> On output IARRAY is a random permutation of its       
!                 value on input                                        
!                         INTEGER IARRAY( LARRAY )                      
!                                                                       
!     LARRAY <--> Length of IARRAY                                      
!                         INTEGER LARRAY                                
!                                                                       
!********************************************************************** 
!     .. Scalar Arguments ..                                            
      INTEGER larray 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER iarray(larray) 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER i,itmp,iwhich 
!     ..                                                                
!     .. External Functions ..                                          
      INTEGER ignuin 
      EXTERNAL ignuin 
!     ..                                                                
!     .. Executable Statements ..                                       
      DO 10,i = 1,larray 
          iwhich = ignuin(i,larray) 
          itmp = iarray(iwhich) 
          iarray(iwhich) = iarray(i) 
          iarray(i) = itmp 
   10 END DO 
      RETURN 
                                                                        
      END                                           
