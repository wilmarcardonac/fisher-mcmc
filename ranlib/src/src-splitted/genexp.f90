      REAL FUNCTION genexp(av)

!**********************************************************************
!
!     REAL FUNCTION GENEXP( AV )
!
!                    GENerate EXPonential random deviate
!
!
!                              Function
!
!
!     Generates a single random deviate from an exponential
!     distribution with mean AV.
!
!
!                              Arguments
!
!
!     AV --> The mean of the exponential distribution from which
!            a random deviate is to be generated.
!                              REAL AV
!
!     GENEXP <-- The random deviate.
!                              REAL GENEXP
!
!
!                              Method
!
!
!     Renames SEXPO from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!
!               Ahrens, J.H. and Dieter, U.
!               Computer Methods for Sampling From the
!               Exponential and Normal Distributions.
!               Comm. ACM, 15,10 (Oct. 1972), 873 - 882.
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL av
!     ..
!     .. External Functions ..
      REAL sexpo
      EXTERNAL sexpo
!     ..
!     .. Executable Statements ..
      genexp = sexpo()*av
      RETURN

      END
