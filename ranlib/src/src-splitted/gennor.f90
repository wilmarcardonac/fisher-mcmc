      REAL FUNCTION gennor(av,sd)
!**********************************************************************
!
!     REAL FUNCTION GENNOR( AV, SD )
!
!         GENerate random deviate from a NORmal distribution
!
!
!                              Function
!
!
!     Generates a single random deviate from a normal distribution
!     with mean, AV, and standard deviation, SD.
!
!
!                              Arguments
!
!
!     AV --> Mean of the normal distribution.
!                              REAL AV
!
!     SD --> Standard deviation of the normal distribution.
!                              REAL SD
!
!     GENNOR <-- Generated normal deviate.
!                              REAL GENNOR
!
!
!                              Method
!
!
!     Renames SNORM from TOMS as slightly modified by BWB to use RANF
!     instead of SUNIF.
!
!     For details see:
!               Ahrens, J.H. and Dieter, U.
!               Extensions of Forsythe's Method for Random
!               Sampling from the Normal Distribution.
!               Math. Comput., 27,124 (Oct. 1973), 927 - 937.
!
!
!**********************************************************************
!     .. Scalar Arguments ..
      REAL av,sd
!     ..
!     .. External Functions ..
      REAL snorm
      EXTERNAL snorm
!     ..
!     .. Executable Statements ..
      gennor = sd*snorm() + av
      RETURN

      END
