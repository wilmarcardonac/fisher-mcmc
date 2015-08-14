      LOGICAL FUNCTION qrgnin()
!**********************************************************************
!
!     LOGICAL FUNCTION QRGNIN()
!               Q Random GeNerators INitialized?
!
!     A trivial routine to determine whether or not the random
!     number generator has been initialized.  Returns .TRUE. if
!     it has, else .FALSE.
!
!**********************************************************************
!     .. Scalar Arguments ..
      LOGICAL qvalue
!     ..
!     .. Local Scalars ..
      LOGICAL qinit
!     ..
!     .. Entry Points ..
      LOGICAL qrgnsn
!     ..
!     .. Save statement ..
      SAVE qinit
!     ..
!     .. Data statements ..
      DATA qinit/.FALSE./
!     ..
!     .. Executable Statements ..
      qrgnin = qinit
      RETURN

      ENTRY qrgnsn(qvalue)
!**********************************************************************
!
!     LOGICAL FUNCTION QRGNSN( QVALUE )
!               Q Random GeNerators Set whether iNitialized
!
!     Sets state of whether random number generator is initialized
!     to QVALUE.
!
!     This routine is actually an entry in QRGNIN, hence it is a
!     logical function.  It returns the (meaningless) value .TRUE.
!
!**********************************************************************
      qinit = qvalue
      qrgnsn = .TRUE.
      RETURN

      END
