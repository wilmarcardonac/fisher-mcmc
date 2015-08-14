      SUBROUTINE getcgn(g)
      INTEGER g
!**********************************************************************
!
!      SUBROUTINE GETCGN(G)
!                         Get GeNerator
!
!     Returns in G the number of the current random number generator
!
!
!                              Arguments
!
!
!     G <-- Number of the current random number generator (1..32)
!                    INTEGER G
!
!**********************************************************************
!
      INTEGER curntg,numg
      SAVE curntg
      PARAMETER (numg=32)
      DATA curntg/1/
!
      g = curntg
      RETURN

      ENTRY setcgn(g)
!**********************************************************************
!
!     SUBROUTINE SETCGN( G )
!                      Set GeNerator
!
!     Sets  the  current  generator to G.    All references to a generat
!     are to the current generator.
!
!
!                              Arguments
!
!
!     G --> Number of the current random number generator (1..32)
!                    INTEGER G
!
!**********************************************************************
!
!     Abort if generator number out of range
!
      IF (.NOT. (g.LT.0.OR.g.GT.numg)) GO TO 10
      WRITE (*,*) ' Generator number out of range in SETCGN:',          &
     &  ' Legal range is 1 to ',numg,' -- ABORT!'
      STOP ' Generator number out of range in SETCGN'

   10 curntg = g
      RETURN

      END
