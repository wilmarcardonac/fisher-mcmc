      INTEGER FUNCTION lennob(string)
      IMPLICIT INTEGER (a-p,r-z),LOGICAL (q)
!**********************************************************************
!
!     INTEGER FUNCTION LENNOB( STRING )
!                LENgth NOt counting trailing Blanks
!
!
!                              Function
!
!
!     Returns the length of STRING up to and including the last
!     non-blank character.
!
!
!                              Arguments
!
!
!     STRING --> String whose length not counting trailing blanks
!                is returned.
!
!**********************************************************************
      CHARACTER*(*) string

      end = len(string)
      DO 20,i = end,1,-1
          IF (.NOT. (string(i:i).NE.' ')) GO TO 10
          lennob = i
          RETURN

   10     CONTINUE
   20 END DO
      lennob = 0
      RETURN

      END
