c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c
c Released under GPL v 2.0
c




      SUBROUTINE ZINY(Y,VECTJ,ISIZE_MAXNODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER ISIZE_MAXNODE
      DOUBLE COMPLEX VECTJ(ISIZE_MAXNODE)
      DOUBLE COMPLEX Y(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX ZERO/(0.0D0,0.0D0)/

C     ZERO OUT MATRIX Y AND VECTOR VEKTJ
C      print *, 'ZINY: ',ISIZE_MAXNODE
      DO 500 I=1,ISIZE_MAXNODE
      VECTJ(I)=ZERO
  500 CONTINUE

      DO 501 I=1,ISIZE_MAXNODE
      DO 501 J=1,ISIZE_MAXNODE
  501 Y(J,I)=ZERO

      RETURN
      END
