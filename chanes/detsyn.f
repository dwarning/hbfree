c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE DETSYN (MAXSYN)
C
C     �/� O�PE�E�EH�� MAX KO�-BA BX/B�X BE����H ��� �P-� ��P�E
C                  ( B���C�EH�� ��� INKOOR )
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/POINT/M(1)/POINTR/NMPNT,NN,NP,LN,LP,NNPR,LENNTP

C
      NR=0
      NB=0
      IEND=NMPNT*20
      DO 10 I=1,IEND,20
      ITY=M(I+5)
      IF(ITY.NE.3) GO TO 10
      NB=MAX0(M(I+17),NB)
      NR=MAX0(M(I+18),NR)
   10 CONTINUE
      MAXSYN=MAX0(NR,NB,1)
      RETURN
      END
