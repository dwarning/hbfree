c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE DECODE(U,NOHAR,ISIZE_MAXNODE)
C
C  �/� �EPEC��K� ��EMEHOB BEKTOPA-PE�EH�� BH�TP� CAMO�O
C  BEKTOPA-PE�EH�� ��� �P�BE�EH�� H�MEPA��� ���OB
C  B COOTBETCTB�E C �CXO�HO� H�MEPA��E�.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/KOLNAL/KOL,NAL
      COMMON/NOUZL /NOUZ,INOUZ1

      LOGICAL NAL(4)
      INTEGER ISIZE_MAXNODE
      INTEGER KOL(4),NTOT,NOUZ(100),INOUZ1(100),TERM
      DOUBLE COMPLEX S,U(1),UT(ISIZE_MAXNODE)

C  O��EE KO�-BO ���OB:
      NTOT=KOL(1)+KOL(2)+KOL(3)
C     WRITE(6, 810) (IVA,NOUZ(IVA),IVA=1,NTOT)
C 810 FORMAT(2X,'DECODE: NOUZ(',I3,')=',I4)
C     WRITE(6, 815) NTOT
C 815 FORMAT(2X,'NTOT=',I5)

C  BO BCEX ���AX
      DO 10 I=1,NTOT
C  �POBE�EM �EPEC��K� ��  U  B UT   B COOTBETCTB�� C
C  BEKTOPOM OTO�PA�EH��  NOUZ    /CM. �/� SORTUZ/
      TERM=NOUZ(I)
   10 UT(TERM)=U(I)
C  TE�EP� �EPE���EM �� C��E�HO�O BEKTOPA UT
C  B BEKTOP PE�EH�� U:
      DO 15 I=1,NTOT
C     WRITE(6, 14) I,UT(I)
C  14 FORMAT(2X,'DECODE: U(',I3,')=',E12.5,2X,E12.5)
   15 U(I)=UT(I)
      RETURN
C     DEBUG SUBTRACE,SUBCHK,INIT(I,TERM)
      END
