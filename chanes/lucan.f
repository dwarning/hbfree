c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE LUCAN(ALU,NTOT,N,NF,FLAG)
C**************************************************************
C         �O��PO�PAMMA �P�BE�EH�� MATP��� A K B���,           *
C     KAHOH��ECKOM� ��� Y-MATP���.                            *
C**************************************************************
C
C$LARGE: ALU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX ALU(NTOT,NTOT)
      DOUBLE COMPLEX SUM
      INTEGER FLAG
C  �POBEPKA BXO�H�X �AHH�X
      FLAG=0
      IF(NF.GT.N) FLAG=5
C  BHECEH� KOPPEKT�B� �O CPABHEH�� C LUSLV
      IF((NF.GT.NTOT).OR.(N.GT.NTOT)) FLAG=3
      IF((NF.LT.1).OR.(N.LT.1)) FLAG=2
      IF(NTOT.LT.1) FLAG=1
C      IF(FLAG.NE.0) WRITE(6, 95)  FLAG
      IF(FLAG.NE.0) RETURN
C  EC�� N=1,TO KAHOH��A��� MO�HO C��TAT� OKOH�EHHO� -
C  MATP��A CO�EP��T 1 ��EMEHT - ALU(1,1).
      IF(N.EQ.1) RETURN
C  �P�BE�EH�E AKT�BHO� �ACT� MATP��� A K KAHOH��ECKOM�
C  B���. ��A�OHA��H�E ��EMEHT� CPA�� �AHOC�TC� B A.
      DO 85 IROW=NF,N
      SUM=2.D0*ALU(IROW,IROW)
      DO 90 JCOL=NF,N
      SUM=SUM-ALU(IROW,JCOL)
C     WRITE(6, 83) SUM, IROW, JCOL, ALU(IROW,JCOL)
C  83 FORMAT(2X,'LUCAN:SUM=',2(1X,D15.8),'ALU(',I3,I3,')=',2(1X,E12.5))
   90 CONTINUE
      ALU(IROW,IROW)=SUM
C     WRITE(6, 84) IROW, IROW, ALU(IROW,IROW)
C  84 FORMAT(2X,25X,'ALU(',I3,I3,')=',E12.5,2X,E12.5)
   85 CONTINUE
      RETURN
C   95 FORMAT('   LUCAN:'/'   INPUT DATA ERROR. FLAG=',I4)
      END
