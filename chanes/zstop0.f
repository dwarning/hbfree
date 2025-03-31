c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE STOP0(N,F,X,SF,SX,ICODE,KMAXDU)
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION F(1), X(1)
      DOUBLE PRECISION SF(1),SX(1)
      INTEGER ICODE,KMAXDU

      COMMON/NEWTON/EPSSOL,EPSDU,EPSMIN,MAXDU,LIMIT
      DOUBLE PRECISION  EPSSOL,EPSDU,EPSMIN,MAXDU
      COMMON/PRINT /KPRLEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,              KPR
     +VAR,KPRGRF,KPRQUP

      DOUBLE PRECISION CONV

C      �CTAHOB�T� C�ET��K KO���ECTBA
C  MAKC�MA��H�X �A�OB HA 0
      KMAXDU=0

C      O�PE�E�EH�E HEB��K� B HA�A��HO� TO�KE
      ICODE=0
      CONV=0.0D0
      DO 10 J=1,N,1
   10 CONV=DMAX1(CONV,SF((J+1)/2)*DABS(F(J)))
      IF(CONV.LE.1.D-02*EPSSOL)ICODE=1

C  �E�AT� COO��EH��.
      IF(ICODE.EQ.1.AND.KPRSOL.GT.0) WRITE(6, 100) CONV
      IF(ICODE.EQ.1)                 PRINT    100, CONV

      RETURN
  100 FORMAT(2X,'     INITIAL POINT IS SOLUTION OF '/       2X,' SIMULTA
     +NEOUS EQUATIONS. TOLERANCE:',E13.6   )
C     DEBUG SUBTRACE,INIT
      END


