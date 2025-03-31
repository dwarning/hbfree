c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE STAB(OM,P1,L1,P2,L2,P3,L3,N)
C *** �O��PO�PAMMA �OPM�POBAH�� S-MATP��� �� �APAHEE
C     �A�AHH�X �HA�EH��
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SUBS/     S(15,15)
      DOUBLE COMPLEX           S,J,Y
      DOUBLE PRECISION              P1(L1),P2(L2),P3(L3)

C  O�PE�E�EH�E ��C�A BBE�EHH�X �ACTOTH�X TO�EK
      M=L3/(2*N**2+1)
      DO 10 I10=1,N
      DO 10 J10=1,N
   10 S(I10,J10)=DCMPLX(0.D0,0.D0)
C  HAXO��M I TAKOE,�TO FI ����E BCE�O K F0
C  �� BCEX FJ �P� J=1,M
      PI=3.14159D0
      F0=OM/(2*PI)
      DF=F0
      KFR=0
      DO 20 I=1,M
      FI=P3(1+(2*N**2+1)*(I-1))
      DF1=DABS(F0-FI)
       IF(DF1.GT.DF) GO TO 20
       DF=DF1
       KFR=I
   20  CONTINUE
C
C  B���C�EH�E IB (I-�A�OBOE)
C
       IB=(KFR-1)*(2*N**2+1)+1
C
       DO 40 I40=1,N
       DO 50 I50=1,N

C  �O�CK �O�����
       IPR=IB+(2*I50-1)+(I40-1)*2*N
       IPI=IPR+1
C  �A�O�HEH�E S-MATP���
       S(I50,I40)=DCMPLX(P3(IPR),P3(IPI))
C      PRINT 25,I50,I40,S(I50,I40)
C  25 FORMAT(2X,'S(',I4,',',I4,')=',E13.6,2X,E13.6)
   50  CONTINUE
   40  CONTINUE
C
C
C *****************************
       CALL TEST(N)
C *****************************







       RETURN
C     DEBUG SUBTRACE,INIT
       END
