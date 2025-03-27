c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE FTMAS2(ZNN,KR,KC,NNR,KNR,KNC,KN,                  
     +                  NR,NB,M1,B1,B2,IS,FLGFFT,*)
C
C-----------------------------------------------------------------------
C
C     �O��PO�PAMMA PEA����ET �B�MEPHOE O�PATHOE ��� ,
C  B�XO� HA �POME��TO�HOE HE��HE�HOE �PEO�PA�OBAH�E
C  � �B�MEPHOE �P�MOE ��� �B�MEPH�X PA�PE�EHH�X BEKTOPOB.
C
C     ��EHT���KATOP� �EPEMEHH�X � MACC�BOB :
C
C          ZNN - ��AKOBAHH�� MACC�B AM���T��;
C          KNC - ���HA O�HOMEPHO�O ���;
C          NNR - MACC�B HOMEPOB HEH��EB�X CTPOK;
C          KNR - KO���ECTBO HEH��EB�X CTPOK;
C          KR  - MACC�B HOMEPOB CTO���OB HEH��EB�X
C                ��EMEHTOB B KA��O� HEH��EBO� CTPOKE;
C          KC  - MACC�B, CO�EP�A��� KO���ECTBO
C                HEH��EB�X ��EMEHTOB B KA��O� CTPOKE;
C          B1  - PA�O��� MACC�B ��� XPAHEH��
C                PE����TATOB O�HOMEPHO�O ���;
C          B2  - ���EP ��� XPAHEH�� �PEO�PA�OBAHH�X
C                CTPOK;
C          NR  - KO���ECTBO O�HOBPEMEHHO �PEO�PA��EM�X
C                AM���T�� ZNN �P� O�PATHOM ���;
C          NB  - KO���ECTBO O�HOBPEMEHHO �PEO�PA��EM�X
C                BEKTOPOB BPEMEHH�X OTC�ETOB
C                �P� �P�MOM ���.
C
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON / MEP / MEPHF, FLGMNW
      DOUBLE COMPLEX ZNN(KN,NR),B1(KNC,NR),B2(KNC,KNR,NR),ZERO/(0.0D0,0.
     +0D0)/
             INTEGER        FLGMNW,FLGFFT
      INTEGER  KR(1),KC(1),NNR(1)
C     INTEGER  INV(32),S(32),M(3)/0,0,0/
      DIMENSION INV(32),S(32)
      INTEGER M(3)/0,0,0/
      SAVE INV,S
C  MACC�B�    INV(32),S(32),M(3) - PA�O��E ��� �/� HARM
C     PRINT 976
C 976 FORMAT('    ZNN:')
C     PRINT 975,((ZNN(I,J),         J=1,NR), I=1,KN)
C 975 FORMAT(4(2X,E12.5))
C 977 FORMAT('    B1:')

      M(1)=M1
      IF (FLGFFT.NE.0) GO TO 5
      CALL HARM (B1,M,INV,S,0,IFERR)
      FLGFFT=1
    5 KNC1=KNC/2
      KNC2=KNC+2
C
C  �OPM�POBAH�E MACC�BA CTPOK
C
      DO 30 L=1,NR
      N1=1
      N2=0
      DO 30 K=1,KNR
        DO 10 I=1,KNC
   10   B1(I,L)=ZERO
      N2=N2+KC(K)
        DO 20 N=N1,N2
        I=KR(N)
   20   B1(I,L)=ZNN(N,L)
      N1=N2+1
      IF (MEPHF.EQ.0) GO TO 30
      IF (NNR(K).NE.1) GO TO 25
        DO 22 I=2,KNC1
        K1=KNC2-I
   22   B1(K1,L)=DCONJG(B1(I,L))
C  �PEO�PA�OBAH�E ��P�E (�P�MOE) CTPOK�
   25 CALL HARM(B1(1,L),M,INV,S,2,IFERR)
      IF (MEPHF.EQ.1) GO TO 30
      DO 27 I=1,KNC
   27 B2(I,K,L)=B1(I,L)
   30 CONTINUE
C
C  O�PA�OTKA CTO���OB �� MACC�BA CTPOK
C
      IS=KNC
      IF (MEPHF.NE.2) GO TO 1056
      IS=1
   34 DO 56 L=1,NR
   35 DO 40 N=1,KNC
   40 B1(N,L)=ZERO
      DO 50 K=1,KNR
      N=NNR(K)
   50 B1(N,L)=B2(IS,K,L)
      DO 55 J=2,KNC1
      K1=KNC2-J
   55 B1(K1,L)=DCONJG(B1(J,L))
C  �PEO�PA�OBAH�E ��P�E (�P�MOE) CTO���A
   56 CALL HARM(B1(1,L),M,INV,S,2,IFERR)
 1056 RETURN 1
C-----------------------------------------------------------------------
      ENTRY FT2(ZNN,KR,KC,NNR,KNR,KNC,KN,          
     +          NR,NB,B1,B2,IS,FLGFFT,*)
C
      IF (NB.GT.0) GO TO 1057
      IS=IS+1
      IF (IS.GT.KNC) GO TO 95
      GO TO 34
 1057 IF (MEPHF.NE.2) GO TO 65
C  �PEO�PA�OBAH�E ��P�E (O�PATHOE) CTO���A
   57 DO 60 LP=1,NB
   58 CALL HARM(B1(1,LP),M,INV,S,-2,IFERR)
      DO 60 K=1,KNR
      N=NNR(K)
   60 B2(IS,K,LP)=B1(N,LP)
      IS=IS+1
      IF(IS.LE.KNC) GO TO 34
C
C  O�PA�OTKA CTPOK �� MACC�BA CTO���OB
   65 DO 90 LP=1,NB
      N1=1
      N2=0
      DO 90 K=1,KNR
      IF (MEPHF.EQ.0) GO TO 76
      IF (MEPHF.EQ.1) GO TO 75
      DO 70 I=1,KNC
   70 B1(I,LP)=B2(I,K,LP)
C  �PEO�PA�OBAH�E ��P�E (O�PATHOE) CTPOK�
   75 CONTINUE
C     PRINT 977
C     PRINT 975,(B1(II,LP),II=1,KNC)
      CALL HARM(B1(1,LP),M,INV,S,-2,IFERR)
C     PRINT 977
C     PRINT 975,(B1(II,LP),II=1,KNC)
   76 N2=N2+KC(K)
      DO 80 N=N1,N2
      I=KR(N)
   80 ZNN(N,LP)=B1(I,LP)
   90 N1=N2+1
   95 CONTINUE
C     PRINT 976
C     PRINT 975,((ZNN(I,J),J=1,NB),I=1,KN)
      RETURN
C     DEBUG SUBTRACE,INIT(N1,N2,IFERR,M,FLGFFT)
      END
