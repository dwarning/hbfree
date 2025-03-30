c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE NEJAC(NTOT,U,DFDX,FLAG,FLGFFT)
C******************************************************************
C                                                                 *
C     �/� B���C��ET HA I-� �TEPA���                               *
C              �KO��AH  DFDX                                      *
C                                                                 *
C******************************************************************
C                                                                 *
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EQUIVALENCE      (K3,KOL(3))
      include 'circuit.i'
      include 'funcsize.i'
C      COMMON/POINTR/   NMPNT,NNODE,NPARAM,LENNOD,LENPAR,NNETPR,LENNTP
      COMMON/KOLNAL/   KOL(4),NAL(4)
C      COMMON/POINT/    MPOINT(1)
C      COMMON/NODEL/    NODEEL(1)
C      COMMON/PARAMS/   PARAM(1)

      COMMON/MATY/     BUFFER (6000),BUFLEN
      DOUBLE COMPLEX          BUFFER
      INTEGER*4        BUFLEN


      LOGICAL          NAL
      COMMON/BLK1/     KR,KR1,KC,KC1,NNR,NNR1,MN,MN1
      COMMON/BLK2/     KNC,KNR,KN,KNR1,KN1
      COMMON/BLW1/     W,W1
      COMMON/BLW2/     WR,WS
      DOUBLE PRECISION             W(20),W1(200)
      INTEGER          KR(20),KC(10),NNR(10),MN(2,20)
      INTEGER          KR1(200),KC1(20),NNR1(20),MN1(2,200)
      INTEGER          WR(20,20),WS(20,20)
C  ��� ������ NAME � �������� MPOINT ������ ���� CHARACTER*4
      INTEGER          NAME(4),NOI(5,2),NOU(5,2),NU1(5,2),NU2(5,2)
      INTEGER          KOUV(5),KOPV(5),KOI,NR1V(5),NB1V(5),NG,INDEP
      DOUBLE COMPLEX  U(1),DFDX(NTOT,2,1)
      DOUBLE COMPLEX  B1(B1_SIZE) ,B2(B2_SIZE)
      DOUBLE COMPLEX  UNEL(MAXU_MD*MAXKN1),DUNDT(MAXD_MD*MAXKN1)
      DOUBLE COMPLEX  ZNN1((MAXU_MD+MAXD_MD)*MAXKN1)

C..............PA�MEPHOCT� -�TO�H�T�!!!.....
      DOUBLE COMPLEX          EW,G,GI
      DOUBLE COMPLEX          SUM1,SUM2,DIFF1,DIFF2
      DOUBLE COMPLEX          IM/(0.0D0,1.0D0)/,ZERO/(0.0D0,0.0D0)/
      DOUBLE COMPLEX          U1,U2
      LOGICAL          FLAG,EXS(5,2),EXIST(5,2)
      INTEGER          LENF/30/
C..............BH�MAH�E! MAX P-HOCT� F(600=20*30).....

C  ��������� �� 30.01.91    ������� ������ �.�.   ��. MAIN
C     ���������� ������� ��� ����������:
c      IFIND1(I,M,NU)=(I+(M-1)*NU)
      IFIND2(I,J,M,NU,MF)=NU*MF+I+(J-1)*NU+(M-1)*NU*NU


      N1=NNETPR
      L1=LENNTP
C  KO�-BO �PABHEH�� = KO�-BO �ACTOT + KO�-BO ���OB
      KUR=KN*K3
C  O�H��EH�E �KO��AHA ( DFDX )
      KUR1=KUR+1
      DO 75 I=1,KUR
      DO 75 J=1,KUR
      DFDX(J,1,I)=ZERO
   75 DFDX(J,2,I)=ZERO
C  EC�� �PABHEH�� �O���E,�EM PA�MEPHOCT� C�CTEM�
C  �EPEXO� HA METK� ..600..
      IF(KUR.GT.NTOT) GO TO 600
C  EC�� KO�-BO HEH��EB�X CTPOK �O���E 10, �EPEXO�
C  HA METK� ..500..
      IF(KNR.GT.10) GO TO 500

C  O�PE�E�EH�E KPATHOCT� 2-KE KNC. KNC-PA�MEPHOCT� �PEO�PA-
C  �OBAH�� ��P�E.
      K=8
      DO 20 I=3,7
      IF(KNC.LE.K) GO TO 25
   20 K=K+K
      GO TO 500
C............�TO O��C�OB�EHO /�O-MOEM�/ PA�MEPHOCT�� B1...
   25 KNC=K
      M1=I
      KNC2=KNC+KNC
C  ��K� �O�CKA HE��HE�H�X ��-TOB
C  B���C�EH�E �A�A ��� �O�CKA HE��HE�H�X ��EMEHTOB.
C  NMPNT-KO���ECTBO T��OB ��EMEHTOB.
      I1END=20*NMPNT
      DO 100 I1=1,I1END,20
      IF(MPOINT(I1+5).NE.3) GO TO 100
C  EC�� HA��EH ��HE�H�� ��EMEHT, �EPEXO� HA METK� ..100..
      I2E=MPOINT(I1+4)
C  I2E-KO���ECTBO ��EMEHTOB �AHHO�O T��A.
      IF(I2E.EQ.0)GO TO 100
      NF=MPOINT(I1+9)
      LE=MPOINT(I1+6)
      N2=MPOINT(I1+11)
C  L2-KO���ECTBO �APAMETPOB �AHHO�O T��A ��EMEHTOB.
      L2=MPOINT(I1+10)
C  O�PE�E�EH�E HA�BAH�� T��A ��EMEHTA
      NAME(1)=MPOINT(I1)
      NAME(2)=MPOINT(I1+1)
      NAME(3)=MPOINT(I1+2)
      NAME(4)=MPOINT(I1+3)
      CALL LIBMD5(NAME,NOI,NOU,EXIST,KOI,KOUV,KOPV,NR1V,NB1V)
C  ��K� �O ��-TAM B T��E
      DO 110 I2=1,I2E
      L3=MPOINT(I1+12)
C  N3=MPOINT(I1+13)+(I2-1)*L3 -TAK �E�AT� HE���� !!! �E�AT� H��HO TAK:
      KNODES=MPOINT(I1+7)
      NA=NF+(I2-1)*LE
      N3=NODEEL(NA+KNODES+3)
C  ��K� �O HE�AB�C�M�M (ME��� CO�O�) �CTO�H�KAM TOKA
      NADRU=1
      DO 110 INDEP=1,KOI
      NG=INDEP

      KOU=KOUV(NG)
      KOP=KOPV(NG)
      NR=KOU+KOP
      NB=1
      NR1=NR1V(NG)
      NB1=NB1V(NG)
C  O�PE�E�EH�E HOMEPA ���A �P��O�EH�� ��PAB����E�O HA�P�-
C  �EH�� (NU1) � TOKA (NU2)
C   � �EPEC��KA �P��HAKOB �� EXIST B EXS ,OTHOC���XC� K TEK��E�
C   �P���E
      DO 22 I=1,KOU
      NU1(I,1)=NODEEL(NA+NOU(NADRU,1))
      NU1(I,2)=NODEEL(NA+NOU(NADRU,2))
      EXS(I,1)=EXIST(NADRU,1)
      EXS(I,2)=EXIST(NADRU,2)
   22 NADRU=NADRU+1
      NU2(1,1)=NODEEL(NA+NOI(INDEP,1))
      NU2(1,2)=NODEEL(NA+NOI(INDEP,2))

C     PRINT 6543,IM
C  �A�O�HEH�E ZNN,UNEL,DUNDT
C        BCE�O: UNEL(KN*KOU)
C               DUNDT(KN*KOP)
C               ZNN(KN*(KOU+KOP))
C     MPOINT(I1+17)=KOU+KOP
C
      KUNEL=KN*KOU
      KDUN=KN*KOP
C     KZNN=(KOU+KOP)*KN
      DO 30 I=1,KUNEL
   30 UNEL(I)=ZERO
      IF(KDUN.EQ.0) GO TO 37
      DO 35 I=1,KDUN
   35 DUNDT(I)=ZERO
   37 CONTINUE
C  �A�O�HEH�E UNEL,DUNDT,ZNN
C  �A�O�H�ETC� KOU '��OKOB' ...
      IDU=0
      DO 40 IU=1,KOU
      IUBEG=(IU-1)*KN
C  �O KN ��-TOB
      DO 43 JW=1,KN
      JWBEG=(JW-1)*K3
      U1=ZERO
      U2=ZERO
      IF(NU1(IU,1).NE.0) U1=U(JWBEG+NU1(IU,1))
      IF(NU1(IU,2).NE.0) U2=U(JWBEG+NU1(IU,2))
      UNEL(IUBEG+JW)=U1-U2
   43 CONTINUE
      IF(.NOT.EXS(IU,2)) GO TO 40
      IDU=IDU+1
      DO 47 JW=1,KN
      IDUBEG=(IDU-1)*KN
      DUNDT(IDUBEG+JW)=IM*W(JW)*UNEL(IUBEG+JW)
   47 CONTINUE
   40 CONTINUE

C    �OCTPOEH�E UNEL  � DUNDT HA PAC��PEHHO� CETKE
C    !! BE����HA NR1 �PE��O�A�AETC� PABHO� KOU+KOP
      KZNN1=NR1*KN1
      DO 60 J=1,KZNN1
   60 ZNN1(J)=ZERO
C  ���HHA TO� �ACT� ZNN1 ,B KOTOPO� CO�EP�ATC� HA�P��EH�� UNEL
      KUNEL1=KN1*KOU
C  CO�CTBEHHO PACC��KA UNEL � DUNDT B ZNN1
      JJ=0
      DO 65 J=1,KOU
      IF(EXS(J,2))JJ=JJ+1
      J1KN1=(J-1)*KN1
      J1KN=(J-1)*KN
      DO 65 I=1,KN
      LS=WS(I,1)
      ZNN1(J1KN1+LS)=UNEL(J1KN+I)
      IF(.NOT.EXS(J,2))GO TO 64
      INDZN1=KUNEL1+LS+(JJ-1)*KN1
      ZNN1(INDZN1)=DUNDT(I+J1KN)
   64 CONTINUE
   65 CONTINUE

C     �PEO�PA�OBAH�E ��P�E - B���C�EH�E DI/DU

      IF=0
      CALL FTMAS2(ZNN1,KR1,KC1,NNR1,KNR1,KNC,KN1,NR1,NB1,M1,B1,B2,IF,   
     +                                            FLGFFT,*70)
   70 CALL LIBMD4(NAME,NG,N1,L1,N2,L2,N3,L3,B1,KNC2,NR1,*300)
      CALL FT2(ZNN1,KR1,KC1,NNR1,KNR1,KNC,KN1,NR1,NB1,B1,B2,IF,         
     +                                      FLGFFT,*70)
C !    CM. �PE���PE��EH�� B �EPBOM B��OBE

C  �A�O�HEH�E DF/DX
      N0=KN1*KOU
C  $-  ���HHA �ACT� ZNN1 ,CO�EP�A�A� DI/DU
C  OCTAB�A�C� �ACT� CO�EP��T DI/D(DU/DT)
      N01=0
      N02=0
      KKI=1
C     INDEP,NG     -�KB�BA�EHTH�E BE����H�.
C
      DO 200 KKU=1,KOU
      N01=N01+1
      IF(EXS(KKU,2))N02=N02+1
C T.E.EC�� ��OK �� KN1 ��EMEHTOB �PO��EH-�EPEXO��M K C�E����EM�
      ISB=0
      DO 250 IS=1,KN
      EW=IM*W(IS)
      IRB=0
      DO 310 IR=1,KN
      SUM1=ZERO
      SUM2=ZERO
      DIFF1=ZERO
      DIFF2=ZERO

      N001=(N01-1)*KN1+1
      CALL SUMDIF(ZNN1(N001),IR,IS,SUM1,DIFF1)
   80 CONTINUE
      IF(.NOT.EXS(KKU,2))GOTO 90
      N002=N0+(N02-1)*KN1+1
      CALL SUMDIF(ZNN1(N002),IR,IS,SUM2,DIFF2)
   90 CONTINUE
C      WRITE(6, 120) SUM1,EW,DIFF2,G,
C     *              IM,DIFF1,IS,W(IS),SUM2,GI
C  120 FORMAT(2X,'ZNEJAC: SUM1=',E12.6,',',E12.6,' EW=',E12.6,',',E12.6/
C     *       2X,'        DIFF2=',E12.6,',',E12.6,' G=',E12.6,',',E12.6/
C     *       2X,'       IM=',E12.6,',',E12.6,' DIFF1=',E12.6,',',E12.6/
C     *       2X,'       W(',I5,')=',E12.6,' SUM2=',E12.6,',',E12.6/
C     *       2X,'       GI=',E12.6,',',E12.6)
      G=SUM1+EW*DIFF2
      GI=IM*DIFF1-W(IS)*SUM2
      IB1=IRB+NU2(KKI,1)
      IB2=IRB+NU2(KKI,2)
      IS1=ISB+NU1(KKU,1)
      IS2=ISB+NU1(KKU,2)
C
C  �A�O�HEH�E MATP��� Y. EC�� ��E� �O�K���EH�� - HEH��EBO�.
      IF(NU1(KKU,1).NE.0.AND.NU2(KKI,1).NE.0)       DFDX(IB1,1,IS1)=DFDX
     +(IB1,1,IS1)+G
      IF(NU1(KKU,2).NE.0.AND.NU2(KKI,2).NE.0)       DFDX(IB2,1,IS2)=DFDX
     +(IB2,1,IS2)+G
      IF(NU1(KKU,2).NE.0.AND.NU2(KKI,1).NE.0)       DFDX(IB1,1,IS2)=DFDX
     +(IB1,1,IS2)-G
      IF(NU1(KKU,1).NE.0.AND.NU2(KKI,2).NE.0)       DFDX(IB2,1,IS1)=DFDX
     +(IB2,1,IS1)-G

      IF(NU1(KKU,1).NE.0.AND.NU2(KKI,1).NE.0)       DFDX(IB1,2,IS1)=DFDX
     +(IB1,2,IS1)+GI
      IF(NU1(KKU,2).NE.0.AND.NU2(KKI,2).NE.0)       DFDX(IB2,2,IS2)=DFDX
     +(IB2,2,IS2)+GI
      IF(NU1(KKU,2).NE.0.AND.NU2(KKI,1).NE.0)       DFDX(IB1,2,IS2)=DFDX
     +(IB1,2,IS2)-GI
      IF(NU1(KKU,1).NE.0.AND.NU2(KKI,2).NE.0)       DFDX(IB2,2,IS1)=DFDX
     +(IB2,2,IS1)-GI

  310 IRB=IRB+K3
      ISB=ISB+K3
  250 CONTINUE
  200 CONTINUE
  110 CONTINUE
  100 CONTINUE

C  �AHECEH�E Y-MATP���
      IDIAG=0
      DO 140 IBL=1,KN
      DO 150 ITR=1,K3
      IDR=IDIAG+ITR
      DO 150 ITS=1,K3
      IDS=IDIAG+ITS

C  ��������� �� 30.01.91    ������� ������ �.�.
      DFDX(IDS,1,IDR)=DFDX(IDS,1,IDR)+ BUFFER(IFIND2(ITS,ITR,IBL,K3,KN))
      IF(IDIAG.NE.0) DFDX(IDS,2,IDR)=DFDX(IDS,2,IDR)+ IM*BUFFER(IFIND2(I
     +TS,ITR,IBL,K3,KN))

  150 CONTINUE
      IDIAG=IDIAG+K3
  140 CONTINUE

      DO 160 I160=1,K3
  160 DFDX(I160,2,I160)=IM

C  F & DFDX C�OPM�POBAH�
      FLAG=.TRUE.
      RETURN
C     DEBUG SUBTRACE,INIT(I160,IB1,IB2,IS1,IS2,IDS,IDR)
C  B�XO� B �A�PE�EHH�� O��ACT
  300 FLAG=.FALSE.
      STOP

C  O���KA KN �/��� KNC
  500 WRITE(6, 49) KNR,KNC
      PRINT    49, KNR,KNC
      STOP

C  MA�A P-HOCT� DFDX
  600 WRITE(6, 48) K3,KN,NTOT
      PRINT    48, K3,KN,NTOT
      STOP
C
C***********************************************************************
   49 FORMAT (/3X,'INPUT DATA ERROR:'        /3X,'KNR=',I5,5X,'KNC=',I5)

   48 FORMAT (//9X,' CIRCUIT CAN NOT BE ANALYSED:'         /3X,' NUMBER 
     +OF EQUATIONS EXCEEDS MAXIMUM.'         /3X,' NUMBER OF NODES=',I5 
     +        /3X,' NUMBER OF FREQNCYS=',I5         /3X,' MAX SIZE=',I5 
     +  )
C     DEBUG SUBTRACE,INIT(IB1,IB2,IS1,IS2,IRB,ISB,NU1,NU2)
C     DEBUG SUBTRACE,SUBCHK
      END
