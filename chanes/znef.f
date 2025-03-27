c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE NEF(NTOT,U,F,SF,FNOR,FLAG,FLGFFT)
C******************************************************************
C                                                                 *
C     �/� B���C��ET HA I-� �TEPA��� BEKTOP HEB��OK (F)            *
C                                                                 *
C                                                                 *
C******************************************************************
C                                                                 *
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'      
      include 'funcsize.i'
      EQUIVALENCE (K3,KOL(3))
C      COMMON/POINTR/NMPNT,NNODE,NPARAM,LENNOD,LENPAR,NNETPR,LENNTP
      COMMON/KOLNAL/KOL(4),NAL(4)
C      /POINT/MPOINT(1)/NODEL/NODEEL(1)
C      COMMON/PARAMS/PARAM(1)
C  ��������� �� 30.01.91    ������� ������ �.�.     ��. MAIN
      COMMON/MATY/  BUFFER (6000),BUFLEN
      DOUBLE COMPLEX BUFFER
      INTEGER*4 BUFLEN

      LOGICAL NAL
      COMMON/BLK1/KR,KR1,KC,KC1,NNR,NNR1,MN,MN1
      COMMON/BLK2/KNC,KNR,KN,KNR1,KN1
      COMMON/BLW1/W,W1/BLW2/WR,WS
      DOUBLE PRECISION W(20),W1(200)
      INTEGER KR(20),KC(10),NNR(10),MN(2,20)
      INTEGER KR1(200),KC1(20),NNR1(20),MN1(2,200)
      INTEGER WR(20,20),WS(20,20)
      INTEGER NAME(4),NOI(5,2),NOU(5,2),NU1(5,2),NU2(5,2)
      INTEGER KOUV(5),KOPV(5),KOI,NR1V(5),NB1V(5),NG,INDEP
      DOUBLE PRECISION SF(1),FNOR
      DOUBLE COMPLEX F(1),U(1)
      DOUBLE COMPLEX UNEL(MAXU_MD*MAXKN),DUNDT(MAXD_MD*MAXKN)
      DOUBLE COMPLEX B1(B1_SIZE) ,B2(B2_SIZE)
      DOUBLE COMPLEX ZNN((MAXU_MD+MAXD_MD)*MAXKN)
C..............PA�MEPHOCT� -�TO�H�T�!!!.....

C     COMPLEX VJ,YR   **** ������. 30.01.91  ������ �.�.  ��. MAIN

      DOUBLE COMPLEX IM/(0.0D0,1.0D0)/,ZERO/(0.0D0,0.0D0)/
      DOUBLE COMPLEX U1,U2
      LOGICAL FLAG,EXS(5,2),EXIST(5,2)

C  ��������� �� 30.01.91    ������� ������ �.�.     ��. MAIN
C   ���������� �-��� ��� ���������� ������� � BUFFER
      IFIND1(I,M,NU)=I+(M-1)*NU
      IFIND2(I,J,M,NU,MF)=NU*MF+I+(J-1)*NU+(M-1)*NU*NU

C..............BH�MAH�E! MAX P-HOCT� F(600=20*30).....
C  O�H��EH�E BEKTOPA HEB��K�

      KUR=KN*K3
      DO 3 J=1,KUR
    3 F(J)=ZERO
      N1=NNETPR
      L1=LENNTP
C  KO�-BO �PABHEH�� = KO�-BO �ACTOT + KO�-BO ���OB
C  EC�� �PABHEH�� �O���E,�EM PA�MEPHOCT� BEKTOPA HEB��K� F,TO �EPEXO�
C   HA METK� ..600 .
      IF(KUR.GT.NTOT) GO TO 600

C  EC�� KO�-BO HEH��EB�X CTPOK �O���E 10, �EPEXO�
C  HA METK� ..500..
      IF(KNR.GT.10) GO TO 500
C  O�PE�E�EH�E KPATHOCT� 2-K� B KNC. KNC-PA�MEPHOCT� �PEO�PA-
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
C     WRITE(6,1002) I1,L3
C1002 FORMAT(2X,'ZNEF: MPOINT(',I3,'+12)=',I4,' (=L3)')
C  N3=MPOINT(I1+13)+(I2-1)*L3 -TAK �E�AT� HE���� !!! �E�AT� H��HO TAK:
      KNODES=MPOINT(I1+7)
      NA=NF+(I2-1)*LE
      N3=NODEEL(NA+KNODES+3)
C     WRITE(6,1003) NA,KNODES,N3
C1003 FORMAT(2X,'      NODEEL(',I3,'+',I3,'+3)=N3=',I4)
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
C     WRITE(6,1004) NADRU,NOU(NADRU,1),NA,NODEEL(NA+NOU(NADRU,1))
C1004 FORMAT(2X,'ZNEF: NOU(',I3,',1)=',I5,' NA=',I3,' NU1(I,1)=',I3)

      NU1(I,1)=NODEEL(NA+NOU(NADRU,1))
C     WRITE(6,1005) NADRU,NOU(NADRU,2),NA,NODEEL(NA+NOU(NADRU,2))
C1005 FORMAT(2X,'ZNEF: NOU(',I3,',2)=',I5,' NA=',I3,' NU1(I,2)=',I3)
      NU1(I,2)=NODEEL(NA+NOU(NADRU,2))
      EXS(I,1)=EXIST(NADRU,1)
      EXS(I,2)=EXIST(NADRU,2)
   22 NADRU=NADRU+1
      NU2(1,1)=NODEEL(NA+NOI(INDEP,1))
      NU2(1,2)=NODEEL(NA+NOI(INDEP,2))

C  �A�O�HEH�E ZNN,UNEL,DUNDT
C        BCE�O: UNEL(KN*KOU)
C               DUNDT(KN*KOP)
C               ZNN(KN*(KOU+KOP))
C     MPOINT(I1+17)=KOU+KOP
C
      KUNEL=KN*KOU
      KDUN=KN*KOP
      KZNN=(KOU+KOP)*KN
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
C     WRITE(6, 41) IU,NU1(IU,1),JWBEG
CC!  *             NU1(IU,1),U(JWBEG+NU1(IU,1))

C  41 FORMAT(2X,'ZNEF: NU1(',I3,',1)=',I5,'  JWBEG=',I5)
CC!  * 'U(',I3,'+',I3,')=',E12.5,2X,E12.5)

      IF(NU1(IU,1).NE.0) U1=U(JWBEG+NU1(IU,1))
      IF(NU1(IU,2).NE.0) U2=U(JWBEG+NU1(IU,2))
      UNEL(IUBEG+JW)=U1-U2
   43 ZNN(IUBEG+JW)=UNEL(IUBEG+JW)
      IF(.NOT.EXS(IU,2)) GO TO 40
      IDU=IDU+1
      DO 47 JW=1,KN
      IDUBEG=(IDU-1)*KN
      DUNDT(IDUBEG+JW)=IM*W(JW)*UNEL(IUBEG+JW)
      ITERM=KUNEL+IDUBEG
   47 ZNN(ITERM+JW)=DUNDT(IDUBEG+JW)
   40 CONTINUE
C     ��P�E-�PEO�PA�OBAH�E - B���C�EH�E IHE
      IF=0
C     WRITE(6,1000) N1,L1,N2,L2,N3,L3
      CALL FTMAS2(ZNN,KR,KC,NNR,KNR,KNC,KN,NR,NB,M1,B1,B2,IF,FLGFFT,*50)

C1000 FORMAT(2X,'ZNEF: N1=',I3,' L1=',I3,' N2=',I3,' L2=',I3,
C    *         ' N3=',I5,' L3=',I3)

   50 CALL LIBMD3(NAME,NG,N1,L1,N2,L2,N3,L3,B1,KNC2,NR,*300)

C     PRINT 1232,(B1(II),II=1,KNC)
      CALL FT2(ZNN,KR,KC,NNR,KNR,KNC,KN,NR,NB,B1,B2,IF,FLGFFT,*50)
C !       �P� �OCTAHOBKE VFFT-BCTAB�T� M1         $
C     WRITE(6, 1232) (II,ZNN(II),II=1,KZNN)
C     �OPM�POBAH�E BEKTOPA HEB��OK - �AHECEH�E ZNN B F
      I=1
      NUI21=NU2(I,1)
      NUI22=NU2(I,2)
      KNI1=KN*(I-1)
      DO 55 J=1,KN
      N0=(J-1)*K3
      IND1=N0+NUI21
      IND2=N0+NUI22
      IND3=J+KNI1
      IF(NUI21.NE.0) F(IND1)=F(IND1)+ZNN(IND3)
   55 IF(NUI22.NE.0) F(IND2)=F(IND2)-ZNN(IND3)
C     PRINT 6543,IM
  110 CONTINUE
  100 CONTINUE

C  �AHECEH�E TOKOB ��H.�ACT� � TOKOB HE�AB�C�M�X �CTO�H�KOB
C  B BEKTOP HEB��OK.
      DO 311 IW=1,KN
      KW=(IW-1)*K3
      DO 311 KST=1,K3
      F(KW+KST)=F(KW+KST)-BUFFER( IFIND1(KST,IW,K3) )
      DO 311 KE=1,K3
C     WRITE(6,3111) KW,KST,F(KW+KST),KE,KST,IW,
C    *		BUFFER( IFIND2(KE,KST,IW,K3,KN) ),KE,KW,
C    *              U(KE+KW)
C3111 FORMAT(2X,'F(',I3,'+',I3,')=',E12.5,2X,E12.5/
C    *       2X,'YR(',3(I5),')=',E12.5,2X,E12.5/
C    *       2X,'U(',I3,'+',I3,')=',E12.5,2X,E12.5)
      F(KW+KST)=F(KW+KST)+BUFFER( IFIND2(KE,KST,IW,K3,KN) )*U(KE+KW)
  311 CONTINUE

C  B���C��M FNOR=1/2*FVEC_TRANSP*FVEC,( FVEC=F )
      FNOR=0.0D0
      DO 15 I=1,KUR
C     WRITE(6,155) FNOR,I,SF(I),F(I)
C 155 FORMAT(2X,'ZNEF : FNOR,I,SF(I),F(I) = ',E12.5,I5,2X,E12.5,
C    *       5X,E12.5,2X,E12.5)
   15 FNOR=FNOR+SF(I)*SF(I)*F(I)*DCONJG(F(I))
      FNOR=FNOR/2.D0

C  F        C�OPM�POBAH
      FLAG=.TRUE.
      RETURN
C     DEBUG SUBTRACE
C  B�XO� B �A�PE�EHH�� O��ACT
  300 FLAG=.FALSE.
      STOP

C  O���KA KN �/��� KNC
  500 PRINT 49,KNR,KNC
      WRITE(6, 49) KNR,KNC
      STOP

C  MA�A P-HOCT� F
  600 PRINT 48,K3,KN,NTOT
      WRITE(6, 48) K3,KN,NTOT
      STOP
C
C***********************************************************************
   49 FORMAT (/3X,'INPUT DATA ERROR:'        /3X,'KNR=',I5,5X,'KNC=',I5)

   48 FORMAT (//9X,' CIRCUIT CAN NOT BE ANALYSED:'         /3X,' NUMBER 
     +OF EQUATIONS EXCEEDS MAXIMUM.'         /3X,' NUMBER OF NODES=',I5 
     +        /3X,' NUMBER OF FREQNCYS=',I5         /3X,' MAX SIZE=',I5 
     +  )
C1232 FORMAT(2X,'ZNN(',I3,')=',E12.6,2X,E12.6)
      END
