c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE INITI(U,YY,VECTJ,ISIZE_MAXNODE,IERR)

C*********************************************************************
C* �/� 'HA�A�O'-B���BA��A� ��� TO�OO�PA�OTK�,�OCTPOEH�� CETK� �ACTOT *
C* � �OPM�POBAH��  Y � J HA �ACTOTHO� CETKE. B C���AE ��HE�HO� CXEM� *
C*                     -B���C��ET BEKTOP PE�EH��                     *
C*********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX          S
C   ������� ��������� ����������� MAIN  30.01.91 (������ �.�.)
C $LARGE: J,Y
C      COMMON/MATY/    J(15,20),Y(15,15,20)
C     COMPLEX          Y,J

      DOUBLE PRECISION U(1)
      
      DOUBLE COMPLEX YY(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX VECTJ(ISIZE_MAXNODE)
      
      COMMON/MATY/     BUFFER (6000), BUFLEN
      DOUBLE COMPLEX          BUFFER
      INTEGER*4        BUFLEN
      COMMON/BLW1/     W,W1
      COMMON/BLW2/     WR,WS
      COMMON/FRE/      F(2)
      DOUBLE PRECISION             W(20),W1(200),F
      INTEGER          WR(20,20),WS(20,20)
      COMMON/BLK1/     KR,KR1,KC,KC1,NNR,NNR1,MN,MN1
      COMMON/BLK2/     KNC,KNR,KN,KNR1,KN1
      INTEGER          KR(20),KC(10),NNR(10),KR1(200),KC1(20),NNR1(20)
      INTEGER          MN(2,20),MN1(2,200)
      COMMON/KOLNAL/   KOL,NAL
      COMMON/MEP/      MEPHF,FLGMNW
      INTEGER          KOL(4),FLGMNW,IP/6/
      LOGICAL          NAL(4)
      COMMON /BLMNI/   MNI(2,20),KMNI
      DOUBLE PRECISION             OM,PI
      COMMON/PRINT/    KPRLEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,              
     +   KPRVAR,KPRGRF,KPRQUP
      INTEGER IERR



C     B��OB �/� COPT�POBK� � �EPEKO��POBK� HOMEPOB ���OB
      CALL SORTUZ

C     �POBEPKA HA����� HE��HE�H�X ��EMEHTOB
C     IF(KOL(3)-KOL(4).EQ.0) WRITE(IP, 1020)

C     �/� O�PE�E�EH�� MAX KO���ECTBA BXO�H�X/B�XO�H�X BE����H ���
C           �PEO�P-� ��P�E
      CALL DETSYN(MAXSYN)

C     �OCTPOEH�E CETOK �ACTOT � �P.(CM.KOMMEHT.B �/�)
      DO 80 II=1,KN
      MN(1,II)=MNI(1,II)
80    MN(2,II)=MNI(2,II)

      CALL INKOOR(MAXSYN,MNMAX,F(1),F(2),*100, *101)


C     �OPM�POBAH�E Y � J HA KA��O� �ACTOTE CETK� MN ( MACC�B W )

      DO 30 IO=1,KN
      IOO=IO
   30 CALL STEPFR (W(IOO),IOO,YY,VECTJ,ISIZE_MAXNODE)
C     �PEO�PA�OBAH�E HYMEPA��� Y��OB
      K3=KOL(3)
      K23=KOL(2)+K3
      K12=KOL(1)+KOL(2)
      CALL TOPO(-1,K12)
C     �AHECEH�E TO�O-X-K MHO�O�O��CH�KA B MACC�B� MPOINT & NODEEL
      CALL TOPOIN(K23,1)
      CALL TOPOIN(K3,2)

C     EC�� HET HE��HE�H�X ��EMEHTOB � �PAH��H�X ���OB
C                                   - B��EC�EH�E BEKTOPA PE�EH��
      IF(NAL(3).OR.NAL(4)) then
        IERR=0
        RETURN
      endif 
       
C      WRITE(IP, 1020)
      DO 50 IRRO=1,KN
      IRR=IRRO
   50 CALL STBACK (U,IRR)

      IERR=1  
      RETURN

C***** �O���A� CETKA MN1 ( �O���E 200 ) *****************************
  100 WRITE(IP, 1000)

      IERR =2 
      RETURN 

C***** KNC > KNCMAX - B�XO� B �A�E�EHH�� O��ACT� *******************
  101 WRITE(IP, 1010) MNMAX

      IERR=3
      RETURN

C**********************************************************************
C      ENTRY AGAIN
C this entry should not used any more
C
C     BXO� ��� �OBTOPHO�O B��OBA �OPM�POBAH�� ��MEH�EM�X
C                       ��EMEHTOB
C
C
C     BOCCTAHOB�EH�E HYMEPA��� Y��OB
C      K12=KOL(1)+KOL(2)
C      CALL TOPO(1,K12)
C
C
C     �OBTOPHOE �OPM�POBAH�E BAP��PYEMO� �ACT�
C      DO 60 IO=1,KN
C      IOO=IO
C   60 CALL DOUBLE(W(IOO),IOO)
C
C     �PEO�PA�OBAH�E HYM-��� Y��OB /O��T�/
C      CALL TOPO(-1,K12)
C
C     �AHECEH�E TO�O-X-K
C      K3=KOL(3)
C      CALL TOPOIN(K3,2)
C
C     EC�� HET HE��HE�H�X ��EMEHTOB � �PAH��H�X ���OB
C                                   - B���C�EH�E BEKTOPA PE�EH��
C      IF(NAL(3).OR.NAL(4)) RETURN
C      WRITE(IP, 1020)
C      DO 70 IRRO=1,KN
C      IRR= IRRO
C      CALL STBACK(U,IRR)
C   70 CONTINUE
C
C
C      RETURN 1

 1000 FORMAT(/10X,'SIZE OF AUX. FREQUENCY GRID IS MORE THAN MAX=200.')
 1010 FORMAT(/10X,'CANT COMPUTE HARMONICS HIGHER THAN ',I4)
C
      END
