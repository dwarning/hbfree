c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE SHLEIF(OM,P1,L1,P2,L2,P3,L3,N,NAME)
C     7.02.91
C
C  ������ �/�  SM61(FREQ,OUT)
C
C  ��E�� ��, XX
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(L1),P2(L2),P3(L3)

      DOUBLE PRECISION      OM,P1,P2,P3
      DOUBLE PRECISION      L

      COMMON /SUBC/  YY(15,15),YJ(15)
      COMMON /SUBS/  SV(15,15)

      DOUBLE COMPLEX    YY,VJ,SV
      DOUBLE COMPLEX    X1,X2,X3,X4,U,P,R,Y,Z,GAM
      DOUBLE COMPLEX    PAR(4),PARAM(10)

      LOGICAL*4  OUT

      CHARACTER  NAME, NKZ, NXX

      DOUBLE COMPLEX    CCEXP
      CCEXP(Z) = DEXP(DREAL(Z))*DCMPLX(DCOS(DIMAG(Z)),DSIN(DIMAG(Z)))
C
C
      DATA       NKZ/4HKZ  /,   NXX/4HXX  /

C
C     ��������� ������ : W1,W2,T,H,EPS,TGD,SIGMA,L
C               W1   -   ������ ����� (�)
C               W2   -   ������ ������ (�)
C               T    -   ������� ������� ��� (�)
C               H    -   ������� �������� ��� (�)
C               EPS  -   ���. ����������. �����������
C               TGD  -   ������� ���� ��������������� �����
C               SIGM -   �������� ������������ ������� (���/�)
C               L    -   ����� ������ (�)
C
C     ���������, ����� �� �����
      EPS = P1(1)
      TGD = P1(2)
      MU  = P1(3)
      SIGM = P1(4)/1.D+03
C
C     ���������, ����� ��� ������� ���� ������
      H   = P2(1)*1.D+03
      T   = P2(2)*1.D+03
C
C     ��������� �������������
      W1  = P3(1)*1.D+03
      W2  = P3(2)*1.D+03
      L   = P3(3)*1.D+03
C
      OM=OM*1.D-09
      U = (1.D0,0.D0)
      R = (-0.93584D0,0.D0)


      WRITE (6,5) W1,W2,T,H,EPS,TGD,SIGM,L,N
5     FORMAT(2X,' ��E��. �CXO�H�E �AHH�E:'/2X,'W OCH=',E12.5,'  W ��=',E
     +12.5,2X,'T=',E12.5,3X,'H=',E12.5/2X,'EPS=',E12.5,3X,'TANG.DELTA=',
     +E14.6,/,3X,'SIGM=',E14.6,3X,'L �� =',E12.5,1X,'N=',I5)
C     IF(IFR.GT.1) GOTO 1
C
C
C     M�KPO�O�OCKOBA� ��H��
C
      EP = EPS

      CALL MSL(W1,T,H,EP,WEF1,EPE1,Z1)
      CALL MSL(W2,T,H,EP,WEF2,EPE2,Z2)
      A=WEF1/H
      B=WEF2/H
      CALL SM22(W2,T,2.D0*H,EP,ZS2)
      WRITE (6,6) Z1,Z2
   6  FORMAT(2X,' Z OCH=',E12.5,'  Z ��=',E12.5)
      EPS1=DSQRT(EPE1)
      EPS2=DSQRT(EPE2)
      D1=SM63(Z1,EPS1,H)
      D2=SM63(Z2,EPS2,H)
      F=149.89623D0/DMAX1(EPS1*D1,EPS2*D2)
C   S - ������� ��� ������ ����������� ���
      IF(OM.NE.0.0D0) GOTO 7
      SV(1,1)=DCMPLX(0.0D0,0.0D0)
      SV(1,2)=DCMPLX(0.99997D0,0.0D0)
      SV(2,1)=DCMPLX(0.99997D0,0.0D0)
      SV(2,2)=DCMPLX(0.0D0,0.0D0)
      RETURN

   7  CONTINUE

      WN=.02095845D0*OM*1.D-09
      CALL SM21(EP,EPE1,H,Z1,WN,FEP1)
      CALL SM21(EP,EPE2,H,Z2,WN,FEP2)
      WRITE  (6,9) FEP1,WN
    9 FORMAT(2X,'SHLEIF : FEP1, WN',E12.5,1X,E12.5)
      WN1=WN*DSQRT(FEP1)
      WN2=WN*DSQRT(FEP2)
      CALL SM36(WN1,D1,D2,XA,XB,XC,XD,NOL)
C
C     �O�OHH�E �APAMETP� OTBETB�EH��
C
C     ��� M�KPO�O�OCKOBO� ��H��
C
      CALL SM84(WN,W2,T,H,EP,TEP,EPE2,Z2,ZS2,B,SIGM,AS,GAM)
      IF(NAME.EQ.NXX) CALL SM35(W2,H,FEP2,Z2,WN2,R)
      P=R*CCEXP(-2.D0*GAM)
      Z=(U+P)/(U-P)
      X2=DCMPLX(0.D0,-XB)
      X3=DCMPLX(0.D0,-XC)
      X4=DCMPLX(0.D0,XD)
      Z=Z*Z2/Z1
      Z=(Z+X4)*X3/(Z+X3+X4)+X2
C   ������������ Z-������
      Z=Z*50.D0

      WRITE(6,999) X2,X3,X4,Z
  999 FORMAT(2X,'X2=',2(1X,E12.5)/,'  X3=',2(1X,E12.5)/        '  X4=',2
     +(1X,E12.5)/ '   Z=',2(1X,E12.5))

C     �APAMETP� ��EMEHTOB T-CXEM�
C
      X1=DCMPLX(0.D0,XA)
      X1=X1*50.D0
      Y=U/Z
C
C     S-�APAMETP�
C
C
C   ===============================

      CALL SM03(X1,Y,X1,1.D0,1,PAR)
      DO 18 J=1,3
  18  PARAM(J)=PAR(J)
C
C    ������������ S - �������
C      L=1
C      DO 20 I=1,2
C      DO 20 J=1,2
C      CALL SM16(2,2,I,J,IJ,OUT)
C      K=2*IJ
C      SV(L,1)=PARAM(K-1)
C      SV(L,2)=PARAM(K)
C      WRITE(6,19) I,J,IJ,K,SV(L,1),SV(L,2)
C   19 FORMAT(2X,'SHLEIF : I,J,IJ,K =',4I5/2X,
C     *          'SV1=',E12.5,1X,E12.5,'  SV2=',E12.5,1X,E12.5)
C      L=L+1
C   20 CONTINUE
C
       SV(1,1) = PARAM(1)
       SV(1,2) = PARAM(2)
       SV(2,1) = PARAM(2)
       SV(2,2) = PARAM(3)
C
      WRITE(6,21) SV(1,1),SV(1,2),SV(2,1),SV(2,2)
   21 FORMAT(2X,'SV(1,1)=',E12.5,1X,E12.5/       2X,'SV(1,2)=',E12.5,1X,
     +E12.5/       2X,'SV(2,1)=',E12.5,1X,E12.5/       2X,'SV(2,2)=',E12
     +.5,1X,E12.5)

C
C    ������� �� S -������� � � - �������

      CALL TEST(N)
C



      RETURN
      END



C      SUBROUTINE MSL(W,T,H,ER,WE,EE,Z)
C   KOCT�KEB�� A.B.,�EK.1979�.
C  PROCEEDINGS_OF_THE_IEEE,1977,V.65,N11,MTT-25
C  BAHL I.J.,GARG R. ABTOP� CTAT��.
C     DATA A1,A2,A3,A4/376.9911,.3978873,12.56637,.1591549/
C      WE=W
C      WH=W/H
C      C=(ER-1.)/4.6*T/(H*SQRT(WH))
C         EA=(ER+1.)*.5
C      EB=(ER-1.)*.5
C      EC=1./SQRT(1.+12./WH)
C      IF(T.LT.1.E-5)GO TO 2
C      EF=.5*H/T
C      IF(WH.GT.A4)EF=A3*W/T
C      WE=WE+A2/H*T*(1.+ALOG(EF))
C    2 IF(WH.GT.1.) GO TO 1
CC  ��KA� �O�OCKA
C      EE=EA-C+EB*(EC+.04*(1.-WH)**2)
C      Z=60./SQRT(EE)*ALOG(8./WE*H+.25/H*WE)
C      RETURN
CC  ��POKA� �O�OCKA.
C    1 EE=EA-C+EB*EC
C      Z=A1/(SQRT(EE)*(WE/H+1.393+.667*ALOG(WE/H+1.444)))
C      RETURN
C      END

C      SUBROUTINE SM21(EP,EPE,H,Z,WN,FEP)
C      WNR=Z/H/119.916984
c      G=.6+.009*Z
C      FEP=EP-(EP-EPE)/(1.+G*(WN/WNR)**2)
C      RETURN
C      END
      SUBROUTINE SM25(ZS,ZM,H,WN,ZF)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      ZT=2.D0*ZS
      WNR=ZM/H/119.916984D0
      G=.6D0+.009D0*ZM
      ZF=ZT+(ZM-ZT)/(1.D0+G*(WN/WNR)**2)
      RETURN
      END




      SUBROUTINE SM26(W,T,B,EP,TEP,RS,Z0,WN,ALFC,ALFD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.14159265D0/
      ALFC=0.D0
      IF(T.EQ.0) GO TO 3
      G=W/(B-T)
      IF(G.LT.0.35D0)GO TO 2
      A=1.D0/(1.D0-T/B)
      S=A**2
      ALFC=RS*EP*Z0/(35456.86D0*B)*(A+2.D0*S*W/B+S*(1.D0+T/B)/PI*DLOG((A
     ++1.D0)/(A-1.D0)))
      GO TO 3
    2 X=T/W
      DO=((((.5343325D0*X-1.817245D0)*X+2.318656D0)*X-1.410188D0)*X+1.05
     +9607D0)*X+.4999405D0
      DO=DO*W
      DSUM=((((((((-17.18875D0*X+108.9861D0)*X-292.5251D0)*X+437.1874D0)
     +*X-402.3509D0)*X+238.2397D0)*X-92.2279D0)*X+23.3323D0)*X-3.820357D
     +0)*X+1.547924D0
      DSUM=DSUM*B
      ALFC=RS*(1.D0+DSUM/DO)/(2.D0*PI*Z0*B)
    3 ALFD=.5D0*WN*DSQRT(EP)*TEP
      RETURN
      END





      SUBROUTINE SM27(W,RW,T,H,EP,EPE,TEP,RS,Z,WN,ALFC,ALFD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=DSQRT(EP)
      ALFD=.5D0*WN*A*TEP
      IF(EP.EQ.1.D0)GO TO 10
      Q=(EPE-1.D0)/(EP-1.D0)
      ALFD=A*Q*ALFD/DSQRT(EPE)
C
   10 ALFC=0.D0
      IF(T.EQ.0.D0)RETURN
      P=W/H
      IF(P-.159154943D0)11,11,12
   11 A=DLOG(12.5663706D0*W/T-T/W)
      GO TO 13
   12 A=DLOG(2.D0*H/T-T/H)
   13 Q=1.D0+(1.D0+A/3.14159265D0)/RW
      ALFC=.159154943D0*Q*(1.D0-.0625D0*RW**2)
      IF(P.LE.2.D0)GO TO 14
      A=.5D0*RW+.94D0
      Q=RW*(1.D0+1.D0/(A*3.14159265D0))*Q
      ALFC=Q/(RW+.63661972D0*DLOG(17.0794684D0*A))**2
   14 ALFC=ALFC*RS/(H*Z)
      RETURN
      END

C      SUBROUTINE SM22(W,T,B,EP,Z)
C      A=SIN(1.57079633/B*T)
C      P=.5*ALOG((1.+A)/(1.-A))
C      A=EXP(1.57079633*W/(B-T)+P)
C      AK=2./(A+1./A)
C      AK1=SQRT(1.-AK**2)
C
C      CALL SM10(AK1,E,F)
C      CALL SM10(AK,E1,F1)
C      Z=94.2477795*F/(SQRT(EP)*F1)
C      RETURN
C      END


      SUBROUTINE SM36(WNG,B,B1,BA,BB,BC,BD,NOL)
C     LOGICAL NOL
C     NOL=.FALSE.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PI=3.14159265D0
      WLG=2.D0*PI/WNG
C     IF((B.GT.0.5*WLG).OR.(B1.GT.0.5*WLG)) NOL=.TRUE.
      AK=B1/(2.D0*B)
      C=DATAN(AK)
      X=AK*AK
      AN=1.D0*X
      H=DATAN(1.D0/AK)
      Z=C/AK
      W=-2.D0*Z
      A1=-(2.D0*AK/PI)*DEXP(W)*(1.D0+(5.D0+X)/(4.D0*(1.D0+X))*DEXP(W)+ (
     +4.D0/AN+((5.D0+X)/AN)**2)*DEXP(2.D0*W)/9.D0)
      A2=2.D0*(AK*H+Z+DLOG(AN/(4.D0*AK))-PI*AN/(6.D0*AK))-A1
      BA=2.D0*B1/WLG*(H+DLOG(DSQRT(AN))/AK)
      BB=.5D0*(BA-(2.D0*B/WLG*(PI*AK/3.D0+A1)))
      BC=WLG/(2.D0*PI*B1)
      BD=B/WLG*(PI/(3.D0*AK)+A2)
      RETURN
      END






      SUBROUTINE SM84(WN,W,T,H,EP,TEP,ES,Z,ZS,B,SIG,DL,GAM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX GAM
C
      CALL SM21(EP,ES,H,Z,WN,FP)
C
      CALL SM25(ZS,Z,H,WN,ZF)
      RS=SM28(WN,1.D0,SIG)
C
      CALL SM27(W,B,T,H,EP,FP,TEP,RS,ZF,WN,AC,AD)
      ALF=(AC+AD)
      BET=WN*DSQRT(FP)
      BET=BET+.5D0*(AC-AD)**2/BET
      GAM=DCMPLX(ALF*DL,BET*DL)
      RETURN
      END





      SUBROUTINE SM03(Z1,Z2,Z3,RAT,IT,PARS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX Z1,Z2,Z3,A,B,C,D,PARS(3)
      T=DFLOAT(IT)
      A=Z1+1.D0
      B=Z3+RAT
      C=A*Z2+1.D0
      D=1.D0/(A+B*C)
      PARS(2)=2.D0*D*DSQRT(RAT)
      B=Z3-RAT
      PARS(3)=T*D*(A+B*C)
      A=Z1-1.D0
      B=Z3+RAT
      C=A*Z2+1.D0
      PARS(1)=T*D*(A+B*C)
      RETURN
      END




C      SUBROUTINE SM10(AK1,E,F)
C      T=AK1**2
CC
CC     �����T��ECK�� �HTE�PA� �EPBO�O PO�A
CC
C      F=((.032024666*T+.054544409)*T+.097932891)*T+1.3862944-
C     *(((.010944912*T+.060118519)*T+.12475074)*T+.5)*ALOG(T)
CC
CC     ����T��ECK�� �HTE�PA� BTOPO�O PO�A
CC
C      E=((.040905094*T+.085099193)*T+.44479204)*T+1.-
C     *(((.01382999*T+.08150224)*T+.24969795)*T)*ALOG(T)
C      RETURN
C      END




      DOUBLE PRECISION FUNCTION SM28(WN,AMU,SIGM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SM28=DSQRT(WN*AMU*188.49559D0/SIGM)
      RETURN
      END





C      FUNCTION SM63(Z,SEP,H)
CC       ������� ��������� 7.02.91 �.�.
CC     LOGICAL*4 SYML
C      P=.25
CC     IF(SYML)P=1.
C      SM63=376.687*P*H/(Z*SEP)
C      RETURN
C      END


      SUBROUTINE SM16(KC,MS,I,J,INC,OUT)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL*4 OUT
      OUT=.FALSE.
      K=0
      IR1=(I-1)/KC
      JR1=(J-1)/KC
      IF(IR1-JR1)20,10,20
   10 IF(J.GT.KC) K=K+JR1
      IF(K)11,11,12
   11 IR=I
      JR=J
      GO TO (13,14,16),MS
   12 IR=MOD((I-1),KC)+1
      JR=MOD((J-1),KC)+1
      GO TO (13,14,16),MS
   13 INC=K*KC**2+KC*(IR-1)+JR
      RETURN
   14 IF(I.LE.J) GO TO 15
      JR=MOD((I-1),KC)+1
      IR=MOD((J-1),KC)+1
   15 INC=K*(KC*(KC+1))/2+(IR-1)*KC+JR-IR*(IR-1)/2
      RETURN
   16 IF(I-J)20,17,20
   17 INC=K*KC+IR
      RETURN
   20 OUT=.TRUE.
      INC=0
      RETURN
      END



      SUBROUTINE   SM35(W,H,EPE,Z,WN,R)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DOUBLE COMPLEX R,C,COEXP
       COEXP(C)=DEXP(DREAL(C))*DCMPLX(DCOS(DIMAG(C)),DSIN(DIMAG(C)))
      CL=SM77(W,2.D0*H,WN)
      R=DCMPLX(0.D0,-2.D0*WN*CL)
      GR=SM78(W,EPE,WN)*Z
      GR=(1.D0-GR)/(1.D0+GR)
      R=DCMPLX(GR,0.D0)*COEXP(R)
      RETURN
      END




      DOUBLE PRECISION FUNCTION SM77(W,B,WN1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      C=.220635D0*B
      X=WN1*C
      X=2.D0*(2.D0*C+W)/(C+2.D0*W)*DCOS(X)/DSIN(X)
      SM77=(1.570796D0-DATAN(X))/WN1
      RETURN
      END




      DOUBLE PRECISION FUNCTION SM78(W,EPE,BET)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL SM79
      COMMON /GINT/P(2)
      P(1)=BET
      P(2)=W
      SM78=DSQRT(EPE)*(SM08(0.D0,1.5707D0,SM79)+SM08(1.5708D0,3.14159265
     +D0,SM79))/2368.79422D0
      RETURN
      END




      DOUBLE PRECISION FUNCTION SM08(A,B,FUN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /GINT/P(2)
      DIMENSION X(6),W(6)
      DATA X /0.1252334085D0,0.3678314989D0,0.5873179542D0,0.7699026741D
     +0,0.9041172563D0,0.9815606342D0/,W /0.2491470458D0,0.2334925365D0,
     +0.2031674267D0,0.1600783285D0,0.1069393259D0,0.0471753363D0/
C
      C=0
      D=(A+B)/2.D0
      DO 10 I=1,6
      E=(B-A)*X(I)/2.D0
   10 C=C+W(I)*(FUN(D+E)+FUN(D-E))
      SM08=C*(B-A)/2.D0
      RETURN
      END





      DOUBLE PRECISION FUNCTION SM79(T)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/GINT/P(2)
      SM79=(DSIN(5.D0*P(1)*P(2)*DCOS(T)))**2*(DSIN(T))**3/(DCOS(T))**2
      RETURN
      END
