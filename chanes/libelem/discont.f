c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE DISCONT(OM,P1,L1,P2,L2,P3,L3,N)
C
C  ������  �/�  SM38 (FREQ,NOL)
C
C  ������
C
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P1(L1),P2(L2),P3(L3)

      DOUBLE PRECISION      OM,P1,P2,P3

      COMMON /SUBC/  YY(15,15),YJ(15)
      COMMON /SUBS/  SV(15,15)

      DOUBLE COMPLEX    YY,VJ,SV
      DOUBLE COMPLEX    PAR(4),PARAM(10),ZP

      LOGICAL*4  SYMS, SYML, NOL


      SYML=.TRUE.

C     ��������� ������ : W1,W2,T,H,EPS
C               W1   -   ������ ������� �� ������ (�)
C               W2   -   ������ ������� ����� ������ (�)
C               T    -   ������� ������� ��� (�)
C               H    -   ������� �������� ��� (�)
C               EPS  -   ���. ����������. �����������
C
C     ���������, ����� �� �����
      EPS = P1(1)
C
C     ���������, ����� ��� ������� ���� ������
      H   = P2(1)*1.D+03
      T   = P2(2)*1.D+03
C
C     ��������� �������������
C    EC�� FL=1, �� ������ ��������������,
C    ���� FL=-1, ������ ������������
      FL  = P3(1)
      W1  = P3(2)*1.D+03
      W2  = P3(3)*1.D+03
      SYMS=.FALSE.
      IF(FL.GT.0) SYMS=.TRUE.
      OM=OM*1.D-09


      WRITE (6,5) W1,W2,T,H,EPS
5     FORMAT(2X,' ������. �CXO�H�E �AHH�E:'/2X,'W1=',E12.5,'  W2=',E12.5
     +,2X,'T=',E12.5,3X,'H=',E12.5/2X,'EPS=',E12.5)
C     IF(IFR.GT.1) GOTO 1
C
C
C     M�KPO�O�OCKOBA� ��H��
C
      EP = EPS

      CALL MSL(W1,T,H,EP,WEF1,EPE1,Z1)
      CALL MSL(W2,T,H,EP,WEF2,EPE2,Z2)
      WRITE (6,6) Z1,Z2,EPE1,EPE2,WEF1,WEF2
   6  FORMAT(2X,' ������: Z1=',E12.5,'  Z2=',E12.5/2X,' EPE1=',E12.5,'  
     +EPE2=',E12.5,'  WEF1=',E12.5,'  WEF2=',E12.5)
C   S - ������� ��� ������ ����������� ���
      IF(OM.NE.0.0D0) GOTO 7
      SV(1,1)=DCMPLX(0.0D0,0.0D0)
      SV(1,2)=DCMPLX(0.99997D0,0.0D0)
      SV(2,1)=DCMPLX(0.99997D0,0.0D0)
      SV(2,2)=DCMPLX(0.0D0,0.0D0)
      RETURN
   7  CONTINUE
      EPS1=DSQRT(EPE1)
      EPS2=DSQRT(EPE2)
   10 R=Z2/Z1
      D1=SM63(Z1,EPS1,H)
       D2=SM63(Z2,EPS2,H)
      F=149.89623D0/DMAX1(EPS1*D1,EPS2*D2)

C    �������� - ��������� ���������� ���������� ���������.
C    � ������������ ������ ������������� ������.

          IF(.NOT.SYMS) F=2.D0*F

      WN=.02095845D0*OM
C       IF(SYML) GO TO 70
   70 CALL SM21(EP,EPE1,H,Z1,WN,FEP1)
      WN1=WN*DSQRT(FEP1)
   80 CALL SM32(WN1,D1,D2,SYMS,X,NOL)
      IF(.NOT.NOL) GO TO 12
      WRITE(6,11) F
   11 FORMAT(10X,'������:','F  MAX =',E12.5)
      RETURN
   12 ZP=DCMPLX(0.D0,X)
   13 CALL SM01(ZP,R,1,PAR)
C
      PARAM(1)=PAR(1)
      PARAM(2)=PAR(2)
      PARAM(3)=PAR(3)

C   ������������ S-�������
      SV(1,1)=PARAM(1)
      SV(1,2)=PARAM(2)
      SV(2,1)=PARAM(2)
      SV(2,2)=PARAM(3)

      WRITE(6,21) SV(1,1),SV(1,2),SV(2,1),SV(2,2)
   21 FORMAT(2X,'SV(1,1)=',E12.5,1X,E12.5/       2X,'SV(1,2)=',E12.5,1X,
     +E12.5/       2X,'SV(2,1)=',E12.5,1X,E12.5/       2X,'SV(2,2)=',E12
     +.5,1X,E12.5)
C
C   ������������ Y-�������
      CALL TEST(N)
C


      RETURN
      END



      SUBROUTINE SM32(WNG,B1,B2,ASYM,B,NOL)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL NOL,ASYM
      NOL=.FALSE.
      WLG=6.2831853D0/WNG
      IF(ASYM)WLG=WLG*.5D0
      IF((B1.LE.WLG).AND.(B2.LE.WLG)) GO TO 10
      NOL=.TRUE.
      RETURN
   10 R=B2/B1
      IF(R-1.D0)11,12,13
   11 BW=B1
      BW1=B2
      GOTO14
   12 B=0.D0
      RETURN
   13 BW=B2
      BW1=B1
   14 ALFA=BW1/BW
      BW=BW/WLG
      BW1=BW1/WLG
      D=(1.D0+ALFA)/(1.D0-ALFA)
      D1=D**(1.D0/ALFA)
      C=4.D0*ALFA
      D=D**ALFA
      ALFA=ALFA*ALFA
      ALFA1=1.D0/(1.D0-ALFA)
      A=DSQRT(1.D0-BW*BW)
      A=(A+1.D0)/(1.D0-A)
      A=A*D*D-(1.D0+3.D0*ALFA)*ALFA1
      A1=DSQRT(1.D0-BW1*BW1)
      A1=(1.D0+A1)/(1.D0-A1)
      A1=A1*D1*D1+(3.D0+ALFA)*ALFA1
      C=C*ALFA1
      D1=D*D1
      B=DLOG((1.D0/C)*DSQRT(D1))
      C=C*C
      B=B+2.D0*(A+A1+2.D0*C)/(A*A1-C*C)
          A1=((5.D0*ALFA-1.D0)*ALFA1+1.3333333D0*ALFA*C/A)**2
      ALFA=BW1/BW
      D=((1.D0-ALFA)/(1.D0+ALFA))**(4.D0*ALFA)
      B=B+.0625D0*BW**2*D*A1
      B=B*BW*2.D0
      IF(R.GT.1.D0) B=B*ALFA
      RETURN
      END



      SUBROUTINE SM01(Z,RAT,IT,PARS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX Z,A,D,PARS(3)
      T=DFLOAT(IT)
      A=Z+RAT
      D=1.D0/(1.D0+A)
      PARS(1)=T*D*(A-1.D0)
      PARS(2)=2.D0*D*DSQRT(RAT)
      PARS(3)=T*D*(Z-RAT+1.D0)
      RETURN
      END



      SUBROUTINE MSL(W,T,H,ER,WE,EE,Z)
C  KOCT�KEB�� A.B.,�EK.1979�.
C  PROCEEDINGS_OF_THE_IEEE,1977,V.65,N11,MTT-25
C  BAHL I.J.,GARG R. ABTOP� CTAT��.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA A1,A2,A3,A4/376.9911D0,.3978873D0,12.56637D0,.1591549D0/
      WE=W
      WH=W/H
      C=(ER-1.D0)/4.6D0*T/(H*DSQRT(WH))
         EA=(ER+1.D0)*.5D0
      EB=(ER-1.D0)*.5D0
      EC=1.D0/DSQRT(1.D0+12.D0/WH)
      IF(T.LT.1.D-5)GO TO 2
      EF=.5D0*H/T
      IF(WH.GT.A4)EF=A3*W/T
      WE=WE+A2/H*T*(1.D0+DLOG(EF))
    2 IF(WH.GT.1.D0) GO TO 1
C  ��KA� �O�OCKA
      EE=EA-C+EB*(EC+.04D0*(1.D0-WH)**2)
      Z=60.D0/DSQRT(EE)*DLOG(8.D0/WE*H+.25D0/H*WE)
      RETURN
C  ��POKA� �O�OCKA.
    1 EE=EA-C+EB*EC
      Z=A1/(DSQRT(EE)*(WE/H+1.393D0+.667D0*DLOG(WE/H+1.444D0)))
      RETURN
      END




      SUBROUTINE SM21(EP,EPE,H,Z,WN,FEP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WNR=Z/H/119.916984D0
      G=.6D0+.009D0*Z
      FEP=EP-(EP-EPE)/(1.D0+G*(WN/WNR)**2)
      RETURN
      END



      SUBROUTINE SM22(W,T,B,EP,Z)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=DSIN(1.57079633D0/B*T)
      P=.5D0*DLOG((1.D0+A)/(1.D0-A))
      A=DEXP(1.57079633D0*W/(B-T)+P)
      AK=2.D0/(A+1.D0/A)
      AK1=DSQRT(1.D0-AK**2)
C
      CALL SM10(AK1,E,F)
      CALL SM10(AK,E1,F1)
      Z=94.2477795D0*F/(DSQRT(EP)*F1)
      RETURN
      END




      SUBROUTINE SM10(AK1,E,F)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      T=AK1**2
C
C     �����T��ECK�� �HTE�PA� �EPBO�O PO�A
C
      F=((.032024666D0*T+.054544409D0)*T+.097932891D0)*T+1.3862944D0-(((
     +.010944912D0*T+.060118519D0)*T+.12475074D0)*T+.5D0)*DLOG(T)
C
C     ����T��ECK�� �HTE�PA� BTOPO�O PO�A
C
      E=((.040905094D0*T+.085099193D0)*T+.44479204D0)*T+1.D0-(((.0138299
     +9D0*T+.08150224D0)*T+.24969795D0)*T)*DLOG(T)
      RETURN
      END




      DOUBLE PRECISION FUNCTION SM63(Z,SEP,H)
C       ������� ��������� 7.02.91 �.�.
C     LOGICAL*4 SYML
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      P=.25D0
C     IF(SYML)P=1.
      SM63=376.687D0*P*H/(Z*SEP)
      RETURN
      END
