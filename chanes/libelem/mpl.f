c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE MP(OM,P1,L1,P2,L2,P3,L3,N)
C
C     MO�E�� ��������������� ����� 
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION     P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION          OM,P1,P2,P3

      COMMON/SUBC/  YY(15,15),VJ(15)
      COMMON/SUBS/  SV(15,15)
      DOUBLE COMPLEX       G1,G2,CH1,SH1,Y11,Y12,Y13
      DOUBLE COMPLEX       YY,SV,VJ,CTH1,CSCH,YR
      DOUBLE PRECISION          M0
C
C
      EPS  = P1(1)
      TGD  = P1(2)
      H    = P2(1)
      T    = P2(2)
      W    = P3(1)
      FL   = P3(2)
      EER  = P3(3)
      ZO   = P3(4)
      ALFA = P3(5)
      SKL1 = P3(6)
      SKL2 = P3(7)
C
      ER = EPS
      E0 = 8.85418D-12
      PI = 3.1415926D0
      CC = 2.99D+8
      M0 = 1.256637D-06
      RM=5.8D-07
C
      IF(OM.EQ.0.0D0)GOTO 478
      F=OM/(2.D0*PI)
C
C
      IF(EER.GT.0.0D0)GOTO 7345
      CALL SERF(W,H,T,EER,WEH1,ER)
 7345 CONTINUE
      IF(ZO.GT.0.0D0)GOTO 7347
      CALL SZ0(W,H,T,EER,WEH1,ZO)
 7347 CONTINUE
C
      IF(SKL1.EQ.1.D0)GOTO 917
      GOTO 918
 917  CONTINUE
C
C               ������ ��������� ���������
C  ��������� ��������� ������������� � ��������������� ����������
C
       F=OM/6.2831852D0
       G=DSQRT((ZO-5.D0)/60.D0)+0.004D0*ZO
       FR=0.3976D0*ZO/(H*1.D+03)
       G1=1.D0+G*(F*1.D-09/FR)**2.D0
       CALL Z00P(W,H,T,ER,ZOP)
       ZO=ZOP-(ZOP-ZO)/G1
       EER=ER-(ER-EER)/G1    
C       
 918  CONTINUE
      IF(SKL2.EQ.1.D0) GOTO 120
      GOTO 121
 120  WRITE(6,100)EER,ZO,F
 100  FORMAT(8X,'EER=',F5.2,4X,'Zo=',F6.2,' OM',4X,'F=',E11.4,' ��')
 121  CONTINUE
C 
C
      BETT=OM*DSQRT(EER)/CC
      OLAM=6.2831852D0/BETT
C 
      DGR=360.D0*FL/OLAM
      IF(SKL2.EQ.1.D0)GOTO 130
      GOTO 131
  130 WRITE(6,102)DGR,F
  102 FORMAT(8X,'������� ����� �����=',F6.2,' ����',4X,'F=',E11.4,' ��')
 131  CONTINUE
C

      IF(ALFA.GT.0.D0)GOTO 34
C 
C      PAC��� ������
C  ������ � ��������������� �����  (��/�)
C
    
      IF(T.EQ.0.D0) T=0.1D-08
      RO=5.8D-07
      F=OM/6.2831852D0
      RS=DSQRT(3.1415926D0*F*M0*RO)
      OLAM=3.D+08/F
      IF((W/H).GE.0.15915494D0)THEN
      B=H
      WEH=W/H+0.3978873D0*T/H*(1.D0+DLOG(2.D0*H/T))
      ELSE
      B=6.2831852D0*W
      WEH=W/H+0.3978873D0*T/H*(1.D0+DLOG(12.56637D0*W/T))
      ENDIF
      A=1.D0+(1.D0+0.31830989D0*DLOG(2.D0*B/T))/WEH
      IF((W/H).LE.1.D0)THEN
      ALFP=1.38D0*A*RS*(32.D0-WEH*WEH)/(H*ZO*(32.D0+WEH*WEH))
      ELSE
      ALFP=6.1D-05*A*RS*ZO*EER/H*(WEH+0.667D0*WEH/(WEH+1.444D0))
      ENDIF
      ALFD=27.3D0*ER*(EER-1.D0)*TGD/(ER-1.D0)/DSQRT(EER)/OLAM
      ALFA=(ALFP+ALFD)/8.686D0
  34  CONTINUE
C
      IF(SKL2.EQ.1.D0)GOTO 125
      GOTO 126
 125  WRITE(6,101)ALFA,F
 101  FORMAT(8X,'ALFA=',E11.4,' ��/�',4X,'F=',E11.4,' ��')
 126  CONTINUE
C
C
      YO=1.D0/ZO
      D2  = BETT
      D1  = ALFA
      G1  = DCMPLX(D1,D2)
      D11 = -D1
      D22 = -D2
      G2  = DCMPLX(D11,D22)
      CH1 = 0.5D0*(ZEXP(G1*FL)+ZEXP(G2*FL))
      SH1 = 0.5D0*(ZEXP(G1*FL)-ZEXP(G2*FL))
      CTH1=CH1/SH1
      CSCH=1.D0/SH1
C
      Y11=YO*CTH1
      Y12=YO*CSCH
      Y13=DCMPLX(0.0D0,0.0D0)
C
      YY(1,1)=Y11
      YY(2,2)=Y11
      YY(1,2)=-Y12
      YY(2,1)=-Y12
      YY(1,3)=Y13
      YY(1,4)=Y13
      YY(2,3)=Y13
      YY(2,4)=Y13
      YY(3,1)=Y13
      YY(3,2)=Y13
      YY(3,3)=Y13
      YY(3,4)=Y13
      YY(4,1)=Y13
      YY(4,2)=Y13
      YY(4,3)=Y13
      YY(4,4)=Y13
C
       GOTO 479
C
C+++++++++++++++++++++++++++++++++++++++++++
C ������ Y-������� ��� ����������� ����    |
C+++++++++++++++++++++++++++++++++++++++++++
  478 CONTINUE
      DO 70 I=1,4
      DO 71 J=1,4
      YY(I,J)=DCMPLX(0.0D0,0.0D0)
 71   CONTINUE
 70   CONTINUE
C
      YRR=(1.D0/RM)*FL
      YR=DCMPLX(YRR,0.0D0)
      YY(1,1)=YR
      YY(2,2)=YY(1,1)
      YY(1,2)=-YY(1,1)
      YY(2,1)=YY(1,2)
  77  CONTINUE
C
 479  CONTINUE
      IF(SKL2.EQ.1.D0)GOTO 134
      GOTO 135
 134  WRITE(6,103)F,YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(2,1),YY(2,2),YY(2
     +,3),YY(2,4),YY(3,1),YY(3,2),YY(3,3),YY(3,4),YY(4,1),YY(4,2),YY(4,3
     +),YY(4,4)
 103  FORMAT(24X,'F=',E11.4,' ��',/(E11.4,E11.4),'i',1X,(E11.4,E11.4),'i
     +',1X,(E11.4,E11.4),'i',1X,(E11.4,E11.4),'i',/(E11.4,E11.4),'i',1X,
     +(E11.4,E11.4),'i',1X,(E11.4,E11.4),'i',1X,(E11.4,E11.4),'i',/(E11.
     +4,E11.4),'i',1X,(E11.4,E11.4),'i',1X,(E11.4,E11.4),'i',1X,(E11.4,E
     +11.4),'i',/(E11.4,E11.4),'i',1X,(E11.4,E11.4),'i',1X,(E11.4,E11.4)
     +,'i',1X,(E11.4,E11.4),'i')
 135  CONTINUE
      RETURN
      END
             SUBROUTINE Z00P(W,H,T,ER,ZOP)
C
C  ������   ��������� ������������� ���������� �����
C  ��� ��������� ��������������� ���������� � 
C  ��������� ������������� ���
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      B=2.D0*H
      IF((T/H).LE.0.00005D0)THEN
      SK=DTANH(1.5707963D0*W/B)
      SK1=DSQRT(1.D0-SK*SK)
      IF(SK.GE.0.0D0.AND.SK.LE.0.7D0) GOTO 421
      SKK=1.D0/(3.1830989D-01*DLOG(2.D0*(1.D0+DSQRT(SK))/(1.D0-DSQRT(SK)
     +)))
      GOTO 422
 421  SKK=3.183098D-01*DLOG(2.D0*(1.D0+DSQRT(SK1))/(1.D0-DSQRT(SK1)))
 422  CONTINUE
      ZOP=94.24778D0*SKK/DSQRT(ER)
      ELSE
      X=T/B
      XM=2.D0/(1.D0+0.666666D0*X/(1.D0-X))
      A1=X/(3.1415926D0*(1.D0-X))*(1.D0-0.5D0*DLOG((X/(2.D0-X))**2.D0+(0
     +.0796D0*X/(W/B+1.1D0*X))**XM))
      W33=W/(B-T)
      W1BT=W33+A1
      ZOP=30.D0/DSQRT(ER)*DLOG(1.D0+1.2732395D0/W1BT*(2.546479D0/W1BT+DS
     +QRT((2.546479D0/W1BT)**2.D0+6.27D0)))
      ENDIF
      RETURN
      END
 
          SUBROUTINE SMPL(OM,P1,L1,P2,L2,P3,L3,N)
C
C     MO�E��  CB��AHH�X ��������������� �����
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION     P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION          OM,P1,P2,P3

      COMMON/SUBC/  YY(15,15),VJ(15)
      COMMON/SUBS/  SV(15,15)
      DOUBLE COMPLEX       jj,QE,QO,YR,SIE,SIO,COE,COO,TAE,TAO
      DOUBLE COMPLEX       YY,SV,VJ
      DOUBLE COMPLEX       YY11,YY12,YY13,YY14
C
      DOUBLE PRECISION          M0
      PARAMETER      (jj=(0.D0,1.D0))

C
C
      EPS  = P1(1)
      TGD  = P1(2)
      RD   = P1(4)
      H    = P2(1)
      T    = P2(2)
      W    = P3(1)
      S    = P3(2)
      FL   = P3(3)
      EREE = P3(4)
      EREO = P3(5)
      ZOE  = P3(6)
      ZOO  = P3(7)
      ALFE = P3(8)
      ALFO = P3(9)
      SKL1 = P3(10)
      SKL2 = P3(11)
C
      ER = EPS
      E0 = 8.85418D-12
      PI = 3.1415926D0
      CC = 2.99D+8
      M0 = 1.256637D-06
      RM =5.8D-07
      F=OM/(2.D0*PI)
C
      IF(OM.EQ.0.0D0) GOTO 1000
C
      IF(P3(4).GT.0.0D0.OR.P3(5).GT.0.0D0.OR.P3(6).GT.0.0D0.OR.P3(7).GT.
     +0.D0)GOTO 7345
      CALL SERF(W,H,T,EER,WEH1,ER)
      CALL SZ0(W,H,T,EER,WEH1,Z01)
      CALL COCE(E0,ER,W,H,CC,S,T,EER,Z01,CO,CE)
      CALL SERF(W,H,T,EER,WEH1,1.D0)
      CALL SZ0(W,H,T,EER,WEH1,Z01)
      CALL COCE(E0,1.D0,W,H,CC,S,T,EER,Z01,COB,CEB)
C
      EREE=CE/CEB
      EREO=CO/COB
C 
      ZOE=1.D0/(CC*DSQRT(CE*CEB))
      ZOO=1.D0/(CC*DSQRT(CO*COB))
C
      IF(SKL1.EQ.1.D0)GOTO 4147
      GOTO 4148
 4147 CALL DISPZ(OM,ZOE,ZOO,ER,EREE,EREO,H,W,S,PI)
 4148 CONTINUE
 7345 CONTINUE
      Z0=DSQRT(ZOE*ZOO)
      IF(P3(6).GT.0.0D0.OR.P3(7).GT.0.0D0)GOTO 7349
      WH=W/H
      IF(WH.LE.0.37D0.AND.W.LE.0.3D0.AND.S.LE.0.078D0)GOTO 3000
      ZOO=ZOO-4.5D0
      ZOE=ZOE-4.5D0
      GOTO 3023
 3000 CONTINUE
      ZOO=ZOO-4.5D0
      ZOE=ZOE-20.D0
 3023 CONTINUE
 7349 CONTINUE
C
      IF(SKL2.EQ.1.0D0)GOTO 1001
      GOTO 1010
 1001 WRITE(6,1020)EREE,EREO,ZOE,ZOO,Z0,F
 1020 FORMAT(/,4X,'EREE=',F5.2,2X,'EREO=',F5.2,4X,'ZOE=',F6.2,' OM',' ZO
     +O=',F6.2,' OM',4X,'Z0=',F6.2,' OM   F=',E11.4,' ��',/)
 1010 CONTINUE
C
      CALL BET1(OM,CC,EREE,EREO,BETT,BETTE,BETTO,PI)
C
      IF(SKL2.EQ.1.0D0)GOTO 1500
      GOTO 1510
 1500 FL4=2.D0*PI/BETT/4.D0
      WRITE(6,1520)FL4,F
 1520 FORMAT(/,4X,'����� LAMDA/4.=',E11.4,        '(�)   ������A F=',E11
     +.4,' ��',/)
 1510 CONTINUE
C
      IF(ALFE.GT.0.0D0.AND.ALFO.GT.0.0D0) GOTO 472
C
      CALL ALF1(W,RM,ZOE,ZOO,PI,OM,ER,EREE,EREO,TGD,CC,ALFE,ALFO)
C
  472 CONTINUE
C
      IF(SKL2.EQ.1.0D0)GOTO 105
      GOTO 106
 105  WRITE(6,107)ALFE,ALFO,F
 107  FORMAT(/,8X,'ALFE=',E11.4,' ��/�',4X,'ALFO=',E11.4,' ��/�',4X,'F='
     +,E11.4,' ��',/)
 106  CONTINUE
C
      IF(SKL1.GT.1.01D0)GOTO 4140
      GOTO 4141
 4140 CONTINUE
      Cend=W*ER/(120.D0*PI*CC)*(1.35D0/DLOG10(4.D0*H/T)+W*1.333313D0/(3.
     +D0*H*DSQRT(ER)))
      DFLE=0.92D0*DATAN(OM*Cend*ZOE)/BETTE
      DFLO=0.95D0*DATAN(OM*Cend*ZOO)/BETTO
      IF(SKL2.EQ.1.0D0)GOTO 4152
      GOTO 4155
 4152 WRITE(6,4151)Cend,DFLE,DFLO
 4151 FORMAT(/,1X,'(��� ��������� ���� �� ����� �����) ������� ������� �
     +�=',E11.4,/10X,'���������� ����� DFLE=',E11.4,1X,'���������� �����
     + DFLO=',E11.4)
 4155 CONTINUE
      GOTO 4156    
 4141 CONTINUE     
      DFLE=0.0D0
      DFLO=0.0D0
 4156 CONTINUE
C
      QE=BETTE*(FL+DFLE)-jj*ALFE*(FL+DFLE)
      QO=BETTO*(FL+DFLO)-jj*ALFO*(FL+DFLO)
      SIE=SIN(QE)
      SIO=SIN(QO)
      COE=COS(QE)
      COO=COS(QO)
      TAE=SIE/COE
      TAO=SIO/COO
C
      YY11=-jj/2.D0*((1.D0/TAE)/ZOE+(1.D0/TAO)/ZOO)
      YY14=jj/2.D0*((1.D0/SIN(QE))/ZOE+(1.D0/SIN(QO))/ZOO)
      YY13=jj/2.D0*((1.D0/SIN(QE))/ZOE-(1.D0/SIN(QO))/ZOO)
      YY12=-jj/2.D0*((1.D0/TAE)/ZOE-(1.D0/TAO)/ZOO)
C
      YY(1,1)=YY11
      YY(1,2)=YY12
      YY(1,3)=YY14
      YY(1,4)=YY13
      YY(2,1)=YY12
      YY(2,2)=YY11
      YY(2,3)=YY13
      YY(2,4)=YY14
      YY(3,1)=YY14
      YY(3,2)=YY13
      YY(3,3)=YY11
      YY(3,4)=YY12
      YY(4,1)=YY13
      YY(4,2)=YY14
      YY(4,3)=YY12
      YY(4,4)=YY11
C
      GOTO 2000
C
 1000 CONTINUE
      DO 7200 I=1,4
      DO 7300 J=1,4
      YY(I,J)=DCMPLX(0.0D0,0.0D0)
 7300 CONTINUE
 7200 CONTINUE
      YR=JJ*(1.D0/RM)*FL
      YY(1,1)=YR
      YY(2,2)=YY(1,1)
      YY(3,3)=YY(1,1)
      YY(4,4)=YY(1,1)
      YY(1,3)=-YY(1,1)
      YY(2,4)=YY(1,3)
      YY(3,1)=YY(1,3)
      YY(4,2)=YY(1,3)
C
 2000 CONTINUE
      IF(SKL2.EQ.1.0D0)GOTO 134
      GOTO 135
 134  WRITE(6,103)F,YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(2,1),YY(2,2),YY(2
     +,3),YY(2,4),YY(3,1),YY(3,2),YY(3,3),YY(3,4),YY(4,1),YY(4,2),YY(4,3
     +),YY(4,4)
 103  FORMAT(24X,'F=',E11.4,' ��',/(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11
     +.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',/(E11.4,1X
     +,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E1
     +1.4,1X,E11.4),'i',/(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X
     +,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',/(E11.4,1X,E11.4),'i
     +',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11
     +.4),'i')
 135  CONTINUE
C
      RETURN
      END

      SUBROUTINE BET1(OM,CC,EREE,EREO,BETT,BETTE,BETTO,PI)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      OLAM=CC*2.D0*PI/OM
      BETTE=2.D0*PI*DSQRT(EREE)/OLAM
      BETTO=2.D0*PI*DSQRT(EREO)/OLAM
      BETT=0.5D0*(BETTE+BETTO)
C
      RETURN
      END


      SUBROUTINE ALF1(W,RM,ZOE,ZOO,PI,OM,ER,EREE,EREO,TGD,CC,ALFE,ALFO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      FRQ=OM/(PI*2.D0)
      ALFE=27.3D0*ER*(EREE-1.D0)*TGD/((ER-1.D0)*DSQRT(EREE)*CC/FRQ)
      ALFO=27.3D0*ER*(EREO-1.D0)*TGD/((ER-1.D0)*DSQRT(EREO)*CC/FRQ)
      ALFE=ALFE/8.686D0
      ALFO=ALFO/8.686D0
      ALFEC=PI*DSQRT(FRQ*RM)/(W*ZOO)
      ALFOC=PI*DSQRT(FRQ*RM)/(W*ZOE)
      ALFE=(ALFE+ALFEC)*1.D-04+0.25D0
      ALFO=(ALFO+ALFOC)*1.D-04-0.035D0
      RETURN
      END


      SUBROUTINE COCE(E0,ER,W1,H1,CC,S1,T1,EER,Z01,CO,CE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      T2=T1/2.D0
      WH1=W1/H1
      WH2=WH1
      IF(S1.GE.T2) GOTO 1
   2  CONTINUE
      CPE=E0*ER*WH1
      CPO=E0*ER*WH2
      CFE=0.5D0*DSQRT(EER)/(CC*Z01)-0.5D0*CPE
      CFO=0.5D0*DSQRT(EER)/(CC*Z01)-0.5D0*CPO
      A=DEXP(-0.1D0*DEXP(2.33D0-2.53D0*WH1))
      CF1=CFE*DSQRT(ER/EER)/(1.D0+A*(H1/S1)*DTANH(10.D0*S1/H1))
      CE=CPE+CFE+CF1
      SK=(S1/H1)/(S1/H1+2.D0*WH2)
      CALL SK1K(SK,SKK1)
      CGA=E0*SKK1
      CGD=E0*ER/3.14115926D0*DLOG(1.D0/DTANH(S1*0.7854D0/H1))+0.65D0*CFO
     +*(0.02D0*DSQRT(ER)*H1/S1+1.D0-1.D0/(ER*ER))
      CO=CPO+CFO+CGA+CGD
      GOTO 3
  1   DTH=T1/(ER*S1)
      IF(WH1.LE.0.159155D0) THEN
      DWH=0.397887D0*T1*(1.D0+DLOG(12.5663D0*W1/T1))/H1
      ELSE
      DWH=0.397887D0*T1*(1.D0+DLOG(2.D0*H1/T1))/H1
      ENDIF
      WH1=W1/H1+DWH*(1.D0-0.5D0*DEXP(-0.69D0*DWH/DTH))
      WH2=WH1+DTH
      GOTO 2
  3   RETURN
      END

      SUBROUTINE SK1K(SK,SKK1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SK1=DSQRT(1.D0-SK)
      IF(SK.LE.0.7D0) THEN
      SKK=DLOG(2.D0*(1.D0+DSQRT(SK1))/(1.D0-DSQRT(SK1)))/3.1415926D0
      ELSE
      SKK=3.1415926D0/DLOG(2.D0*(1.D0+DSQRT(SK))/(1.D0-DSQRT(SK)))
      ENDIF
      SKK1=SKK
      RETURN
      END

      SUBROUTINE DISPZ(OM,ZOE,ZOO,ER,EREE,EREO,H,W,S,PI)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      FR1=OM/(2.D0*PI)*1.D-09
      FRO=0.7952D0*ZOO/(H*1.D+03)
      FRE=0.1988D0*ZOE/(H*1.D+03)
      GRO=0.6D0+0.018D0*ZOO
      GRE=0.6D0+0.0045D0*ZOE
      EREE=ER-(ER-EREE)/(1.D0+(FR1/FRE)**2.D0*GRE)
      EREO=ER-(ER-EREO)/(1.D0+(FR1/FRO)**2.D0*GRO)
      PKE=DTANH(PI*0.25D0*W/H)*DTANH(PI*0.25D0*(W+S)/H)
      PK1E=DSQRT(1.D0-PKE*PKE)
      PKO=(DTANH(PI*0.25D0*W/H))/(DTANH(PI*0.25D0*(W+S)/H))
      PK1O=DSQRT(1.D0-PKO*PKO)
      IF(PKE.LE.0.7D0) GOTO 4000
      KPKE=1.D0/(1.D0/PI*DLOG(2.D0*(1.D0+DSQRT(PKE))/(1.D0-DSQRT(PKE))))
      GOTO 4001
 4000 KPKE=1.D0/PI*DLOG(2.D0*(1.D0+DSQRT(PK1E))/(1.D0-DSQRT(PK1E)))
 4001 IF(PKO.LE.07) GOTO 4002
      KPKO=1.D0/(1.D0/PI*DLOG(2.D0*(1.D0+DSQRT(PKO))/(1.D0-DSQRT(PKO))))
      GOTO 4003
 4002 KPKO=1.D0/PI*DLOG(2.D0*(1.D0+DSQRT(PK1O))/(1.D0-DSQRT(PK1O)))
 4003 ZOES=2.D0*30.D0*PI*KPKE/DSQRT(ER)
      ZOOS=2.D0*30.D0*PI*KPKO/DSQRT(ER)
      ZOE=ZOES-(ZOES-ZOE)/(1.D0+(FR1/FRE)**1.6D0*GRE)
      ZOO=ZOOS-(ZOOS-ZOO)/(1.D0+(FR1/FRO)**1.6D0*GRO)
      RETURN
      END

      SUBROUTINE SERF(W1,H1,T1,EER,WEH1,ER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WH=W1/H1
      TH=T1/H1
      IF(TH.EQ.0.0D0)GOTO 1
      IF(WH.LE.0.159155D0) THEN
      DWH=0.39788736D0*TH*(1.D0+DLOG(12.56637D0*W1/T1))
      ELSE
      DWH=0.39788736D0*TH*(1.D0+DLOG(2.D0/TH))
      ENDIF
      WEH1=WH+DWH
      FWH=1.D0/DSQRT(1.D0+10.D0/WH)
      Q=(ER-1.D0)*T1/(H1*4.6D0*DSQRT(WH))
      EER=(ER+1.D0)*0.5D0+0.5D0*(ER-1.D0)*FWH-Q
      GOTO 2
   1  EER=(ER+1.D0)*0.5D0+0.5D0*(ER-1.D0)/DSQRT(1.D0+10.D0/WH)
   2  CONTINUE
      RETURN
      END

      SUBROUTINE SZ0(W1,H1,T1,EER,WEH1,Z01)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WH=W1/H1
      TH=T1/H1
      IF(TH.LE.1.0D0)GOTO 1
      IF(WH.LE.1.0D0) THEN
      Z01=60.D0*DLOG(8.D0/WEH1+0.25D0*WEH1)/DSQRT(EER)
      ELSE
      Z01=376.7D0/(DSQRT(EER)*(WEH1+1.393D0+0.667D0*DLOG(WEH1+1.444D0)))
      ENDIF
      GOTO 2
  1   IF(WH.LE.1.0D0) THEN
      Z01=60.D0*DLOG(8.D0/WH+0.25D0*WH)/DSQRT(EER)
      ELSE
      Z01=376.7D0/(DSQRT(EER)*(WH+1.393D0+0.667D0*DLOG(WH+1.444D0)))
      ENDIF
  2   CONTINUE
      RETURN
      END
      SUBROUTINE LANG(OM,P1,L1,P2,L2,P3,L3,N)
C
C     MO�E��  CB��AHH�X ��������������� �����
C         ( ���������� ���� )
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION     P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION          OM,P1,P2,P3

      COMMON/SUBC/  YY(15,15),VJ(15)
      COMMON/SUBS/  SV(15,15)
      DOUBLE COMPLEX       jj,QE,QO,YR,SIE,SIO,COE,COO,TAE,TAO
      DOUBLE COMPLEX       YY,SV,VJ
      DOUBLE COMPLEX       YY11,YY12,YY13,YY14
C
      DOUBLE PRECISION          M0
      PARAMETER      (jj=(0.D0,1.D0))

C
C
      EPS  = P1(1)
      TGD  = P1(2)
      RD   = P1(4)
      H    = P2(1)
      T    = P2(2)
      W    = P3(1)
      S    = P3(2)
      FL   = P3(3)
      EREE = P3(4)
      EREO = P3(5)
      ZOE  = P3(6)
      ZOO  = P3(7)
      ALFE = P3(8)
      ALFO = P3(9)
      ENN  = P3(10)
      SKL1 = P3(11)
      SKL2 = P3(12)
C
      ER = EPS
      E0 = 8.85418D-12
      PI = 3.1415926D0
      CC = 2.99D+8
      M0 = 1.256637D-06
      RM =5.8D-07
      F=OM/(2.D0*PI)
      WH=W/H
C
      IF(OM.EQ.0.0D0) GOTO 1000
C
      IF(P3(4).GT.0.0D0.OR.P3(5).GT.0.0D0.OR.P3(6).GT.0.0D0.OR.P3(7).GT.
     +0.D0)GOTO 7345
      CALL SERF(W,H,T,EER,WEH1,ER)
      CALL SZ0(W,H,T,EER,WEH1,Z01)
      CALL COCE(E0,ER,W,H,CC,S,T,EER,Z01,CO,CE)
      CALL SERF(W,H,T,EER,WEH1,1.D0)
      CALL SZ0(W,H,T,EER,WEH1,Z01)
      CALL COCE(E0,1.D0,W,H,CC,S,T,EER,Z01,COB,CEB)
      EREE=CE/CEB
      EREO=CO/COB
C
      B1=ENN*WH
      B11=ENN*WH
      B2=WH/(3.D0*DSQRT(ER))
      B22=WH/(3.D0*DSQRT(ER))
      B3=1.35D0/(DLOG10(4.D0*H/T))
      B4=(2.D0*ENN-1.D0)*W*S/(3.D0*H*(3.4D0*W+S)*DSQRT(ER))
      B44=4.D0*(2.D0*ENN-1.D0)*3.4D0*W/(3.D0*(S+3.4D0*W)*DSQRT(ER))
      B5=1.35D0*(2.D0*ENN-1.D0)*S/(DLOG10(4.D0*H/T)*(3.4D0*W+S))
      B55=1.35D0*(2.D0*ENN-1.D0)/DLOG10(4.D0*S*DTANH(4.D0*H/S)/PI/T)
      B01=376.9D0/DSQRT(ER)
      B02=378.D0/DSQRT(ER)
C
      ZOE=B01/(B1+B2+B3+B4+B5)
      ZOE=ZOE*1.0039843D0
C
      ZOO=B02/(B11+B22+B3+B44+B55)
      ZOO=ZOO*1.0440309D0
      IF(SKL1.EQ.1.D0)GOTO 611
      CALL DISPZ(OM,ZOE,ZOO,ER,EREE,EREO,H,W,S,PI)
 611  CONTINUE
 7345 CONTINUE
      Z0=DSQRT(ZOE*ZOO)
C
      IF(SKL2.EQ.1.D0)GOTO 1001
      GOTO 1010
 1001 WRITE(6,1020)EREE,EREO,ZOE,ZOO,Z0,F
 1020 FORMAT(4X,'EREE=',F5.2,2X,'EREO=',F5.2,4X,'ZOE=',F6.2,' OM',' ZOO=
     +',F6.2,' OM',4X,'Z0=',F6.2,' OM   F=',E11.4,' ��')
 1010 CONTINUE
C
C
      CALL BET1(OM,CC,EREE,EREO,BETT,BETTE,BETTO,PI)
C
      IF(SKL2.EQ.1.D0)GOTO 1500
      GOTO 1510
 1500 FL4=2.D0*PI/BETT/4.D0
      WRITE(6,1520)FL4,F
 1520 FORMAT(4X,'����� LAMDA/4.=',E11.4,'(�)   ������A F=',E11.4,' ��')
 1510 CONTINUE
C
      IF(ALFE.GT.0.D0.AND.ALFO.GT.0.D0) GOTO 472
C
      CALL ALF1(W,RM,ZOE,ZOO,PI,OM,ER,EREE,EREO,TGD,CC,ALFE,ALFO)
C
  472 CONTINUE
C
      IF(SKL2.EQ.1.D0)GOTO 105
      GOTO 106
 105  WRITE(6,107)ALFE,ALFO,F
 107  FORMAT(8X,'ALFE=',E11.4,' ��/�',4X,'ALFO=',E11.4,' ��/�',4X,'F=',E
     +11.4,' ��')
 106  CONTINUE
C
      QE=BETTE*FL-jj*ALFE*FL
      QO=BETTO*FL-jj*ALFO*FL
      SIE=SIN(QE)
      SIO=SIN(QO)
      COE=COS(QE)
      COO=COS(QO)
      TAE=SIE/COE
      TAO=SIO/COO
C
C
      YY11=-jj/2.D0*((1.D0/TAE)/ZOE+(1.D0/TAO)/ZOO)
      YY14=jj/2.D0*((1.D0/SIN(QE))/ZOE+(1.D0/SIN(QO))/ZOO)
      YY13=jj/2.D0*((1.D0/SIN(QE))/ZOE-(1.D0/SIN(QO))/ZOO)
      YY12=-jj/2.D0*((1.D0/TAE)/ZOE-(1.D0/TAO)/ZOO)
C

C
      YY(1,1)=YY11
      YY(1,2)=YY12
      YY(1,4)=YY14
      YY(1,3)=YY13
C
      YY(2,1)=YY12
      YY(2,2)=YY11
      YY(2,4)=YY13
      YY(2,3)=YY14
C
      YY(4,1)=YY14
      YY(4,2)=YY13
      YY(4,4)=YY11
      YY(4,3)=YY12
C
      YY(3,1)=YY13
      YY(3,2)=YY14
      YY(3,4)=YY12
      YY(3,3)=YY11
C
      GOTO 2000
C
 1000 CONTINUE
      DO 7200 I=1,4
      DO 7300 J=1,4
      YY(I,J)=DCMPLX(0.0D0,0.0D0)
 7300 CONTINUE
 7200 CONTINUE
      YR=JJ*(1.D0/RM)*FL
      YY(1,1)=YR
      YY(2,2)=YY(1,1)
      YY(3,3)=YY(1,1)
      YY(4,4)=YY(1,1)
      YY(1,4)=-YY(1,1)
      YY(2,3)=YY(1,4)
      YY(4,1)=YY(1,4)
      YY(3,2)=YY(1,4)
C
 2000 CONTINUE
      IF(SKL2.EQ.1.D0)GOTO 134
      GOTO 135
 134  WRITE(6,103)F,YY(1,1),YY(1,2),YY(1,3),YY(1,4),YY(2,1),YY(2,2),YY(2
     +,3),YY(2,4),YY(3,1),YY(3,2),YY(3,3),YY(3,4),YY(4,1),YY(4,2),YY(4,3
     +),YY(4,4)
 103  FORMAT(24X,'F=',E11.4,' ��',/(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11
     +.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',/(E11.4,1X
     +,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E1
     +1.4,1X,E11.4),'i',/(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X
     +,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',/(E11.4,1X,E11.4),'i
     +',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11.4),'i',2X,(E11.4,1X,E11
     +.4),'i')
 135  CONTINUE
C
      RETURN
      END
