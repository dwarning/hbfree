c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




       SUBROUTINE SVUTL(OM,P1,L1,P2,L2,P3,L3,N)
C
C
C     MOδEμψ Mπμ - CBρϊAHHωX μιHικ C πOTEPρMι,νιλςο-
C     πομοσλοχωθ μιξικ  ι σοεδιξεξιε μεξ
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION     P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION          OM,P1,P2,P3

      COMMON/SUBC/  YY(15,15),VJ(15)
      COMMON/SUBS/  SV(15,15)
      DOUBLE PRECISION          M0

C

C
C
      EPS= P1(1)
      TGD= P1(2)
      RD = P1(4)
      H  = P2(1)
      T  = P2(2)
      W  = P3(1)
      FL = P3(2)
      ZO1= P3(3)
      ZO2= P3(4)
      ALFA=P3(5)
      ZN0=P3(6)
      SKL1 = P3(7)
      SKL2= P3(8)
      SKL3= P3(9)
C
      ER = EPS
      E0 = 8.85418D-12
      PI = 3.1415926D0
      CC = 2.99D+8
      M0 = 1.256637D-06

C      χωβοςλα όμενεξ
C   λϊ-STL1=5;
C   θθ-STL=4;
C   σλαώελ-STL=9;
C
C
C
      IF(SKL1.EQ.4.D0) GO TO 10
      IF(SKL1.EQ.5.D0) GO TO 20
      IF(SKL1.EQ.9.D0) GO TO 30
      PRINT 40,SKL1
  40  FORMAT('  ξελοςςελτξωκ λοδ όμενεξτα :',E10.5,'οσταξοχ')
      STOP

C      ςαϊονλξυτωκ ϋμεκ
10    IF(OM.EQ.0.D0)GO TO 500
       I=-1
      CALL BET11(OM,CC,BETT,PI,W,H,ER)
      CALL SHLEYF(I,ZO1,BETT,FL,W,H,ER)
      N=2
      CALL TEST(N)
      GO TO 50

C      λοςοτλοϊανλξυτωκ ϋμεκ
20    IF(OM.EQ.0.D0) GO TO 501
      I=1
      CALL BET11(OM,CC,BETT,PI,W,H,ER)
      CALL SHLEYF(I,ZO1,BETT,FL,W,H,ER)
      N=2
      CALL TEST(N)
      GO TO 50

C      σλαώολ χομξοχοηο σοπςοτιχμεξιρ
30    CALL  STEPSC(ZO1,ZO2)
      N=2
      CALL TEST(N)
      GO TO 50
 500  SV(1,1)=0.0D0
      SV(2,2)=0.0D0
      SV(1,2)=0.99997D0
      SV(2,1)=0.99997D0
      N=2
      CALL TEST(N)
      GO TO 50
 501  SV(1,1)=.99997D0
      SV(1,2)=0.0D0
      SV(2,1)=0.0D0
      SV(2,2)=.99997D0
      N=2
      CALL TEST(N)
 50   END

      SUBROUTINE BET11(OM,CC,BETT,PI,W,H,ER)
C      IF(OM.EQ.0.0) THEN
C     BETT=0.
C      ELSE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CALL EREFF(W,H,ER,ERE)
      OLAM=CC*2.D0*PI/OM
      BETT=2.D0*PI*DSQRT(ERE)/OLAM
C      ENDIF
      RETURN
      END

C      ςασώετ ϋμεκ
      SUBROUTINE SHLEYF(I,Z0S,BETT,FL,W,H,ER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SUBS/SV(15,15)
      DOUBLE COMPLEX ST,DS
      ST=(0.D0,1.D0)
      CALL Z0MPL(Z,W,H,ER)
      DS=ST*2.D0*Z/(DTAN(BETT*FL))**I/Z0S-I
      SV(1,1)=I/DS
      SV(1,2)=(DS+I)/DS
      SV(2,1)=SV(1,2)
      SV(2,2)=-SV(1,1)
      RETURN
      END

C       ςασώετ χομξοχοηο σοπςοτιχμεξιρ
      SUBROUTINE Z0MPL(Z,W,H,ER)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WH=W/H
      CALL EREFF(W,H,ER,ERE)
      IF (WH.GT.1.D0) GO TO 5
      Z=376.7D0/DSQRT(ERE)*DLOG(8.D0/WH+.25D0*WH)
      RETURN
   5  Z=60.D0/DSQRT(ERE)/(WH+1.393D0+.667D0*DLOG(WH+1.444D0))
      RETURN
      END

C      ςασώετ όζζελτιχξοκ διόμελτςιώεσλοκ ποστορξξοκ
      SUBROUTINE EREFF(W,H,ER,ERE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      WH=W/H
      ERE=(ER+1.D0)/2.D0+(ER-1.D0)/2.D0/DSQRT(1.D0+10.D0*WH)
      RETURN
      END

C      σλαώολ χομξοχοηο σοπςοτιχμεξιρ
      SUBROUTINE STEPSC(Z1,Z2)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SUBS/ SV(15,15)
      DS=Z1+Z2
      SV(1,1)=(Z2-Z1)/DS
      SV(1,2)=2*DSQRT(Z1*Z2)/DS
      SV(2,1)=SV(1,2)
      SV(2,2)=-SV(1,1)
      RETURN
      END

