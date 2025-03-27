c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE CBARR1(IVAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KLC,KLV,KNL,IVAR
      COMMON/MDLA/MT(15)

      MT(1)=2
      MT(2)=2
      KLC= 0
      KLV= 0
      KNL=2
      RETURN
C     DEBUG SUBTRACE
      END


      SUBROUTINE CBARR2(OM,P1,L1,P2,L2,P3,L3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SUBC/Y(15,15),J(15)
      DOUBLE PRECISION P1    ,P2    ,P3
      DOUBLE COMPLEX Y,J
      DIMENSION P1(L1),P2(L2),P3(L3)

      Y(1,1)= DCMPLX(0.0D0,0.0D0)
      Y(1,2)= DCMPLX(0.0D0,0.0D0)
      Y(2,1)= DCMPLX(0.0D0,0.0D0)
      Y(2,2)= DCMPLX(0.0D0,0.0D0)
      RETURN
C     DEBUG SUBTRACE
      END


      SUBROUTINE CBARR3(NG,P1,L1,P2,L2,P3,L3,B1,KNC2,NR,*)
C
C      �/� MAT. MO�E�� �AP�EPHO� EMKOCT�
C          =�AB�C�MOCT� I(U)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION P1,P2,P3
      DIMENSION P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION B1
      DIMENSION B1(KNC2,NR)
      DOUBLE PRECISION C0,FI0,AN,UG

      F1(U)=AN/(FI0-U)
      C(U)=C0/(1.D0-U/FI0)**AN
      C0=P3(1)
      FI0=P3(2)
      AN=P3(3)
      UG=P3(4)
C
      DO 10 K=1,KNC2,2
      U=B1(K,1)
      IF(U.LT.UG) GO TO 20
      B1(K,1)=C(UG)*(1.D0+F1(UG)*(2.D0*U-UG))*B1(K,2)
      GO TO 15
   20 B1(K,1)=C(U)*(1.D0+F1(U)*U)*B1(K,2)
   15 B1(K+1,1)=0.0D0
   10 B1(K+1,2)=0.0D0
      RETURN
C     DEBUG SUBTRACE,INIT(C0,FI0,AN,UG,U,B1  )
      END


      SUBROUTINE CBARR4(NG,P1,L1,P2,L2,P3,L3,B1,KNC2,NR,*)

C
C      CM. �/� CBARR3
C

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION P1,P2,P3
      DIMENSION P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION B1   ,C0,FI0,AN,UG
      DIMENSION  B1(KNC2,NR)
      F1(U)=AN/(FI0-U)
      C(U)=C0/(1-U/FI0)**AN
      F2(U)=(AN+1.D0)/(FI0-U)
      C0=P3(1)
      FI0=P3(2)
      AN=P3(3)
      UG=P3(4)
C
      DO 10 K=1,KNC2,2
      U=B1(K,1)
      IF(U.LT.UG) GO TO 20
      B1(K,1)=2.D0*C(UG)*F1(UG)*B1(K,2)
      B1(K,2)=C(UG)*(1.D0+F1(UG)*(2.D0*U-UG))
      B1(K+1,1)=0.D0
      B1(K+1,2)=0.D0
      GO TO 10
   20 B1(K,1)=C(U)*F1(U)*(2.D0+F2(U)*U)*B1(K,2)
      B1(K,2)=C(U)*(1.D0+F1(U)*U)
      B1(K+1,1)=0.D0
      B1(K+1,2)=0.D0
   10 CONTINUE
      RETURN
C     DEBUG INIT(C0,AN,FI0,UG,B1,U  )
      END


      SUBROUTINE CBARR5(NOI,NOU,EXIST,KOI,KOUV,KOPV,NR1V,NB1V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER NOI(5,2),NOU(5,2)
      LOGICAL EXIST(5,2)
      INTEGER KOI,KOUV(5),KOPV(5),NR1V(5),NB1V(5)
      NOI(1,1)=1
      NOI(1,2)=2
      NOU(1,1)=1
      NOU(1,2)=2
      EXIST(1,1)=.TRUE.
      EXIST(1,2)=.TRUE.
      KOI    =1
      KOUV(1)=1
      KOPV(1)=1
      NR1V(1)=2
      NB1V(1)=2
      RETURN
C
      END


