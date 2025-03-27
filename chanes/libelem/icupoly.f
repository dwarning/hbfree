c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



C******** �T�H C BAX I=I0+....+I5(U-U0)**5      ******************
C    �C�O����ETC� ��� MO�E��POBAH�� �T�H'OB B
C     ��E ��O�HO .
      SUBROUTINE ICUPL1(IVAR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 KLC,KLV,KNL,IVAR
      COMMON/MDLA/MT(15)

      MT(1)=2
      MT(2)=2
      MT(3)=2
      MT(4)=2
      KLC= 0
      KLV= 0
      KNL=4
      RETURN
C     DEBUG SUBTRACE
      END


      SUBROUTINE ICUPL2(OM,P1,L1,P2,L2,P3,L3)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/SUBC/Y(15,15),J(15)
      DOUBLE PRECISION P1    ,P2    ,P3    ,ZN
      DOUBLE COMPLEX Y,J
      DIMENSION P1(L1),P2(L2),P3(L3)

      DO 10 I=1,4
      DO 10 II=1,4
      Y(II,I)= DCMPLX(0.0D0,0.0D0)
  10  CONTINUE

      RETURN
C     DEBUG SUBTRACE
      END


      SUBROUTINE ICUPL3(NG,P1,L1,P2,L2,P3,L3,B1,KNC2,NR,*)
C
C      �/� MAT. MO�E��   �O��HOM�A��HO�O �T�H'A .
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION P1,P2,P3
      DIMENSION P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION A0,A1,A2,A3,A4,A5,U0
      DIMENSION B1(KNC2,NR)
      DOUBLE PRECISION B1
      UN1(U)=A0+U*(A1+U*(A2+U*(A3+U*(A4+U*A5))))
      U0=P3(1)
      A0=P3(2)
      A1=P3(3)
      A2=P3(4)
      A3=P3(5)
      A4=P3(6)
      A5=P3(7)
C
      DO 10 K=1,KNC2,2
      U=B1(K,1)-U0
      B1(K,1)=UN1(U)
   10 B1(K+1,1)=0.0D0
      RETURN
C     DEBUG SUBTRACE,INIT
      END

      SUBROUTINE ICUPL4(NG,P1,L1,P2,L2,P3,L3,B1,KNC2,NR,*)

C
C      CM. �/� ICUJ3
C

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION B1
      DOUBLE PRECISION P1,P2,P3
      DIMENSION P1(L1),P2(L2),P3(L3)
      DOUBLE PRECISION A0,A1,A2,A3,A4,A5,U0
      DIMENSION  B1(KNC2,NR)
      DUN1(U)=A1+U*(2.D0*A2+U*(3.D0*A3+U*(4.D0*A4+U*5.D0*A5)))
      U0=P3(1)
      A1=P3(3)
      A2=P3(4)
      A3=P3(5)
      A4=P3(6)
      A5=P3(7)

      DO 10 K=1,KNC2,2
      U=B1(K,1)-U0
      B1(K,1)= DUN1(U)
      B1(K+1,1)= 0.0D0
   10 CONTINUE
      RETURN
C     DEBUG SUBTRACE
      END

      SUBROUTINE ICUPL5(NOI,NOU,EXIST,KOI,KOUV,KOPV,NR1V,NB1V)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER NOI(5,2),NOU(5,2)
      LOGICAL EXIST(5,2)
      INTEGER KOI,KOUV(5),KOPV(5),NR1V(5),NB1V(5)
      NOI(1,1)=3
      NOI(1,2)=4
      NOU(1,1)=1
      NOU(1,2)=2
      EXIST(1,1)=.TRUE.
      EXIST(1,2)=.FALSE.
      KOI    =1
      KOUV(1)=1
      KOPV(1)=0
      NR1V(1)=1
      NB1V(1)=1
      KGR=1
      RETURN
      END

