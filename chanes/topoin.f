c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE TOPOIN(KOLPOL,NOMER)
C
C     �/� �AHOC�T B MACC�B� , XPAH���E �H�OPMA��� O CXEME,
C     �AHH�E O C�OPM�POBAHHOM � �A��CAHHOM HA DA MHO�O�O��CH�KE
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'
C      COMMON/POINT/M(1)/NODEL/N(1)
C      COMMON/POINTR/NMPNT,NNODE,NP,LN,LP,NT,LT
      INTEGER NAM(6)/4HNPOL,4HREDU,4HTERM,4HNOMT,4HSIMB,4HADDR/

      I=(NMPNT+NOMER-1)*20+1
C     �AHECEH�E B MPOINT
      MPOINT(I)=NAM(1)
      MPOINT(I+1)=NAM(2)
      MPOINT(I+2)=NOMER
      MPOINT(I+3)=NOMER
      MPOINT(I+4)=1
      MPOINT(I+5)=4
      MPOINT(I+6)=KOLPOL+8
      MPOINT(I+7)=0
      MPOINT(I+8)=0
      MPOINT(I+9)=NNODE+(NOMER-1)*MPOINT(NMPNT*20+7)
      MPOINT(I+10)=0
      MPOINT(I+11)=0
      MPOINT(I+12)=0
      MPOINT(I+13)=0
      MPOINT(I+14)=NOMER-1
      MPOINT(I+15)= 0
      MPOINT(I+16)= 0
      MPOINT(I+17)= 0
      MPOINT(I+18)= 0
      MPOINT(I+19)=0

      K=MPOINT(I+9)
C     �AHECEH�E B NODEEL
      NODEEL(K)  =NAM(3)
      NODEEL(K+1)= KOLPOL
      NODEEL(K+2)= KOLPOL*(KOLPOL+1)
      NODEEL(K+3)=NAM(4)
       L=K+3
        DO 10 J=1,KOLPOL
   10   NODEEL(L+J)=J
       L=L+KOLPOL
      NODEEL(L+1)=NAM(5)
      NODEEL(L+2)=NAM(6)
      NODEEL(L+3)= NOMER-1
      NODEEL(L+4)=       0
       RETURN
C      DEBUG INIT,SUBTRACE
       END
