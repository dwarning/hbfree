c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE TOPO(S,ND)
C
C     π/π πEPEKOδιPOBKι N υϊμOB ( CδBιη B CTOPOHυ S HA BEμιώιHυ
C                      ND ιμι ND+1)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'
C      COMMON/POINT/M(1)/NODEL/N(1)/POINTR/NMPNT,NOT,NP,LN,LP,NT,LT
      INTEGER M,N,S,END
      IF(ND.EQ.0) RETURN
      S=S
      ND=ND
      END=NMPNT*20
C     DO πO TιπAM όμ-TOB
      DO 10 I=1,END,20
      NFIR=MPOINT(I+9)
      NLEN=MPOINT(I+6)
      ITYP=MPOINT(I+5)
      IE=MPOINT(I+4)
C     DO πO όμ-TAM BHυTPι TιπA
      DO 10 II=1,IE
      NAT=NFIR+(II-1)*NLEN
      GO TO(20,30,40),ITYP
C     υCTAHOBKA ηPAHιγ ιϊMEHEHιρ ιHδEKCOB όμ-TOB MACCιBA N,
C     XPAHρύιX ϊHAώEHιρ υϊμOB ( B ϊAB-Tι OT TιπA )
   20 NN=1
      NK=2
      GO TO 50
   30 NN=4
      NK=NODEEL(NAT+1)+3
      GO TO 50
   40 NN=1
      NK=MPOINT(I+7)
   50 DO 60 III=NN,NK
C     Bωώ-E ϊH-ρ υϊμA
      ITI=NODEEL(NAT+III)
      IF(ITI.EQ.0) GO TO 60
C     CδBιη ϊHAώEHιρ υϊμA
      IF(S.LT.0.AND.ITI.GT.ND)GOTO 70
      IF(S.LT.0.AND.ITI.LE.ND)GOTO 75
      IF(S.GT.0.AND.ITI.GT.0) GOTO 80
      IF(S.GT.0.AND.ITI.LT.0) GOTO 85
      GOTO 60
   70 ITI=ITI-ND
      GOTO 100
   75 ITI=ITI-ND-1
      GOTO 100
   80 ITI=ITI+ND
      GOTO 100
   85 ITI=ITI+ND+1
C
  100 NODEEL(NAT+III)=ITI
   60 CONTINUE
   10 CONTINUE
      RETURN
C     DEBUG SUBTRACE,INIT(ITI,S,ND)
      END
