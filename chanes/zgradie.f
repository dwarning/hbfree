c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE GRADIE(NTOT,N,DJ,F,SF,GR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION F(1),SF(1)
      DOUBLE PRECISION GR(1)
c$LARGE: DJ
      DOUBLE PRECISION DJ(NTOT,1)


      DO 10 I=1,N
      GR(I)=0.D0
      DO 10 J=1,N
      GR(I)=GR(I)+DJ(J,I)*F(J)*SF((J+1)/2)**2
   10 CONTINUE
C  ��� �B�X COCE�H�X ��EMEHTOB �P�MEH�ETC� O��H � TOT �E SCALE,
C  T.K.C�T� RE � IM O�HO�O ��C�A.
      RETURN

C     DEBUG SUBTRACE,INIT(GR)
      END
