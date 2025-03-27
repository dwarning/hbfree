c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE LUFRW(ALU,B,NTOT,N,NF,NEND,FLAG)
C**************************************************************
C*       נ/נ נPסMOחO יCKלא‏EHיס                               *
C*                                                            *
C*       B-BEKTOP CBOגOהHשX ‏לEHOB                            *
C*                                                            *
C**************************************************************
C$LARGE: ALU,B
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX ALU(NTOT,NTOT),B(NTOT)
      INTEGER FLAG
C  נPOBEPKי BXOהHשX הAHHשX
      FLAG=0
      IF((NF.GT.N).OR.(NEND.GT.N)) FLAG=5
      IF(NF.GT.NEND) FLAG=4
      IF((NF.GT.NTOT).OR.(NEND.GT.NTOT).OR.(N.GT.NTOT)) FLAG=3
      IF((NF.LT.1).OR.(NEND.LT.0).OR.(N.LT.1)) FLAG=2
      IF(NTOT.LT.1) FLAG=1
C  יCKלא‏EHיE:ECלי NEND=NF-1,TO BEהץ‎יX üלEMEHTOB - 0
C  (HOPMAלרHOE OKOH‏AHיE.BOתBPAT)  (!) FLAG=4 (!)
      IF(NEND-NF+1.EQ.0) RETURN
      IF(FLAG.NE.0) WRITE (6, 55) FLAG
      IF(FLAG.NE.0) RETURN
C  ECלי N=1,TO HEOגXOהיMO BשנOלHיTר TOלרKO  B(N)=B(N)/ALU(N,N)
      IF(N.EQ.1) GO TO 45
C  נPסMOE יCKלא‏EHיE
C  ECלי NEND=N,TO NEND=NEND-1
      NE=NEND
      IF(NEND.EQ.N) NE=NEND-1
      DO 40 JCOL=NF,NE
      B(JCOL)=B(JCOL)/(ALU(JCOL,JCOL)+0.1D-30)
      JCOLP1=JCOL+1
      DO 50 IROW=JCOLP1,N
      B(IROW)=B(IROW)-ALU(IROW,JCOL)*B(JCOL)
   50 CONTINUE
   40 CONTINUE
C  נOüTOMץ BMECTO NEND B נPEהשהץ‎EM KOMMEHTAPיי יCנOלרתOBAלי NE.
C  י E‎E נOTOMץ,‏TO HABOהיTCס OûיגKA: NEND ץMEHרûAETCס HA
C  EהיHידץ י נEPEהAETCס הAלרûE.
   45 IF(NEND.EQ.N) B(N)=B(N)/ALU(N,N)
      RETURN
   55 FORMAT('  LUFRW: '/'  INPUT DATA ERROR. FLAG=',I4)
      END
