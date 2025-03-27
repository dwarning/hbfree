c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE LUSLV (ALU,NTOT,N,NF,NEND,FLAG)
C**************************************************************
C*          נ/נ KOMנלEKCHOך LU-זAKTOPיתAדיי                   *
C*   נPEהHAתHA‏EHA הלס זAKTOPיתAדיי ‏ACTי MATPידש ALU .       *
C*                                                            *
C*          זOPMAלרHשE נAPAMETPש:                             *
C*    ALU-יCXOהHAס MATPידA,יMEET HEHOPMAלר.(HEKAHOHי‏EC.) הלס *
C*   Y-MATPידש Bיה:HA BשXOהE:L-MATPידA נPEהCTABלסET COגOך נPO-*
C*   יתBEהEHיE D HA L' (L=L'*D),נPי‏EM L' יMEET EהיHי‏Hץא הי- *
C*   AחOHAלר,U-MATPידA TAKצE יMEET EהיHי‏Hץא היAחOHAלר,A OT   *
C*   üלEMEHTA A(NEND+1,NEND+1) HA‏יHAETCס HEPAתלOצEHHAס MAT-  *
C*   PידA,KOTOPAס COOTBETCTBץET חPAזץ C יCKלא‏EHHשMי ץתלAMי   *
C*   1..N                                                     *
C*   HEPAתלOצEHHAס MATPידA COXPAHסET CBOך HEKAHOHי‏ECKיך Bיה. *
C*   B PAתלOצEHHOך ‏ACTי היAחOHAלר KAHOHיתיPOBAHA.            *
C*   HEKAHOHי‏ECKOך C‏יTAETCס Y-MATPידA,היAחOHAלרHשE üלEMEHTש *
C*   KOTOPOך HE נOלHשE ץתלOBשE נPOBOהיMOCTי,A TOלרKO נPOBOהי- *
C*   MOCTי MEצהץ I-M י HץלEBשM ץתלAMי.                        *
C*    COMPLEX ALU(NTOT,NTOT)                                  *
C*      NTOT-נOלHשך PAתMEP MATPידש ALU (נO Oג'סBלEHיא PAתMEP- *
C*  HOCTי B BשתשBAא‎Eך נPOחPAMME);                            *
C*      N-PAתMEP OגPAגATשBAEMOך (T.E. תAנOלHEHHOך) ‏ACTי ALU; *
C*      NF-HOMEP CTPOKי,C KOTOPOך HEOגXOהיMO HA‏ATר זAKTOPי-  *
C*   תAדיא;                                                   *
C*      NEND-HOMEP CTPOKי,KOTOPOך HEOגXOהיMO תAKOH‏יTר זAKTO- *
C*   PיתAדיא;                                                 *
C*      FLAG-ץKAתATEלר OûיגOK:                                *
C*        0-OûיגOK HET,                                       *
C*        1-N,NEND ילי NF MEHרûE 1                            *
C*        2-N,NEND ילי NF גOלרûE NTOT                         *
C*        3-NF גOלרûE NEND                                    *
C*        4-NF ילי NEND גOלרûE N                              *
C*                                                            *
C**************************************************************

C$LARGE: ALU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX ALU(NTOT,NTOT)
      DOUBLE COMPLEX DIAG(100),AKI,DI,SUM,DIFF
      INTEGER FLAG
C  נPOBEPKA BXOהHשX הAHHשX
C     DO 4 IV=1,N
C     DO 4 JV=1,N
C     WRITE(6, 3) IV, JV, ALU(IV,JV)
C   3 FORMAT(2X,'LUSLV: ALU(',I3,',',I3,')=',E12.5,2X,E12.5)
C   4 CONTINUE
      FLAG=0
      IF ((NF.GT.N).OR.(NEND.GT.N)) FLAG=5
      IF (NF.GT.NEND) FLAG=4
      IF ((NF.GT.NTOT).OR.(NEND.GT.NTOT).OR.(N.GT.NTOT))FLAG=3
      IF ((NF.LT.1).OR.(NEND.LT.0).OR.(N.LT.1)) FLAG=2
      IF (NTOT.LT.1) FLAG=1
C  יCKלא‏EHיE:ECלי NEND=NF-1,TO BEהץ‎יX üלEMEHTOB - 0
C  (HOPMAלרHOE OKOH‏AHיE.BOתBPAT)  (!) FLAG=4 (!)
      IF(NEND-NF+1.EQ.0) RETURN
      IF(FLAG.NE.0)  WRITE(6, 35)  FLAG
      IF(FLAG.NE.0)RETURN
C  ECלי N=1,TO זAKTOPיתAדיא MOצHO C‏יTATר OKOH‏EHHOך - MATPידA
C  COהEPציT OהיH üלEMEHT ALU(1,1)
      IF(N.EQ.1) RETURN
C  נPיBEהEHיE AKTיBHOך ‏ACTי Y-MATPידש ALU K KAHOHי‏ECKOMץ
C  Bיהץ. היAחOHAלרHשE üלEMEHTש גץהEM XPAHיTר B OTהEלרHOM
C  BEKTOPE הBOךHOך TO‏HOCTי. נOCלE יCנOלרתOBAHיס üלEMEHTA BEK-
C  TOPA DIAG OH תAHOCיTCס HA CBOE MECTO B ALU. HEיCנOלרתOBAHHשE
C  נOTOM נPיBOהסTCס K HEKAHOHי‏. Bיהץ י תAHOCסTCס HA CBOי MECTA
      DO 5 IROW=NF,N
      SUM=2.D0*ALU(IROW,IROW)
      DO 7 JCOL=NF,N
   7  SUM=SUM-ALU(IROW,JCOL)
      DIAG(IROW)=SUM
   5  CONTINUE
C  זAKTOPיתAדיס NEND-NF+1 BEהץ‎יX üלEMEHTOB
C  יCKלא‏EHיE: ECלי NEND=N,TO NEND=NEND-1
      NE=NEND
      IF(NEND.EQ.N) NE=NEND-1
      DO 10 KPIV=NF,NE
C תAHECEHיE יCנOלרתOBAHHOחO üלEMEHTA DIAG B A(KPIV,KPIV)
C     WRITE(6,1111) NF,NE,KPIV,DIAG(KPIV)
C1111 FORMAT(2X,'LUSLV: NF=',I5,' NE=',I5,' KPIV=',I5,' DIAG=',2E12.5)
      ALU(KPIV,KPIV)=DIAG(KPIV)
C  HOPMיPץא‎יך MHOציTEלר הלס üל-TOB BEהץ‎Eך CTPOKי
      DI=1.D0/(DIAG(KPIV)+0.1D-30)
C  BO BCEX CTOלגדAX AKTיBHOך נOהMATPידש:
      KPIVP1=KPIV+1
      DO 20 JCOL=KPIVP1,N
C  HOPMיPץEM üלEMEHTש BEהץ‎Eך CTPOKי
      AKI=ALU(KPIV,JCOL)*DI
      ALU(KPIV,JCOL)=AKI
C  הOMHOציM EחO HA üל-T BEה.CTOלגדA
C  י Bש‏TEM ית TEKץ‎EחO üל-TA.
C  י TAK CO BCEMי üל-TAMי CTOלגדA
      DO 30 IROW=KPIVP1,N
      IF(IROW.NE.JCOL)ALU(IROW,JCOL)=ALU(IROW,JCOL)-AKI*ALU(IROW,KPIV)
      IF(IROW.EQ.JCOL)    DIAG(IROW)=DIAG(IROW)    -AKI*ALU(IROW,KPIV)
   30 CONTINUE
   20 CONTINUE
   10 CONTINUE
C  נPיBEהEHיE OCTABûEךCס ‏ACTי AKTיBHOך נOהMATPידש
C  K HEKAHOHי‏ECKOMץ Bיהץ.
C
C  נO BCEM CTPOKAM AKTיBHOך נOהMATPידש:
      NEP1=NE+1
      DO 23 IROW=NEP1,N
      DIFF=DIAG(IROW)
C  נO BCEM üלEMEHTAM CTPOKי (KPOME היAחOHAלרHOחO)
      DO 25 JCOL=NEP1,N
      IF(IROW.NE.JCOL)DIFF=DIFF+ALU(IROW,JCOL)
   25 CONTINUE
      ALU(IROW,IROW)=DIFF
   23 CONTINUE
      RETURN
   35 FORMAT('    LUSLV:'/'   ERR IN INPUT DATA.FLAG=',I4)
      END
