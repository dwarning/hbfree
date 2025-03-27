c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c



      SUBROUTINE INITI(U,YY,VECTJ,ISIZE_MAXNODE,IERR)

C*********************************************************************
C* נ/נ 'HA‏AלO'-BשתשBAא‎Aס הלס TOנOOגPAגOTKי,נOCTPOEHיס CETKי ‏ACTOT *
C* י זOPMיPOBAHיס  Y י J HA ‏ACTOTHOך CETKE. B Cלץ‏AE ליHEךHOך CXEMש *
C*                     -Bש‏יCלסET BEKTOP PEûEHיך                     *
C*********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX          S
C   קמוףומש יתםומומיס במבלןחי‏משו MAIN  30.01.91 (ףועהאכ ח.ק.)
C $LARGE: J,Y
C      COMMON/MATY/    J(15,20),Y(15,15,20)
C     COMPLEX          Y,J

      DOUBLE PRECISION U(1)
      
      DOUBLE COMPLEX YY(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX VECTJ(ISIZE_MAXNODE)
      
      COMMON/MATY/     BUFFER (6000), BUFLEN
      DOUBLE COMPLEX          BUFFER
      INTEGER*4        BUFLEN
      COMMON/BLW1/     W,W1
      COMMON/BLW2/     WR,WS
      COMMON/FRE/      F(2)
      DOUBLE PRECISION             W(20),W1(200),F
      INTEGER          WR(20,20),WS(20,20)
      COMMON/BLK1/     KR,KR1,KC,KC1,NNR,NNR1,MN,MN1
      COMMON/BLK2/     KNC,KNR,KN,KNR1,KN1
      INTEGER          KR(20),KC(10),NNR(10),KR1(200),KC1(20),NNR1(20)
      INTEGER          MN(2,20),MN1(2,200)
      COMMON/KOLNAL/   KOL,NAL
      COMMON/MEP/      MEPHF,FLGMNW
      INTEGER          KOL(4),FLGMNW,IP/6/
      LOGICAL          NAL(4)
      COMMON /BLMNI/   MNI(2,20),KMNI
      DOUBLE PRECISION             OM,PI
      COMMON/PRINT/    KPRLEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,              
     +   KPRVAR,KPRGRF,KPRQUP
      INTEGER IERR



C     BשתOB נ/נ COPTיPOBKי י נEPEKOהיPOBKי HOMEPOB ץתלOB
      CALL SORTUZ

C     נPOBEPKA HAלי‏יס HEליHEךHשX üלEMEHTOB
C     IF(KOL(3)-KOL(4).EQ.0) WRITE(IP, 1020)

C     נ/נ OנPEהEלEHיס MAX KOלי‏ECTBA BXOהHשX/BשXOהHשX BEלי‏יH הלס
C           נPEOגP-ס זץPרE
      CALL DETSYN(MAXSYN)

C     נOCTPOEHיE CETOK ‏ACTOT י נP.(CM.KOMMEHT.B נ/נ)
      DO 80 II=1,KN
      MN(1,II)=MNI(1,II)
80    MN(2,II)=MNI(2,II)

      CALL INKOOR(MAXSYN,MNMAX,F(1),F(2),*100, *101)


C     זOPMיPOBAHיE Y י J HA KAצהOך ‏ACTOTE CETKי MN ( MACCיB W )

      DO 30 IO=1,KN
      IOO=IO
   30 CALL STEPFR (W(IOO),IOO,YY,VECTJ,ISIZE_MAXNODE)
C     נPEOגPAתOBAHיE HYMEPAדיי YתלOB
      K3=KOL(3)
      K23=KOL(2)+K3
      K12=KOL(1)+KOL(2)
      CALL TOPO(-1,K12)
C     תAHECEHיE TOנO-X-K MHOחOנOלאCHיKA B MACCיBש MPOINT & NODEEL
      CALL TOPOIN(K23,1)
      CALL TOPOIN(K3,2)

C     ECלי HET HEליHEךHשX üלEMEHTOB י חPAHי‏HשX ץתלOB
C                                   - Bש‏ECלEHיE BEKTOPA PEûEHיך
      IF(NAL(3).OR.NAL(4)) then
        IERR=0
        RETURN
      endif 
       
C      WRITE(IP, 1020)
      DO 50 IRRO=1,KN
      IRR=IRRO
   50 CALL STBACK (U,IRR)

      IERR=1  
      RETURN

C***** גOלרûAס CETKA MN1 ( גOלרûE 200 ) *****************************
  100 WRITE(IP, 1000)

      IERR =2 
      RETURN 

C***** KNC > KNCMAX - BשXOה B תAנE‎EHHץא OגלACTר *******************
  101 WRITE(IP, 1010) MNMAX

      IERR=3
      RETURN

C**********************************************************************
C      ENTRY AGAIN
C this entry should not used any more
C
C     BXOה הלס נOBTOPHOחO BשתOBA זOPMיPOBAHיס יתMEHסEMשX
C                       üלEMEHTOB
C
C
C     BOCCTAHOBלEHיE HYMEPAדיי YתלOB
C      K12=KOL(1)+KOL(2)
C      CALL TOPO(1,K12)
C
C
C     נOBTOPHOE זOPMיPOBAHיE BAPריPYEMOך ‏ACTי
C      DO 60 IO=1,KN
C      IOO=IO
C   60 CALL DOUBLE(W(IOO),IOO)
C
C     נPEOגPAתOBAHיE HYM-דיי YתלOB /OנסTר/
C      CALL TOPO(-1,K12)
C
C     תAHECEHיE TOנO-X-K
C      K3=KOL(3)
C      CALL TOPOIN(K3,2)
C
C     ECלי HET HEליHEךHשX üלEMEHTOB י חPAHי‏HשX ץתלOB
C                                   - Bש‏יCלEHיE BEKTOPA PEûEHיס
C      IF(NAL(3).OR.NAL(4)) RETURN
C      WRITE(IP, 1020)
C      DO 70 IRRO=1,KN
C      IRR= IRRO
C      CALL STBACK(U,IRR)
C   70 CONTINUE
C
C
C      RETURN 1

 1000 FORMAT(/10X,'SIZE OF AUX. FREQUENCY GRID IS MORE THAN MAX=200.')
 1010 FORMAT(/10X,'CANT COMPUTE HARMONICS HIGHER THAN ',I4)
C
      END
