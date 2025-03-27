c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c


        
        SUBROUTINE LEN_S
C ***
C *** נPOחPAMMA ‏TEHיס BXOהHOחO תAהAHיס
C ***
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'charint.i'
      include 'circuit.i'
C       COMMON/NODEL/   NODEEL(500)
C       COMMON/POINT/   MPOINT(500)
C       COMMON/PARAMS/  PARAM (2000)
C       COMMON/POINTR/  NMPNT,NNODE,NPARAM,LENNOD,LENPAR,NNETPR,LENNTP

       COMMON/FRE/        F(2)
       COMMON/BLK1/    KR,KR1,KC,KC1,NNR,NNR1,MN,MN1
       INTEGER         KR(20),KR1(200),KC(10),KC1(20)
       INTEGER         NNR(10),NNR1(20),MN(2,20),MN1(2,200)
       COMMON/BLK2/    KNC,KNR,KN,KNR1,KN1
       COMMON/SERV/    EPSIW,LIMERR,KITU
       COMMON/NEWTON/  EPSSOL,EPSDU,EPSMIN,MAXDU,LIMIT
       DOUBLE PRECISION            EPSSOL,EPSDU,EPSMIN,MAXDU
       COMMON/MODGLB/  MGLOB,IAPR /BLMNI/MNI(2,20),KMNI
       COMMON/TYPVAL/  TYPU,TYPI
       DOUBLE PRECISION            TYPU,TYPI
       COMMON/PRINT /  KPRLEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,
     +                 KPRVAR,KPRGRF,KPRQUP
       DOUBLE PRECISION   EPS,MU,RO,TGD ,P(50),PAR(1500), YS(1500)
       COMMON/BLKA/    A,KA
       INTEGER*4       A(20,50), KA, KNOT(50)
       

       CHARACTER*4     IT(4), NE, KOH, ITOB, IPAR,  IS, ISTR,           
     +      FNE1,  FNE2, GRAF, YES, NO
       CHARACTER*60    NAME, NNNN
C
C
      COMMON/BLIFF/IFF(7,4),KIFF,NNIFF(4,8),KNNIFF,PNIFF(8),FNE1,FNE2
C
      DATA   IT/4H    ,4H    ,4H    ,4H    /,  IP/6/
      DATA NNNN /'                                                      
     +      '/
      DATA IIN/10/,  KOH/4HEND /,  ITOB/4H    /, IEX/4HEXTR/,           
     +                               IPAR/4H    /
      DATA IS/4H    /, YES/4HYES /, NO/4HNO  /

      NAMELIST/CIRCOM/ EPS,MU,RO,TGD
      NAMELIST/TYP/ IT,KOL,P
      NAMELIST/ELEM/NE,KNOT,IPR,ISTR,IDOP,PAR,IPAR
      NAMELIST/FREQU/ F1,F2,MN,KN
      NAMELIST/SERV/    NAME, GRAF,                EPSIW,LIMERR,KITU, 
     +               EPSSOL,EPSDU,EPSMIN,MAXDU,LIMIT,                KPR
     +LEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,                KPRVAR,KPRQUP,    
     +            MGLOB,IAPR,                KNC
C
C     נPיCBOEHיE HA‏AלרHשX תHA‏EHיך נEPEMEHHשM
C
      LENMPO=500
C             LENMPO - MAKC. הליHA MACCיBA "MPOINT"
      LENNOD=500
C             LENNOD - MAKC. הליHA MACCיBA "NODEEL"
      LENPAR=2000
C             LENPAR - MAKC. הליHA MACCיBA "PARAM"
      NNETPR=1
C             NNETPR - AהPEC HA‏AלA Oג‎יX נAPAM. הלס
C                      BCEך CXEMש B "PARAM"
      LENNTP=4
C             LENNTP - KOל-BO נAPAMETPOB, Oג‎יX הלס
C                      BCEך CXEMש
C
C     HA‏AלרHOE "OגHץלEHיE" MAC. MPOINT,NODEEL,PARAM
      DO 5 II=1,2000
      IF(II.LE.500) MPOINT(II)=0
      IF(II.LE.500) NODEEL(II)=0
      PARAM(II)=0.0D0
   5  CONTINUE
C
C     נPיCBOEHיE HA‏AלרHשX תHA‏EHיך
      NNODE=1
C     NNODE- HOMEP נEPBOחO CBOג. üלEMEHTA B "NODEEL"
      NMPOIN=1
C     NMPOIN- HOMEP נEPBOחO CBOג. üלEMEHTA B "MPOINT"
      NPARAM=1
C     NPARAM- HOMEP נEPBOחO CBOג. üלEMEHTA B "PARAM"
      NMPNT=0
C     NMPNT- KOל-BO,OגPAגATשB. נP-MOך TינOB üל-TOB,
C            T.E. KOל-BO תAנOלHEHHשX CTPOK B "MPOINT"
C
C *** BBOה NAMELIST/SERV/
C            NAME   - מביםומןקבמיו ףטוםש
C            GRAF   - תבהבמיו חעבזי‏וףכןחן עוציםב
C            LIMERR - MAKC. KOלי‏ECTBO OûיגOK
C            EPSIW  - TO‏HOCTר CPABHEHיס ‏ACTOT
C            LIMIT  - MAKC. KOלי‏ECTBO יTEPAדיך
C            KPRLEN - PEציM נE‏ATי נ/נ ‏TEHיס BX.תAהAHיס
C            KPRSRT - PEציM נE‏ATי נ/נ COPTיPOBKי ץתלOB CXEMש
C            KPRNKR - PEציM נE‏ATי נ/נ נOCTPOEHיס CETOK ‏ACTOT
C            KPRLIN - PEציM נE‏ATי נ/נ OגPAגOTKי ליH. נOהCXEMש
C            KPRSOL - PEציM נE‏ATי נ/נ PEûEHיס CיCTEMש HEליH. ץP-ך
C            KPRVAR - PEציM נE‏ATי הלס BAPיAדיי נAPAMETPOB
C            KPRQUP - עוצים נו‏בפי נ/נ ‏פומיס קט. תבהבמיס כב‏. נןכ.
C            EPSSOL - HEOגXOהיMAס TO‏HOCTר PEûEHיס
C            EPSDU  - MיHיMAלרHO BOתMOצHשך ûAח
C            EPSMIN - נPיMEHסETCס הלס OגHAPץצEHיס לOK.MיHיMץMA ,
C                       HE סBל.-Cס PEûEHיEM
C            MAXDU  - OנPEהEלסET MAX. ûA
C            KITU   - נEPEKלא‏ATEלר BיהA HA‏AלרHOחO
C                                      נPיגליצEHיס
C            KNC    - KOלי‏ECTBO TO‏EK PAתלOצEHיס OהHOMEPHOחO הנז
C            MGLOB  - OנPEהEלסET METOה חלOגAליתAדיי
C                     0 - HET חלOגAליתAדיי
C                     1 - ליH. נOיCK B HראTOHOBCKOM HAנPABלEHיי
C            IAPR   - BKלא‏EHיE AנPיOPHOחO METOהA OחPAHי‏EHיס ûAחA
C                     0 - BשKל./ 1 - BKל.
C     תAהAHיE תHA‏EHיך נO YMOל‏AHיא
      LIMERR=3
      EPSIW=1.0D-5
      KITU=0

      KPRSRT=0
      KPRNKR=0
      KPRLIN=0
      KPRSOL=0
      KPRLEN=1
      KPRVAR=1
      KPRQUP=1
      KNC=32

      NAME  = NNNN
      GRAF  = NO

C   ץCTAHOBKי נO ץMOל‏AHיא CM. נ/נ NEINCK
      EPSSOL=1.D-6
      EPSDU=1.0D-6
      EPSMIN=1.0D-7
      MAXDU=0.0D0
      LIMIT=500

      IAPR=0
      MGLOB =0

      TYPI=0.D0
      TYPU=0.D0

C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      READ(IIN,SERV)
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 
      IF(KPRLEN.GT.0) WRITE(IP, 6) 
    6 FORMAT(//5X,'C I R C U I T  DESCRIPTION '/         5X,'-----------
     +---------------'/)
      IF(KPRLEN.GT.0.AND.NAME.NE.NNNN)               WRITE(IP, 7) NAME 
    7 FORMAT(5X,A60//)
C     *         5X,'--------------------------'//)

      KPRGRF=0
      IF(GRAF.EQ.YES) KPRGRF=1

C *** BBOה "NAMELIST/CIRKOM/
C             EPS - היüלEKTPי‏ECKAס נPOHידAEMOCTר נOהלOצKי
C             MU  - MAחHיTHAס נPOHידAEMOCTר MATEPיAלA
C                   נPOBOהHיKOB
C             RO  - ץהEלרHOE COנPOTיBלEHיE MATEPיAלA
C                   נPOBOהHיKOB
C             TGD - TAHחEC ץחלA נOTEPר B היüלEKTPיKE
C                   נOהלOצKי
C
C
C     תAהAHיE תHA‏EHיך נO ץMOל‏AHיא
C
C
      EPS=9.6D0
      MU=1.0D0
      RO=5.7D+07
      TGD=1.D-04
C
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      READ(IIN,CIRCOM)
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C     HA הAHHOM üTAנE יCנOלרתץETCס 4 Oג‎יX נAPAMETPA ( EPS,MU,RO,TGD )
C     הלס BCEך CXEMש,T.E. LENNTP=4. נPי üTOM NNETPR=1.
C
      PARAM(NNETPR)=EPS
      PARAM(NNETPR+1)=TGD
      PARAM(NNETPR+2)=MU
      PARAM(NNETPR+3)=RO
C
C
      IF(KPRLEN.GE.0) WRITE(IP, 8)   EPS,MU,RO,TGD
    8 FORMAT(/2X,'   COMMON PARAMETERS OF CKT :'/        5X,'DIELECTRIC 
     +PERMITTIVITY OF SUBSTRATE    ',E12.5/        5X,'MAGNITE PERMITTIV
     +ITY OF CONDUCTORS            = ',        E12.5/        5X,'SPESIFI
     +C RESISTIVITY OF CONDUCTORS           = ',        E12.5/        5X
     +,'tg(delta) - LOSSES IN DIELECTRIC          = ',E12.5)
C
C *********************************************************
C *** קשתןק נ/נ, ףןהועצב‎וך םבףףיק "A"
      CALL LENA
C *********************************************************
C
C
C
C     ץBEל-E C‏ET‏יKA NPARAM HA KOל-BO Oג‎יX נAPAM-OB
      NPARAM=NPARAM+LENNTP
C
C *** BBOה NAMELIST/TYP/ IT, KOL, P
C          IT - Tינ üלEMEHTA
C          KOL- KOל-BO üלEMEHTOB הAHHOחO TינA
C          P  - Oג‎יE נAPAMETPש TינA (ECלי OHי ECTר)
C
   10 CONTINUE
      KOL=1
      DO 13 IPP=1,50
      P(IPP)=0.D0
      IF(IPP.LE.4) IT(IPP)=ITOB
   13 CONTINUE
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      READ(IIN,TYP)
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
      IF(IT(1).EQ.KOH) GOTO 110
      NMPNT=NMPNT+1
C
C     BשBOה יHזOPMAדיי נO NAMELIST/TYP/
      IF(KPRLEN.GT.0)WRITE(IP, 9) (IT(ITN),ITN=1,4)
    9 FORMAT(/1X,'ELEMENT TYPE - ',4A4)
C
C     OנPEהEלEHיE HOMEPA CTPOKי (IA) B MAC. "A" ,
C     KOTOPAס COOTBETCTBYET OגPAגATשBAEMOMY TינY üל-TA
      DO 11 IA=1,KA
      IF(C2I(IT(1)).EQ.A(1,IA).AND.C2I(IT(2)).EQ.A(2,IA).AND.   C2I(IT(3
     +)).EQ.A(3,IA).AND.C2I(IT(4)).EQ.A(4,IA)) GOTO12
      IF(IA.EQ.KA) GOTO 200
   11 CONTINUE
C
C     הOCTATO‏HO לי MECTA B MACCיBE "MPOINT" הלס
C     תAHECEHיס HOBOחO TינA זü ?
      NNNNN=NMPOIN+20
      IF(NNNNN.GT.LENMPO) GOTO 300
C
C *** תAנOלHEHיE MACCיBA "MPOINT"  ************************************
C
   12 CONTINUE
      NM=NMPOIN-1
C       1-4-HAתBAHיE TינA üלEMEHTA, (HE גOלEE 16-Tי
C           CיMBOלOB; ‏ETשPE נEPBשX סBל-Cס Kלא‏EBשMי)
      MPOINT(NM+1)=A(1,IA)
      MPOINT(NM+2)=A(2,IA)
      MPOINT(NM+3)=A(3,IA)
      MPOINT(NM+4)=A(4,IA)
C       5 - KOל-BO üלEMEHTOB הAHHOחO TינA
      MPOINT(NM+5)=KOL
C       6 - נPיתHAK, OנPEהEלס‎יך PAתHOBיהHOCTר üל-TA
      MPOINT(NM+6)=A(6,IA)
C       7 - הליHA CTPOKי OניCAHיס OהHOחO üל-TA הAHHOחO
C           TינA B "NODEEL"
      MPOINT(NM+7)=A(7,IA)
C       8 - KOל-BO ץתלOB B MATEMATי‏ECKOך MOהEלי
C           (הלס ליH. MHOחOנOלCHיKOB, T.E. A(6,IA)=2,
C           B MPOINT תAHOCיTCס 0, T.K. üTO תHA‏EHיE
C           OנPEהEלסETCס B "NODEEL" הלס KAצהOחO ליH.
C           MHOחOנOלאCHיKA OTהEלרHO)
      IF(A(6,IA).EQ.2) MPOINT(NM+8)=0
      IF(A(6,IA).NE.2) MPOINT(NM+8)=A(8,IA)
C       9 - KOל-BO BHYT-HיX YתלOB, ABTOM-Kי HYMEPYEMשX
C           נPי OניCAHיי YתלOB BKלא‏EHיס üלEMEHTA
      MPOINT(NM+9)=A(9,IA)
C      10 - AהPEC HA‏AלA OניCAHיס üלEMEHTOB הAHHOחO
C           TינA B "NODEEL"
      MPOINT(NM+10)=NNODE
C      11 - KOל-BO נAPAMETPOB, Oג‎יX הלס הAHHOחO
C           TינA üלEMEHTOB
      MPOINT(NM+11)=A(11,IA)
C      12 - AהPEC HA‏AלA CניCKA נAPAMETPOB, Oג‎יX הלס
C           TינA üלEMEHTA, B MACCיBE "PARAM"
      MPOINT(NM+12)=NPARAM
C      13 - KOל-BO נAPAMETPOB, OניCשBAא‎יX MATEMAT.
C           MOהEלר üלEMEHTA (HAKלAה.הOנ.AHAלOח.נOת N8)
      IF(A(6,IA).EQ.2) MPOINT(NM+13)=A(13,IA)
      IF(A(6,IA).NE.2) MPOINT(NM+13)=A(13,IA)
C      14 - AהPEC HA‏AלA CניCKA נAPAM. הלס PAתלי‏HשX
C           BAPיAHTOB MAT. MOהEלEך üלEMEHTOB הAHHOחO
C           TינA B "PARAM"
      MPOINT(NM+14)=A(11,IA)+NPARAM
C      15 - נPיתHAK BAPיAדיי נAPAMETPOB üלEMEHTOB
C           תAנOלHסETCס נPי נOCTYנלEHיי
C           BAPריPYEMOחO üלEMEHTA. HA‏AלרHOE תHA‏EHיE
C           PABHO 0, KOTOPOE XPAHיTCס B MACCיBE "A"
      MPOINT(NM+15)=A(15,IA)
C      16 - AהPEC CניCKA נOלHOחO HAיMEHOBAHיס TינA
C           üל-TA י EחO BשBOהOB (נOKA HE יCנOל-Cס)
      MPOINT(NM+16)=A(16,IA)
C      17 - AהPEC HA‏AלA CניCKA נOלHOחO HAיMEH. נAP-OB
C           üלEMEHTA (נOKA HE יCנOלרתYETCס)
      MPOINT(NM+17)=A(17,IA)
C      18 - MAKC. KOל-BO BX.BEלי‏יH, נPי PAC‏ETE TOKA
C           HEליHEךHOחO üלEMEHTA
C           (הלס OCTAלרHשX TינOB üTOT נAPAM.=0)
      MPOINT(NM+18)=A(18,IA)
C      19 - MAKC. KOל-BO BשXOהHשX TOKOB נPי AHAליתE
C           HEליHEךHOחO üלEMEHTA
C           (הלס OCTAלרHשX TינOB üTOT נAPAMETP =0)
      MPOINT(NM+19)=A(19,IA)
C      20 - הOנOלHיTEלרHשך נPיתHAK (תAPEתEPBיPOBAH)
      MPOINT(NM+20)=A(20,IA)
C
C     KOHTPOלרHAס נE‏ATר CTPOKי "MPOINT"
      NM1=NM+1
      NM20=NM+20
      IF(KPRLEN.GT.2)WRITE(IP, 14)  (MPOINT(NN),NN=NM1,NM20)
   14 FORMAT('  MPOINT= ',4A4,16(1X,I3)/)
C
      NMPOIN=NM+20+1
C
C *** זOPMיPOBAHיE MAC. "PARAM" הלס Oג‎יX נAPAMETPOB פינA ************
C
      KOLC=A(11,IA)
      DO 15 IKOL=1,KOLC
      NKOLC=NPARAM-1+IKOL
      IF(KPRLEN.GE.3) WRITE(IP, 1513) KOLC, IKOL, NPARAM, NKOLC, P(IKOL)
 1513 FORMAT(2X,'KOLC=',I3,' IKOL=',I3,' NPARAM=',I4,' NKOLC=',I4,      
     +    ' P(IKOL)=',E12.5)
      PARAM(NKOLC)=P(IKOL)
      IF(KPRLEN.LE.3) GOTO 15
      WRITE(IP, 1515) NKOLC,PARAM(NKOLC)
 1515 FORMAT(2X,'COMM PARAM(',I4,')=',E12.5)
   15 CONTINUE
C     BשBOה יHזOPMAדיי נO נAPAMETPAM Oג‎יM הלס TינA
      NPKOLC=NPARAM-1+KOLC
      DO 16 N=NPARAM,NPKOLC
      B=0.1D-15
      IF(PARAM(N).GE.B.OR.PARAM(N).LE.-B) GOTO 17
   16 CONTINUE
      GOTO 19
   17 CONTINUE
      IF(KPRLEN.GE.1)WRITE(IP, 18) (PARAM(NKOLC),NKOLC=NPARAM,NPKOLC)
   18 FORMAT(22X,'COMM PARAM=',3(E12.5,', ')/       34X,3(E12.5,', ')/)
   19 CONTINUE
      NPARAM=NPARAM+KOLC
C
C *** OגPAגOTKA BCEX üלEMEHTOB OהHOחO TינA  ***************************
C
C     נPEהBAPיTEלרHO נPOBEPיM KOL , תAהAHHOE BO BXOה. תAהAHיי
      IF(KOL.LT.1) GOTO 370
C
C
      DO 100 IK=1,KOL
C
C     תAהAHיE HA‏AלרHשX תHA‏EHיך הלס üלEMEHTOB
C     OהHOחO TינA
      DO 4  I=1,50
      KNOT(I)=-77777
    4 CONTINUE
      DO 20  I=1,1500
      PAR(I)=0.5D+35
      YS(I) =0.5D+35 
   20 CONTINUE
      IPR=0
      ISTR=IS
      IDOP=0
      IPAR=ITOB
C
C *** BBOה NAMELIST/ELEM/ NE,KNOT,IPR,ISTR,IDOP,PAR,IPAR
C          NE   - CיMBOלרHOE OגOתHA‏EHיE üלEMEHTA
C                 (4 CיMBOלA)
C          KNOT - HOMEPA YתלOB CXEMש
C          IPR  - נPיתHAK BAPיAדיי נAPAMETPOB üל-TA
C          ISTR - Tינ נOלYנPOBOהHיKOBOך CTPYKTYPש
C                 (TOלרKO הלס HEליHEךHשX üל-TOB)
C          IDOP - HOMEP חPYננש נPיHAהלEצHOCTי
C          PAR  - נAPAMETPש, CנEדיזי‏.הלס KOHKP.üל-TA
C          IPAR - OגOתHA‏EHיE üלEMEHTA, Y KOTOPOחO
C                 נAPAMETPש יהEHTי‏Hש נAPAMETPAM
C                 üלEMEHTA NE
C
C
C  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  
      READ(IIN,ELEM)
C  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
      IF(KPRLEN.GE.1)WRITE(IP, 21) NE
   21 FORMAT(6X,'ELEMENT - ',A4,':')
C
C     נPOBEPKA OגOתHA‏EHיך OגPAגATשBAEMX üל-TOB
      DO 22 IL=1,NMPNT
      IF(C2I(NE).EQ.MPOINT(1+(NMPNT-1)*20)) GOTO 240
   22 CONTINUE
      DO 23 I=1,NNODE
      IF(C2I(NE).EQ.NODEEL(I)) GOTO 240
   23 CONTINUE
C
C     OנPEהEלEHיE KOלי‏ECTBA ץתלOB (KOLPOL) 
C
      DO 255 I=1,50
      IF(KNOT(I).GT.-77770) KOLPOL=I
      IF(KOLPOL.EQ.I.AND.KPRLEN.GE.3)     WRITE(IP,24) I, KNOT(I), KOLPO
     +L
  24  FORMAT(2X,' KNOT(',I3,')=',I6,'       KOLPOL=',I3)
 255  CONTINUE
C
C  &&&&  קקןה נבעבםופעןק, תבהבקבוםשט נן 8-םץ כבמבלץ  &&&&&&&&&&
      IF(C2I(IPAR).NE.IEX) GOTO 27
      OPEN(8,FILE='SNTPY',STATUS='OLD')
      READ(8,*,END=259,ERR=259) (YS(I),I=1,27)
  259 CONTINUE
      IF(KPRLEN.GE.3) WRITE(IP,25) (I,YS(I),I=1,27)
   25 FORMAT(2X,'YS(',I4,')=',E12.6)  
      DO 26 I=1,1500
      PAR(I)=YS(I)
   26 CONTINUE
C  &&&&  כןמוד קקןהב  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
C     OנPEהEלEHיE KOלי‏ECTBA נAPAMETPOB (KOLPAR), נOCTץניBûיX HA BXOה
   27 CONTINUE  
      DO 29 I=1,1500
      IF(PAR(I).LT.0.5D+34)             KOLPAR=I
      IF(KOLPAR.EQ.I.AND.KPRLEN.GE.3) WRITE(IP, 28) I,PAR (I),KOLPAR
  28  FORMAT(2X,'  PAR(',I4,')=',E12.5,' KOLPAR=',I4)
  29  CONTINUE
C
C
C     B תABיCיMOCTי OT PAתHOBיהHOCTי üלEMEHTA יCXOהHשE
C     הAHHשE OגPAגATשBAאTCס נO-PAתHOMY
      IF(A(6,IA).EQ.1) GOTO 30
      IF(A(6,IA).EQ.2) GOTO 40
      IF(A(6,IA).EQ.3) GOTO 50
      IF(A(6,IA).EQ.5) GOTO 8000
C
C === זOPMיPOBAHיE CTPOKי "NODEEL" הלס ליH. הBYXנOל-KOB
C
   30 CONTINUE
C       הOCTATO‏HO לי MECTA B MACCיBE "NODEEL" הלס תAניCי
C       HOBOחO üלEMEHTA
      NNNNN=NNODE+5
      IF(NNNNN.GT.LENNOD) GOTO 350
C
C     נPOBEPKA KOלי‏ECTBA ץתלOB BKלא‏EHיס י נAPAMETPOB
      IF(KOLPOL.NE.2) GOTO 320
      IF(KOLPAR.NE.A(13,IA)) GOTO 330
C
      NODEEL(NNODE)=C2I(NE)
      IF(NNNNN.GT.LENNOD) GOTO 350
      NODEEL(NNODE+1)=KNOT(1)
      NODEEL(NNODE+2)=KNOT(2)
C       KNOT(1),KNOT(2) - Yתלש BKלא‏EHיס
      NODEEL(NNODE+3)=IPR
C       IPR - נPיתHAK BAPיAדיי. MOצET OTCYTCTBOBATר
C             B CניCKE "NAMELIST"
      IF(IPR.EQ.1) MPOINT(NM+15)=1
      NODEEL(NNODE+4)=IDOP
C       IDOP - HOMEP חPYננש נPיHAהלEצHOCTי. MOצET
C              OTCYTCTBOBATר B CניCKE "NAMELIST".
C
      IF(KPRLEN.GE.1)WRITE(IP, 33) KNOT(1),KNOT(2)
   33 FORMAT(22X,'Yתלש BKל.=',I2,',',I2)
C
C     KOHTPOלרHAס נE‏ATר CTPOKי "NODEEL"
      IF(KPRLEN.LE.2) GOTO 36
      NNODE4=NNODE+4
      NNODE5=NNODE+5
      WRITE(IP, 34)  NNODE
  34  FORMAT(2X,'NODEEL(',I3,')=')
      WRITE(IP, 35) (NODEEL(N),N=NNODE,NNODE4),NNODE5
  35  FORMAT(15X,A4,4(2X,I4),2X,'NNODE CB.=',I3)
  36  CONTINUE
C
      NNODE=NNODE+5
      GOTO 60
C
C === זOPMיPOBAHיE CTPOKי "NODEEL" הלס ליHEךHשX
C === MHOחOנOלאCHיKOB
C
   40 CONTINUE
C     הOCTATO‏HO לי MECTA B MACCיBE "NODEEL"
      NNNNN=NNODE+8+KOLPOL
      IF(NNNNN.GT.LENNOD) GOTO 350
      NODEEL(NNODE)=C2I(NE)
C       NE - CיMBOלרHOE OגOתHA‏EHיE üלEMEHTA
C     KOלי‏ECTBO נOלאCOB COהEPציTCס B "A(8,IA)"
      IF(KOLPOL.NE.A(8,IA)) GOTO 320
      NODEEL(NNODE+1)=KOLPOL
C     KOלי‏ECTBO נAPAMETPOB, OניCשBAא‎יX üלEMEHT B
C     OהHOך ‏ACTOTHOך TO‏KE, COהEPציTCס B "A(13,IA)"
      IF(KOLPAR.NE.A(13,IA)) GOTO 330
      NODEEL(NNODE+2)=KOLPAR
C     נPיתHAK, OניCשBAא‎יך KלACC üלEMEHTA (AKTיBHשך ילי נACC.)
C     MECTO XPAHEHיס B "A(20,IA)"
      NODEEL(NNODE+3)=A(20,IA)
C     Yתלש BKלא‏EHיס
      DO 41 IKN=1,KOLPOL
      NODEEL(NNODE+3+IKN)=KNOT(IKN)
   41 CONTINUE
      IKN=KOLPOL
C     CיMBOלרHOE OגOתHA‏EHיE HAגOPA נAPAMETPOB
C     (HA הAHHOM üTAנE תAHOCיTCס CיMBOלי‏ECKOE
C     OגOתHA‏EHיE üלEMEHTA, T.E. "NE")
      NODEEL(NNODE+4+IKN)=C2I(NE)
C     AהPEC HAגOPA נAPAMETPOB B "PARAM" Bש‏יCלסETCס
      IADR=MPOINT(NM+14)+(IK-1)*MPOINT(NM+13)
      IADR=NPARAM
      NODEEL(NNODE+5+IKN)=IADR
      NODEEL(NNODE+6+IKN)=IPR
C       IPR -ננPיתHAK BAPיAדיי. MOצET OTCYTCTBOBATר B
C            CניCKE "NAMELIST"
      IF(IPR.EQ.1) MPOINT(NM+15)=1
      NODEEL(NNODE+7+IKN)=IDOP
C       IDOP - HOMEP חPYננש נPיHAהלEצHOCTי. MOצET
C              OTCYTCTBOBATר B CניCKE "NAMELIST".
C
C     נE‏ATר יHזOPMAדיי נO "NAMELIST/ELEM/"
C
      IF(KPRLEN.GE.1)WRITE(IP, 44) (KNOT(JJ),JJ=1,KOLPOL)
   44 FORMAT(22X,'NODES   .=',10(I2,','))
C
C     KOHTPOלרHAס נE‏ATר CTPOKי "NODEEL"
      IF(KPRLEN.LE.2) GOTO 48
      NNODE3=NNODE+3+KOLPOL
      WRITE(IP, 45) NNODE
   45 FORMAT(2X,'NODEEL(',I3,')=')
      WRITE(IP,46) (NODEEL(NN),NN=NNODE,NNODE3)
   46 FORMAT(15X,A4,10(1X,I4))
      NNODE4=NNODE+4+KOLPOL
      NNODE7=NNODE+7+KOLPOL
      NNODE8=NNODE+8+KOLPOL
      WRITE(IP, 47) (NODEEL(NN),NN=NNODE4,NNODE7),NNODE8
   47 FORMAT(17X,A4,3(1X,I4),'  NNODE CB.=',I3)
   48 CONTINUE
C
      NNODE=NNODE+8+KOLPOL
C
      GOTO 60
C
C
C === זOPMיPOBAHיE CTPOKי "NODEEL" הלס üלEMEHTOB , üAהABAEMשX
C === C נOMO‎רא Y- ילי S- MATPיד
C
 8000 CONTINUE
      NNNNN=NNODE+8+KOLPOL
      IF(NNNNN.GT.LENNOD) GOTO 350
      NODEEL(NNODE)=C2I(NE)
C       NE - CיMBOלרHOE OגOתHA‏EHיE üלEMEHTA
C     KOלי‏ECTBO נOלאCOB הלס KAצהOחO üלEMEHTA OנPEהEלסETCס ABTOM.
      NODEEL(NNODE+1)=KOLPOL
C     KOלי‏ECTBO נAPAMETPOB OנPEהEלסETCס הלס KAצהOחO üלEMEHTA
C     ABTOMATי‏ECKי
      NODEEL(NNODE+2)=KOLPAR
C  ?? נPיתHAK , OניCשBAא‎יך Bיה MATPידש (Y ילי S) ??
      NODEEL(NNODE+3)=A(20,IA)
C     ץתלש BKלא‏EHיס
      DO 8010 IKN=1,KOLPOL
      NODEEL(NNODE+3+IKN)=KNOT(IKN)
 8010 CONTINUE
      IKN=KOLPOL
C     CיMBOלי‏ECKOE OגOתHA‏EHיE HAגOPA נAPAMETPOB
C     (HA הAHHOM üTAנE תAHOCיTCס CיMBOלי‏ECKOE
C      OגOתHA‏EHיE üלEMEHTA, T.E. "NE")
      NODEEL(NNODE+4+IKN)=C2I(NE)
C     AהPEC HAגOPA נAPAMETPOB B "PARAM"
      NODEEL(NNODE+5+IKN)=NPARAM
C !!! נPיתHAK BAPיAדיי (HEOגסתATEלרHשך OנEPATOP)
      NODEEL(NNODE+6+IKN)=IPR
C !!! ECלי תAהAH נPיתHAK BAPיAדיי , TO HEOגXOהיMO
C !!! BHECTי הOנOלHEHיס B "MPOINT"
      IF(IPR.EQ.1) MPOINT(NM+15)=1
C !!! HOMEP חPץננש נPיHAהלEצHOCTי (HEOגסתATEלרHשך OנEPATOP)
      NODEEL(NNODE+7+IKN)=IDOP
C
C     נE‏ATר יHזOPMAדיי נO "NAMELIST/ELEM/"
C
      IF(KPRLEN.GE.1)   WRITE(IP, 8044) (KNOT(JJ),JJ=1,KOLPOL)
 8044 FORMAT(22X,'NODES    =',10(I2,','))
C
C     KOHTPOלרHAס נE‏ATר CTPOKי "NODEEL"
      IF(KPRLEN.LE.2) GOTO 8048
      NNODE3=NNODE+3+KOLPOL
      WRITE(IP, 8045) NNODE
 8045 FORMAT(2X,'NODEEL(',I3,')=')
      WRITE(IP, 8046) (NODEEL(NN),NN=NNODE,NNODE3)
 8046 FORMAT(15X,A4,10(1X,I3))
      NNODE4=NNODE+4+KOLPOL
      NNODE7=NNODE+7+KOLPOL
      NNODE8=NNODE+8+KOLPOL
      WRITE(IP, 8047) (NODEEL(NN),NN=NNODE4,NNODE7),NNODE8
 8047 FORMAT(17X,A4,3(1X,I4),'  NNODE CB.=',I3)
 8048 CONTINUE
C
      NNODE=NNODE+8+KOLPOL
C
      GOTO 60
C
C
C
C
C === זOPMיPOBAHיE CTPOKי "NODEEL" הלס HEליHEךHשX
C === üלEMEHTOB
C
   50 CONTINUE
C
C     הOCTATO‏HO לי MECTA B MACCיBE "NODEEL"
      NNNNN=NNODE+7+KOLPOL+A(9,IA)
      IF(NNNNN.GT.LENNOD) GOTO 350
C
      NODEEL(NNODE)=C2I(NE)
C       NE - CיMBOלרHOE OגOתHA‏EHY üלEMEHTA
C     Oג‎EE KOלי‏ECTBO YתלOB COהEPציTCס B "A(8,IA)"
C     KOלי‏ECTBG BHEûHיX YתלOB PABHO PAתHOCTי Oג‎EחO
C     KOלי‏ECTBA YתלOB י KOלי‏ECTBA BHYTPEHHיX YתלOB
      KOLUZ=A(8,IA)-A(9,IA)
C     נPOBEPKA KOלי‏ECTBA BHEûHיX ץתלOB
      IF(KOLPOL.NE.KOLUZ) GOTO 320
C     HOMEPA YתלOB,K KOTOPשM נOהKלא‏AאTCס BHEûHיE
C     üלEKTPOהש üלEMEHTA
      DO 51 IKN=1,KOLUZ
      NODEEL(NNODE+IKN)=KNOT(IKN)
   51 CONTINUE
C     KOל. BHYTPEHHיX YתלOB COהEPציTCס B "A(9,IA)"
      KOLVN=A(9,IA)
C     ECלי KOLVN=0, TO BHץTPEHHיE ץתלש HE PACCMATPיBAאTCס
      IF(KOLVN.EQ.0) GOTO 556
C     HOMEPA BHYTPEHHיX YתלOB
      IKV1=1+KOLUZ
      IKV2=KOLUZ+KOLVN
      DO 56 IKV=IKV1,IKV2
      NODEEL(NNODE+IKV)=100+IKV
      KNOT(IKV)=100+IKV
   56 CONTINUE
  556 IF(KOLVN.EQ.0) IKV=KOLUZ
      IF(KOLVN.NE.0) IKV=IKV2
C     CיMBOלרHOE OגOתHA‏EHיE HAגOPA נAPAMETPOB
C     (HA הAHHOM üTAנE תAHOCיTCס CיMBOלרHOE
C     OגOתHA‏EHיE TינA üל-HTA, T.E."IT(1)","IT(2)")
      NODEEL(NNODE+1+IKV)=C2I(IT(1))
      NODEEL(NNODE+2+IKV)=C2I(IT(2))
C     AהPEC HAגOPA נAPAMETPOB B "PARAM" Bש‏יCלסETCס
      IADR=NPARAM
      NODEEL(NNODE+3+IKV)=IADR
      NODEEL(NNODE+4+IKV)=IPR
C       IPR - נPיתHAK BAPיAדיי.MOצET OTCYTCTBOBATר B
C             CניCKE "NAMELIST"
      IF(IPR.EQ.1) MPOINT(NM+15)=1
C     IF(ISTR.EQ.IS) GOTO 260  *** ץגPAHA תA HEHAהOגHOCTרא  3.11.89 ***
      NODEEL(NNODE+5+IKV)=C2I(ISTR)
C       ISTR - Tינ נOלYנPOBOהHיKOBOך CTPYKTYPש
C              (TOלרKO הלס HEליHEךHשX üל-TOB)
      NODEEL(NNODE+6+IKV)=IDOP
C       IDOP - HOMEP חPYננש נPיHAהלEצHOCTי. MOצET
C              OTCYTCTBOBATר B CניCKE "NAMELIST"
C      נPOBEPKA KOלי‏ECTBA נAPAMETPOB
       IF(KOLPAR.NE.A(13,IA)) GOTO 330
C
C
C     נE‏ATר יHזOPMAדיי נO "NAMELIST/ELEM/"
C
      IF(KPRLEN.GE.1)WRITE(IP, 53) (KNOT(JJ),JJ=1,KOLUZ)
   53 FORMAT(22X,'NODES    =',10(I2,','))
C
      IF(KPRLEN.GT.1.AND.KOLVN.GT.0)WRITE(IP, 52) (KNOT(JJ),JJ=IKV1,IKV2
     +)
   52 FORMAT(22X,'INTERN NODS=',10(I3,',')/       34X,10(I3,','))
C
      IF(KPRLEN.GE.1.AND.ISTR.NE.IS)WRITE(IP, 54) ISTR
   54 FORMAT(22X,'STRUCTURE - ',A4)
C
C     KOHTPOלרHAס נE‏ATר CTPOKי "NODEEL"
      IF(KPRLEN.LE.2) GOTO 55
      NNODEK=NNODE+KOLUZ
      WRITE(IP, 555) NNODE
  555 FORMAT(2X,'NODEEL(',I3,')=')
      WRITE(IP, 57) (NODEEL(NN),NN=NNODE,NNODEK)
   57 FORMAT(15X,A4,10(1X,I4))
      IF(KOLVN.EQ.0) GOTO 558
      NNODEK=NNODE+IKV1
      NNODEL=NNODE+IKV2
      WRITE(IP, 58) (NODEEL(NN),NN=NNODEK,NNODEL)
   58 FORMAT(15X,10(1X,I4))
  558 CONTINUE
      NNODE1=NNODE+1+IKV
      NNODE6=NNODE+6+IKV
      NNODE7=NNODE+7+IKV
      WRITE(IP, 59) (NODEEL(NN),NN=NNODE1,NNODE6),NNODE7
   59 FORMAT(15X,A4,A4,2(1X,I4),1X,A4,1X,I4,        ' NNODE CB.=',I3)
   55 CONTINUE
C
      NNODE=NNODE+7+IKV
C
   60 CONTINUE
C
C     COOג‎EHיE O חPץננE נPיHAהלEצHOCTי
C     (BשBOהיTCס ECלי üלEMEHT - BHEûHיך)
      IF(KPRLEN.GE.1.AND.IDOP.EQ.1) WRITE(IP, 660)
  660 FORMAT(22X,'ELEMENT -EXTERNAL')
C
C === B CלY‏AE OTCYTCTBיס CניCKA נAPAMETPOB י
C === HAלי‏יס CCשלKי HA OהיH ית נPEהשהY‎יX
C === üלEMEHTOB:
      IF(IPAR.EQ.ITOB) GOTO 64
      IF(KPRLEN.GE.3) WRITE(IP,670) IPAR,IEX
  670 FORMAT(2X,'IPAR=',A4,' IEX=',A4)
      IF(C2I(IPAR).EQ.IEX) GOTO 64
C     (הלס ליHEךHשX הBץXנOלאCHיKOB üTOחO הEלATר HEלרתס)
      IF(A(6,IA).EQ.1) GOTO 280
C     (TAK צE KAK י הלס üלEMEHTOB , תAהABAEMשX Y- MATPידEך)
      IF(A(6,IA).EQ.4) GOTO 340
C
C     OנPEהEלסEM AהPEC HA‏AלA OניCAHיס üלEMEHTOB
C     הAHHOחO TינA B "NODEEL"
      NMP=NMPOIN-21
      NADR=MPOINT(NMP+10)
C     OנPEהEלסEM הליHY CTPOKי OניCAHיס OהHOחO
C     üלEMEHTA הAHHOחO TינA
      NDL=A(7,IA)
C     נPOBEPKA HAלי‏יס üלEMEHTA,YKAתAHHOחO
C     B CCשלKE
      DO 61 NN=1,KOL
      NND=NADR+NDL*(NN-1)
      IF(C2I(IPAR).EQ.NODEEL(NND)) GOTO 62
      IF(KPRLEN.GE.3) WRITE(IP,680) NN,KOL
  680 FORMAT(2X,'NN=',I4,' KOL=',I4)
      IF(NN.EQ.KOL) GOTO 220
   61 CONTINUE
   62 CONTINUE
C     OנPEהEלEHיE AהPECA HA‏AלA CניCKA נAPAMETPOB
C     B MACCיBE "PARAM", XPAHס‎EחOCס B MACCיBE
C     "NODEEL", הלס üלEMEHTA YKAתAHHOחO B CCשלKE.
      IF(A(6,IA).EQ.2) NIADR=NND+5+A(8,IA)
      IF(A(6,IA).EQ.3) NIADR=NND+3+A(8,IA)
      IADR=NODEEL(NIADR)
C     תAניCר B MACCיB "NODEEL" HOBOחO תHA‏EHיס
C     AהPECA הלס OגPAגATשBAEMOחO üלEMEHTA.
      NNN=NNODE-NDL
      IF(A(6,IA).EQ.2) NNDA=NNN+5+IKN
      IF(A(6,IA).EQ.3) NNDA=NNN+3+IKV
      NODEEL(NNDA)=IADR
C     PRINT 6666,NND,NIADR,IADR,NNN,NNODE,IKV
C6666 FORMAT(2X,'NND,NIADR,IADR,NNN,NNODE,IKV=',6I4)
      IF(KPRLEN.GE.2) WRITE(IP, 6262) NNDA,IADR
 6262 FORMAT(2X,'NEW VALUE  "NODEEL(',I4,')"=',I4)
      IF(KPRLEN.GE.1)WRITE(IP, 66) IPAR
   66 FORMAT(22X,'PARAMETERS ARE IDENTICAL TO PAR',           'OF ELEM. 
     +',A4)
      GOTO 100
   64 CONTINUE
C
C *** זOPMיPYEM MACCיB "PARAM"  ***************************************
C
C     נPEהBAPיTEל. נPOBEPיB הOCTATO‏HO לי MECTA B "PARAM"

      NNNNN=NPARAM+KOLPAR
      IF(NNNNN.GT.LENPAR) GOTO 360

      F1=0.D0
      F2=0.D0

      DO 75 JF=1,KIFF
      IF(KPRLEN.GE.3) WRITE(IP,69) JF
   69 FORMAT(2X,'   JF=',I3)
      JJF=-1
      IF(IFF(1,JF).EQ.C2I(IT(1)).AND.IFF(2,JF).EQ.C2I(IT(2)).AND.   IFF(
     +3,JF).EQ.C2I(IT(3)).AND.IFF(4,JF).EQ.C2I(IT(4))) GOTO 70
C      IF(IFF(1,JF).EQ.IT(1).AND.IFF(2,JF).EQ.IT(2).AND.
C     *   IFF(3,JF).EQ.IT(3).AND.IFF(4,JF).EQ.IT(4)) JJF=JF
      GO TO 73
  70  CONTINUE
      IFF(6,JF)=JF
      JJF=JF
      IF(KPRLEN.GE.3) WRITE(IP,71) JF,(IFF(JJF1,JF),JJF1=1,7)
  71  FORMAT(2X,'IFF(   ,',I3,')=',4A4,3(1X,I3))

  73  CONTINUE
      DO 85 IKOL=1,KOLPAR
      NI=NPARAM-1+IKOL
      PARAM(NI)=PAR(IKOL)
      IF(JJF.EQ.-1) GOTO 85
      DO 83 KF=1,KIFF
C     IF(KPRLEN.GT.3) WRITE(IP,76) IFF(6,KF),KF,IKOL,IFF(5,KF)
C  76 FORMAT(2X,'IFF(6,KF)=',I3,' KF=',I3,' IKOL=',I3,' IFF(5,KF)=',I3)
      IF(IFF(6,KF).EQ.JJF.AND.IKOL.EQ.IFF(5,KF)) GOTO 77
      GOTO 83
   77 CONTINUE
      KNNIFF=KNNIFF+1
      NNIFF(1,KNNIFF)=C2I(NE)
      NNIFF(2,KNNIFF)=JJF
      NNIFF(3,KNNIFF)=NI
      IF(KPRLEN.GE.3) WRITE(IP,80) KNNIFF, NE, KNNIFF, JJF,             
     +                KNNIFF, NI, KF, NI, PARAM(NI)
   80 FORMAT(2X,'NNIFF=(1',I3,')= NE  = ',A4/       2X,'NNIFF=(2',I3,')=
     + JJF =',I3/       2X,'NNIFF=(3',I3,')= NI  =',I3/       2X,' KF=',
     +I3,' PARAM(',I4,')=',E12.5)

      GOTO 85
   83 CONTINUE
   85 CONTINUE
   75 CONTINUE
C     KOHTPOלרHAס נE‏ATר "PARAM"
      NPARA1=NPARAM-1+KOLPAR
      IF(KPRLEN.GE.1)WRITE(IP, 90) (PARAM(NN),NN=NPARAM,NPARA1)
   90 FORMAT(22X,'PARAMETRS=',4(E11.5,',')/(32X,4(E11.5,',')))
C    *      (32X,4(E11.5,','))/(32X,4(E11.5,','))/(32X,4(E11.5,',))/)
      IF(KPRLEN.GE.3) WRITE(IP, 95) NPARAM,NPARA1
   95 FORMAT(22X,'IN ARRAY  "PARAM" FILLED ',           'POS. SINCE',I4,
     +' TO ',I4)
C
      NPARAM=NPARAM+KOLPAR
      NNNP=NPARAM
  100 CONTINUE
C
C *** KOHEד BBOהA NAMELIST/TYP/ י NAMELIST/ELEM/
C
      GOTO 10
C
C
  110 CONTINUE
C
C
C *********************************************************
C *** קשתןק נ/נ ןגעבגןפכי ‏בףפןפ
      KPR=KPRLEN
      CALL FREQUEN(NF3,KPR)
C *********************************************************
C
      F1OLD=F(1)
      F2OLD=F(2)
      F1   =F(1)
      F2   =F(2)
C
C *** ‏TEHיE NAMELIST/FREQU/ F1,F2,MN,KN
C                     F1-נEPBAס ‏ACTOTA
C                     F2-BTOPAס ‏ACTOTA
C                     MN-KOMגיHAד.COCTABל.CנEKTPA
C                     KN-KOלי‏ECTBO KOMגיHAדיך
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      READ(IIN,FREQU)
C $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C   תAHECEHיE תHA‏EHיך F1 י F2 B MACCיB F
      F(1)=F1
      F(2)=F2
C
C    KOניPOBAHיE MACCיBA MN B MACCיB MNI הלס XPAHEHיס EחO HEיתMEHHשM
      DO 118 J118=1,KN
      MNI(1,J118)=MN(1,J118)
      MNI(2,J118)=MN(2,J118)
  118 CONTINUE
      KMNI=KN
C
C     BשBOה HA נE‏ATר יHזOPMAדיי נO NAMELIST/FRE/

      IF(KPRLEN.GE.1.AND.F1OLD.NE.F1) WRITE(IP, 120) F1
  120 FORMAT(2X,'FUNDAMENTAL FR.  F1 =',E12.5,',')

      IF(KPRLEN.GE.1.AND.F1OLD.EQ.F1) WRITE(IP, 121) F1, FNE1
  121 FORMAT(2X,'FUNDAMENTAL FR.  F1 =',E12.5,' ( ELEMENT ',A4,' )')

      IF(KPRLEN.GE.1.AND.F2OLD.NE.F2) WRITE(IP, 122) F2
  122 FORMAT(2X,'FUNDAMENTAL FR.  F2 =',E12.5,',')

      IF(KPRLEN.GE.1.AND.F2OLD.EQ.F2.AND.FNE2.NE.IS)                    
     +             WRITE(IP, 123) F2, FNE2
  123 FORMAT(2X,'FUNDAMENTAL FR.  F2 =',E12.5,' ( ELEMENT ',A4,' )')

      IF(KPRLEN.GE.1.AND.F2OLD.EQ.F2.AND.FNE2.EQ.IS)                    
     +            WRITE(IP, 122) F2
      IF(KPRLEN.GE.1.AND.NF3.EQ.1)    WRITE(IP, 124)
  124 FORMAT(//2X,'A T T E N T  O N: MORE THAN 2 FREQUENCIES  !'/       
     + /2X,' MAKE CHANGES IN INPUT TASK !')
     
      IF(NF3.EQ.1) STOP

      IF(KPRLEN.GE.1) WRITE(IP, 125) 
  125 FORMAT(2X,'FREQUENCY GRID   ( F = mi*F1 + ni*F2 ) ')
      DO 127 IZ=1,KN
      FS=MN(1,IZ)*F(1)+MN(2,IZ)*F(2)
      IF(KPRLEN.GE.1) WRITE(IP, 126) IZ,MN(1,IZ), IZ,MN(2,IZ), FS
  126 FORMAT(12X,'m',I2,' =',I3,',   n',I2,' =',I3,'   F = ',E12.6)
  127 CONTINUE

C     עבףףםןפעים ףלץ‏בך הקץט מץלוקשט ‏בףפןפ
      IF(F(1).NE.0.D0.OR.F(2).NE.0.D0) GOTO 129
      MN(1,1) = 0
      MN(2,1) = 0
      MNI(1,1)= 0
      MNI(2,1)= 0
      KN=1
      IF(KPRLEN.GE.1) WRITE(IP,128) 
  128 FORMAT(/2X,'ALL FREQUENCIES ARE = TO ZERO '/         2X,'ONLY DC A
     +NALYSIS !'/)
  129 CONTINUE
C
C     KOHEד BBOהA NAMELIST/FRE/
C
C
C *** BשBOה HA נE‏ATר יHזOPMAדיי נO NAMELIST/SERV/
      IF(KPRLEN.GE.1)WRITE(IP, 130) EPSIW,LIMIT,               KPRLEN,KP
     +RSRT,KPRNKR,KPRLIN,               KPRSOL,KPRVAR,KPRGRF,KPRQUP,    
     +           EPSSOL,KITU,KNC
  130 FORMAT(2X,'SERVICE VALUES  :',20X,'FREQUENCIES CMP PRECISION = ',E
     +12.5/20X,'MAX NUMBER OF ITERATIONS  = ',I3/20X,'PRINTINGS     = ',
     +8(I2,',')/20X,'REQUIRED PREC. OF SOLUTION = ',E12.5/20X,'KIND OF I
     +NITIAL APPROXIMATION = ',I2/20X,'NUMBER OF POINTS IN FFT = ',I2/)


C ********** כןמפעןלרמבס נו‏בפר םבףףיקןק ******************

      IF(KPRLEN.LE.1) GOTO 677

      WRITE(IP,*) 'PRINTS AT OUT OF SUBR LEN'

      WRITE(IP, 610) EPSIW,LIMIT,               KPRLEN,KPRSRT,KPRNKR,KPR
     +LIN,               KPRSOL,KPRVAR,KPRQUP,               EPSSOL,EPSD
     +U,EPSMIN,MAXDU,               KITU,KNC,MGLOB,IAPR
  610 FORMAT(10X,' CONTROL VALUES :',/2X,'EPSIW (1.0E-5) - FREQUENCIES C
     +MP PRECISION = ',E12.5/2X,'LIMIT (500) - MAX NUM OF ITERATIONS = '
     +,I3/2X,'KPRLEN (1) - PRN LEVEL OF INPUT TASK READING = ',I2/2X,'KP
     +RSRT (0) - PRN LEVEL Tי נ/נ COPTיPOBKי ץתלOB CXEMש = ',I2/2X,'KPRN
     +KR (0) - PRN LEVEL Tי נ/נ נOCTPOEHיס CETOK ‏ACTOT = ',I2/2X,'KPRLI
     +N (0) - PRN LEVEL Tי נ/נ OגPAגOTKי ליH. נOהCXEMש = ',I2/2X,'KPRSOL
     + (0) - PRN LEVEL Tי נ/נ PEûEH. CיC-Mש HEל. ץP-ך = ',I2/2X,'KPRVAR 
     +(1) - PRN LEVEL Tי הלס BAPיAדיי נAPAMETPOB = ',I2/2X,'KPRQUP (0) -
     + PRN LEVEL פי נ/נ ‏פומיס קט.תבהבמיס כ.נ. = ',I2/2X,'EPSSOL (1.E-6)
     + - REQUIRED PREC. OF SOLUTION   = ',E12.5/2X,'EPSDU (1.E-6) - MINI
     +MUM ALLOWED STEP = ',E12.5/2X,'EPSMIN (1.E-7) - USED TO DETECT LOC
     +AL MINIMUM ',/2X,'                 WHICH NOT A SOLUTION = ',E12.5/
     +2X,'MAXDU (0.0) - DEF. MAX ALLOWED STEP=',E12.5/2X,'KITU (0) - KIN
     +D OF INITIAL APPROXIMATION = ',I2/2X,'KNC (32) - NUMBER OF POINTS 
     +IN FFT = ',I2/2X,'MGLOB (0) - GLOBALIZATION APPROACH = ',I2/2X,'  
     +          0 - NO GLOBALIZATION',/2X,'            1 - LINEAR SEARCH
     + IN NEWTON DIRECTION',/2X,'IAPR (0) - TURN ON A-PRIORY STEP LIMITA
     +TION  = ',I2/2X,'            0 - OFF 1 - ON   ',/)
C
C *** KOHTPOלרHAס נE‏ATר MACCיBA "MPOINT"
      WRITE(IP,*) 'MPOINT = '
      DO 612 JJ=1,NMPNT
      N1=20*(JJ-1)+1
      N20=N1+20-1
      WRITE(IP, 611) N1,N20,(MPOINT(N),N=N1,N20)
  611 FORMAT(2X,'(',I3,',',I3,')=',4A4,16(I3))
  612 CONTINUE
C
C *** כןמפעןלרמבס נו‏בפר םבףףיקב "NODEEL"
      NNOD=NNODE-1
      DO 630 JJ=1,NNOD
      WRITE(IP, 620)  JJ, NODEEL(JJ), NODEEL(JJ)
  620 FORMAT(2X,'NODEEL(',I3,')=',I4,2X,A4)
  630 CONTINUE
      WRITE(IP, 633) NNODE
  633 FORMAT(/2X,'FIRST FREE CELL IN  NODEEL - ',I4/)
 
C *** KOHTPOלרHAס נE‏ATר MACCיBA "PARAM"
      NNP=NPARAM-1
      WRITE(IP, 635) (NN,PARAM(NN),NN=1,NNP)
  635 FORMAT(2X,'PARAM(',I4,')=',E12.5)
      WRITE(IP, 637) NPARAM
  637 FORMAT(/2X,'FIRST FREE CELL IN PARAM - ',I4/)
C
C *** כןמפעןלרמבס נו‏בפר םבףףיקב "IFF"
      DO 650 JF=1,KIFF
      WRITE(IP, 640) JF,(IFF(JJF1,JF),JJF1=1,7)
 640  FORMAT(2X,'IFF(   ,',I3,')=',4A4,3(1X,I3))
 650  CONTINUE
C
C *** כןמפעןלרמבס נו‏בפר םבףףיקב "NNIFF"
      DO 675 IJ=1,KNNIFF
      WRITE(IP, 665) IJ,(NNIFF(I,IJ),I=1,4)
  665 FORMAT(2X,'NNIFF( ,',I2,')=',A4,', ',I2,', ',I4,', ',I3)
  675 CONTINUE
   
C *** KOHTPOלרHAס נE‏ATר נEPEMEHHשX
      WRITE(IP, 676) NMPNT,NNODE,NPARAM,LENNOD,          LENPAR,NNETPR,L
     +ENNTP
  676 FORMAT(2X,'NMPNT=',I2,' NNODE=',I3,       1X,'NPARAM=',I4,' LENNOD
     +=',I3/       2X,'LENPAR=',I4,' NNETPR=',I3,       1X,'LENNTP=',I3)
C

  677 CONTINUE

C *** KOHEד BBOהA היPEKTיB *******************************************
C
C
      RETURN
C
C
C *** היAחHOCTיKA OûיגOK  *********************************************
  140 FORMAT(/1X,78('-')//)
C
  200 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 210) IT
  210 FORMAT(2X,'TYPE"',4A4,'" IS ABSENT IN  ',          'LIBRARY OF ELE
     +MENTS')
      GOTO 500
C
  220 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 230) IPAR
  230 FORMAT(2X,'ELEMENT "',A4,'" IS ABSENT IN CKT')
      GOTO 500
C
  240 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 250) NE
  250 FORMAT(2X,'NAME OF ELEMEN (NE="',A4,'") '/       2X,'SHOULD NOT BE
     + EQUAL TO NAME OF '/       2X,'TYPE OR NAME OF OTHER ELEMENT ')
      GOTO 500
C
  260 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 270) NE
  270 FORMAT(2X,'FOR ELEMENT ',A4,' IS NOT DEF.'/       2X,'SEMICONDUCTO
     +R SRUCTURE ')
      GOTO 500
C
  280 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 290)
  290 FORMAT(2X,'LINEAR 2-POLES DO NOT ALLOW IPAR VALUE IN  &ELEM')
      GOTO 500
C
  300 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 305)
  305 FORMAT(2X,'NO MORE ROOM FOR INPUT')
c HEOגXOהיMO ץBEלי‏יTר PAתMEPש MACCיBA "MPOINT" י נPיCBOיTר HOBOE תHA‏EH
c     יE 
c נEPEMEHHOך  LENMPO CM. נ/נ LEN 
      GOTO 500
C
  320 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 325) NE, KOLPOL
  325 FORMAT(2X,'WRONG NODES NUMBER FOR ELEMENT ',A4,'(',I3,')')
      GOTO 500
C
  330 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 335) NE, KOLPAR
  335 FORMAT(2X,'WRONG PARAMETERS NUMBER FOR ELEMENT ',A4,'(',I4,')')
      GOTO 500
C
  340 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 345)
  345 FORMAT( 2X,'CANT USE IPAR IN &ELEM FOR TABLE DEFINED COMPONENT')
      GOTO 500
C
  350 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 355)
  355 FORMAT(2X,'NO MORE ROOM IN NODEEL')
C HEOגXOהיMO ץBEלי‏יTר PAתMEPש MACCיBA NODEEL י נPיCBOיTר HOBOE תHA‏EHיE
c      
C נEPEMEHHOך LENNOD , CM נ/נ LEN
      GOTO 500
C
  360 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 365)
  365 FORMAT(2X,'NO MORE ROOM IN PARAM')
C HEOגXOהיMO ץBEלי‏יTר PAתMEPש MACCיBA "PARAM" י נPיCBOיTר HOBOE תHA‏EHי
c     E 
C נEPEMEHHOך LENPAR
      GOTO 500
C
  370 CONTINUE
      WRITE(IP, 140)
      WRITE(IP, 375) KOL
  375 FORMAT(2X,'WRONG NUMBER OF ELEMENTS (KOL=',I4,')'/       2X,'IN &T
     +YP')
      GOTO 500
C
C
C
  500 CONTINUE
      WRITE(IP, 140)
C
      STOP
C
      END
