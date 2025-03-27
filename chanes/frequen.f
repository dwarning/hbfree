c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




        SUBROUTINE FREQUEN(NF3,KPR)
C ***
C *** πPOηPAMMA οβςAβοτλι ώAστοτ
C ***
C       δατα ποσμεδξεηο ςεδαλτιςοχωαξιρ  -  28.04.92
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'charint.i'
      include 'circuit.i'
C       COMMON/NODEL/   NODEEL(500)
C       COMMON/POINT/   MPOINT(500)
C       COMMON/PARAMS/  PARAM (2000)
C       COMMON/POINTR/  NMPNT,NNODE,NPARAM,LENNOD,
C     +                 LENPAR,NNETPR,LENNTP

       COMMON/FRE/        F(2)
       COMMON/BLK1/    KR,KR1,KC,KC1,NNR,NNR1,MN,MN1
       INTEGER         KR(20),KR1(200),KC(10),KC1(20)
       INTEGER         NNR(10),NNR1(20),MN(2,20),MN1(2,200)
       COMMON/BLK2/    KNC,KNR,KN,KNR1,KN1
       COMMON/BLMNI/   MNI(2,20),KMNI
       COMMON/PRINT /  KPRLEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,KPRVAR,       
     +          KPRGRF,KPRQUP
       CHARACTER*4     IS, FNE1,  FNE2
C
       COMMON/BLIFF/IFF(7,4),KIFF,NNIFF(4,8),KNNIFF,PNIFF(8),FNE1,FNE2
       INTEGER IFFRON
C
       DATA  IP  /6/,  KNPRI/4/
       DATA IIN /10/,  IS/'    '/
       
C
C *** initialization of variables
      IFFRON=0


C *** ϊAδAξιε ξαώαμψξωθ σοστορξικ
       F1  = 0.D0
       F2  = 0.D0
      NF1  = 0
      NF2  = 0
      NF3  = 0
      FNE1 = IS
      FNE2 = IS
      INN1 = 0
      INN2 = 0

      DO 3001 IK=1,KNNIFF
      NNIFF(4,IK)=0
 3001 CONTINUE

C  χ νασσιχ PNIFF ϊαξοσιν ϊξαώεξιρ ώαστοτ, ϊαπισαξξωθ χ νασσιχε
C  PARAM 

      DO 3005 IC=1,KNNIFF
      PNIFF(IC) = PARAM(NNIFF(3,IC))
      IF(KPR.GE.2) WRITE(6,3004) IC,PNIFF(IC),NNIFF(3,IC)
 3004 FORMAT(2X,'FREQUEN : PNIFF(',I2,')=',E12.6,' (PARAM(',I4,'))')
 3005 CONTINUE

C  NPRI - ξονες πςιοςιτετα ώαστοτω, σοοτχετστχυΰύικ IFF(3,N) ι
C         NNIFF(2,K) 
      NPRI = 1
 
C  IFFRP - τελυύικ ξονες ώαστοτω (πομοφιτεμψξοκ)
C  IFFRO - τελυύικ ξονες ώαστοτω (ξυμεχοκ)
      IFFRP = 0
      IFFRO = 0
 3015 CONTINUE
      DO 3030 IC=1,KNNIFF
C  οπςεδεμρεν όμενεξτ σ ξαιχωσϋιν πςιοςιτετον ι δμρ ξεηο, 
C  χ ϊαχισινοστι οτ ϊξαώεξιρ ώαστοτω, ϊαξοσιν τελυύεε ϊξαώεξιε
C  ώαστοτω
      IF(NNIFF(2,IC).NE.NPRI) GOTO 3030
      IF(PNIFF(IC).EQ.0.D0.AND.NNIFF(4,IC).EQ.0) IFFRO=IFFRO-1   
      IF(PNIFF(IC).EQ.0.D0.AND.NNIFF(4,IC).EQ.0) NNIFF(4,IC)=IFFRO   
      IF(PNIFF(IC).NE.0.D0.AND.NNIFF(4,IC).EQ.0) IFFRP=IFFRP+1
      IF(PNIFF(IC).NE.0.D0.AND.NNIFF(4,IC).EQ.0) NNIFF(4,IC)=IFFRP
      IF(KPR.GE.3) WRITE(6,3017) NPRI,IC,NNIFF(4,IC)
 3017 FORMAT(2X,'NPRI=',I2,', NNIFF(4,',I2,')=',I4) 
      
C  πςοσνατςιχαεν οσταμψξωε όμενεξτω. εσμι χστςεώαετσρ όμενεξτ
C  σ ταλιν φε ϊξαώεξιν ώαστοτω , το ότονυ όμενεξτυ πςισχαιχετσρ
C  το φε τελυύεε ϊξαώεξιε ώαστοτω.
      DO 3020 IRR=1,KNNIFF
      IF(IC.EQ.IRR) GOTO 3020
      IF(NNIFF(4,IRR).NE.0) GOTO 3020 
      IF(PNIFF(IC).EQ.PNIFF(IRR)) NNIFF(4,IRR)=NNIFF(4,IC)
      IF(KPR.GE.3) WRITE(6,3019) IRR, IC, NNIFF(4,IRR)
 3019 FORMAT(2X,'IRR=',I2,', IC=',I2,', NNIFF(4,IRR)=',I3)
 3020 CONTINUE
 3030 CONTINUE
      NPRI = NPRI + 1
      IF(NPRI.LE.KNPRI) GOTO 3015

      DO 3050 IC=1,KNNIFF
      IF(KPR.GE.3) WRITE(6,3040) IC,(NNIFF(NG,IC),NG=1,4)
 3040 FORMAT(2X,'NNIFF( ,',I3,')=',A4,',',I3,',',I3,',',I3)
 3050 CONTINUE

C  χωβος 1 - οκ ώAστοτω      
      NPRI=1
 3055 CONTINUE
      DO 3060 IC=1,KNNIFF
      IF(NNIFF(2,IC).NE.NPRI) GOTO 3060
      IF(NNIFF(4,IC).EQ.1) GOTO 3070
 3060 CONTINUE
      NPRI=NPRI+1
      IF(NPRI.LE.KNPRI) GOTO 3055
      GOTO 3075
 3070 CONTINUE
      F1   = PNIFF(IC)
      FNE1 = I2C(NNIFF(1,IC))
      INN1 = NNIFF(3,IC)
      NF1  = 1

      IF(KPR.GE.3) WRITE(IP,3073) F1, NF1, INN1, FNE1
 3073 FORMAT(2X,'F1=',E12.5,' NF1=',I3,' INN1=',I4,' FNE1=',A4)
 
C  χωβος 2 - οκ ώAστοτω
 3075 CONTINUE
      NPRI=1
 3077 CONTINUE
      DO 3080 IC=1,KNNIFF
      IF(NNIFF(2,IC).NE.NPRI) GOTO 3080
      IF(NNIFF(4,IC).EQ.2) GOTO 3090
 3080 CONTINUE
      NPRI=NPRI+1
      IF(NPRI.LE.KNPRI) GOTO 3077
      GOTO 3105
 3090 CONTINUE
      F2   = PNIFF(IC)
      FNE2 = I2C(NNIFF(1,IC))
      INN2 = NNIFF(3,IC)
      NF2  = 1

      IF(KPR.GE.3) WRITE(IP,3100) F2, NF2, INN2, FNE2
 3100 FORMAT(2X,'F2=',E12.5,' NF2=',I3,' INN2=',I4,' FNE2=',A4)

      IF(NF1.EQ.1.AND.NF2.EQ.1.AND.F1.NE.0.AND.F2.NE.0.D0)  GOTO 3123

C  οβςαβοτλα ξυμεχοκ ώαστοτω
 3105 CONTINUE
      NPRI=1
 3107 CONTINUE
      IFFRO=-IFFRO
      DO 3110 IFFR=1,IFFRO
      DO 3110 IC=1,KNNIFF
      IF(NNIFF(2,IC).NE.NPRI) GOTO 3110
      IF(NNIFF(4,IC).EQ.-IFFR) GOTO 3120
 3110 CONTINUE
      NPRI=NPRI+1
      IF(NPRI.LE.KNPRI) GOTO 3107
      GOTO 3140
 3120 CONTINUE
      F2   = PNIFF(IC)
      FNE2 = I2C(NNIFF(1,IC))
      INN2 = NNIFF(3,IC)
      NF2  = 1
      NNIFF(4,IC) = 2
      IFFRON=-IFFR
      IF(KPR.GE.3) WRITE(IP,3100) F2, NF2, INN2, FNE2

C  πςοχεςρεν - εστψ μι εύ³ ξυμεχωε ώαστοτω ?
 3123 CONTINUE
      DO 3130 IC=1,KNNIFF
      IF(NNIFF(4,IC).LT.0.AND.IFFRP.LT.2) NNIFF(4,IC)=2
      IF(NNIFF(4,IC).LT.0.AND.IFFRP.GE.2) NNIFF(4,IC)=IFFRON
      IF(KPR.GE.3) WRITE(6,3125) IC, NNIFF(4,IC)
 3125 FORMAT(2X,'NNIFF(4,',I2,')=',I3)
 3130 CONTINUE

C  πςοχεςρεν - εστψ μι εύ³ ώαστοτω, λςονε F1 ι F2 ?  
 3140 CONTINUE
      DO 3150 IC=1,KNNIFF
      IF(NNIFF(4,IC).GT.2) NF3=1
 3150 CONTINUE
C
      DO 3170 IC=1,KNNIFF
      IF(KPR.GE.3) WRITE(6,3160) IC,(NNIFF(NG,IC),NG=1,4)
 3160 FORMAT(2X,'FREQUEN (OUT) : NNIFF( ,',I3,')=',                     
     +      A4,',',I3,',',I3,',',I3)
 3170 CONTINUE
C
      DO 3190 IC=1,KNNIFF
      IF(KPR.GE.3) WRITE(6,3180) IC, PNIFF(IC)
 3180 FORMAT(2X,'FREQUEN (OUT) : PNIFF(',I2,')=',E12.6)
 3190 CONTINUE
C  
      F1OLD=F1
      F2OLD=F2

      F(1)=F1
      F(2)=F2
C
C

      IF(KPR.GE.2) WRITE(IP, 4000) F1, FNE1
 4000 FORMAT(2X,'FREQUEN: FREQUENCY VARIATION'/       2X,'FREQNCY 1 =',E
     +12.5,' ( ELEMENT ',A4,' )')


      IF(KPR.GE.2) WRITE(IP, 4010) F2, FNE2
 4010 FORMAT(2X,'FREQNCY 2 =',E12.5,' ( ELEMENT ',A4,' )')

      IF(KPR.GE.2.AND.NF3.EQ.1)    WRITE(IP, 4020)
 4020 FORMAT(2X,'ATTENTION: MORE THAN 2 FREQ. DEFINED!')

      RETURN
C
      END
