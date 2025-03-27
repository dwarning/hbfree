c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c





      SUBROUTINE SORTUZ

C     BAP�AHT �E� BK�. �PAH��H�X ���OB B C�CT. �P-�.
C              CT�K�ETC� C TEK���M� TEKCTAM�
C                 OCT. MO���E�.
C
C

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'
      LOGICAL         NAL(4)
      INTEGER*4       NLDIM(100),LCDIM(100),LVDIM(100),KOL(4)
      INTEGER*4       NOUZ(100),INOUZ1(100)
      INTEGER*4       KMOD,KSCH,IVAR,LUK
      COMMON/KOLNAL/  KOL,NAL
      COMMON/KOLUZ/   KMOD,KSCH
      COMMON/MDLA/    MT(15)

C      COMMON/POINT/   MPOINT(1)
C      COMMON/NODEL/   NODEEL(1)
C      COMMON/POINTR/  NMPNT,NNODE,NPARAM,LENNOD,LENPAR,NNETPR,LENNTP

      COMMON/NOUZL/   NOUZ,INOUZ1
      COMMON/PRINT/   KPRLEN,KPRSRT,KPRNKR,KPRLIN,                KPRSOL
     +,KPRVAR,KPRGRF,KPRQUP

      DIMENSION NAME(4)


C     HA�.  �CTAHOBK�     **********************************************

      KOL(1)=0
      KOL(2)=0
      KOL(3)=0
      KOL(4)=0
      NAL(1)=.FALSE.
      NAL(2)=.FALSE.
      NAL(3)=.FALSE.
      NAL(4)=.FALSE.
      LUK0=100
      LUK=LUK0
      DO 02 J=1,LUK0
      NLDIM(J)=0
      LVDIM(J)=0
      LCDIM(J)=0
      NOUZ(J)=0
   02 INOUZ1(J)=0

C     DO  �O BCEM T��AM ��-TOB          *****     *****     *****

      IE10=NMPNT*20
      DO 01 I10=1,IE10,20
      IF (MPOINT(I10+4).EQ.0) GO TO 01
      NFIRST=MPOINT(I10+9)
      NLEN=MPOINT(I10+6)
      NP=NFIRST

      NAME(1)=MPOINT(I10)
      NAME(2)=MPOINT(I10+1)
      NAME(3)=MPOINT(I10+2)
      NAME(4)=MPOINT(I10+3)
      ITYP=MPOINT(I10+5)
C     DO    �O ��-TAM BH�TP� T��A       *****     *****     *****
      IE11=MPOINT(I10+4)
      DO 17 I11=1,IE11

C     A�PEC  ��-TA B  NODEEL            *****     *****     *****
      NATADR=NFIRST+(I11-1)*NLEN

C     IF  ��-T  HE��HE�H��:             *****     *****     *****
      IF(ITYP.EQ.3) GO TO 12

C     IF   ��HE�H�� MHO�O�O��CH�K       *****     *****     *****
      IF(ITYP.EQ.2) GO TO 11


C     IF   Y - MATP��A                  *****     *****     *****
      IF(ITYP.EQ.5) GO TO 13

C     IF  ��HE�H�� �B�X�O��CH�K         *****     *****     *****
      IVAR=NODEEL(NATADR+3)
      ITYPE=IVAR
      NB=2-1
      NF=3-1
      GO TO 14

C     ��HE�H��  MHO�O�O��CH�K:          *****     *****     *****

   11 IDE=NODEEL(NATADR+1)
      IVAR=NODEEL(NATADR+IDE+6)
      ITYPE=IVAR
      NB=5-1
      NF=NODEEL(NATADR+1)+4-1
      GO TO 14

C     Y - MATP��A                       *****     *****     ******

   13 IDE=NODEEL(NP+1)
      IVAR=NODEEL(NP+IDE+6)
      ITYPE=IVAR
      NB=5-1
      NF=NODEEL(NP+1)+4-1
      NATADR=NP
      NP=NP+8+IDE
      GO TO 14

C     HE��HE�H��  MHO�O�O��CH�K         *****     *****     *****
   12 IDE=MPOINT(I10+7)
      IVAR=NODEEL(NATADR+IDE+3+1)
      CALL LIBMD1(NAME,IVAR)
      NB=2-1
      NF=MPOINT(I10+7)+1-1

C     �CTAHOBKA A�PECOB 1-�O � �OC�E�HE�O ���OB B �AB�C�MOCT� OT ITYP
C     �POBE�EHA B��E
C     DO �O ���AM B ��-TE:              *****     *****     *****
   14 DO 10 I12=NB,NF,1
C     HOMEP ���A                        *****     *****     *****
      INUZ=NODEEL(NATADR+I12)
      IF (INUZ.EQ.0) GO TO 10
      IF (ITYP.NE.3) GO TO 35

C     O�PE�E��M T�� ���A                *****     *****     *****

      ITYPE=MT(I12)

C     BHE�H�E ���� HE H�MEP�EM          *****     *****     *****

      ITIME3=NF-MPOINT(I10+8)
      IF (I12.LE.ITIME3) GO TO 35

C     BH�TPEHH�E - H�MEP�EM             *****     *****     *****

      LUK=LUK+1
      INUZ=LUK
      NODEEL(NATADR+I12)=LUK

C     O�O�HAEM T�� ���A � O�PE�E��M ECT� �� �TOT ��E� B
C     O�HOM �� �A�O�H�EM�X MACC�BOB.
C     EC�� HET - BHECEM
  35  IF(ITYPE.EQ.0)  CALL FIND(KOL(1),LCDIM,INUZ)
      IF(ITYPE.EQ.0)  GO TO 10
      IF(ITYPE.EQ.1)  CALL FIND(KOL(2),LVDIM,INUZ)
      IF(ITYPE.EQ.1)  GO TO 10
      IF(ITYPE.EQ.2)  CALL FIND(KOL(3),NLDIM,INUZ)
      IF(ITYPE.EQ.2)  GO TO 10
C
   10 CONTINUE
   17 CONTINUE
   01 CONTINUE
C     �H��TO��M (�P�CBO�M 0) ���� B LCDIM � LVDIM EC�� OH� ECT� B NLDIM
C     � B LCDIM EC�� ECT� B LVDIM
C
      CALL DOWN(KOL(3),NLDIM,KOL(2),LVDIM)
      CALL DOWN(KOL(3),NLDIM,KOL(1),LCDIM)
      CALL DOWN(KOL(2),LVDIM,KOL(1),LCDIM)
C
C     �EPE���EM �OO�EPE�HO �� LCDIM,LVDIM,NLDIM B INOUZ HA��HA� C
C     MIN ��EMEHTOB MACC�BOB
C
C     KSCN=KO�-BO ���OB B CXEME
C
      KSCH=0
      CALL RANGER(KSCH,NOUZ,LCDIM,KOL(1),NAL(1))
      CALL RANGER(KSCH,NOUZ,LVDIM,KOL(2),NAL(2))
      CALL RANGER(KSCH,NOUZ,NLDIM,KOL(3),NAL(3))
      KMOD=LUK-LUK0
      KSCH=KSCH-KMOD
C  C�B�� ���OB HA LUK0-KSCH B�EBO
      IDEL=LUK0-KSCH
      DO 40 I40=1,LUK0
      IZNN=NOUZ(I40)
   40 IF(IZNN.GT.LUK0)NOUZ(I40)=IZNN-IDEL
C �PEO�PA�OBAH�E H�MEPA���
      DO 70 NU=1,LUK0
      I20END=KSCH+KMOD
      DO 75 I20=1,I20END
      ITERM2=NOUZ(I20)
      IF (ITERM2.EQ.NU) GO TO 80
   75 CONTINUE
      GO TO 70
   80 INOUZ1(NU)=I20
   70 CONTINUE
C NOUZ(I)=NUM POLZ(BH.H.1)
C INOUZ1(I)=BH.H.1(H�M.�O���)
C
C KMOD=KO�-BO ���OB B MO�.
C KSCH=KO�-BO ���OB B CXEME
C
C     �EPEKO��POBKA TO�O�O���ECKO� �H�OPMA���
C     ��K� �O T��AM ��EMEHTOB
      I00END=NMPNT*20
      DO 87 I00=1,I00END,20
      NFIRST=MPOINT(I00+9)
      NLEN=MPOINT(I00+6)
      ITYP=MPOINT(I00+5)
      I01END=MPOINT(I00+4)
      NP=NFIRST

      DO 85 I01=1,I01END
      NATADR=NFIRST+(I01-1)*NLEN

      GO TO (90,95,100,96,97),ITYP
   90 NB=1
      NF=2
      GO TO 105
   95 NB=4
      NF=NODEEL(NATADR+1)+3
      GO TO 105
   96 CONTINUE
      GO TO 105
   97 NB=4
      NF=NODEEL(NP+1)+3
      NATADR=NP
      NP=NP+8+NODEEL(NP+1)
      GO TO 105

  100 NB=1
      NF=MPOINT(I00+7)
  105 DO 85 I02=NB,NF
      ITIME=NODEEL(NATADR+I02)
      IF(ITIME.EQ.0) GO TO 85
      IF(ITIME.GT.LUK0) ITIME=ITIME-IDEL
      NODEEL(NATADR+I02)=INOUZ1(ITIME)
      NAAA=NATADR+I02
C     WRITE(6,900) NATADR,I02,NODEEL(NAAA)
C 900 FORMAT(2X,'SORTUZ : NODEEL(',I3,'+',I3,')=',I5)
   85 CONTINUE
   87 CONTINUE
      KLL=KOL(1)+KOL(2)+KOL(3)
      IF(KPRSRT.LT.2) GOTO 900
      WRITE(6, 800)
      WRITE(6, 810) (IVA,NOUZ(IVA),IVA=1,KLL)
      WRITE(6, 820)
      WRITE(6, 830) (IVA,INOUZ1(IVA),IVA=1,KLL)
      WRITE(6, 833) (KOL(IP),IP=1,3)
      WRITE(6, 836) (NAL(IR),IR=1,3)
      WRITE(6, 840)
      WRITE(6, 850) (IN,NODEEL(IN),NODEEL(IN),IN=1,LUK0)
  800 FORMAT(10X,'RESULTS OF WORK: SORTUZ:')
  810 FORMAT(2X,'NOUZ(',I3,')=',I4)
  830 FORMAT(2X,'INOUZ1(',I3,')=',I4)
  820 FORMAT(120(' '))
  833 FORMAT(10X,'ARRAY KOL:'/6X,3(4X,I4))
  836 FORMAT(10X,'ARRAY NAL:'/6X,3(4X,L4))
  840 FORMAT(10X,'ARRAY NODEEL AFTER RECODING')
  850 FORMAT(2X,I3,2X,I4,2X,A4)
  900 CONTINUE

      RETURN
      END


      SUBROUTINE FIND(NEND,NNDIM,NUM)
C     B MACC�BE NNDIM ��ETC� BE����HA NUM.EC�� EE HET-B���EM
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 NEND,NUM,NNDIM(100)
      IF(NEND.EQ.0) GO TO 15
      DO 10 I=1,NEND
      IF(NNDIM(I).EQ.NUM) RETURN
   10 CONTINUE
   15 NEND=NEND+1
      NNDIM(NEND)=NUM
      RETURN
      END


      SUBROUTINE DOWN(ED,DOMIN,EI,INSERT)
C     �H��TO�A�TC� ��EMEHT� INSERT'A KOTOP�E ECT� B DOMIN'E
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER*4 DOMIN(100),INSERT(100),ED,EI
      DO 10 ID=1,ED
      IF(DOMIN(ID).EQ.0)GO TO 10
      DO 20 II=1,EI
      IF(DOMIN(ID).NE.INSERT(II)) GO TO 20
      INSERT(II)=0
      GO TO 10
   20 CONTINUE
   10 CONTINUE
      RETURN
      END


      SUBROUTINE RANGER(KS,INZ,NDIM,END,EXIST)
C     B���PAET �� NDIM'A B INZ ��EMEHT� HA��HA� C MIN
C
C     END - �EPEO�PE�E��ETC� . �P� BXO�E END - O��EE KO�-BO ���OB �AHHO
C T��A �O �CK���EH�� DOWN'OM , �P� B�XO�E �E - �E�CTB�TE��HOE KO�-BO
C ���OB �AHHO�O T��A /CM. B��OB/
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL EXIST
      INTEGER*4 INZ(100),NDIM(100),KS,END
      EXIST=.FALSE.
      IF(END.EQ.0)RETURN
      K=0
   20 MIN=9999
      DO 10 I=1,END
      IF(NDIM(I).EQ.0.OR.NDIM(I).GE.MIN) GO TO 10
      NMIN=I
      MIN=NDIM(NMIN)
   10 CONTINUE
      IF(MIN.EQ.9999) GO TO 30
      K=K+1
      KS=KS+1
      NDIM(NMIN)=0
      INZ(KS)=MIN
      GO TO 20
   30 END=K
      EXIST=.TRUE.
      RETURN
C     DEBUG INIT(KS,INZ,EXIST)
      END
