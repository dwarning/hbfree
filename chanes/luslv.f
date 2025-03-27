c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE LUSLV (ALU,NTOT,N,NF,NEND,FLAG)
C**************************************************************
C*          �/� KOM��EKCHO� LU-�AKTOP��A���                   *
C*   �PE�HA�HA�EHA ��� �AKTOP��A��� �ACT� MATP��� ALU .       *
C*                                                            *
C*          �OPMA��H�E �APAMETP�:                             *
C*    ALU-�CXO�HA� MATP��A,�MEET HEHOPMA��.(HEKAHOH��EC.) ��� *
C*   Y-MATP��� B��:HA B�XO�E:L-MATP��A �PE�CTAB��ET CO�O� �PO-*
C*   ��BE�EH�E D HA L' (L=L'*D),�P��EM L' �MEET E��H��H�� ��- *
C*   A�OHA��,U-MATP��A TAK�E �MEET E��H��H�� ��A�OHA��,A OT   *
C*   ��EMEHTA A(NEND+1,NEND+1) HA��HAETC� HEPA��O�EHHA� MAT-  *
C*   P��A,KOTOPA� COOTBETCTB�ET �PA�� C �CK���EHH�M� ���AM�   *
C*   1..N                                                     *
C*   HEPA��O�EHHA� MATP��A COXPAH�ET CBO� HEKAHOH��ECK�� B��. *
C*   B PA��O�EHHO� �ACT� ��A�OHA�� KAHOH���POBAHA.            *
C*   HEKAHOH��ECKO� C��TAETC� Y-MATP��A,��A�OHA��H�E ��EMEHT� *
C*   KOTOPO� HE �O�H�E ���OB�E �POBO��MOCT�,A TO��KO �POBO��- *
C*   MOCT� ME��� I-M � H��EB�M ���AM�.                        *
C*    COMPLEX ALU(NTOT,NTOT)                                  *
C*      NTOT-�O�H�� PA�MEP MATP��� ALU (�O O�'�B�EH�� PA�MEP- *
C*  HOCT� B B���BA��E� �PO�PAMME);                            *
C*      N-PA�MEP O�PA�AT�BAEMO� (T.E. �A�O�HEHHO�) �ACT� ALU; *
C*      NF-HOMEP CTPOK�,C KOTOPO� HEO�XO��MO HA�AT� �AKTOP�-  *
C*   �A���;                                                   *
C*      NEND-HOMEP CTPOK�,KOTOPO� HEO�XO��MO �AKOH��T� �AKTO- *
C*   P��A���;                                                 *
C*      FLAG-�KA�ATE�� O���OK:                                *
C*        0-O���OK HET,                                       *
C*        1-N,NEND ��� NF MEH��E 1                            *
C*        2-N,NEND ��� NF �O���E NTOT                         *
C*        3-NF �O���E NEND                                    *
C*        4-NF ��� NEND �O���E N                              *
C*                                                            *
C**************************************************************

C$LARGE: ALU
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE COMPLEX ALU(NTOT,NTOT)
      DOUBLE COMPLEX DIAG(100),AKI,DI,SUM,DIFF
      INTEGER FLAG
C  �POBEPKA BXO�H�X �AHH�X
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
C  �CK���EH�E:EC�� NEND=NF-1,TO BE����X ��EMEHTOB - 0
C  (HOPMA��HOE OKOH�AH�E.BO�BPAT)  (!) FLAG=4 (!)
      IF(NEND-NF+1.EQ.0) RETURN
      IF(FLAG.NE.0)  WRITE(6, 35)  FLAG
      IF(FLAG.NE.0)RETURN
C  EC�� N=1,TO �AKTOP��A��� MO�HO C��TAT� OKOH�EHHO� - MATP��A
C  CO�EP��T O��H ��EMEHT ALU(1,1)
      IF(N.EQ.1) RETURN
C  �P�BE�EH�E AKT�BHO� �ACT� Y-MATP��� ALU K KAHOH��ECKOM�
C  B���. ��A�OHA��H�E ��EMEHT� ���EM XPAH�T� B OT�E��HOM
C  BEKTOPE �BO�HO� TO�HOCT�. �OC�E �C�O���OBAH�� ��EMEHTA BEK-
C  TOPA DIAG OH �AHOC�TC� HA CBOE MECTO B ALU. HE�C�O���OBAHH�E
C  �OTOM �P�BO��TC� K HEKAHOH��. B��� � �AHOC�TC� HA CBO� MECTA
      DO 5 IROW=NF,N
      SUM=2.D0*ALU(IROW,IROW)
      DO 7 JCOL=NF,N
   7  SUM=SUM-ALU(IROW,JCOL)
      DIAG(IROW)=SUM
   5  CONTINUE
C  �AKTOP��A��� NEND-NF+1 BE����X ��EMEHTOB
C  �CK���EH�E: EC�� NEND=N,TO NEND=NEND-1
      NE=NEND
      IF(NEND.EQ.N) NE=NEND-1
      DO 10 KPIV=NF,NE
C �AHECEH�E �C�O���OBAHHO�O ��EMEHTA DIAG B A(KPIV,KPIV)
C     WRITE(6,1111) NF,NE,KPIV,DIAG(KPIV)
C1111 FORMAT(2X,'LUSLV: NF=',I5,' NE=',I5,' KPIV=',I5,' DIAG=',2E12.5)
      ALU(KPIV,KPIV)=DIAG(KPIV)
C  HOPM�P����� MHO��TE�� ��� ��-TOB BE���E� CTPOK�
      DI=1.D0/(DIAG(KPIV)+0.1D-30)
C  BO BCEX CTO���AX AKT�BHO� �O�MATP���:
      KPIVP1=KPIV+1
      DO 20 JCOL=KPIVP1,N
C  HOPM�P�EM ��EMEHT� BE���E� CTPOK�
      AKI=ALU(KPIV,JCOL)*DI
      ALU(KPIV,JCOL)=AKI
C  �OMHO��M E�O HA ��-T BE�.CTO���A
C  � B��TEM �� TEK��E�O ��-TA.
C  � TAK CO BCEM� ��-TAM� CTO���A
      DO 30 IROW=KPIVP1,N
      IF(IROW.NE.JCOL)ALU(IROW,JCOL)=ALU(IROW,JCOL)-AKI*ALU(IROW,KPIV)
      IF(IROW.EQ.JCOL)    DIAG(IROW)=DIAG(IROW)    -AKI*ALU(IROW,KPIV)
   30 CONTINUE
   20 CONTINUE
   10 CONTINUE
C  �P�BE�EH�E OCTAB�E�C� �ACT� AKT�BHO� �O�MATP���
C  K HEKAHOH��ECKOM� B���.
C
C  �O BCEM CTPOKAM AKT�BHO� �O�MATP���:
      NEP1=NE+1
      DO 23 IROW=NEP1,N
      DIFF=DIAG(IROW)
C  �O BCEM ��EMEHTAM CTPOK� (KPOME ��A�OHA��HO�O)
      DO 25 JCOL=NEP1,N
      IF(IROW.NE.JCOL)DIFF=DIFF+ALU(IROW,JCOL)
   25 CONTINUE
      ALU(IROW,IROW)=DIFF
   23 CONTINUE
      RETURN
   35 FORMAT('    LUSLV:'/'   ERR IN INPUT DATA.FLAG=',I4)
      END
