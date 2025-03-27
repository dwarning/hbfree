c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE LNFORM(OMEGA1,Y,VJ,ISIZE_MAXNODE)
C
C

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'
      include 'charint.i'

      INTEGER ISIZE_MAXNODE
      DOUBLE COMPLEX Y(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX VJ(ISIZE_MAXNODE)
C      COMMON/FORMY/VEKTJ(70),Y(70,70)

      LOGICAL      DSIGN
      INTEGER      VARTYP,VARABL,ADPAR1,ADRES1
      DOUBLE PRECISION    PAR,OMEGA,OMEGA1
      DOUBLE COMPLEX      SUBY,SUBJ
      CHARACTER*4  NAME(4)

C      COMMON/POINT/MPOINT(1)
C      COMMON/NODEL/NODEEL(1)
C      COMMON/POINTR/NMPNT,NNODE,NPARAM,LENNOD,LENPAR,NNETPR,LENNTP
C      COMMON/PARAMS/PARAM(1)

      COMMON/SUBC/SUBY(15,15),SUBJ(15)


C      PRINT *, 'ENTER LNFORM' 
C      print *,(vj(ikkk),ikkk=1,20)

      VARABL=0
      GO TO 5
C  BXO� ��� �OPM�POBAH�� ��HE�HO� ��MEH�EMO� �ACT�.
      ENTRY VLFORM(OMEGA1,Y,VJ,ISIZE_MAXNODE)
      VARABL=1
C  O�PE�E�EH�E �HAKA �ACTOT�.
    5 DSIGN=.TRUE.
      IF(OMEGA1.LT.0.0D0) DSIGN=.FALSE.
      OMEGA=DABS(OMEGA1)

C     DO �O T��AM ��-TOB
      I10E=NMPNT*20
      DO 10 I10=1,I10E,20
C
C     EC�� ��-TOB �AHHO�O T��A HET - �PO��CT�M
      IF(MPOINT(I10+4).EQ.0)GO TO10
C
C     MO�ET H� O��H �� ��-TOB T��A HE MEH��TC� , A �OPM�POBAH�E-
C     ��MEH�EMOE . �PO��CKAEM
      IF(VARABL.NE.0.AND.MPOINT(I10+14).EQ.0)GO TO 10

      NFIRST=MPOINT(I10+9)
      NLEN=MPOINT(I10+6)
      ITYP=MPOINT(I10+5)
      LENPAR=MPOINT(I10+10)
      IBASE=MPOINT(I10+13)
C     LENBAS=MPOINT(I10+12)-LENPAR-LENNTP
      LENBAS=MPOINT(I10+12)
      ADPAR1=MPOINT(I10+11)
      NAME(1)=I2C(MPOINT(I10))
C      WRITE(6,1110) NAME(1)
C 1110 FORMAT(2X,'LNFORM: NAME(1)=',A4)
C      WRITE(6,1111) NFIRST,NLEN,ITYP,LENPAR,IBASE,
C     *              LENNTP,LENBAS,ADPAR1
C 1111 FORMAT(2X,'LNFORM: NFIRST, NLEN, ITYP, LENPAR, IBASE, LENNTP=',
C     *  6I5,/10X,'LENBAS=',I3,' ADPAR1=',I3)
      NAME(2)=I2C(MPOINT(I10+1))
      NAME(3)=I2C(MPOINT(I10+2))
      NAME(4)=I2C(MPOINT(I10+3))
C
      NP=NFIRST
C
C     DO �O ��-TAM BH�TP� T��A
      I11E=MPOINT(I10+4)
      DO 20 I11=1,I11E
      NATADR=NFIRST+(I11-1)*NLEN


C ##  ��HE�H�� 2X-�O��CH�K ############################################
      IF(ITYP.NE.1) GO TO 30
C  �POBEP�M COOTBETCTB�E �P��HAKA BAP�A��� IVAR T��� �OPM�POBAH�� VAR-
C  ABL
      IF(NODEEL(NATADR+3).NE.VARABL) GO TO 20
      CALL LIN2P(OMEGA,NAME,PARAM(1),PARAM(ADPAR1),
     +           PARAM(IBASE+I11-1),DSIGN,NATADR,Y,VJ,ISIZE_MAXNODE)
      GO TO 20

C ## ��HE�H�� MHO�O�O��CH�K ###########################################
   30 IF(ITYP.NE.2)GO TO 35
C  �POBEP�M COOTBETCTB�E IVAR � VARABL
      IDE=NODEEL(NATADR+1)
      IF(NODEEL(NATADR+IDE+6).NE.VARABL) GO TO 20
C  HA�.A�PEC � ���HA �O�HA�OPA �H��B��. �AP-POB.
      ADRES1=NODEEL(NATADR+IDE+5)
      LENTH=NODEEL(NATADR+2)
C  B��OB MATP��� ��EMEHTA.
      CALL LIBLIN(NAME,OMEGA,NNETPR,LENNTP,ADPAR1,
     +            LENPAR,ADRES1,LENTH,IDE)
C  �AHECEH�E B C�MMAPH�� MATP���.
      CALL LINNP(OMEGA,NATADR,DSIGN,Y,VJ,ISIZE_MAXNODE)
      GOTO 20

C ## Y-MATP��A ########################################################
   35 CONTINUE
      IF(ITYP.NE.5) GOTO 40
      NP10=NP+10
C     PRINT 355,NP,(NODEEL(JNP),JNP=NP,NP10)
C 355 FORMAT(2X,'NODEEL(',I3,')=',A4,9(1X,I5))

C  �POBEP�M COOTBETCTB�E IVAL � VARABL
      IDE=NODEEL(NP+1)
      IF(NODEEL(NP+IDE+6).NE.VARABL) GOTO 20
C  HA�. A�PEC � ���HA �O�HA�OPA �H��B��. �AP-POB.
      ADRES1=NODEEL(NP+IDE+5)
      LENTH=NODEEL(NP+2)
C  B��OB MATP��� ��EMEHTA.
      CALL LIBLIN(NAME,OMEGA,NNETPR,LENNTP,ADPAR1,LENPAR,ADRES1,
     +            LENTH,IDE)
C  �AHECEH�E B C�MMAPH�� MATP���.
      newnp=np
      CALL LINNP(OMEGA,newNP,DSIGN,Y,VJ,ISIZE_MAXNODE)
      NP=NP+8+IDE
      GOTO 20
C
C
C ### HE��HE�H�� MHO�O�O��CH�K ########################################
   40 IF(ITYP.NE.3)GO TO 20
C  �POBEPKA IVAR - VARABL
      IDE=MPOINT(I10+7)
      IF(NODEEL(NATADR+IDE+4).NE.VARABL) GO TO 20
C  KOE-KAK�E BE����H� (B T.�. A�PEC)
      III=I10
      ADRES1=NODEEL(NATADR+IDE+3)
C  Y-MATP��A ��H. �ACT� HE��H. ��EMEHA...
      LENBAS=MPOINT(I10+12)
      CALL LIBMD2(NAME,OMEGA,NNETPR,LENNTP,ADPAR1,LENPAR,ADRES1,LENBAS)
C  ...�AHOC�TC� B C�MMAPH�� Y-MATP���.
      CALL NONLIN(III,NATADR,DSIGN,Y,VJ,ISIZE_MAXNODE)

C END OF DO - �O ��EMEHTAM.
C      print *,'element  ',I11
C      print *,(vj(ikkk),ikkk=1,20)


   20 CONTINUE
C END OF DO - �O T��AM ��-TOB.
   10 CONTINUE
      RETURN
      END


      SUBROUTINE LIN2P(OM,N,PN,PT,PE,S,NR,Y,J,ISIZE_MAXNODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'
      
      DOUBLE PRECISION   OM
      LOGICAL S
      DIMENSION NAMES(4),N(4)
      DOUBLE PRECISION PN,PT,PE,ZN
      DOUBLE COMPLEX YS
C      COMMON/NODEL/NODEEL(1)

C      COMMON/FORMY/J(70),Y(70,70)
      INTEGER ISIZE_MAXNODE
      DOUBLE COMPLEX Y(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX J(ISIZE_MAXNODE)
      
      CHARACTER*4 NAMES/'R   ','L   ','C   ','G   '/,N

      IF(N(1).EQ.NAMES(1))YS=DCMPLX(1.D0/PE,0.0D0)
      IF(N(1).EQ.NAMES(3))YS=DCMPLX(PT,OM*PE)
      IF(N(1).NE.NAMES(2))GO TO 10
      ZN=PT**2+(PE*OM)**2
      YS=DCMPLX(PT/ZN,-PE*OM/ZN)
   10 IF(N(1).EQ.NAMES(4))YS=DCMPLX(PE,0.0D0)
      IF(.NOT.S)YS=DCONJG(YS)
C  B���C��M HOMEPA ���OB ��EMEHTA
      IY=NODEEL(NR+1)
      JY=NODEEL(NR+2)
C     WRITE(6, 13) NR, IY, JY
C  13 FORMAT(2X,'LIN2P: NR, IY, JY =',3I5)
C  EC�� ��EMEHT OTK���EH-BO�BPAT
      IF(IY.EQ.JY) RETURN
C  EC�� �O�K���EH K �EM�E-O�PA�OTAEM H��E
      IF(IY.EQ.0.OR.JY.EQ.0) GO TO 25
C  � EC�� BK���EH B CXEM� � HE�O�COE��HEH K �EM�E:
      Y(IY,JY)=Y(IY,JY)-YS
      Y(JY,IY)=Y(JY,IY)-YS
C     WRITE(6, 15) IY,JY,Y(IY,JY),JY,IY,Y(JY,IY)
C  15 FORMAT(2X,'LIN2P:'/(2X,'Y(',I3,',',I3,')=',E12.5,2X,E12.5))
      RETURN
C  �A�EM�EHH�� ��EMEHT
   25 CONTINUE
      KK=IY+JY
      IF(KK.NE.0)Y(KK,KK)=Y(KK,KK)+YS
C     WRITE(6, 15) KK,KK,Y(KK,KK)
      RETURN
      END


      SUBROUTINE LINNP(OMEGA,NR,S,Y,VECTJ,ISIZE_MAXNODE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'circuit.i'
      LOGICAL S,INPUT
      INTEGER END

      DOUBLE PRECISION   OMEGA
      DOUBLE COMPLEX ZERO/(0.0D0,0.0D0)/,YD
      DOUBLE COMPLEX SUBY,SUBJ

c      COMMON/POINT/MPOINT(1)/NODEL/NODEEL(1)
      COMMON/SUBC/SUBY(15,15),SUBJ(15)
      
C      COMMON/FORMY/VEKTJ(70),Y(70,70)
      INTEGER ISIZE_MAXNODE
      DOUBLE COMPLEX Y(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX VECTJ(ISIZE_MAXNODE)

C BXO� ��� BHECEH�� B Y-MATP��� ��H. MHO�O�O��CH�KOB...
      INPUT=.FALSE.

      END=NODEEL(NR+1)
      NR=NR+3
C      WRITE(6,100) END, NR
C  100 FORMAT(2X,'LINNP: END=',I4,', NR=',I4)
C      WRITE(6, 2)((II,JJ,SUBY(II,JJ),JJ=1,END),II=1,END)
C    2 FORMAT (2X,'LINNP:'/('  SUBY(',I3,',',I3,')=',E12.6,2X,E12.6))
      GOTO 3
C ...��� Y-MAPT�� ��H. �O�CXEM HE��HE�H�X ��EMEHTOB.
      ENTRY NONLIN(IS,NR,S,Y,VECTJ,ISIZE_MAXNODE)
      END=MPOINT(IS+7)
      
      INPUT=.TRUE.
C B���C��M KOM��EKCHO-CO�P��.MATP��� (EC�� �ACTOTA < 0.)
    3 IF(S) GO TO 5
      DO 11 IN=1,END
      DO 11 IM=1,END
   11 SUBY(IM,IN)=DCONJG(SUBY(IM,IN))
    5 CONTINUE
C      WRITE(6, 2) ((II,JJ,SUBY(II,JJ),JJ=1,END),II=1,END)

C �P�BE�EM �O�MATP��� K HEKAHOH��ECKOM� B���:               !
C  Y(I,I)=Y(I,I)+Y(I,J),�P� J=1..N,(J.NE.I).                !
      DO 10 IN=1,END
      YD=ZERO
      DO 15 JN=1,END
   15 YD=YD+SUBY(IN,JN)
   10 SUBY(IN,IN)=YD
C      WRITE(6, 2) ((II,JJ,SUBY(II,JJ),JJ=1,END),II=1,END)

C  �AHECEH�E B ��A�OHA�� �A�EM�EHH�X ���OB:
      DO 20 I20=1,END
      NRI20=NR+I20
C      WRITE(6,110) END, NR, NRI20, NODEEL(NRI20), I20
C  110 FORMAT(2X,'END=',I4,' NR=',I4,' NODEEL(',I4,')=',I4,' I20=',I4)
      IF(NODEEL(NR+I20).NE.0) GO TO 20
      DO 30 I30=1,END
C      WRITE(6,120) I30, I20, SUBY(I30,I30), SUBY(I20,I30)
C  120 FORMAT(2X,'I30',I4,' I20',I4,' SUBY=',2(1X,2(1X,E12.6)))      
   30 SUBY(I30,I30)=SUBY(I30,I30)-SUBY(I20,I30)
C      WRITE(6,130) SUBY(I30,I30)
C  130 FORMAT(2X,'SUBY-SUBY=',2(1X,E12.6))
   20 CONTINUE
C      WRITE(6, 2) ((II,JJ,SUBY(II,JJ),JJ=1,END),II=1,END)

C �OPM�POBAH�E (CO�CTBEHHO)
      DO 40 I40=1,END
      IY=NODEEL(NR+I40)
      IF(IY.EQ.0)GO TO 40

      DO 50 I50=1,END
      JY=NODEEL(NR+I50)
      IF(JY.EQ.0)GO TO 50

C  �������� ! ������������   28.02.92 
      
      if(i40.ne.i50.and.iy.eq.jy) then
                              write(6,43) 
   43 format(2x,' ####### LINNP : LOOP IS DETECTED  #######')
                               goto 50
      end if

      Y(JY,IY)=Y(JY,IY)+SUBY(I50,I40)
C      WRITE(6,45) JY,IY,Y(JY,IY)
C   45 FORMAT(2X,'LINNP:'/ 2X,'Y(',I3,',',I3,')=',2(E12.5,2X))

   50 CONTINUE
   40 CONTINUE


      IF(INPUT)RETURN
      IF(NODEEL(NR).NE.2)RETURN
C  �AHECEH�E B BEKTOP VECTJ TOKOB
C  �CTO�H�KOB ��HE�H�X AKT�BH�X N-�O��CH�KOB.
      DO 60 I60=1,END
      J=NODEEL(NR+I60)
      IF(J.EQ.0) GO TO 60
      VECTJ(J)=VECTJ(J)+SUBJ(I60)
C      WRITE(6, 55) J,VEKTJ(J)
C   55 FORMAT(2X,'LINNP:'/ 2X,'VEKTJ(',I3,')=',2(E12.5,2X))
   60 CONTINUE

      RETURN
      END
