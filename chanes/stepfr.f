c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE STEPFR (OM,NREC,Y,VJ,ISIZE_MAXNODE)
C*********************************************************************
C* �/� �OPM�POBAH�� � PE��K��� MHO�O�O��CH�KA ��HE�HO� �O�CXEM� �    *
C*                      OP�AH��A��� �X I/O HA DA                     *
C*                                                                   *
C*********************************************************************

C ��������� �� 30.01.91 .  �������� MAIN
C$LARGE: BUFFER
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/MATY/  BUFFER (6000),BUFLEN
      DOUBLE COMPLEX       BUFFER
      INTEGER*4     BUFLEN
      COMMON/BLK2/  KNC,KNR,KN,KNR1,KN1

      INTEGER ISIZE_MAXNODE
      DOUBLE COMPLEX VJ(ISIZE_MAXNODE)
      DOUBLE COMPLEX Y(ISIZE_MAXNODE,ISIZE_MAXNODE)

      COMMON/KOLNAL/ KOL,NAL
      INTEGER        KOL(4),DY,FLAG
      LOGICAL        NAL(4)
      DOUBLE PRECISION           OM

C ��������� �� 30.01.91 .  �������� MAIN
C  ���������� ������� ��� ����������:
      IFIND1(I,M,NU)=(I+(M-1)*NU)
      IFIND2(I,J,M,NU,MF)=NU*MF+I+(J-1)*NU+(M-1)*NU*NU


      DY=ISIZE_MAXNODE
C @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      KKK=KOL(1)+KOL(2)+KOL(3)

C     O�H��EH�E MATP��� Y � BEKTOPA VJ
      CALL ZINY(Y,VJ,ISIZE_MAXNODE)
C      print *,'after ZINY'
C      print *,(VJ(ikkk),ikkk=1,20)
      
      IF(.NOT.NAL(1)) GO TO 20

C     LIN.CONST.-�OPM�POBAH�E
      CALL LNFORM (OM,Y,VJ,ISIZE_MAXNODE)

C   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,2)((III,JJJ,Y(III,JJJ),JJJ=1,KKK),III=1,KKK)
C   1  FORMAT(2X,' Y   STEPFR ',I2,' KOL(1),KOL(2),KOL(3),KKK=',4I3)
C   2  FORMAT(2X,'Y(',I3,',',I3,')=',1X,E12.6,1X,E12.6)
C      WRITE(6,3) (III, VJ(III),III=1,KKK)      
C   3  FORMAT(2X,'STEPFR VJ(',I3,')=',E12.6,',',E12.6)
      
      IF(KOL(1).EQ.0) GO TO 10
      NF=1
      NEND=KOL(1)
      N=KOL(1)+KOL(2)+KOL(3)
C  PE��K��� Y
      CALL LUSLV (Y,DY,N,NF,NEND,FLAG)

C  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

C  PE��K��� VJ
      CALL LUFRW (Y,VJ,DY,N,NF,NEND,FLAG)

C  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      write (6,*) 'Y matrix'
C      do ii=1,n
C            write (6,120) (Y(ii,jj), jj=1,n)
C      enddo      
C120   format (2x,'(',1x,e12.5,1x,e12.5,')')


C     ��AKOBKA � �A��C� MATP��� Y � BEKTOPA VJ HA DA
   10 CALL PACK1 (NREC,Y,VJ,ISIZE_MAXNODE)
      GO TO 20
C**********************************************************************
      ENTRY DOUBLE(OM,NREC)
C**********************************************************************
C* BXO� B �/� ��� �OBTOPHO�O �OPM�POBAH�� (�P� BAP��POBAH�� ��MEH�EM�X*
C*                      ��EMEHTOB)                                    *
C*                                                                    *
C**********************************************************************

C     O�H��EH�E Y
      CALL ZINY(Y,VJ,ISIZE_MAXNODE)
C   �TEH�E C DA � PAC�AKOBKA C�OPM�POBAHH�X Y � VJ �� CONST.-��EMEHTOB
      CALL DPACK1(NREC,Y,VJ,ISIZE_MAXNODE)



   20 IF(.NOT.NAL(2)) GO TO 30

C     LIN.VAR.-�OPM�POBAH�E
      CALL VLFORM(OM,Y,VJ,ISIZE_MAXNODE)

C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      IF(KOL(2).EQ.0) GO TO 30
      NF=KOL(1)+1
      NEND=KOL(1)+KOL(2)
      N=KOL(1)+KOL(2)+KOL(3)
C  PE��K��� �O�HO� MATP��� LIN.-�O�CXEM�
      CALL LUSLV (Y,DY,N,NF,NEND,FLAG)

C @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

C  PE��K��� �O�HO�O BEKTOPA �A�A���X TOKOB LIN.-�O�CXEM�
      CALL LUFRW (Y,VJ,DY,N,NF,NEND,FLAG)


C @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

C  �P�BE�EH�E MATP��� Y K KAHOH��ECKOM� B���
   30 CONTINUE
      N=KOL(1)+KOL(2)+KOL(3)
      NF=KOL(1)+KOL(2)+1
      CALL LUCAN(Y,DY,N,NF,FLAG)

C @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

C   �E�EH�E �O�O�AM BEKTOPA �A�A���X
C  TOKOB (IF(OMEGA.NE.0.)). CB��AHO C B��OM ���.
C
C
      K123=KOL(1)+KOL(2)+KOL(3)
      IF(NREC.EQ.1)GOTO 80
      DO 90 JJ=1,K123
   90 VJ(JJ)=VJ(JJ)/2.D0
   80 CONTINUE

C  ��AKOBKA � �A��C� Y HA M�
      CALL PACK2 (NREC,Y,VJ,ISIZE_MAXNODE)

C  �EPEC��KA PE�-TA �-� � PE��K��� B /MATY/-��OK,XPAH���� Y � J
C  PE����POBAHHO� ��HE�HO� �O�CXEM� HA BCEX �ACTOTAX
      K3=KOL(3)
      K12=KOL(1)+KOL(2)
      DO 70 KI=1,K3
      IND1=KI+K12

C  ��������� �� 30.01.91    ������� ������ �.�.
C      JR(KI,NREC)=VJ(IND1)       **** ������ ������
      BUFFER( IFIND1(KI,NREC,K3) )=VJ(IND1)

      DO 70 KJ=1,K3
      IND2=K12+KJ

C  ��������� �� 30.01.91    ������� ������ �.�.
C      YR(KJ,KI,NREC)=Y(IND2,IND1)   **** ������ ������
      ILOCAL=IFIND2(KJ,KI,NREC,K3,KN)
      IF (ILOCAL.GT.BUFLEN) GOTO 100
      BUFFER( ILOCAL )=Y(IND2,IND1)

C  NO    WRITE(6,3000) KJ,KI,NREC,YR(KJ,KI,NREC)
C  NO 3000 FORMAT(2X,'STEPFR,YR(',I3,',',I3,',',I3,')=',E12.5,2X,E12.5)
C      WRITE(6,3003) IND2,IND1,Y(IND2,IND1)
C 3003 FORMAT(2X,'        Y(',I3,',',I3,')=     ',E12.5,2X,E12.5)
   70 CONTINUE
      RETURN
100   WRITE(6,2000)
      STOP
C     DEBUG SUBTRACE,INIT(K12,K3,KI,KJ,IND1,IND2)

2000  FORMAT (2X,' BUFFER SIZE FOR REDUCED MATRICES EXCEDED'/        2X,
     +' ABNORMAL TERMINATION.')



      END
