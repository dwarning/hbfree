c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




        SUBROUTINE LENA
C ***
C *** �PO�PAMMA �������� �A���� "A"
C ***
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       COMMON/BLKA/    A,KA
       COMMON/BLIFF/IFF(7,4),KIFF,NNIFF(4,8),KNNIFF,PNIFF(8),FNE1,FNE2
      CHARACTER*4 FNE1,FNE2
C
C
C      MACC�B "A" �B�. �A�OTOBKO� ��� �OPM�P. "MPOINT"
       INTEGER*4   A (20,50)
C                         KA - MAKC. KO�-BO T��OB ��EMEHTOB
      DATA A/
     +4HR   ,4H    ,4H    ,4H    ,0,1, 5,2,0,0,1,0, 1,0,0,0,0,0,0,0, 
     +4HL   ,4H    ,4H    ,4H    ,0,1, 5,2,0,0,1,0, 1,0,0,0,0,0,0,0,
     +4HC   ,4H    ,4H    ,4H    ,0,1, 5,2,0,0,1,0, 1,0,0,0,0,0,0,0, 
     +4HE   ,4H    ,4H    ,4H    ,0,2,10,2,0,0,1,0, 4,0,0,0,0,0,0,2, 
     +4HJ   ,4H    ,4H    ,4H    ,0,2,10,2,0,0,1,0, 4,0,0,0,0,0,0,2, 
     +4HP2  ,4H    ,4H    ,4H    ,0,2,10,2,0,0,1,0, 7,0,0,0,0,0,0,2, 
     +4HVD  ,4HSCHT,4H    ,4H    ,0,3,10,3,1,0,2,0, 8,0,0,0,0,1,1,0, 
     +4HGN  ,4HLIN ,4H    ,4H    ,0,3, 9,2,0,0,1,0, 1,0,0,0,0,1,1,0, 
     +4HGN  ,4HPOLY,4H    ,4H    ,0,3, 9,2,0,0,1,0, 7,0,0,0,0,1,1,0, 
     +4HGN  ,4HJUNC,4H    ,4H    ,0,3, 9,2,0,0,1,0, 4,0,0,0,0,1,1,0, 
     +4HFET ,4HCURT,4H    ,4H    ,0,3,13,6,3,0,1,0,17,0,0,0,0,2,2,0, 
     +4HICU ,4HJUNC,4H    ,4H    ,0,3,11,4,0,0,1,0, 5,0,0,0,0,1,1,0, 
     +4HICU ,4HPOLY,4H    ,4H    ,0,3,11,4,0,0,1,0, 7,0,0,0,0,1,1,0, 
     +4HCN  ,4HDIFF,4H    ,4H    ,0,3, 9,2,0,0,1,0, 5,0,0,0,0,2,2,0, 
     +4HCN  ,4HBARR,4H    ,4H    ,0,3, 9,2,0,0,1,0, 4,0,0,0,0,2,2,0, 
     +4HCN  ,4HLIN ,4H    ,4H    ,0,3, 9,2,0,0,1,0, 1,0,0,0,0,2,2,0, 
     +4HCN  ,4HPOLY,4H    ,4H    ,0,3, 9,2,0,0,1,0, 7,0,0,0,0,2,2,0, 
     +4HLIB0,4HLL0 ,4H    ,4H    ,0,2,12,4,0,0,1,0, 2,0,0,0,0,0,0,0, 
     +4HYTAB,4H    ,4H    ,4H    ,0,5, 0,0,0,0,1,0, 0,0,0,0,0,0,0,1, 
     +4HSTAB,4H    ,4H    ,4H    ,0,5, 0,0,0,0,1,0, 0,0,0,0,0,0,0,1, 
     +4HLIB ,4HMPL ,4H    ,4H    ,0,2,12,4,0,0,2,0, 7,0,0,0,0,0,0,0, 
     +4HLIB ,4HLANG,4H    ,4H    ,0,2,12,4,0,0,2,0,12,0,0,0,0,0,0,0, 
     +4HLIB ,4HSMPL,4H    ,4H    ,0,2,12,4,0,0,2,0,11,0,0,0,0,0,0,0, 
     +4HBIP ,4HTR  ,4H    ,4H    ,0,3,14,7,4,0,1,0,23,0,0,0,0,2,2,0, 
     +4HSHL ,4HKZ  ,4H    ,4H    ,0,2,10,2,0,0,2,0, 3,0,0,0,0,0,0,0, 
     +4HSHL ,4HXX  ,4H    ,4H    ,0,2,10,2,0,0,2,0, 3,0,0,0,0,0,0,0,
     +4HDISC,4H    ,4H    ,4H    ,0,2,10,2,0,0,2,0, 3,0,0,0,0,0,0,0,
     +4HINDS,4H    ,4H    ,4H    ,0,2,14,6,0,0,1,0,12,0,0,0,0,0,0,0, 
     +440*0/
C
C      ��� �������� ������ ���A ��������� ���������� ������ ����������
C      � ���������� INSTIL.TXT ( ������������� YINSTR ) � � CHAPTER5.REP
C      (������������� REPORT)
C
C      ���� ��������� ,4HBIP TR, 'SHL KZ, 'SHL XX'
C      ���� �� ������������ (��� ��� �� ������������).
C
      KA=28
C
C   ���������� ������� 'A'
C     A(1) ... A(4) - �������� ���� ��������;
C     A(6) - ������� ������������ ��� ��������:
C              �������� ������������ -  1,
C              �������� ������������� - 2,
C              ���������� ������� - 3,
C              Y-, S-������� - 5.
C     A(7) - ����� ������ �������� ������ ��������
C            ������� ���� � "NODEEL":
C              ��� �������� �������������� - 5,
C              ��� �������� ��������������� - 8+���������� ����. �����,
C              ��� ���������� ��������� - 7+���������� ����. �����
C              ��� Y-, S-������ - 0.
C     A(8) - ���������� ����� � �������������� ������
C            (�������+����������):
C              ��� �������� �������������� - 2,
C              ��� �������� ��������������� - 0, �.�. ��� ��������
C              ������������ ��� ������� ���. �������������� ������������
c     �,
C              ��� ���������� ��������� - ���-�� �����,
C              ��� Y-, S-������ - 0.
C    A(9) - ���������� ���������� �����, ������������� ����������
C           ��� �������� ����� ��������� ��������.
C    A(11) - ���������� ���������� ����� ��� ������� ���� (���� ��������
c     �
C            ����������, �� ���������� ������� 1).
C    A(13) - ���������� �������������� ����������, ����������
C            �������������� ������ ��������.
C    A(18) - ��� ���������� ��������� ��������� ����������
C            ���������� ������� ������� ��� ������� ����.
C    A(19) - ��� ���������� ��������� ��������� ����������
C            ���������� �������� ������� ��� ������� ����.
C
C
C  ������� ���� "R"        ���������� �/� LIN2P         (���� FORMER)
C  ������� ���� "L"        ���������� �/� LIN2P         (���� FORMER)
C  ������� ���� "C"        ���������� �/� LIN2P         (���� FORMER)
C  ������� ���� "E"        ���������� �/� EMF           (���� LIBLIN)
C  ������� ���� "J"        ���������� �/� JDRFE         (���� LIBLIN)
C  ������� ���� "P2"       ���������� �/� RFFIPN        (���� LIBLIN)
C  ������� ���� "VD SCHT"  ���������� �/� MDSCH1-MDSCH6 (���� MDSCH)
C  ������� ���� "GN LLIN"  ���������� �/� LIN1-LIN5     (���� LIN)
C  ������� ���� "GN POLY"  ���������� �/� POLY51-POLY55 (���� POLY5)
C  ������� ���� "GN JUNC"  ���������� �/� JUNC1-JUNC6   (���� JUNC)
C  ������� ���� "FET CURT" ���������� �/� CURT1-CURT6   (���� CURT)
C  ������� ���� "ICU JUNC" ���������� �/� ICUJ1-ICUJ6   (���� ICUJUNC)
C  ������� ���� "ICU POLY" ���������� �/� ICUPL1-ICUPL5 (���� ICUPOLY)
C  ������� ���� "CN DIFF"  ���������� �/� CDIFF1-CDIFF6 (���� CDIFF)
C  ������� ���� "CN BARR"  ���������� �/� CBARR1-CBARR5 (���� CBARR)
C  ������� ���� "CN LIN"   ���������� �/� CLIN1-CLIN5   (���� CLIN)
C  ������� ���� "CN POLY"  ���������� �/� CPOLY1-CPOLY5 (���� CPOLY)
C  ������� ���� "LIB0 LL0" ���������� �/� LINE1,LINE2   (���� LIB0)
C  ������� ���� "YTAB"     ���������� �/� YTAB          (���� YTAB)
C  ������� ���� "STAB"     ���������� �/� STAB          (���� STAB)
C  ������� ���� "LIB MPL"  ���������� �/� MP            (���� MPL)
C  ������� ���� "LIB LANG" ���������� �/� LANG          (���� LANGE)
C  ������� ���� "LIB SMPL" ���������� �/� SMPL          (���� SMPL)
C  ������� ���� "BIP TR"   ���������� �/� BIPTR1-BIPTR5 (���� BIPTR)
C  ������� ���� "SHL KZ"   ���������� �/� SHLEIF        (���� SHLEIF)
C  ������� ���� "SHL XX"   ���������� �/� SHLEIF        (���� SHLEIF)
C  ������� ���� "DISC"     ���������� �/� DISCONT       (���� DISCONT)
C  ������� ���� "INDS"     ���������� �/� INDSV         (���� INDSV)
C
C
      DATA IFF/
     +4HP2  ,4H    ,4H    ,4H    ,2,0,0, 
     +4HP2  ,4H    ,4H    ,4H    ,5,0,0, 
     +4HE   ,4H    ,4H    ,4H    ,2,0,0, 
     +4HJ   ,4H    ,4H    ,4H    ,2,0,0
     +/

      DATA   KIFF/4/,   KNNIFF/0/
C
      RETURN
      END
