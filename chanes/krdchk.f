c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      INTEGERfunction krdchk(MN,KR,KC,NNR,KNR,KN,KNC,IR1,IR2)
C
C     �������� 12.05.91    ������ �.�.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER    KR(IR2),KC(IR1),NNR(IR1)
      INTEGER    MN(2,IR2)

      integer err,sum1,sum2,sum3

C     �������� ������������� ����������� ������ ���
C     �������� ����������� �
C
C     ������ ���������� ���������� - ��� �   KOORD
C
C     ��EHT���KATOP� �EPEMEHH�X � MACC�BOB :
C
C          KNC - ���HA O�HOMEPHO�O ���;
C          NNR - MACC�B HOMEPOB HEH��EB�X CTPOK;
C          KNR - KO���ECTBO HEH��EB�X CTPOK;
C          KR  - MACC�B HOMEPOB CTO���OB HEH��EB�X
C                ��EMEHTOB B KA��O� HEH��EBO� CTPOKE;
C          KC  - MACC�B, CO�EP�A��� KO���ECTBO
C                HEH��EB�X ��EMEHTOB B KA��O� CTPOKE;

      err=0


c    �������� ����� �����:
c           /                  \
c   i=IR1  |  1 if NNR(i) != 0  |
c    SUM  <                      >  == KNR     ���� ���, �� err=err+1
c    i=1   |  0 if NNR(i) == 0  |
c           \                  /
      sum1=0
      do 10 i=1,IR1
 10   if (NNR(i).ne.0) sum1=sum1+1

      if (sum1.ne.KNR) err=err+1

c     ����� �������� ��������� � �������
c                        /                 \
c    i=IR1       i=IR2  |  1 if KR(i) != 0  |
c    SUM KC(i) ==SUM   <                     > ���� ���, �� err=err+2
c    i=1         i=1    |  0 if KR(i) == 0  |
c                        \                 /
      sum2=0
      do 20 i=1,IR1
 20   sum2=sum2+KC(i)

      sum3=0
      do 30 i=1,IR2
 30   if( kr(i).ne.0) sum3=sum3+1

      if (sum2.ne.sum3) err=err+2


c     �������� ������������ � ������� ( ����� �� ����������� !!!)
c   for j=1 to KNR
c
c  1 + SUM {1}   =  KC(j)                       ���� ���, �� err=err+4
c  KR(i+1) > KR(i)
c   KR(i) > 0
c
c  end_for
c                -------- !!!  � ��������� ����� �� ����������� !!!

      krdchk=err
      return
      end
