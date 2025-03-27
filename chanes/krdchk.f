c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      INTEGERfunction krdchk(MN,KR,KC,NNR,KNR,KN,KNC,IR1,IR2)
C
C     מבניףבמב 12.05.91    ףÅÒÄÀË ח.ק.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER    KR(IR2),KC(IR1),NNR(IR1)
      INTEGER    MN(2,IR2)

      integer err,sum1,sum2,sum3

C     נעןקועכב נעוהףפבקלומיס עבתעוצוממשט םבפעיד הלס
C     טעבמומיס עותץלרפבפןק ה
C
C     ףניףןכ זןעםבלרמשט נבעבםופעןק - כבכ ק   KOORD
C
C     יהEHTיזיKATOPש נEPEMEHHשX י MACCיBOB :
C
C          KNC - הליHA OהHOMEPHOחO גנז;
C          NNR - MACCיB HOMEPOB HEHץלEBשX CTPOK;
C          KNR - KOלי‏ECTBO HEHץלEBשX CTPOK;
C          KR  - MACCיB HOMEPOB CTOלגדOB HEHץלEBשX
C                üלEMEHTOB B KAצהOך HEHץלEBOך CTPOKE;
C          KC  - MACCיB, COהEPצA‎יך KOלי‏ECTBO
C                HEHץלEBשX üלEMEHTOB B KAצהOך CTPOKE;

      err=0


c    ÐÒÏ×ÅÒËÁ ÞÉÓÌÁ ÓÔÒÏË:
c           /                  \
c   i=IR1  |  1 if NNR(i) != 0  |
c    SUM  <                      >  == KNR     ÅÓÌÉ ÎÅÔ, ÔÏ err=err+1
c    i=1   |  0 if NNR(i) == 0  |
c           \                  /
      sum1=0
      do 10 i=1,IR1
 10   if (NNR(i).ne.0) sum1=sum1+1

      if (sum1.ne.KNR) err=err+1

c     ÏÂÝÁÑ ÐÒÏ×ÅÒËÁ ÜÌÅÍÅÎÔÏ× × ÓÔÒÏËÁÈ
c                        /                 \
c    i=IR1       i=IR2  |  1 if KR(i) != 0  |
c    SUM KC(i) ==SUM   <                     > ÅÓÌÉ ÎÅÔ, ÔÏ err=err+2
c    i=1         i=1    |  0 if KR(i) == 0  |
c                        \                 /
      sum2=0
      do 20 i=1,IR1
 20   sum2=sum2+KC(i)

      sum3=0
      do 30 i=1,IR2
 30   if( kr(i).ne.0) sum3=sum3+1

      if (sum2.ne.sum3) err=err+2


c     ÐÒÏ×ÅÒËÁ ÕÐÏÒÑÄÏÞÅÎÉÑ × ÓÔÒÏËÁÈ ( ÍÏÖÅÔ ÎÅ ×ÙÐÏÌÎÑÔØÓÑ !!!)
c   for j=1 to KNR
c
c  1 + SUM {1}   =  KC(j)                       ÅÓÌÉ ÎÅÔ, ÔÏ err=err+4
c  KR(i+1) > KR(i)
c   KR(i) > 0
c
c  end_for
c                -------- !!!  ק מבףפןס‎וו קעוםס מו עובליתןקבמב !!!

      krdchk=err
      return
      end
