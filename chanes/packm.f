c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c






      SUBROUTINE PACK1(NREC,Y,VJ,ISIZE_MAXNODE)
C
C     this routhine just stores Y(part) and VJ arrays and allows to get 
C     them when necesary. most probably it should be rewritten - but 
C     now I left it as is (just simplify it a bit)
C     if stored via PACK1 - should be get via DPACK1 - same for PACK2/DPACK2
C     
C***********************************************************************
C
C
C    AAAAA  !!     BO�MO�HO �BE���EH�E YEL � NOY �O 900 HO TO��A B XO�E
C   A    A  !!     I/O ���ET �PO�CXO��T� O�MEH 2-M� �A��C�M�
C  A     A  !!     !!!-���HA �A��C�<=���HE �OPO�K�=7040 �A�T
C  AAAAAAA  !!
C  A     A         ...'NREC...=>...'2*NREC-1...
C  A     A  !!
C
C
C        O�EPA��� VY=CABS... � VJR=CABS... MO�HO �CK����T
C     O�'�B�B : EQUIVALENCE((Y(1,1),RY(1,1,1)),(VJ(1),RJR(1,1))
C               REAL RY(2,100,100),RJR(2,100)
C
C    � B TEKCTE : IF(RJR(1,I).EQ.0.0.AND.RJR(2,I).EQ.0.0) GO TO 10
C             IF(RY(1,II,I).EQ.0.0.AND.RY(2,II,I).EQ.0.0) GO TO 20
C
c$LARGE: VJ,Y
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      INTEGER ISIZE_MAXNODE
      DOUBLE COMPLEX Y(ISIZE_MAXNODE,ISIZE_MAXNODE)
      DOUBLE COMPLEX VJ(ISIZE_MAXNODE)

      COMMON/ELPACK/  KPKY(2,20)
      COMMON/SERV/    EPSIW,LIMERR,KITU
      COMMON/KOLNAL/  KOL,NAL
      DOUBLE COMPLEX         YEL(762)
      INTEGER         KOL(4),ENTRY,ASS1,ASS2,NOVJ(5),NOY(2,762),KERR/0/
c$LARGE: RY
      LOGICAL         NAL(4)
      DATA            KYM/762/
      INTEGER INDMAX

C      OPEN ( 1, FILE=  'TMP1_XX',
C    *          STATUS='NEW',
C     *          ACCESS='DIRECT',
C     *          RECL=   12992,
C    *          CHARE= 'COMPAT',
C     *          MODE=  'READWRITE'
C     *    )
C
C     DEFINE FILE 1(20,3248,U,ASS1)
C
C     OPEN ( 2, FILE=  'TMP2_XX',
C    *          STATUS='NEW',
C     *          ACCESS='DIRECT',
C     *          RECL=   12992,
C    *          CHARE= 'COMPAT',
C     *          MODE=  'READWRITE'
C     *   )
C
C     DEFINE FILE 2(20,3248,U,ASS2)

      ENTRY=1
      IF=KOL(1)+1
      IE=KOL(1)+KOL(2)+KOL(3)
      GO TO 50
C      write (6,*) 'Y matrix at ENTRY =1 '

C*********************************************************************
      ENTRY PACK2(NREC,Y,VJ,ISIZE_MAXNODE)
C
C     BXO� ��� ��AKOBK� � �A��C� MATP��� Y,BEKTOPA J �E� ��AKOBK�
      ENTRY=2
      IF=KOL(1)+KOL(2)+1
      IE=KOL(1)+KOL(2)+KOL(3)

C      write (6,*) 'Y matrix (ENTRY 2)'
C      do ii=1,IE
C            write (6,1220) (Y(ii,jj), jj=1,IE)
C      enddo      
C1220   format (2x,'(',1x,e12.5,1x,e12.5,')')



   50 K=1
      DO 20 I=1,IE
      JJ3=I
      IF(JJ3.GE.IF )JJ3=IE
      DO 20 II=1,JJ3
C     VY=CABS(Y(II,I))
C     IF(VY     .EQ.0.0 ) GO TO 20
      IF(DBLE(Y(II,I)).EQ.0.0D0.AND.DIMAG(Y(II,I)).EQ.0.0D0)GO TO 20
      IF(K.GT.KYM) GO TO 110
      YEL(K)=Y(II,I)
      NOY(1,K)=II
      NOY(2,K)=I
      K=K+1
   20 CONTINUE
   
      KPKY(ENTRY,NREC)=K-1
C      print *,'WR: ENTRY, NREC, KPKY =',ENTRY, NREC, KPKY
      
C     CO�CTBEHHO �A��C�

C      print *,'YEL, NOY, NOY: WRITING'
C      DO I=1,50
C        print *,I, YEL(I), NOY(1,I), NOY(2,I)
C      ENDDO


      IF(ENTRY.EQ.1) CALL SAVEDATA(1,NREC,YEL,NOY,VJ,ISIZE_MAXNODE,KYM)

C      WRITE(1,REC=NREC) YEL, NOY, VJ
C     IF(ENTRY.EQ.1) WRITE(1'NREC)YEL,NOY,VJ  !! REC=20 !!

      IF(ENTRY.EQ.2) CALL SAVEDATA(2,NREC,YEL,NOY,VJ,ISIZE_MAXNODE,KYM)
C      WRITE(2,REC=NREC) YEL, NOY, VJ
C     IF(ENTRY.EQ.2) WRITE(2'NREC)YEL,NOY,VJ  !! REC=20 !!

      RETURN

C********************************************************************
      ENTRY DPACK1(NREC,Y,VJ,ISIZE_MAXNODE)
C
C     BXO� ��� �TEH�� � PAC�AKOBK� VJ & Y
C
      ENTRY=1
  121 CONTINUE    
C     READ(1,REC=NREC,ERR=120) YEL,NOY,VJ
C 121 READ(1'NREC,ERR=120)YEL,NOY,VJ
      CALL GETDATA(1,NREC,YEL,NOY,VJ,ISIZE_MAXNODE,KYM)

      GO TO 70

C**********************************************************************
      ENTRY DPACK2(NREC,Y,VJ,ISIZE_MAXNODE)
C
C     �TEH�E Y � PAC�AKOBKA
C
      ENTRY=2
C      print *, 'DPACK2: NREC: ', NREC
  131 CONTINUE
C     READ(2,REC=NREC,ERR=130) YEL,NOY,VJ
C 131 READ(2'NREC,ERR=130)YEL,NOY,VJ
      CALL GETDATA(2,NREC,YEL,NOY,VJ,ISIZE_MAXNODE,KYM)

C      print *,'YEL, NOY, NOY: READING'
C      DO I=1,50
C        print *,I, YEL(I), NOY(1,I), NOY(2,I)
C      ENDDO


   70 KL=KPKY(ENTRY,NREC)
C      print *,'READ: ENTRY, NREC, KPKY =',ENTRY, NREC, KPKY

      DO 80 K=1,KL
   80 Y(NOY(1,K),NOY(2,K))=YEL(K)

C      write (6,*) 'Y matrix (UNPACKED)'
C      do ii=1,4
C            write (6,1220) (Y(ii,jj), jj=1,4)
C      enddo      

      RETURN

C
  110 WRITE(6, 111) NREC,KYM
      STOP

C
  120 KERR=KERR+1
      IF(KERR.LT.LIMERR) GO TO 121
      WRITE(6, 122) KERR,ENTRY
      STOP

C
  130 KERR=KERR+1
      IF(KERR.LT.LIMERR) GO TO 131
      WRITE(6, 122) KERR,ENTRY
      STOP
  111 FORMAT (10X,'MATRIX  Y AT FREQUENCY ',I4,'CONTAINS'/10X,
     +            'MORE THAN', I4,'ELEMENTS ')
  122 FORMAT (10X,I2,'-TH FILE IO ERROR',I2)
C     DEBUG SUBTRACE,INIT(NREC,YEL,NOY,VJ,IF,IE,ENTRY,K)
      END



      SUBROUTINE SAVEDATA(IR,NREC,YEL,NOY,VJ,SIZE_VJ, SIZE_YEL)
C TO SAVE DATA IN FILE. NOW - EMULATE BY STATIC ARRAYS TO SIMPLIFY      
c      
C IR - FIRST OR SECOND SET OF MATRICES AFTER FISRT OF SECOND REDUCTION
C NREC- NUMBER OF FREQUENCY - SO FAR 1-20
C YEL - ELEMENTS OF Y -MATRIX
C VJ - VECTOR OF FREE SOURCES
C NOI - POSITIONS OF ELEMENTS IN Y-MATRIX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IR,NREC, SIZE_VJ,SIZE_YEL
      DOUBLE COMPLEX VJ(SIZE_VJ)
      DOUBLE COMPLEX YEL(SIZE_YEL)
      INTEGER NOY(2,SIZE_YEL)
      
      
      DOUBLE COMPLEX YEL_SAVE(2,20,1000)
      DOUBLE COMPLEX VJ_SAVE(2,20,200)
      INTEGER NOY_SAVE(2,20,2000,2)
      COMMON /SAVEDATA_CB/ YEL_SAVE,VJ_SAVE,NOY_SAVE
      
      DO I=1,SIZE_VJ
        VJ_SAVE(IR,NREC,I)=VJ(I)
      ENDDO
      
      DO I=1,SIZE_YEL
        YEL_SAVE(IR,NREC,I)=YEL(I)
        NOY_SAVE(IR,NREC,I,1)=NOY(1,I)
        NOY_SAVE(IR,NREC,I,2)=NOY(2,I)
      ENDDO

C      print *,'SAVE::: YEL, NOY, NOY'
C      DO I=1,50
C       print *,I, YEL_SAVE(IR,NREC,I), NOY_SAVE(IR,NREC,I,1),
C     +            NOY_SAVE(IR,NREC,I,2)
c      ENDDO


      
      RETURN 
      END
      
      SUBROUTINE GETDATA(IR,NREC,YEL,NOY,VJ,SIZE_VJ, SIZE_YEL)
C TO GET DATA IN FROM FILE. NOW - EMULATE BY STATIC ARRAYS TO SIMPLIFY  
c          
C IR - FIRST OR SECOND SET OF MATRICES AFTER FISRT OF SECOND REDUCTION
C NREC- NUMBER OF FREQUENCY - SO FAR 1-20
C YEL - ELEMENTS OF Y -MATRIX
C VJ - VECTOR OF FREE SOURCES
C NOI - POSITIONS OF ELEMENTS IN Y-MATRIX
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER IR,NREC, SIZE_VJ,SIZE_YEL
      DOUBLE COMPLEX VJ(SIZE_VJ)
      DOUBLE COMPLEX YEL(SIZE_YEL)
      INTEGER NOY(2,SIZE_YEL)
      
      
      DOUBLE COMPLEX YEL_SAVE(2,20,1000)
      DOUBLE COMPLEX VJ_SAVE(2,20,200)
      INTEGER NOY_SAVE(2,20,2000,2)
      COMMON /SAVEDATA_CB/ YEL_SAVE,VJ_SAVE,NOY_SAVE
      
      DO I=1,SIZE_VJ
        VJ(I)=VJ_SAVE(IR,NREC,I)
      ENDDO
      
      DO I=1,SIZE_YEL
        YEL(I)=YEL_SAVE(IR,NREC,I)
        NOY(1,I)=NOY_SAVE(IR,NREC,I,1)
        NOY(2,I)=NOY_SAVE(IR,NREC,I,2)
      ENDDO
      
      RETURN 
      END
