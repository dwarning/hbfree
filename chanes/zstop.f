c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE STOP(N,X,DX,F,FNOR,G,SX,SF,IRETCD,ITER,MAXTKN,         
     +       KMAXDU,TERMCD)
C
C   נ/נ OנPEהEלEHיס נPי‏יHש OCTAHOBA.
C     * TERMCD=0 - HET OCTAHOBA;
C       ...

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/PRINT /   KPRLEN,KPRSRT,KPRNKR,KPRLIN,KPRSOL,KPRVAR,       
     +          KPRGRF,KPRQUP
      COMMON/NEWTON/   EPSSOL,EPSDU,EPSMIN,MAXDU,LIMIT
      DOUBLE PRECISION             EPSSOL,EPSDU,EPSMIN,MAXDU
      INTEGER          TERMCD
      LOGICAL          MAXTKN
      DOUBLE PRECISION             X(1),DX(1),F(1),G(1)
      DOUBLE PRECISION             SX(1),SF(1)

      TERMCD=0

      IF(IRETCD.NE.1)GOTO 10
C  B LSERCH HAM HEץהAלOCר CהEלATר ץהOBלETBOPיTEליHשך
C        ûA
      TERMCD=3
      IF(KPRSOL.GT.0) WRITE(6, 103) ITER,EPSDU,FNOR
                      PRINT    103, ITER,EPSDU,FNOR
      RETURN
  10  CONTINUE

C  MAX.MACûTAגיPOBAHHשך ‏לEH HEBסתKי:
      CONV=0.D0
      DO 20 I=1,N
  20  CONV=DMAX1(CONV,SF((I+1)/2)*DABS(F(I)))
      IF(CONV.GT.EPSSOL) GOTO 30
C  Mש יMEEM נPיגליתיTEלרHOE PEûEHיE
C   ( ECלי EPSSOL HE O‏EHר BEליKO)
      TERMCD=1
      IF(KPRSOL.GT.0) WRITE(6, 101) ITER,CONV,EPSSOL,FNOR
                      PRINT    101, ITER,CONV,EPSSOL,FNOR
      RETURN
  30  CONTINUE

C  MAX. MACûTAגיPOBAHHשך ‏לEH נOנPABKי
      DUNOR=0.D0
      DO 40 I=1,N
      SINV=1/SX((I+1)/2)
  40  DUNOR=DMAX1(DUNOR,DABS(DX(I)/DMAX1(DABS(X(I)),SINV)))
      IF(DUNOR.GT.EPSDU) GOTO 50
C  נOנPABKA MEHרûE MיH. הOנץCTיMOך.
      TERMCD=2
      IF(KPRSOL.GT.0) WRITE(6, 102) ITER,DUNOR,EPSDU,CONV,FNOR
                      PRINT    102, ITER,DUNOR,EPSDU,CONV,FNOR
      RETURN
  50  CONTINUE

      IF(ITER.LT.LIMIT)GOTO 60
C  יC‏EPנAH ליMיT יTEPAדיך
      TERMCD=4
      IF(KPRSOL.GT.0) WRITE(6, 104) ITER,CONV,EPSSOL,DUNOR,EPSDU
                      PRINT    104, ITER,CONV,EPSSOL,DUNOR,EPSDU
      RETURN
  60  CONTINUE

      IF(.NOT.MAXTKN)GOTO 70
C  גשל CהEלAH ûAח הליHOך MAXDU
      KMAXDU=KMAXDU+1
      IF(KMAXDU.LT.5)GOTO 70
C  גשלO CהEלAHO 5 ûAחOB הליHOך MAXDU
      TERMCD=5
      IF(KPRSOL.GT.0) WRITE(6, 105) ITER,MAXDU,CONV,EPSSOL
                      PRINT    105, ITER,MAXDU,CONV,EPSSOL
      RETURN
  70  CONTINUE

      KMAXDU=0
C   OנPEהEלEHיE OTHOCיTEלרHOך CKOPOCTי ץMEHרûEHיס  FNOR
      TERM=0.D0
      DO 80 I=1,N
      SINV=1/SX((I+1)/2)
      TERM=DMAX1(TERM,DABS(G(I))*DMAX1(X(I)      ,SINV)/                
     + DMAX1(FNOR,DFLOAT(N)/2.D0))
C     ץהAלEH PRINT 222 C COOTB. FORMATOM 8.12.89. KOûMAHOBA H.B.  *****
  80  CONTINUE
      IF(TERM.GT.EPSMIN)GOTO 90
C  יTAK,Mש HAXOהיMCס B לOKAלרHOM MיHיMץME FNOR.
      TERMCD=6
      IF(KPRSOL.GT.0) WRITE(6, 106) ITER,TERM,EPSMIN,FNOR,CONV,EPSSOL
                      PRINT    106, ITER,TERM,EPSMIN,FNOR,CONV,EPSSOL
      RETURN
  90  CONTINUE
C   Hי‏EחO HE נPOיתOûלO
      IF(KPRSOL.GE.2) WRITE(6, 107) CONV,DUNOR,TERM
                      PRINT    107, CONV,DUNOR,TERM
      RETURN
 101  FORMAT('     AT ',I5,' -TH ITERATION CONVERGED TO '/       '  SOLU
     +TION WITH ERROR <= ',E12.6 ,' ( < ',E12.6,' )'/       '  1/2 OF SQ
     +UARED L-2 NORM OF ERROR =',E12.6 )
 102  FORMAT('     AT ',I5,' -T ITERATION MAX STEP=',E12.6,' ( <',E12.6,
     +')'/       '  ERROR =',E12.6/       '  1/2 OF SQUARED L-2 NORM OF 
     +ERROR =',E12.6)
 103  FORMAT('     AT ',I5,' -TH ITERATION CAN NOT MAKE GOOD STEP'/     
     +  '  > ',E12.6/       '  1/2 OF SQUARED L-2 NORM OF ERROR =',E12.6
     +)
 104  FORMAT('     ITERATION LIMIT:',I4,' IS REACHED.'/       '  ERROR =
     +',E12.6,'( > ',E12.6,' );'/       '  STEP =',E12.6,'( > ',E12.6,' 
     +).')
 105  FORMAT('     AT ',I4,' ITERATION , IT IS MADE'/       '  5 STEPS O
     +F LENGTH ',E12.6/       '  ERROR =',E12.6,' ( > ',E12.6,' ).')
 106  FORMAT('  IT IS LOCAL MINIMUM :  '/       '              ITERATION
     +                        ',I4/       '              GRADIENT OF ERR
     +OR NORM        ',E12.6,                                           
     +    '(<',E12.6 ,')'/       '              1/2 SQUARED  L-2 ERROR N
     +ORM   ',E12.6/       '               ERROR                        
     +',E12.6/       '               REQUIRED                     ',E12.
     +6)

 107  FORMAT(' ERROR  =',E12.6,', STEP=',E12.6,', SPEED OF DECREASING=' 
     +      , E12.6)



C     DEBUG SUBTRACE,INIT
      END
