c
c Copyright (c) 1996-2004 by Gennady Serdyuk.  All rights reserved.
c gserdyuk@mail.ru
c 
c Released under GPL v 2.0
c




      SUBROUTINE MACHEP   (EPSIM)
C
C   �/� B���C�EH�� MA��HHO�O ��C��OH.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION EPSIM
C      print 5
C    5 format(2x,'�/� MACHEP')

      EPSIM=1.D0
   10 CONTINUE
      EPSIM=EPSIM/2.D0

C      print 15, epsim

C      IF((EPSIM+1.0).NE.1.) GOTO 10
           
C      EPSIM=2.*EPSIM
      EPSIM=.1084202172D-18 
   
C      print 15, epsim
C   15 format(2x,'EPSIM=',E16.10)

      RETURN
C     DEBUG INIT(EPSIM),SUBTRACE
      END
