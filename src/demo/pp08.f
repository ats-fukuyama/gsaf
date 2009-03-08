C     $Id$
C
      INCLUDE 'button.inc'
      INCLUDE 'plasma.inc'
      INTEGER ID,KID,KEY
      CHARACTER KIN*(1)
C
      CALL GSOPEN
      CALL GSTOFF
      CALL DEF_IMAGE
      CALL INIT_PLASMA
C
      CALL PAGES
      CALL DVINQRES(ISCRW,ISCRH)
C
      MODE=1
      IND=2
      NT=0
      NTMAX=0
C
      CALL BUTTON1
      CALL SET_BUTTON
C
      CALL GF_SET_EVENT(7)
C
   10 CONTINUE
C
         IF(IND.EQ.1) THEN
            CALL GF_CHECK_EVENT(ID,X,Y,KID,KEY)
         ELSE
            CALL GF_GET_EVENT(ID,X,Y,KID,KEY)
         ENDIF
C         IF(ID.NE.0) WRITE(6,'(I5,1P2E12.4,2I5)') ID,X,Y,KID,IND
         IF(ID.EQ.-1) THEN
            GOTO 9000
         ELSEIF(ID.EQ.0) THEN
            IF(IND.EQ.1) THEN
               NT=NT+1
               CALL LOOP(MODE,1)
               IF(NT.GT.NTMAX) THEN
                  CALL LOOP(MODE,2)
                  IND=2
               ENDIF
            ENDIF
         ELSEIF(ID.EQ.1) THEN
            CALL ASCCHR(KID,KIN,1)
            CALL GUCPTL(KIN)
C            WRITE(6,*) KIN
            IF(KIN.EQ.'Q') GOTO 9000
         ELSEIF(ID.EQ.2) THEN
            DO NB=1,NBMAX
               IF(BUTTON_CLICK(NB).EQ.1) THEN
                  IF((X.GT.BUTTON_XMIN(NB)).AND.
     &               (X.LT.BUTTON_XMAX(NB)).AND.
     &               (Y.GT.BUTTON_YMIN(NB)).AND.
     &               (Y.LT.BUTTON_YMAX(NB))) THEN
                     BUTTON_CLICK(NB)=0
                     CALL REVERSE_BUTTON(NB)
                     IF(NB.EQ.16) GOTO 9000
                     IF(NB.EQ.15) THEN
                        IF(IND.EQ.0) THEN
                           IND=1
                        ELSEIF(IND.EQ.1) THEN
                           IND=0
                        ENDIF
                        GOTO 10
                     ENDIF
                     IF(IND.EQ.0.OR.IND.EQ.1) THEN
                        CALL LOOP(MODE,2)
                        IND=2
                     ENDIF
                     MODE=NB
                     CALL ERAS
                     CALL SET_BUTTON
                     IF(IND.EQ.2) THEN
                        CALL LOOP(MODE,0)
                        IND=1
                        NT=0
                     ENDIF
                     GOTO 10
                  ELSE
                     BUTTON_CLICK(NB)=0
                     CALL REVERSE_BUTTON(NB)
                  ENDIF
               ENDIF
            ENDDO
         ELSEIF(ID.EQ.4) THEN
            DO NB=1,NBMAX
               IF((X.GT.BUTTON_XMIN(NB)).AND.
     &            (X.LT.BUTTON_XMAX(NB)).AND.
     &            (Y.GT.BUTTON_YMIN(NB)).AND.
     &            (Y.LT.BUTTON_YMAX(NB))) THEN
                  BUTTON_CLICK(NB)=1
                  CALL REVERSE_BUTTON(NB)
                  GOTO 10
               ENDIF
            ENDDO
         ENDIF
      GOTO 10
C
 9000 IF(IND.EQ.1) THEN
         CALL LOOP(MODE,2)
         IND=2
      ENDIF
      CALL GF_SET_EVENT(0)
      CALL PAGEY
C
      CALL GSCLOS
      STOP
      END
C
      SUBROUTINE LOOP(MODE,IND)
C
      INCLUDE 'plasma.inc'
C
      IF(MODE.EQ. 1) CALL cyclotron(IND)
      IF(MODE.EQ. 2) CALL mass1836(IND)
      IF(MODE.EQ. 3) CALL diamagnetic(IND)
      IF(MODE.EQ. 4) CALL ExB(IND)
C      IF(MODE.EQ. 5) CALL ExBei(IND)
C      IF(MODE.EQ. 6) CALL gravity(IND)
C      IF(MODE.EQ. 7) CALL cetrifugal(IND)
C      IF(MODE.EQ. 8) CALL sub(IND)
C      IF(MODE.EQ. 9) CALL sub(IND)
C      IF(MODE.EQ.10) CALL sub(IND)
C      IF(MODE.EQ.11) CALL sub(IND)
C      IF(MODE.EQ.12) CALL sub(IND)
C      IF(MODE.EQ.13) CALL sub(IND)
C      IF(MODE.EQ.14) CALL sub(IND)
C
      RETURN
      END
C
      SUBROUTINE INIT_PLASMA
C
      INCLUDE 'plasma.inc'
C
      DT=0.1
      VT=0.3
      RMASS=10.0
C
      TINTV=0.05
C
      NTMAX=100
      NPMAX=50
C
      XMIN=0.0
      XMAX=1.5
      YMIN=0.0
      YMAX=1.0
C
      MRSEED=0
      FE=0.0
      FB=0.0
      FG=0.0
      FEX=0.0
      XLE=0.20
C
      RETURN
      END
C
      SUBROUTINE cyclotron(IND)
C
      INCLUDE 'plasma.inc'
C
      IF(IND.EQ.0) THEN
         DT=0.1
         FE=0.0
         FB=5.0
         NTMAX=200
         NPMAX=2
C
         CALL MOVE( 5.0,15.5)
         CALL TEXTX('@B=1, E=0@')
C
         XLEN=XMAX-XMIN
         YLEN=YMAX-YMIN
         CALL INIT_PLASMA_PARTICLESF(1)
C      
         CALL MOVE( 5.0, 5.0)
         CALL DRAW(20.0, 5.0)
         CALL DRAW(20.0,15.0)
         CALL DRAW( 5.0,15.0)
         CALL DRAW( 5.0, 5.0)
C
         CALL GF_DEFIMAGE(0,11,11,IDATA1B)
         CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL INITDRAW_PARTICLES
         CALL OFFVEW
C
      ELSEIF(IND.EQ.1) THEN
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL PUSH_PLASMA_PARTICLESX
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
         CALL OFFVEW
C
      ELSEIF(IND.EQ.2) THEN
         CALL GF_UNDEFIMAGE(0)
         CALL GF_UNDEFIMAGE(1)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE mass1836(IND)
C
      INCLUDE 'plasma.inc'
C
      RMASS_SAVE=RMASS
      RMASS=1836.0
      IF(IND.EQ.0) THEN
         DT=0.1
         FE=0.0
         FB=15.0
         NTMAX=200
         NPMAX=2
C
         CALL MOVE( 5.0,15.5)
         CALL TEXTX('@B=1, E=0@')
C
         XLEN=XMAX-XMIN
         YLEN=YMAX-YMIN
         CALL INIT_PLASMA_PARTICLESF(1)
C      
         CALL MOVE( 5.0, 5.0)
         CALL DRAW(20.0, 5.0)
         CALL DRAW(20.0,15.0)
         CALL DRAW( 5.0,15.0)
         CALL DRAW( 5.0, 5.0)
C
         CALL GF_DEFIMAGE(0,11,11,IDATA1B)
         CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL INITDRAW_PARTICLES
         CALL OFFVEW
C
      ELSEIF(IND.EQ.1) THEN
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL PUSH_PLASMA_PARTICLESX
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
         CALL OFFVEW
C
      ELSEIF(IND.EQ.2) THEN
         CALL GF_UNDEFIMAGE(0)
         CALL GF_UNDEFIMAGE(1)
      ENDIF
      RMASS=RMASS_SAVE
C
      RETURN
      END
C
      SUBROUTINE diamagnetic(IND)
C
      INCLUDE 'plasma.inc'
C
      IF(IND.EQ.0) THEN
         DT=0.1
         FE=0.0
         FB=5.0
         NTMAX=200
         NPMAX=10
C
         CALL MOVE( 5.0,15.5)
         CALL TEXTX('@B=1, E=0@')
C
         XLEN=XMAX-XMIN
         YLEN=YMAX-YMIN
         CALL INIT_PLASMA_PARTICLESF(2)
C      
         CALL MOVE( 5.0, 5.0)
         CALL DRAW(20.0, 5.0)
         CALL DRAW(20.0,15.0)
         CALL DRAW( 5.0,15.0)
         CALL DRAW( 5.0, 5.0)
C
         CALL GF_DEFIMAGE(0,11,11,IDATA1B)
         CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL INITDRAW_PARTICLES
         CALL OFFVEW
C
      ELSEIF(IND.EQ.1) THEN
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL PUSH_PLASMA_PARTICLESX
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
         CALL OFFVEW
C
      ELSEIF(IND.EQ.2) THEN
         CALL GF_UNDEFIMAGE(0)
         CALL GF_UNDEFIMAGE(1)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE ExB(IND)
C
      INCLUDE 'plasma.inc'
C
      IF(IND.EQ.0) THEN
         DT=0.1
         FE=0.0
         FB=5.0
         NTMAX=200
         NPMAX=2
C
         CALL MOVE( 5.0,15.5)
         CALL TEXTX('@B=1, E=0@')
C
         XLEN=XMAX-XMIN
         YLEN=YMAX-YMIN
         CALL INIT_PLASMA_PARTICLESF(1)
C      
         CALL MOVE( 5.0, 5.0)
         CALL DRAW(20.0, 5.0)
         CALL DRAW(20.0,15.0)
         CALL DRAW( 5.0,15.0)
         CALL DRAW( 5.0, 5.0)
C
         CALL GF_DEFIMAGE(0,11,11,IDATA1B)
         CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL INITDRAW_PARTICLES
         CALL OFFVEW
C
      ELSEIF(IND.EQ.1) THEN
         CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
         CALL PUSH_PLASMA_PARTICLESX
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
         CALL OFFVEW
C
      ELSEIF(IND.EQ.2) THEN
         CALL GF_UNDEFIMAGE(0)
         CALL GF_UNDEFIMAGE(1)
      ENDIF
C
      RETURN
      END
