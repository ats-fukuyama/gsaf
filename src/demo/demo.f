C     $Id$
C
      INCLUDE 'button.inc'
      INTEGER IW,IH,ID,KID,KEY
      CHARACTER KIN*(1)
C
      CALL GSOPEN
      CALL GSTOFF
      CALL DEF_IMAGE
      CALL INIT_PLASMA
C
      CALL PAGES
C      CALL DVINQRES(IW,IH)
C      WRITE(6,*) IW,IH
C
      MODE=1
C
      CALL BUTTON1
      CALL SET_BUTTON
C
      CALL GF_SET_EVENT(7)
C
   10 CONTINUE
C
         CALL GF_GET_EVENT(ID,X,Y,KID,KEY)
C         WRITE(6,'(I5,1P2E12.4,I5)') ID,X,Y,KID
         IF(ID.EQ.-1) THEN
            GOTO 9000
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
                     MODE=NB
                     CALL ERAS
                     CALL SET_BUTTON
C                     IF(MODE.EQ.1) CALL SOLID
C                     IF(MODE.EQ.2) CALL LIQUID
                     IF(MODE.EQ.3) CALL GAS
C                     IF(MODE.EQ.4) CALL XATOM
C                     IF(MODE.EQ.5) CALL XION
                     IF(MODE.EQ.6) CALL PLASMA1
                     IF(MODE.EQ.7) CALL PLASMA2
                     IF(MODE.EQ.8) CALL MAGNETIZE1
                     IF(MODE.EQ.9) CALL MAGNETIZE2
                     IF(MODE.EQ.10) GOTO 9000
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
 9000 CALL GF_SET_EVENT(0)
      CALL PAGEY
C
      CALL GSCLOS
      STOP
      END
C
      SUBROUTINE INIT_PLASMA
C
      INCLUDE 'plasma.inc'
C
      DT=0.1
      VT=0.3
C
      TINTV=0.05
C
      NTMAX=100
C      NPMAX=100
      NPMAX=50
C
      XMIN=0.0
      XMAX=1.5
      YMIN=0.0
      YMAX=1.0
C
      DELP=0.3
      MRSEED=0
      FE=1.0
      FB=0.0
      FG=0.0
C
      RETURN
      END
C
      SUBROUTINE GAS
C
      INCLUDE 'plasma.inc'
C
C      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
C      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
C      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
C      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
C      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
C      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
C      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      DT=0.05
      FG=3.0
      NTMAX=200
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_GAS_PARTICLES
C
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,11,11,IDATA1G)
C
      CALL INITDRAW_PARTICLES
C
      DO NT=1,NTMAX
         CALL PUSH_GAS_PARTICLES
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
      ENDDO
      CALL GF_UNDEFIMAGE(0)
      CALL OFFVEW
C
C 9000 CONTINUE
      RETURN
      END
C
      SUBROUTINE PLASMA1
C
      INCLUDE 'plasma.inc'
C
C      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
C      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
C      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
C      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
C      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
C      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
C      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      DT=0.002
      FE=3.0
      FB=0.0
      NTMAX=200
      NPMAX=20
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C      
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,11,11,IDATA1B)
      CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
      CALL INITDRAW_PARTICLES
C
      DO NT=1,NTMAX
         CALL PUSH_PLASMA_PARTICLES
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
      ENDDO
      CALL GF_UNDEFIMAGE(0)
      CALL GF_UNDEFIMAGE(1)
      CALL OFFVEW
C
C 9000 CONTINUE
      RETURN
      END
C
      SUBROUTINE PLASMA2
C
      INCLUDE 'plasma.inc'
C
C      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
C      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
C      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
C      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
C      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
C      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
C      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      DT=0.002
      FE=9.0
      FB=0.0
      NTMAX=300
      NPMAX=100
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C      
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,11,11,IDATA1B)
      CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
      CALL INITDRAW_PARTICLES
C
      DO NT=1,NTMAX
         CALL PUSH_PLASMA_PARTICLES
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
      ENDDO
      CALL GF_UNDEFIMAGE(0)
      CALL GF_UNDEFIMAGE(1)
      CALL OFFVEW
C
C 9000 CONTINUE
      RETURN
      END
C
      SUBROUTINE MAGNETIZE1
C
      INCLUDE 'plasma.inc'
C
C      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
C      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
C      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
C      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
C      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
C      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
C      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      DT=0.1
      FE=0.0
      FB=2.0
      NTMAX=200
      NPMAX=2
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C      
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,11,11,IDATA1B)
      CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
      CALL INITDRAW_PARTICLES
C
      DO NT=1,NTMAX
         CALL PUSH_PLASMA_PARTICLES
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
      ENDDO
      CALL GF_UNDEFIMAGE(0)
      CALL GF_UNDEFIMAGE(1)
      CALL OFFVEW
C
C 9000 CONTINUE
      RETURN
      END
C
      SUBROUTINE MAGNETIZE2
C
      INCLUDE 'plasma.inc'
C
C      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
C      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
C      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
C      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
C      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
C      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
C      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      DT=0.1
      FE=0.1
      FB=2.0
      NTMAX=200
      NPMAX=50
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C      
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,11,11,IDATA1B)
      CALL GF_DEFIMAGE(1,21,21,IDATA2R)
C
      CALL INITDRAW_PARTICLES
C
      DO NT=1,NTMAX
         CALL PUSH_PLASMA_PARTICLES
         CALL DRAW_PARTICLES
         CALL GU_SLEEP(TINTV)
      ENDDO
      CALL GF_UNDEFIMAGE(0)
      CALL GF_UNDEFIMAGE(1)
      CALL OFFVEW
C
C 9000 CONTINUE
      RETURN
      END
