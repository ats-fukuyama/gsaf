C     $Id$
C
C     ****** PLASMA DEMO ******
C
      CALL GSOPEN
      CALL DEF_IMAGE
C
      CALL INIT_PLASMA
C
    1 WRITE(6,*) '## INPUT : TYPE (0,1,2,9) ?'
      READ(5,*,ERR=1,END=9000) ID
C
      IF(ID.EQ.0) THEN
         CALL PLASMA0
      ELSEIF(ID.EQ.1) THEN
         CALL PLASMA1
      ELSEIF(ID.EQ.2) THEN
         CALL PLASMA2
      ELSEIF(ID.EQ.9) THEN
         GOTO 9000
      ENDIF
      GOTO 1
C
 9000 CALL GSCLOS
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
      NPMAX=100
C
      XMIN=0.0
      XMAX=1.5
      YMIN=0.0
      YMAX=1.0
C
      DELP=0.3
      MRSEED=1
C
      RETURN
      END
C
      SUBROUTINE PLASMA0
C
      INCLUDE 'plasma.inc'
C
      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C
      CALL PAGES
C      
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,11,11,IDATA1R)
      CALL GF_DEFIMAGE(1,11,11,IDATA1G)
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
      CALL PAGEE
C
 9000 CONTINUE
      RETURN
      END
C
      SUBROUTINE PLASMA1
C
      INCLUDE 'plasma.inc'
C
      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C
      CALL PAGES
C      
      CALL MOVE( 5.0, 5.0-DELP)
      CALL DRAW(20.0+DELP, 5.0-DELP)
      CALL DRAW(20.0+DELP,15.0)
      CALL DRAW( 5.0,15.0)
      CALL DRAW( 5.0, 5.0-DELP)
      CALL SETVEW(5.0,20.0,5.0,15.0,XMIN,XMAX,YMIN,YMAX)
C
      CALL GF_DEFIMAGE(0,41,41,IDATA4R)
      CALL GF_DEFIMAGE(1,41,41,IDATA4G)
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
      CALL PAGEE
C
 9000 CONTINUE
      END
C
      SUBROUTINE PLASMA2
C
      INCLUDE 'plasma.inc'
C
      WRITE(6,'(A,1PE12.4)') '  DT    = ',DT
      WRITE(6,'(A,1PE12.4)') '  VT    = ',VT
      WRITE(6,'(A,1PE12.4)') '  TINTV = ',TINTV
      WRITE(6,'(A,I12    )') '  NTMAX = ',NTMAX
      WRITE(6,'(A,I12    )') '  NPMAX = ',NPMAX
      WRITE(6,*) '## DT,VT,TINTV,NTMAX,NPMAX ?'
      READ(5,*,END=9000) DT,VT,TINTV,NTMAX,NPMAX
C
      XLEN=XMAX-XMIN
      YLEN=YMAX-YMIN
      CALL INIT_PLASMA_PARTICLES
C
      CALL PAGES
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
      CALL PAGEE
C
 9000 CONTINUE
      RETURN
      END
