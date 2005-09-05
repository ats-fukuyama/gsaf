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
      SUBROUTINE DEF_IMAGE
C
      INCLUDE 'plasma.inc'
C
      DO IX=1,11
      DO IY=1,11
         X=0.2*(IX-6)
         Y=0.2*(IY-6)
         R1=SQRT(X**2+Y**2)
         R2=SQRT((X+0.3)**2+(Y+0.3)**2)
C
         IF(R2.LT.0.5) THEN
            IB=255-NINT(255*4*R2*R2)
            IR=255
         ELSE
            IR=255-NINT(255*(R2-0.5)**2)
            IF(IR.LE.0) IR=0
            IB=0
         ENDIF
C         
         IF(R1.GE.1.0) THEN
            IR=255
            IB=255
         ELSEIF(R1.GE.0.9) THEN
            IR=IR+(255-IR)*(R1-0.9)/0.1
            IB=IB+(255-IB)*(R1-0.9)/0.1
         ENDIF
C
         IDATA1R(IX,IY)=IR*256*256+IB*256+IB
         IDATA1G(IX,IY)=IB*256*256+IR*256+IB
         IDATA1B(IX,IY)=IB*256*256+IB*256+IR
      ENDDO
      ENDDO
C
      DO IX=1,21
      DO IY=1,21
         X=0.1*(IX-11)
         Y=0.1*(IY-11)
         R1=SQRT(X**2+Y**2)
         R2=SQRT((X+0.3)**2+(Y+0.3)**2)
C
         IF(R2.LT.0.5) THEN
            IB=255-NINT(255*4*R2*R2)
            IR=255
         ELSE
            IR=255-NINT(255*(R2-0.5)**2)
            IF(IR.LE.0) IR=0
            IB=0
         ENDIF
C         
         IF(R1.GE.1.0) THEN
            IR=255
            IB=255
         ELSEIF(R1.GE.0.9) THEN
            IR=IR+(255-IR)*(R1-0.9)/0.1
            IB=IB+(255-IB)*(R1-0.9)/0.1
         ENDIF
C
         IDATA2R(IX,IY)=IR*256*256+IB*256+IB
         IDATA2G(IX,IY)=IB*256*256+IR*256+IB
         IDATA2B(IX,IY)=IB*256*256+IB*256+IR
      ENDDO
      ENDDO
C
      DO IX=1,41
      DO IY=1,41
         X=0.05*(IX-21)
         Y=0.05*(IY-21)
         R1=SQRT(X**2+Y**2)
         R2=SQRT((X+0.3)**2+(Y+0.3)**2)
C
         IF(R2.LT.0.5) THEN
            IB=255-NINT(255*4*R2*R2)
            IR=255
         ELSE
            IR=255-NINT(255*(R2-0.5)**2)
            IF(IR.LE.0) IR=0
            IB=0
         ENDIF
C         
         IF(R1.GE.1.0) THEN
            IR=255
            IB=255
         ELSEIF(R1.GE.0.9) THEN
            IR=IR+(255-IR)*(R1-0.9)/0.1
            IB=IB+(255-IB)*(R1-0.9)/0.1
         ENDIF
C
         IDATA4R(IX,IY)=IR*256*256+IB*256+IB
         IDATA4G(IX,IY)=IB*256*256+IR*256+IB
         IDATA4B(IX,IY)=IB*256*256+IB*256+IR
      ENDDO
      ENDDO
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
      SUBROUTINE INITDRAW_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      CALL GF_SET_GCFUNC(6)
      DO NP=1,NPMAX
         CALL GF_PUTIMAGE(IPD(NP),PX(1,NP),PX(2,NP))
      ENDDO
      CALL GF_SET_GCFUNC(3)
      CALL GU_XFLUSH
C
      RETURN
      END
C
      SUBROUTINE DRAW_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      CALL GF_SET_GCFUNC(6)
      DO NP=1,NPMAX
         CALL GF_PUTIMAGE(IPD(NP),PX(1,NP),PX(2,NP))
         PX(1,NP)=PXN(1,NP)
         PX(2,NP)=PXN(2,NP)
         PV(1,NP)=PVN(1,NP)
         PV(2,NP)=PVN(2,NP)
         CALL GF_PUTIMAGE(IPD(NP),PX(1,NP),PX(2,NP))
      ENDDO
      CALL GF_SET_GCFUNC(3)
      CALL GU_XFLUSH
C
      RETURN
      END
C
      SUBROUTINE PUSH_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      DO NP=1,NPMAX
         PVN(1,NP)=PV(1,NP)
         PVN(2,NP)=PV(2,NP)
         PXN(1,NP)=PX(1,NP)+PVN(1,NP)*DT
         PXN(2,NP)=PX(2,NP)+PVN(2,NP)*DT
         IF(PXN(1,NP).LT.XMIN) THEN
            PXN(1,NP)=2*XMIN-PXN(1,NP)
            PVN(1,NP)=-PVN(1,NP)
         ELSEIF(PXN(1,NP).GT.XMAX) THEN
            PXN(1,NP)=2*XMAX-PXN(1,NP)
            PVN(1,NP)=-PVN(1,NP)
         ENDIF
         IF(PXN(2,NP).LT.YMIN) THEN
            PXN(2,NP)=2*YMIN-PXN(2,NP)
            PVN(2,NP)=-PVN(2,NP)
         ELSEIF(PXN(2,NP).GT.YMAX) THEN
            PXN(2,NP)=2*YMAX-PXN(2,NP)
            PVN(2,NP)=-PVN(2,NP)
         ENDIF
      ENDDO
      RETURN
      END
C
      SUBROUTINE INIT_PARTICLES
C
      INCLUDE 'plasma.inc'
      REAL*8 R(2)
C
      IF(MRSEED.EQ.0) THEN
         CALL GUDATE(NDY1,NDM1,NDD1,NTH1,NTM1,NTS1)
         IR=((NDD1*24+NTH1)*60+NTM1)*60+NTS1
      ELSE
         IR=MRSEED
      ENDIF
      CALL WFRNDI(IR)
C
      DO NP=1,NPMAX
         IF(NP.LE.NPMAX/2) THEN
            IPD(NP)=0
         ELSE
            IPD(NP)=1
         ENDIF
         CALL WFRNDU(2,R)
         PX(1,NP)=XMIN+GUCLIP(R(1))*XLEN
         PX(2,NP)=YMIN+GUCLIP(R(2))*YLEN
         CALL WFRNDN(2,R)
         PV(1,NP)=GUCLIP(R(1))*VT
         PV(2,NP)=GUCLIP(R(2))*VT
      ENDDO
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
      CALL INIT_PARTICLES
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
         CALL PUSH_PARTICLES
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
      CALL INIT_PARTICLES
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
         CALL PUSH_PARTICLES
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
      CALL INIT_PARTICLES
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
         CALL PUSH_PARTICLES
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
C     ****** UNIFORM RANDOM NUMBER GENERATION ******
C
      SUBROUTINE WFRNDU(N,R)
C
      DOUBLE PRECISION R(N)
      COMMON /WFRND1/ IRSEED
C
      IR=IRSEED
      CALL URAND1(N,R,IR)
      IRSEED=IR
C
      RETURN
      END
C
C     ****** NORMAL RANDOM NUMBER GENERATION ******
C
      SUBROUTINE WFRNDN(N,R)
C
      DOUBLE PRECISION R(N)
      COMMON /WFRND1/ IRSEED
C
      IR=IRSEED
      CALL NRAND(N,R,IR)
      IRSEED=IR
      RETURN
      END
C
C     ****** RANDOM NUMBER INTIALIZATION ******
C
      SUBROUTINE WFRNDI(IR)
C
      COMMON /WFRND1/ IRSEED
C
      IRSEED=MOD(IR,1664501)
      RETURN
      END
C
C     ****** UNIFORM RANDOM NUMBER FUNCTION ******
C
      FUNCTION DURAND(D)
C
      REAL*8 D,R,DURAND
      DIMENSION R(1)
C
      CALL WFRNDU(1,R)
      DURAND=D*R(1)
      RETURN
      END
C
      SUBROUTINE URAND1(N, X, IR)
C***********************************************************************
C UNIFORM RANDOM NUMBER GENERATOR (MIXED CONGRUENTIAL METHOD)          *
C     PORTABLE BUT SLOW.  THE PERIOD IS ONLY 1664501.                  *
C PARAMETERS                                                           *
C   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
C                  (INPUT)                                             *
C   (2) X      (D) UNIFORM RANDOM NUMBERS (OUTPUT)                     *
C   (3) IR     (I) THE INITIAL SEED  (INPUT)                           *
C                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
C COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
C***********************************************************************
C
       DOUBLE PRECISION X(N), INVM
       PARAMETER (M = 1664501, LAMBDA = 1229, MU = 351750)
       PARAMETER (INVM = 1.0D0 / M)
CPAREMETER CHECK
      IF( N .LE. 0) THEN
       WRITE(6,*) '(SUBR.URAND1) PARAMETER ERROR. N = ', N
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      END IF
      IF( IR .LT. 0 .OR. IR .GE. M) THEN
       WRITE(6,*) '(SUBR.URAND1) WARNING. IR = ', IR
      END IF
CMAIN LOOP
      DO 10 I = 1, N
       IR = MOD( LAMBDA * IR + MU, M)
       X(I) = IR * INVM
   10 CONTINUE
      RETURN
      END
C
       SUBROUTINE NRAND(N, X, IR)
C***********************************************************************
C NORMAL RANDOM NUMBER GENERATOR (BOX-MULLER METHOD)                   *
C PARAMETERS                                                           *
C   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
C                  (INPUT)                                             *
C   (2) X      (D) NORMAL RANDOM NUMBERS (OUTPUT)                      *
C   (3) IR     (I) THE INITIAL SEED FOR UNIFORM RANDOM NUMBER GENERATOR*
C                  (INPUT)                                             *
C                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
C COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
C***********************************************************************
       DOUBLE PRECISION X(N), R, T, PI2
       PARAMETER(PI2=2.D0*3.14159265358979D0)
CPAREMETER CHECK
      NN = N
      IF( NN .LE. 0) THEN
       WRITE(6,*) '(SUBR.NRAND) PARAMETER ERROR. N = ', NN
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      ELSE IF( MOD(N, 2) .NE. 0) THEN
       WRITE(6,*) '(SUBR.NRAND) WARNING. N IS ODD'
       NN=N+1
      END IF
CMAIN LOOP
      CALL URAND1(NN, X, IR)
      DO 10 I = 1, NN, 2
       R=SQRT(-2.0D0*LOG(X(I)))
       T=PI2*X(I+1)
       X(I  ) = R * SIN(T)
       X(I+1) = R * COS(T)
   10 CONTINUE
      RETURN
      END
