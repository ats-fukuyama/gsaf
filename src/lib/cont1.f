C     $Id$
C
C     *********************************************
C     ****** GSAF APLLICATION V3.5 : CONTOUR ******
C     *********************************************
C
C     ****** CONTOUR PLOT : XY, FIXED STEP, PATTERN ******
C
      MODULE GSCTR5
        IMPLICIT NONE
        INTEGER :: NXAC, NXMAXC, NYMAXC
      END MODULE

      SUBROUTINE CONTR1(Z,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTS1
      DIMENSION Z(NXA,*),DELX(1),DELY(1)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      DELX(1)=(PXE-PXS)/REAL(NXM)
      DELY(1)=(PYE-PYS)/REAL(NYM)
C
      CALL CONTR0(Z,DELX,DELY,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,CONTS1)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : XY, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTR2(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTS2
      DIMENSION Z(NXA,*),X(*),Y(*)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      CALL CONTR0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,CONTS2)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTR3(Z,R,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTS3
      DIMENSION Z(NXA,*),R(*),DELT(1)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      RMAX=R(NXMAX)
      DELT(1)=2*3.1415926/NYMAX
C
      CALL CONTR0(Z,R,DELT,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,2,CONTS3)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTR4(Z,R,T,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTS4
      DIMENSION Z(NXA,*),R(*),T(*)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      RMAX=R(NXMAX)
C
      CALL CONTR0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,0,CONTS4)
C
      RETURN
      END
      
C     ****** CONTOUR PLOT : XY, VARIABLE POSITION, PATTERN ******

      SUBROUTINE CONTR5(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP)

      USE GSCTR5
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: NXA, NXMAX, NYMAX, NSTEP
      REAL,    INTENT(INOUT) :: ZORG, ZSTEP
      REAL, DIMENSION(NXA,*), INTENT(INOUT) :: Z, X, Y
      REAL  :: EPS = 1.E-32
      EXTERNAL CONTS5

      IF(ABS(ZSTEP).LT.EPS) RETURN

      NXAC=NXA
      NXMAXC=NXMAX
      NYMAXC=NYMAX
      CALL CONTR0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,2,CONTS5)
      RETURN
      END SUBROUTINE CONTR5
C
C     ****** CONTOUR PLOT : XY, FIXED STEP, PATTERN ******
C
      SUBROUTINE CONTP1(Z,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTS1
      DIMENSION Z(NXA,*),KA(2,*),DELX(1),DELY(1)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      DELX(1)=(PXE-PXS)/REAL(NXM)
      DELY(1)=(PYE-PYS)/REAL(NYM)
C
      CALL CONTP0(Z,DELX,DELY,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS1)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : XY, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTP2(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTS2
      DIMENSION Z(NXA,*),KA(2,*),X(*),Y(*)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      CALL CONTP0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS2)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTP3(Z,R,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTS3
      DIMENSION Z(NXA,*),R(*),KA(2,*),DELT(1)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      RMAX=R(NXMAX)
      DELT(1)=2*3.1415926/NYMAX
C
      CALL CONTP0(Z,R,DELT,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,2,IPAT,KA,CONTS3)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTP4(Z,R,T,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTS4
      DIMENSION Z(NXA,*),R(*),T(*),KA(2,*)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      RMAX=R(NXMAX)
C
      CALL CONTP0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,0,IPAT,KA,CONTS4)
C
      RETURN
      END
      
C     ****** CONTOUR PLOT : XY, VARIABLE POSITION, PATTERN ******

      SUBROUTINE CONTP5(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA)

      USE GSCTR5
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: NXA, NXMAX, NYMAX, NSTEP, IPRD, IPAT
      INTEGER, DIMENSION(2,*), INTENT(INOUT) :: KA
      REAL,    INTENT(INOUT) :: ZORG, ZSTEP
      REAL,  DIMENSION(NXA,*), INTENT(INOUT) :: Z, X, Y
      REAL  :: EPS = 1.E-32
      EXTERNAL CONTS5


      IF(ABS(ZSTEP).LT.EPS) RETURN

      NXAC=NXA
      NXMAXC=NXMAX
      NYMAXC=NYMAX
      CALL CONTP0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS5)
      RETURN
      END SUBROUTINE CONTP5
      
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******

      SUBROUTINE CONTP6(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPAT,KA)

      USE GSCTR5
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: NXA, NXMAX, NYMAX, NSTEP, IPAT
      INTEGER, DIMENSION(2,*), INTENT(INOUT) :: KA
      REAL,                    INTENT(INOUT) :: ZORG, ZSTEP
      REAL,  DIMENSION(*),     INTENT(INOUT) :: X, Y
      REAL,  DIMENSION(NXA,*), INTENT(INOUT) :: Z
      REAL  :: EPS = 1.E-32
      EXTERNAL CONTS6

      IF(ABS(ZSTEP).LT.EPS) RETURN

      NXAC=NXA
      NXMAXC=NXMAX
      NYMAXC=NYMAX

      CALL CONTP0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,0,IPAT,KA,CONTS6)

      RETURN
      END SUBROUTINE CONTP6
C
C     ****** CONTOUR PLOT : XY, FIXED STEP, PATTERN ******
C
      SUBROUTINE CONTQ1(Z,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTS1
      DIMENSION Z(NXA,*),KA(2,*),DELX(1),DELY(1)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      DELX(1)=(PXE-PXS)/REAL(NXM)
      DELY(1)=(PYE-PYS)/REAL(NYM)
C
      CALL CONTQ0(Z,DELX,DELY,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS1)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : XY, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTQ2(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTS2
      DIMENSION Z(NXA,*),KA(2,*),X(*),Y(*)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      CALL CONTQ0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS2)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTQ3(Z,R,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTS3
      DIMENSION Z(NXA,*),R(*),KA(2,*),DELT(1)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      RMAX=R(NXMAX)
      DELT(1)=2*3.1415926/NYMAX
C
      CALL CONTQ0(Z,R,DELT,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,2,IPAT,KA,CONTS3)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTQ4(Z,R,T,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTS4
      DIMENSION Z(NXA,*),R(*),T(*),KA(2,*)
      DATA EPS/1.E-32/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.EPS) RETURN
C
      RMAX=R(NXMAX)
C
      CALL CONTQ0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,0,IPAT,KA,CONTS4)
C
      RETURN
      END
      
C     ****** CONTOUR PLOT : XY, VARIABLE POSITION, PATTERN ******

      SUBROUTINE CONTQ5(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA)

      USE GSCTR5
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: NXA, NXMAX, NYMAX, NSTEP, IPRD, IPAT
      INTEGER, DIMENSION(4,*), INTENT(INOUT) :: KA
      REAL,    INTENT(INOUT) :: ZORG, ZSTEP
      REAL,  DIMENSION(NXA,*), INTENT(INOUT) :: Z, X, Y
      REAL  :: EPS = 1.E-32
      EXTERNAL CONTS5


      IF(ABS(ZSTEP).LT.EPS) RETURN

      NXAC=NXA
      NXMAXC=NXMAX
      NYMAXC=NYMAX
      CALL CONTQ0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS5)
      RETURN
      END SUBROUTINE CONTQ5
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : XY FIXED ******
C
      SUBROUTINE CONTS1(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION X(*),Y(*)
C
      IF(NAX.GT.0) THEN
         XA=X(1)*(NAX-1)+PXS
         YA=Y(1)*(NAY-1)+PYS
      ELSE
         XA=X(1)*(-NAX-0.5)+PXS
         YA=Y(1)*(-NAY-0.5)+PYS
      ENDIF
      IF(NBX.GT.0) THEN
         XB=X(1)*(NBX-1)+PXS
         YB=Y(1)*(NBY-1)+PYS
      ELSE
         XB=X(1)*(-NBX-0.5)+PXS
         YB=Y(1)*(-NBY-0.5)+PYS
      ENDIF
      XS=(XB-XA)*(U0-UA)/(UB-UA)+XA
      YS=(YB-YA)*(U0-UA)/(UB-UA)+YA
C
      IF(IND.EQ.1) THEN
         CALL MOVEPT(XS,YS,IPAT)
      ELSEIF(IND.EQ.-1) THEN
         CALL MOVEPT(XS,YS,-IPAT)
      ELSEIF(IND.EQ.0) THEN
         CALL DRAWPT(XS,YS)
      ENDIF
      RETURN
      END
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : XY VARIABLE ******
C
      SUBROUTINE CONTS2(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION X(*),Y(*)
C
      IF(NAX.GT.0) THEN
         XA=DX*(X(NAX)-GXS)+PXS
         YA=DY*(Y(NAY)-GYS)+PYS
      ELSE
         XA=DX*(0.5*(X(-NAX)+X(-NAX+1))-GXS)+PXS
         YA=DY*(0.5*(Y(-NAY)+Y(-NAY+1))-GYS)+PYS
      ENDIF
      IF(NBX.GT.0) THEN
         XB=DX*(X(NBX)-GXS)+PXS
         YB=DY*(Y(NBY)-GYS)+PYS
      ELSE
         XB=DX*(0.5*(X(-NBX)+X(-NBX+1))-GXS)+PXS
         YB=DY*(0.5*(Y(-NBY)+Y(-NBY+1))-GYS)+PYS
      ENDIF
      XS=(XB-XA)*(U0-UA)/(UB-UA)+XA
      YS=(YB-YA)*(U0-UA)/(UB-UA)+YA
      IF(IND.EQ.1) THEN
         CALL MOVEPT(XS,YS,IPAT)
      ELSEIF(IND.EQ.-1) THEN
         CALL MOVEPT(XS,YS,-IPAT)
      ELSEIF(IND.EQ.0) THEN
         CALL DRAWPT(XS,YS)
      ENDIF
      RETURN
      END
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : R-THETA FIXED ******
C
      SUBROUTINE CONTS3(NAX,NAY,NBX,NBY,U0,UA,UB,R,T,IPAT,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
      DIMENSION R(*),T(*)
C
      IF(NAX.GT.0) THEN
         RA=R(NAX)
         TA=T(1)*(NAY-1)
      ELSE
         RA=0.5*(R(-NAX)+R(-NAX+1))
         TA=T(1)*(-NAY-0.5)
      ENDIF
      IF(NBX.GT.0) THEN
         RB=R(NBX)
         TB=T(1)*(NBY-1)
      ELSE
         RB=0.5*(R(-NBX)+R(-NBX+1))
         TB=T(1)*(-NBY-0.5)
      ENDIF
      RS=(RB-RA)*(U0-UA)/(UB-UA)+RA
      TS=(TB-TA)*(U0-UA)/(UB-UA)+TA
      XS=DX*(RS*COS(TS)-GXS)+PXS
      YS=DY*(RS*SIN(TS)-GYS)+PYS
C
      IF(IND.EQ.1) THEN
         CALL MOVEPT(XS,YS,IPAT)
      ELSEIF(IND.EQ.-1) THEN
         CALL MOVEPT(XS,YS,-IPAT)
      ELSEIF(IND.EQ.0) THEN
         IMAX=INT(SQRT((XS-XT)**2+(YS-YT)**2)*16/RMAX)+1
         DELR=(RS-RT)/IMAX
         DELT=(TS-TT)/IMAX
         DO 100 I=1,IMAX
            RI=DELR*I+RT
            TI=DELT*I+TT
            XI=DX*(RI*COS(TI)-GXS)+PXS
            YI=DY*(RI*SIN(TI)-GYS)+PYS
            CALL DRAWPT(XI,YI)
  100    CONTINUE
      ENDIF
      RT=RS
      TT=TS
      XT=XS
      YT=YS
      RETURN
      END
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : R-THETA VARIABLE ******
C
      SUBROUTINE CONTS4(NAX,NAY,NBX,NBY,U0,UA,UB,R,T,IPAT,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
      DIMENSION R(*),T(*)
C
      IF(NAX.GT.0) THEN
         RA=R(NAX)
         TA=T(NAY)
      ELSE
         RA=0.5*(R(-NAX)+R(-NAX+1))
         TA=0.5*(T(-NAY)+T(-NAY+1))
      ENDIF
      IF(NBX.GT.0) THEN
         RB=R(NBX)
         TB=T(NBY)
      ELSE
         RB=0.5*(R(-NBX)+R(-NBX+1))
         TB=0.5*(T(-NBY)+T(-NBY+1))
      ENDIF
      RS=(RB-RA)*(U0-UA)/(UB-UA)+RA
      TS=(TB-TA)*(U0-UA)/(UB-UA)+TA
      XS=DX*(RS*COS(TS)-GXS)+PXS
      YS=DY*(RS*SIN(TS)-GYS)+PYS
C
      IF(IND.EQ.1) THEN
         CALL MOVEPT(XS,YS,IPAT)
      ELSEIF(IND.EQ.-1) THEN
         CALL MOVEPT(XS,YS,-IPAT)
      ELSEIF(IND.EQ.0) THEN
         IMAX=INT(SQRT((XS-XT)**2+(YS-YT)**2)*16/RMAX)+1
         DELR=(RS-RT)/IMAX
         DELT=(TS-TT)/IMAX
         DO 100 I=1,IMAX
            RI=DELR*I+RT
            TI=DELT*I+TT
            XI=DX*(RI*COS(TI)-GXS)+PXS
            YI=DY*(RI*SIN(TI)-GYS)+PYS
            CALL DRAWPT(XI,YI)
  100    CONTINUE
      ENDIF
      RT=RS
      TT=TS
      XT=XS
      YT=YS
      RETURN
      END
      
C     ****** CONTOUR PLOT SLAVE ROUTINE : XY VARIABLE ******

      SUBROUTINE CONTS5(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,IND)

      USE GSCTR5
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: NAX, NAY, NBX, NBY, IPAT, IND
      REAL,    INTENT(INOUT) :: U0, UA, UB
      REAL, DIMENSION(*), INTENT(INOUT) ::  X, Y
      INTEGER NA, NA1, NA2, NAXL, NAYL, NAX1, NAY1, NB, NB1
      INTEGER NB2, NBXL, NBYL, NBX1, NBY1
      REAL    DX, DY, PXMAX, PXMIN, PYMAX, PYMIN
      REAL    GXMAX, GXMIN, GYMAX, GYMIN
      REAL    XA, YA, XB, YB, XS, YS

      CALL INQGDEFIN(PXMIN,PXMAX,PYMIN,PYMAX,
     &                      GXMIN,GXMAX,GYMIN,GYMAX)
      DX=(PXMAX-PXMIN)/(GXMAX-GXMIN)
      DY=(PYMAX-PYMIN)/(GYMAX-GYMIN)

      NAXL=ABS(NAX)
      NAYL=ABS(NAY)
      IF(NAXL.GT.NXMAXC) NAXL=NAXL-NXMAXC
      IF(NAYL.GT.NYMAXC) NAYL=NAYL-NYMAXC

      NBXL=ABS(NBX)
      NBYL=ABS(NBY)
      IF(NBXL.GT.NXMAXC) NBXL=NBXL-NXMAXC
      IF(NBYL.GT.NYMAXC) NBYL=NBYL-NYMAXC

      IF(NAX.GT.0) THEN
         NA=NXAC*(NAYL-1)+NAXL
         XA=DX*(X(NA)-GXMIN)+PXMIN
         YA=DY*(Y(NA)-GYMIN)+PYMIN
      ELSE
         NAX1=NAXL+1
         IF(NAX1.GT.NXMAXC) NAX1=NAX1-NXMAXC
         NAY1=NAYL+1
         IF(NAY1.GT.NYMAXC) NAY1=NAY1-NYMAXC
         NA1=NXAC*(NAYL-1)+NAX1
         NA2=NXAC*(NAY1-1)+NAXL
         XA=DX*(0.5*(X(NA1)+X(NA2))-GXMIN)+PXMIN
         YA=DY*(0.5*(Y(NA1)+Y(NA2))-GYMIN)+PYMIN
      ENDIF
      IF(NBX.GT.0) THEN
         NB=NXAC*(NBYL-1)+NBXL
         XB=DX*(X(NB)-GXMIN)+PXMIN
         YB=DY*(Y(NB)-GYMIN)+PYMIN
      ELSE
         NBX1=NBXL+1
         IF(NBX1.GT.NXMAXC) NBX1=NBX1-NXMAXC
         NBY1=NBYL+1
         IF(NBY1.GT.NYMAXC) NBY1=NBY1-NYMAXC
         NB1=NXAC*(NBYL-1)+NBX1
         NB2=NXAC*(NBY1-1)+NBXL
         XB=DX*(0.5*(X(NB1)+X(NB2))-GXMIN)+PXMIN
         YB=DY*(0.5*(Y(NB1)+Y(NB2))-GYMIN)+PYMIN
      ENDIF
      IF(UB.NE.UA) THEN
         XS=(XB-XA)*(U0-UA)/(UB-UA)+XA
         YS=(YB-YA)*(U0-UA)/(UB-UA)+YA
         IF(IND.EQ.1) THEN
            CALL MOVEPT(XS,YS,IPAT)
         ELSEIF(IND.EQ.-1) THEN
            CALL MOVEPT(XS,YS,-IPAT)
         ELSEIF(IND.EQ.0) THEN
            CALL DRAWPT(XS,YS)
         ENDIF
      ENDIF
C      WRITE(6,*) NAX,NAY,NBX,NBY,XS,YS
      RETURN
      END SUBROUTINE CONTS5

C     ****** CONTOUR PLOT SLAVE ROUTINE : R-THETA VARIABLE ******

      SUBROUTINE CONTS6(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,IND)

      USE GSCTR5
      IMPLICIT NONE
      INTEGER, INTENT(INOUT) :: NAX, NAY, NBX, NBY, IPAT, IND
      REAL,    INTENT(INOUT) :: U0, UA, UB
      REAL, DIMENSION(*), INTENT(INOUT) :: X(*),Y(*)
      COMMON /GSCTR4/ RMAX, RT, TT, XT, YT
      REAL :: RMAX, RT, TT, XT, YT
      INTEGER :: NAX1, NAX2, NAY1, NAY2, NBX1, NBX2, NBY1, NBY2
      REAL    :: DX, DY, PXMIN,PXMAX,PYMIN,PYMAX,GXMIN,GXMAX,GYMIN,GYMAX
      REAL    :: XA, XB, XS, YA, YB, YS


      CALL INQGDEFIN(PXMIN,PXMAX,PYMIN,PYMAX,
     &               GXMIN,GXMAX,GYMIN,GYMAX)
      DX=(PXMAX-PXMIN)/(GXMAX-GXMIN)
      DY=(PYMAX-PYMIN)/(GYMAX-GYMIN)

      IF(NAX.GT.0) THEN
         XA=X(NAX+(NAY-1)*NXAC)
         YA=Y(NAX+(NAY-1)*NXAC)
      ELSE
         NAX1=ABS(NAX)
         NAX2=ABS(NAX)+1
         IF(NAX2.GT.NXMAXC) NAX2=NAX2-NXMAXC
         NAY1=NAY
         NAY2=NAY+1
         IF(NAY2.GT.NYMAXC) NAY2=NAY2-NYMAXC
         XA=0.5D0*(X(NAX1+(NAY2-1)*NXAC)+X(NAX2+(NAY2-1)*NXAC))
         YA=0.5D0*(Y(NAX1+(NAY2-1)*NXAC)+Y(NAX2+(NAY2-1)*NXAC))
      ENDIF

      IF(NBX.GT.0) THEN
         XB=X(NBX+(NBY-1)*NXAC)
         YB=Y(NBX+(NBY-1)*NXAC)
      ELSE
         NBX1=ABS(NBX)
         NBX2=ABS(NBX)+1
         IF(NBX2.GT.NXMAXC) NBX2=NBX2-NXMAXC
         NBY1=NBY
         NBY2=NBY+1
         IF(NBY2.GT.NYMAXC) NBY2=NBY2-NYMAXC
         XB=0.5D0*(X(NBX1+(NBY2-1)*NXAC)+X(NBX2+(NBY2-1)*NXAC))
         YB=0.5D0*(Y(NBX1+(NBY2-1)*NXAC)+Y(NBX2+(NBY2-1)*NXAC))
      ENDIF

      XS=(XB-XA)*(U0-UA)/(UB-UA)+XA
      YS=(YB-YA)*(U0-UA)/(UB-UA)+YA
      XS=DX*(XS-GXMIN)+PXMIN
      YS=DY*(YS-GYMIN)+PYMIN

      IF(IND.EQ.1) THEN
         CALL MOVEPT(XS,YS,IPAT)
      ELSEIF(IND.EQ.-1) THEN
         CALL MOVEPT(XS,YS,-IPAT)
      ELSEIF(IND.EQ.0) THEN
         CALL DRAWPT(XS,YS)
      ENDIF
      XT=XS
      YT=YS
      RETURN
      END SUBROUTINE CONTS6
C
C     ****** CONTOUR PLOT : COMMON SUB (RECTANGULAR) ******
C
      SUBROUTINE CONTR0(Z,X,Y,NXA,NX,NY,ZORG,ZSTEP,NSTEP,IPRD,SUB)
C
      IMPLICIT LOGICAL(L)
      EXTERNAL SUB
      DIMENSION Z(NXA,*),X(*),Y(*)
      DATA EPS/1.E-32/
C
      KMAX=NSTEP
      NXMAX=NX-1
      NYMAX=NY-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXMAX=NX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYMAX=NY
      ZBIAS=0.9999999
C
      DO 100 JDO=1,NYMAX
         J=JDO
         JJ=J+1
         IF(J.EQ.NY) JJ=1
         U1=(Z(1,J )-ZORG)/ZSTEP+ZBIAS
         U4=(Z(1,JJ)-ZORG)/ZSTEP+ZBIAS
         K1=INTCLIP(MAX(U1,0.0))
         K4=INTCLIP(MAX(U4,0.0))
C
      DO 100 IDO=1,NXMAX
         I=IDO
         II=I+1
         IF(I.EQ.NX) II=1
         U2=(Z(II,J )-ZORG)/ZSTEP+ZBIAS
         U3=(Z(II,JJ)-ZORG)/ZSTEP+ZBIAS
         K2=INTCLIP(MAX(U2,0.0))
         K3=INTCLIP(MAX(U3,0.0))
C
         LJ1=ABS(U2-U1).GE.EPS
         LJ2=ABS(U3-U2).GE.EPS
         LJ3=ABS(U4-U3).GE.EPS
         LJ4=ABS(U1-U4).GE.EPS
C
C
         IF(LJ1.AND.(K1.NE.K2)
     &         .AND.(K1.GT.0.OR.K2.GT.0)
     &         .AND.(K1.LT.KMAX.OR.K2.LT.KMAX)) THEN
            KS=MAX(MIN(K1,K2),0)+1
            KE=MIN(MAX(K1,K2),KMAX)
C
            DO 20 K=KS,KE
               U0=REAL(K)
               IF(LJ2.AND.(U0.LE.U3.OR.U0.LE.U2)
     &               .AND.(U3.LE.U0.OR.U2.LE.U0)) THEN
                  CALL SUB(I  ,J  ,I+1,J  ,U0,U1,U2,X,Y,0,1)
                  CALL SUB(I+1,J  ,I+1,J+1,U0,U2,U3,X,Y,0,0)
               ENDIF
C
               IF((U0.LE.U1.OR.U0.LE.U3).AND.
     &            (U1.LE.U0.OR.U3.LE.U0)) THEN
C
                  IF(LJ3.AND.(U0.LE.U4.OR.U0.LE.U3)
     &                  .AND.(U4.LE.U0.OR.U3.LE.U0)
     &                  .AND.(U0.LE.U2.OR.U0.LE.U4)
     &                  .AND.(U2.LE.U0.OR.U4.LE.U0)) THEN
                     CALL SUB(I  ,J  ,I+1,J  ,U0,U1,U2,X,Y,0,1)
                     CALL SUB(I  ,J+1,I+1,J+1,U0,U4,U3,X,Y,0,0)
                  ENDIF
C
                  IF(LJ4.AND.(U0.LE.U1.OR.U0.LE.U4)
     &                  .AND.(U1.LE.U0.OR.U4.LE.U0)) THEN
                     CALL SUB(I  ,J  ,I+1,J  ,U0,U1,U2,X,Y,0,1)
                     CALL SUB(I  ,J  ,I  ,J+1,U0,U1,U4,X,Y,0,0)
                  ENDIF
               ENDIF
   20       CONTINUE
         ENDIF
C
         IF(LJ2.AND.(K2.NE.K3)
     &         .AND.(K2.GT.0.OR.K3.GT.0)
     &         .AND.(K2.LT.KMAX.OR.K3.LT.KMAX)) THEN
            KS=MAX(MIN(K2,K3),0)+1
            KE=MIN(MAX(K2,K3),KMAX)
C
            DO 30 K=KS,KE
               U0=REAL(K)
               IF((U0.LE.U1.OR.U0.LE.U3).AND.
     &            (U1.LE.U0.OR.U3.LE.U0)) THEN
C
                  IF(LJ3.AND.(U0.LE.U3.OR.U0.LE.U4)
     &                  .AND.(U3.LE.U0.OR.U4.LE.U0)) THEN
                     CALL SUB(I+1,J  ,I+1,J+1,U0,U2,U3,X,Y,0,1)
                     CALL SUB(I  ,J+1,I+1,J+1,U0,U4,U3,X,Y,0,0)
                  ENDIF
C
                  IF(LJ4.AND.(U0.LE.U1.OR.U0.LE.U4)
     &                  .AND.(U1.LE.U0.OR.U4.LE.U0)
     &                  .AND.(U0.LE.U2.OR.U0.LE.U4)
     &                  .AND.(U2.LE.U0.OR.U4.LE.U0)) THEN
                     CALL SUB(I+1,J  ,I+1,J+1,U0,U2,U3,X,Y,0,1)
                     CALL SUB(I  ,J  ,I  ,J+1,U0,U1,U4,X,Y,0,0)
                  ENDIF
               ENDIF
   30       CONTINUE
         ENDIF
C
         IF(LJ3.AND.(K3.NE.K4)
     &         .AND.(K3.GT.0.OR.K4.GT.0)
     &         .AND.(K3.LT.KMAX.OR.K4.LT.KMAX)) THEN
            KS=MAX(MIN(K3,K4),0)+1
            KE=MIN(MAX(K3,K4),KMAX)
C
            DO 40 K=KS,KE
               U0=REAL(K)
               IF(LJ4.AND.(U1.LE.U0.OR.U4.LE.U0)
     &               .AND.(U0.LE.U1.OR.U0.LE.U4)) THEN
                  CALL SUB(I  ,J+1,I+1,J+1,U0,U4,U3,X,Y,0,1)
                  CALL SUB(I  ,J  ,I  ,J+1,U0,U1,U4,X,Y,0,0)
               ENDIF
   40       CONTINUE
         ENDIF
C
         U1=U2
         U4=U3
         K1=K2
         K4=K3
  100 CONTINUE
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : COMMON SUB (2 TRIANGLE) ******
C
      SUBROUTINE CONTP0(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,SUB)
C
      IMPLICIT LOGICAL(L)
C
      EXTERNAL SUB
      DIMENSION Z(NXA,*),X(*),Y(*),KA(2,*)
      DATA EPS/1.E-7/
C
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
C
      ZBIAS=2.-10.*EPS
      KMAX=NSTEP
C
      IE=0
      DO 100 NY=1,NYM
      DO 100 NX=1,NXM
         IE=IE+1
         NXP=NX+1
         NYP=NY+1
         IF(NXP.GT.NXMAX) NXP=1
         IF(NYP.GT.NYMAX) NYP=1
         K1=INTCLIP((Z(NX ,NY )-ZORG)/ZSTEP+ZBIAS)
         K2=INTCLIP((Z(NXP,NYP)-ZORG)/ZSTEP+ZBIAS)
         K3=INTCLIP((Z(NX ,NYP)-ZORG)/ZSTEP+ZBIAS)
         KA(1,IE)=MAX(MIN(K1,K2,K3),1)
         KA(2,IE)=MAX(K1,K2,K3,1)
         IE=IE+1
         K3=INTCLIP((Z(NXP,NY )-ZORG)/ZSTEP+ZBIAS)
         KA(1,IE)=MAX(MIN(K1,K2,K3),1)
         KA(2,IE)=MAX(K1,K2,K3,1)
  100 CONTINUE
C
      DO 1000 K=1,KMAX
         U0=REAL(K+1)
      DO 1000 NY=1,NYM
      DO 1000 NX=1,NXM
      DO 1000 NE=1,2
         IE=(NXM*(NY-1)+NX-1)*2+NE
         IF(KA(1,IE).LE.K.AND.KA(2,IE).GT.K) THEN
            N1X=NX
            N1Y=NY
            N2X=NX+1
            N2Y=NY+1
            IF(NE.EQ.1) THEN
               N3X=NX
               N3Y=NY+1
            ELSE
               N3X=NX+1
               N3Y=NY
            ENDIF
            U1=(Z(N1X,N1Y)-ZORG)/ZSTEP+ZBIAS
            I2X=N2X
            I2Y=N2Y
            IF(I2X.GT.NXMAX) I2X=1
            IF(I2Y.GT.NYMAX) I2Y=1
            U2=(Z(I2X,I2Y)-ZORG)/ZSTEP+ZBIAS
            I3X=N3X
            I3Y=N3Y
            IF(I3X.GT.NXMAX) I3X=1
            IF(I3Y.GT.NYMAX) I3Y=1
            U3=(Z(I3X,I3Y)-ZORG)/ZSTEP+ZBIAS
C
            IF((U1.GT.U0.AND.U2.LE.U0).OR.
     &         (U1.LE.U0.AND.U2.GT.U0)) THEN
               NAX=N1X
               NAY=N1Y
               UA=U1
               NBX=N2X
               NBY=N2Y
               UB=U2
               NSAX=N3X
               NSAY=N3Y
               USA=U3
               MODE=3
               IF((U2.GT.U0.AND.U3.LE.U0).OR.
     &            (U2.LE.U0.AND.U3.GT.U0)) THEN
                  NSBX=N2X
                  NSBY=N2Y
                  USB=U2
                  MODES=1
               ELSEIF((U1.GT.U0.AND.U3.LE.U0).OR.
     &                (U1.LE.U0.AND.U3.GT.U0)) THEN
                  NSBX=N1X
                  NSBY=N1Y
                  USB=U1
                  MODES=2
               ELSE
                  GOTO 1000
               ENDIF
            ELSEIF((U1.GT.U0.AND.U3.LE.U0).OR.
     &             (U1.LE.U0.AND.U3.GT.U0)) THEN
               NAX=N1X
               NAY=N1Y
               UA=U1
               NBX=N3X
               NBY=N3Y
               UB=U3
               NSAX=N2X
               NSAY=N2Y
               USA=U2
               NSBX=N3X
               NSBY=N3Y
               USB=U3
               MODE=2
               MODES=1
            ELSE
               GOTO 1000
            ENDIF
C
            CALL SUB(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,1)
            NAXQ=NAX
            NAYQ=NAY
            NBXQ=NBX
            NBYQ=NBY
            UAQ=UA
            UBQ=UB
C
            IEL=IE
            NEL=NE
            NXL=NX
            NYL=NY
            MODEL=MODE
            LINV=.FALSE.
            LEND=.FALSE.
C
  200       IF(NEL.EQ.1) THEN
               NEL=2
               IF(MODEL.EQ.1) NYL=NYL+1
               IF(MODEL.EQ.2) NXL=NXL-1
            ELSE
               NEL=1
               IF(MODEL.EQ.1) NXL=NXL+1
               IF(MODEL.EQ.2) NYL=NYL-1
            ENDIF
C
            IF(NXL.GE.1.AND.NXL.LE.NXM.AND.
     &         NYL.GE.1.AND.NYL.LE.NYM) THEN
               IEL=(NXM*(NYL-1)+NXL-1)*2+NEL
               N1X=NXL
               N1Y=NYL
               N2X=NXL+1
               N2Y=NYL+1
               IF(NEL.EQ.1) THEN
                  N3X=NXL
                  N3Y=NYL+1
               ELSE
                  N3X=NXL+1
                  N3Y=NYL
               ENDIF
               U1=(Z(N1X,N1Y)-ZORG)/ZSTEP+ZBIAS
               I2X=N2X
               I2Y=N2Y
               IF(I2X.GT.NXMAX) I2X=1
               IF(I2Y.GT.NYMAX) I2Y=1
               U2=(Z(I2X,I2Y)-ZORG)/ZSTEP+ZBIAS
               I3X=N3X
               I3Y=N3Y
               IF(I3X.GT.NXMAX) I3X=1
               IF(I3Y.GT.NYMAX) I3Y=1
               U3=(Z(I3X,I3Y)-ZORG)/ZSTEP+ZBIAS
C
               IF(MODEL.EQ.3) THEN
                  NAX=N3X
                  NAY=N3Y
                  UA=U3
                  IF((U2.GT.U0.AND.U3.LE.U0).OR.
     &               (U2.LE.U0.AND.U3.GT.U0)) THEN
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=1
                  ELSEIF((U1.GT.U0.AND.U3.LE.U0).OR.
     &                   (U1.LE.U0.AND.U3.GT.U0)) THEN
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=2
                  ELSE
                     LEND=.TRUE.
                  ENDIF
               ELSE IF(MODEL.EQ.2) THEN
                  NAX=N1X
                  NAY=N1Y
                  UA=U1
                  IF((U1.GT.U0.AND.U3.LE.U0).OR.
     &               (U1.LE.U0.AND.U3.GT.U0)) THEN
                     NBX=N3X
                     NBY=N3Y
                     UB=U3
                     MODEL=2
                  ELSEIF((U1.GT.U0.AND.U2.LE.U0).OR.
     &                   (U1.LE.U0.AND.U2.GT.U0)) THEN
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=3
                  ELSE
                     LEND=.TRUE.
                  ENDIF
               ELSE IF(MODEL.EQ.1) THEN
                  NAX=N2X
                  NAY=N2Y
                  UA=U2
                  IF((U2.GT.U0.AND.U1.LE.U0).OR.
     &               (U2.LE.U0.AND.U1.GT.U0)) THEN
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=3
                  ELSEIF((U2.GT.U0.AND.U3.LE.U0).OR.
     &                   (U2.LE.U0.AND.U3.GT.U0)) THEN
                     NBX=N3X
                     NBY=N3Y
                     UB=U3
                     MODEL=1
                  ELSE
                     LEND=.TRUE.
                  ENDIF
               ENDIF
            ELSE
               IF(LINV) THEN
                  LEND=.TRUE.
               ELSE
C
                  CALL SUB(NAXQ,NAYQ,NBXQ,NBYQ,U0,UAQ,UBQ,X,Y,IPAT,-1)
C
                  IEL=IE
                  NAX=NSAX
                  NAY=NSAY
                  UA=USA
                  NBX=NSBX
                  NBY=NSBY
                  UB=USB
                  NXL=NX
                  NYL=NY
                  NEL=NE
                  MODEL=MODES
                  LINV=.TRUE.
               ENDIF
            ENDIF
            IF(.NOT.LEND) THEN
C
               CALL SUB(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,0)
C
               KA(1,IEL)=KA(1,IEL)+1
               IF(IEL.EQ.IE.AND..NOT.LINV) LEND=.TRUE.
            ENDIF
C
            IF(.NOT.LEND) GOTO 200
C
         ENDIF
 1000 CONTINUE
      RETURN
      END
C
C     ****** CONTOUR PLOT : COMMON SUB (4 TRIANGLE) ******
C
      SUBROUTINE CONTQ0(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,SUB)
C
      IMPLICIT LOGICAL(L)
C
      EXTERNAL SUB
      DIMENSION Z(NXA,*),X(*),Y(*),KA(2,*)
      DATA EPS/1.E-7/
C
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
C
      ZBIAS=2.-10.*EPS
      KMAX=NSTEP
C
      IE=0
      DO 100 NY=1,NYM
      DO 100 NX=1,NXM
         NXP=NX+1
         NYP=NY+1
         IF(NXP.GT.NXMAX) NXP=1
         IF(NYP.GT.NYMAX) NYP=1
         K1=INTCLIP((Z(NX ,NY )-ZORG)/ZSTEP+ZBIAS)
         K2=INTCLIP((Z(NXP,NY )-ZORG)/ZSTEP+ZBIAS)
         K3=INTCLIP((Z(NXP,NYP)-ZORG)/ZSTEP+ZBIAS)
         K4=INTCLIP((Z(NX ,NYP)-ZORG)/ZSTEP+ZBIAS)
         ZC=0.25*(Z(NX,NY)+Z(NXP,NY)+Z(NXP,NYP)+Z(NX,NYP))
         KC=INTCLIP((ZC        -ZORG)/ZSTEP+ZBIAS)
         IE=IE+1
         KA(1,IE)=MAX(MIN(K1,K2,KC),1)
         KA(2,IE)=MAX(K1,K2,KC,1)
         IE=IE+1
         KA(1,IE)=MAX(MIN(K2,K3,KC),1)
         KA(2,IE)=MAX(K2,K3,KC,1)
         IE=IE+1
         KA(1,IE)=MAX(MIN(K3,K4,KC),1)
         KA(2,IE)=MAX(K3,K4,KC,1)
         IE=IE+1
         KA(1,IE)=MAX(MIN(K4,K1,KC),1)
         KA(2,IE)=MAX(K4,K1,KC,1)
  100 CONTINUE
C
      DO 1000 K=1,KMAX
         U0=REAL(K+1)
      DO 1000 NY=1,NYM
      DO 1000 NX=1,NXM
         I1X=NX
         I1Y=NY
         I2X=NX+1
         I2Y=NY+1
         IF(I2X.GT.NXMAX) I2X=1
         IF(I2Y.GT.NYMAX) I2Y=1
         ZC=0.25*(Z(I1X,I1Y)+Z(I2X,I1Y)+Z(I2X,I2Y)+Z(I1X,I2Y))
         UC=(ZC-ZORG)/ZSTEP+ZBIAS
      DO 1000 NE=1,4
         IE=(NXM*(NY-1)+NX-1)*4+NE
         IF(KA(1,IE).LE.K.AND.KA(2,IE).GT.K) THEN
            IF(NE.EQ.1) THEN
               N1X=NX
               N1Y=NY
               N2X=NX+1
               N2Y=NY
            ELSEIF(NE.EQ.2) THEN
               N1X=NX+1
               N1Y=NY
               N2X=NX+1
               N2Y=NY+1
            ELSEIF(NE.EQ.3) THEN
               N1X=NX+1
               N1Y=NY+1
               N2X=NX
               N2Y=NY+1
            ELSE
               N1X=NX
               N1Y=NY+1
               N2X=NX
               N2Y=NY
            ENDIF
            I1X=N1X
            I1Y=N1Y
            IF(I1X.GT.NXMAX) I1X=1
            IF(I1Y.GT.NYMAX) I1Y=1
            U1=(Z(I1X,I1Y)-ZORG)/ZSTEP+ZBIAS
            I2X=N2X
            I2Y=N2Y
            IF(I2X.GT.NXMAX) I2X=1
            IF(I2Y.GT.NYMAX) I2Y=1
            U2=(Z(I2X,I2Y)-ZORG)/ZSTEP+ZBIAS
C
            IF((U1.GT.U0.AND.U2.LE.U0).OR.
     &         (U1.LE.U0.AND.U2.GT.U0)) THEN
               NAX=N1X
               NAY=N1Y
               UA=U1
               NBX=N2X
               NBY=N2Y
               UB=U2
               NSAX=-NX
               NSAY=-NY
               USA=UC
               MODE=3
               IF((U2.GT.U0.AND.UC.LE.U0).OR.
     &            (U2.LE.U0.AND.UC.GT.U0)) THEN
                  NSBX=N2X
                  NSBY=N2Y
                  USB=U2
                  MODES=1
               ELSEIF((U1.GT.U0.AND.UC.LE.U0).OR.
     &                (U1.LE.U0.AND.UC.GT.U0)) THEN
                  NSBX=N1X
                  NSBY=N1Y
                  USB=U1
                  MODES=2
               ELSE
                  GOTO 1000
               ENDIF
            ELSEIF((U1.GT.U0.AND.UC.LE.U0).OR.
     &             (U1.LE.U0.AND.UC.GT.U0)) THEN
               NAX=N1X
               NAY=N1Y
               UA=U1
               NBX=-NX
               NBY=-NY
               UB=UC
               NSAX=N2X
               NSAY=N2Y
               USA=U2
               NSBX=-NX
               NSBY=-NY
               USB=UC
               MODE=2
               MODES=1
            ELSE
               GOTO 1000
            ENDIF
C
            CALL SUB(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,1)
            NAXQ=NAX
            NAYQ=NAY
            NBXQ=NBX
            NBYQ=NBY
            UAQ=UA
            UBQ=UB
C
            IEL=IE
            NEL=NE
            NXL=NX
            NYL=NY
            UCL=UC
            MODEL=MODE
            LINV=.FALSE.
            LEND=.FALSE.
C
  200       IF(MODEL.EQ.1) THEN
               NEL=NEL+1
               IF(NEL.EQ.5) NEL=1
            ELSEIF(MODEL.EQ.2) THEN
               NEL=NEL-1
               IF(NEL.EQ.0) NEL=4
            ELSE
               IF(NEL.EQ.1) THEN
                  NYL=NYL-1
               ELSEIF(NEL.EQ.2) THEN
                  NXL=NXL+1
               ELSEIF(NEL.EQ.3) THEN
                  NYL=NYL+1
               ELSE
                  NXL=NXL-1
               ENDIF
               IF(NXL.GE.1.AND.NXL.LE.NXM.AND.
     &            NYL.GE.1.AND.NYL.LE.NYM) THEN
                  NEL=NEL+2
                  IF(NEL.GT.4) NEL=NEL-4
                  I1X=NXL
                  I1Y=NYL
                  IF(I1X.GT.NXMAX) I1X=1
                  IF(I1Y.GT.NYMAX) I1Y=1
                  I2X=NXL+1
                  I2Y=NYL+1
                  IF(I2X.GT.NXMAX) I2X=1
                  IF(I2Y.GT.NYMAX) I2Y=1
                  ZC=0.25*(Z(I1X,I1Y)+Z(I2X,I1Y)+Z(I2X,I2Y)+Z(I1X,I2Y))
                  UCL=(ZC-ZORG)/ZSTEP+ZBIAS
               ENDIF
            ENDIF
C
            IF(NXL.GE.1.AND.NXL.LE.NXM.AND.
     &         NYL.GE.1.AND.NYL.LE.NYM) THEN
               IEL=(NXM*(NYL-1)+NXL-1)*4+NEL
               IF(NEL.EQ.1) THEN
                  N1X=NXL
                  N1Y=NYL
                  N2X=NXL+1
                  N2Y=NYL
               ELSEIF(NEL.EQ.2) THEN
                  N1X=NXL+1
                  N1Y=NYL
                  N2X=NXL+1
                  N2Y=NYL+1
               ELSEIF(NEL.EQ.3) THEN
                  N1X=NXL+1
                  N1Y=NYL+1
                  N2X=NXL
                  N2Y=NYL+1
               ELSE
                  N1X=NXL
                  N1Y=NYL+1
                  N2X=NXL
                  N2Y=NYL
               ENDIF
               I1X=N1X
               I1Y=N1Y
               IF(I1X.GT.NXMAX) I1X=1
               IF(I1Y.GT.NYMAX) I1Y=1
               U1=(Z(I1X,I1Y)-ZORG)/ZSTEP+ZBIAS
               I2X=N2X
               I2Y=N2Y
               IF(I2X.GT.NXMAX) I2X=1
               IF(I2Y.GT.NYMAX) I2Y=1
               U2=(Z(I2X,I2Y)-ZORG)/ZSTEP+ZBIAS
C
               IF(MODEL.EQ.3) THEN
                  NAX=-NXL
                  NAY=-NYL
                  UA=UCL
                  IF((U2.GT.U0.AND.UCL.LE.U0).OR.
     &               (U2.LE.U0.AND.UCL.GT.U0)) THEN
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=1
                  ELSEIF((U1.GT.U0.AND.UCL.LE.U0).OR.
     &                   (U1.LE.U0.AND.UCL.GT.U0)) THEN
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=2
                  ELSE
                     LEND=.TRUE.
                  ENDIF
               ELSE IF(MODEL.EQ.2) THEN
                  NAX=N1X
                  NAY=N1Y
                  UA=U1
                  IF((U1.GT.U0.AND.UCL.LE.U0).OR.
     &               (U1.LE.U0.AND.UCL.GT.U0)) THEN
                     NBX=-NXL
                     NBY=-NYL
                     UB=UCL
                     MODEL=2
                  ELSEIF((U1.GT.U0.AND.U2.LE.U0).OR.
     &                   (U1.LE.U0.AND.U2.GT.U0)) THEN
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=3
                  ELSE
                     LEND=.TRUE.
                  ENDIF
               ELSE IF(MODEL.EQ.1) THEN
                  NAX=N2X
                  NAY=N2Y
                  UA=U2
                  IF((U2.GT.U0.AND.U1.LE.U0).OR.
     &               (U2.LE.U0.AND.U1.GT.U0)) THEN
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=3
                  ELSEIF((U2.GT.U0.AND.UCL.LE.U0).OR.
     &                   (U2.LE.U0.AND.UCL.GT.U0)) THEN
                     NBX=-NXL
                     NBY=-NYL
                     UB=UCL
                     MODEL=1
                  ELSE
                     LEND=.TRUE.
                  ENDIF
               ENDIF
            ELSE
               IF(LINV) THEN
                  LEND=.TRUE.
               ELSE
C
                  CALL SUB(NAXQ,NAYQ,NBXQ,NBYQ,U0,UAQ,UBQ,X,Y,IPAT,-1)
C
                  NAX=NSAX
                  NAY=NSAY
                  UA=USA
                  NBX=NSBX
                  NBY=NSBY
                  UB=USB
                  IEL=IE
                  NXL=NX
                  NYL=NY
                  NEL=NE
                  UCL=UC
                  MODEL=MODES
                  LINV=.TRUE.
               ENDIF
            ENDIF
            IF(.NOT.LEND) THEN
C
               CALL SUB(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,0)
C
               KA(1,IEL)=KA(1,IEL)+1
               IF(IEL.EQ.IE.AND..NOT.LINV) LEND=.TRUE.
            ENDIF
C
            IF(.NOT.LEND) GOTO 200
C
         ENDIF
 1000 CONTINUE
      RETURN
      END
C
      FUNCTION INTCLIP(G)
      INTEGER INTCLIP
      REAL G
      IF(G.GT.1.D5) then
         INTCLIP= 100000
      ELSEIF(G.LT.-1.D5) then
         INTCLIP=-100000
      ELSE
         INTCLIP=INT(G)
      ENDIF
      RETURN
      END
