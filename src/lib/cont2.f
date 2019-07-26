C     $Id$
C
C     ***********************************************
C     ****** GSAF APLLICATION V3.5 : CONTOUR 2 ******
C     ***********************************************
C
C     ****** CONTOUR PLOT : XY, FIXED STEP, PATTERN ******
C
      SUBROUTINE CONTE1(Z,NXA,NXMAX,NYMAX,ZL,NSTEP,IPRD,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTV1
      DIMENSION Z(NXA,NYMAX),X(2),Y(2),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,1),ILN(1),WLN(1)
C
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      X(1)=(GXE-GXS)/REAL(NXM)
      Y(1)=(GYE-GYS)/REAL(NYM)
      X(2)=GXS
      Y(2)=GYS
      ILN(1)=IPAT
      CALL INQLNW(WLN(1))
      CALL INQRGB(RGB(1,1),RGB(2,1),RGB(3,1))
C
      CALL CONTG0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,-1,IPRD,0,CONTV1,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : XY, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTE2(Z,X,Y,NXA,NXMAX,NYMAX,ZL,NSTEP,IPAT,IPRD,KA)
C
      IMPLICIT LOGICAL(L)
C
      EXTERNAL CONTV1
      DIMENSION Z(NXA,NYMAX),X(NXMAX),Y(NYMAX),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,1),ILN(1),WLN(1)
C
      ILN(1)=IPAT
      CALL INQLNW(WLN(1))
      CALL INQRGB(RGB(1,1),RGB(2,1),RGB(3,1))

      CALL CONTG0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,-1,IPRD,3,CONTV1,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE R, PATTERN ******
C
      SUBROUTINE CONTE3(Z,R,NXA,NXMAX,NYMAX,ZL,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTV2
      DIMENSION Z(NXA,NYMAX),R(NXMAX),T(2),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,1),ILN(1),WLN(1)
C
      RMAX=R(NXMAX)
      T(1)=2*3.1415926/NYMAX
      T(2)=0.0
      ILN(1)=IPAT
      CALL INQLNW(WLN(1))
      CALL INQRGB(RGB(1,1),RGB(2,1),RGB(3,1))
C
      CALL CONTG0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,-1,2,1,CONTV2,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTE4(Z,R,T,NXA,NXMAX,NYMAX,ZL,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTV2
      DIMENSION Z(NXA,NYMAX),R(NXMAX),T(NYMAX),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,1),ILN(1),WLN(1)
C
      RMAX=R(NXMAX)
      ILN(1)=IPAT
      CALL INQLNW(WLN(1))
      CALL INQRGB(RGB(1,1),RGB(2,1),RGB(3,1))
C
      CALL CONTG0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,-1,0,3,CONTV2,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : XY, FIXED STEP, PATTERN ******
C
      SUBROUTINE CONTG1(Z,NXA,NXMAX,NYMAX,
     &                  ZL,RGB,ILN,WLN,NSTEP,ISPL,IPRD,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTV1
      DIMENSION Z(NXA,NYMAX),X(2),Y(2),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP),ILN(NSTEP),WLN(NSTEP)
C
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      X(1)=(GXE-GXS)/REAL(NXM)
      Y(1)=(GYE-GYS)/REAL(NYM)
      X(2)=GXS
      Y(2)=GYS
C
      CALL CONTG0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,ISPL,IPRD,0,CONTV1,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : XY, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTG2(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZL,RGB,ILN,WLN,NSTEP,ISPL,IPRD,KA)
C
      IMPLICIT LOGICAL(L)
C
      EXTERNAL CONTV1
      DIMENSION Z(NXA,NYMAX),X(NXMAX),Y(NYMAX),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP),ILN(NSTEP),WLN(NSTEP)
C
      CALL CONTG0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,ISPL,IPRD,3,CONTV1,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE R, PATTERN ******
C
      SUBROUTINE CONTG3(Z,R,NXA,NXMAX,NYMAX,
     &                  ZL,RGB,ILN,WLN,NSTEP,ISPL,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTV2
      DIMENSION Z(NXA,NYMAX),R(NXMAX),T(2),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP),ILN(NSTEP),WLN(NSTEP)
C
      RMAX=R(NXMAX)
      T(1)=2*3.1415926/NYMAX
      T(2)=0.0
C
      CALL CONTG0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,ISPL,2,1,CONTV2,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : R-THETA, VARIABLE STEP, PATTERN ******
C
      SUBROUTINE CONTG4(Z,R,T,NXA,NXMAX,NYMAX,
     &                  ZL,RGB,ILN,WLN,NSTEP,ISPL,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTV2
      DIMENSION Z(NXA,NYMAX),R(NXMAX),T(NYMAX),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP),ILN(NSTEP),WLN(NSTEP)
C
      RMAX=R(NXMAX)
C
      CALL CONTG0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZL,RGB,ILN,WLN,NSTEP,ISPL,0,3,CONTV2,KA)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : COMMON SUB ******
C
      SUBROUTINE CONTG0(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZL,RGB,ILN,WLN,NSTEP,
     &                  ISPL,IPRD,INDX,SUBV,KA)
C
      IMPLICIT LOGICAL(L)
      EXTERNAL SUBV
      DIMENSION Z(NXA,NYMAX),X(NXMAX),Y(NYMAX),KA(2,NXMAX*NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP),ILN(NSTEP),WLN(NSTEP)
      DIMENSION ZLS(NSTEP),RGBS(3,NSTEP),ILNS(NSTEP),WLNS(NSTEP)
      PARAMETER (NFMAX=2000,NGMAX=4000)
      DIMENSION XF(NFMAX),YF(NFMAX)
      DIMENSION XP(NFMAX),YP(NFMAX)
      DIMENSION XG(NGMAX),YG(NGMAX)
C
      IF(ISPL.GE.0) THEN
         CALL INQRGB(RS,GS,BS)
         CALL INQLNW(WS)
      ENDIF
C
      KMAX=NSTEP
C
      IF(ZL(1).LE.ZL(KMAX)) THEN
         ZORG=ZL(1)
         DO 10 I=1,KMAX
            ZLS(I)=ZL(I)-ZORG
   10    CONTINUE
      ELSE
         ZORG=ZL(KMAX)
         DO 20 I=1,KMAX
            ZLS(I)=ZL(KMAX-I+1)-ZORG
   20    CONTINUE
      ENDIF
C
      IF(ISPL.GE.0) THEN
         IF(ZL(1).LE.ZL(KMAX)) THEN
            DO 30 I=1,KMAX
               RGBS(1,I)=RGB(1,I)
               RGBS(2,I)=RGB(2,I)
               RGBS(3,I)=RGB(3,I)
               ILNS(I)=ILN(I)
               WLNS(I)=WLN(I)
   30       CONTINUE
         ELSE
            DO 40 I=1,KMAX
               RGBS(1,I)=RGB(1,KMAX-I+1)
               RGBS(2,I)=RGB(2,KMAX-I+1)
               RGBS(3,I)=RGB(3,KMAX-I+1)
               ILNS(I)=ILN(KMAX-I+1)
               WLNS(I)=WLN(KMAX-I+1)
   40       CONTINUE
         ENDIF
      ELSE
         DO 50 I=1,KMAX
            ILNS(I)=ILN(1)
            WLNS(I)=WLN(1)
   50    CONTINUE
      ENDIF
C
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
C
      DO 70 NY=1,NYM
      DO 70 NX=1,NXM
         IE=NXM*(NY-1)+NX
         NXP=NX+1
         NYP=NY+1
         IF(NXP.GT.NXMAX) NXP=1
         IF(NYP.GT.NYMAX) NYP=1
         U1=Z(NX ,NY )-ZORG
         U2=Z(NXP,NY )-ZORG
         U3=Z(NXP,NYP)-ZORG
         U4=Z(NX ,NYP)-ZORG
         UMAX=MAX(U1,U2,U3,U4)
         UMIN=MIN(U1,U2,U3,U4)
         KA(1,IE)=1
         KA(2,IE)=0
         DO 60 K=1,KMAX
            IF(ZLS(K).LT.UMIN) KA(1,IE)=K+1
            IF(ZLS(K).LE.UMAX) KA(2,IE)=K
   60    CONTINUE
   70 CONTINUE
C
      DO 1000 K=1,KMAX
         U0=ZLS(K)
         IF(ISPL.GE.0) THEN
            CALL SETRGB(RGBS(1,K),RGBS(2,K),RGBS(3,K))
            CALL SETLNW(WLNS(K))
         ENDIF
      DO 1000 NY=1,NYM
      DO 1000 NX=1,NXM
         IE=NXM*(NY-1)+NX
         IF(ABS(KA(1,IE)).LE.K.AND.ABS(KA(2,IE)).GE.K) THEN
            N1X=NX
            N1Y=NY
            N2X=NX+1
            N2Y=NY
            N3X=NX+1
            N3Y=NY+1
            N4X=NX
            N4Y=NY+1
C     
            U1=Z(N1X,N1Y)-ZORG
            I2X=N2X
            I2Y=N2Y
            IF(I2X.GT.NXMAX) I2X=1
            IF(I2Y.GT.NYMAX) I2Y=1
            U2=Z(I2X,I2Y)-ZORG
            I3X=N3X
            I3Y=N3Y
            IF(I3X.GT.NXMAX) I3X=1
            IF(I3Y.GT.NYMAX) I3Y=1
            U3=Z(I3X,I3Y)-ZORG
            I4X=N4X
            I4Y=N4Y
            IF(I4X.GT.NXMAX) I4X=1
            IF(I4Y.GT.NYMAX) I4Y=1
            U4=Z(I4X,I4Y)-ZORG
            LDBL=.FALSE.
C
            IF((U1.GT.U0.AND.U2.LE.U0).OR.
     &         (U1.LE.U0.AND.U2.GT.U0)) THEN
               NAX=N1X
               NAY=N1Y
               UA=U1
               NBX=N2X
               NBY=N2Y
               UB=U2
               MODE=1
               IF((U2.GT.U0.AND.U3.LE.U0).OR.
     &            (U2.LE.U0.AND.U3.GT.U0)) THEN
                  NSAX=N2X
                  NSAY=N2Y
                  USA=U2
                  NSBX=N3X
                  NSBY=N3Y
                  USB=U3
                  MODES=2
                  IF((U3.GT.U0.AND.U4.LE.U0).OR.
     &               (U3.LE.U0.AND.U4.GT.U0)) LDBL=.TRUE.
               ELSEIF((U3.GT.U0.AND.U4.LE.U0).OR.
     &                (U3.LE.U0.AND.U4.GT.U0)) THEN
                  NSAX=N3X
                  NSAY=N3Y
                  USA=U3
                  NSBX=N4X
                  NSBY=N4Y
                  USB=U4
                  MODES=3
               ELSEIF((U4.GT.U0.AND.U1.LE.U0).OR.
     &                (U4.LE.U0.AND.U1.GT.U0)) THEN
                  NSAX=N4X
                  NSAY=N4Y
                  USA=U4
                  NSBX=N1X
                  NSBY=N1Y
                  USB=U1
                  MODES=4
               ELSE
                  WRITE(6,*) 'XX GSAF:CONT2 Logical error 1'
                  GOTO 1000
               ENDIF
            ELSEIF((U2.GT.U0.AND.U3.LE.U0).OR.
     &             (U2.LE.U0.AND.U3.GT.U0)) THEN
               NAX=N2X
               NAY=N2Y
               UA=U2
               NBX=N3X
               NBY=N3Y
               UB=U3
               MODE=2
               IF((U3.GT.U0.AND.U4.LE.U0).OR.
     &            (U3.LE.U0.AND.U4.GT.U0)) THEN
                  NSAX=N3X
                  NSAY=N3Y
                  USA=U3
                  NSBX=N4X
                  NSBY=N4Y
                  USB=U4
                  MODES=3
               ELSEIF((U4.GT.U0.AND.U1.LE.U0).OR.
     &                (U4.LE.U0.AND.U1.GT.U0)) THEN
                  NSAX=N4X
                  NSAY=N4Y
                  USA=U4
                  NSBX=N1X
                  NSBY=N1Y
                  USB=U1
                  MODES=4
               ELSE
                  WRITE(6,*) 'XX GSAF:CONT2 Logical error 2'
                  GOTO 1000
               ENDIF
            ELSEIF((U3.GT.U0.AND.U4.LE.U0).OR.
     &             (U3.LE.U0.AND.U4.GT.U0)) THEN
               NAX=N3X
               NAY=N3Y
               UA=U3
               NBX=N4X
               NBY=N4Y
               UB=U4
               MODE=3
               IF((U4.GT.U0.AND.U1.LE.U0).OR.
     &            (U4.LE.U0.AND.U1.GT.U0)) THEN
                  NSAX=N4X
                  NSAY=N4Y
                  USA=U4
                  NSBX=N1X
                  NSBY=N1Y
                  USB=U1
                  MODES=4
               ELSE
                  WRITE(6,*) 'XX GSAF:CONT2 Logical error 3'
                  GOTO 1000
               ENDIF
            ELSE
               GOTO 1000   ! no equi-contour in this element
            ENDIF

!    Initial element found

            IF(INDX.EQ.0.OR.INDX.EQ.2) THEN
               XA=X(1)*(NAX-1)+X(2)
               XB=X(1)*(NBX-1)+X(2)
            ELSE
               XA=X(NAX)
               XB=X(NBX)
            ENDIF
            IF(INDX.EQ.0.OR.INDX.EQ.1) THEN
               YA=Y(1)*(NAY-1)+Y(2)
               YB=Y(1)*(NBY-1)+Y(2)
            ELSE
               YA=Y(NAY)
               YB=Y(NBY)
            ENDIF
            J=1
            RT=(U0-UA)/(UB-UA)
            XF(J)=(XB-XA)*RT+XA
            YF(J)=(YB-YA)*RT+YA
            XF0=XF(J)
            YF0=YF(J)
C
            IEL=IE
            NXL=NX
            NYL=NY
            MODEL=MODE
            LINV=.FALSE.
            LEND=.FALSE.

  100       CONTINUE
C
            NXL1=NX
            NYL1=NY
            IF(MODEL.EQ.1) NYL1=NYL-1
            IF(MODEL.EQ.2) NXL1=NXL+1
            IF(MODEL.EQ.3) NYL1=NYL+1
            IF(MODEL.EQ.4) NXL1=NXL-1
C
            IF(NXL1.GE.1.AND.NXL1.LE.NXM.AND.
     &         NYL1.GE.1.AND.NYL1.LE.NYM) THEN
               IE1=NXM*(NYL1-1)+NXL1
            ELSE
               IE1=IE
            ENDIF
C
  200       IF(MODEL.EQ.1) NYL=NYL-1
            IF(MODEL.EQ.2) NXL=NXL+1
            IF(MODEL.EQ.3) NYL=NYL+1
            IF(MODEL.EQ.4) NXL=NXL-1
C
            IF(NXL.GE.1.AND.NXL.LE.NXM.AND.
     &         NYL.GE.1.AND.NYL.LE.NYM) THEN
               IEL=NXM*(NYL-1)+NXL
               N1X=NXL
               N1Y=NYL
               N2X=NXL+1
               N2Y=NYL
               N3X=NXL+1
               N3Y=NYL+1
               N4X=NXL
               N4Y=NYL+1
               U1=Z(N1X,N1Y)-ZORG
               I2X=N2X
               I2Y=N2Y
               IF(I2X.GT.NXMAX) I2X=1
               IF(I2Y.GT.NYMAX) I2Y=1
               U2=Z(I2X,I2Y)-ZORG
               I3X=N3X
               I3Y=N3Y
               IF(I3X.GT.NXMAX) I3X=1
               IF(I3Y.GT.NYMAX) I3Y=1
               U3=Z(I3X,I3Y)-ZORG
               I4X=N4X
               I4Y=N4Y
               IF(I4X.GT.NXMAX) I4X=1
               IF(I4Y.GT.NYMAX) I4Y=1
               U4=Z(I4X,I4Y)-ZORG
               LDBL=.FALSE.
C
               IF(MODEL.EQ.1) THEN
                  IF((U4.GT.U0.AND.U1.LE.U0).OR.
     &               (U4.LE.U0.AND.U1.GT.U0)) THEN
                     NAX=N4X
                     NAY=N4Y
                     UA=U4
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=4
                     IF((U1.GT.U0.AND.U2.LE.U0).OR.
     &                  (U1.LE.U0.AND.U2.GT.U0)) LDBL=.TRUE.
                  ELSEIF((U1.GT.U0.AND.U2.LE.U0).OR.
     &                   (U1.LE.U0.AND.U2.GT.U0)) THEN
                     NAX=N1X
                     NAY=N1Y
                     UA=U1
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=1
                  ELSEIF((U2.GT.U0.AND.U3.LE.U0).OR.
     &                   (U2.LE.U0.AND.U3.GT.U0)) THEN
                     NAX=N2X
                     NAY=N2Y
                     UA=U2
                     NBX=N3X
                     NBY=N3Y
                     UB=U3
                     MODEL=2
                  ELSE
                     WRITE(6,*) 'XX GSAF:CONT2 Logical error 5'
                     LEND=.TRUE.
                  ENDIF
               ELSE IF(MODEL.EQ.2) THEN
                  IF((U1.GT.U0.AND.U2.LE.U0).OR.
     &               (U1.LE.U0.AND.U2.GT.U0)) THEN
                     NAX=N1X
                     NAY=N1Y
                     UA=U1
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=1
                     IF((U2.GT.U0.AND.U3.LE.U0).OR.
     &                  (U2.LE.U0.AND.U3.GT.U0)) LDBL=.TRUE.
                  ELSEIF((U2.GT.U0.AND.U3.LE.U0).OR.
     &                   (U2.LE.U0.AND.U3.GT.U0)) THEN
                     NAX=N2X
                     NAY=N2Y
                     UA=U2
                     NBX=N3X
                     NBY=N3Y
                     UB=U3
                     MODEL=2
                  ELSEIF((U3.GT.U0.AND.U4.LE.U0).OR.
     &                   (U3.LE.U0.AND.U4.GT.U0)) THEN
                     NAX=N3X
                     NAY=N3Y
                     UA=U3
                     NBX=N4X
                     NBY=N4Y
                     UB=U4
                     MODEL=3
                  ELSE
                     WRITE(6,*) 'XX GSAF:CONT2 Logical error 6'
                     LEND=.TRUE.
                  ENDIF
               ELSEIF(MODEL.EQ.3) THEN
                  IF((U2.GT.U0.AND.U3.LE.U0).OR.
     &               (U2.LE.U0.AND.U3.GT.U0)) THEN
                     NAX=N2X
                     NAY=N2Y
                     UA=U2
                     NBX=N3X
                     NBY=N3Y
                     UB=U3
                     MODEL=2
                     IF((U3.GT.U0.AND.U4.LE.U0).OR.
     &                  (U3.LE.U0.AND.U4.GT.U0)) LDBL=.TRUE.
                  ELSEIF((U3.GT.U0.AND.U4.LE.U0).OR.
     &                   (U3.LE.U0.AND.U4.GT.U0)) THEN
                     NAX=N3X
                     NAY=N3Y
                     UA=U3
                     NBX=N4X
                     NBY=N4Y
                     UB=U4
                     MODEL=3
                  ELSEIF((U4.GT.U0.AND.U1.LE.U0).OR.
     &                   (U4.LE.U0.AND.U1.GT.U0)) THEN
                     NAX=N4X
                     NAY=N4Y
                     UA=U4
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=4
                  ELSE
                     WRITE(6,*) 'XX GSAF:CONT2 Logical error 7'
                     LEND=.TRUE.
                  ENDIF
               ELSEIF(MODEL.EQ.4) THEN
                  IF((U3.GT.U0.AND.U4.LE.U0).OR.
     &                   (U3.LE.U0.AND.U4.GT.U0)) THEN
                     NAX=N3X
                     NAY=N3Y
                     UA=U3
                     NBX=N4X
                     NBY=N4Y
                     UB=U4
                     MODEL=3
                     IF((U4.GT.U0.AND.U1.LE.U0).OR.
     &                  (U4.LE.U0.AND.U1.GT.U0)) LDBL=.TRUE.
                  ELSEIF((U4.GT.U0.AND.U1.LE.U0).OR.
     &                   (U4.LE.U0.AND.U1.GT.U0)) THEN
                     NAX=N4X
                     NAY=N4Y
                     UA=U4
                     NBX=N1X
                     NBY=N1Y
                     UB=U1
                     MODEL=4
                  ELSEIF((U1.GT.U0.AND.U2.LE.U0).OR.
     &               (U1.LE.U0.AND.U2.GT.U0)) THEN
                     NAX=N1X
                     NAY=N1Y
                     UA=U1
                     NBX=N2X
                     NBY=N2Y
                     UB=U2
                     MODEL=1
                  ELSE
                     WRITE(6,*) 'XX GSAF:CONT2 Logical error 8'
                     LEND=.TRUE.
                  ENDIF
               ENDIF
            ELSE
               IF(LINV) THEN   ! Both ends on boundary
                  LEND=.TRUE.
               ELSE    ! One end on boundary, draw and restart from initial
C
                  CALL GUSP2D(XF(1),YF(1),J,XP,YP,NFMAX,NP,ISPL)
                  CALL SUBV(XP,YP,NP,XG,YG,NGMAX,NN)
                  CALL LINEPT(XG,YG,NN,ILNS(K))
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
                  MODEL=MODES
                  LINV=.TRUE.
                  J=1
                  XF(J)=XF0
                  YF(J)=YF0
                  GO TO 100
               ENDIF
            ENDIF
C
            IF(.NOT.LEND) THEN  ! One point extended
C
               IF(INDX.EQ.0.OR.INDX.EQ.2) THEN
                  XA=X(1)*(NAX-1)+X(2)
                  XB=X(1)*(NBX-1)+X(2)
               ELSE
                  XA=X(NAX)
                  XB=X(NBX)
               ENDIF
               IF(INDX.EQ.0.OR.INDX.EQ.1) THEN
                  YA=Y(1)*(NAY-1)+Y(2)
                  YB=Y(1)*(NBY-1)+Y(2)
               ELSE
C                  IF(NAY.GE.NYM) NAY=NYM
C                  IF(NBY.GE.NYM) NBY=NYM
                  YA=Y(NAY)
                  YB=Y(NBY)
               ENDIF
               IF(J.EQ.NFMAX) THEN ! Buffer full and draw
                  CALL GUSP2D(XF(1),YF(1),J,XP,YP,NFMAX,NP,ISPL)
                  CALL SUBV(XP,YP,NP,XG,YG,NGMAX,NN)
                  IF(.NOT.LINV) THEN
                     CALL LINEPT(XG,YG,NN,ILNS(K))
                  ELSE
                     CALL LINEPT(XG,YG,NN,-ILNS(K))
                  END IF
                  J=1
                  XF(J)=XF(NFMAX)
                  YF(J)=YF(NFMAX)
               ENDIF
               J=J+1
               RT=(U0-UA)/(UB-UA)
               XF(J)=(XB-XA)*RT+XA
               YF(J)=(YB-YA)*RT+YA
C
               IF(KA(1,IEL).GE.0.AND.LDBL) THEN
                  KA(1,IEL)=-KA(1,IEL)
               ELSE
                  KA(1,IEL)=ABS(KA(1,IEL))+1
                  IF(IEL.EQ.IE) LEND=.TRUE.
               END IF
            ENDIF
C
            IF(.NOT.LEND) GOTO 200
C
            CALL GUSP2D(XF(1),YF(1),J,XP,YP,NFMAX,NP,ISPL)
            CALL SUBV(XP,YP,NP,XG,YG,NGMAX,NN)
            IF(.NOT.LINV) THEN
               CALL LINEPT(XG,YG,NN,ILNS(K))
            ELSE
               CALL LINEPT(XG,YG,NN,-ILNS(K))
            END IF

         ENDIF
 1000 CONTINUE
      IF(ISPL.GE.0) THEN
         CALL SETLNW(WS)
         CALL SETRGB(RS,GS,BS)
      ENDIF
      RETURN
      END
C
C     ****** CONTOUR PAINT : XY, FIXED STEP ******
C
      SUBROUTINE CONTF1(Z,NXA,NXMAX,NYMAX,ZL,RGB,NSTEP,IPRD)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      EXTERNAL CONTV1
      DIMENSION Z(NXA,NYMAX),ZL(NSTEP),RGB(3,0:NSTEP),X(2),Y(2)
C
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      X(1)=(GXE-GXS)/REAL(NXM)
      Y(1)=(GYE-GYS)/REAL(NYM)
      X(2)=GXS
      Y(2)=GYS
C
      CALL CONTF0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZL,RGB,NSTEP,IPRD,0,CONTV1)
C
      RETURN
      END
C
C     ****** CONTOUR PAINT : XY, VARIABLE STEP ******
C
      SUBROUTINE CONTF2(Z,X,Y,NXA,NXMAX,NYMAX,
     &                  ZL,RGB,NSTEP,IPRD)
C
      IMPLICIT LOGICAL(L)
C
      EXTERNAL CONTV1
      DIMENSION Z(NXA,NYMAX),X(NXMAX),Y(NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP)
C
      CALL CONTF0(Z,X,Y,NXA,NXMAX,NYMAX,
     &            ZL,RGB,NSTEP,IPRD,3,CONTV1)
C
      RETURN
      END
C
C     ****** CONTOUR PAINT : R-THETA, VARIABLE R STEP ******
C
      SUBROUTINE CONTF3(Z,R,NXA,NXMAX,NYMAX,ZL,RGB,NSTEP)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTV2
      DIMENSION Z(NXA,NYMAX),R(NXMAX),T(2)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP)
C
      RMAX=R(NXMAX)
      T(1)=2*3.1415926/NYMAX
      T(2)=0.0
C
      CALL CONTF0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZL,RGB,NSTEP,2,1,CONTV2)
C
      RETURN
      END
C
C     ****** CONTOUR PAINT : R-THETA, VARIABLE STEP ******
C
      SUBROUTINE CONTF4(Z,R,T,NXA,NXMAX,NYMAX,ZL,RGB,NSTEP)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
C
      EXTERNAL CONTV2
      DIMENSION Z(NXA,NYMAX),R(NXMAX),T(NYMAX)
      DIMENSION ZL(NSTEP),RGB(3,NSTEP)
C
      RMAX=R(NXMAX)
C
      CALL CONTF0(Z,R,T,NXA,NXMAX,NYMAX,
     &            ZL,RGB,NSTEP,0,3,CONTV2)
C
      RETURN
      END
C
C     ****** CONTOUR PAINT : COMMON SUB *******
C
      SUBROUTINE CONTF0(Z,X,Y,NXA,NX,NY,ZL,RGB,NSTEP,
     &                  IPRD,INDX,SUBV)
C
      IMPLICIT LOGICAL(L)
      EXTERNAL SUBV
      DIMENSION Z(NXA,NY),X(NX),Y(NY),ZL(NSTEP),RGB(3,0:NSTEP)
      DIMENSION XA(4),YA(4),ZA(4)
      DIMENSION XG(101),YG(101)
      DIMENSION XT(8,0:NSTEP),YT(8,0:NSTEP),JH(0:NSTEP),HT(0:NSTEP+1)
      DIMENSION RGBS(3,0:NSTEP)
C
      CALL INQRGB(RS,GS,BS)
C
      KMAX=NSTEP
C
      IF(ZL(1).LE.ZL(KMAX)) THEN
         RGBS(1,0)=RGB(1,0)
         RGBS(2,0)=RGB(2,0)
         RGBS(3,0)=RGB(3,0)
         DO 10 J=1,KMAX
            HT(J)=ZL(J)
            RGBS(1,J)=RGB(1,J)
            RGBS(2,J)=RGB(2,J)
            RGBS(3,J)=RGB(3,J)
   10    CONTINUE
      ELSE
         RGBS(1,0)=RGB(1,KMAX)
         RGBS(2,0)=RGB(2,KMAX)
         RGBS(3,0)=RGB(3,KMAX)
         DO 20 J=1,KMAX
            HT(J)=ZL(KMAX-J+1)
            RGBS(1,J)=RGB(1,KMAX-J+1)
            RGBS(2,J)=RGB(2,KMAX-J+1)
            RGBS(3,J)=RGB(3,KMAX-J+1)
   20    CONTINUE
      ENDIF
C
      HT(0)=-1.E35
      HT(KMAX+1)=1.E35
C
      NXMAX=NX-1
      NYMAX=NY-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXMAX=NX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYMAX=NY
C
      DO 100 J=1,NYMAX
      DO 100 I=1,NXMAX
         IF(INDX.EQ.0.OR.INDX.EQ.2) THEN
            XA(1)=X(1)*(I-1)+X(2)
            XA(2)=X(1)* I   +X(2)
         ELSE
            XA(1)=X(I)
            XA(2)=X(I+1)
         ENDIF
         XA(3)=XA(2)
         XA(4)=XA(1)
         IF(INDX.EQ.0.OR.INDX.EQ.1) THEN
            YA(1)=Y(1)*(J-1)+Y(2)
            YA(3)=Y(1)* J   +Y(2)
         ELSE
            YA(1)=Y(J)
            YA(3)=Y(J+1)
         ENDIF
         YA(2)=YA(1)
         YA(4)=YA(3)
C
         JJ=J+1
         IF(J.EQ.NY) JJ=1
         II=I+1
         IF(I.EQ.NX) II=1
         ZA(1)=Z(I, J )
         ZA(2)=Z(II,J )
         ZA(3)=Z(II,JJ)
         ZA(4)=Z(I, JJ)
C
         CALL CONTFX(XA,YA,ZA,4,XT,YT,JH,HT,KMAX)
C
         DO 100 IH=0,KMAX
            IF(JH(IH).GE.3) THEN
               CALL SETRGB(RGBS(1,IH),RGBS(2,IH),RGBS(3,IH))
               CALL SUBV(XT(1,IH),YT(1,IH),JH(IH),XG,YG,100,NN)
               XG(NN+1)=XG(1)
               YG(NN+1)=YG(1)
               CALL POLY(XG,YG,NN+1)
            ENDIF
  100 CONTINUE
C
      CALL SETRGB(RS,GS,BS)
      RETURN
      END
C
C     ****** CONTOUR PAINT : SLAVE ROUTINE ******
C
      SUBROUTINE CONTFX(X,Y,Z,N,XT,YT,JH,HT,KMAX)
C
      DIMENSION X(N),Y(N),Z(N)
      DIMENSION XT(8,0:KMAX),YT(8,0:KMAX),JH(0:KMAX),HT(0:KMAX+1)
C
      DO 100 IH=0,KMAX
         JH(IH)=0
  100 CONTINUE
C
      DO 1000 J=1,N
C
         IF(J.NE.N) THEN
            JL=J+1
         ELSE
            JL=1
         ENDIF
C
         IF(Z(J).LT.Z(JL)) THEN
            DO 200 IH=0,KMAX
               IF((Z(J).GE.HT(IH)).AND.
     &            (Z(J).LT.HT(IH+1))) THEN
                  IHS=IH
                  JH(IHS)=JH(IHS)+1
                  XT(JH(IHS),IHS)=X(J)
                  YT(JH(IHS),IHS)=Y(J)
               ELSEIF(Z(J).EQ.HT(IH)) THEN
                  IHS=IH-1
                  JH(IHS)=JH(IHS)+1
                  XT(JH(IHS),IHS)=X(J)
                  YT(JH(IHS),IHS)=Y(J)
                  IHS=IH
                  IHL=IH
                  JH(IHL)=JH(IHL)+1
                  XT(JH(IHL),IHL)=X(J)
                  YT(JH(IHL),IHL)=Y(J)
               ENDIF
               IF((Z(JL).GE.HT(IH)).AND.
     &            (Z(JL).LT.HT(IH+1))) THEN
                  IHL=IH
               ELSEIF(Z(JL).EQ.HT(IH)) THEN
                  IHL=IH-1
               ENDIF
  200       CONTINUE
C
            DO 300 IH=IHS+1,IHL
               RT=(HT(IH)-Z(J))/(Z(JL)-Z(J))
               XP=X(J)+(X(JL)-X(J))*RT
               YP=Y(J)+(Y(JL)-Y(J))*RT
               DO 400 IHV=IH-1,IH
                  JH(IHV)=JH(IHV)+1
                  XT(JH(IHV),IHV)=XP
                  YT(JH(IHV),IHV)=YP
  400          CONTINUE
  300       CONTINUE
C
         ELSEIF(Z(J).EQ.Z(JL)) THEN
C
            DO 500 IH=0,KMAX
               IF((Z(J).GE.HT(IH)).AND.
     &            (Z(J).LT.HT(IH+1))) THEN
                  JH(IH)=JH(IH)+1
                  XT(JH(IH),IH)=X(J)
                  YT(JH(IH),IH)=Y(J)
               ENDIF
  500       CONTINUE
C
         ELSE
C
            DO 600 IH=KMAX,0,-1
               IF((Z(J).GE.HT(IH)).AND.
     &            (Z(J).LT.HT(IH+1))) THEN
                  IHL=IH
                  JH(IHL)=JH(IHL)+1
                  XT(JH(IHL),IHL)=X(J)
                  YT(JH(IHL),IHL)=Y(J)
               ELSEIF(Z(J).EQ.HT(IH)) THEN
                  IHL=IH
                  JH(IHL)=JH(IHL)+1
                  XT(JH(IHL),IHL)=X(J)
                  YT(JH(IHL),IHL)=Y(J)
                  IHL=IH-1
                  IHS=IH-1
                  JH(IHS)=JH(IHS)+1
                  XT(JH(IHS),IHS)=X(J)
                  YT(JH(IHS),IHS)=Y(J)
               ENDIF
               IF((Z(JL).GE.HT(IH)).AND.
     &            (Z(JL).LT.HT(IH+1))) THEN
                  IHS=IH
               ELSEIF(Z(JL).EQ.HT(IH)) THEN
                  IHS=IH
               ENDIF
  600       CONTINUE
C
            DO 700 IH=IHL,IHS+1,-1
               RT=(HT(IH)-Z(JL))/(Z(J)-Z(JL))
               XP=X(JL)+(X(J)-X(JL))*RT
               YP=Y(JL)+(Y(J)-Y(JL))*RT
               DO 800 IHV=IH,IH-1,-1
                  JH(IHV)=JH(IHV)+1
                  XT(JH(IHV),IHV)=XP
                  YT(JH(IHV),IHV)=YP
  800          CONTINUE
  700       CONTINUE
         ENDIF
C
 1000 CONTINUE
      RETURN
      END
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : CONV XY ******
C
      SUBROUTINE CONTV1(XA,YA,N,XB,YB,M,NN)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION XA(N),YA(N),XB(M),YB(M)
C
      NN=N
      IF(NN.GT.M) THEN
         WRITE(6,*) 'XX GSAF CONTV1 ERROR: N .GT. M'
         NN=M
      END IF
      DO 100 I=1,NN
         XB(I)=DX*(XA(I)-GXS)+PXS
         YB(I)=DY*(YA(I)-GYS)+PYS
  100 CONTINUE
      RETURN
      END
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : CONV RT ******
C
      SUBROUTINE CONTV2(RA,TA,N,XB,YB,M,NN)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      COMMON /GSCTR4/ RMAX,RT,TT,XT,YT
      DIMENSION RA(N),TA(N),XB(M),YB(M)
C
      RT=RA(1)
      TT=TA(1)
      XT=DX*(RT*COS(TT)-GXS)+PXS
      YT=DY*(RT*SIN(TT)-GYS)+PYS
      J=1
      XB(J)=XT
      YB(J)=YT
C
      DO 100 I=2,N
         RS=RA(I)
         TS=TA(I)
         XS=DX*(RS*COS(TS)-GXS)+PXS
         YS=DY*(RS*SIN(TS)-GYS)+PYS
C
         IMAX=INT(SQRT((XS-XT)**2+(YS-YT)**2)*8/RMAX+1)
         DELR=(RS-RT)/IMAX
         DELT= TS-TT
         IF(DELT.GT. 4.0) DELT=DELT-2*3.1415926
         IF(DELT.LT.-4.0) DELT=DELT+2*3.1415926
         DELT=DELT/IMAX
         DO 80 K=1,IMAX
            RI=DELR*K+RT
            TI=DELT*K+TT
            XI=DX*(RI*COS(TI)-GXS)+PXS
            YI=DY*(RI*SIN(TI)-GYS)+PYS
            IF(J.GE.M) THEN
               WRITE(6,*) 'XX GSAF CONTV2 ERROR: BUFFER OVER'
               NN=J
               RETURN
            ENDIF
            J=J+1
            XB(J)=XI
            YB(J)=YI
   80    CONTINUE
         RT=RS
         TT=TS
         XT=XS
         YT=YS
  100 CONTINUE
      NN=J
C
C     ***** slightly expand the region *****
C
      X0=0.D0
      Y0=0.D0
      DO J=1,NN
         X0=X0+XB(J)
         Y0=Y0+YB(J)
      ENDDO
      X0=X0/NN
      Y0=Y0/NN
      DO J=1,NN
         S=SQRT((XB(J)-X0)**2+(YB(J)-Y0)**2)
         IF(S.EQ.0.D0) S=1.D0
         XB(J)=XB(J)+0.02*(XB(J)-X0)/S
         YB(J)=YB(J)+0.02*(YB(J)-Y0)/S
      ENDDO
      RETURN
      END
C
C     ****** DRAW LINES WITH PATTERN ******
C
      SUBROUTINE LINEPT(XG,YG,N,IPAT)
C
      DIMENSION XG(N),YG(N)
C
      CALL MOVEPT(XG(1),YG(1),IPAT)
      DO 100 I=2,N
         CALL DRAWPT(XG(I),YG(I))
  100 CONTINUE
      RETURN
      END
C
C     ****** SPLINE INTERPOLATION OF 2D LINES ******
C
      SUBROUTINE GUSP2D(XH,YH,N,XP,YP,NPM,NP,ISPL)
C
      PARAMETER(M=3)
      DIMENSION XH(N),YH(N),XP(NPM),YP(NPM)
      DIMENSION IKN(0-M:NPM+M)
C
      NQ=NPM
C
      IF((ABS(XH(N)-XH(1)).LT.1.E-30).AND.
     &   (ABS(YH(N)-YH(1)).LT.1.E-30)) THEN
         IOC=1
      ELSE
         IOC=0
      ENDIF
C
      IF((ISPL.LE.0).OR.((N.EQ.2).AND.(IOC.EQ.0))) THEN
         DO 1 I=1,N
            XP(I)=XH(I)
            YP(I)=YH(I)
    1    CONTINUE
         NP=N
      ELSEIF(N.GT.2) THEN
         NP=ISPL*N
         IF(NP.GT.NQ) NP=NQ
C
         IF(IOC.EQ.0) THEN
            DO 10 I=0-M,0
               IKN(I)=0
   10       CONTINUE
C
            DO 20 I=1,N-1
               IKN(I)=I
   20       CONTINUE
C
            DO 30 I=N,N+M-1
               IKN(I)=N-1
   30       CONTINUE
         ELSEIF(IOC.EQ.1) THEN
            DO 110 I=0-M,N+M-1
               IKN(I)=I
  110       CONTINUE
         ENDIF
         CALL GUCSPL(N-1,XH,YH,IKN,IOC,NP,XP,YP)
      ENDIF
      RETURN
      END
C
C     ****** SPLINE ******
C
      SUBROUTINE GUCSPL(N,X,Y,IKN,IOC,NP,XP,YP)
C
      PARAMETER (M=3)
      DIMENSION IKN(0-M:N+M),B(0-M:M,0:M)
      DIMENSION X(0:N),Y(0:N),XP(NP),YP(NP)
C
      H=(IKN(N)-IKN(0))/REAL(NP-1)
C
      DO 100 J=1,NP
         TP=IKN(0)+H*(J-1)
         CALL GUBSPL(TP,ITM,N,IKN,M,B)
         XV=0
         YV=0
         DO 10 I=0,0-M,-1
            IV=ITM+I+2
            IF(IOC.EQ.1) THEN
               IF(IV.LT.0) THEN
                  IV=IV+N
               ELSEIF(IV.GT.N) THEN
                  IV=IV-N
               ENDIF
            ENDIF
            XV=XV+X(IKN(IV))*B(I,M)
            YV=YV+Y(IKN(IV))*B(I,M)
   10    CONTINUE
         XP(J)=XV
         YP(J)=YV
  100 CONTINUE
      RETURN
      END
C
C     ****** B SPLINE ******
C
      SUBROUTINE GUBSPL(TP,ITM,N,IKN,M,B)
C
      DIMENSION IKN(0-M:N+M),B(0-M:M,0:M)
C
      DO 10 JT=N-1,0,-1
         IF(TP.GE.REAL(IKN(JT))) THEN
            ITM=JT
            GOTO 11
         ENDIF
   10 CONTINUE
   11 CONTINUE
C
      DO 20 K=0,M-1
      DO 20 I=-1-K,M-K
         B(I,K)=0.0
   20 CONTINUE
C
      B(0,0)=1.0
      DO 30 K=1,M
      DO 30 I=0-K,0
         IV=ITM+I
         IF(IKN(IV+K).GT.IKN(IV)) THEN
            B0=(TP-IKN(IV))/(IKN(IV+K)-IKN(IV)+1.E-30) 
     &           * B(I,K-1)
         ELSE
            B0=0
         ENDIF
C
         IF(IKN(IV+K+1).GT.IKN(IV+1)) THEN
            B1=(IKN(IV+K+1)-TP)/(IKN(IV+K+1)-IKN(IV+1)+1.E-30)
     &           *B(I+1,K-1)
         ELSE
            B1=0
         ENDIF
C
         B(I,K)=B0+B1
   30 CONTINUE
      RETURN
      END
C
C     ****** RGB COLOR PATERN ******
C
      SUBROUTINE RGBBAR(X1,X2,Y1,Y2,RGB,N,IND)
C
      DIMENSION RGB(3,N),X(5),Y(5)
C
      IF(IND.EQ.0) THEN
         DX=(X2-X1)/(N-1)
         DY=0.0
         DXL=0.0
         DYL=Y2-Y1
      ELSE
         DX=0.0
         DY=(Y2-Y1)/(N-1)
         DXL=X2-X1
         DYL=0.0
      ENDIF
      DO 100 I=1,N
         X(1)=X1+DX*(I-1)
         Y(1)=Y1+DY*(I-1)
         X(2)=X1+DX*I+DXL
         Y(2)=Y(1)
         X(3)=X(2)
         Y(3)=Y1+DY*I+DYL
         X(4)=X(1)
         Y(4)=Y(3)
         X(5)=X(1)
         Y(5)=Y(1)
         CALL SETRGB(RGB(1,I),RGB(2,I),RGB(3,I))
         CALL POLY(X,Y,5)
  100 CONTINUE
C
      X(1)=X1
      Y(1)=Y1
      X(2)=X2
      Y(2)=Y1
      X(3)=X2
      Y(3)=Y2
      X(4)=X1
      Y(4)=Y2
      X(5)=X1
      Y(5)=Y1
      CALL SETRGB(0.0,0.0,0.0)
      CALL LINES(X,Y,5)
C
      RETURN
      END
