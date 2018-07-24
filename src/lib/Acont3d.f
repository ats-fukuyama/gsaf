C     $Id$
C     *************************************************
C     ********** GSAF 3D ROUTINES : Contour ***********
C     *************************************************
C
C     言葉の説明：
C        3次元座標での値．．．ユーザが与えた座標での値
C        3次元での cm   ．．．gdefine3d で与えられる
C                       xl1,yl1,zl1 を元にした長さ(位置)
C        2次元での cm   ．．．実際に描画される長さ(位置)
C
C
C     ****** CONTOUR PLOT : XY, FIXED STEP, PATTERN ******
C
      SUBROUTINE CONTQ3D1(ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,RGBFUNC,IZ)
C
      EXTERNAL RGBFUNC
      EXTERNAL CONTS3D1
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXMAX,NYMAX
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /CONTZV/ ZV
      COMMON /ANGL3D/ PHI,THETA
      DIMENSION KA(2,*),DELX(1),DELY(1)
      DATA EPS/1.E-32/
C
      CALL GTTTDATA
      CALL gtMkZBuffer
C
      IF (IZ.EQ.0) THEN
         ZV=ZMIN
      ELSE IF (IZ.EQ.1) THEN
         ZV=ZMAX
      ELSE IF (IZ.EQ.2) THEN
         IF (THETA.GE.90.0) THEN
            ZV=ZMAX
         ELSE
            ZV=ZMIN
         ENDIF
      ELSE IF (IZ.EQ.3) THEN
         IF (THETA.GE.90.0) THEN
            ZV=ZMIN
         ELSE
            ZV=ZMAX
         ENDIF
      ENDIF
C         
      IF(ABS(ZSTEP).LT.EPS) RETURN
      NXM=NXMAX-1
      NYM=NYMAX-1
      IF(IPRD.EQ.1.OR.IPRD.EQ.3) NXM=NXMAX
      IF(IPRD.EQ.2.OR.IPRD.EQ.3) NYM=NYMAX
      DELX(1)=(GXE-GXS)/REAL(NXM)
      DELY(1)=(GYE-GYS)/REAL(NYM)
C
      CALL CONTQ3D0(DELX,DELY,ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,CONTS3D1,
     &     RGBFUNC)
C
      RETURN
      END
C
C     ****** CONTOUR PLOT : COMMON SUB (4 TRIANGLE) ******
C
      SUBROUTINE CONTQ3D0(X,Y,ZORG,ZSTEP,NSTEP,IPRD,IPAT,KA,SUB,RGBFUNC)
C
      IMPLICIT LOGICAL(L)
C
      EXTERNAL SUB
      EXTERNAL RGBFUNC
      INCLUDE 'A3dcomm.inc'
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXMAX,NYMAX
      COMMON /TDATA1/ XDATA(NXDMP),YDATA(NYDMP),Z(NXDMP,NYDMP)
      DIMENSION X(*),Y(*),KA(2,*)
      DIMENSION RGB(3)
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
         K1=INT((Z(NX ,NY )-ZORG)/ZSTEP+ZBIAS)
         K2=INT((Z(NXP,NY )-ZORG)/ZSTEP+ZBIAS)
         K3=INT((Z(NXP,NYP)-ZORG)/ZSTEP+ZBIAS)
         K4=INT((Z(NX ,NYP)-ZORG)/ZSTEP+ZBIAS)
         ZC=0.25*(Z(NX,NY)+Z(NXP,NY)+Z(NXP,NYP)+Z(NX,NYP))
         KC=INT((ZC        -ZORG)/ZSTEP+ZBIAS)
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
C      IEMAX=IE
C
      CALL INQRGB(CR,CG,CB)
      DO 1000 K=1,KMAX
         U0=REAL(K+1)
         COLOR=(ZSTEP*(K-1)+ZORG-ZMIN)/(ZMAX-ZMIN)
         CALL RGBFUNC(COLOR,RGB)
         CALL SETRGB(RGB(1),RGB(2),RGB(3))
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
      CALL SETRGB(CR,CG,CB)
      RETURN
      END
C
C     ****** CONTOUR PLOT SLAVE ROUTINE : XY FIXED ******
C
      SUBROUTINE CONTS3D1(NAX,NAY,NBX,NBY,U0,UA,UB,X,Y,IPAT,IND)
C
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXMAX,NYMAX
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /CNTS3D/ XSA,YSA,IPATA
      COMMON /CONTZV/ ZV
      DIMENSION X(*),Y(*)
C
      IF(NAX.GT.0) THEN
         XA=X(1)*(NAX-1)
         YA=Y(1)*(NAY-1)
      ELSE
         XA=X(1)*(-NAX-0.5)
         YA=Y(1)*(-NAY-0.5)
      ENDIF
      IF(NBX.GT.0) THEN
         XB=X(1)*(NBX-1)
         YB=Y(1)*(NBY-1)
      ELSE
         XB=X(1)*(-NBX-0.5)
         YB=Y(1)*(-NBY-0.5)
      ENDIF
      XS1=(XB-XA)*(U0-UA)/(UB-UA)+XA+GXS
      YS1=(YB-YA)*(U0-UA)/(UB-UA)+YA+GYS
C
C      IF(IND.EQ.1) THEN
C         CALL MOVEPT(XS,YS,IPAT)
C      ELSEIF(IND.EQ.-1) THEN
C         CALL MOVEPT(XS,YS,-IPAT)
C      ELSEIF(IND.EQ.0) THEN
C         CALL DRAWPT(XS,YS)
C      ENDIF
      IF(IND.EQ.1) THEN
         XSA=XS1
         YSA=YS1
         IPATA=IPAT
      ELSEIF(IND.EQ.-1) THEN
         XSA=XS1
         YSA=YS1
         IPATA=-IPAT
      ELSEIF(IND.EQ.0) THEN
         CALL HLINPT3D(XSA,YSA,ZV,XS1,YS1,ZV,IPATA)
         XSA=XS1
         YSA=YS1
      ENDIF
      RETURN
      END
