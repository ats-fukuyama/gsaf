C     *************************************************
C     ************ GSAF 3D ROUTINES : BASIC ***********
C     *************************************************
C
C     言葉の説明：
C        3次元座標での値．．．ユーザが与えた座標での値
C        3次元での cm   ．．．gdefine3d で与えられる
C                       xl1,yl1,zl1 を元にした長さ(位置)
C        2次元での cm   ．．．実際に描画される長さ(位置)
C
C     ****** GDEFIN FOR 3D PLOT ******
C     PX*,PY* で描画領域の指定．
C     *L1 でそれぞれの軸の長さ (cm) の指定．
C
C     ここでは common ブロックにいれているだけ．
C
      SUBROUTINE GDEFIN3D(PX1,PX2,PY1,PY2,XL1,YL1,ZL1)
C
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN3D/ PXMIN,PXMAX,PYMIN,PYMAX
      COMMON /FRAG3D/ NTRN,NDFN,NZBUF
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
C
      XL=XL1
      YL=YL1
      ZL=ZL1
      PXMIN=PX1
      PXMAX=PX2
      PYMIN=PY1
      PYMAX=PY2
      DOX=0.5*(PX1+PX2)
      DOY=0.5*(PY1+PY2)
C     
      NDFN=0
C
      RETURN
      END
C
C     ****** SET VIEW-POINT FOR 3D PLOT ******
C
C     グラフを見る視点を定める．
C     phi (rad),theta(rad),radius(3次元でのcm) は極座標．
C     R1 はグラフの拡大縮小率．
C
      SUBROUTINE GVIEW3D(PHI1,THETA1,RADIUS1,R1,IAXIS1,OX1,OY1,OZ1)
C
      COMMON /ANGL3D/ PHIW,THETAW
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /FRAG3D/ NTRN,NDFN,NZBUF
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /GTRN3D/ R,IAXIS
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
C
C     PHI   should be 0.0 <= phi < 360.0
C     THETA should be 0.0 <= theta < 180.0
C
      PHI=PHI1
      THETA=THETA1
      IF (THETA.GE.180.0) THEN
         CT1=180.0-AMOD(THETA,180.0)
         CT2=AMOD(THETA,360.0)
         THETA=CT1
         IF (CT2.GE.180.0) PHI=PHI+180.0
      ELSE IF (THETA.LT.0.0) THEN
         THETA=-THETA
         CT1=180.0-AMOD(THETA,180.0)
         CT2=AMOD(THETA,360.0)
         THETA=CT1
         IF (CT2.GE.180.0) PHI=PHI+180.0
      ENDIF
C
      IF (PHI.GE.360.0) THEN
         PHI=AMOD(PHI,360.0)
      ELSE IF (PHI.LT.0.0) THEN
         PHI=-PHI
         PHI=360.0-AMOD(PHI,360.0)
      ENDIF
      PHIW=PHI
      THETAW=THETA
C
C     ここは perse との互換性のため．
C
      RAD=3.141593/180.0
      CA=COS(RAD*PHI)
      SA=SIN(RAD*PHI)
      CC=COS(RAD*THETA)
      SC=SIN(RAD*THETA)
C
C     common ブロックへ．
C
      EL=RADIUS1
      OX=OX1
      OY=OY1
      OZ=OZ1
C
C     if GDATAn(n=1,2...) is already called, next is to be done.
C
      NDFN=NDFN+1
      IF (NDFN.EQ.2) THEN
C
C     XTMIN,XTMAX,YTMIN,YTMAX の初期化．
C         
         XTMIN1=0.0
         YTMIN1=0.0
         DELX1=0.0
         DELY1=0.0
         RXY1=1.0
         CALL GTTTB(XDATA(1),YDATA(1),ZDATA(1,1),XTMIN,YTMIN)
         XTMAX=XTMIN
         YTMAX=YTMIN
         CALL GTRNS3D(R1,IAXIS1,1)
         CALL GTPREPFP
      ELSE
         R=R1
         IAXIS=IAXIS1
      ENDIF
C
      RETURN
      END
C
C     ****** PREPARE FOR PERSE2 & LINE3D ******
C
      SUBROUTINE GTPREPFP
C
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /ANGL3D/ PHI,THETA
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /COM3D1/ CAP,SAP,ELP,ETP
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
      RAD=3.141593/180.0
      A=-90.0-PHI+360.0
      IF (A.GT.180.0) THEN
         A=A-360.0
      ENDIF     
      OX2=(OX-GXS)*XFCTR
      OY2=(OY-GYS)*YFCTR
      IF (A.GE.0.0.AND.A.LT.90.0) THEN
         D=0.0
      ELSE IF (A.GE.-90.0.AND.A.LT.0.0) THEN
         D=(XL-OX2)*SIN(RAD*A)
      ELSE IF (A.GE.-180.0.AND.A.LT.-90.0) THEN
         D=(XL-OX2)*SIN(RAD*A)+(YL-OY2)*COS(RAD*A)
      ELSE IF (A.GE.90.0.AND.A.LE.180.0) THEN
         D=(YL-OY2)*COS(RAD*A)
      ENDIF
      E=EL*SIN(RAD*THETA)
C
      CAP=COS(RAD*A)
      SAP=SIN(RAD*A)
      ELP=E
      ETP=E+D
C
      RETURN
      END
C
C     ****** PUT DATA FOR 3D PLOT TO COMMON VARIABLE ******
C
      SUBROUTINE GDATA3D1(Z,NXA,NX1,NY1,XMIN1,XMAX1,YMIN1,YMAX1,
     &     ZMIN1,ZMAX1)
C
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /POSSCL/ I1X,I1Y,I1Z,SCLX,SCLY,SCLZ
      COMMON /GDFN2D/ XMIN,XMAX,YMIN,YMAX
      COMMON /FRAG3D/ NTRN,NDFN,NZBUF
      COMMON /GTRN3D/ R,IAXIS
C      COMMON /FSCL3D/ NSCLX(4),NSCLY(4),NSCLZ(4)
C      COMMON /FVAL3D/ NVALX(4),NVALY(4),NVALZ(4)
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
C
      DIMENSION Z(NXA,*)
C
      NX=NX1
      NY=NY1
      XMIN=XMIN1
      XMAX=XMAX1
      YMIN=YMIN1
      YMAX=YMAX1
      ZMIN=ZMIN1
      ZMAX=ZMAX1
      XFCTR=XL/(XMAX-XMIN)
      YFCTR=YL/(YMAX-YMIN)
      ZFCTR=ZL/(ZMAX-ZMIN)
C
      DO 10 IX=1,NX
      DO 10 IY=1,NY
         IF (Z(IX,IY).GT.ZMAX) THEN
            ZDATA(IX,IY)=ZMAX
         ELSE IF(Z(IX,IY).LT.ZMIN) THEN
            ZDATA(IX,IY)=ZMIN
         ELSE
            ZDATA(IX,IY)=Z(IX,IY)
         ENDIF
 10   CONTINUE
      DX=(XMAX-XMIN)/FLOAT(NX-1)
      DO 20 IX=1,NX
         XDATA(IX)=XMIN+REAL(IX-1)*DX
 20   CONTINUE
      DY=(YMAX-YMIN)/FLOAT(NY-1)
      DO 30 IY=1,NY
         YDATA(IY)=YMIN+REAL(IY-1)*DY
 30   CONTINUE
C
      NTRN=0
      NZBUF=0
      I1X=0
      I1Y=0
      I1Z=0
      SCLX=0
      SCLY=0
      SCLZ=0
C      DO I=1,4
C         NSCLX(I)=0
C         NSCLY(I)=0
C         NSCLZ(I)=0
C         NVALX(I)=0
C         NVALY(I)=0
C         NVALZ(I)=0
C      ENDDO
C
C     if gview3d is already called, next is to be done.
C
      NDFN=NDFN+1
      IF (NDFN.EQ.2) THEN
         XTMIN1=0.0
         YTMIN1=0.0
         DELX1=0.0
         DELY1=0.0
         RXY1=1.0
         CALL GTTTB(XDATA(1),YDATA(1),ZDATA(1,1),XTMIN,YTMIN)
         XTMAX=XTMIN
         YTMAX=YTMIN
         CALL GTRNS3D(R,IAXIS,1)
         CALL GTPREPFP
      ENDIF
C
 9999 RETURN
      END
C
C     ****** TRANSLATE,MAGNIFICATE or REDUCE DATA & AXISES ******
C
      SUBROUTINE GTRNS3D(R1,IAXIS1,IDATA)
C ---------------------------------------------------------------
C     IAXIS<0 平行移動及び拡大、縮小をしない。
C     IAXIS=0 グラフ本体のみを収めるようにする．
C     IAXIS=1 奥側の軸のみ考慮にいれる．
C     IAXIS=2 全ての軸を考慮にいれる．
C --------------------------------------------------------------
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /ANGL3D/ PHI,THETA
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /GDFN3D/ PXMIN,PXMAX,PYMIN,PYMAX
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
      COMMON /CORNER/ CORNERPOINTS(2,8)
C
      INUM=0
 50   IF (IAXIS1.LT.0) GOTO 500
      IAXIS=MOD(IAXIS1,3)
C      IF (IAXIS1.GE.3) THEN
C         IAXIS2=1
C      ELSE
C         IAXIS2=0
C      ENDIF
      IF (IAXIS.EQ.0) GOTO 500
      IF (IDATA.EQ.1) THEN
         Z1=ZMIN
         Z2=ZMAX
         IF (THETA.GT.90.0) THEN
            Z1=ZMAX
            Z2=ZMIN
         ENDIF
         IF (PHI.LT.90.0) THEN
            GOTO 1
         ELSE IF (PHI.LT.180.0) THEN
            GOTO 2
         ELSE IF (PHI.LT.270.0) THEN
            GOTO 3
         ELSE IF (PHI.LT.360.0) THEN
            GOTO 4
         ENDIF
      ENDIF
C
 1    CALL GTTTA(GXE,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYS,Z2,AA,BB)
      CALL GTTTA(GXS,GYS,Z2,AA,BB)
      CALL GTTTA(GXS,GYE,Z2,AA,BB)
      IF (IAXIS.EQ.1) GOTO 500
      CALL GTTTA(GXE,GYE,Z2,AA,BB)
      GOTO 500
C
 2    CALL GTTTA(GXE,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYS,Z2,AA,BB)
      CALL GTTTA(GXS,GYS,Z2,AA,BB)
      CALL GTTTA(GXE,GYE,Z2,AA,BB)
      IF (IAXIS.EQ.1) GOTO 500
      CALL GTTTA(GXS,GYE,Z2,AA,BB)
      GOTO 500
C     
 3    CALL GTTTA(GXE,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYS,Z2,AA,BB)
      CALL GTTTA(GXS,GYE,Z2,AA,BB)
      CALL GTTTA(GXE,GYE,Z2,AA,BB)
      IF (IAXIS.EQ.1) GOTO 500
      CALL GTTTA(GXS,GYS,Z2,AA,BB)
      GOTO 500
C  
 4    CALL GTTTA(GXE,GYS,Z1,AA,BB)
      CALL GTTTA(GXE,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYE,Z1,AA,BB)
      CALL GTTTA(GXS,GYS,Z1,AA,BB)
      CALL GTTTA(GXS,GYE,Z2,AA,BB)
      CALL GTTTA(GXS,GYS,Z2,AA,BB)
      CALL GTTTA(GXE,GYE,Z2,AA,BB)
      IF (IAXIS.EQ.1) GOTO 500
      CALL GTTTA(GXE,GYS,Z2,AA,BB)
C
 500  DO 100 IX=1,NX
      DO 100 IY=1,NY
         CALL GTTTA(XDATA(IX),YDATA(IY),ZDATA(IX,IY),AA,BB)
 100  CONTINUE
      IF (IAXIS1.LT.0) GOTO 999
C     
      IF (INUM.EQ.1) GOTO 999
      IF (R1.LT.0.0) THEN
         RXY1=1.0
         DELX1=.5*(PXMIN+PXMAX-ABS(XTMAX-XTMIN))
         DELY1=.5*(PYMIN+PYMAX-ABS(YTMAX-YTMIN))
      ELSE
         RX=ABS((PXMAX-PXMIN)/(XTMAX-XTMIN))
         RY=ABS((PYMAX-PYMIN)/(YTMAX-YTMIN))
         RXY1=R1*MIN(RX,RY)
         DELX1=.5*(PXMIN+PXMAX-RXY1*ABS(XTMAX-XTMIN))
         DELY1=.5*(PYMIN+PYMAX-RXY1*ABS(YTMAX-YTMIN))
      ENDIF
      XTMIN1=XTMIN
      YTMIN1=YTMIN
      XTMIN=.5*(PXMIN+PXMAX)
      XTMAX=XTMIN
      YTMIN=.5*(PYMIN+PYMAX)
      YTMAX=YTMIN
C
 999  IF (INUM.EQ.0) THEN
         INUM=1
         GOTO 50
      ENDIF
C    
            Z1=ZMIN
      Z2=ZMAX
      IF (PHI.LT.90.0) THEN
         GOTO 11
      ELSE IF (PHI.LT.180.0) THEN
         GOTO 12
      ELSE IF (PHI.LT.270.0) THEN
         GOTO 13
      ELSE IF (PHI.LT.360.0) THEN
         GOTO 14
      ENDIF
C
 11   CALL GTTTB(GXE,GYS,Z1,CORNERPOINTS(1,1),CORNERPOINTS(2,1))
      CALL GTTTB(GXE,GYE,Z1,CORNERPOINTS(1,2),CORNERPOINTS(2,2))
      CALL GTTTB(GXS,GYE,Z1,CORNERPOINTS(1,3),CORNERPOINTS(2,3))
      CALL GTTTB(GXS,GYS,Z1,CORNERPOINTS(1,4),CORNERPOINTS(2,4))
      CALL GTTTB(GXE,GYS,Z2,CORNERPOINTS(1,5),CORNERPOINTS(2,5))
      CALL GTTTB(GXE,GYE,Z2,CORNERPOINTS(1,6),CORNERPOINTS(2,6))
      CALL GTTTB(GXS,GYE,Z2,CORNERPOINTS(1,7),CORNERPOINTS(2,7))
      CALL GTTTB(GXS,GYS,Z2,CORNERPOINTS(1,8),CORNERPOINTS(2,8))
      GOTO 20
C
 12   CALL GTTTB(GXE,GYE,Z1,CORNERPOINTS(1,1),CORNERPOINTS(2,1))
      CALL GTTTB(GXS,GYE,Z1,CORNERPOINTS(1,2),CORNERPOINTS(2,2))
      CALL GTTTB(GXS,GYS,Z1,CORNERPOINTS(1,3),CORNERPOINTS(2,3))
      CALL GTTTB(GXE,GYS,Z1,CORNERPOINTS(1,4),CORNERPOINTS(2,4))
      CALL GTTTB(GXE,GYE,Z2,CORNERPOINTS(1,5),CORNERPOINTS(2,5))
      CALL GTTTB(GXS,GYE,Z2,CORNERPOINTS(1,6),CORNERPOINTS(2,6))
      CALL GTTTB(GXS,GYS,Z2,CORNERPOINTS(1,7),CORNERPOINTS(2,7))
      CALL GTTTB(GXE,GYS,Z2,CORNERPOINTS(1,8),CORNERPOINTS(2,8))
      GOTO 20
C     
 13   CALL GTTTB(GXS,GYE,Z1,CORNERPOINTS(1,1),CORNERPOINTS(2,1))
      CALL GTTTB(GXS,GYS,Z1,CORNERPOINTS(1,2),CORNERPOINTS(2,2))
      CALL GTTTB(GXE,GYS,Z1,CORNERPOINTS(1,3),CORNERPOINTS(2,3))
      CALL GTTTB(GXE,GYE,Z1,CORNERPOINTS(1,4),CORNERPOINTS(2,4))
      CALL GTTTB(GXS,GYE,Z2,CORNERPOINTS(1,5),CORNERPOINTS(2,5))
      CALL GTTTB(GXS,GYS,Z2,CORNERPOINTS(1,6),CORNERPOINTS(2,6))
      CALL GTTTB(GXE,GYS,Z2,CORNERPOINTS(1,7),CORNERPOINTS(2,7))
      CALL GTTTB(GXE,GYE,Z2,CORNERPOINTS(1,8),CORNERPOINTS(2,8))
      GOTO 20
C  
 14   CALL GTTTB(GXS,GYS,Z1,CORNERPOINTS(1,1),CORNERPOINTS(2,1))
      CALL GTTTB(GXE,GYS,Z1,CORNERPOINTS(1,2),CORNERPOINTS(2,2))
      CALL GTTTB(GXE,GYE,Z1,CORNERPOINTS(1,3),CORNERPOINTS(2,3))
      CALL GTTTB(GXS,GYE,Z1,CORNERPOINTS(1,4),CORNERPOINTS(2,4))
      CALL GTTTB(GXS,GYS,Z2,CORNERPOINTS(1,5),CORNERPOINTS(2,5))
      CALL GTTTB(GXE,GYS,Z2,CORNERPOINTS(1,6),CORNERPOINTS(2,6))
      CALL GTTTB(GXE,GYE,Z2,CORNERPOINTS(1,7),CORNERPOINTS(2,7))
      CALL GTTTB(GXS,GYE,Z2,CORNERPOINTS(1,8),CORNERPOINTS(2,8))
C
 20   CALL GDEFIN(PXMIN,PXMAX,PYMIN,PYMAX,GXS,GXE,GYS,GYE) 
      RETURN
      END
C
C     ****** SUBSTITUTE A VECTER TO ANOTHER ******
C
      SUBROUTINE GTSUBVEC(X,Y,Z,V)
C
      DIMENSION V(3)
C
      V(1)=X
      V(2)=Y
      V(3)=Z
C      
      RETURN
      END
C
C     ****** CALL HIDENLINE3D WITH ARRAY ******
C
      SUBROUTINE HLINPTP3D(A1,A2)
C
      DIMENSION A1(3),A2(3)
C
      CALL HLINPT3D(A1(1),A1(2),A1(3),A2(1),A2(2),A2(3),0)
C      
      RETURN
      END
C
C     ****** DRAW AXISES ******
C
      SUBROUTINE GAXIS3D(IAXIS)
C
C     if IAXIS=0, draw back axises only
C     if IAXIS=1, draw front axises only
C     if IAXIS=2, draw all axises
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DIMENSION A1(3),A2(3),A3(3),A4(3),A5(3),A6(3),A7(3),A8(3)
C
      CALL GTTTDATA
      CALL gtMkZBuffer
C
      IF (THETA.LE.90.0) THEN
         Z1=ZMIN
         Z2=ZMAX
      ELSE IF (THETA.GT.90.0) THEN
         Z1=ZMAX
         Z2=ZMIN
      ENDIF
      IF (PHI.LT.90.0) THEN
         GOTO 51
      ELSE IF (PHI.LT.180.0) THEN
         GOTO 52
      ELSE IF (PHI.LT.270.0) THEN
         GOTO 53
      ELSE IF (PHI.LT.360.0) THEN
         GOTO 54
      ENDIF
C
 51   CALL GTSUBVEC(GXE,GYS,Z1,A1)
      CALL GTSUBVEC(GXE,GYE,Z1,A2)
      CALL GTSUBVEC(GXS,GYE,Z1,A3)
      CALL GTSUBVEC(GXS,GYS,Z1,A4)
      CALL GTSUBVEC(GXE,GYS,Z2,A5)
      CALL GTSUBVEC(GXS,GYS,Z2,A8)
      CALL GTSUBVEC(GXS,GYE,Z2,A7)
      CALL GTSUBVEC(GXE,GYE,Z2,A6)
      GOTO 999
C
 52   CALL GTSUBVEC(GXE,GYS,Z1,A4)
      CALL GTSUBVEC(GXE,GYE,Z1,A1)
      CALL GTSUBVEC(GXS,GYE,Z1,A2)
      CALL GTSUBVEC(GXS,GYS,Z1,A3)
      CALL GTSUBVEC(GXE,GYS,Z2,A8)
      CALL GTSUBVEC(GXS,GYS,Z2,A7)
      CALL GTSUBVEC(GXE,GYE,Z2,A5)
      CALL GTSUBVEC(GXS,GYE,Z2,A6)
      GOTO 999
C     
 53   CALL GTSUBVEC(GXE,GYS,Z1,A3)
      CALL GTSUBVEC(GXE,GYE,Z1,A4)
      CALL GTSUBVEC(GXS,GYE,Z1,A1)
      CALL GTSUBVEC(GXS,GYS,Z1,A2)
      CALL GTSUBVEC(GXE,GYS,Z2,A7)
      CALL GTSUBVEC(GXS,GYE,Z2,A5)
      CALL GTSUBVEC(GXE,GYE,Z2,A8)
      CALL GTSUBVEC(GXS,GYS,Z2,A6)
      GOTO 999
C 
 54   CALL GTSUBVEC(GXE,GYS,Z1,A2)
      CALL GTSUBVEC(GXE,GYE,Z1,A3)
      CALL GTSUBVEC(GXS,GYE,Z1,A4)
      CALL GTSUBVEC(GXS,GYS,Z1,A1)
      CALL GTSUBVEC(GXS,GYE,Z2,A8)
      CALL GTSUBVEC(GXS,GYS,Z2,A5)
      CALL GTSUBVEC(GXE,GYE,Z2,A7)
      CALL GTSUBVEC(GXE,GYS,Z2,A6)
C
 999  IF (IAXIS.NE.1) THEN
         CALL HLINPTP3D(A2,A1)
         CALL HLINPTP3D(A1,A5)
         CALL HLINPTP3D(A2,A3)
         CALL HLINPTP3D(A3,A4)
         CALL HLINPTP3D(A4,A1)
         CALL HLINPTP3D(A5,A8)
         CALL HLINPTP3D(A8,A7)
         CALL HLINPTP3D(A7,A3)
         CALL HLINPTP3D(A4,A8)
      ENDIF
      IF (IAXIS.NE.0) THEN
         CALL HLINPTP3D(A5,A6)
         CALL HLINPTP3D(A6,A7)
         CALL HLINPTP3D(A6,A2)
      ENDIF
C
 9999 RETURN
      END
C     
C     ****** GET Z-VALUE IN  ******
C     
      SUBROUTINE GTTTZ(X,Y,Z,Z2)
C     
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
C     
      XP=(X-OX)*XFCTR
      YP=(Y-OY)*YFCTR
      ZP=(Z-OZ)*ZFCTR
      R=XP*CA+YP*SA
      Z2=ZP*CC+R*SC
C     
      RETURN
      END
C     
C     ****** TRANSFORM 3D DATA TO 2D WITH CHANGING XTMIN,... ******
C     
      SUBROUTINE GTTTA(X,Y,Z,W1,W2)
C     
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
C     
      XP=(X-OX)*XFCTR
      YP=(Y-OY)*YFCTR
      ZP=(Z-OZ)*ZFCTR
      R=XP*CA+YP*SA
      XPP=YP*CA-XP*SA
      YPP=ZP*SC-R*CC
      ZPP=ZP*CC+R*SC
      W1=EL*XPP/(EL-ZPP)+DOX
      W2=EL*YPP/(EL-ZPP)+DOY
C
      W1=DELX1+RXY1*(W1-XTMIN1)
      W2=DELY1+RXY1*(W2-YTMIN1)
      XTMIN=MIN(XTMIN,W1)
      XTMAX=MAX(XTMAX,W1)
      YTMIN=MIN(YTMIN,W2)
      YTMAX=MAX(YTMAX,W2)
C     
      RETURN
      END
C     
C     ****** TRANSFORM 3D DATA TO 2D WITH CHANGING XTMIN,... ******
C     
      SUBROUTINE GTTTB(X,Y,Z,W1,W2)
C     
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
C     
      XP=(X-OX)*XFCTR
      YP=(Y-OY)*YFCTR
      ZP=(Z-OZ)*ZFCTR
      R=XP*CA+YP*SA
      XPP=YP*CA-XP*SA
      YPP=ZP*SC-R*CC
      ZPP=ZP*CC+R*SC
      W1=EL*XPP/(EL-ZPP)+DOX
      W2=EL*YPP/(EL-ZPP)+DOY
      W1=DELX1+RXY1*(W1-XTMIN1)
      W2=DELY1+RXY1*(W2-YTMIN1)
C     
      RETURN
      END
C     
C     ****** TRANSFORM 3D DATA TO 2D WITH CHANGING XTMIN,... ******
C     
      SUBROUTINE GTTT1C(X,Y,Z,W1,W2)
C     
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX1,OY1,OZ
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /GDFN2D/ XMIN,XMAX,YMIN,YMAX
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
C     
      OX=(OX1-XMIN)*XFCTR
      OY=(OY1-YMIN)*YFCTR
      XP=X-OX
      YP=Y-OY
      ZP=(Z-OZ)*ZFCTR
      R=XP*CA+YP*SA
      XPP=YP*CA-XP*SA
      YPP=ZP*SC-R*CC
      ZPP=ZP*CC+R*SC
      W1=EL*XPP/(EL-ZPP)+DOX
      W2=EL*YPP/(EL-ZPP)+DOY
      W1=DELX1+RXY1*(W1-XTMIN1)
      W2=DELY1+RXY1*(W2-YTMIN1)
C
      XTMIN=MIN(XTMIN,W1)
      XTMAX=MAX(XTMAX,W1)
      YTMIN=MIN(YTMIN,W2)
      YTMAX=MAX(YTMAX,W2)
C     
      RETURN
      END
C     
C     ******  3D DATA TO 2D WITHOUT CHANGING XTMIN,... ******
C     
      SUBROUTINE GTTT1D(X,Y,Z,W1,W2)
C     
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX1,OY1,OZ
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /GDFN2D/ XMIN,XMAX,YMIN,YMAX
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
C
      OX=(OX1-XMIN)*XFCTR
      OY=(OY1-YMIN)*YFCTR
      XP=X-OX
      YP=Y-OY
      ZP=(Z-OZ)*ZFCTR
      R=XP*CA+YP*SA
      XPP=YP*CA-XP*SA
      YPP=ZP*SC-R*CC
      ZPP=ZP*CC+R*SC
      W1=EL*XPP/(EL-ZPP)+DOX
      W2=EL*YPP/(EL-ZPP)+DOY
      W1=DELX1+RXY1*(W1-XTMIN1)
      W2=DELY1+RXY1*(W2-YTMIN1)
C     
      RETURN
      END
C
C     ****** TRSNSPORT 3D DATA TO 2D ******
C
      SUBROUTINE GTTTDATA
C
      COMMON /FRAG3D/ NTRN,NDFN,NZBUF
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /GTHNDT/ XHDATA(1001,2),YHDATA(1001,2)
      COMMON /ZBUFUR/ IXINDEX(10000),IYINDEX(10000),ZVALUE(1000,1000)
      DIMENSION PV(3)
      DIMENSION ZVALUE1(1001,1001),INDEX(10000)
      DIMENSION ZPLANE(10000)
C
      IF (NTRN.EQ.1) GOTO 999
C
      NTRN=1
      DO 1000 IX=1,NX
      DO 1000 IY=1,NY
         CALL TRNP3D(XDATA(IX),YDATA(IY),ZDATA(IX,IY),PV)
         WORK(IX,IY,1)=PV(1)
         WORK(IX,IY,2)=PV(2)
         WORK(IX,IY,3)=PV(3)
 1000 CONTINUE 
      DO 100 IX=1,NX-1
      DO 100 IY=1,NY-1
         WORK(IX,IY,4)=MIN(WORK(IX,IY,1),WORK(IX+1,IY,1),
     &                     WORK(IX+1,IY+1,1),WORK(IX,IY+1,1))
         WORK(IX,IY,5)=MAX(WORK(IX,IY,1),WORK(IX+1,IY,1),
     &                     WORK(IX+1,IY+1,1),WORK(IX,IY+1,1))
         WORK(IX,IY,6)=MIN(WORK(IX,IY,2),WORK(IX+1,IY,2),
     &                     WORK(IX+1,IY+1,2),WORK(IX,IY+1,2))
         WORK(IX,IY,7)=MAX(WORK(IX,IY,2),WORK(IX+1,IY,2),
     &                     WORK(IX+1,IY+1,2),WORK(IX,IY+1,2))
         WORK(IX,IY,8)=MIN(WORK(IX,IY,3),WORK(IX+1,IY,3),
     &                     WORK(IX+1,IY+1,3),WORK(IX,IY+1,3))
  100 CONTINUE
C
      DO 200 IY=1,NY-1
         XMIN=WORK(1,IY,4)
         XMAX=WORK(1,IY,5)
         YMIN=WORK(1,IY,6)
         YMAX=WORK(1,IY,7)
         WMIN=WORK(1,IY,8)
         DO 190 IX=2,NX-1
            IF (WORK(IX,IY,4).LT.XMIN) XMIN=WORK(IX,IY,4)
            IF (WORK(IX,IY,5).GT.XMAX) XMAX=WORK(IX,IY,5)
            IF (WORK(IX,IY,6).LT.YMIN) YMIN=WORK(IX,IY,6)
            IF (WORK(IX,IY,7).GT.YMAX) YMAX=WORK(IX,IY,7)
            IF (WORK(IX,IY,8).LT.WMIN) WMIN=WORK(IX,IY,8)
 190     CONTINUE
         WORK(NX,IY,4)=XMIN
         WORK(NX,IY,5)=XMAX
         WORK(NX,IY,6)=YMIN
         WORK(NX,IY,7)=YMAX
         WORK(NX,IY,8)=WMIN
 200  CONTINUE
      DO 300 I=1,NX
         XMIN=WORK(I,1,2)
         XMAX=WORK(I,1,2)
         DO 301 J=1,NY
            XMIN=MIN(WORK(I,J,2),XMIN)
            XMAX=MAX(WORK(I,J,2),XMAX)
 301     CONTINUE
         XHDATA(I,1)=XMIN
         XHDATA(I,2)=XMAX
 300  CONTINUE
      DO 400 J=1,NY
         YMIN=WORK(1,J,2)
         YMAX=WORK(1,J,2)
         DO 401 I=1,NX
            YMIN=MIN(WORK(I,J,2),YMIN)
            YMAX=MAX(WORK(I,J,2),YMAX)
 401     CONTINUE
         YHDATA(J,1)=YMIN
         YHDATA(J,2)=YMAX
 400  CONTINUE
C -----------------------------
C -------- For Z-Buffer -------
C -----------------------------
      DO IX=1,NX
      DO IY=1,NY
         CALL GTTTZ(XDATA(IX),YDATA(IY),ZDATA(IX,IY),ZVALUE1(IX,IY))
      ENDDO
      ENDDO
      DO IX=1,NX-1
      DO IY=1,NY-1
         ZPLANE((IX-1)*(NY-1)+IY)=0.25*(ZVALUE1(IX,IY)+ZVALUE1(IX+1,IY)
     &        +ZVALUE1(IX,IY+1)+ZVALUE1(IX+1,IY+1))
         ZVALUE(IX,IY)=ZPLANE((IX-1)*(NY-1)+IY)
      ENDDO
      ENDDO
      CALL GTindexx((NX-1)*(NY-1),ZPLANE,INDEX)
      DO I=1,(NX-1)*(NY-1)
         IYINDEX(I)=MOD(INDEX(I),NY-1)
         IF (IYINDEX(I).EQ.0) IYINDEX(I)=NY-1
         IXINDEX(I)=INT(REAL(INDEX(I)-IYINDEX(I))/REAL(NY-1))+1
      ENDDO
C          
 999  RETURN
      END
C
C     ****** 1から8番の角の座標を求める ******
C
      SUBROUTINE INQCNR3D(POS)
C
      COMMON /CORNER/ CORNERPOINTS(2,8)
      DIMENSION POS(2,8)
C
      DO I=1,8
         POS(1,I)=CORNERPOINTS(1,I)
         POS(2,I)=CORNERPOINTS(2,I)
      ENDDO
C
      RETURN
      END
C
C     ****** theta と phi を返す ******
C
      SUBROUTINE INQANG3D(THETA,PHI)
C
      COMMON /ANGL3D/ PHI1,THETA1
C
      THETA=THETA1
      PHI=PHI1
C
      RETURN
      END
C
C     ****** DRAW TICKS ON X-AXIS ******
C     
      SUBROUTINE GSCALE3DX(XORG,XSTEP,SCL,IPOS)
C
C     POSITION1
C     if IPOS=0, draw lines.
C     if IPOS=1, internal .
C     if IPOS=2, outer .
C     if IPOS=3, internal and outer .
C     +10 POSITION2
C     +20 POSITION3
C     +30 POSITION4
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /POSSCL/ IPOSS,I1Y,I1Z,SCAL1,SCLY,SCLZ
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXX,NYY
      DATA EPS/0.01/
C
      CALL GTTTDATA
      CALL gtMkZBuffer
C
      IPOSS=IPOS
      IF (IPOS.LT.0) THEN
         ISCL=0
         IPOS1=MOD(-IPOS,10)
         IPOS2=INT(-0.1*IPOS+EPS)
         SCAL1=SCL
      ELSE
         ISCL=1
         IPOS1=MOD(IPOS,10)
         IPOS2=INT(0.1*IPOS+EPS)
         SCAL1=SCL/YL*(GYE-GYS)
      ENDIF
C     目盛りを格子状に書くときは別に扱う．
      IF (IPOS1.EQ.0) THEN
         CALL GTXPOS(IPOS2+1,X2,X3,I1,I1)
         IF (ABS((X2-GYS)/(GYE-GYS)).LT.EPS) THEN
            SCALX=GYE-GYS
         ELSE
            SCALX=-(GYE-GYS)
         ENDIF
         ILINE=INT(SCL)
         ISCL=1
         IYZ=0
         IR=1
         GOTO 3
      ENDIF
      ILINE=0
C     目盛りが内側 IND=1,4,7
      IF (MOD(IPOS1,3).EQ.1) THEN
         SCAL1=-SCAL1
      ENDIF
      SCAL=SCAL1
      IR=1
      RAD=3.141593/180.0
C     見る角度によって+0か+3か (7以上)
      IF (INT((IPOS1-4)/3).EQ.1) THEN
         IF (COS(2.*RAD*THETA).GE.0.0) THEN
            IYZ=0
         ELSE
            IYZ=1
         ENDIF
         GOTO 2
      ENDIF
C     z軸に平行に (4以上)
      IF (INT((IPOS1-1)/3).EQ.1) THEN
         IYZ=1
      ELSE
         IYZ=0
      ENDIF
      IF (IPOS1.EQ.0) THEN
         ISCL=2
         GOTO 3
      ENDIF
 2    IF (MOD(IPOS1,3).EQ.0.AND.IPOS1.NE.0) IR=0
C     見る角度によって目盛りの方向を変化
 1    CALL GTXPOS(IPOS2+1,X2,X3,I1,I1)
      IF (IYZ.EQ.0) THEN
         IF (ABS((X2-GYS)/(GYE-GYS)).LT.EPS) THEN
            SCALX=-SCAL
         ELSE
            SCALX=SCAL
         ENDIF
      ELSE IF (IYZ.EQ.1) THEN
         IF (ABS((X3-ZMIN)/(ZMAX-ZMIN)).LT.EPS) THEN
            SCALX=-SCAL
         ELSE
            SCALX=SCAL
         ENDIF
      ENDIF
C     実際の描画
 3    CALL GTXSCL(XORG,XSTEP,X2,X3,SCALX,ISCL,IYZ,ILINE)
C     3,6,9 ならば目盛りの方向を変えてもう1度描画
      IF (MOD(IPOS1,3).EQ.0.AND.IR.EQ.0) THEN
         SCAL=-SCAL
         IR=1
         GOTO 1
      ENDIF
C
      RETURN
      END
C
C     ****** DRAW TICKS ON XAXIS ******
C
      SUBROUTINE GTXSCL(XORG,DPX,YPOS,ZPOS,SCAL,I1,IYZ,ILINE)
C
C     if I1=0, tick's length is scl(cm) .
C     if I1=1, scl is pre-transformed tick's length .
C     if I1=2, 格子．
C
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DATA EPS/0.01/
C
      IF (XORG.LT.GXS.OR.XORG.GT.GXE) GOTO 9999
      IF (ABS(DPX)/(GXE-GXS).LT.EPS) GOTO 9999
      PX0=GXS-XORG
      PX1=GXE-XORG
      NS=INT((PX0-EPS)/DPX)
      NE=INT((PX1+EPS)/DPX)
C
      IF (IYZ.EQ.0) THEN
         PY=YPOS
         PZ=ZPOS
         PX0=XORG
         PY1=PY
         PY2=PY+SCAL
         PZ1=PZ
         PZ2=PZ
      ELSE
         PY=YPOS
         PZ=ZPOS
         PX0=GXS
         PY1=PY
         PY2=PY
         PZ1=PZ
         PZ2=PZ+SCAL
      ENDIF
C
      CALL GUGRPS
      DO 1000 N=NS,NE
         PX=PX0+DPX*N
         CALL GTTTB(PX,PY1,PZ1,TPX1,TPY1)
         CALL GTTTB(PX,PY2,PZ2,TPX2,TPY2)
         IF (I1.EQ.0) THEN
            CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
            RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)
            RATIOY=(XMAX-XMIN)/(PXMAX-PXMIN)
            DL=SQRT(((TPX2-TPX1)/RATIOX)**2+((TPY2-TPY1)/RATIOY)**2)
C
C     only when delta zpp is sufficiently small, is this valid ? 
C  
            DLY=(PY2-PY1)*ABS(SCAL)/DL
            DLZ=(PZ2-PZ1)*ABS(SCAL)/DL
            PY2=PY1+DLY
            PZ2=PZ1+DLZ
         ENDIF
         CALL HLINPT3D(PX,PY1,PZ1,PX,PY2,PZ2,ILINE)
 1000 CONTINUE
      CALL GUGRPE
C
 9999 RETURN
      END
C
C     ****** DRAW TICKS ON Y-AXIS ******
C     
      SUBROUTINE GSCALE3DY(YORG,YSTEP,SCL,IPOS)
C
C     POSITION1
C     if IPOS=0, 
C     if IPOS=1, internal .
C     if IPOS=2, outer .
C     if IPOS=3, internal and outer .
C     +10 POSITION2
C     +20 POSITION3
C     +30 POSITION4
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /POSSCL/ I1X,IPOSS,I1Z,SCLX,SCAL1,SCLZ
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXX,NYY
      DATA EPS/0.01/
C
      CALL GTTTDATA     
      CALL gtMkZBuffer
C
      IPOSS=IPOS
      IF (IPOS.LT.0) THEN
         ISCL=0
         IPOS1=MOD(-IPOS,10)
         IPOS2=INT(-0.1*IPOS+EPS)
         SCAL1=SCL
      ELSE
         ISCL=1
         IPOS1=MOD(IPOS,10)
         IPOS2=INT(0.1*IPOS+EPS)
         SCAL1=SCL/XL*(GXE-GXS)
      ENDIF
C     目盛りを格子状に書くときは別に扱う．
      IF (IPOS1.EQ.0) THEN
         CALL GTYPOS(IPOS2+1,Y1,Y3,I1,I1)
         IF (ABS((Y1-GXS)/(GXE-GXS)).LT.EPS) THEN
            SCALY=GXE-GXS
         ELSE
            SCALY=-(GXE-GXS)
         ENDIF
         ISCL=1
         IXZ=0
         ILINE=INT(SCL)
         IR=1
         GOTO 3
      ENDIF
      ILINE=0
      IF (MOD(IPOS1,3).EQ.1) THEN
         SCAL1=-SCAL1
      ENDIF
      SCAL=SCAL1
      IR=1
C
      RAD=3.141593/180.0
      IF (INT((IPOS1-4)/3).EQ.1) THEN
         IF (COS(2.*RAD*THETA).GE.0.0) THEN
            IXZ=0
         ELSE
            IXZ=1
         ENDIF
         GOTO 2
      ENDIF
      IF (INT((IPOS1-1)/3).EQ.1) THEN
         IXZ=1
      ELSE
         IXZ=0
      ENDIF
 2    IF (MOD(IPOS1,3).EQ.0) IR=0
C
 1    CALL GTYPOS(IPOS2+1,Y1,Y3,I1,I1)
      IF (IXZ.EQ.0) THEN
         IF (ABS((Y1-GXS)/(GXE-GXS)).LT.EPS) THEN
            SCALY=-SCAL
         ELSE
            SCALY=SCAL
         ENDIF
      ELSE IF (IXZ.EQ.1) THEN
         IF (ABS((Y3-ZMIN)/(ZMAX-ZMIN)).LT.EPS) THEN
            SCALY=-SCAL
         ELSE
            SCALY=SCAL
         ENDIF
      ENDIF
C
 3    CALL GTYSCL(YORG,YSTEP,Y1,Y3,SCALY,ISCL,IXZ,ILINE)
C
      IF (MOD(IPOS1,3).EQ.0.AND.IR.EQ.0) THEN
         SCAL=-SCAL
         IR=1
         GOTO 1
      ENDIF
C
      RETURN
      END
C
C     ****** DRAW TICKS ON YAXIS ******
C
      SUBROUTINE GTYSCL(YORG,DPY,XPOS,ZPOS,SCAL,I1,IXZ,ILINE)
C
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DATA EPS/0.01/
C
      IF (YORG.LT.GYS.OR.YORG.GT.GYE) GOTO 9999
      IF (ABS(DPY)/(GYE-GYS).LT.EPS) GOTO 9999
      PY0=GYS-YORG
      PY1=GYE-YORG
      NS=INT((PY0-EPS)/DPY)
      NE=INT((PY1+EPS)/DPY)
C
      IF (IXZ.EQ.0) THEN
         PX=XPOS
         PZ=ZPOS
         PY0=YORG
         PX1=PX
         PX2=PX+SCAL
         PZ1=PZ
         PZ2=PZ
      ELSE
         PX=XPOS
         PZ=ZPOS
         PY0=GYS
         PX1=PX
         PX2=PX
         PZ1=PZ
         PZ2=PZ+SCAL
      ENDIF
      CALL GUGRPS
      DO 2000 N=NS,NE
         PY=PY0+DPY*N
         CALL GTTTB(PX1,PY,PZ1,TPX1,TPY1)
         CALL GTTTB(PX2,PY,PZ2,TPX2,TPY2)
         IF (I1.EQ.0) THEN
            CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
            RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)
            RATIOY=(XMAX-XMIN)/(PXMAX-PXMIN)
            DL=SQRT(((TPX2-TPX1)/RATIOX)**2+((TPY2-TPY1)/RATIOY)**2)
C
C     only when delta zpp is sufficiently small, this is valid ? 
C
            DLX=(PX2-PX1)*ABS(SCAL)/DL
            DLZ=(PZ2-PZ1)*ABS(SCAL)/DL
            PX2=PX1+DLX
            PZ2=PZ1+DLZ
         ENDIF
         CALL HLINPT3D(PX1,PY,PZ1,PX2,PY,PZ2,ILINE)
 2000 CONTINUE
      CALL GUGRPE
C
 9999 RETURN
      END
C
C     ****** DRAW TICKS ON Z-AXIS ******
C     
      SUBROUTINE GSCALE3DZ(ZORG,ZSTEP,SCL,IPOS)
C
C     POSITION1
C     if IPOS=0, 
C     if IPOS=1, internal .
C     if IPOS=2, outer .
C     if IPOS=3, internal and outer .
C     +10 POSITION2
C     +20 POSITION3
C     +30 POSITION4
C
      COMMON /POSSCL/ I1X,I1Y,IPOSS,SCLX,SCLY,SCAL1
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXX,NYY
      DATA EPS/0.01/
C
      CALL GTTTDATA
      CALL gtMkZBuffer
C
      IPOSS=IPOS
C
      IF (IPOS.LT.0) THEN
         ISCL=0
         IPOS1=MOD(-IPOS,10)
         IPOS2=INT(-0.1*IPOS+EPS)
         SCAL1=SCL
         CALL GTZPOS(IPOS2+1,Z1,Z2,I1,I1,NEC)
      ELSE
         ISCL=1
         IPOS1=MOD(IPOS,10)
         IPOS2=INT(0.1*IPOS+EPS)
         CALL GTZPOS(IPOS2+1,Z1,Z2,I1,I1,NEC)
         IF (NEC.EQ.0) THEN
            SCAL1=SCL/YL*(GYE-GYS)
         ELSE
            SCAL1=SCL/XL*(GXE-GXS)
         ENDIF
      ENDIF
C ------------------------------------------
C     目盛りを格子状に書くときは別に扱う．
C ------------------------------------------
C     前面に目盛りを書くとき
      IF (IPOS1.EQ.0) THEN
         CALL GTZPOS(1,Z1,Z2,I1,I1,NEC)
         IF (NEC.EQ.0) INEC=1
         IF (NEC.EQ.1) INEC=0
         NEC=INEC
         IF (NEC.EQ.0) THEN
            IF (ABS((Z1-GXS)/(GXE-GXS)).LT.EPS) THEN
               SCALZ=GXE-GXS
            ELSE
               SCALZ=-(GXE-GXS)
            ENDIF
         ELSE
            IF (ABS((Z2-GYS)/(GYE-GYS)).LT.EPS) THEN
               SCALZ=GYE-GYS
            ELSE
               SCALZ=-(GYE-GYS)
            ENDIF
         ENDIF
         ISCL=1
         ILINE=INT(SCL)
         IZERO=1
         GOTO 3
      ENDIF
 4    IF (IPOS1.EQ.0) THEN
         CALL GTZPOS(2,Z1,Z2,I1,I1,NEC)
         IF (NEC.EQ.0) THEN
            IF (ABS((Z1-GXS)/(GXE-GXS)).LT.EPS) THEN
               SCALZ=GXE-GXS
            ELSE
               SCALZ=-(GXE-GXS)
            ENDIF
         ELSE
            IF (ABS((Z2-GYS)/(GYE-GYS)).LT.EPS) THEN
               SCALZ=GYE-GYS
            ELSE
               SCALZ=-(GYE-GYS)
            ENDIF
         ENDIF
         GOTO 3
      ENDIF
C
      ILINE=0
C
 9999 IF (IPOS1.EQ.1) THEN
         SCAL1=-SCAL1
      ENDIF
      SCAL=SCAL1
      IR=1
C
      IF (IPOS1.EQ.3) IR=0
C
 1    IF (NEC.EQ.0) THEN
         IF (ABS((Z1-GXS)/(GXE-GXS)).LT.EPS) THEN
            SCALZ=-SCAL
         ELSE
            SCALZ=SCAL
         ENDIF
      ELSE
         IF (ABS((Z2-GYS)/(GYE-GYS)).LT.EPS) THEN
            SCALZ=-SCAL
         ELSE
            SCALZ=SCAL
         ENDIF
      ENDIF
C
 3    CALL GTZSCL(ZORG,ZSTEP,Z1,Z2,NEC,SCALZ,ISCL,ILINE)
      IF (IPOS1.EQ.0.AND.IZERO.EQ.1) THEN
         IZERO=0
         GOTO 4
      ENDIF
C
      IF (IPOS1.EQ.3.AND.IR.EQ.0) THEN
         SCAL=-SCAL
         IR=1
         GOTO 1
      ENDIF
C
      RETURN
      END
C
C     ****** DRAW TICKS ON ZAXIS ******
C
      SUBROUTINE GTZSCL(ZORG,DPZ,XPOS,YPOS,NEC,SCAL,I1,ILINE)
C
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      DATA EPS/0.01/
C
      IF (ZORG.LT.ZMIN.OR.ZORG.GT.ZMAX) GOTO 9999
      IF (ABS(DPZ/(ZMAX-ZMIN)).LT.EPS) GOTO 9999
      PZ0=ZMIN-ZORG
      PZ1=ZMAX-ZORG
      NS=INT((PZ0-EPS)/DPZ)
      NE=INT((PZ1+EPS)/DPZ)
C
      PX=XPOS
      PY=YPOS
      PZ0=ZORG
C
      IF (NEC.EQ.0) THEN
         PX1=PX
         PX2=PX+SCAL
         PY1=PY
         PY2=PY
      ELSE IF (NEC.EQ.1) THEN
         PX1=PX
         PX2=PX
         PY1=PY
         PY2=PY+SCAL
      ENDIF
      CALL GUGRPS
      DO 3000 N=NS,NE
         PZ=PZ0+DPZ*N
         CALL GTTTB(PX1,PY1,PZ,TPX1,TPY1)
         CALL GTTTB(PX2,PY2,PZ,TPX2,TPY2)
         IF (I1.EQ.0) THEN
            CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
            RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)
            RATIOY=(XMAX-XMIN)/(PXMAX-PXMIN)
            DL=SQRT(((TPX2-TPX1)/RATIOX)**2+((TPY2-TPY1)/RATIOY)**2)
C
C     only when delta zpp is sufficiently small, this is valid ?
C  
            DLX=(PX2-PX1)*ABS(SCAL)/DL
            DLY=(PY2-PY1)*ABS(SCAL)/DL
            PX2=PX1+DLX
            PY2=PY1+DLY
         ENDIF
         CALL HLINPT3D(PX1,PY1,PZ,PX2,PY2,PZ,ILINE)
 3000 CONTINUE
      CALL GUGRPE
C
 9999 RETURN
      END
C
C     ****** DRAW NUMBERS ON X-AXIS ******
C
      SUBROUTINE GVALUE3DX(XORG,DPX,IPOSV,IND)
C
C     if IPOS=1 POSITION1
C     +10 POSITION2
C     +20 POSITION3
C     +30 POSITION4
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /POSSCL/ I1X,I1Y,I1Z,SCAL,SCLY,SCLZ
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DATA EPS/0.01/
C
      IPOS=IPOSV
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      IF (IPOS.LT.0) THEN
         IPOS1=MOD(-IPOS,10)
         IPOS2=INT(FLOAT(-IPOS)*0.1+EPS)
      ELSE
         IPOS1=MOD(IPOS,10)
         IPOS2=INT(FLOAT(IPOS)*0.1+EPS)
      ENDIF
C
      RAD=3.141593/180.0
      IF (INT((IPOS1-3)/3).EQ.1) THEN
         IF (IPOS1.EQ.6) THEN 
            IYZ=0
            IF (COS(2.*RAD*THETA).GE.0.0) THEN
               IPOS=1
            ELSE
               IPOS=3
            ENDIF
         ELSE IF (COS(2.*RAD*THETA).GE.0.0) THEN
            IYZ=0
         ELSE
            IYZ=1
         ENDIF
         GOTO 2
      ENDIF
      IF (INT(IPOS1/4).EQ.0) THEN
         IYZ=0
      ELSE
         IYZ=1
      ENDIF 
C      
 2    CALL GTXPOS(IPOS2+1,X2,X3,I1,I1)
C
      IF (IYZ.EQ.0) THEN
         IF (ABS((X2-GYS)/(GYE-GYS)).LT.EPS) THEN
            SCALX=-SCAL
         ELSE
            SCALX=SCAL
         ENDIF
      ELSE IF (IYZ.EQ.1) THEN
         IF (ABS((X3-ZMIN)/(ZMAX-ZMIN)).LT.EPS) THEN
            SCALX=-SCAL
         ELSE
            SCALX=SCAL
         ENDIF
      ENDIF
C      
      CALL GTXVAL(XORG,DPX,X2,X3,SCALX,IND,IPOS,IYZ)
C     
      CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      RETURN
      END
C
C     ****** DRAW NUMBERS ON XAXIS ******
C
      SUBROUTINE GTXVAL(XORG,DPX,YPOS,ZPOS,SCAL,IND,IPOSV,IYZ)
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /POSSCL/ IPOS,I1Y,I1Z,SCX,SCY,SCZ
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DIMENSION XTIC(-1001:1001,4)
      DATA EPS/0.01/
C     
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      IF (XORG.LT.GXS.OR.XORG.GT.GXE) GOTO 9999
      IF (IPOSV.LT.0) THEN
         IPOSV1=MOD(-IPOSV,10)
         IPOSV2=INT(-0.1*IPOSV+EPS)
      ELSE
         IPOSV1=MOD(IPOSV,10)
         IPOSV2=INT(0.1*IPOSV+EPS)
      ENDIF
      IF (IPOS.LT.0) THEN
         ISCL=0
         IPOS1=MOD(-IPOS,10)
      ELSE
         ISCL=1
         IPOS1=MOD(IPOS,10)
      ENDIF
C
      IF (ABS(DPX/(GXE-GXS)).LT.EPS) GOTO 9999
      PX0=GXS-XORG
      PX1=GXE-XORG
      NS=INT((PX0-EPS)/DPX)
      NE=INT((PX1+EPS)/DPX)
C
      IF (IYZ.EQ.0) THEN
         PY=YPOS
         PZ=ZPOS
         PX0=XORG
         PY1=PY
         PY2=PY+SCAL
         PZ1=PZ
         PZ2=PZ
      ELSE
         PY=YPOS
         PZ=ZPOS
         PX0=XORG
         PY1=PY
         PY2=PY
         PZ1=PZ
         PZ2=PZ+SCAL
      ENDIF
C
      DO 5000 N=NS,NE
         PX=PX0+DPX*N
         CALL GTTTB(PX,PY1,PZ1,TPX1,TPY1)
         CALL GTTTB(PX,PY2,PZ2,TPX2,TPY2)
         IF (ISCL.EQ.0) THEN
            CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
            RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)
            RATIOY=(XMAX-XMIN)/(PXMAX-PXMIN)
            DL=SQRT(((TPX2-TPX1)/RATIOX)**2+((TPY2-TPY1)/RATIOY)**2)
            DLY=(PY2-PY1)*ABS(SCAL)/DL
            DLZ=(PZ2-PZ1)*ABS(SCAL)/DL
            PY2=PY1+DLY
            PZ2=PZ1+DLZ
         ENDIF
         IF (MOD(IPOS1,3).NE.1) THEN
            XTIC(N+1,1)=PX
            XTIC(N+1,2)=PY2
            XTIC(N+1,3)=PZ2
         ELSE
            XTIC(N+1,1)=PX
            XTIC(N+1,2)=PY1
            XTIC(N+1,3)=PZ1
         ENDIF
         XTIC(N+1,4)=PX
 5000 CONTINUE
C
      CALL GTXPOS(1,YTMP,ZTMP,IX1,IX2)
      CALL GTZPOS(1,XTMP,YTMP,IZ1,IZ2,NEC)
C
      IF (IYZ.EQ.0) THEN
         IF (MOD(IPOSV1-2,3).EQ.0) THEN
            IDRC=0
         ELSE IF (MOD(IPOSV1,3).EQ.0) THEN
            GOTO 999
         ELSE
            IDRC=3
         ENDIF
         IPM1=1
         IPM2=1
         DO 100 I=NS+1,NE+1
            IF (MOD(IPOSV2,2).EQ.0) THEN
               IF (THETA.LE.90.0) THEN
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 3
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 1
                  GOTO 2
               ELSE
                  IPM1=-1
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 3
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 1
                  GOTO 4  
               ENDIF         
            ELSE
               IF (THETA.LE.90.0) THEN
                  IPM2=-1
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 1
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 3
                  GOTO 4
               ELSE
                  IPM1=-1
                  IPM2=-1
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 1
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 3
                  GOTO 2 
               ENDIF
            ENDIF
 1          CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           IPM1*0.5*CHH,IPM2*1.5*CHW,0.0,XTIC(I,4),IND,0,IDRC)
            GOTO 100
 2          CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,IPM2*2.0*CHH,0.0,XTIC(I,4),IND,3,IDRC)
            GOTO 100
 3          CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           IPM1*0.5*CHH,IPM2*1.5*CHW,0.0,XTIC(I,4),IND,1,IDRC)
            GOTO 100
 4          CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,IPM2*1.0*CHH,0.0,XTIC(I,4),IND,3,IDRC)
            GOTO 100
 100     CONTINUE
         GOTO 9999
 999     IDRC=7
         IPM1=1
         IPM2=1
         DO 300 I=NS+1,NE+1
            IF (MOD(IPOSV2,2).EQ.0) THEN
               IF (THETA.LE.90.0) THEN
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 23
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 21
                  GOTO 22
               ELSE
                  IPM1=-1
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 23
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 21
                  GOTO 24  
               ENDIF         
            ELSE
               IF (THETA.LE.90.0) THEN
                  IPM2=-1
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 21
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 23
                  GOTO 24
               ELSE
                  IPM1=-1
                  IPM2=-1
                  IF (IX1.EQ.1.AND.IZ1.EQ.3) GOTO 21
                  IF (IX1.EQ.2.AND.IZ1.EQ.1) GOTO 23
                  GOTO 22 
               ENDIF
            ENDIF
 21         CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,IPM2*1.5*CHW,IPM1*0.5*CHH,XTIC(I,4),IND,0,IDRC)
            GOTO 300
 22         CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,IPM2*2.0*CHH,IPM1*0.5*CHH,XTIC(I,4),IND,3,IDRC)
            GOTO 300
 23         CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,IPM2*1.5*CHW,IPM1*0.5*CHH,XTIC(I,4),IND,1,IDRC)
            GOTO 300
 24         CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,IPM2*1.0*CHH,IPM1*0.5*CHH,XTIC(I,4),IND,3,IDRC)
            GOTO 300
 300     CONTINUE
      ELSE IF (IYZ.EQ.1) THEN
         IF (MOD(IPOSV1-2,3).EQ.0) THEN
            IDRC=0
         ELSE
            IDRC=7
         ENDIF
         IPM1=1
         IPM2=1
         IF (MOD(IPOSV2,2).EQ.0) THEN
         ELSE
            IPM2=-1
         ENDIF
         DO 200 I=NS+1,NE+1
            CALL GTRNUMBR(XTIC(I,1),XTIC(I,2),XTIC(I,3),
     &           0.0,0.0,IPM2*2.0*CHH,XTIC(I,4),IND,3,IDRC)
 200     CONTINUE
      ENDIF
 9999 CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
      RETURN
      END
C
C     ****** DRAW NUMBERS ON Y-AXIS ******
C
      SUBROUTINE GVALUE3DY(YORG,DPY,IPOSV,IND)
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /POSSCL/ I1X,I1Y,I1Z,SCLX,SCAL,SCLZ
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      DATA EPS/0.01/
C
      IPOS=IPOSV
      IF (IPOS.LT.0) THEN
         IPOS1=MOD(-IPOS,10)
         IPOS2=INT(FLOAT(-IPOS)*0.1+EPS)
      ELSE
         IPOS1=MOD(IPOS,10)
         IPOS2=INT(FLOAT(IPOS)*0.1+EPS)
      ENDIF
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      RAD=3.141593/180.0
      IF (INT((IPOS1-3)/3).EQ.1) THEN
         IF (IPOS1.EQ.6) THEN 
            IXZ=0
            IF (COS(2.*RAD*THETA).GE.0.0) THEN
               IPOS=1
            ELSE
               IPOS=3
            ENDIF
         ELSE IF (COS(2.*RAD*THETA).GE.0.0) THEN
            IXZ=0
         ELSE
            IXZ=1
         ENDIF
         GOTO 2
      ENDIF
      IF (INT(IPOS1/4).EQ.0) THEN
         IXZ=0
      ELSE
         IXZ=1
      ENDIF
C     
 2    CALL GTYPOS(IPOS2+1,Y1,Y3,I1,I1)
      IF (IXZ.EQ.0) THEN
         IF (ABS((Y1-GXS)/(GXE-GXS)).LT.EPS) THEN
            SCALY=-SCAL
         ELSE
            SCALY=SCAL
         ENDIF
      ELSE IF (IXZ.EQ.1) THEN
         IF (ABS((Y3-ZMIN)/(ZMAX-ZMIN)).LT.EPS) THEN
            SCALY=-SCAL
         ELSE
            SCALY=SCAL
         ENDIF
      ENDIF
C         
      CALL GTYVAL(YORG,DPY,Y1,Y3,SCALY,IND,IPOS,IXZ)
C     
      CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      RETURN
      END
C
C     ****** DRAW NUMBERS ON YAXIS ******
C
      SUBROUTINE GTYVAL(YORG,DPY,XPOS,ZPOS,SCAL,IND,IPOSV,IXZ)
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /POSSCL/ I1X,IPOS,I1Z,SCX,SCY,SCZ
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DIMENSION YTIC(-1001:1001,4)
      DATA EPS/0.01/
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      IF (YORG.LT.GYS.OR.YORG.GT.GYE) GOTO 9999
      IF (IPOSV.LT.0) THEN
         IPOSV1=MOD(-IPOSV,10)
         IPOSV2=INT(-0.1*IPOSV+EPS)
      ELSE
         IPOSV1=MOD(IPOSV,10)
         IPOSV2=INT(0.1*IPOSV+EPS)
      ENDIF
      IF (IPOS.LT.0) THEN
         ISCL=0
         IPOS1=MOD(-IPOS,10)
      ELSE
         ISCL=1
         IPOS1=MOD(IPOS,10)
      ENDIF
C
      IF (ABS(DPY/(GYE-GYS)).LT.EPS) GOTO 9999
      PY0=GYS-YORG
      PY1=GYE-YORG
      NS=INT((PY0-EPS)/DPY)
      NE=INT((PY1+EPS)/DPY)
C
      IF (IXZ.EQ.0) THEN
         PX=XPOS
         PZ=ZPOS
         PY0=YORG
         PX1=PX
         PX2=PX+SCAL
         PZ1=PZ
         PZ2=PZ
      ELSE
         PX=XPOS
         PZ=ZPOS
         PY0=YORG
         PX1=PX
         PX2=PX
         PZ1=PZ
         PZ2=PZ+SCAL
      ENDIF
C
      DO 5000 N=NS,NE
         PY=PY0+DPY*N
         CALL GTTTB(PX1,PY,PZ1,TPX1,TPY1)
         CALL GTTTB(PX2,PY,PZ2,TPX2,TPY2)
         IF (ISCL.EQ.0) THEN
            CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
            RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)
            RATIOY=(XMAX-XMIN)/(PXMAX-PXMIN)
            DL=SQRT(((TPX2-TPX1)/RATIOX)**2+((TPY2-TPY1)/RATIOY)**2)
            DLX=(PX2-PX1)*ABS(SCAL)/DL
            DLZ=(PZ2-PZ1)*ABS(SCAL)/DL
            PX2=PX1+DLX
            PZ2=PZ1+DLZ
         ENDIF
         IF (MOD(IPOS1,3).NE.1) THEN
            YTIC(N+1,1)=PX2
            YTIC(N+1,2)=PY
            YTIC(N+1,3)=PZ2
         ELSE
            YTIC(N+1,1)=PX1
            YTIC(N+1,2)=PY
            YTIC(N+1,3)=PZ1
         ENDIF
         YTIC(N+1,4)=PY
 5000 CONTINUE
C     
      CALL GTYPOS(1,XTMP,ZTMP,IY1,IY2)
      CALL GTZPOS(1,XTMP,YTMP,IZ1,IZ2,NEC)
C     
      IF (IXZ.EQ.0) THEN
         IF (MOD(IPOSV1,3).EQ.2) THEN
            IDRC=0
         ELSE IF (MOD(IPOSV1,3).EQ.0) THEN
            GOTO 999
         ELSE
            IDRC=3
         ENDIF
         IPM1=1
         IPM2=1
         DO 100 I=NS+1,NE+1
            IF (MOD(IPOSV2,2).EQ.0) THEN
               IF (THETA.LE.90.0) THEN
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 3
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 1
                  GOTO 2
               ELSE
                  IPM1=-1
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 3
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 1
                  GOTO 4  
               ENDIF         
            ELSE
               IF (THETA.LE.90.0) THEN
                  IPM2=-1
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 1
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 3
                  GOTO 4
               ELSE
                  IPM1=-1
                  IPM2=-1
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 1
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 3
                  GOTO 2 
               ENDIF
            ENDIF
 1          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*1.5*CHW,IPM1*0.5*CHH,0.0,YTIC(I,4),IND,0,IDRC)
            GOTO 100
 2          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*2.0*CHH,0.0,0.0,YTIC(I,4),IND,3,IDRC)
            GOTO 100
 3          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*1.5*CHW,IPM1*0.5*CHH,0.0,YTIC(I,4),IND,1,IDRC)
            GOTO 100
 4          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*1.0*CHH,0.0,0.0,YTIC(I,4),IND,3,IDRC)
            GOTO 100
 100     CONTINUE
         GOTO 9999
 999     IDRC=7
         IPM1=1
         IPM2=1
         DO 300 I=NS+1,NE+1
            IF (MOD(IPOSV2,2).EQ.0) THEN
               IF (THETA.LE.90.0) THEN
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 23
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 21
                  GOTO 22
               ELSE
                  IPM1=-1
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 23
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 21
                  GOTO 24  
               ENDIF         
            ELSE
               IF (THETA.LE.90.0) THEN
                  IPM2=-1
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 21
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 23
                  GOTO 24
               ELSE
                  IPM1=-1
                  IPM2=-1
                  IF (IY1.EQ.1.AND.IZ1.EQ.3) GOTO 21
                  IF (IY1.EQ.2.AND.IZ1.EQ.1) GOTO 23
                  GOTO 22 
               ENDIF
            ENDIF
 21          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*1.5*CHW,0.0,IPM1*0.5*CHH,YTIC(I,4),IND,0,IDRC)
            GOTO 300
 22          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*2.0*CHH,0.0,IPM1*0.5*CHH,YTIC(I,4),IND,3,IDRC)
            GOTO 300
 23          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*1.5*CHW,0.0,IPM1*0.5*CHH,YTIC(I,4),IND,1,IDRC)
            GOTO 300
 24          CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           IPM2*1.0*CHH,0.0,IPM1*0.5*CHH,YTIC(I,4),IND,3,IDRC)
            GOTO 300
 300     CONTINUE
      ELSE IF (IXZ.EQ.1) THEN
         IF (MOD(IPOSV1-2,3).EQ.0) THEN
            IDRC=0
         ELSE IF (MOD(IPOSV1-1,3).EQ.0) THEN
            IDRC=6
         ELSE
            IDRC=7
         ENDIF
         IPM1=1
         IPM2=1
         IF (MOD(IPOSV2,2).EQ.0) THEN
         ELSE
            IPM2=-1
         ENDIF
         DO 200 I=NS+1,NE+1
            CALL GTRNUMBR(YTIC(I,1),YTIC(I,2),YTIC(I,3),
     &           0.0,0.0,IPM2*2.0*CHH,YTIC(I,4),IND,3,IDRC)
 200     CONTINUE
      ENDIF
 9999 CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
      RETURN
      END
C
C     ****** DRAW NUMBERS ON Z-AXIS ******
C
      SUBROUTINE GVALUE3DZ(ZORG,DPZ,IPOS,INDZ)
C
      COMMON /POSSCL/ I1X,I1Y,I1Z,SCX,SCY,SCAL
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DATA EPS/0.01/
C
      IF (IPOS.LT.0) THEN
         IPOS1=MOD(-IPOS,10)
         IPOS2=INT(FLOAT(-IPOS)*0.1+EPS)
      ELSE
         IPOS1=MOD(IPOS,10)
         IPOS2=INT(FLOAT(IPOS)*0.1+EPS)
      ENDIF
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      CALL GTZPOS(IPOS2+1,Z1,Z2,I1,I1,NEC)
      IF (NEC.EQ.0) THEN
         IF (ABS((Z1-GXS)/(GXE-GXS)).LT.EPS) THEN
            SCALZ=-SCAL
         ELSE
            SCALZ=SCAL
         ENDIF
      ELSE
         IF (ABS((Z2-GYS)/(GYE-GYS)).LT.EPS) THEN
            SCALZ=-SCAL
         ELSE
            SCALZ=SCAL
         ENDIF
      ENDIF
C
      CALL GTZVAL(ZORG,DPZ,Z1,Z2,NEC,SCALZ,INDZ,IPOS1,IPOS2)
C     
      CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      RETURN
      END
C
C     ****** DRAW NUMBERS ON ZAXIS ******
C
      SUBROUTINE GTZVAL(ZORG,DPZ,XPOS,YPOS,NEC,SCAL,IND,ICHAR,IPOSV)
C
      COMMON /ANGL3D/ PHI,THETA
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /POSSCL/ I1X,I1Y,IPOS,SCX,SCY,SCZ
      DIMENSION ZTIC(-1001:1001,4)
      DATA EPS/0.01/
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      IF (ZORG.LT.ZMIN.OR.ZORG.GT.ZMAX) GOTO 9999
      IF (IPOS.LT.0) THEN
         ISCL=0
         IPOS1=MOD(-IPOS,10)
      ELSE
         ISCL=1
         IPOS1=MOD(IPOS,10)
      ENDIF
      IF (IPOSV.LT.0) THEN
         IPOSV1=MOD(-IPOSV,10)
         IPOSV2=INT(-0.1*IPOSV+EPS)
      ELSE
         IPOSV1=MOD(IPOSV,10)
         IPOSV2=INT(0.1*IPOSV+EPS)
      ENDIF
C
      PZ0=ZMIN-ZORG
      PZ1=ZMAX-ZORG
      IF (ABS(DPZ/(ZMAX-ZMIN)).LT.EPS) GOTO 9999
      NS=INT((PZ0-EPS)/DPZ)
      NE=INT((PZ1+EPS)/DPZ)
C
      PX=XPOS
      PY=YPOS
      PZ0=ZORG
C
      IF (NEC.EQ.0) THEN
         PX1=PX
         PX2=PX+SCAL
         PY1=PY
         PY2=PY
      ELSE IF (NEC.EQ.1) THEN
         PX1=PX
         PX2=PX
         PY1=PY
         PY2=PY+SCAL
      ENDIF
C
      DO 3000 N=NS,NE
         PZ=PZ0+DPZ*N
         CALL GTTTB(PX1,PY1,PZ,TPX1,TPY1)
         CALL GTTTB(PX2,PY2,PZ,TPX2,TPY2)
         IF (ISCL.EQ.0) THEN
            CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
            RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)
            RATIOY=(XMAX-XMIN)/(PXMAX-PXMIN)
            DL=SQRT(((TPX2-TPX1)/RATIOX)**2+((TPY2-TPY1)/RATIOY)**2)
            DLX=(PX2-PX1)*ABS(SCAL)/DL
            DLY=(PY2-PY1)*ABS(SCAL)/DL
            PX2=PX1+DLX
            PY2=PY1+DLY
         ENDIF
         IF (IPOS1.NE.1) THEN
            ZTIC(N+1,1)=PX2
            ZTIC(N+1,2)=PY2
            ZTIC(N+1,3)=PZ
         ELSE
            ZTIC(N+1,1)=PX1
            ZTIC(N+1,2)=PY1
            ZTIC(N+1,3)=PZ
         ENDIF
         ZTIC(N+1,4)=PZ
 3000 CONTINUE
C     
      CALL GTZPOS(IPOSV2+1,XTMP,YTMP,IZ1,IZ2,NEC)
      CALL GTZPOS(1,XTMP,YTMP,IZPOS,IZPOS2,NEC2)
      IF (MOD(IPOSV1,3).EQ.2) THEN
         IDRC=0
      ELSE
         IDRC=7
      ENDIF
      IPM1=1
      IPM2=-1
      IF (THETA.GT.90.0) THEN
         IPM1=-1
      ENDIF
      DO 100 I=NS+1,NE+1
         IF (NEC.EQ.1) THEN
            DX=0.0
            DY=IPM2*1.5*CHW
         ELSE
            DX=IPM2*1.5*CHW
            DY=0.0
         ENDIF
         IF (ICHAR.EQ.2) IDRC=9999
         IF (IZ1.EQ.1) THEN
            CALL GTRNUMBR(ZTIC(I,1),ZTIC(I,2),ZTIC(I,3),
     &           DX,DY,IPM1*0.5*CHH,ZTIC(I,4),IND,1,IDRC)
         ELSE
            CALL GTRNUMBR(ZTIC(I,1),ZTIC(I,2),ZTIC(I,3),
     &           DX,DY,IPM1*0.5*CHH,ZTIC(I,4),IND,0,IDRC)
         ENDIF
 100  CONTINUE
C     
 9999 CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
      RETURN
      END
C
C
C
      SUBROUTINE GTindexx(n,arr,indx)
      INTEGER n,indx(n),M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a
      do 11 j=1,n
         indx(j)=j
 11   continue
      jstack=0
      l=1
      ir=n
 1    if(ir-l.lt.M)then
         do 13 j=l+1,ir
            indxt=indx(j)
            a=arr(indxt)
            do 12 i=j-1,1,-1
               if(arr(indx(i)).le.a)goto 2
               indx(i+1)=indx(i)
 12         continue
            i=0
 2          indx(i+1)=indxt
 13      continue
         if(jstack.eq.0)return
         ir=istack(jstack)
         l=istack(jstack-1)
         jstack=jstack-2
      else
         k=(l+ir)/2
         itemp=indx(k)
         indx(k)=indx(l+1)
         indx(l+1)=itemp
         if(arr(indx(l+1)).gt.arr(indx(ir)))then
            itemp=indx(l+1)
            indx(l+1)=indx(ir)
            indx(ir)=itemp
         endif
         if(arr(indx(l)).gt.arr(indx(ir)))then
            itemp=indx(l)
            indx(l)=indx(ir)
            indx(ir)=itemp
         endif
         if(arr(indx(l+1)).gt.arr(indx(l)))then
            itemp=indx(l+1)
            indx(l+1)=indx(l)
            indx(l)=itemp
         endif
         i=l+1
         j=ir
         indxt=indx(l)
         a=arr(indxt)
 3       continue
         i=i+1
         if(arr(indx(i)).lt.a)goto 3
 4       continue
         j=j-1
         if(arr(indx(j)).gt.a)goto 4
         if(j.lt.i)goto 5
         itemp=indx(i)
         indx(i)=indx(j)
         indx(j)=itemp
         goto 3
 5       indx(l)=indx(j)
         indx(j)=indxt
         jstack=jstack+2
         if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
         if(ir-i+1.ge.j-l)then
            istack(jstack)=ir
            istack(jstack-1)=i
            ir=j-1
         else
            istack(jstack)=j-1
            istack(jstack-1)=l
            l=i
         endif
      endif
      goto 1
      END
C -------------------------------
C     グラフの背面を塗り潰す
C -------------------------------
      SUBROUTINE GDRWBK(R,G,B)
      DIMENSION CNRPOS(2,8),XCNR(4),YCNR(4)
C
      CALL INQANG3D(THETA,PHI)
      CALL INQCNR3D(CNRPOS)
      CALL INQRGB(CR,CG,CB)
      CALL SETRGB(R,G,B)
      IF (THETA.GT.90.0) THEN
         DO I=1,4
            XCNR(I)=CNRPOS(1,I+4)
            YCNR(I)=CNRPOS(2,I+4)
         ENDDO
      ELSE
         DO I=1,4
            XCNR(I)=CNRPOS(1,I)
            YCNR(I)=CNRPOS(2,I)
         ENDDO
      ENDIF
      CALL POLY(XCNR,YCNR,4)
      XCNR(1)=CNRPOS(1,1)
      YCNR(1)=CNRPOS(2,1)
      XCNR(2)=CNRPOS(1,4)
      YCNR(2)=CNRPOS(2,4)
      XCNR(3)=CNRPOS(1,8)
      YCNR(3)=CNRPOS(2,8)
      XCNR(4)=CNRPOS(1,5)
      YCNR(4)=CNRPOS(2,5)
      CALL POLY(XCNR,YCNR,4)
      XCNR(1)=CNRPOS(1,3)
      YCNR(1)=CNRPOS(2,3)
      XCNR(2)=CNRPOS(1,4)
      YCNR(2)=CNRPOS(2,4)
      XCNR(3)=CNRPOS(1,8)
      YCNR(3)=CNRPOS(2,8)
      XCNR(4)=CNRPOS(1,7)
      YCNR(4)=CNRPOS(2,7)
      CALL POLY(XCNR,YCNR,4)
      CALL SETRGB(CR,CG,CB)
      RETURN
      END
