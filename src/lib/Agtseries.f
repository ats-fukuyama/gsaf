C     $Id$
C     *************************************************
C     ********** GSAF 3D ROUTINES : INTERNAL **********
C     *************************************************
C
C     ****** INQUIRE X-AXIS'S POSITION ******
C
      SUBROUTINE GTXPOS(IPOS,Y,Z,I1,I2)
C
C -------------------------------------------------------
C     IPOS  : POSITION 1 or 2 or 3 or 4
C     I1,I2 : 目的のx軸の両端の位置
C             1～4 ... z=zmin 平面で最も左に見える角が1で，
C                      以下上側からみて反時計回り．
C             5～8 ... z=zmax 平面で最も左に見える角が5で，
C                      以下上側からみて反時計回り．
C -------------------------------------------------------
      COMMON /ANGL3D/ PHI,THETA
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
      IF (THETA.LE.90.0) THEN
         Z1=ZMIN
         Z2=ZMAX
      ELSE IF (THETA.GT.90.0) THEN
         Z1=ZMAX
         Z2=ZMIN
      ENDIF
      IF (PHI.LT.90.0) THEN
         IF (IPOS.EQ.1) THEN
            Y=GYE
            Z=Z1
            I1=2
            I2=3
         ELSE IF (IPOS.EQ.2) THEN
            Y=GYS
            Z=Z1
            I1=1
            I2=4
         ELSE IF (IPOS.EQ.3) THEN
            Y=GYE
            Z=Z2
            I1=6
            I2=7
         ELSE IF (IPOS.EQ.4) THEN
            Y=GYS
            Z=Z2
            I1=5
            I2=8
         ENDIF
      ELSE IF (PHI.LT.180.0) THEN
         IF (IPOS.EQ.1) THEN
            Y=GYE
            Z=Z1
            I1=1
            I2=2
         ELSE IF (IPOS.EQ.2) THEN
            Y=GYS
            Z=Z1
            I1=4
            I2=3
         ELSE IF (IPOS.EQ.3) THEN
            Y=GYE
            Z=Z2
            I1=5
            I2=6
         ELSE IF (IPOS.EQ.4) THEN
            Y=GYS
            Z=Z2
            I1=8
            I2=7
         ENDIF
      ELSE IF (PHI.LT.270.0) THEN
         IF (IPOS.EQ.1) THEN
            Y=GYS
            Z=Z1
            I1=2
            I2=3
         ELSE IF (IPOS.EQ.2) THEN
            Y=GYE
            Z=Z1
            I1=1
            I2=4
         ELSE IF (IPOS.EQ.3) THEN
            Y=GYS
            Z=Z2
            I1=6
            I2=7
         ELSE IF (IPOS.EQ.4) THEN
            Y=GYE
            Z=Z2
            I1=5
            I2=8
         ENDIF
      ELSE IF (PHI.LT.360.0) THEN
         IF (IPOS.EQ.1) THEN
            Y=GYS
            Z=Z1
            I1=1
            I2=2
         ELSE IF (IPOS.EQ.2) THEN
            Y=GYE
            Z=Z1
            I1=4
            I2=3
         ELSE IF (IPOS.EQ.3) THEN
            Y=GYS
            Z=Z2
            I1=5
            I2=6
         ELSE IF (IPOS.EQ.4) THEN
            Y=GYE
            Z=Z2
            I1=8
            I2=7
         ENDIF
      ENDIF
C
      RETURN
      END
C
C     ****** INQUIRE Y-AXIS'S POSITION ******
C
      SUBROUTINE GTYPOS(IPOS,X,Z,I1,I2)
C
C -------------------------------------------------------
C     IPOS  : POSITION 1 or 2 or 3 or 4
C     I1,I2 : 目的のx軸の両端の位置
C             1～4 ... z=zmin 平面で最も左に見える角が1で，
C                      以下上側からみて反時計回り．
C             5～8 ... z=zmax 平面で最も左に見える角が5で，
C                      以下上側からみて反時計回り．
C -------------------------------------------------------
      COMMON /ANGL3D/ PHI,THETA
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
      IF (THETA.LE.90.0) THEN
         Z1=ZMIN
         Z2=ZMAX
      ELSE IF (THETA.GT.90.0) THEN
         Z1=ZMAX
         Z2=ZMIN
      ENDIF
      IF (PHI.LT.90.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXE
            Z=Z1
            I1=1
            I2=2
         ELSE IF (IPOS.EQ.2) THEN
            X=GXS
            Z=Z1
            I1=4
            I2=3
         ELSE IF (IPOS.EQ.3) THEN
            X=GXE
            Z=Z2
            I1=5
            I2=6
         ELSE IF (IPOS.EQ.4) THEN
            X=GXS
            Z=Z2
            I1=8
            I2=7
         ENDIF
      ELSE IF (PHI.LT.180.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXS
            Z=Z1
            I1=2
            I2=3
         ELSE IF (IPOS.EQ.2) THEN
            X=GXE
            Z=Z1
            I1=1
            I2=4
         ELSE IF (IPOS.EQ.3) THEN
            X=GXS
            Z=Z2
            I1=6
            I2=7
         ELSE IF (IPOS.EQ.4) THEN
            X=GXE
            Z=Z2
            I1=5
            I2=8
         ENDIF
      ELSE IF (PHI.LT.270.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXS
            Z=Z1
            I1=1
            I2=2
         ELSE IF (IPOS.EQ.2) THEN
            X=GXE
            Z=Z1
            I1=4
            I2=3
         ELSE IF (IPOS.EQ.3) THEN
            X=GXS
            Z=Z2
            I1=5
            I2=6
         ELSE IF (IPOS.EQ.4) THEN
            X=GXE
            Z=Z2
            I1=8
            I2=7
         ENDIF
      ELSE IF (PHI.LT.360.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXE
            Z=Z1
            I1=2
            I2=3
         ELSE IF (IPOS.EQ.2) THEN
            X=GXS
            Z=Z1
            I1=1
            I2=4
         ELSE IF (IPOS.EQ.3) THEN
            X=GXE
            Z=Z2
            I1=6
            I2=7
         ELSE IF (IPOS.EQ.4) THEN
            X=GXS
            Z=Z2
            I1=5
            I2=8
         ENDIF
      ENDIF
C
      RETURN
      END
C
C     ****** INQUIRE Z-AXIS'S POSITION ******
C
      SUBROUTINE GTZPOS(IPOS,X,Y,I1,I2,NEC)
C
C -------------------------------------------------------
C     IPOS  : POSITION 1 or 2 or 3 or 4
C     I1,I2 : 目的のx軸の両端の位置
C             1～4 ... z=zmin 平面で最も左に見える角が1で，
C                      以下上側からみて反時計回り．
C             5～8 ... z=zmax 平面で最も左に見える角が5で，
C                      以下上側からみて反時計回り．
C     NEC   : 目盛りを普通に(IPOSが0でないとき)描く場合，
C             その目盛りがx軸と平行ならば0，y軸と平行ならば
C             1となる．
C -------------------------------------------------------
      COMMON /ANGL3D/ PHI,THETA
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
      IF (IPOS.EQ.3) THEN
         I1=2
         I2=6
      ELSE IF (IPOS.EQ.4) THEN
         I1=4
         I2=8
      ENDIF
      IF (PHI.LT.45.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXE
            Y=GYS
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.2) THEN
            X=GXS
            Y=GYE
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.3) THEN
            X=GXE
            Y=GYE
         ELSE IF (IPOS.EQ.4) THEN
            X=GXS
            Y=GYS
         ENDIF
         NEC=1
      ELSE IF (PHI.LT.90.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXS
            Y=GYE
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.2) THEN
            X=GXE
            Y=GYS
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.3) THEN
            X=GXE
            Y=GYE
         ELSE IF (IPOS.EQ.4) THEN
            X=GXS
            Y=GYS
         ENDIF
         NEC=0
      ELSE IF (PHI.LT.135.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXE
            Y=GYE
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.2) THEN
            X=GXS
            Y=GYS
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.3) THEN
            X=GXS
            Y=GYE
         ELSE IF (IPOS.EQ.4) THEN
            X=GXE
            Y=GYS
         ENDIF
         NEC=0
      ELSE IF (PHI.LT.180.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXS
            Y=GYS
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.2) THEN
            X=GXE
            Y=GYE
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.3) THEN
            X=GXS
            Y=GYE
         ELSE IF (IPOS.EQ.4) THEN
            X=GXE
            Y=GYS
         ENDIF
         NEC=1
      ELSE IF (PHI.LT.225.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXS
            Y=GYE
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.2) THEN
            X=GXE
            Y=GYS
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.3) THEN
            X=GXS
            Y=GYS
         ELSE IF (IPOS.EQ.4) THEN
            X=GXE
            Y=GYE
         ENDIF
         NEC=1
      ELSE IF (PHI.LT.270.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXE
            Y=GYS
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.2) THEN
            X=GXS
            Y=GYE
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.3) THEN
            X=GXS
            Y=GYS
         ELSE IF (IPOS.EQ.4) THEN
            X=GXE
            Y=GYE
         ENDIF
         NEC=0
      ELSE IF (PHI.LT.315.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXS
            Y=GYS
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.2) THEN
            X=GXE
            Y=GYE
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.3) THEN
            X=GXE
            Y=GYS
         ELSE IF (IPOS.EQ.4) THEN
            X=GXS
            Y=GYE
         ENDIF
         NEC=0
      ELSE IF (PHI.LT.360.0) THEN
         IF (IPOS.EQ.1) THEN
            X=GXE
            Y=GYE
            I1=3
            I2=7
         ELSE IF (IPOS.EQ.2) THEN
            X=GXS
            Y=GYS
            I1=1
            I2=5
         ELSE IF (IPOS.EQ.3) THEN
            X=GXE
            Y=GYS
         ELSE IF (IPOS.EQ.4) THEN
            X=GXS
            Y=GYE
         ENDIF
         NEC=1
      ENDIF
C
      RETURN
      END
C
C     ****** RMOVE FOR 3D ******
C
      SUBROUTINE GTRMOVE(X,Y,Z,DX1,DY1,DZ1,XT,YT)
C
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXX,NYY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      DATA EPS/0.01/
C     
      DEG=180.0/3.141593
      CALL GTTTB(X,Y,Z,XP,YP)
C
      IF (ABS(DX1).LT.EPS) THEN
         A=0.0
         DX=DX1  
         GOTO 2
      ENDIF
      DX=DX1     
      DELX=0.1*(GXE-GXS)
      CALL GTTTB(X+DELX,Y,Z,X1,Y1)
      DL=SQRT((Y1-YP)**2+(X1-XP)**2)
      A=ACOS((X1-XP)/DL)
      IF (Y1-YP.LT.0.0.AND.A*DEG.LT.90.0) THEN
         A=-A
      ELSE IF (Y1-YP.LT.0.0.AND.A*DEG.GE.90.0) THEN
         A=360.0/DEG-A
      ENDIF
      CALL GTYPOS(1,Y1,Y3,I1,I1)    
      IF (ABS((Y1-GXS)/(GXE-GXS)).LT.EPS) DX=-DX
C
 2    IF (ABS(DY1).LT.EPS) THEN
         B=0.0
         DY=DY1   
         GOTO 3
      ENDIF
      DY=DY1     
      DELY=0.1*(GYE-GYS)
      CALL GTTTB(X,Y+DELY,Z,X2,Y2)
      DL=SQRT((Y2-YP)**2+(X2-XP)**2)
      B=ACOS((X2-XP)/DL)
      IF (Y2-YP.LT.0.0.AND.B*DEG.LT.90.0) THEN
         B=-B
      ELSE IF (Y2-YP.LT.0.0.AND.B*DEG.GE.90.0) THEN
         B=360.0/DEG-B
      ENDIF
      CALL GTXPOS(1,X2,X3,I1,I1)       
      IF (ABS((X2-GYS)/(GYE-GYS)).LT.EPS) DY=-DY
C
 3    IF (ABS(DZ1).LT.EPS) THEN
         C=0.0
         DZ=DZ1   
         GOTO 9999
      ENDIF
      DZ=DZ1
      DELZ=0.1*(ZMAX-ZMIN)
      CALL GTTTB(X,Y,Z+DELZ,X3,Y3)
      DL=SQRT((Y3-YP)**2+(X3-XP)**2)
      C=ACOS((X3-XP)/DL)
      IF (Y3-YP.LT.0.0.AND.C*DEG.LT.90.0) THEN
         C=-C
      ELSE IF (Y3-YP.LT.0.0.AND.C*DEG.GE.90.0) THEN
         C=360.0/DEG-C
      ENDIF
      CALL GTXPOS(1,X2,X3,I1,I1)
      IF (ABS((X3-ZMIN)/(ZMAX-ZMIN)).LT.EPS) DZ=-DZ
C
 9999 CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      CALL INQ_PAGE(PXOFFSET,PYOFFSET,PXSCALE,PYSCALE)
      RATIOX=(XMAX-XMIN)/(PXMAX-PXMIN)*PXSCALE
      RATIOY=(YMAX-YMIN)/(PYMAX-PYMIN)*PYSCALE
      XT=XP+(DX*COS(A)+DY*COS(B)+DZ*COS(C))*RATIOX
      YT=YP+(DX*SIN(A)+DY*SIN(B)+DZ*SIN(C))*RATIOY
      RETURN
      END
C
C     ****** GNUMBER3D WITH GTRMOVE ******
C
      SUBROUTINE GTRNUMBR(X,Y,Z,DX1,DY1,DZ1,R,IND,IJUST,IDRC)
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      CALL GTRMOVE(X,Y,Z,DX1,DY1,DZ1,XT,YT)
      CALL GTSETCHR(X,Y,Z,IDRC)
      CALL GNUMBR(XT,YT,R,IND,IJUST)
      CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      RETURN
      END
C
C     ****** SETCHR FOR 3D ******
C
      SUBROUTINE GTSETCHR(X,Y,Z,IDRC)
C
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NXX,NYY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
      DEG=180.0/3.141593
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      CALL GTTTB(X,Y,Z,XP,YP)
      IF (IDRC.EQ.0) GOTO 9999
      DELX=0.1*(GXE-GXS)
      DELY=0.1*(GYE-GYS)
      DELZ=0.1*(ZMAX-ZMIN)
      CALL GTTTB(X+DELX,Y,Z,X1,Y1)
      CALL GTTTB(X,Y+DELY,Z,X2,Y2)
      CALL GTTTB(X,Y,Z+DELZ,X3,Y3)
      A=ATAN((Y1-YP)/(X1-XP))
      B=ATAN((Y2-YP)/(X2-XP))
      C=ATAN((Y3-YP)/(X3-XP))
      AB=B-A
      BC=C-B
      CA=A-C
      IF (IDRC.EQ.1) THEN
         GOTO 1
      ELSE IF (IDRC.EQ.2) THEN
         GOTO 2
      ELSE IF (IDRC.EQ.3) THEN
         GOTO 3
      ELSE IF (IDRC.EQ.4) THEN
         GOTO 4
      ELSE IF (IDRC.EQ.5) THEN
         GOTO 5
      ELSE IF (IDRC.EQ.6) THEN
         GOTO 6 
      ELSE IF (IDRC.EQ.7) THEN
         GOTO 7
      ELSE IF (IDRC.EQ.8) THEN
         GOTO 8
      ELSE IF (IDRC.EQ.9999) THEN
         GOTO 9999
      ENDIF
 1    CALL SETCHR(CHH,CHW,CHSP,A*DEG,90.0+AB*DEG)
      GOTO 9999
C
 2    CALL SETCHR(CHH,CHW,CHSP,B*DEG,90.0-AB*DEG)
      GOTO 9999
C
 3    CALL GTXPOS(1,YTMP,ZTMP,I1,ITMP)
      CALL GTZPOS(1,XTMP,YTMP,I3,ITMP,NEC)
      IF (I1.EQ.I3.OR.(I1.EQ.2.AND.I3.EQ.3)) THEN
         GOTO 1
      ELSE
         GOTO 2
      ENDIF
C
 4    CALL GTXPOS(1,YTMP,ZTMP,I1,ITMP)
      CALL GTZPOS(1,XTMP,YTMP,I3,ITMP,NEC)
      IF (I1.EQ.I3.OR.(I1.EQ.2.AND.I3.EQ.3)) THEN
         GOTO 2
      ELSE
         GOTO 1
      ENDIF    
C
 5    CALL SETCHR(CHH,CHW,CHSP,A*DEG,90.0-CA*DEG)
      GOTO 9999
C
 6    CALL SETCHR(CHH,CHW,CHSP,B*DEG,90.0+BC*DEG)
      GOTO 9999
C
 7    CALL GTXPOS(1,YTMP,ZTMP,I1,ITMP)
      CALL GTZPOS(1,XTMP,YTMP,I3,ITMP,NEC)
      IF (I1.EQ.I3.OR.(I1.EQ.2.AND.I3.EQ.3)) THEN
         GOTO 5
      ELSE
         GOTO 6
      ENDIF
      GOTO 9999
C
 8    CALL GTXPOS(1,YTMP,ZTMP,I1,ITMP)
      CALL GTZPOS(1,XTMP,YTMP,I3,ITMP,NEC)
      IF (I1.EQ.I3.OR.(I1.EQ.2.AND.I3.EQ.3)) THEN
         GOTO 6
      ELSE
         GOTO 5
      ENDIF
      GOTO 9999
C
 9999 RETURN
      END
C
C     ****** GTEXTX FOR 3D ******
C
      SUBROUTINE GTRTEXTX(X,Y,Z,DX1,DY1,DZ1,KTEXT,IJUST,IDRC)
C
      CHARACTER*254 KTEXT
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      CALL GTSETCHR(X,Y,Z,IDRC)
      CALL GTRMOVE(X,Y,Z,DX1,DY1,DZ1,XT,YT)
      CALL GTEXTX(XT,YT,KTEXT,IJUST)
      CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      RETURN
      END
