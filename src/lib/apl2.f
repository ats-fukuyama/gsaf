C     $Id$
C
C     ********************************************
C     ****** GSAF APPLICATION V3.5 : GRAPH1 ******
C     ********************************************
C
C     ****** DEFINE GRAPH SIZE ******
C
      SUBROUTINE GDEFIN(PXMIN,PXMAX,PYMIN,PYMAX,GXMIN,GXMAX,GYMIN,GYMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(ABS(GXMAX-GXMIN).LT.1.E-32) GOTO 9000
      IF(ABS(GYMAX-GYMIN).LT.1.E-32) GOTO 9000
      IF(PXMIN.GT.PXMAX) GOTO 9100
      IF(PYMIN.GT.PYMAX) GOTO 9100
C
      DX=(PXMAX-PXMIN)/(GXMAX-GXMIN)
      DY=(PYMAX-PYMIN)/(GYMAX-GYMIN)
      PXS=PXMIN
      PYS=PYMIN
      PXE=PXMAX
      PYE=PYMAX
      GXS=GXMIN
      GYS=GYMIN
      GXE=GXMAX
      GYE=GYMAX
      LGF=.TRUE.
      RETURN
C
 9000 WRITE(6,601) GXMIN,GXMAX,GYMIN,GYMAX
      LGF=.FALSE.
      RETURN
C
 9100 WRITE(6,602) PXMIN,PXMAX,PYMIN,PYMAX
      LGF=.FALSE.
      RETURN
C
  601 FORMAT(1H ,'XX GDEFIN PARAMETER ERROR.'/
     &       1H ,'#     GXMIN,GXMAX,GYMIN,GYMAX = '/
     &       1H ,'#     ',1P4E15.7)
  602 FORMAT(1H ,'XX GDEFIN PARAMETER ERROR.'/
     &       1H ,'#     PXMIN,PXMAX,PYMIN,PYMAX = '/
     &       1H ,'#     ',1P4E15.7)
      END
C
C     ****** INQUIRE GRAPH SIZE ******
C
      SUBROUTINE INQGDEFIN(PXMIN,PXMAX,PYMIN,PYMAX,
     &                     GXMIN,GXMAX,GYMIN,GYMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      PXMIN=PXS
      PYMIN=PYS
      PXMAX=PXE
      PYMAX=PYE
      GXMIN=GXS
      GYMIN=GYS
      GXMAX=GXE
      GYMAX=GYE
      RETURN
      END
C
C     ****** INQUIRE POSITION IN GDEFIN COORDINATES ******
C
      SUBROUTINE INQPOS2D(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
      CALL INQPOS(PX,PY)
      X=(PX-PXS)/DX+GXS
      Y=(PY-PYS)/DY+GYS
      RETURN
      END
C
C     ****** MOVE IN GDEFIN COORDINATES ******
C
      SUBROUTINE MOVE2D(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
      PX=DX*(X-GXS)+PXS
      PY=DY*(Y-GYS)+PYS
      CALL MOVE(PX,PY)
      RETURN
      END
C
C     ****** DRAW LINE ON GDEFIN COORDINATES ******
C
      SUBROUTINE DRAW2D(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
      PX=DX*(X-GXS)+PXS
      PY=DY*(Y-GYS)+PYS
      CALL DRAW(PX,PY)
      RETURN
      END
C
C     ****** MOVE PATTERN ON GDEFIN COORDINATES ******
C
      SUBROUTINE MOVEPT2D(X,Y,IPT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
      PX=DX*(X-GXS)+PXS
      PY=DY*(Y-GYS)+PYS
      CALL MOVEPT(PX,PY,IPT)
      RETURN
      END
C
C     ****** DRAW LINE PATTERN IN GDEFIN COORDINATES ******
C
      SUBROUTINE DRAWPT2D(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
      PX=DX*(X-GXS)+PXS
      PY=DY*(Y-GYS)+PYS
      CALL DRAWPT(PX,PY)
      RETURN
      END
C
C     ****** DRAW A LINE IN GDEFIN COORDINATES ******
C
      SUBROUTINE LINE2D(X1,Y1,X2,Y2)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
      CALL MOVE2D(X1,Y1)
      CALL DRAW2D(X2,Y2)
      RETURN
      END
C
C     ****** DRAW LINES IN GDEFIN COORDINATES ******
C
      SUBROUTINE LINES2D(X,Y,N)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      DIMENSION X(*),Y(*),PX(1024),PY(1024)
C
      IF(.NOT.LGF) RETURN
C
      NAMAX=(N-1)/1024+1
      DO NA=1,NAMAX
         N0=(NA-1)*1024
         N1=MIN(N-N0,1024)
C
         DO I=1,N1
            PX(I)=DX*(X(N0+I)-GXS)+PXS
            PY(I)=DY*(Y(N0+I)-GYS)+PYS
         ENDDO
         CALL LINES(PX,PY,N1)
      ENDDO
      RETURN
      END
C
C     ****** DRAW CLOSED LINES IN GDEFIN COORDINATES ******
C
      SUBROUTINE LINESC2D(X,Y,N)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      DIMENSION X(*),Y(*),PX(1024),PY(1024)
C
      IF(.NOT.LGF) RETURN
C
      NAMAX=(N-1)/1024+1
      DO NA=1,NAMAX
         N0=(NA-1)*1024
         N1=MIN(N-N0,1024)
C
         DO I=1,N1
            PX(I)=DX*(X(N0+I)-GXS)+PXS
            PY(I)=DY*(Y(N0+I)-GYS)+PYS
         ENDDO
         CALL LINESC(PX,PY,N1)
      ENDDO
      RETURN
      END
C
C     ****** FILL POLYGON IN GDEFIN COORDINATES ******
C
      SUBROUTINE POLY2D(X,Y,N)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      DIMENSION X(*),Y(*),PX(1024),PY(1024)
C
      IF(.NOT.LGF) RETURN
C
      NAMAX=(N-1)/1024+1
      DO NA=1,NAMAX
         N0=(NA-1)*1024
         N1=MIN(N-N0,1024)
C
         DO I=1,N1
            PX(I)=DX*(X(N0+I)-GXS)+PXS
            PY(I)=DY*(Y(N0+I)-GYS)+PYS
         ENDDO
         CALL POLY(PX,PY,N1)
      ENDDO
      RETURN
      END
C
C     ****** FILL TRIANGLE BY GRADATION IN GDEFIN COORDINATES ******
C
      SUBROUTINE RGBTRG2D(X,Y,R,G,B)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      DIMENSION X(*),Y(*),R(*),G(*),B(*)
      DIMENSION PX(3),PY(3)
C
      IF(.NOT.LGF) RETURN
C
      DO I=1,3
         PX(I)=DX*(X(I)-GXS)+PXS
         PY(I)=DY*(Y(I)-GYS)+PYS
      ENDDO
      CALL RGBTRG(PX,PY,R,G,B)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED TEXT ******
C
      SUBROUTINE GTEXT2D(X2D,Y2D,KTEXT,NCHAR,IJUST)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      CHARACTER KTEXT*256
C
      IF(.NOT.LGF) RETURN
C
      X = DX*(X2D-GXS)+PXS
      Y = DY*(Y2D-GYS)+PYS
      CALL GTEXT(X,Y,KTEXT,NCHAR,IJUST)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED DELIMITTED TEXT ******
C
      SUBROUTINE GTEXTX2D(X2D,Y2D,KTEXTX,IJUST)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      CHARACTER KTEXT*256
C
      IF(.NOT.LGF) RETURN
C
      X = DX*(X2D-GXS)+PXS
      Y = DY*(Y2D-GYS)+PYS
      CALL GTEXTX(X,Y,KTEXTX,IJUST)
      RETURN
      END
C
C     ****** DRAW GRAPH FRAME ******
C
      SUBROUTINE GFRAME
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
C
      IF(.NOT.LGF) RETURN
C
      CALL MOVE(PXS,PYS)
      CALL DRAW(PXE,PYS)
      CALL DRAW(PXE,PYE)
      CALL DRAW(PXS,PYE)
      CALL DRAW(PXS,PYS)
      RETURN
      END
C
C     ****** DRAW TICKS ON AXIS ******
C
      SUBROUTINE GSCALE(XORG,XSTEP,YORG,YSTEP,SLENG,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DATA EPS/0.01/
C
      IF(.NOT.LGF) RETURN
C
      LZERO=MOD(IND,16).EQ.0
      LIN=MOD(IND,2).EQ.1
      LOUT=MOD(IND/2,2).EQ.1
      LBOT=MOD((IND+4)/4,2).EQ.1
      LTOP=MOD((IND+4)/8,2).EQ.1
      LNBOT=MOD(IND/16,2).EQ.1
      LNTOP=MOD(IND/32,2).EQ.1
      IF(LZERO) THEN
         IPAT=NINT(SLENG)
      ELSE
         IPAT=0
      ENDIF
C
      DPX=ABS(DX*XSTEP)
      IF(DPX.GT.EPS.AND.
     &   (GXE-XORG)*(XORG-GXS).GE.0.0) THEN
         PX0=DX*(XORG-GXS)+PXS
         IF(LNBOT) THEN
            NS=INT((PXS-PX0+EPS)/DPX)
         ELSE
            NS=INT((PXS-PX0-EPS)/DPX)
         ENDIF
         IF(LNTOP) THEN
            NE=INT((PXE-PX0-EPS)/DPX)
         ELSE
            NE=INT((PXE-PX0+EPS)/DPX)
         ENDIF
C
         DO 2000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 2000
               PY=PYS
               SCL=SLENG
            ELSE
               IF(.NOT.LTOP) GOTO 2000
               PY=PYE
               SCL=-SLENG
            ENDIF
            PY1=PY
            IF(LOUT) PY1=PY-SCL
            PY2=PY
            IF(LIN) PY2=PY+SCL
            IF(LZERO) PY2=PYE
C
            CALL GUGRPS
            DO 1000 N=NS,NE
               PX=PX0+DPX*N
               CALL MOVEPT(PX,PY1,IPAT)
               CALL DRAWPT(PX,PY2)
 1000       CONTINUE
            CALL GUGRPE
 2000    CONTINUE
      ENDIF
C
      DPY=ABS(DY*YSTEP)
      IF(DPY.GT.EPS.AND.
     &   (GYE-YORG)*(YORG-GYS).GE.0.0) THEN
         PY0=DY*(YORG-GYS)+PYS
         IF(LNBOT) THEN
            NS=INT((PYS-PY0+EPS)/DPY)
         ELSE
            NS=INT((PYS-PY0-EPS)/DPY)
         ENDIF
         IF(LNTOP) THEN
            NE=INT((PYE-PY0-EPS)/DPY)
         ELSE
            NE=INT((PYE-PY0+EPS)/DPY)
         ENDIF
C
         DO 4000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 4000
               PX=PXS
               SCL=SLENG
            ELSE
               IF(.NOT.LTOP) GOTO 4000
               PX=PXE
               SCL=-SLENG
            ENDIF
            PX1=PX
            IF(LOUT) PX1=PX-SCL
            PX2=PX
            IF(LIN) PX2=PX+SCL
            IF(LZERO) PX2=PXE
C
            CALL GUGRPS
            DO 3000 N=NS,NE
               PY=PY0+DPY*N
               CALL MOVEPT(PX1,PY,IPAT)
               CALL DRAWPT(PX2,PY)
 3000       CONTINUE
            CALL GUGRPE
 4000    CONTINUE
      ENDIF
      RETURN
      END
C
C     ****** DRAW NUMBERS ON AXIS ******
C
      SUBROUTINE GVALUE(XORG,XSTEP,YORG,YSTEP,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION POSD(4)
      DATA POSD/2.0,1.5,1.0,0.5/
      DATA EPS/0.01/
C
      IF(.NOT.LGF) RETURN
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      IND1=MOD(ABS(IND),400)
      IF(IND.LT.0) IND1=-IND1
      IND2=ABS(IND)/400*4
      POS=POSD(MOD(IND2,4)+1)
      LBOT=MOD((IND2+4)/4,2).EQ.1
      LTOP=MOD((IND2+4)/8,2).EQ.1
      LNBOT=MOD(IND2/16,2).EQ.1
      LNTOP=MOD(IND2/32,2).EQ.1
C
      DPX=ABS(DX*XSTEP)
      IF(DPX.GT.EPS.AND.
     &   (GXE-XORG)*(XORG-GXS).GE.0.0) THEN
         IF(DX.GE.0.0) THEN
            XSTP= ABS(XSTEP)
         ELSE
            XSTP=-ABS(XSTEP)
         ENDIF
C
         PX0=DX*(XORG-GXS)+PXS
         IF(LNBOT) THEN
            NS=INT((PXS-PX0+EPS)/DPX)
         ELSE
            NS=INT((PXS-PX0-EPS)/DPX)
         ENDIF
         IF(LNTOP) THEN
            NE=INT((PXE-PX0-EPS)/DPX)
         ELSE
            NE=INT((PXE-PX0+EPS)/DPX)
         ENDIF
C
         DO 2000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 2000
               PY=PYS-0.5*(POS+2)*CHH
               IJUST=3
            ELSE
               IF(.NOT.LTOP) GOTO 2000
               PY=PYE+0.5*POS*CHH
               IJUST=3
            ENDIF
C
            DO 1000 N=NS,NE
               GX=XORG+XSTP*N
               PX=PX0+DPX*N
               CALL GNUMBR(PX,PY,GX,IND1,IJUST)
 1000       CONTINUE
 2000    CONTINUE
      ENDIF
C
      DPY=ABS(DY*YSTEP)
      IF(DPY.GT.EPS.AND.
     &   (GYE-YORG)*(YORG-GYS).GE.0.0) THEN
C
         IF(DY.GE.0.0) THEN
            YSTP= ABS(YSTEP)
         ELSE
            YSTP=-ABS(YSTEP)
         ENDIF
         PY0=DY*(YORG-GYS)+PYS
         IF(LNBOT) THEN
            NS=INT((PYS-PY0+EPS)/DPY)
         ELSE
            NS=INT((PYS-PY0-EPS)/DPY)
         ENDIF
         IF(LNTOP) THEN
            NE=INT((PYE-PY0-EPS)/DPY)
         ELSE
            NE=INT((PYE-PY0+EPS)/DPY)
         ENDIF
C
         DO 4000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 4000
               PX=PXS-0.5*POS*CHW
               IJUST=1
            ELSE
               IF(.NOT.LTOP) GOTO 4000
               PX=PXE+0.5*POS*CHW
               IJUST=0
            ENDIF
C
            DO 3000 N=NS,NE
               GY=YORG+YSTP*N
               PY=PY0+DPY*N-0.5*CHH
               CALL GNUMBR(PX,PY,GY,IND1,IJUST)
 3000       CONTINUE
 4000    CONTINUE
      ENDIF
C
      RETURN
      END
C
C     ****** DRAW TICKS ON LOG AXIS ******
C
      SUBROUTINE GSCALL(XORG,NXSTEP,YORG,NYSTEP,SLENG,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION SCAL(9,5)
      DATA SCAL/0.0000,8*1.0,
     &          0.0000,0.4771,7*1.0,
     &          0.0000,0.3010,0.6990,6*1.0,
     &          0.0000,0.3010,0.4771,0.6990,0.8451,4*1.0,
     &          0.0000,0.3010,0.4771,0.6021,0.6990,
     &          0.7782,0.8451,0.9031,0.9542/
      DATA EPS/0.01/
C
      IF(.NOT.LGF) RETURN
C
      LZERO=MOD(IND,16).EQ.0
      LIN=MOD(IND,2).EQ.1
      LOUT=MOD(IND/2,2).EQ.1
      LBOT=MOD((IND+4)/4,2).EQ.1
      LTOP=MOD((IND+4)/8,2).EQ.1
      LNBOT=MOD(IND/16,2).EQ.1
      LNTOP=MOD(IND/32,2).EQ.1
      IF(LZERO) THEN
         IPAT=NINT(SLENG)
      ELSE
         IPAT=0
      ENDIF
C
      IF(NXSTEP.GT.0.AND.
     &   (GXE-XORG)*(XORG-GXS).GE.0.0) THEN
         IXORG=INT(XORG+EPS)
         PX0=DX*(IXORG-GXS)+PXS
         IF(LNBOT) THEN
            PXSL=PXS-PX0+EPS
         ELSE
            PXSL=PXS-PX0-EPS
         ENDIF
         IF(LNTOP) THEN
            PXEL=PXE-PX0-EPS
         ELSE
            PXEL=PXE-PX0+EPS
         ENDIF
         NS=INT(PXSL/ABS(DX))
         NE=INT(PXEL/ABS(DX))
C
         DO 2000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 2000
               PY=PYS
               SCL=SLENG
            ELSE
               IF(.NOT.LTOP) GOTO 2000
               PY=PYE
               SCL=-SLENG
            ENDIF
            DSC1=0.0
            IF(LOUT) DSC1=-SCL
            DSC2=0.0
            IF(LIN)  DSC2= SCL
            IF(LZERO) DSC2=PYE-PYS
            IF(NXSTEP.LE.3) THEN
               NSC=NXSTEP
               ISC=NXSTEP
            ELSEIF(NXSTEP.LE.5) THEN
               NSC=5
               ISC=4
            ELSE
               NSC=9
               ISC=5
            ENDIF
C
            CALL GUGRPS
            DO 1000 N=NS-1,NE
              DO 900 M=1,NSC
                 PXL=ABS(DX)*(N+SCAL(M,ISC))
                 IF(PXL.LT.PXSL.OR.PXL.GT.PXEL) GOTO 900
                 IF(LZERO) THEN
                    PY1=PY+DSC1
                    PY2=PY+DSC2
                 ELSEIF(M.EQ.1) THEN
                    PY1=PY+2.0*DSC1
                    PY2=PY+2.0*DSC2
                 ELSEIF(NSC.EQ.9.AND.M.EQ.5.OR.
     &                  NSC.EQ.5.AND.M.EQ.4) THEN
                    PY1=PY+1.5*DSC1
                    PY2=PY+1.5*DSC2
                 ELSE
                    PY1=PY+DSC1
                    PY2=PY+DSC2
                 ENDIF
                 CALL MOVEPT(PX0+PXL,PY1,IPAT)
                 CALL DRAWPT(PX0+PXL,PY2)
  900          CONTINUE
 1000       CONTINUE
            CALL GUGRPE
 2000    CONTINUE
      ENDIF
C
      IF(NYSTEP.GT.0.AND.
     &   (GYE-YORG)*(YORG-GYS).GE.0.0) THEN
         IYORG=INT(YORG+EPS)
         PY0=DY*(IYORG-GYS)+PYS
         IF(LNBOT) THEN
            PYSL=PYS-PY0+EPS
         ELSE
            PYSL=PYS-PY0-EPS
         ENDIF
         IF(LNTOP) THEN
            PYEL=PYE-PY0-EPS
         ELSE
            PYEL=PYE-PY0+EPS
         ENDIF
         NS=INT(PYSL/ABS(DY))
         NE=INT(PYEL/ABS(DY))
C
         DO 4000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 4000
               PX=PXS
               SCL=SLENG
            ELSE
               IF(.NOT.LTOP) GOTO 4000
               PX=PXE
               SCL=-SLENG
            ENDIF
            DSC1=0.0
            IF(LOUT) DSC1=-SCL
            DSC2=0.0
            IF(LIN)  DSC2= SCL
            IF(LZERO) DSC2=PXE-PXS
            IF(NYSTEP.LE.3) THEN
               NSC=NYSTEP
               ISC=NYSTEP
            ELSEIF(NYSTEP.LE.5) THEN
               NSC=5
               ISC=4
            ELSE
               NSC=9
               ISC=5
            ENDIF
C
            CALL GUGRPS
            DO 3000 N=NS-1,NE
              DO 2900 M=1,NSC
                  PYL=ABS(DY)*(N+SCAL(M,ISC))
                 IF(PYL.LT.PYSL.OR.PYL.GT.PYEL) GOTO 2900
                 IF(LZERO) THEN
                    PX1=PX+DSC1
                    PX2=PX+DSC2
                 ELSEIF(M.EQ.1) THEN
                    PX1=PX+2.0*DSC1
                    PX2=PX+2.0*DSC2
                 ELSEIF(NSC.EQ.9.AND.M.EQ.5.OR.
     &                  NSC.EQ.5.AND.M.EQ.4) THEN
                    PX1=PX+1.5*DSC1
                    PX2=PX+1.5*DSC2
                 ELSE
                    PX1=PX+DSC1
                    PX2=PX+DSC2
                 ENDIF
                 CALL MOVEPT(PX1,PY0+PYL,IPAT)
                 CALL DRAWPT(PX2,PY0+PYL)
 2900          CONTINUE
 3000       CONTINUE
            CALL GUGRPE
 4000    CONTINUE
      ENDIF
C
      RETURN
      END
C
C     ****** DRAW NUMBERS ON LOG AXIS ******
C
      SUBROUTINE GVALUL(XORG,NXSTEP,YORG,NYSTEP,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION POSD(4)
      DIMENSION SCAL(9,5),INUM(9,5)
      DATA POSD/2.0,1.5,1.0,0.5/
      DATA SCAL/0.0000,8*1.0,
     &          0.0000,0.4771,7*1.0,
     &          0.0000,0.3010,0.6990,6*1.0,
     &          0.0000,0.3010,0.4771,0.6990,0.8451,4*1.0,
     &          0.0000,0.3010,0.4771,0.6021,0.6990,
     &          0.7782,0.8451,0.9031,0.9542/
      DATA INUM/0,8*0,
     &          0,3,7*0,
     &          0,2,5,6*0,
     &          0,2,3,5,7,4*0,
     &          0,2,3,4,5,6,7,8,9/
      DATA EPS/0.01/
C
      IF(.NOT.LGF) RETURN
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      IND1=MOD(ABS(IND),400)
      IF(IND.LT.0) IND1=-IND1
      IND2=ABS(IND)/400*4
      POS=POSD(MOD(IND2,4)+1)
      LBOT=MOD((IND2+4)/4,2).EQ.1
      LTOP=MOD((IND2+4)/8,2).EQ.1
      LNBOT=MOD(IND2/16,2).EQ.1
      LNTOP=MOD(IND2/32,2).EQ.1
C
      IF(NXSTEP.GT.0.AND.
     &   (GXE-XORG)*(XORG-GXS).GE.0.0) THEN
         IXORG=INT(XORG+EPS)
         IF(DX.GE.0.0) THEN
            IXSTP= 1
         ELSE
            IXSTP=-1
         ENDIF
         PX0=DX*(IXORG-GXS)+PXS
         IF(LNBOT) THEN
            PXSL=PXS-PX0+EPS
         ELSE
            PXSL=PXS-PX0-EPS
         ENDIF
         IF(LNTOP) THEN
            PXEL=PXE-PX0-EPS
         ELSE
            PXEL=PXE-PX0+EPS
         ENDIF
         NS=INT(PXSL/ABS(DX))
         NE=INT(PXEL/ABS(DX))
C
         DO 2000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 2000
               PY=PYS-0.5*(POS+2)*CHH
               IJUST=2
            ELSE
               IF(.NOT.LTOP) GOTO 2000
               PY=PYE+0.5*POS*CHH
               IJUST=2
            ENDIF
            IF(NXSTEP.LE.3) THEN
               NSC=NXSTEP
               ISC=NXSTEP
            ELSEIF(NXSTEP.LE.5) THEN
               NSC=5
               ISC=4
            ELSE
               NSC=9
               ISC=5
            ENDIF
C
            DO 1000 N=NS-1,NE
               DO 900 M=1,NSC
                 IGX=IXORG+IXSTP*N
                 PXL=ABS(DX)*(N+SCAL(M,ISC))
                 IF(PXL.LT.PXSL.OR.PXL.GT.PXEL) GOTO 900
                 IF(M.EQ.1) THEN
                   CALL GNUMBP(PX0+PXL,PY,10,IGX,IJUST)
                 ELSE
                   CALL GNUMBI(PX0+PXL,PY,INUM(M,ISC),IJUST)
                 ENDIF
  900          CONTINUE
 1000       CONTINUE
 2000    CONTINUE
      ENDIF
C
      IF(NYSTEP.GT.0.AND.
     &   (GYE-YORG)*(YORG-GYS).GE.0.0) THEN
         IYORG=INT(YORG+EPS)
         IF(DY.GE.0.0) THEN
            IYSTP= 1
         ELSE
            IYSTP=-1
         ENDIF
         PY0=DY*(IYORG-GYS)+PYS
         IF(LNBOT) THEN
            PYSL=PYS-PY0+EPS
         ELSE
            PYSL=PYS-PY0-EPS
         ENDIF
         IF(LNTOP) THEN
            PYEL=PYE-PY0-EPS
         ELSE
            PYEL=PYE-PY0+EPS
         ENDIF
         NS=INT(PYSL/ABS(DY))
         NE=INT(PYEL/ABS(DY))
C
         DO 4000 I=1,2
            IF(I.EQ.1) THEN
               IF(.NOT.LBOT) GOTO 4000
               PX=PXS-0.5*POS*CHW
               IJUST=1
            ELSE
               IF(.NOT.LTOP) GOTO 4000
               PX=PXE+0.5*POS*CHW
               IJUST=0
            ENDIF
            IF(NYSTEP.LE.3) THEN
               NSC=NYSTEP
               ISC=NYSTEP
            ELSEIF(NYSTEP.LE.5) THEN
               NSC=5
               ISC=4
            ELSE
               NSC=9
               ISC=5
            ENDIF
C
            DO 3000 N=NS-1,NE
               DO 2900 M=1,NSC
                  IGY=IYORG+IYSTP*N
                  PYL=ABS(DY)*(N+SCAL(M,ISC))
                  IF(PYL.LT.PYSL.OR.PYL.GT.PYEL) GOTO 2900
                  PY=PY0+PYL-0.5*CHH
                  IF(M.EQ.1) THEN
                     CALL GNUMBP(PX,PY,10,IGY,IJUST)
                  ELSE
                     CALL GNUMBI(PX,PY,INUM(M,ISC),IJUST)
                  ENDIF
 2900          CONTINUE
 3000       CONTINUE
 4000    CONTINUE
      ENDIF
C
      RETURN
      END
C
C     ****** DRAW XY GRAPH ******
C
      SUBROUTINE GPLOTP(GX,GY,NSRT,NEND,NSTEP,IMARK,ISTEP,IPAT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION GX(NEND),GY(NEND)
C
      IF(.NOT.LGF) RETURN
C
      CALL SETCLP(PXS,PXE,PYS,PYE)
      IF(IMARK.EQ.0) THEN
         CALL GUGRPS
         PX=DX*(GX(NSRT)-GXS)+PXS
         PY=DY*(GY(NSRT)-GYS)+PYS
         CALL MOVEPT(PX,PY,IPAT)
         DO 1010 N=NSRT+NSTEP,NEND,NSTEP
            PX=DX*(GX(N)-GXS)+PXS
            PY=DY*(GY(N)-GYS)+PYS
            CALL DRAWPT(PX,PY)
 1010    CONTINUE
         CALL GUGRPE
      ELSEIF(IMARK.GT.0) THEN
         CALL INQMRK(IMARKS,HMRK,WMRK,ANGL,TILT)
         CALL SETMRK(IMARK,HMRK,WMRK,ANGL,TILT)
         CALL GUGRPS
         DO 2010 N=NSRT,NEND,NSTEP
            PX=DX*(GX(N)-GXS)+PXS
            PY=DY*(GY(N)-GYS)+PYS
            CALL MARK(PX,PY)
 2010    CONTINUE
         CALL GUGRPE
         CALL SETMRK(IMARKS,HMRK,WMRK,ANGL,TILT)
      ELSE
         CALL GUGRPS
         PX=DX*(GX(NSRT)-GXS)+PXS
         PY=DY*(GY(NSRT)-GYS)+PYS
         CALL MOVEPT(PX,PY,IPAT)
         DO 3010 N=NSRT+NSTEP,NEND,NSTEP
            PX=DX*(GX(N)-GXS)+PXS
            PY=DY*(GY(N)-GYS)+PYS
            CALL DRAWPT(PX,PY)
 3010    CONTINUE
         CALL GUGRPE
         CALL INQMRK(IMARKS,HMRK,WMRK,ANGL,TILT)
         CALL SETMRK(ABS(IMARK),HMRK,WMRK,ANGL,TILT)
         CALL GUGRPS
         DO 3020 N=NSRT,NEND,NSTEP*ISTEP
            PX=DX*(GX(N)-GXS)+PXS
            PY=DY*(GY(N)-GYS)+PYS
            CALL MARK(PX,PY)
 3020    CONTINUE
         CALL GUGRPE
         CALL SETMRK(IMARKS,HMRK,WMRK,ANGL,TILT)
      ENDIF
      CALL OFFCLP
      RETURN
      END
C
C     ****** DRAW XY ERRORBAR GRAPH ******
C
      SUBROUTINE GPLOTPE(GX,GY1,GY2,NSRT,NEND,NSTEP,SCAL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION GX(NEND),GY1(NEND),GY2(NEND)
C
      IF(.NOT.LGF) RETURN
C
      CALL SETCLP(PXS,PXE,PYS,PYE)
C
         CALL GUGRPS
         DO N=NSRT,NEND,NSTEP
            CALL GUGRPS
            PX=DX*(GX(N)-GXS)+PXS
            PY1=DY*(GY1(N)-GYS)+PYS
            PY2=DY*(GY2(N)-GYS)+PYS
            IF(SCAL.GT.0.0) THEN
               CALL MOVE(PX-0.5*SCAL,PY1)
               CALL DRAW(PX+0.5*SCAL,PY1)
            ENDIF
            CALL MOVE(PX,PY1)
            CALL DRAW(PX,PY2)
            IF(SCAL.GT.0.0) THEN
               CALL MOVE(PX-0.5*SCAL,PY2)
               CALL DRAW(PX+0.5*SCAL,PY2)
            ENDIF
            CALL GUGRPE
         ENDDO
         CALL GUGRPE
      CALL OFFCLP
      RETURN
      END
C
C     ****** DRAW VECTOR GRAPH ******
C
      SUBROUTINE GVECTR(VX,VY,X,Y,NXA,NXS,NXE,NXSTEP,
     &                                NYS,NYE,NYSTEP,XN,YN,AL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION VX(NXA,NYE),VY(NXA,NYE),X(NXE),Y(NYE)
      DATA ROOT3/1.732051/
C
      IF(.NOT.LGF) RETURN
      PXN=1./XN
      PYN=1./YN
C
      CALL SETCLP(PXS,PXE,PYS,PYE)
C
      CALL GUGRPS
      DO 20 NX=NXS,NXE,NXSTEP
      DO 20 NY=NYS,NYE,NYSTEP
         PX=PXS+DX*(X(NX)-GXS)
         PY=PYS+DY*(Y(NY)-GYS)
         PVX=VX(NX,NY)*PXN
         PVY=VY(NX,NY)*PYN
         PVA=SQRT(PVX*PVX+PVY*PVY)
C
         IF(PVA.GT.0.01) THEN
            ACOS1=-ROOT3*PVX      -PVY
            ASIN1=       PVX-ROOT3*PVY
            ACOS2=-ROOT3*PVX      +PVY
            ASIN2=      -PVX-ROOT3*PVY
C
            PXT=PX
            PYT=PY
            PXE=PX+PVX
            PYE=PY+PVY
            PX1=PXE+AL*ACOS1
            PY1=PYE+AL*ASIN1
            PX2=PXE+AL*ACOS2
            PY2=PYE+AL*ASIN2
C
            CALL GUGRPS
            CALL MOVE(PXT,PYT)
            CALL DRAW(PXE,PYE)
            CALL DRAW(PX1,PY1)
            CALL MOVE(PX2,PY2)
            CALL DRAW(PXE,PYE)
            CALL GUGRPE
         ENDIF
   20 CONTINUE
      CALL GUGRPE
      CALL OFFCLP
      RETURN
      END
