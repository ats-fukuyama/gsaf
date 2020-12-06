C     $Id$C
C
C     ****************************************************
C     ****** GSAF BASIC ROUTINES V3.5 : INTERFACE 2 ******
C     ****************************************************
C
C     ****** DRAW A LINE ******
C
      SUBROUTINE LINE1(XS,YS,XE,YE)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      DIMENSION XA(2),YA(2),XB(3),YB(3)
      DIMENSION IX(3),IY(3)
C
      IF(.NOT.LPAGE) RETURN
C
         XA(1)=XS
         YA(1)=YS
         XA(2)=XE
         YA(2)=YE
C
      CALL GUPCLP(XA,YA,2,XB,YB,M)
C
      DO I=1,M
         IX(I)=NINT(XB(I)*XDEL+XORG)
         IY(I)=NINT(YB(I)*YDEL+YORG)
         IF(IX(I).LT.0)     IX(I)=0
         IF(IX(I).GT.32767) IX(I)=32767
         IF(IY(I).LT.0)     IY(I)=0
         IF(IY(I).GT.32767) IY(I)=32767
      ENDDO
C
      IF(M.GT.1) THEN
         CALL GUGRPS
         CALL DVLINS(IX,IY,M)
         IF(LFIL) CALL BUFFXN(11,IX,IY,M)
         CALL GUGRPE
      ENDIF
C
      X1=XE
      Y1=YE
      LMV=.FALSE.
      RETURN
      END
C
C     ****** DRAW LINES ******
C
      SUBROUTINE LINES(X,Y,N)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      DIMENSION X(*),Y(*),XA(1024),YA(1024),XB(2048),YB(2048)
      DIMENSION IX(2048),IY(2048)
C
      IF(.NOT.LPAGE) RETURN
C
      NBMAX=(N-1)/1024+1
      DO NB=1,NBMAX
         N0=(NB-1)*1024
         N1=MIN(N-N0,1024)
C
      DO I=1,N1
         XA(I)=X(N0+I)
         YA(I)=Y(N0+I)
      ENDDO
C
      CALL GUPCLP(XA,YA,N1,XB,YB,M)
C
      DO I=1,M
         IX(I)=NINT(XB(I)*XDEL+XORG)
         IY(I)=NINT(YB(I)*YDEL+YORG)
         IF(IX(I).LT.0)     IX(I)=0
         IF(IX(I).GT.32767) IX(I)=32767
         IF(IY(I).LT.0)     IY(I)=0
         IF(IY(I).GT.32767) IY(I)=32767
      ENDDO
C
      IF(M.GT.1) THEN
         CALL GUGRPS
         CALL DVLINS(IX,IY,M)
         IF(LFIL) CALL BUFFXN(11,IX,IY,M)
         CALL GUGRPE
      ENDIF
      ENDDO
      X1=X(N)
      Y1=Y(N)
      LMV=.FALSE.
      RETURN
      END
C
C     ****** DRAW CLOSED LINES ******
C
      SUBROUTINE LINESC(X,Y,N)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      DIMENSION X(*),Y(*),XA(1025),YA(1025),XB(2050),YB(2050)
      DIMENSION IX(2050),IY(2050)
C
      IF(.NOT.LPAGE) RETURN
C
      NBMAX=(N-1)/1024+1
      DO NB=1,NBMAX
         N0=(NB-1)*1024
         N1=MIN(N-N0,1024)
C
      DO I=1,N1
         XA(I)=X(N0+I)
         YA(I)=Y(N0+I)
      ENDDO
      IF(N0+N1.EQ.N) THEN
         N1=N1+1
         XA(N1)=X(1)
         YA(N1)=Y(1)
      ENDIF
C
      CALL GUPCLP(XA,YA,N1,XB,YB,M)
C
      DO I=1,M
         IX(I)=NINT(XB(I)*XDEL+XORG)
         IY(I)=NINT(YB(I)*YDEL+YORG)
         IF(IX(I).LT.0)     IX(I)=0
         IF(IX(I).GT.32767) IX(I)=32767
         IF(IY(I).LT.0)     IY(I)=0
         IF(IY(I).GT.32767) IY(I)=32767
      ENDDO
C
      IF(M.GT.1) THEN
         CALL GUGRPS
         CALL DVLINS(IX,IY,M)
         IF(LFIL) CALL BUFFXN(11,IX,IY,M)
         CALL GUGRPE
      ENDIF
      ENDDO
      X1=X(N)
      Y1=Y(N)
      LMV=.FALSE.
      RETURN
      END
C
C     ****** FILL POLYGON ******
C
      SUBROUTINE POLY(X,Y,N)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      DIMENSION X(*),Y(*),XA(1025),YA(1025),XB(2050),YB(2050)
      DIMENSION IX(2050),IY(2050)
C
      IF(.NOT.LPAGE) RETURN
C
      NBMAX=(N-1)/1024+1
      DO NB=1,NBMAX
         N0=(NB-1)*1024
         N1=MIN(N-N0,1024)
C
      DO I=1,N1
         XA(I)=X(N0+I)
         YA(I)=Y(N0+I)
      ENDDO
      IF(N0+N1.EQ.N) THEN
         N1=N1+1
         XA(N1)=X(1)
         YA(N1)=Y(1)
      ENDIF
C
      CALL GUPCLP(XA,YA,N1,XB,YB,M)
C
      DO 2000 I=1,M
         IX(I)=NINT(XB(I)*XDEL+XORG)
         IY(I)=NINT(YB(I)*YDEL+YORG)
         IF(IX(I).LT.0)     IX(I)=0
         IF(IX(I).GT.32767) IX(I)=32767
         IF(IY(I).LT.0)     IY(I)=0
         IF(IY(I).GT.32767) IY(I)=32767
 2000 CONTINUE
C
      IF(M.GT.2) THEN
         CALL GUGRPS
         CALL  DVPOLY(IX,IY,M)
         IF(LFIL) CALL BUFFXN(12,IX,IY,M)
         CALL GUGRPE
      ENDIF
      ENDDO
      X1=X(N)
      Y1=Y(N)
      LMV=.FALSE.
      RETURN
      END
C
C     ****** CLIP POLYGON ******
C
      SUBROUTINE GUPCLP(XA,YA,N,XB,YB,M)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      DIMENSION XA(*),YA(*),XB(*),YB(*)
C
      LCLOSE = XA(1).EQ.XA(N).AND.YA(1).EQ.YA(N)
C
      DX=XR-XL
      DY=YR-YL
      DO 1000 I=1,N
         XB(I)=XA(I)
         YB(I)=YA(I)
         IF((XA(I)-XL)*DX.LT.0.0.OR.
     &      (XR-XA(I))*DX.LT.0.0.OR.
     &      (YA(I)-YL)*DX.LT.0.0.OR.
     &      (YR-YA(I))*DX.LT.0.0) GOTO 1010
 1000 CONTINUE
      M=N
      RETURN
C
 1010 CONTINUE
C
      M=N
C
C     X < XL ---> X = XL
C
      J=0
      DO 2000 I=1,M
         IF(I.EQ.1) THEN
            X1=XA(M)
            Y1=YA(M)
         ELSE
            X1=XB(J)
            Y1=YB(J)
         ENDIF
         X2=XA(I)
         Y2=YA(I)
         IF(I.NE.M) THEN
            X3=XA(I+1)
            Y3=YA(I+1)
         ELSE
            X3=XB(1)
            Y3=YB(1)
         ENDIF
         IF((X1-XL)*DX.LT.0.0) THEN
            IF((X2-XL)*DX.LT.0.0) THEN
               IF((X3-XL)*DX.LT.0.0) THEN
                  J=J+1
                  XB(J)=XL
                  YB(J)=Y2
               ELSE
                  J=J+1
                  XB(J)=XL
                  YB(J)=Y2+(Y3-Y2)*(XL-X2)/(X3-X2)
               ENDIF
            ELSE
               J=J+1
               XB(J)=XL
               YB(J)=Y1+(Y2-Y1)*(XL-X1)/(X2-X1)
               IF((X3-XL)*DX.LT.0.0) THEN
                  J=J+1
                  XB(J)=XL
                  YB(J)=Y2+(Y3-Y2)*(XL-X2)/(X3-X2)
               ELSE
                  J=J+1
                  XB(J)=X2
                  YB(J)=Y2
               ENDIF
            ENDIF
         ELSE
            IF((X2-XL)*DX.LT.0.0) THEN
               J=J+1
               XB(J)=XL
               YB(J)=Y1+(Y2-Y1)*(XL-X1)/(X2-X1)
               IF((X3-XL)*DX.LT.0.0) THEN
                  J=J+1
                  XB(J)=XL
                  YB(J)=Y2
               ELSE
                  J=J+1
                  XB(J)=XL
                  YB(J)=Y2+(Y3-Y2)*(XL-X2)/(X3-X2)
               ENDIF
            ELSE
               J=J+1
               XB(J)=X2
               YB(J)=Y2
            ENDIF
         ENDIF
 2000 CONTINUE
      M=J
      DO 2500 I=1,M
         XA(I)=XB(I)
         YA(I)=YB(I)
 2500 CONTINUE
C
C     XR < X  ---> X = XR
C
      J=0
      DO 3000 I=1,M
         IF(I.EQ.1) THEN
            X1=XA(M)
            Y1=YA(M)
         ELSE
            X1=XB(J)
            Y1=YB(J)
         ENDIF
         X2=XA(I)
         Y2=YA(I)
         IF(I.NE.M) THEN
            X3=XA(I+1)
            Y3=YA(I+1)
         ELSE
            X3=XB(1)
            Y3=YB(1)
         ENDIF
         IF((XR-X1)*DX.LT.0.0) THEN
            IF((XR-X2)*DX.LT.0.0) THEN
               IF((XR-X3)*DX.LT.0.0) THEN
                  J=J+1
                  XB(J)=XR
                  YB(J)=Y2
               ELSE
                  J=J+1
                  XB(J)=XR
                  YB(J)=Y2+(Y3-Y2)*(XR-X2)/(X3-X2)
               ENDIF
            ELSE
               J=J+1
               XB(J)=XR
               YB(J)=Y1+(Y2-Y1)*(XR-X1)/(X2-X1)
               IF((XR-X3)*DX.LT.0.0) THEN
                  J=J+1
                  XB(J)=XR
                  YB(J)=Y2+(Y3-Y2)*(XR-X2)/(X3-X2)
               ELSE
                  J=J+1
                  XB(J)=X2
                  YB(J)=Y2
               ENDIF
            ENDIF
         ELSE
            IF((XR-X2)*DX.LT.0.0) THEN
               J=J+1
               XB(J)=XR
               YB(J)=Y1+(Y2-Y1)*(XR-X1)/(X2-X1)
               IF((XR-X3)*DX.LT.0.0) THEN
                  J=J+1
                  XB(J)=XR
                  YB(J)=Y2
               ELSE
                  J=J+1
                  XB(J)=XR
                  YB(J)=Y2+(Y3-Y2)*(XR-X2)/(X3-X2)
               ENDIF
            ELSE
               J=J+1
               XB(J)=X2
               YB(J)=Y2
            ENDIF
         ENDIF
 3000 CONTINUE
      M=J
      DO 3500 I=1,M
         XA(I)=XB(I)
         YA(I)=YB(I)
 3500 CONTINUE
C
C     Y < YL ---> Y = YL
C
      J=0
      DO 4000 I=1,M
         IF(I.EQ.1) THEN
            Y1=YA(M)
            X1=XA(M)
         ELSE
            Y1=YB(J)
            X1=XB(J)
         ENDIF
         Y2=YA(I)
         X2=XA(I)
         IF(I.NE.M) THEN
            Y3=YA(I+1)
            X3=XA(I+1)
         ELSE
            Y3=YB(1)
            X3=XB(1)
         ENDIF
         IF((Y1-YL)*DY.LT.0.0) THEN
            IF((Y2-YL)*DY.LT.0.0) THEN
               IF((Y3-YL)*DY.LT.0.0) THEN
                  J=J+1
                  YB(J)=YL
                  XB(J)=X2
               ELSE
                  J=J+1
                  YB(J)=YL
                  XB(J)=X2+(X3-X2)*(YL-Y2)/(Y3-Y2)
               ENDIF
            ELSE
               J=J+1
               YB(J)=YL
               XB(J)=X1+(X2-X1)*(YL-Y1)/(Y2-Y1)
               IF((Y3-YL)*DY.LT.0.0) THEN
                  J=J+1
                  YB(J)=YL
                  XB(J)=X2+(X3-X2)*(YL-Y2)/(Y3-Y2)
               ELSE
                  J=J+1
                  YB(J)=Y2
                  XB(J)=X2
               ENDIF
            ENDIF
         ELSE
            IF((Y2-YL)*DY.LT.0.0) THEN
               J=J+1
               YB(J)=YL
               XB(J)=X1+(X2-X1)*(YL-Y1)/(Y2-Y1)
               IF((Y3-YL)*DY.LT.0.0) THEN
                  J=J+1
                  YB(J)=YL
                  XB(J)=X2
               ELSE
                  J=J+1
                  YB(J)=YL
                  XB(J)=X2+(X3-X2)*(YL-Y2)/(Y3-Y2)
               ENDIF
            ELSE
               J=J+1
               YB(J)=Y2
               XB(J)=X2
            ENDIF
         ENDIF
 4000 CONTINUE
      M=J
      DO 4500 I=1,M
         YA(I)=YB(I)
         XA(I)=XB(I)
 4500 CONTINUE
C
C     YR < Y  ---> Y = YR
C
      J=0
      DO 5000 I=1,M
         IF(I.EQ.1) THEN
            Y1=YA(M)
            X1=XA(M)
         ELSE
            Y1=YB(J)
            X1=XB(J)
         ENDIF
         Y2=YA(I)
         X2=XA(I)
         IF(I.NE.M) THEN
            Y3=YA(I+1)
            X3=XA(I+1)
         ELSE
            Y3=YB(1)
            X3=XB(1)
         ENDIF
         IF((YR-Y1)*DY.LT.0.0) THEN
            IF((YR-Y2)*DY.LT.0.0) THEN
               IF((YR-Y3)*DY.LT.0.0) THEN
                  J=J+1
                  YB(J)=YR
                  XB(J)=X2
               ELSE
                  J=J+1
                  YB(J)=YR
                  XB(J)=X2+(X3-X2)*(YR-Y2)/(Y3-Y2)
               ENDIF
            ELSE
               J=J+1
               YB(J)=YR
               XB(J)=X1+(X2-X1)*(YR-Y1)/(Y2-Y1)
               IF((YR-Y3)*DY.LT.0.0) THEN
                  J=J+1
                  YB(J)=YR
                  XB(J)=X2+(X3-X2)*(YR-Y2)/(Y3-Y2)
               ELSE
                  J=J+1
                  YB(J)=Y2
                  XB(J)=X2
               ENDIF
            ENDIF
         ELSE
            IF((YR-Y2)*DY.LT.0.0) THEN
               J=J+1
               YB(J)=YR
               XB(J)=X1+(X2-X1)*(YR-Y1)/(Y2-Y1)
               IF((YR-Y3)*DY.LT.0.0) THEN
                  J=J+1
                  YB(J)=YR
                  XB(J)=X2
               ELSE
                  J=J+1
                  YB(J)=YR
                  XB(J)=X2+(X3-X2)*(YR-Y2)/(Y3-Y2)
               ENDIF
            ELSE
               J=J+1
               YB(J)=Y2
               XB(J)=X2
            ENDIF
         ENDIF
 5000 CONTINUE
      M=J
      DO 5500 I=1,M
         YA(I)=YB(I)
         XA(I)=XB(I)
 5500 CONTINUE
      IF(M.EQ.0) GOTO 9000
C
C      DO I=1,M
C         WRITE(6,*) I,XA(I),YA(I)
C      END DO
C
      J=0
      L1=XA(1).EQ.XL
      L2=XA(2).EQ.XL
      IF(LCLOSE) THEN
         L3=XA(M).EQ.XL
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XB(J)=XA(1)
            YB(J)=YA(1)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XB(J)=XA(1)
            YB(J)=YA(1)
         ENDIF
      ENDIF
      DO 6000 I=3,M
         L3=XA(I).EQ.XL
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XB(J)=XA(I-1)
            YB(J)=YA(I-1)
         ENDIF
         L1=L2
         L2=L3
 6000 CONTINUE
      IF(LCLOSE) THEN
         L3=XB(1).EQ.XL
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XB(J)=XA(M)
            YB(J)=YA(M)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XB(J)=XA(M)
            YB(J)=YA(M)
         ENDIF
      ENDIF
      M=J
      IF(M.EQ.0) GOTO 9000
C
C      DO I=1,M
C         WRITE(6,*) I,XB(I),YB(I)
C      END DO
C
      J=0
      L1=XB(1).EQ.XR
      L2=XB(2).EQ.XR
      IF(LCLOSE) THEN
         L3=XB(M).EQ.XR
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XA(J)=XB(1)
            YA(J)=YB(1)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XA(J)=XB(1)
            YA(J)=YB(1)
         ENDIF
      ENDIF
      DO 6500 I=3,M
         L3=XB(I).EQ.XR
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XA(J)=XB(I-1)
            YA(J)=YB(I-1)
         ENDIF
         L1=L2
         L2=L3
 6500 CONTINUE
      IF(LCLOSE) THEN
         L3=XA(1).EQ.XR
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XA(J)=XB(M)
            YA(J)=YB(M)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XA(J)=XB(M)
            YA(J)=YB(M)
         ENDIF
      ENDIF
      M=J
      IF(M.EQ.0) GOTO 9000
C
C      DO I=1,M
C         WRITE(6,*) I,XA(I),YA(I)
C      END DO
C
      J=0
      L1=YA(1).EQ.YL
      L2=YA(2).EQ.YL
      IF(LCLOSE) THEN
         L3=YA(M).EQ.YL
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XB(J)=XA(1)
            YB(J)=YA(1)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XB(J)=XA(1)
            YB(J)=YA(1)
         ENDIF
      ENDIF
      DO 7000 I=3,M
         L3=YA(I).EQ.YL
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XB(J)=XA(I-1)
            YB(J)=YA(I-1)
         ENDIF
         L1=L2
         L2=L3
 7000 CONTINUE
      IF(LCLOSE) THEN
         L3=YB(1).EQ.YL
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XB(J)=XA(M)
            YB(J)=YA(M)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XB(J)=XA(M)
            YB(J)=YA(M)
         ENDIF
      ENDIF
      M=J
      IF(M.EQ.0) GOTO 9000
C
C      DO I=1,M
C         WRITE(6,*) I,XB(I),YB(I)
C      END DO
C
      J=0
      L1=YB(1).EQ.YR
      L2=YB(2).EQ.YR
      IF(LCLOSE) THEN
         L3=YB(M).EQ.YR
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XA(J)=XB(1)
            YA(J)=YB(1)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XA(J)=XB(1)
            YA(J)=YB(1)
         ENDIF
      ENDIF
      DO 7500 I=3,M
         L3=YB(I).EQ.YR
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XA(J)=XB(I-1)
            YA(J)=YB(I-1)
         ENDIF
         L1=L2
         L2=L3
 7500 CONTINUE
      IF(LCLOSE) THEN
         L3=YA(1).EQ.YR
         IF(.NOT.(L1.AND.L2.AND.L3)) THEN
            J=J+1
            XA(J)=XB(M)
            YA(J)=YB(M)
         ENDIF
      ELSE
         IF(.NOT.(L1.AND.L2)) THEN
            J=J+1
            XA(J)=XB(M)
            YA(J)=YB(M)
         ENDIF
      ENDIF
      M=J
      IF(M.EQ.0) GOTO 9000
C
C      DO I=1,M
C         WRITE(6,*) I,XA(I),YA(I)
C      END DO
C
      J=1
      XB(J)=XA(1)
      YB(J)=YA(1)
      DO 8000 I=2,M
         IF((XA(I-1).NE.XA(I)).OR.(YA(I-1).NE.YA(I))) THEN
            J=J+1
            XB(J)=XA(I)
            YB(J)=YA(I)
         ENDIF
 8000 CONTINUE
      M=J
      IF(M.EQ.0) GOTO 9000
C
      IF(LCLOSE.AND.M.GT.2) THEN
         IF((XB(1).NE.XB(M)).OR.(YB(1).NE.YB(M))) THEN
            M=M+1
            XB(M)=XB(1)
            YB(M)=YB(1)
         ENDIF
      ENDIF
C
C      DO I=1,M
C         WRITE(6,*) I,XB(I),YB(I)
C      END DO
 9000 RETURN
      END
C
C     ****** FILL TRIANGLE BY GRADATION ******
C
      SUBROUTINE RGBTRG(X,Y,R,G,B)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      DIMENSION X(*),Y(*),XA(4),YA(4),XB(4),YB(4),R(*),G(*),B(*)
      DIMENSION IX(9),IY(9),IR(3),IG(3),IB(3)
C
      IF(.NOT.LPAGE) RETURN
C
      N = 3
      DO I=1,N
         XA(I)=X(I)
         YA(I)=Y(I)
      ENDDO
      XA(N+1)=X(1)
      YA(N+1)=Y(1)
C
C      CALL GUPCLP(XA,YA,N+1,XB,YB,M)
C
      DO I=1,N+1
         XB(I)=XA(I)
         YB(I)=YA(I)
      ENDDO
C
C      IF(M.LT.4) GOTO 9000
C
      M = 3
      DO 2000 I=1,M
         IX(I)=NINT(XB(I)*XDEL+XORG)
         IY(I)=NINT(YB(I)*YDEL+YORG)
         IF(IX(I).LT.0)     IX(I)=0
         IF(IX(I).GT.32767) IX(I)=32767
         IF(IY(I).LT.0)     IY(I)=0
         IF(IY(I).GT.32767) IY(I)=32767
 2000 CONTINUE
      DO I=1,3
         IR(I)=NINT(R(I)*255)
         IG(I)=NINT(G(I)*255)
         IB(I)=NINT(B(I)*255)
      ENDDO
C
      CALL  DVRGBTRG(IX,IY,IR,IG,IB)
      IF(LFIL) THEN
         IX(4)=IX(2)
         IY(4)=IY(2)
         IX(7)=IX(3)
         IY(7)=IY(3)
         IX(2)=IR(1)
         IY(2)=IG(1)
         IX(3)=IB(1)
         IY(3)=0
         IX(5)=IR(2)
         IY(5)=IG(2)
         IX(6)=IB(2)
         IY(6)=0 
         IX(8)=IR(3)
         IY(8)=IG(3)
         IX(9)=IB(3)
         IY(9)=0
         CALL BUFFXN(13,IX,IY,M*3)
      ENDIF
      X1=X(N)
      Y1=Y(N)
      LMV=.FALSE.
      RETURN
      END
C
C     ****** SET LINE WIDTH ******
C
      SUBROUTINE SETLNW(W)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFS3/ ILNS,IBLS,ICLS
      COMMON /GSAFS6/ WS,RS,GS,BS
C
      IF(.NOT.LPAGE) RETURN
C
      IF(W.LT.0.0) THEN
         I=IBLS
         CALL SETLIN(-1,I,-1)
         RETURN
      ENDIF
C
      IW=NINT(MIN(W*XDEL,W*YDEL))
      CALL DVLWDT(IW)
      IF(LFIL) CALL BUFFST(6,IW,0,0,0.0,0.0)
      WS=W
      RETURN
      END
C
C     ****** INQUIRE LINE WIDTH ******
C
      SUBROUTINE INQLNW(W)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS6/ WS,RS,GS,BS
C
      IF(.NOT.LPAGE) RETURN
C
      W=WS
      RETURN
      END
C
C     ****** SET RGB COLOR ******
C
      SUBROUTINE SETRGB(R,G,B)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS3/ ILNS,IBLS,ICLS
      COMMON /GSAFS6/ WS,RS,GS,BS
      COMMON /GSAFS9/ ICRS,ICGS,ICBS
C
      IF(.NOT.LPAGE) RETURN
C
      IF(R.LT.0.0) THEN
         I=ICLS
         CALL SETLIN(-1,-1,I)
         RETURN
      ENDIF
C
      IR=NINT(R*255)
      IF(IR.LT.0) THEN
         IR=0
      ELSEIF(IR.GT.255) THEN
         IR=255
      ENDIF
C
      IG=NINT(G*255)
      IF(IG.LT.0) THEN
         IG=0
      ELSEIF(IG.GT.255) THEN
         IG=255
      ENDIF
C
      IB=NINT(B*255)
      IF(IB.LT.0) THEN
         IB=0
      ELSEIF(IB.GT.255) THEN
         IB=255
      ENDIF

      IF(IR.NE.ICRS.OR.IG.NE.ICGS.OR.IB.NE.ICBS) THEN
         CALL DVCRGB(IR,IG,IB)
         IF(LFIL) CALL BUFFST(5,IR,IG,IB,0.0,0.0)
      END IF

      RS=R
      GS=G
      BS=B
      ICRS=IR
      ICGS=IG
      ICBS=IB
      RETURN
      END
C
C     ****** INQUIRE RGB COLOR ******
C
      SUBROUTINE INQRGB(R,G,B)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS6/ WS,RS,GS,BS
C
      IF(.NOT.LPAGE) RETURN
C
      R=RS
      G=GS
      B=BS
      RETURN
      END
