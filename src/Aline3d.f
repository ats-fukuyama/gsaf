C     *************************************************
C     ********** GSAF 3D ROUTINES : draw line *********
C     *************************************************
C
C     言葉の説明：
C        3次元座標での値．．．ユーザが与えた座標での値
C        3次元での cm   ．．．gdefine3d で与えられる
C                       xl1,yl1,zl1 を元にした長さ(位置)
C        2次元での cm   ．．．実際に描画される長さ(位置)
C
C
      SUBROUTINE TRNP3D(XP,YP,ZP,PV)
C
      COMMON /COM3D1/ CA,SA,EL,ET
      COMMON /GVIEW1/ CA1,SA1,CC1,SC1,EL1,OX,OY,OZ
      COMMON /WFCTR/ XFCTR,YFCTR,ZFCTR,DOX,DOY
      DIMENSION PV(3)
C
      CALL GTTTB(XP,YP,ZP,PV(1),PV(2))
      PV(3)=(XP-OX)*XFCTR*SA+(YP-OY)*YFCTR*CA+EL
      RETURN
      END
C
      SUBROUTINE LINP3D(PS,PE)
C
      PARAMETER (IBMAX=100)
      REAL*8 DD
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA2/ WORK(1001,1001,8),XTMIN,XTMAX,YTMIN,YTMAX
      DIMENSION P1BUFF(IBMAX,3),P2BUFF(IBMAX,3),IBUFF(IBMAX)
      DIMENSION P1(3),P2(3),Q(3,3),P3(3),P4(3),PS(3),PE(3)
C
      DO I=1,3
         P1(I)=PS(I)
         P2(I)=PE(I)
      ENDDO
      PXMIN=MIN(P1(1),P2(1))
      PXMAX=MAX(P1(1),P2(1))
      PYMIN=MIN(P1(2),P2(2))
      PYMAX=MAX(P1(2),P2(2))
      PZMAX=MAX(P1(3),P2(3))
C
      IBEND=1
      P1BUFF(IBEND,1)=P1(1)
      P1BUFF(IBEND,2)=P1(2)
      P1BUFF(IBEND,3)=P1(3)
      P2BUFF(IBEND,1)=P2(1)
      P2BUFF(IBEND,2)=P2(2)
      P2BUFF(IBEND,3)=P2(3)
      IBUFF(IBEND)=1
C
      DO 1000 IY=1,NY-1
C
         IF(PXMIN.GE.WORK(NX,IY,5)) GOTO 1000
         IF(PXMAX.LE.WORK(NX,IY,4)) GOTO 1000
         IF(PYMIN.GE.WORK(NX,IY,7)) GOTO 1000
         IF(PYMAX.LE.WORK(NX,IY,6)) GOTO 1000
         IF(PZMAX.LE.WORK(NX,IY,8)) GOTO 1000
C
      DO 900 IX=1,NX-1
C
         IF(PXMIN.GE.WORK(IX,IY,5)) GOTO 900
         IF(PXMAX.LE.WORK(IX,IY,4)) GOTO 900
         IF(PYMIN.GE.WORK(IX,IY,7)) GOTO 900
         IF(PYMAX.LE.WORK(IX,IY,6)) GOTO 900
         IF(PZMAX.LE.WORK(IX,IY,8)) GOTO 900
C
C**** FIND VERTEX WITH ANGLE.GT.180, BY CHECKING SIDE OF OTHER VERTEX
C
         X1=WORK(IX,IY,1)
         Y1=WORK(IX,IY,2)
         Z1=WORK(IX,IY,3)
         X2=WORK(IX+1,IY,1)
         Y2=WORK(IX+1,IY,2)
         Z2=WORK(IX+1,IY,3)
         X3=WORK(IX+1,IY+1,1)
         Y3=WORK(IX+1,IY+1,2)
         Z3=WORK(IX+1,IY+1,3)
         X4=WORK(IX,IY+1,1)
         Y4=WORK(IX,IY+1,2)
         Z4=WORK(IX,IY+1,3)
C
         DD=((DBLE(Y2)-DBLE(Y1))*(DBLE(X3)-DBLE(X1))
     &      -(DBLE(X2)-DBLE(X1))*(DBLE(Y3)-DBLE(Y1)))
     &     *((DBLE(Y2)-DBLE(Y1))*(DBLE(X4)-DBLE(X1))
     &      -(DBLE(X2)-DBLE(X1))*(DBLE(Y4)-DBLE(Y1)))
     &     *((DBLE(Y3)-DBLE(Y2))*(DBLE(X4)-DBLE(X2))
     &      -(DBLE(X3)-DBLE(X2))*(DBLE(Y4)-DBLE(Y2)))
     &     *((DBLE(Y3)-DBLE(Y2))*(DBLE(X1)-DBLE(X2))
     &      -(DBLE(X3)-DBLE(X2))*(DBLE(Y1)-DBLE(Y2)))
         IF(DD.LT.0.0) THEN
            XM=0.5*(X1+X3)
            YM=0.5*(Y1+Y3)
            ZM=0.5*(Z1+Z3)
         ELSE
            XM=0.5*(X2+X4)
            YM=0.5*(Y2+Y4)
            ZM=0.5*(Z2+Z4)
         ENDIF
C
      DO 800 IQ=1,4
C
         IF(IQ.EQ.1) THEN
            IX1=IX
            IY1=IY
            IX2=IX+1
            IY2=IY
         ELSEIF(IQ.EQ.2) THEN
            IX1=IX+1
            IY1=IY
            IX2=IX+1
            IY2=IY+1
         ELSEIF(IQ.EQ.3) THEN
            IX1=IX+1
            IY1=IY+1
            IX2=IX
            IY2=IY+1
         ELSEIF(IQ.EQ.4) THEN
            IX1=IX
            IY1=IY+1
            IX2=IX
            IY2=IY
         ENDIF
C
         Q(1,1)=WORK(IX1,IY1,1)
         Q(1,2)=WORK(IX1,IY1,2)
         Q(1,3)=WORK(IX1,IY1,3)
         Q(2,1)=WORK(IX2,IY2,1)
         Q(2,2)=WORK(IX2,IY2,2)
         Q(2,3)=WORK(IX2,IY2,3)
         Q(3,1)=XM
         Q(3,2)=YM
         Q(3,3)=ZM
C
         IBENDN=IBEND
C
         DO 100 IB=1,IBEND
C
            IF(IBUFF(IB).EQ.0) GOTO 100
C
            P1(1)=P1BUFF(IB,1)
            P1(2)=P1BUFF(IB,2)
            P1(3)=P1BUFF(IB,3)
            P2(1)=P2BUFF(IB,1)
            P2(2)=P2BUFF(IB,2)
            P2(3)=P2BUFF(IB,3)
C
            XPMIN=MIN(P1(1),P2(1))
            XMAX=MAX(Q(1,1),Q(2,1),Q(3,1))
            IF(XPMIN.GE.XMAX) GOTO 100
C
            XPMAX=MAX(P1(1),P2(1))
            XMIN=MIN(Q(1,1),Q(2,1),Q(3,1))
            IF(XPMAX.LE.XMIN) GOTO 100
C
            YPMIN=MIN(P1(2),P2(2))
            YMAX=MAX(Q(1,2),Q(2,2),Q(3,2))
            IF(YPMIN.GE.YMAX) GOTO 100
C
            YPMAX=MAX(P1(2),P2(2))
            YMIN=MIN(Q(1,2),Q(2,2),Q(3,2))
            IF(YPMAX.LE.YMIN) GOTO 100
C
            ZPMAX=MAX(P1(3),P2(3))
            WMIN=MIN(Q(1,3),Q(2,3),Q(3,3))
            IF(ZPMAX.LE.WMIN) GOTO 100
C
            CALL LINQ3D(P1,P2,Q,IC,P3,P4)
C
            IF(IC.EQ.0) THEN
               IBUFF(IB)=0
C
            ELSEIF(IC.EQ.2) THEN
               IF(IBENDN.LT.IBMAX) THEN
                  IBENDN=IBENDN+1
                  IBN=IBENDN
               ELSE
                  IBN=0
                  DO 10 I=1,IBMAX
                     IF(IBUFF(I).EQ.0) IBN=I
   10             CONTINUE
               ENDIF
               IF(IBN.NE.0) THEN
                  P1BUFF(IBN,1)=P3(1)
                  P1BUFF(IBN,2)=P3(2)
                  P1BUFF(IBN,3)=P3(3)
                  P2BUFF(IBN,1)=P4(1)
                  P2BUFF(IBN,2)=P4(2)
                  P2BUFF(IBN,3)=P4(3)
                  IBUFF(IBN)=1
               ENDIF
C
            ELSEIF(IC.NE.1) THEN
               WRITE(6,*) 'ERROR IN LINP3D : INCORRECT IC, IC = ',IC
            ENDIF
C
            P1BUFF(IB,1)=P1(1)
            P1BUFF(IB,2)=P1(2)
            P1BUFF(IB,3)=P1(3)
            P2BUFF(IB,1)=P2(1)
            P2BUFF(IB,2)=P2(2)
            P2BUFF(IB,3)=P2(3)
C
      IF(IBUFF(IB).EQ.1) THEN
         DP=(P1(1)-P2(1))**2+(P1(2)-P2(2))**2
         IF(DP.LT.1.E-6) IBUFF(IB)=0
      ENDIF
C
  100    CONTINUE
C
         IBEND=IBENDN
C
  800 CONTINUE
  900 CONTINUE
 1000 CONTINUE
C
      DO 2000 IB=1,IBEND
         IF(IBUFF(IB).EQ.0) GOTO 2000
C         X1=DX*(P1BUFF(IB,1)-GXORG)+PXORG
C         Y1=DY*(P1BUFF(IB,2)-GYORG)+PYORG
C         X2=DX*(P2BUFF(IB,1)-GXORG)+PXORG
C         Y2=DY*(P2BUFF(IB,2)-GYORG)+PYORG
         X1=P1BUFF(IB,1)
         Y1=P1BUFF(IB,2)
         X2=P2BUFF(IB,1)
         Y2=P2BUFF(IB,2)
         IF((X2-X1)**2+(Y2-Y1)**2.LT.1.E-6) GOTO 2000
         CALL MOVE(X1,Y1)
         CALL DRAW(X2,Y2)
 2000 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE LINQ3D(P1,P2,Q,IC,P3,P4)
C
C     P1,P2 : LINE POINTS
C     Q     : TRIANGLE POINTS
C     IC    : 0 : LINE IS COMPLETELY MASKED
C             1 : NEW LINE P1-P2
C             2 : NEW LINE P1-P2,P3-P4
C
      DIMENSION P1(3),P2(3),P3(3),P4(3),Q(3,3),T(3),Z(3)
      REAL*8 XYP,XY,D,DD
      DATA EPS/1.E-3/,EPSD/1.E-6/
C
      XP1=P1(1)
      YP1=P1(2)
      ZP1=P1(3)
      XP2=P2(1)
      YP2=P2(2)
      ZP2=P2(3)
C
      XP21=XP2-XP1
      YP21=YP2-YP1
      ZP21=ZP2-ZP1
      XYP =DBLE(XP1)*DBLE(YP2)-DBLE(XP2)*DBLE(YP1)
C
      IF(XP21*XP21+YP21*YP21.LT.EPSD) THEN
         IC=0
         RETURN
      ENDIF
C
      X1=Q(1,1)
      Y1=Q(1,2)
      Z1=Q(1,3)
      X2=Q(2,1)
      Y2=Q(2,2)
      Z2=Q(2,3)
      X3=Q(3,1)
      Y3=Q(3,2)
      Z3=Q(3,3)
C
      X21=X2-X1
      Y21=Y2-Y1
      X32=X3-X2
      Y32=Y3-Y2
      X13=X1-X3
      Y13=Y1-Y3
C
C     BETWEEN P1 AND P2
C
      D=(DBLE(YP2)-DBLE(YP1))*(DBLE(X2)-DBLE(X1))
     & -(DBLE(XP2)-DBLE(XP1))*(DBLE(Y2)-DBLE(Y1))
      IF(ABS(D).LT.EPSD) THEN
         G21=1.E7
      ELSE
         DD=XYP-(DBLE(YP2)-DBLE(YP1))*DBLE(X1)
     &         +(DBLE(XP2)-DBLE(XP1))*DBLE(Y1)
         G21=REAL(DD/D)
         IF(ABS(G21).LT.EPS) G21=0.0
         IF(ABS(G21-1.0).LT.EPS) G21=1.0
      ENDIF
C
C     BETWEEN P2 AND P3
C
      D=(DBLE(YP2)-DBLE(YP1))*(DBLE(X3)-DBLE(X2))
     & -(DBLE(XP2)-DBLE(XP1))*(DBLE(Y3)-DBLE(Y2))
      IF(ABS(D).LT.EPSD) THEN
         G32=1.E7
      ELSE
         DD=XYP-(DBLE(YP2)-DBLE(YP1))*DBLE(X2)
     &         +(DBLE(XP2)-DBLE(XP1))*DBLE(Y2)
         G32=REAL(DD/D)
         IF(ABS(G32).LT.EPS) G32=0.0
         IF(ABS(G32-1.0).LT.EPS) G32=1.0
      ENDIF
C
C     BETWEEN P3 AND P1
C
      D=(DBLE(YP2)-DBLE(YP1))*(DBLE(X1)-DBLE(X3))
     & -(DBLE(XP2)-DBLE(XP1))*(DBLE(Y1)-DBLE(Y3))
      IF(ABS(D).LT.EPSD) THEN
         G13=1.E7
      ELSE
         DD=XYP-(DBLE(YP2)-DBLE(YP1))*DBLE(X3)
     &         +(DBLE(XP2)-DBLE(XP1))*DBLE(Y3)
         G13=REAL(DD/D)
         IF(ABS(G13).LT.EPS) G13=0.0
         IF(ABS(G13-1.0).LT.EPS) G13=1.0
      ENDIF
C
      IC=0
C
      IF(G21.GE.0.0.AND.G21.LE.1.0) THEN
         IF(ABS(XP21).GT.EPS) THEN
            XC=X21*G21+X1
            TC1=(XC-XP1)/XP21
         ELSE
            YC=Y21*G21+Y1
            TC1=(YC-YP1)/YP21
         ENDIF
         IF(ABS(TC1).LT.EPS) TC1=0.0
         IF(ABS(TC1-1.0).LT.EPS) TC1=1.0
         IF(TC1.GE.0.0.AND.TC1.LE.1.0) THEN
            IC=IC+1
            T(IC)=TC1
            Z(IC)=(Z2-Z1)*G21+Z1
         ENDIF
      ENDIF
C
      IF(G32.GE.0.0.AND.G32.LE.1.0) THEN
         IF(ABS(XP21).GT.EPS) THEN
            XC=X32*G32+X2
            TC2=(XC-XP1)/XP21
         ELSE
            YC=Y32*G32+Y2
            TC2=(YC-YP1)/YP21
         ENDIF
         IF(ABS(TC2).LT.EPS) TC2=0.0
         IF(ABS(TC2-1.0).LT.EPS) TC2=1.0
         IF(TC2.GE.0.0.AND.TC2.LE.1.0) THEN
            IC=IC+1
            T(IC)=TC2
            Z(IC)=(Z3-Z2)*G32+Z2
         ENDIF
      ENDIF
C
      IF(G13.GE.0.0.AND.G13.LE.1.0) THEN
         IF(ABS(XP21).GT.EPS) THEN
            XC=X13*G13+X3
            TC3=(XC-XP1)/XP21
         ELSE
            YC=Y13*G13+Y3
            TC3=(YC-YP1)/YP21
         ENDIF
         IF(ABS(TC3).LT.EPS) TC3=0.0
         IF(ABS(TC3-1.0).LT.EPS) TC3=1.0
         IF(TC3.GE.0.0.AND.TC3.LE.1.0) THEN
            IC=IC+1
            T(IC)=TC3
            Z(IC)=(Z1-Z3)*G13+Z3
         ENDIF
      ENDIF
C
      IF(IC.EQ.3) THEN
         IF(ABS(T(1)-T(2)).LT.EPSD) THEN
            TT=T(2)
            T(2)=T(3)
            T(3)=TT
            ZT=Z(2)
            Z(2)=Z(3)
            Z(3)=ZT
         ENDIF
         IC=2
      ENDIF
C--      IF(IC.EQ.2) THEN
C--        IF(ABS(T(1)-T(2)).LT.EPSD) THEN
C--           IC=1
C--         ENDIF
C--      ENDIF
C
C *** NO INTERSECTION POINT : CHECK LINE P IS IN Q OR NOT.
C                             CALCULATE DEPTH OF P1 AND P2
C
      IF(IC.EQ.0) THEN
         IC=1
         T1=0.0
         T2=1.0
C
C *** CHECK WHETHER HALFWAY POINT OF LINE P1-P2 IN Q
C
         XPM=0.5*(XP1+XP2)
         YPM=0.5*(YP1+YP2)
         C1=(Y21*XPM-X21*YPM-X1*Y2+X2*Y1)
     &     *(Y21*X3 -X21*Y3 -X1*Y2+X2*Y1)
         IF(C1.GE.0.0) THEN
            C2=(Y32*XPM-X32*YPM-X2*Y3+X3*Y2)
     &        *(Y32*X1 -X32*Y1 -X2*Y3+X3*Y2)
            IF(C2.GE.0.0) THEN
               C3=(Y13*XPM-X13*YPM-X3*Y1+X1*Y3)
     &           *(Y13*X2 -X13*Y2 -X3*Y1+X1*Y3)
               IF(C3.GE.0.0) THEN
C
C *** IF LINE P1-P2 IS IN Q, CALCULATE ZQ(P1) AND ZQ(P2)
C
                  XY=(DBLE(X2)-DBLE(X1))*(DBLE(Y1)-DBLE(Y3))
     &              -(DBLE(X1)-DBLE(X3))*(DBLE(Y2)-DBLE(Y1))
                  GZI=(Y13*(XP1-X1)-X13*(YP1-Y1))/REAL(XY)
                  ETA=(Y21*(XP1-X1)-X21*(YP1-Y1))/REAL(XY)
                  ZQ1=(Z2-Z1)*GZI+(Z3-Z1)*ETA+Z1
                  GZI=(Y13*(XP2-X1)-X13*(YP2-Y1))/REAL(XY)
                  ETA=(Y21*(XP2-X1)-X21*(YP2-Y1))/REAL(XY)
                  ZQ2=(Z2-Z1)*GZI+(Z3-Z1)*ETA+Z1
                  DZ1=ZP1-ZQ1
                  DZ2=ZP2-ZQ2
                  IF(DZ1.GT.EPS) THEN
                     IF(DZ2.GT.EPS) THEN
                        IC=0
                     ELSE
                        T1=DZ1/(DZ1-DZ2)
                        T2=1.0
                     ENDIF
                  ELSE
                     IF(DZ2.GT.EPS) THEN
                        T1=0.0
                        T2=DZ1/(DZ1-DZ2)
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
C
C *** ONE INTERSECTION : CHECK WHICH SIDE OF LINE P1-P2 IS IN Q
C                        CALCULATE DEPTH OF INTERSECTION POINT
C
      ELSEIF(IC.EQ.1) THEN
         IF(T(1).GT.0.5) THEN
            C1= (Y21*XP1-X21*YP1-X1*Y2+X2*Y1)
     &         *(Y21*X3 -X21*Y3 -X1*Y2+X2*Y1)
            C2= (Y32*XP1-X32*YP1-X2*Y3+X3*Y2)
     &         *(Y32*X1 -X32*Y1 -X2*Y3+X3*Y2)
            C3= (Y13*XP1-X13*YP1-X3*Y1+X1*Y3)
     &         *(Y13*X2 -X13*Y2 -X3*Y1+X1*Y3)
            IF(C1.GE.0.0.AND.C2.GE.0.0.AND.C3.GE.0.0) THEN
               C= 1.0
            ELSE
               C=-1.0
            ENDIF
         ELSE
            C1= (Y21*XP2-X21*YP2-X1*Y2+X2*Y1)
     &         *(Y21*X3 -X21*Y3 -X1*Y2+X2*Y1)
            C2= (Y32*XP2-X32*YP2-X2*Y3+X3*Y2)
     &         *(Y32*X1 -X32*Y1 -X2*Y3+X3*Y2)
            C3= (Y13*XP2-X13*YP2-X3*Y1+X1*Y3)
     &         *(Y13*X2 -X13*Y2 -X3*Y1+X1*Y3)
            IF(C1.GE.0.0.AND.C2.GE.0.0.AND.C3.GE.0.0) THEN
               C=-1.0
            ELSE
               C= 1.0
            ENDIF
         ENDIF
C
C *** C > 0 IMPLIES P1 IS IN Q
C
         IF(C.GT.0.0) THEN
            XY=(DBLE(X2)-DBLE(X1))*(DBLE(Y1)-DBLE(Y3))
     &        -(DBLE(X1)-DBLE(X3))*(DBLE(Y2)-DBLE(Y1))
            GZI=(Y13*(XP1-X1)-X13*(YP1-Y1))/REAL(XY)
            ETA=(Y21*(XP1-X1)-X21*(YP1-Y1))/REAL(XY)
            ZQ1=(Z2-Z1)*GZI+(Z3-Z1)*ETA+Z1
            DZ1=ZP1-ZQ1
            ZPC=ZP21*T(1)+ZP1
            DZC=ZPC-Z(1)
            IF(DZ1.GT.EPS) THEN
               IF(DZC.GT.EPS) THEN
                  T1=T(1)
                  T2=1.0
               ELSE
                  T1=T(1)*DZ1/(DZ1-DZC)
                  T2=1.0
               ENDIF
            ELSE
               IF(DZC.GT.EPS) THEN
                  IC=2
                  T1=T(1)*DZ1/(DZ1-DZC)
                  T2=T(1)
               ELSE
                  T1=0.0
                  T2=1.0
               ENDIF
            ENDIF
         ELSE
            XY=(DBLE(X2)-DBLE(X1))*(DBLE(Y1)-DBLE(Y3))
     &        -(DBLE(X1)-DBLE(X3))*(DBLE(Y2)-DBLE(Y1))
            GZI=(Y13*(XP2-X1)-X13*(YP2-Y1))/REAL(XY)
            ETA=(Y21*(XP2-X1)-X21*(YP2-Y1))/REAL(XY)
            ZQ2=(Z2-Z1)*GZI+(Z3-Z1)*ETA+Z1
            DZ2=ZP2-ZQ2
            ZPC=ZP21*T(1)+ZP1
            DZC=ZPC-Z(1)
            IF(DZ2.GT.EPS) THEN
               IF(DZC.GT.EPS) THEN
                  T1=0.0
                  T2=T(1)
               ELSE
                  T1=0.0
                  T2=T(1)+(1.0-T(1))*DZC/(DZC-DZ2)
               ENDIF
            ELSE
               IF(DZC.GT.EPS) THEN
                  IC=2
                  T1=T(1)
                  T2=T(1)+(1.0-T(1))*DZC/(DZC-DZ2)
               ELSE
                  T1=0.0
                  T2=1.0
               ENDIF
            ENDIF
         ENDIF
C
C *** TWO INTERSECTION : CALCULATE DEPTH OF INTERSECTION POINTS
C
      ELSEIF(IC.EQ.2) THEN
         IF(T(1).GT.T(2)) THEN
            TT=T(1)
            T(1)=T(2)
            T(2)=TT
            ZT=Z(1)
            Z(1)=Z(2)
            Z(2)=ZT
         ENDIF
         ZPC1=ZP21*T(1)+ZP1
         ZPC2=ZP21*T(2)+ZP1
         DZC1=ZPC1-Z(1)
         DZC2=ZPC2-Z(2)
         IF(DZC1.GT.EPS) THEN
            IF(DZC2.GT.EPS) THEN
               T1=T(1)
               T2=T(2)
            ELSE
               T1=T(1)
               T2=(T(2)-T(1))*DZC1/(DZC1-DZC2)+T(1)
            ENDIF
         ELSE
            IF(DZC2.GT.EPS) THEN
               T1=(T(2)-T(1))*DZC1/(DZC1-DZC2)+T(1)
               T2=T(2)
            ELSE
               IC=1
               T1=0.0
               T2=1.0
            ENDIF
         ENDIF
      ENDIF
C
      IF(IC.EQ.1) THEN
         P1(1)=XP21*T1+XP1
         P1(2)=YP21*T1+YP1
         P1(3)=ZP21*T1+ZP1
         P2(1)=XP21*T2+XP1
         P2(2)=YP21*T2+YP1
         P2(3)=ZP21*T2+ZP1
      ELSEIF(IC.EQ.2) THEN
         P2(1)=XP21*T1+XP1
         P2(2)=YP21*T1+YP1
         P2(3)=ZP21*T1+ZP1
         P3(1)=XP21*T2+XP1
         P3(2)=YP21*T2+YP1
         P3(3)=ZP21*T2+ZP1
         P4(1)=XP2
         P4(2)=YP2
         P4(3)=ZP2
      ENDIF
      RETURN
      END
C
      SUBROUTINE LINE3D(X1,Y1,Z1,X2,Y2,Z2)
C
      DIMENSION P1(3),P2(3)
C
      CALL TRNP3D(X1,Y1,Z1,P1)
      CALL TRNP3D(X2,Y2,Z2,P2)
      CALL LINP3D(P1,P2)
      RETURN
      END
C
C     ****** DRAW HIDEN-LINE ******
C
      SUBROUTINE HLINPT3D(X1,Y1,Z1,X2,Y2,Z2,IPAD)
C
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /CPLTHD/ NF,IXMINB,IYMINB,HXY(1000,1000),DPMX(2560,1810),
     &     NNMX(2560,2),AFACTER
      DATA EPS/0.10/
C     
      CALL GTTTB(X1,Y1,Z1,XP1,YP1)
      CALL GTTTB(X2,Y2,Z2,XP2,YP2)
      IF (ZL.EQ.0.0) GOTO 999
      DL=(XP1-XP2)**2+(YP1-YP2)**2
      IF (DL.LT.1.E-5) RETURN
C
      EX=EL*SC*CA/XFCTR+OX
      IF (XFCTR.LT.1.E-4) EX = OX
      EY=EL*SC*SA/YFCTR+OY
      IF (XFCTR.LT.1.E-4) EY = OY
      EZ=EL*CC/ZFCTR+OZ
      IF (XFCTR.LT.1.E-4) EZ = OZ
C
      IF (EZ.LE.ZMIN) THEN
         IF (Z1.LE.ZMIN.AND.Z2.LE.ZMIN) GOTO 999
      ELSE IF (EZ.GE.ZMAX) THEN
         IF (Z1.GE.ZMAX.AND.Z2.GE.ZMAX) GOTO 999
      ENDIF
      IF (EX.LE.GXS ) THEN
         IF (X1.LE.GXS .AND.X2.LE.GXS ) GOTO 999
      ELSE IF (EX.GE.GXE ) THEN
         IF (X1.GE.GXE .AND.X2.GE.GXE ) GOTO 999
      ENDIF
      IF (EY.LE.GYS ) THEN
         IF (Y1.LE.GYS .AND.Y2.LE.GYS ) GOTO 999
      ELSE IF (EY.GE.GYE ) THEN
         IF (Y1.GE.GYE .AND.Y2.GE.GYE ) GOTO 999
      ENDIF
C
      DL=EPS/SQRT(DL)
      DX=X2-X1
      DY=Y2-Y1
      DZ=Z2-Z1
      NE=INT(0.9999/DL)+2
      IMOD=-1
      INUM=0
      XB=X1
      YB=Y1
      ZB=Z1
      DO I=1,NE
         IF (I.NE.NE) THEN
            X=X1+DX*(I-1)*DL
            Y=Y1+DY*(I-1)*DL
            Z=Z1+DZ*(I-1)*DL
         ELSE
            X=X2
            Y=Y2
            Z=Z2
         ENDIF
C
         CALL GTTTB(X,Y,Z,XNM,YNM)
         CALL GTTTZ(X,Y,Z,ZNM)
         IXB=NINT(AFACTER*XNM)
         IYB=NINT(AFACTER*YNM)
         IF (NX*NY.GT.2000) THEN
            IF (ZNM.GT.DPMX(IXB-IXMINB,IYB-IYMINB)) GOTO 300
         ELSE
            IF (ZNM.GE.DPMX(IXB-IXMINB,IYB-IYMINB)+10.0) GOTO 300
         ENDIF
         IF (IMOD.EQ.1) THEN
            CALL GTMIDPNT(XB,YB,ZB,X,Y,Z,XN,YN,ZN)
            CALL GTTTB(XN,YN,ZN,XT2,YT2)
            CALL MOVEPT(XT1,YT1,IPAD)
            CALL DRAWPT(XT2,YT2)
            INUM=0
            IMOD=-1
         ENDIF
         GOTO 1000
 300     IF (INUM.EQ.0) THEN
            IF (I.EQ.1) THEN
               XT1=XNM
               YT1=YNM
            ELSE
               CALL GTMIDPNT(X,Y,Z,XB,YB,ZB,XN,YN,ZN)
               CALL GTTTB(XN,YN,ZN,XT1,YT1)
            ENDIF
         ENDIF
         IF (I.EQ.NE) THEN
            CALL MOVEPT(XT1,YT1,IPAD)
            CALL DRAWPT(XNM,YNM)
         ENDIF
         IMOD=1
         INUM=INUM+1
 1000    XB=X
         YB=Y
         ZB=Z
      ENDDO
      RETURN
 999  CALL MOVEPT(XP1,YP1,IPAD)
      CALL DRAWPT(XP2,YP2)
      RETURN
      END
C
C     ****** GET MIDDLE POINT ******
C
      SUBROUTINE GTMIDPNT(XS,YS,ZS,XE,YE,ZE,XN,YN,ZN)
C
      X1=XS
      Y1=YS
      Z1=ZS
      X2=XE
      Y2=YE
      Z2=ZE
      INUM=1
 1    XN=0.5*(X1+X2)
      YN=0.5*(Y1+Y2)
      ZN=0.5*(Z1+Z2)
      IF (INUM.GE.4) GOTO 9999
      CALL GTHDNPNT(XN,YN,ZN,IMOD)
      IF (IMOD.EQ.-1) THEN
         X2=XN
         Y2=YN
         Z2=ZN
      ELSE
         X1=XN
         Y1=YN
         Z1=ZN
      ENDIF
      INUM=INUM+1
      GOTO 1
C
 9999 RETURN
      END
C
C     ****** JUDGE WHETHER THE POINT IS HIDEN OR NOT.
C
      SUBROUTINE GTHDNPNT(X,Y,Z,IMOD)
C
C     if hiden imod=-1, if not, imod=1
C
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /GVIEW1/ CA,SA,CC,SC,EL,OX,OY,OZ
      COMMON /WFCTR/  XFCTR,YFCTR,ZFCTR,DOX,DOY
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
      COMMON /GTHNDT/ XHDATA(1001,2),YHDATA(1001,2)
      COMMON /COM3D1/ CA1,SA1,EL1,ET1
      COMMON /GTTTX/  XTMIN1,YTMIN1,DELX1,DELY1,RXY1
      DATA EPSZ/1.E-3/
C     
      EX=EL*SC*CA/XFCTR+OX
      EY=EL*SC*SA/YFCTR+OY
      EZ=EL*CC/ZFCTR+OZ
      IF (EZ.GE.Z) THEN
         SX=(ZMAX-Z)*(X-EX)/(Z-EZ)+X
         SY=(ZMAX-Z)*(Y-EY)/(Z-EZ)+Y
      ELSE
         SX=(ZMIN-Z)*(X-EX)/(Z-EZ)+X
         SY=(ZMIN-Z)*(Y-EY)/(Z-EZ)+Y
      ENDIF
      IF (EX.GE.X) THEN
         XVMIN=X
         XVMAX=SX
      ELSE
         XVMIN=SX
         XVMAX=X
      ENDIF
      IF (EY.GE.Y) THEN
         YVMIN=Y
         YVMAX=SY
      ELSE
         YVMIN=SY
         YVMAX=Y
      ENDIF
C     depend on data's being equal interval. 
      IYMIN=INT((YVMIN-GYS)/(GYE-GYS)*(NY-1))+1
      IF (IYMIN.LE.1) IYMIN=1
      IYMAX=INT((YVMAX-GYS)/(GYE-GYS)*(NY-1))+2
      IF (IYMAX.GE.NY) IYMAX=NY
      IXMIN=INT((XVMIN-GXS)/(GXE-GXS)*(NX-1))+1
      IF (IXMIN.LE.1) IXMIN=1
      IXMAX=INT((XVMAX-GXS)/(GXE-GXS)*(NX-1))+2
      IF (IXMAX.GE.NX) IXMAX=NX
      IMOD=1
      ZF=0.0
C
      CALL GTTTB(X,Y,Z,XNM,YNM)
      DELX=EX-X
      DELY=EY-Y
      IF (ABS(DELX).LT.1.E-6) DELX=1.E-6
      IF (ABS(DELY).LT.1.E-6) DELY=1.E-6
      TY=DELY/DELX
      TX=DELX/DELY
      IN=0
      PN=((X-EX)*XFCTR)**2+((Y-EY)*YFCTR)**2+((Z-EZ)*ZFCTR)**2
      DO 1010 IX=IXMIN,IXMAX
         IF (IN.EQ.1) THEN
            IF (ZF.LT.0.0) THEN
               IF (YNM.GT.XHDATA(IX,2)) GOTO 1010
            ELSE
               IF (YNM.LT.XHDATA(IX,1)) GOTO 1010
            ENDIF
         ENDIF
         Q=Y+(XDATA(IX)-X)*TY
         IF (Q.GT.GYE) GOTO 1010
         IF (Q.LT.GYS) GOTO 1010
         CALL GTINQZVLX(IX,Q,ZVAL)
         PV=((XDATA(IX)-EX)*XFCTR)**2+((Q-EY)*YFCTR)**2+
     &           ((ZVAL-EZ)*ZFCTR)**2
         IF (PV.GT.PN) GOTO 1010
         CALL GTTTB(XDATA(IX),Q,ZVAL,XTM,YTM)
         IF (ABS(Z-ZVAL).LT.1.E-3*(ZMAX-ZMIN)) THEN
            IF (ABS(X-XDATA(IX)).LT.EPSZ*(GXE-GXS)) THEN
               IF (ABS(YNM-YTM).LT.0.01) THEN
                  GOTO 1010
               ENDIF
            ENDIF
         ENDIF
         IF (IN.EQ.0) THEN
            IF (YNM.LE.YTM) THEN
               ZF=1.0
            ELSE
               ZF=-1.0
            ENDIF
            IN=1
            GOTO 1010
         ENDIF
         IF (ZF*(YNM-YTM).GT.0.0) THEN
            IMOD=-1
            RETURN
         ENDIF
 1010 CONTINUE
      DO 1020 IY=IYMIN,IYMAX
         IF (IN.EQ.1) THEN
            IF (ZF.LT.0.0) THEN
               IF (YNM.GT.YHDATA(IY,2)) GOTO 1020
            ELSE
               IF (YNM.LT.YHDATA(IY,1)) GOTO 1020
            ENDIF
         ENDIF
         Q=X+(YDATA(IY)-Y)*TX
         IF (Q.GT.GXE) GOTO 1020
         IF (Q.LT.GXS) GOTO 1020
         CALL GTINQZVLY(Q,IY,ZVAL)
         PV=((Q-EX)*XFCTR)**2+((YDATA(IY)-EY)*YFCTR)**2+
     &        ((ZVAL-EZ)*ZFCTR)**2
         IF (PV.GT.PN) GOTO 1020
         CALL GTTTB(Q,YDATA(IY),ZVAL,XTM,YTM)
         IF (ABS(Z-ZVAL).LT.1.E-3*(ZMAX-ZMIN)) THEN
            IF (ABS(Y-YDATA(IY)).LT.EPSZ*(GYE-GYS)) THEN
               IF (ABS(YNM-YTM).LT.0.01) THEN
                  GOTO 1020
               ENDIF
            ENDIF
         ENDIF
         IF (IN.EQ.0) THEN
            IF (YNM.LE.YTM) THEN
               ZF=1.0
            ELSE
               ZF=-1.0
            ENDIF
            IN=1
            GOTO 1020
         ENDIF
         IF (ZF*(YNM-YTM).GT.0.0) THEN
            IMOD=-1
            RETURN
         ENDIF
 1020 CONTINUE
C
      RETURN
      END
C
C     ****** GET Z-VALUE FROM X AND Y ******
C
      SUBROUTINE GTINQZVLX(IX,Y,Z)
C
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
C     depend on data's being equal intervals. 
      IY=INT((Y-GYS)/(GYE-GYS)*REAL(NY-1))+1
      IYMIN=IY
      IYMAX=IY+1
      Y2=Y-YDATA(IYMIN)
      Y3=YDATA(IYMAX)-YDATA(IYMIN)
      RATIO=Y2/Y3
C
      Z =(1.0-RATIO)*ZDATA(IX,IYMIN)+RATIO*ZDATA(IX,IYMAX)
C
      RETURN
      END
C
C     ****** GET Z-VALUE FROM X AND Y ******
C
      SUBROUTINE GTINQZVLY(X,IY,Z)
C
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
C     depend on data's being equal intervals. 
      IX=INT((X-GXS)/(GXE-GXS)*REAL(NX-1))+1
      IXMIN=IX
      IXMAX=IX+1
      X2=X-XDATA(IXMIN)
      X3=XDATA(IXMAX)-XDATA(IXMIN)
      RATIO=X2/X3
C
      Z =(1.0-RATIO)*ZDATA(IXMIN,IY)+RATIO*ZDATA(IXMAX,IY)
C
      RETURN
      END
C
C     ****** GET Z-VALUE FROM X AND Y ******
C
      SUBROUTINE GTINQZVL(X,Y,Z)
C
      COMMON /TDATA1/ XDATA(1001),YDATA(1001),ZDATA(1001,1001)
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C     depend on data's being equal intervals. 
      IY=INT((Y-GYS)/(GYE-GYS)*REAL(NY-1))+1
      IYMIN=IY
      IYMAX=IY+1
      Y2=Y-YDATA(IYMIN)
      Y3=YDATA(IYMAX)-YDATA(IYMIN)
      RATIOY=Y2/Y3
C     depend on being equal intervals. 
      IX=INT((X-GXS)/(GXE-GXS)*REAL(NX-1))+1
      IXMIN=IX
      IXMAX=IX+1
      X2=X-XDATA(IXMIN)
      X3=XDATA(IXMAX)-XDATA(IXMIN)
      RATIOX=X2/X3
C
      Z1=(1.0-RATIOX)*ZDATA(IXMIN,IYMIN)+RATIOX*ZDATA(IXMAX,IYMIN)
      Z2=(1.0-RATIOX)*ZDATA(IXMIN,IYMAX)+RATIOX*ZDATA(IXMAX,IYMAX)
      Z =(1.0-RATIOY)*Z1+RATIOY*Z2
C
      RETURN
      END
