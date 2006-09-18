C     $Id$
C     ********************************************
C     ****** GSAF APPLICATION V3.6 : PERSE3D ******
C     ********************************************
C
      SUBROUTINE PERSE3D(IXY,IND)
C     ****** 3D PERSPECTIVE DRAWING ******
C        IXY  : 0 : CONTOUR ONLY
C               1 : X MESH ONLY
C               2 : Y MESH ONLY
C               3 : X AND Y MESHES
C        IND  : IXY =0 : NUMBER OF CONTOURS     (ABS(IND).LE.1001)
C               IXY<>0 : 0 : TRANSPARENT DRAWING
C                        1 : Z>ZMAX OR Z<ZMIN : NO DRAWING
C                        2 :                  : FRAME ONLY 
C                        3 :                  : FLAT MESH
C      LOGICAL LGF
C
      INCLUDE 'A3dcomm.inc'
      COMMON /COMDT1/ HX,HY,NX,NY,MX,MY,XW,YW
      COMMON /COMDT5/ ALPHA,ELD,ELL
      COMMON /COMHIT/ KH,HEIT(NXYDMP)
      COMMON /COMLHL/ LHL
      COMMON /COMALT/ ALT(NXDMP,NYDMP)
      COMMON /COMJPL/ JPL
      COMMON /COMKXM/ KXM
C
      COMMON /GLNGTH/ XL,YL,ZL,Z0,ZN,NXX,NYY
      COMMON /GVIEW1/ CA1,SA1,CC1,SC1,EL1,OX1,OY1,OZ1
      COMMON /TDATA1/ XDATA(NXDMP),YDATA(NYDMP),ZDATA(NXDMP,NYDMP)
      COMMON /ANGL3D/ AW,CW
      COMMON /WFCTR/  XFCTR1,YFCTR1,ZFCTR1,DOX,DOY
      COMMON /GDFN2D/ GXS,GXE,GYS,GYE
C
      HX=XL/(NXX-1)
      HY=YL/(NYY-1)
      NX=NXX
      NY=NYY
      MX=NXX
      MY=NYY
      XW=XL
      YW=YL
      ALPHA=-90.0-AW+360.0
      IF (ALPHA.GT.180.0) THEN
         ALPHA=ALPHA-360.0
      ENDIF
C     
      RAD=3.141592/180.0
      OX=(OX1-GXS)*XFCTR1
      OY=(OY1-GYS)*YFCTR1
      IF (ALPHA.GE.0.0.AND.ALPHA.LT.90.0) THEN
         ELD=0.0
      ELSE IF (ALPHA.GE.-90.0.AND.ALPHA.LT.0.0) THEN
         ELD=(XL-OX)*SIN(RAD*ALPHA)
      ELSE IF (ALPHA.GE.-180.0.AND.ALPHA.LT.-90.0) THEN
         ELD=(XL-OX)*SIN(RAD*ALPHA)+(YL-OY)*COS(RAD*ALPHA)
      ELSE IF (ALPHA.GE.90.0.AND.ALPHA.LE.180.0) THEN
         ELD=(YL-OY)*COS(RAD*ALPHA)
      ENDIF
C
      ELL=EL1*SIN(RAD*CW)
      IF (ABS(ELL).LT.0.01) ELL=0.001
      IF (ABS(CW).LT.1.0.OR.ABS(CW-180.0).LT.1.0) THEN
         ELL=EL1
      ENDIF
C
      IF(IXY.EQ.0) THEN
         KH=IABS(IND)
         DZ=(ZN-Z0)/(KH-1)
         DO 10 I=1,KH
            HEIT(I)=Z0+DZ*(I-1)
   10    CONTINUE
      ELSE
         KH=2
         HEIT(1)=Z0
         HEIT(2)=ZN
      ENDIF
      LHL=IND
      DO 20 IX=1,NX
      DO 20 IY=1,NY
         ALT(IX,IY)=ZDATA(IX,IY)
   20 CONTINUE
C
      KXM = 1
      CALL PREP3D
C
      IF(IXY.EQ.1.OR.IXY.EQ.3) CALL XYPL3D(1)
      IF(IXY.EQ.2.OR.IXY.EQ.3) CALL XYPL3D(2)
      IF(IXY.EQ.0) THEN
         MXS=MX
         MX=2
         CALL XYPL3D(1)
         MX=MXS
         MYS=MY
         MY=2
         CALL XYPL3D(2)
         MY=MYS
      ENDIF
C
      JPL = 0
      CALL CONT3D
C
      RETURN
      END
C
C
      SUBROUTINE PREP3D
C ** TO CALCULATE PARAMETERS **
      COMMON / COMDT1 / HX,HY,NX,NY,MX,MY,XW,YW
      COMMON / COMDT5 / ALPHA,ELD,ELL
      COMMON / COMCFT / XFCTR,YFCTR,HXFC,HYFC
      COMMON / COMPFT / PX,PZ,PX1,PZ1
      COMMON / COMHL5 / CA,SA,ET,EL,XWS,YWC
      COMMON / COMHL4 / MPX,MPY
C ***
      RAD = 3.141593/180.0
      XFCTR = XW/(HX*FLOAT(NX-1))
      YFCTR = YW/(HY*FLOAT(NY-1))
      HXFC  = HX*XFCTR
      HYFC  = HY*YFCTR
      MPX   = 1
      MPY   = 1
C ***
      CA  = COS(RAD*ALPHA)
      SA  = SIN(RAD*ALPHA)
      XWS = XW*SA
      YWC = YW*CA
C ***
      EL  = ELL
      ET = EL + ELD
C ***
      PX   = 0.02
      PZ   = 0.02
      PX1  = 1.1*PX
      PZ1  = 1.1*PZ
C ***
      RETURN
      END
C
      SUBROUTINE PLOT3D( X, Y, JP )
C ** JP= 3............. START     **
C ** JP= 0  OR  2  .... CONTINUED **
      COMMON / COMLHL / LHL
      COMMON / COMHL2 / XQ,YQ,ZQ,KPL
      COMMON / COMXZP / XP,ZP
      COMMON / COMVAN / X3,X4,Y3,Y4,Z3,Z4,XPS1,ZPS1
      COMMON / COMZHV / HV
      COMMON / COMJPL / JPL
      COMMON / COMIPC / IPC
C ***
      IP=IPC
      XPS1 = XP
      ZPS1 = ZP
      KPLS = KPL
C      V = 1.0/(X*SA+Y*CA+ET)
C      XP = (X*CA-Y*SA+SG)*V*EL
C      ZP = ( (HV*ZFCTR+ZC)*V-ZM )*EL
      CALL GTTT1D(X,Y,HV,XP,ZP)
      IF( LHL .NE. 0 ) GO TO 1
      IP = 3
      IF( JP .NE. 3 ) IP = 2
      CALL PLTZ3D(IP)
      GO TO 999
 1     CONTINUE
      XQ = X
      YQ = Y
      ZQ = HV
      IF( JP .NE. 3 ) GO TO 2
C *** JP=3 ***
      IPC = 3
      KPL = -1
      CALL PLTZ3D(JP)
      GO TO 800
C *** JP=2 OR 0 ***
 2     CONTINUE
      CALL HIDN3D
      IF( KPL .EQ. KPLS ) GO TO 3
      X3 = X4
      X4 = X
      Y3 = Y4
      Y4 = Y
      Z3 = Z4
      Z4 = HV
      CALL VANS3D(IP)
      IPC = IP
      GO TO 800
C ***
 3     CONTINUE
      IPV = IP
      IF( JPL .EQ. 1 ) IPV = 3
      CALL PLTZ3D(IPV)
 800   CONTINUE
      X4 = X
      Y4 = Y
      Z4 = HV
 999   RETURN
      END
C
      SUBROUTINE VANS3D(KP)
C ** TO FIND VANISH POINT **
      COMMON / COMVAN / X3,X4,Y3,Y4,Z3,Z4,XPS1,ZPS1
      COMMON / COMHL2 / XQ,YQ,ZQ,KPL
      COMMON / COMXZP / XP,ZP
      COMMON / COMPFT / PX,PZ,PX1,PZ1
      COMMON / COMJPL / JPL
C ***
      DX = -0.5*(X4-X3)
      DY = -0.5*(Y4-Y3)
      DZ = -0.5*(Z4-Z3)
      ZV = Z4
      XP1 = XP
      ZP1 = ZP
      KPL1 = KPL
      DO 130 L=1,8
      XQ = XQ + DX
      YQ = YQ + DY
      ZV = ZV + DZ
C      V  = 1.0/(XQ*SA+YQ*CA+ET)
      XPS = XP
      ZPS = ZP
C      XP = (XQ*CA-YQ*SA+SG)*V*EL
C      ZP = ( (ZV*ZFCTR+ZC)*V-ZM )*EL
      CALL GTTT1D(XQ,YQ,ZV,XP,ZP)
      ZQ = ZV
      KPLS = KPL
      CALL HIDN3D
      IF( ABS(XP-XPS) .GT. PX ) GO TO 131
      IF( ABS(ZP-ZPS) .GT. PZ ) GO TO 131
      GO TO 132
 131   CONTINUE
      DX = 0.5*DX
      DY = 0.5*DY
      DZ = 0.5*DZ
      IF( KPL .EQ. KPLS ) GO TO 130
      DX = -DX
      DY = -DY
      DZ = -DZ
 130   CONTINUE
 132   CONTINUE
      IF( ABS(XP-XP1) .GT. PX1 ) GO TO 133
      IF( ABS(ZP-ZP1) .GT. PZ1 ) GO TO 133
      XP = XP1
      ZP = ZP1
      GO TO 134
 133   CONTINUE
      IF( ABS(XP-XPS1) .GT. PX1 ) GO TO 134
      IF( ABS(ZP-ZPS1) .GT. PZ1 ) GO TO 134
      XP = XPS1
      ZP = ZPS1
 134   CONTINUE
      KPV = KP
      IF( JPL .EQ. 1 ) KPV = 3
      CALL PLTZ3D( KPV )
      KPL = KPL1
      KP = 5-KP
      XP = XP1
      ZP = ZP1
      KPV = KP
      IF( JPL .EQ. 1 ) KPV = 3
      CALL PLTZ3D( KPV )
C ***
      RETURN
      END
C
CC
      SUBROUTINE HIDN3D
C ** HIDDEN LINE **
C
      INCLUDE 'A3dcomm.inc'
      COMMON / COMLHL / LHL
      COMMON / COMDT1 / HX,HY,NX,NY,MX,MY,XW,YW
      COMMON / GLNGTH / XL,YL,ZL,Z0,ZN,NXX,NYY
      COMMON / COMCFT / XFCTR,YFCTR,HXFC,HYFC
      COMMON / COMHL5 / CA,SA,ET,EL,XWS,YWC
      COMMON / COMHL2 / XQ,YQ,ZQ,KPL
      COMMON / COMHL4 / MPX,MPY
      COMMON / COMXZP / XP,ZP
      COMMON / COMALT / ALT(NXDMP,NYDMP)
      COMMON / GVIEW1 / CA1,SA1,CC1,SC1,EL1,OX1,OY1,OZ
      COMMON / WFCTR  / XFCTR1,YFCTR1,ZFCTR1,DOX,DOY
      COMMON / GDFN2D / GXS,GXE,GYS,GYE
C ***
      KPL = 1
      IF( LHL .EQ. 0 ) GO TO 599
      EPS = 1.0E-04
      OX=(OX1-GXS)*XFCTR1
      OY=(OY1-GYS)*YFCTR1
C      CALL GTTT1D(OX,OY,OZ,XX,ZZ)
C      U = CA-(XP-XX)/EL*SA
      XTMP=(XQ-OX)*CA-(YQ-OY)*SA
      YTMP=(XQ-OX)*SA+(YQ-OY)*CA
      YTMP=YTMP+EL
      RADIUS=SQRT(XTMP**2+YTMP**2)
      CT=YTMP/RADIUS
      ST=XTMP/RADIUS
      U=CT*CA-ST*SA
      IF( ABS(U) .LT. 1.E-5 ) U=1.0E-5
C      V = (XP-XX)/EL*CA+SA
      V=ST*CA+CT*SA
      IF( ABS(V) .LT. 1.E-5 ) V=1.0E-5
      TX = U/V
      TY = V/U
C ***
      KPQ = 0
      DY = TX*XQ
      YV = YQ-DY
      IF( YV .LT. -EPS   ) GO TO 51
      IF( YV .GT. YW+EPS ) GO TO 51
      IV = IFIX(0.9999*XQ/HXFC) + 1
      IF( XQ .LT. EPS ) IV = 0
      JV = IFIX( (ABS(DY)+EPS)/HYFC )
      IA = IV + 1
      AKY = YQ/HYFC
      JA = IFIX(1.0001*AKY) + 1
      KR = 0
      IF( ABS(AKY-FLOAT(JA-1))  .GT. EPS ) KR = 1
      KPQ = 1
      KXV = -1
      KYV = -1
      IF( DY .LT. 0.0 ) KYV = 1
      IF( KYV .EQ. -1 ) JA = JA + KR
      V = YV*CA
 51    DY = TX*(XW-XQ)
      YV = YQ+DY
      IF( YV .LT. -EPS    ) GO TO 52
      IF( YV .GT. YW+ EPS ) GO TO 52
      V1 = XWS+YV*CA
      IF( KPQ .EQ. 0 ) GO TO 151
      IF( V1  .GT. V ) GO TO  52
 151   CONTINUE
      IV = IFIX(0.9999*(XW-XQ)/HXFC)+1
      IF( XQ .GT. XW-EPS ) IV = 0
      JV = IFIX( (ABS(DY)+EPS)/HYFC )
      IA = NX -IV
      AKY =YQ/HYFC
      JA = IFIX(1.0001*AKY)+1
      KR = 0
      IF( ABS(AKY-FLOAT(JA-1)) .GT. EPS ) KR = 1
      KPQ = 1
      KXV = 1
      KYV = 1
      IF( DY .LT. 0.0 ) KYV = -1
      IF( KYV .EQ. -1 ) JA = JA+KR
      V = V1
 52    DX =TY*YQ
      XV=XQ-DX
      IF( XV .LT. -EPS ) GO TO 53
      IF( XV .GT. XW+EPS) GO TO 53
      V1=XV*SA
      IF( KPQ .EQ. 0 ) GO TO 152
      IF( V1 .GE. V ) GO TO 53
 152   CONTINUE
      IV =IFIX( (ABS(DX)+EPS)/HXFC )
      JV =IFIX(0.9999*YQ/HYFC)+1
      IF( YQ .LT. EPS ) JV=0
      AKX = XQ/HXFC
      IA = IFIX(1.0001*AKX)+1
      KR = 0
      IF( ABS(AKX-FLOAT(IA-1)) .GT. EPS ) KR = 1
      JA = JV+1
      KPQ = 1
      KYV = -1
      KXV = -1
      IF( DX .LT. 0.0 ) KXV = 1
      IF( KXV .EQ. -1 ) IA = IA+KR
      V = V1
 53    DX = TY*(YW-YQ)
      XV = XQ+DX
      IF( XV .LT. -EPS ) GO TO 54
      IF( XV .GT. XW+EPS ) GO TO 54
      V1 = YWC+XV*SA
      IF( KPQ .EQ. 0 ) GO TO 153
      IF( V1  .GE. V ) GO TO 54
 153   CONTINUE
      IV = IFIX( (ABS(DX)+EPS) /HXFC )
      JV = IFIX(0.9999*(YW-YQ)/HYFC)+1
      IF( YQ .GT. YW-EPS ) JV = 0
      AKX = XQ/HXFC
      IA = IFIX(1.0001*AKX)+1
      KR = 0
      IF( ABS(AKX-FLOAT(IA-1)) .GT. EPS ) KR = 1
      JA = NY-JV
      KYV = 1
      KXV = 1
      IF( DX .LT. 0.0 ) KXV = -1
      IF( KXV .EQ. -1 ) IA = IA+KR
      V = V1
 54    CONTINUE
C ***
      IF( JV .LT. IV ) GO TO 56
 55    CONTINUE
      IF( IV .EQ. 0 ) GO TO 56
      XV = XQ
      YV = YQ
      ZV = ZQ
C ***
      DO 510 K=1,IV
      IA =IA + KXV
      XVS =XV
      YVS =YV
      XV  = HXFC*FLOAT(IA-1)
      YV  = YQ+TX*(XV-XQ)
      AKY = YV/HYFC
      KY = IFIX(1.0001*AKY)+1
      KYL = KY+1
      ZVS = ZV
      YVV = HYFC*FLOAT(KY-1)
      IAV = MPX*(IA-1)+1
      KYYV = MPY*(KY-1)+1
      KYLV = MPY*(KYL-1)+1
      ZV = ( ALT(IAV,KYLV)-ALT(IAV,KYYV) )*(YV-YVV)/HYFC
     &+ALT(IAV,KYYV)
      IF( ZV .GT. ZN ) GO TO 161
      IF( ZV .LT. Z0 ) GO TO 162
      GO TO 155
 161   Z2 = ZN
      GO TO 163
 162   Z2 = Z0
 163   CONTINUE
      ZVC = 0.0
      IF( ABS(Z2-ZVS) .LT. 1.E-5 ) GO TO 164
      IF( ABS(ZV-ZVS) .GT. 1.E-5 ) ZVC=(ZV-Z2)/(ZV-ZVS)
 164   CONTINUE
      XV = XV-ZVC*(XV-XVS)
      YV = YV-ZVC*(YV-YVS)
      ZV = Z2
 155   CONTINUE
C      ZPV = ( (ZV*ZFCTR+ZC)/(XV*SA+YV*CA+ET)-ZM )*EL
      CALL GTTT1D(XV,YV,ZV,XTMP,ZPV)
      IF( K .NE. 1 ) GO TO  61
      KHL = 2
      IF( ZP .LT. ZPV ) KHL = 1
      GO TO 510
 61    CONTINUE
      GO TO (62,63),KHL
 62    CONTINUE
      IF( ABS(ZV-ZN) .GT. 1.E-5 ) GO TO 171
      IF( IABS(LHL) .EQ. 1 ) GO TO 510
 171   CONTINUE
      IF( ZP .LE. ZPV ) GO TO 510
      GO TO 64
 63    CONTINUE
      IF( ABS(ZV-Z0) .GT. 1.E-5 ) GO TO 172
      IF( IABS(LHL) .EQ. 1 ) GO TO 510
 172   CONTINUE
      IF( ZP .GE. ZPV ) GO TO 510
 64    KPL = -1
      GO TO 599
 510   CONTINUE
      IF( JV .LT. IV ) GO TO 599
C ***
 56    CONTINUE
      IF( JV .EQ. 0 ) GO TO 57
      XV = XQ
      YV = YQ
      ZV = ZQ
C ***
      DO 520 K = 1, JV
      JA = JA+KYV
      YVS = YV
      XVS = XV
      YV = HYFC*FLOAT(JA-1)
      XV = XQ+TY*(YV-YQ)
      AKX = XV/HXFC
      KX = IFIX(1.0001*AKX)+1
      KXL = KX+1
      ZVS = ZV
      XVV = HXFC*FLOAT(KX-1)
      JAV = MPY*(JA-1)+1
      KXXV = MPX*(KX-1)+1
      KXLV = MPX*(KXL-1)+1
      ZV   = ( ALT(KXLV,JAV)-ALT(KXXV,JAV) )*(XV-XVV)/HXFC
     &+ALT(KXXV,JAV)
      IF( ZV .GT. ZN ) GO TO 166
      IF( ZV .LT. Z0 ) GO TO 167
      GO TO 157
 166   Z2 = ZN
      GO TO 168
 167   Z2 = Z0
 168   CONTINUE
      ZVC = 0.0
      IF( ABS(Z2-ZVS) .LT. 1.E-5 ) GO TO 169
      IF( ABS(ZV-ZVS) .GT. 1.E-5 ) ZVC=(ZV-Z2)/(ZV-ZVS)
 169   CONTINUE
      XV = XV-ZVC*(XV-XVS)
      YV = YV-ZVC*(YV-YVS)
      ZV = Z2
 157   CONTINUE
      CALL GTTT1D(XV,YV,ZV,XTMP,ZPV)
C      ZPV = ( (ZV*ZFCTR+ZC)/(XV*SA+YV*CA+ET)-ZM )*EL
      IF( K .NE. 1 ) GO TO 65
      KHL = 2
      IF( ZP .LT. ZPV ) KHL = 1
      GO TO 520
 65    CONTINUE
      GO TO (66,67),KHL
 66    CONTINUE
      IF( ABS(ZV-ZN) .GT. 1.E-5 ) GO TO 175
      IF( IABS(LHL) .EQ. 1 ) GO TO 520
 175   CONTINUE
      IF( ZP .LE. ZPV ) GO TO 520
      GO TO 68
 67    CONTINUE
      IF( ABS(ZV-Z0) .GT. 1.E-5 ) GO TO 176
      IF( IABS(LHL) .EQ. 1 ) GO TO 520
 176   CONTINUE
      IF( ZP .GE. ZPV ) GO TO 520
 68    KPL = -1
      GO TO 599
 520   CONTINUE
 57    CONTINUE
      IF( JV .LT. IV ) GO TO 55
 599   RETURN
      END
C
      SUBROUTINE XYPL3D(L)
C ** L=1 ... PARALLEL TO Y-AXIS **
C ** L=2 ... PARALLEL TO X-AXIS **
      INCLUDE 'A3dcomm.inc'
      COMMON / COMZHV / HV
      COMMON / COMLHL / LHL
      COMMON / COMDT1 / HX,HY,NX,NY,MX,MY,XW,YW
      COMMON /GLNGTH/ XL,YL,ZL,Z0,ZN,NXX,NYY      
      COMMON / COMHL4 / MPX,MPY
      COMMON / COMCFT / XFCTR,YFCTR,HXFC,HYFC
      COMMON / COMALT / ALT(NXDMP,NYDMP)
      COMMON / COMJPL / JPL
C ***
      NXC = NX
      NYC = NY
      MPXC = MPX
      MPYC = MPY
      MPX = (NX-1)/(MX-1)
      MPY = (NY-1)/(MY-1)
C ***
      GO TO (51,52),L
 51    CONTINUE
      M = MX
      N = NY
      HFC = HXFC
      HW  = HYFC
      MP = MPX
      YXN = YW
      GO TO 53
 52    CONTINUE
      M = MY
      N = NX
      HFC = HYFC
      HW  = HXFC
      MP = MPY
      YXN = XW
 53    CONTINUE
C ***     ***
      NX = MX
      NY = MY
      HXFCC = HXFC
      HYFCC = HYFC
      HXFC = HXFCC*FLOAT(MPX)
      HYFC = HYFCC*FLOAT(MPY)
      KS = 1
      DO 10 IP=1,M
      I = MP*(IP-1)+1
      V = HFC*FLOAT(I-1)
      J = 1
      IF( KS .EQ. -1 ) J = N
      W = 0.0
      IF( KS .EQ. -1 ) W = YXN
      GO TO (54,55),L
 54    Z = ALT(I,J)
      X = V
      Y = W
      GO TO 56
 55    Z = ALT(J,I)
      Y = V
      X = W
 56    CONTINUE
      ZS = Z
      IF( Z .GT. ZN ) Z = ZN
      IF( Z .LT. Z0 ) Z = Z0
      HV = Z
      CALL PLOT3D( X, Y, 3 )
      JPL = 0
      JPLV = 0
      JPLC = JPL
      IF( ZS .GT. ZN ) GO TO 39
      IF( ZS .GE. Z0 ) GO TO 42
 39    CONTINUE
      JPL = 1
      JPLV = 1
      JPLC =1
      IF( IABS(LHL) .NE. 2 ) GO TO 41
      IF( IP .EQ. 1 ) GO TO 43
      IF( IP .EQ. M ) GO TO 43
 41    CONTINUE
      IF( IABS(LHL) .EQ. 3 ) JPLV = 0
      JPL = JPLV
 42    CONTINUE
      CALL PLOT3D( X, Y, 0 )
      JPL = JPLC
      GO TO 44
 43    JPL = 0
      JPLV = 0
      CALL PLOT3D( X, Y, 0 )
      JPL = 1
 44    CONTINUE
C ***                    ***
      DO 20 J=2,N
      JV = J
      IF( KS .EQ. -1 ) JV = N-J+1
      W = W+HW
      GO TO (57,58),L
 57    Z = ALT(I,JV)
      Y = W
      GO TO 59
 58    Z = ALT(JV,I)
      X = W
 59    CONTINUE
      IF( Z .GT. ZN ) GO TO 2
      IF( Z .LT. Z0 ) GO TO 3
      IF( JPL .EQ. 1 ) GO TO 1
      HV = Z
      CALL PLOT3D( X, Y, 0 )
      ZS = Z
      GO TO 20
C ***
 1     IF( ZS .GT. ZN ) Z2 = ZN
      IF( ZS .LT. Z0 ) Z2 = Z0
      JPL = JPLV
      JPLV = 0
      HV = Z2
      WW = W-(Z-Z2)/(Z-ZS)*HW
      GO TO (61,62),L
 61    CALL PLOT3D( X, WW, 0 )
      HV = Z
      JPL = 0
      CALL PLOT3D( X, W , 0 )
      GO TO 63
 62    CALL PLOT3D( WW, Y, 0 )
      HV = Z
      JPL = 0
      CALL PLOT3D( W, Y, 0 )
 63    CONTINUE
      ZS = Z
      GO TO 20
C ***
 2     Z2 = ZN
      IF( ZS .GT. ZN ) GO TO 5
      Z5 = Z0
      IF( ZS. LT. Z5 ) GO TO 6
      GO TO 4
 3     Z2 = Z0
      IF( ZS .LT. Z0 ) GO TO 5
      Z5 = ZN
      IF( ZS .GT. Z5 ) GO TO 6
 4     CONTINUE
      JPLV = 1
      HV = Z2
      WW = W-(Z-Z2)/(Z-ZS)*HW
      IF( IABS(LHL) .NE. 2 ) GO TO 46
      IF( IP .EQ. 1 ) GO TO 45
      IF( IP .EQ. M ) GO TO 45
      GO TO 46
 45    IF( ABS(Z2-Z0) .LT. 1.E-5 ) GO TO 46
      JPLV = 0
 46    CONTINUE
      IF( IABS(LHL) .EQ. 3 ) JPLV = 0
      GO TO (64,65),L
 64    CALL PLOT3D( X, WW, 0 )
      JPL = JPLV
      CALL PLOT3D( X , W, 0 )
      GO TO 66
 65    CALL PLOT3D( WW, Y, 0 )
      JPL = JPLV
      CALL PLOT3D( W , Y, 0 )
 66    CONTINUE
      JPL = 1
      ZS = Z
      GO TO 20
 5     CONTINUE
      HV = Z2
      JPLC = JPL
      JPL = JPLV
      CALL PLOT3D( X, Y, 0 )
      JPL = JPLC
      ZS = Z
      GO TO 20
 6     CONTINUE
      HV = Z5
      WW = W-(Z-Z5)/(Z-ZS)*HW
      GO TO (67,68),L
 67    CALL PLOT3D( X, WW, 0 )
      GO TO 4
 68    CALL PLOT3D( WW, Y, 0 )
      GO TO 4
 20    CONTINUE
C ***
      HW = -HW
      KS = -KS
 10    CONTINUE
      NX = NXC
      NY = NYC
      HXFC = HXFCC
      HYFC = HYFCC
      MPX = MPXC
      MPY = MPYC
      RETURN
      END
C
      SUBROUTINE PLTZ3D(KP)
C
      COMMON / COMXZP / XP,ZP
      XPP = XP
      ZPP = ZP
      IF(KP.EQ.2) CALL DRAW(XPP,ZPP)
      IF(KP.EQ.3) CALL MOVE(XPP,ZPP)
      RETURN
      END
      SUBROUTINE CONT3D
C ** TO FIND STARTING POINT **
      INCLUDE 'A3dcomm.inc'
      COMMON / COMALT / ALT(NXDMP,NYDMP)
      COMMON / COMKSW / KSW(NXDMP,NYDMP)
      COMMON / COMKXM / KXM
      COMMON / COMDT1 / HX,HY,NX,NY,MX,MY,XW,YW
      COMMON / COMCFT / XFCTR,YFCTR,HXFC,HYFC
      COMMON / COMSRH / IA,JA,IT,JT,KS,UA1,UA2,KV,IREP
      COMMON / COMHIT / KH,HEIT(NXYDMP)
      COMMON / COMZHV / HV
C ***
      HXFC = XW/FLOAT(NX-1)
      HYFC = YW/FLOAT(NY-1)
      DO 50 I=1,NX
      DO 50 J=1,NY
      KSW(I,J)= 0
 50    CONTINUE
C ***
      DO 10 K=1,KH
C      KERR = 0
      KV = K
      HV = HEIT(KV)
C ***
      DO 20 KX=1,NX,KXM
      V = ALT(KX,1) - HV
      IF( V ) 2,3,1
 1     KG = 1
      GO TO 4
 2     KG = 2
      GO TO 4
 3     KG =3
 4     CONTINUE
      IF( KSW(KX,1) .NE. KV ) GO TO 5
      IF( ALT(KX,2) - HV .LE. 0.0 ) KG = 2
 5     CONTINUE
C ***
      DO 30 KY=2,NY
      V = ALT(KX,KY) - HV
      IF( KSW(KX,KY) .EQ. KV ) GO TO 15
C ***
      GO TO (6,8,21),KG
C ** ALT(KX,1) .GT. HV
 6     IF( V ) 7,7,30
 7     CONTINUE
      IREP = 0
      DO 60 M=1,2
      IA = KX
      JA = KY - 1
      GO TO (31,32),M
 31    IF( KX .EQ. NX ) GO TO 60
      KS = -1
      IT = 1
      GO TO 33
 32    IF( KX .EQ. 1 ) GO TO 60
      IF( IREP .GE. 2 ) GO TO 60
      IT = -1
      KS = 1
      KSW(IA,JA) = 0
 33    JT = 0
      UA1 = ALT(IA,JA  )
      UA2 = ALT(IA,JA+1)
      ALX = HXFC*FLOAT(IA-1)
      RT = (UA2-HV)/(UA1-UA2)
      ALY = HYFC*(FLOAT(JA)+RT)
C      KERR = 1
      CALL PLOT3D( ALX, ALY, 3 )
      CALL PLOT3D( ALX, ALY, 2 )
      CALL SEAR3D
 60    CONTINUE
      KG = 2
      GO TO 30
C ** ALT(KX,1) .LT. HV **
 8     IF( V ) 30,30,9
 9     CONTINUE
      IREP = 0
      DO 70 M=1,2
      IA = KX
      JA = KY
      GO TO (34,35),M
 34    IF( KX .EQ. NX ) GO TO 70
      IT = 1
      KS = 1
      GO TO  36
 35    IF( KX .EQ. 1 ) GO TO 70
      IF(IREP .GE. 2 ) GO TO 70
      IT = -1
      KS = -1
      KSW(IA,JA) = 0
 36    JT = 0
      UA1 = ALT(IA,JA  )
      UA2 = ALT(IA,JA-1)
      ALX = HXFC*FLOAT(IA-1)
      RT = (UA1-HV)/(UA1-UA2)
      ALY = HYFC*(FLOAT(JA-1)-RT)
C      KERR = 1
      CALL PLOT3D( ALX, ALY, 3 )
      CALL PLOT3D( ALX, ALY, 2 )
      CALL SEAR3D
 70    CONTINUE
      KG = 1
      GO TO 30
C ***
 15    GO TO (16,17,30),KG
 16    KG = 2
      IF( KY .EQ. NY ) GO TO 30
      IF( ALT(KX,KY+1)-HV .GT. 0.0 ) KG = 1
      GO TO 30
 17    KG = 1
      IF( V .LT. 0.0 ) KG = 2
      GO TO 30
C ***
 21    IF( V ) 23,24,22
 22    KG = 1
      GO TO 25
 23    KG = 2
      GO TO 25
 24    KG = 3
 25    CONTINUE
C ***
 30    CONTINUE
C ***
 20    CONTINUE
C ***
C     IF( KERR .EQ. 0 ) WRITE(6,2001) HV
C2001  FORMAT('    (CONTL0) HEIT   (',1PE9.2,') IS NOT FOUND.')
C ***
 10    CONTINUE
      RETURN
      END
C
      SUBROUTINE SEAR3D
C ** TO DRAW CONTOUR LINE **
      INCLUDE 'A3dcomm.inc'
      COMMON / COMALT / ALT(NXDMP,NYDMP)
      COMMON / COMKSW / KSW(NXDMP,NYDMP)
      COMMON / COMDT1 / HX,HY,NX,NY,MX,MY,XW,YW
      COMMON / COMCFT / XFCTR,YFCTR,HXFC,HYFC
      COMMON / COMSRH / IA,JA,IT,JT,KS,UA1,UA2,KV,IREP
      COMMON / COMZHV / HV
C ***
      LN = 800
      IREP = 0
C ***
      DO 10 L=2,LN
      IF( KSW(IA,JA) .EQ. KV ) GO TO 99
      IAV = IA + IT
      IF( IAV .LE.  0 ) GO TO 98
      IF( IAV .GT. NX ) GO TO 98
      JAV = JA + JT
      IF( JAV .LE.  0 ) GO TO 98
      IF( JAV .GT. NY ) GO TO 98
      VA1 = ALT(IAV,JAV)
      IF( VA1-HV ) 1,1,2
C ***
 1     IREP = IREP + 1
      RT = (UA1-HV)/(UA1-VA1)
      ALX = HXFC*(  FLOAT(IA-1) + RT*FLOAT(IT)  )
      ALY = HYFC*(  FLOAT(JA-1) + RT*FLOAT(JT)  )
      CALL PLOT3D( ALX, ALY, 2 )
      ITV =IT
      IT = -KS*JT
      JT = KS*ITV
      UA2 = VA1
      IF( IREP .GE. 4 ) GO TO 98
      GO TO 10
C ***
 2     IREP = 0
      ITV = KS*JT
      JTV = -KS*IT
      IAV = IAV+ITV
      JAV = JAV+JTV
      VA2 = ALT(IAV,JAV)
      IF( VA2-HV ) 3,3,4
C ***
 3     RT = (VA1-HV)/(VA1-VA2)
      ALX = HXFC*(  FLOAT(IA+IT-1) + RT*FLOAT(ITV)  )
      ALY = HYFC*(  FLOAT(JA+JT-1) + RT*FLOAT(JTV)  )
      CALL PLOT3D( ALX, ALY, 2 )
      KSW(IA,JA) = KV
      IA = IA+IT
      JA = JA+JT
      UA1 = VA1
      UA2 = VA2
      GO TO 10
C ***
 4     ITV =  KS*JT
      JTV = -KS*IT
      RT = (UA2-HV)/(UA2-VA2)
      ALX = HXFC*(  FLOAT(IA+ITV-1) + RT*FLOAT(IT)  )
      ALY = HYFC*(  FLOAT(JA+JTV-1) + RT*FLOAT(JT)  )
      CALL PLOT3D( ALX, ALY, 2 )
      KSW(IA,JA) = KV
      IA = IA+IT+ITV
      JA = JA+JT+JTV
      IT = ITV
      JT = JTV
      UA1 = VA2
C ***
 10    CONTINUE
C ***
      GO TO 99
 98    KSW(IA,JA) = KV
 99    CONTINUE
      CALL PLOT3D( ALX, ALY, 3 )
      RETURN
      END
