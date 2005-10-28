C     $Id$
C
C     ****** CONTOUR PLOT : XY, FEM, PATTERN ******
C
      SUBROUTINE CONTPF(Z,X,Y,NNOD,IELM,NELM,
     &                  ZORG,ZSTEP,NSTEP,IPAT,KA)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSGFXY/ DX,DY,PXS,PYS,PXE,PYE,GXS,GYS,GXE,GYE,LGF
      DIMENSION Z(NNOD),X(NNOD),Y(NNOD),IELM(3,NELM),KA(2,NELM)
      DATA EPS/1.E-7/
C
      IF(.NOT.LGF.OR.ABS(ZSTEP).LT.1.E-32) RETURN
      ZBIAS=2.-10.*EPS
      KMAX=NSTEP
C
      DO 100 IE=1,NELM
         K1=INT((Z(ABS(IELM(1,IE)))-ZORG)/ZSTEP+ZBIAS)
         K2=INT((Z(ABS(IELM(2,IE)))-ZORG)/ZSTEP+ZBIAS)
         K3=INT((Z(ABS(IELM(3,IE)))-ZORG)/ZSTEP+ZBIAS)
         KA(1,IE)=MAX(MIN(K1,K2,K3),1)
         KA(2,IE)=MAX(K1,K2,K3,1)
  100 CONTINUE
C
      DO 1000 K=1,KMAX
         U0=REAL(K+1)
      DO 1000 IE=1,NELM
         IF(KA(1,IE).LE.K.AND.KA(2,IE).GT.K) THEN
            U1=(Z(ABS(IELM(1,IE)))-ZORG)/ZSTEP+ZBIAS
            U2=(Z(ABS(IELM(2,IE)))-ZORG)/ZSTEP+ZBIAS
            U3=(Z(ABS(IELM(3,IE)))-ZORG)/ZSTEP+ZBIAS
C
            IF(ABS(U1-U2).GT.EPS
     &         .AND.(U1.GT.U0.OR.U2.GT.U0)
     &         .AND.(U1.LE.U0.OR.U2.LE.U0)) THEN
               NA=ABS(IELM(1,IE))
               NB=ABS(IELM(2,IE))
               MA=ABS(IELM(3,IE))
               IF(ABS(U2-U3).GT.EPS
     &            .AND.(U2.GT.U0.OR.U3.GT.U0)
     &            .AND.(U2.LE.U0.OR.U3.LE.U0)) THEN
                  MB=ABS(IELM(2,IE))
               ELSE
                  MB=ABS(IELM(1,IE))
               ENDIF
            ELSE
               NA=ABS(IELM(1,IE))
               NB=ABS(IELM(3,IE))
               MA=ABS(IELM(2,IE))
               MB=ABS(IELM(3,IE))
            ENDIF
            UA=(Z(NA)-ZORG)/ZSTEP+ZBIAS
            UB=(Z(NB)-ZORG)/ZSTEP+ZBIAS
            XA=DX*(X(NA)-GXS)+PXS
            YA=DY*(Y(NA)-GYS)+PYS
            XB=DX*(X(NB)-GXS)+PXS
            YB=DY*(Y(NB)-GYS)+PYS
            IF(UB.NE.UA) THEN
               XS=(XB-XA)*(U0-UA)/(UB-UA)+XA
               YS=(YB-YA)*(U0-UA)/(UB-UA)+YA
            ELSE
               XS=XA
               YS=YA
            ENDIF
            CALL MOVEPT(XS,YS,IPAT)
C
            IEL=IE
            LINV=.FALSE.
            IES=IEL
            IEP=IEL
            IEM=IEL
C
  200       CONTINUE
C
  300       IF(IEM.NE.IE.OR.IEP.NE.NELM) GOTO 400
               IF(LINV) GOTO 500
               CALL MOVEPT(XS,YS,-IPAT)
               IEL=IE
               LINV=.TRUE.
               NA=MA
               NB=MB
               UA=(Z(NA)-ZORG)/ZSTEP+ZBIAS
               UB=(Z(NB)-ZORG)/ZSTEP+ZBIAS
               XA=DX*(X(NA)-GXS)+PXS
               YA=DY*(Y(NA)-GYS)+PYS
               XB=DX*(X(NB)-GXS)+PXS
               YB=DY*(Y(NB)-GYS)+PYS
               IF(UB.NE.UA) THEN
                  XX=(XB-XA)*(U0-UA)/(UB-UA)+XA
                  YY=(YB-YA)*(U0-UA)/(UB-UA)+YA
                  CALL DRAWPT(XX,YY)
               ENDIF
               KA(1,IEL)=KA(1,IEL)+1
               IES=IEL
               IEP=IEL
               IEM=IEL
               GOTO 300
C
  400       IF((IEL.EQ.IEM.AND.IEP.LT.NELM).OR.IEM.EQ.IE) THEN
               IEP=IEP+1
               IEL=IEP
            ELSE
               IEM=IEM-1
               IEL=IEM
            ENDIF
C
            IF(KA(1,IEL).LE.K.AND.KA(2,IEL).GT.K.AND.IEL.NE.IES) THEN
               N1=ABS(IELM(1,IEL))
               N2=ABS(IELM(2,IEL))
               N3=ABS(IELM(3,IEL))
               L12=(N1.EQ.NA.AND.N2.EQ.NB).OR.(N1.EQ.NB.AND.N2.EQ.NA)
               L23=(N2.EQ.NA.AND.N3.EQ.NB).OR.(N2.EQ.NB.AND.N3.EQ.NA)
               L31=(N3.EQ.NA.AND.N1.EQ.NB).OR.(N3.EQ.NB.AND.N1.EQ.NA)
C
               IF(L12.OR.L23.OR.L31) THEN
                  U1=(Z(ABS(IELM(1,IEL)))-ZORG)/ZSTEP+ZBIAS
                  U2=(Z(ABS(IELM(2,IEL)))-ZORG)/ZSTEP+ZBIAS
                  U3=(Z(ABS(IELM(3,IEL)))-ZORG)/ZSTEP+ZBIAS
C
                  IF(L12) THEN
                     NA=ABS(IELM(3,IEL))
                     IF(ABS(U2-U3).GT.EPS
     &                  .AND.(U2.GT.U0.OR.U3.GT.U0)
     &                  .AND.(U2.LE.U0.OR.U3.LE.U0)) THEN
                        NB=ABS(IELM(2,IEL))
                     ELSE
                        NB=ABS(IELM(1,IEL))
                     ENDIF
                  ELSE IF(L23) THEN
                     NA=ABS(IELM(1,IEL))
                     IF(ABS(U3-U1).GT.EPS
     &                  .AND.(U3.GT.U0.OR.U1.GT.U0)
     &                  .AND.(U3.LE.U0.OR.U1.LE.U0)) THEN
                        NB=ABS(IELM(3,IEL))
                     ELSE
                        NB=ABS(IELM(2,IEL))
                     ENDIF
                  ELSE
                     NA=ABS(IELM(2,IEL))
                     IF(ABS(U1-U2).GT.EPS
     &                  .AND.(U1.GT.U0.OR.U2.GT.U0)
     &                  .AND.(U1.LE.U0.OR.U2.LE.U0)) THEN
                        NB=ABS(IELM(1,IEL))
                     ELSE
                        NB=ABS(IELM(3,IEL))
                     ENDIF
                  ENDIF
                  UA=(Z(NA)-ZORG)/ZSTEP+ZBIAS
                  UB=(Z(NB)-ZORG)/ZSTEP+ZBIAS
                  XA=DX*(X(NA)-GXS)+PXS
                  YA=DY*(Y(NA)-GYS)+PYS
                  XB=DX*(X(NB)-GXS)+PXS
                  YB=DY*(Y(NB)-GYS)+PYS
                  IF(UB.NE.UA) THEN
                     XX=(XB-XA)*(U0-UA)/(UB-UA)+XA
                     YY=(YB-YA)*(U0-UA)/(UB-UA)+YA
                     CALL DRAWPT(XX,YY)
                  ENDIF
                  KA(1,IEL)=KA(1,IEL)+1
                  IF(IEL.EQ.IE) GOTO 500
                  IES=IEL
                  IEP=IEL
                  IEM=IEL
               ENDIF
            ENDIF
            GOTO 200
C
  500       CONTINUE
C
         ENDIF
 1000 CONTINUE
      RETURN
      END
