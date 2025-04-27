C     $Id$
C
C     ********************************************
C     ****** GSAF APPLICATION V3.5 : GRAPH2 ******
C     ********************************************
C
C     ****** DRAW TICKS ON AXIS ******
C
      SUBROUTINE GAXIS2(X0,Y0,X1,Y1,FMIN,FMAX,FORG,FSTEP,SLENG,IND)
C
      IMPLICIT LOGICAL(L)
      DATA EPS/0.01/
C
      LIN=MOD(IND,2).EQ.1
      LOUT=MOD(IND/2,2).EQ.1
      LNBOT=MOD(IND/16,2).EQ.1
      LNTOP=MOD(IND/32,2).EQ.1
      IPAT=0
      SCL=SLENG
C
      AL=SQRT((X1-X0)**2+(y1-y0)**2)
      IF(AL.LE.0.0) GOTO 9000
      COSA=(X1-X0)/AL
      SINA=(Y1-Y0)/AL
      XL=AL*COSA
      YL=AL*SINA
      FL=FMAX-FMIN
      IF(FMAX.LT.FORG.OR.FORG.LT.FMIN.OR.FL.EQ.0.0) GOTO 9000
         DX=XL*FSTEP/FL
         DY=YL*FSTEP/FL
         IF(LNBOT) THEN
            NS=INT((FORG-FMIN)/FSTEP-EPS)
         ELSE
            NS=INT((FORG-FMIN)/FSTEP+EPS)
         ENDIF
         IF(LNTOP) THEN
            NE=INT((FMAX-FORG)/FSTEP-EPS)
         ELSE
            NE=INT((FMAX-FORG)/FSTEP+EPS)
         ENDIF
C
         IF(LIN) THEN
            DX1= SCL*SINA
            DY1=-SCL*COSA
         ELSE
            DX1=0.0
            DY1=0.0
         ENDIF
         IF(LOUT) THEN
            DX2=-SCL*SINA
            DY2= SCL*COSA
         ELSE
            DX2=0.0
            DY2=0.0
         ENDIF
C
         CALL MOVE(X0,Y0)
         CALL DRAW(X0+XL,Y0+YL)
         DO 1000 N=-NS,NE
            X=X0+DX*N
            Y=Y0+DY*N
            CALL MOVEPT(X+DX1,Y+DY1,IPAT)
            CALL DRAWPT(X+DX2,Y+DY2)
 1000    CONTINUE
C
 9000 RETURN
      END
C
C     ****** DRAW NUMBERS ON AXIS ******
C
      SUBROUTINE GAXIV2(X0,Y0,X1,Y1,FMIN,FMAX,FORG,FSTEP,INDC,INDP)
C
      IMPLICIT LOGICAL(L)
      DIMENSION IPOSD(4)
      DATA IPOSD/2,1,3,4/
      DATA EPS/0.01/
C
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      IND2=ABS(INDP)
      IPOS=IPOSD(MOD(IND2,4)+1)
      LPOS=MOD(IND2/4,2).EQ.1
      LDIR=MOD(IND2/8,2).EQ.1
      LNBOT=MOD(IND2/16,2).EQ.1
      LNTOP=MOD(IND2/32,2).EQ.1
C
      AL=SQRT((X1-X0)**2+(y1-y0)**2)
      IF(AL.LE.0.0) GOTO 9000
      ANG=ATAN2(X1-X0,Y1-Y0)
      COSA=(X1-X0)/AL
      SINA=(Y1-Y0)/AL
      XL=AL*COSA
      YL=AL*SINA
      FL=FMAX-FMIN
      IF(FMAX.LT.FORG.OR.FORG.LT.FMIN) GOTO 9000
         DX=XL*FSTEP/FL
         DY=YL*FSTEP/FL
         IF(LNBOT) THEN
            NS=INT((FORG-FMIN)/FSTEP-EPS)
         ELSE
            NS=INT((FORG-FMIN)/FSTEP+EPS)
         ENDIF
         IF(LNTOP) THEN
            NE=INT((FMAX-FORG)/FSTEP-EPS)
         ELSE
            NE=INT((FMAX-FORG)/FSTEP+EPS)
         ENDIF
C
         IF(LDIR) THEN
            IF(LPOS) THEN
               DS=0.5*IPOS*CHH
               DX1=-DS*SINA-0.5*CHH*COSA
               DY1= DS*COSA-0.5*CHH*SINA
               IJUST=1
            ELSE
               DS=0.5*IPOS*CHH
               DX1= DS*SINA-0.5*CHH*COSA
               DY1=-DS*COSA-0.5*CHH*SINA
               IJUST=0
            ENDIF
            IF(INDP.GE.0) THEN
               CALL SETCHR(CHH,CHW,CHSP,ANG-90.0,0.0)
            ELSE
               CALL SETCHR(CHH,CHW,CHSP,ANG+90.0,0.0)
            ENDIF
         ELSE
            IF(LPOS) THEN
               DS=0.5*IPOS*CHH
               DX1=-DS*SINA
               DY1= DS*COSA
               IJUST=3
            ELSE
               DS=0.5*(IPOS+2)*CHH
               DX1= DS*SINA
               DY1=-DS*COSA
               IJUST=3
            ENDIF
            IF(INDP.GE.0) THEN
               CALL SETCHR(CHH,CHW,CHSP,ANG,0.0)
            ELSE
               CALL SETCHR(CHH,CHW,CHSP,ANG+180.0,0.0)
            ENDIF
         ENDIF
C
         DO 1000 N=NS,NE
            F=FORG+FSTEP*N
            X=X0+DX*N+DX1
            Y=Y0+DY*N+DY1
            CALL GNUMBR(X,Y,F,INDC,IJUST)
 1000       CONTINUE
      CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
 9000 RETURN
      END

