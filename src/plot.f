C     $Id$
C
C     **************************************
C     ****** GSAF PLOT INTERFACE V3.5 ******
C     **************************************
C
      SUBROUTINE XYOPEN(LDEV,XMIN,YMIN,XMAX,YMAX,ITYPE)
C
      I=LDEV
      X=XMIN
      Y=YMIN
      X=XMAX
      Y=YMAX
      I=ITYPE
      CALL GSOPEN
      RETURN
      END
C
      SUBROUTINE XYCLOS
C
      CALL GSCLOS
      RETURN
      END
C
      SUBROUTINE PLOTS(NAME,SPACE)
C
      COMMON /GSAFPL/ FACTS
      CHARACTER NAME*8,DUM*8
      DATA INIT/0/
C
      DUM=NAME
      FACTS=1.0
      CALL GSOPEN
      IF(INIT.EQ.0) THEN
         CALL GSTITL('//')
         INIT=1
      ENDIF
      IF(SPACE.GT.0.0) CALL GSSIZE(SPACE,SPACE*18.1/25.6)
      CALL PAGES
      RETURN
      END
C
      SUBROUTINE PLOTE
C
      CALL PAGEE
      RETURN
      END
C
      SUBROUTINE PLOT(X,Y,IPEN)
C
      IF(ABS(IPEN).EQ.2) THEN
         CALL DRAW(X,Y)
      ELSEIF(ABS(IPEN).EQ.3) THEN
         CALL MOVE(X,Y)
      ELSEIF(IPEN.EQ.999) THEN
         CALL PAGEE
         CALL GSCLOS
      ENDIF
C
      IF(IPEN.LT.0) THEN
         CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
         XMIN=XMIN-X
         XMAX=XMAX-X
         YMIN=YMIN-Y
         YMAX=YMAX-Y
         CALL SETVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      ENDIF
      RETURN
      END
C
      SUBROUTINE SYMBOL(X,Y,HIGHT,NB,THETA,N)
C
      COMMON /GSAFPL/ FACTS
      CHARACTER NB*256
C
      IF(N.LT.0) THEN
         CALL MARKER(X,Y,HIGHT,NB,THETA,N)
         RETURN
      ENDIF
C
      IF(ABS(X-999.0).LT.1.E-30) THEN
         CALL INQPOS(X1,YY)
      ELSE
         X1=X
      ENDIF
      IF(ABS(Y-999.0).LT.1.E-30) THEN
         CALL INQPOS(XX,Y1)
      ELSE
         Y1=Y
      ENDIF
      CALL SETCHS(HIGHT*FACTS,THETA)
      CALL MOVE(X1,Y1)
      CALL TEXT(NB,N)
      RETURN
      END
C
      SUBROUTINE MARKER(X,Y,HIGHT,ICODE,THETA,N)
C
      COMMON /GSAFPL/ FACTS
C
      IF(N.GE.0) RETURN
C
      IF(ABS(X-999.0).LT.1.E-30) THEN
         CALL INQPOS(X1,YY)
      ELSE
         X1=X
      ENDIF
      IF(ABS(Y-999.0).LT.1.E-30) THEN
         CALL INQPOS(XX,Y1)
      ELSE
         Y1=Y
      ENDIF
C
      IF(N.EQ.-1) THEN
         CALL MOVE(X1,Y1)
      ELSE
         CALL DRAW(X1,Y1)
      ENDIF
C
      I=ICODE
      IF(ICODE.LT.16) THEN
         I=I+16
      ELSE
        IF(ICODE.LT.32) THEN
           I=I-16
        ENDIF
      ENDIF
C
      CALL SETMRK(-I,HIGHT*FACTS,HIGHT*FACTS,THETA,0.0)
      CALL MARK(X1,Y1)
      RETURN
      END
C
      SUBROUTINE NUMBER(X,Y,HIGHT,FPN,THETA,N1)
C
      COMMON /GSAFPL/ FACTS
      CHARACTER KFORM*10,KSTR*20,KSTR1*20
      N=N1
      IF(ABS(X-999.0).LT.1.E-30) THEN
         CALL INQPOS(X1,YY)
      ELSE
         X1=X
      ENDIF
      IF(ABS(Y-999.0).LT.1.E-30) THEN
         CALL INQPOS(XX,Y1)
      ELSE
         Y1=Y
      ENDIF
      CALL SETCHS(HIGHT*FACTS,THETA)
C
      IF(N.EQ.0) THEN
         WRITE(KSTR,'(I20)') NINT(FPN)
      ELSEIF(N.GT.0) THEN
         IF(N.GT.9) N=9
         WRITE(KFORM,801) N
  801    FORMAT('(F20.',I1,')')
         WRITE(KSTR,KFORM) FPN
      ELSE
         IF(N.LT.-9) N=-9
         WRITE(KFORM,802) -N
  802    FORMAT('(',I1,'PF20.0)')
         WRITE(KSTR,KFORM) FPN
      ENDIF
C
      I=0
 1000 CONTINUE
         I=I+1
      IF(KSTR(I:I).EQ.' ') GOTO 1000
      KSTR1=KSTR(I:20)
      NSTR=21-I
      CALL MOVE(X1,Y1)
      CALL TEXT(KSTR1,NSTR)
      RETURN
      END
C
      SUBROUTINE SCALE(ARRY,W,NPT,INC)
C
      DIMENSION ARRY(INC,*)
C
      XMIN=ARRY(1,1)
      XMAX=ARRY(1,1)
      DO 100 N=2,NPT
         X=ARRY(1,N)
         XMIN=MIN(X,XMIN)
         XMAX=MAX(X,XMAX)
  100 CONTINUE
      DELTA=(XMAX-XMIN)/W
      DL=LOG10(DELTA)
      IF(DL.GE.0.0) THEN
         IDL=DL
      ELSE
         IDL=DL-0.999999
      ENDIF
      DDL=10.0**(DL-IDL)
      IF(DDL.LT.1.000001) THEN
         DELTAN=1.0*10.0**IDL
      ELSEIF(DDL.LT.2.000001) THEN
         DELTAN=2.0*10.0**IDL
      ELSEIF(DDL.LT.3.000001) THEN
         DELTAN=3.0*10.0**IDL
      ELSEIF(DDL.LT.5.000001) THEN
         DELTAN=5.0*10.0**IDL
      ELSEIF(DDL.LT.8.000001) THEN
         DELTAN=8.0*10.0**IDL
      ELSE
         DELTAN=10.0**(IDL+1)
      ENDIF
      XN=XMIN/DELTAN
      IF(XN.GE.0.0) THEN
         IXN=XN
      ELSE
         IXN=XN-0.999999
      ENDIF
      XMINN=IXN*DELTAN
C
      ARRY(1,NPT+1)=XMINN
      ARRY(1,NPT+2)=DELTAN
      RETURN
      END
C
      SUBROUTINE LINE(XARY,YARY,NPT,INC,K,L)
C
      COMMON /GSAFPL/ FACTS
      DIMENSION XARY(INC,*),YARY(INC,*)
C
      XMIN=XARY(1,NPT+1)
      DELX=XARY(1,NPT+2)
      YMIN=YARY(1,NPT+1)
      DELY=YARY(1,NPT+2)
C
      X=(XARY(1,1)-XMIN)/DELX
      Y=(YARY(1,1)-YMIN)/DELY
      CALL MOVE(X,Y)
      IF(K.NE.0) THEN
CN         CALL SETMRK(L)
         CALL SETMRK(L,0.14*FACTS,0.14*FACTS,0.0,0.0)
         CALL MARK(X,Y)
      ENDIF
C
      DO 100 N=2,NPT
         X=(XARY(1,N)-XMIN)/DELX
         Y=(YARY(1,N)-YMIN)/DELY
         IF(K.GE.0) CALL DRAW(X,Y)
         IF(K.NE.0) THEN
            IF(MOD(N-1,ABS(K)).EQ.0) CALL MARK(X,Y)
         ENDIF
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE AXIS(X,Y,NB,N,W,THETA,FMIN,DELTA)
C
      COMMON /GSAFPL/ FACTS
      CHARACTER NB*256
      DATA DEG/0.0174533/
C
      DELX=COS(THETA*DEG)
      DELY=SIN(THETA*DEG)
C
      CALL MOVE(X,Y)
      CALL DRAW(X+W*DELX,Y+W*DELY)
C
      NW=W+0.000001
C
      IF(N.GE.0) THEN
         SG= 0.2
      ELSE
         SG=-0.2
      ENDIF
      DO 100 I=0,NW
         XX=X+I*DELX
         YY=Y+I*DELY
         CALL MOVE(XX,YY)
         XX=XX-SG*DELY
         YY=YY+SG*DELX
         CALL DRAW(XX,YY)
  100 CONTINUE
C
      DL=LOG10(DELTA*W)
      IF(DL.GE.0.0) THEN
         IDL=DL
      ELSE
         IDL=DL-0.999999
      ENDIF
      IF(IDL.GE.-1.AND.IDL.LE.0) IDL=0
      FDL=10.0**IDL
      IF(N.GE.0) THEN
         SG= 0.3
      ELSE
         SG=-0.3-0.21
      ENDIF
      DO 200 I=0,NW,2
         XX=X+(I-0.21*FACTS*1.5)*DELX-SG*DELY
         YY=Y+(I-0.21*FACTS*1.5)*DELY+SG*DELX
         FF=(FMIN+I*DELTA)/FDL
         CALL NUMBER(XX,YY,0.21,FF,THETA,2)
  200 CONTINUE
C
      IF(N.GE.0) THEN
         SG= 0.72
      ELSE
         SG=-0.72-0.28
      ENDIF
      XX=X+(0.5*W-0.28*ABS(N)/2)*DELX-SG*DELY
      YY=Y+(0.5*W-0.28*ABS(N)/2)*DELY+SG*DELX
      CALL SYMBOL(XX,YY,0.28,NB,THETA,ABS(N))
C
      IF(IDL.EQ.0) RETURN
C
      XX=XX+0.28*(ABS(N)+3)*DELX
      YY=YY+0.28*(ABS(N)+3)*DELY
      CALL SYMBOL(XX,YY,0.28,'10',THETA,2)
C
      SG= 0.2
      XX=XX+2*0.28*DELX-SG*DELY
      YY=YY+2*0.28*DELY+SG*DELX
      CALL NUMBER(XX,YY,0.21,IDL+0.0,THETA,0)
C
      RETURN
      END
C
      SUBROUTINE FACTOR(FACT)
C
      COMMON /GSAFPL/ FACTS
C
      F=FACT/FACTS
      CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      CALL SETVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN/F,XMAX/F,YMIN/F,YMAX/F)
      CALL INQMRK(IMRK,HMRK,WMRK,ANGL,TILT)
      CALL SETMRK(IMRK,HMRK/F,WMRK/F,ANGL,TILT)
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      CALL SETCHR(CHH/F,CHW/F,CHSP/F,ANGL,TILT)
      FACTS=FACT
      RETURN
      END
C
      SUBROUTINE WHERE(RX,RY,FACT)
C
      COMMON /GSAFPL/ FACTS
C
      CALL INQPOS(RX,RY)
      FACT=FACTS
      RETURN
      END
C
      SUBROUTINE NEWPEN(IPN)
C
      IF(IPN.GE.0) THEN
         CALL SETLIN(-1,-1,IPN)
      ELSE
         CALL SETLIN(-1,-IPN,-1)
      ENDIF
      RETURN
      END
C
      SUBROUTINE WAKU(WX,WY,IPN1,IPN2)
C
      X=WX
      X=WY
      I=IPN1
      I=IPN2
      RETURN
      END

