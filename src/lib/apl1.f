C     $Id$
C
C     **************************************************
C     ****** GSAF APPLICATION V3.5 : TEXT,PATTERN ******
C     **************************************************
C
C     ****** DRAW DELIMITED TEXT ******
C
      SUBROUTINE TEXTX(KTEXTX)
C
      CHARACTER KTEXTX*256,KTEXT*254
C
      CALL GUDSTR(KTEXTX,256,KTEXT,NTEXT)
      CALL TEXT(KTEXT,NTEXT)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED TEXT ******
C
      SUBROUTINE GTEXT(X,Y,KTEXT,NCHAR,IJUST)
C
      CHARACTER KTEXT*256
      DATA DEG/0.0174533/
C
      CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      CALL INQ_PAGE(PXOFFSET,PYOFFSET,PXSCALE,PYSCALE)
      FACX=(XMAX-XMIN)/(PXMAX-PXMIN)*PXSCALE
      FACY=(YMAX-YMIN)/(PYMAX-PYMIN)*PYSCALE
      CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
      IF(IJUST.EQ.0) THEN
         DEL=0.0
      ELSEIF(IJUST.EQ.1) THEN
         CALL INQTSZ(KTEXT,NCHAR,SIZE)
         DEL=-SIZE
      ELSEIF(IJUST.EQ.2) THEN
         CALL INQTSZ(KTEXT,NCHAR,SIZE)
         DEL=-0.5*SIZE
      ELSEIF(IJUST.EQ.3) THEN
         I=0
    1    CONTINUE
            I=I+1
         IF(KTEXT(I:I).NE.'.'.AND.I.NE.NCHAR) GOTO 1
         IF(KTEXT(I:I).EQ.'.') THEN
            CALL INQTSZ(KTEXT,I-1,SIZE1)
            CALL INQTSZ(KTEXT,I,SIZE2)
            DEL=-0.5*(SIZE1+SIZE2)
         ELSE
            CALL INQTSZ(KTEXT,NCHAR,SIZE)
            DEL=-0.5*SIZE
         ENDIF
      ELSE
         DEL=0.0
      ENDIF
C
      XX=X+DEL*COS(ANGL*DEG)*FACX
      YY=Y+DEL*SIN(ANGL*DEG)*FACY
      CALL MOVE(XX,YY)
      CALL TEXT(KTEXT,NCHAR)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED DELIMITTED TEXT ******
C
      SUBROUTINE GTEXTX(X,Y,KTEXTX,IJUST)
C
      CHARACTER KTEXTX*256,KTEXT*254
C
      CALL GUDSTR(KTEXTX,256,KTEXT,NTEXT)
      CALL GTEXT(X,Y,KTEXT,NTEXT,IJUST)
      RETURN
      END
C
C     ****** DRAW INTEGER NUMBER ******
C
      SUBROUTINE NUMBI(I,KFORM,NCHAR)
C
      CHARACTER KFORM*16,KSEND*24
C
      WRITE(KSEND,KFORM) I
      CALL TEXT(KSEND,NCHAR)
      RETURN
      END
C
C     ****** DRAW DOUBLE PRECISION REAL NUMBER ******
C
      SUBROUTINE NUMBD(D,KFORM,NCHAR)
C
      REAL*8 D
      CHARACTER KFORM*16,KSEND*24
C
      WRITE(KSEND,KFORM) D
      N=0
    1 CONTINUE
         N=N+1
      IF(KSEND(N:N).EQ.' ') GOTO 1
C
      IF(KSEND(N:N).EQ.'-') THEN
         IF(N.GT.1.AND.KSEND(N+1:N+1).EQ.'.') THEN
            KSEND(N-1:N-1)='-'
            KSEND(N  :N  )='0'
         ENDIF
      ELSEIF(KSEND(N:N).EQ.'.') THEN
         IF(N.GT.1) THEN
            KSEND(N-1:N-1)='0'
         ENDIF
      ENDIF
C
      CALL TEXT(KSEND,NCHAR)
      RETURN
      END
C
C     ****** DRAW REAL NUMBER ******
C
      SUBROUTINE NUMBR(R,KFORM,NCHAR)
C
      REAL*8 D
      CHARACTER KFORM*16
C
      D=DBLE(R)
      CALL NUMBD(D,KFORM,NCHAR)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED INTEGER NUMBER ******
C
      SUBROUTINE GNUMBI(X,Y,I,IJUST)
C
      CHARACTER KVALUE*25,KTEXT*25
C
      WRITE(KVALUE,'(I25)') I
C
      N=0
    1 CONTINUE
         N=N+1
      IF(KVALUE(N:N).EQ.' ') GOTO 1
      KTEXT=KVALUE(N:25)
      NCHAR=26-N
      CALL GTEXT(X,Y,KTEXT,NCHAR,IJUST)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED DOUBLE PRECISION REAL NUMBER ******
C
      SUBROUTINE GNUMBD(X,Y,D,IND,IJUST)
C
      REAL*8 D
      CHARACTER KFORM*10,KVALUE*25,KTEXT*25
C
      IF(IND.GT.0) THEN
         WRITE(KFORM,801) IND
  801    FORMAT('(F25.',I2,')')
         WRITE(KVALUE,KFORM) D
      ELSEIF(IND.EQ.0) THEN
         KFORM='(I25)'
         WRITE(KVALUE,KFORM) NINT(D)
      ELSEIF(IND.GT.-100) THEN
         WRITE(KFORM,802) -IND
  802    FORMAT('(1PE25.',I2,')')
         WRITE(KVALUE,KFORM) D
      ELSE
         WRITE(KFORM,803) -IND-100
  803    FORMAT('(1PE25.',I2,')')
         WRITE(KVALUE,KFORM) D
      ENDIF
C
      N=0
    1 CONTINUE
         N=N+1
      IF(KVALUE(N:N).EQ.' ') GOTO 1
C
      IF(KVALUE(N:N).EQ.'-') THEN
         IF(N.GT.1.AND.KVALUE(N+1:N+1).EQ.'.') THEN
            KVALUE(N-1:N-1)='-'
            KVALUE(N  :N  )='0'
            N=N-1
         ENDIF
      ELSEIF(KVALUE(N:N).EQ.'.') THEN
         IF(N.GT.1) THEN
            KVALUE(N-1:N-1)='0'
            N=N-1
         ENDIF
      ENDIF
C
      IF(IND.GT.-100) THEN
         KTEXT=KVALUE(N:25)
         NCHAR=26-N
      ELSE
         M=N
    2    CONTINUE
            M=M+1
         IF(KVALUE(M:M).NE.'E') GOTO 2
         NN=M-1
         IF(IND.EQ.-100) NN=NN-1
C
         M=M+1
         IF(KVALUE(M:M).EQ.'-') THEN
            IS=-1
         ELSEIF(KVALUE(M:M).EQ.'+') THEN
            IS=1
         ELSE
            IS=1
            M=M-1
         ENDIF
    3    CONTINUE
            M=M+1
         IF(M.LE.24.AND.KVALUE(M:M).EQ.'0') GOTO 3
         IF(IS.EQ.1) THEN
            KTEXT=KVALUE(N:NN)//'$;$0x$#10$+'//KVALUE(M:25)//'$='
            NCHAR=NN-N+1 + 11 + 25-M+1 + 2
         ELSE
            KTEXT=KVALUE(N:NN)//'$;$0x$#10$+-'//KVALUE(M:25)//'$='
            NCHAR=NN-N+1 + 12 + 25-M+1 + 2
         ENDIF
      ENDIF
      CALL GTEXT(X,Y,KTEXT,NCHAR,IJUST)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED REAL NUMBER ******
C
      SUBROUTINE GNUMBR(X,Y,R,IND,IJUST)
C
      REAL*8 D
C
      D=DBLE(R)
      CALL GNUMBD(X,Y,D,IND,IJUST)
      RETURN
      END
C
C     ****** DRAW JUSTIFIED INTEGER POWER ******
C
      SUBROUTINE GNUMBP(X,Y,I,J,IJUST)
C
      CHARACTER KVALUE*25,KPOWER*25,KTEXT*25
C
      WRITE(KVALUE,'(I25)') I
      N=0
    1 CONTINUE
         N=N+1
      IF(KVALUE(N:N).EQ.' ') GOTO 1
C
      WRITE(KPOWER,'(I25)') J
      M=0
    2 CONTINUE
         M=M+1
      IF(KPOWER(M:M).EQ.' ') GOTO 2
C
      KTEXT=KVALUE(N:25)//'$+'//KPOWER(M:25)//'$='
      NCHAR=26-N+2+26-M+2
      CALL GTEXT(X,Y,KTEXT,NCHAR,IJUST)
      RETURN
      END
C
C     ****** DRAW PATTERN LINE ******
C
      SUBROUTINE DRAWPT(X,Y)
C
      COMMON /GSAPPT/ PAT(11),XT,YT,VT,NT,NPATT
C
      IF(NPATT.LE.1) THEN
         CALL DRAW(X,Y)
      ELSE
         IF(MOD(NT,2).EQ.1) CALL MOVE(XT,YT)
         DX=X-XT
         DY=Y-YT
         DV=SQRT(DX*DX+DY*DY)
         V=VT+DV
C
   10    CONTINUE
            IF(V.GT.PAT(NPATT)) THEN
               NE=NPATT
            ELSE
               DO 20 N=1,NPATT
                  IF(V.LE.PAT(N)) GOTO 21
   20          CONTINUE
   21          NE=N-1
            ENDIF
            DO 30 N=NT,NE
               X1=XT+DX*(PAT(N)-VT)/DV
               Y1=YT+DY*(PAT(N)-VT)/DV
               IF(MOD(N,2).EQ.0) THEN
                  CALL MOVE(X1,Y1)
               ELSE
                  CALL DRAW(X1,Y1)
               ENDIF
   30       CONTINUE
            IF(NE.NE.NPATT) GOTO 40
            XT=X1
            YT=Y1
            V=V-PAT(NPATT)
            VT=0.
            NT=1
         GOTO 10
C
   40    IF(MOD(NE,2).EQ.0) CALL DRAW(X,Y)
         XT=X
         YT=Y
         VT=V
         NT=NE+1
      ENDIF
      RETURN
      END
C
C     ****** INITIALIZE PATTERN LINE ******
C
      SUBROUTINE MOVEPT(X,Y,IPAT)
C
      COMMON /GSAPPT/ PAT(11),XT,YT,VT,NT,NPATT
      COMMON /GSAPPS/ NPATS(15),PATS(10,15)
      DIMENSION NPATSS(15),PATSS(10,15)
      DATA NPATSS/2,2,2,4,4,6,6,8*0/
      DATA PATSS/0.05, 0.20, 8*0.,
     &          0.25, 0.4,  8*0.,
     &          0.6,  0.8,  8*0.,
     &          0.5,  0.6,  0.7,  0.8,  6*0.,
     &          1.15, 1.3,  1.45, 1.60, 6*0.,
     &          0.425,0.500,0.575,0.650,0.725,0.800,4*0.,
     &          1.1,  1.2,  1.3,  1.4,  1.5,  1.6,  84*0./
      DATA INIT/0/
C
      IF(INIT.EQ.0) THEN
         DO 10 I=1,15
            NPATS(I)=NPATSS(I)
         DO 10 N=1,10
            PATS(N,I)=PATSS(N,I)
   10    CONTINUE
         INIT=1
      ENDIF
C
      IF(IPAT.GE.1.AND.IPAT.LE.15) THEN
         NPATT=NPATS(IPAT)
         DO 1010 N=1,NPATT
            PAT(N)=PATS(N,IPAT)
 1010    CONTINUE
      ELSE IF(IPAT.LE.-1.AND.IPAT.GE.-15) THEN
         NPATT=NPATS(-IPAT)+1
         DO 1020 N=1,NPATT-1
            PAT(N)=PATS(NPATT-1,-IPAT)-PATS(NPATT-N,-IPAT)
 1020    CONTINUE
         PAT(NPATT)=PATS(NPATT-1,-IPAT)
      ELSE IF(IPAT.EQ.0) THEN
         NPATT=1
      ENDIF
C
      IF(NPATT.EQ.1) THEN
         CALL MOVE(X,Y)
      ELSE
         XT=X
         YT=Y
         VT=0.
         NT=1
      ENDIF
      RETURN
      END
C
C     ****** DEFINE LINE PATTERN ******
C
      SUBROUTINE SETLPT(IPAT,NPAT,PAT)
C
      COMMON /GSAPPS/ NPATS(15),PATS(10,15)
      DIMENSION PAT(10)
C
      IF(IPAT.LT.8.OR.IPAT.GT.15) RETURN
      IF(NPAT.LT.0) RETURN
C
      NPATS(IPAT)=MIN(NPAT,10)
      PATSUM=0.
      DO 2010 N=1,NPATS(IPAT)
         PATSUM=PATSUM+PAT(N)
         PATS(N,IPAT)=PATSUM
 2010 CONTINUE
      RETURN
      END
C
C     ****** FIND MINIMUM AND MAXMUM OF 1D ARRAY******
C
      SUBROUTINE GMNMX1(A,NS,NE,NSTEP,AMIN,AMAX)
C
      DIMENSION A(*)
C
      AMIN=A(NS)
      AMAX=A(NS)
      DO 100 N=NS+NSTEP,NE,NSTEP
         AMIN=MIN(AMIN,A(N))
         AMAX=MAX(AMAX,A(N))
  100 CONTINUE
      RETURN
      END
C
C     ****** FIND MINIMUM AND MAXMUM OF 2D ARRAY******
C
      SUBROUTINE GMNMX2(A,NXA,NXS,NXE,NXSTEP,
     &                        NYS,NYE,NYSTEP,AMIN,AMAX)
C
      DIMENSION A(NXA,*)
C
      AMIN=A(NXS,NYS)
      AMAX=A(NXS,NYS)
      DO 100 NX=NXS,NXE,NXSTEP
      DO 100 NY=NYS,NYE,NYSTEP
         AMIN=MIN(AMIN,A(NX,NY))
         AMAX=MAX(AMAX,A(NX,NY))
  100 CONTINUE
      RETURN
      END
C
C     ****** CHOOSE GRAPH MINIMUM, MAXMUM AND STEP ******
C
      SUBROUTINE GQSCAL(AMIN,AMAX,GMIN,GMAX,SCALE)
C
      REAL*8 DW,DLW
      DATA EPS/0.01/
C
      DW=DBLE(AMAX)-DBLE(AMIN)
      IF(DW.LE.0.D0) THEN
         IF(DW.LT.0.D0) THEN
            GMIN=AMAX
            GMAX=AMIN
            DW=-DW
         ELSEIF(ABS(AMIN).GT.0.0) THEN
            GMIN=AMIN-ABS(AMIN)
            GMAX=AMIN+ABS(AMIN)
            DW=2.D0*ABS(DBLE(AMIN))
         ELSE
            GMIN=-1.0
            GMAX= 1.0
            DW=2.D0
         ENDIF
      ELSE
         GMIN=AMIN
         GMAX=AMAX
      ENDIF
C
      DLW=DLOG10(DW)
      NLW=INT(DLW+DBLE(EPS))
      IF(DLW+DBLE(EPS).LT.0.D0) NLW=NLW-1
      RAW=SNGL(DW*10.D0**(-NLW))
C
      IF(RAW.GT.6.0) THEN
         SCALE=1.00*10.0**NLW
      ELSEIF(RAW.GT.3.0) THEN
         SCALE=0.50*10.0**NLW
      ELSEIF(RAW.GT.2.0) THEN
         SCALE=0.25*10.0**NLW
      ELSEIF(RAW.GT.1.2) THEN
         SCALE=0.20*10.0**NLW
      ELSE
         SCALE=0.10*10.0**NLW
      ENDIF
C
      IF(GMAX.GE.0.0) THEN
         IF(GMIN.LE.0.0) THEN
            NS=INT( GMAX/SCALE-EPS+1.0)
            GMAX= SCALE*NS
            NS=INT(-GMIN/SCALE-EPS+1.0)
            GMIN=-SCALE*NS
         ELSE
            NS=INT( GMAX/SCALE-EPS+1.0)
            GMAX= SCALE*NS
            NS=INT( GMIN/SCALE+EPS)
            GMIN= SCALE*NS
         ENDIF
      ELSE
         NS=INT(-GMAX/SCALE+EPS)
         GMAX=-SCALE*NS
         NS=INT(-GMIN/SCALE-EPS+1.0)
         GMIN=-SCALE*NS
      ENDIF
      RETURN
      END
