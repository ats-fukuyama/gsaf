C
C     ***************************************************
C     ****** GSAF BASIC ROUTINES V3.5 : INTERNAL 2 ******
C     ***************************************************
C
      SUBROUTINE GS_GRCHAR(IX,IY,IC,IND)
C
      IMPLICIT LOGICAL(L)
      INTEGER MAXCHR, MAXBUF
      PARAMETER (MAXCHR=2000)
      PARAMETER (MAXBUF=38000)
C
      INTEGER   INDEX(0:MAXCHR-1),NC1,NC2
      INTEGER*2 BUFFER(MAXBUF)
      COMMON /GSPGC1/ BUFFER,INDEX,NC1,NC2
      COMMON /GSPGC2/ TFXX,TFXY,TFYX,TFYY,TFLX,TFLY
      COMMON /GSPGC3/ TFSP,TFSPX,TFSPY
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFFN/ IFNTR,LSOFT
C
      INTEGER XYGRID(400)
      LOGICAL LMV
C
      CALL GS_GRCONV(IC,IFNTR,NC)
C
      IF (NC.LT.NC1.OR.NC.GT.NC2) RETURN
C
      ILOC=INDEX(NC)
      IF(ILOC.EQ.0) RETURN
C
      XYGRID(1)=BUFFER(ILOC)
      I=0
  100 CONTINUE
         I=I+2
         ILOC = ILOC + 1
         IDAT=BUFFER(ILOC)
         XYGRID(I  )=IDAT/128-64
         XYGRID(I+1)=MOD(IDAT,128)-64
      IF(XYGRID(I+1).NE.-64) GOTO 100
C
      ILEN=I+1
C
C      IYMIN=XYGRID(1)
      IYBAS=XYGRID(2)
      IYMAX=XYGRID(3)
      IXMIN=XYGRID(4)
C      IXMAX=XYGRID(5)
C
      DL=TFLY/(IYMAX-IYBAS)
      IF(IND.EQ.0) THEN
         IX0=IXMIN
         IY0=IYBAS
      ELSE
         IX0=0
         IY0=0
      ENDIF
C
      LMV=.TRUE.
      DO I=6,ILEN,2
         IF(XYGRID(I).EQ.-64) THEN
            LMV=.TRUE.
         ELSE
            XF=(XYGRID(I  )-IX0)*DL
            YF=(XYGRID(I+1)-IY0)*DL
            X=TFXX*XF+TFXY*YF
            Y=TFYX*XF+TFYY*YF
            IXT=IX+NINT(X*DSIZE)
            IYT=IY+NINT(Y*DSIZE)
            IF(IXT.LT.0)     IXT=0
            IF(IXT.GT.32767) IXT=32767
            IF(IYT.LT.0)     IYT=0
            IF(IYT.GT.32767) IYT=32767
            IF(LMV) THEN
               CALL DVMOVE(IXT,IYT)
            ELSE
               CALL DVDRAW(IXT,IYT)
            ENDIF
            LMV=.FALSE.
         ENDIF
      ENDDO
      CALL GS_GRSIZE(IC,DLX)
      X=TFXX*DLX+TFSPX
      Y=TFYX*DLX+TFSPY
      IX=IX+NINT(X*DSIZE)
      IY=IY+NINT(Y*DSIZE)
C
      RETURN
      END
C
      SUBROUTINE GS_GRSIZE(IC,DLX)
C
      IMPLICIT LOGICAL(L)
      INTEGER MAXCHR, MAXBUF
      PARAMETER (MAXCHR=2000)
      PARAMETER (MAXBUF=38000)
C
      INTEGER   INDEX(0:MAXCHR-1),NC1,NC2
      INTEGER*2 BUFFER(MAXBUF)
      COMMON /GSPGC1/ BUFFER,INDEX,NC1,NC2
      COMMON /GSPGC2/ TFXX,TFXY,TFYX,TFYY,TFLX,TFLY
      COMMON /GSAFS7/ IFNTS
      COMMON /GSAFFW/ IWPSFN(32:127,9) 
C
      INTEGER XYGRID(5)
C
      IF(IFNTS.LE.1) THEN
         DLX=TFLY
      ELSEIF(IFNTS.LT.32) THEN
         CALL GS_GRCONV(IC,IFNTS,NC)
         IF (NC.LT.NC1.OR.NC.GT.NC2) THEN
            DLX=0.0
         ELSE
            ILOC=INDEX(NC)
            IF(ILOC.EQ.0) THEN
               DLX=0.0
            ELSE
               XYGRID(1)=BUFFER(ILOC)
               I=0
               I=I+2
               ILOC = ILOC + 1
               IDAT=BUFFER(ILOC)
               XYGRID(I  )=IDAT/128-64
               XYGRID(I+1)=MOD(IDAT,128)-64
               I=I+2
               ILOC = ILOC + 1
               IDAT=BUFFER(ILOC)
               XYGRID(I  )=IDAT/128-64
               XYGRID(I+1)=MOD(IDAT,128)-64
C     
               IYBAS=XYGRID(2)
               IYMAX=XYGRID(3)
               IXMIN=XYGRID(4)
               IXMAX=XYGRID(5)
C     
               DL=TFLY/(IYMAX-IYBAS)
               DLX=(IXMAX-IXMIN)*DL
            ENDIF
         ENDIF
      ELSEIF(IFNTS.LE.44) THEN
         IF(IC.LT.32) THEN
            DLX=0.0
         ELSE
            IF(IFNTS.LT.40) THEN
               ID=IFNTS-31
               DLX=IWPSFN(IC,ID)/600.0*TFLY*(8.0/9.0)
            ELSEIF(IFNTS.LT.44) THEN
               DLX=TFLY
            ELSE
               ID=IFNTS-35
               DLX=IWPSFN(IC,ID)/600.0*TFLY*(8.0/9.0)
            ENDIF
         ENDIF
      ELSE
         DLX=0.0
      ENDIF
      RETURN
      END
C
      SUBROUTINE GS_GRCONV(IC,ID,NC)
C
      DIMENSION IA1(0:127),IA2(0:127),IA3(0:127)
      DATA IA1/
     &   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     & 400,214,228,375,219,371,218,217,221,222,223,225,211,224,210,220,
     & 200,201,202,203,204,205,206,207,208,209,212,213,325,226,326,215,
     & 373,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
     &  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,323,229,324,347,350,
     & 216,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
     & 116,117,118,119,120,121,122,123,124,125,126,341,329,342,346,400/
      DATA IA2/
     &   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     & 400,214,217,375,274,371,272,251,221,222,219,232,211,231,210,220,
     & 200,201,202,203,204,205,206,207,208,209,212,213,325,238,326,215,
     & 373,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
     &  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,323,218,324,347,350,
     & 252,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
     & 116,117,118,119,120,121,122,123,124,125,126,341,329,342,346,400/
      DATA IA3/
     &   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     & 400,214,217,375,274,371,272,251,221,222,219,232,211,231,210,220,
     & 200,201,202,203,204,205,206,207,208,209,212,213,325,238,326,215,
     & 373,  1,  2, 22,  4,  5, 21,  3,  7,  9,108, 10, 11, 12, 13, 15,
     &  16,  8, 17, 18, 19, 20,161, 24, 14, 23,  6,151,152,153,154,155,
     & 252,101,102,122,104,105,160,103,107,109,121,110,111,112,113,115,
     & 116,159,117,118,119,120,158,124,114,123,106,165,166,167,168,169/
C
      IF(ID.EQ.2) THEN
         ND=IA2(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC=     ND
         ELSEIF(MD.EQ.1) THEN
            NC= 100+ND
         ELSEIF(MD.EQ.2) THEN
            NC= 200+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.3) THEN
         ND=IA2(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0.AND.ND.NE.0) THEN
            NC=  50+ND
         ELSEIF(MD.EQ.1) THEN
            NC= 150+ND
         ELSEIF(MD.EQ.2) THEN
            NC= 200+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSEIF(MD.EQ.4) THEN
            NC= 199
         ELSE
            NC= 0
         ENDIF
      ELSEIF(ID.EQ.4) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC=1000+ND
         ELSEIF(MD.EQ.1) THEN
            NC=1100+ND
         ELSEIF(MD.EQ.2) THEN
            NC=1200+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.5) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC=1050+ND
         ELSEIF(MD.EQ.1) THEN
            NC=1150+ND
         ELSEIF(MD.EQ.2) THEN
            NC=1250+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.6) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC= 500+ND
         ELSEIF(MD.EQ.1) THEN
            NC= 600+ND
         ELSEIF(MD.EQ.2) THEN
            NC= 700+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.7) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC= 550+ND
         ELSEIF(MD.EQ.1) THEN
            NC= 650+ND
         ELSEIF(MD.EQ.2) THEN
            NC= 750+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.8) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC=1300+ND
         ELSEIF(MD.EQ.1) THEN
            NC=1400+ND
         ELSEIF(MD.EQ.2) THEN
            NC=1700+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.9) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC=1500+ND
         ELSEIF(MD.EQ.1) THEN
            NC=1600+ND
         ELSEIF(MD.EQ.2) THEN
            NC=1700+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.10) THEN
         ND=IA1(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0) THEN
            NC=1800+ND
         ELSEIF(MD.EQ.1) THEN
            NC=1900+ND
         ELSEIF(MD.EQ.2) THEN
            NC=1700+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSE
            NC= 199
         ENDIF
      ELSEIF(ID.EQ.11) THEN
         ND=IA3(IC)
         MD=ND/100
         ND=MOD(ND,100)
         IF(MD.EQ.0.AND.ND.NE.0) THEN
            NC=  26+ND
         ELSEIF(MD.EQ.1) THEN
            NC= 126+ND 
         ELSEIF(MD.EQ.2) THEN
            NC= 200+ND
         ELSEIF(MD.EQ.3) THEN
            NC= 200+ND
         ELSEIF(MD.EQ.4) THEN
            NC= 199
         ELSE
            NC= 0
         ENDIF
      ELSEIF(ID.EQ.12) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.65) THEN
            NC= 1300+IC-32
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.13) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.65) THEN
            NC= 1400+IC-32
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.14) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.65) THEN
            NC= 800+IC-32
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.15) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.65) THEN
            NC= 900+IC-32
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.16) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.45) THEN
            NC= 400+IC-32
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.17) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.128) THEN
            NC= 200+IC
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.18) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.65) THEN
            NC= 300+IC-32
         ELSE
            NC=0
         ENDIF
      ELSEIF(ID.EQ.19) THEN
         IF(IC.EQ.32) THEN
            NC=199
         ELSEIF(IC.GT.32.AND.IC.LT.65) THEN
            NC= 300+IC-32+50
         ELSE
            NC=0
         ENDIF
      ENDIF
C
      RETURN
      END
