C     $Id$
C
C     *******************************
C     ****** PAGE TEST PROGRAM ******
C     *******************************
C
      DIMENSION NPAGEA(256)
      CHARACTER KMENU*1,FLNAME*40
C
      CALL GSOPEN
C
      XMIN=0.0
      XMAX=25.6
      YMIN=0.0
      YMAX=18.1
      XCMIN=0.0
      XCMAX=25.6
      YCMIN=0.0
      YCMAX=18.1
C
    1 WRITE(6,*) '# INPUT : 1,2,3,4,5,6  E/END'
      READ(5,'(A1)',END=9000,ERR=1) KMENU
      CALL GUCPTL(KMENU)
C
      IF(KMENU.EQ.'1') THEN
         CALL PAGES
         CALL SET_PAGE(0.0,9.0,0.5,0.5)
         CALL XMARK
         CALL SET_PAGE(0.0,0.0,0.5,0.5)
         CALL XTEXT
         CALL SET_PAGE(12.8,9.0,0.5,0.5)
         CALL XLINE
         CALL SET_PAGE(12.8,0.0,0.5,0.5)
         CALL XCHAR('S')
         CALL PAGEE
      ELSEIF(KMENU.EQ.'2') THEN
         CALL PAGES
         CALL SET_PAGE(0.0,13.5,1.0,0.25)
         CALL XMARK
         CALL SET_PAGE(0.0, 9.0,1.0,0.25)
         CALL XTEXT
         CALL SET_PAGE(0.0, 4.5,1.0,0.25)
         CALL XLINE
         CALL SET_PAGE(0.0, 0.0,1.0,0.25)
         CALL XCHAR('S')
         CALL PAGEE
      ELSEIF(KMENU.EQ.'3') THEN
         CALL PAGES
         CALL SET_PAGE( 0.0, 0.0,0.25,1.0)
         CALL XMARK
         CALL SET_PAGE( 6.4, 0.0,0.25,1.0)
         CALL XTEXT
         CALL SET_PAGE(12.8, 0.0,0.25,1.0)
         CALL XLINE
         CALL SET_PAGE(19.2, 0.0,0.25,1.0)
         CALL XCHAR('S')
         CALL PAGEE
      ELSEIF(KMENU.EQ.'4') THEN
         CALL PAGES
         CALL SET_PAGE(0.0,9.0,0.5,0.5)
         CALL XMARK
         CALL SET_PAGE(0.0,0.0,0.5,0.5)
         CALL XCHAR('S')
         CALL SET_PAGE(12.8,9.0,0.5,0.5)
         CALL XCHAR('H')
         CALL SET_PAGE(12.8,0.0,0.5,0.5)
         CALL XCHAR('F')
         CALL PAGEE
      ELSEIF(KMENU.EQ.'5') THEN
         CALL PAGES
         CALL SET_PAGE(0.0,13.5,1.0,0.25)
         CALL XMARK
         CALL SET_PAGE(0.0, 9.0,1.0,0.25)
         CALL XCHAR('S')
         CALL SET_PAGE(0.0, 4.5,1.0,0.25)
         CALL XCHAR('H')
         CALL SET_PAGE(0.0, 0.0,1.0,0.25)
         CALL XCHAR('F')
         CALL PAGEE
      ELSEIF(KMENU.EQ.'6') THEN
         CALL PAGES
         CALL SET_PAGE( 0.0, 0.0,0.25,1.0)
         CALL XMARK
         CALL SET_PAGE( 6.4, 0.0,0.25,1.0)
         CALL XCHAR('S')
         CALL SET_PAGE(12.8, 0.0,0.25,1.0)
         CALL XCHAR('H')
         CALL SET_PAGE(19.2, 0.0,0.25,1.0)
         CALL XCHAR('F')
         CALL PAGEE
      ELSEIF(KMENU.EQ.'E') THEN
         GOTO 9000
      ENDIF
      GOTO 1
C
 9000 CALL GSCLOS
      STOP
      END
C
      SUBROUTINE XMARK
C
      DEG=3.141593/180.
      R1=1.0
      R2=8.0
      DO 1000 IDO=-31,31
         I=IDO
         ANGL=180.*FLOAT(I)/32.
         CALL SETMRK(I,0.6,0.6,ANGL-90.0,0.0)
         CA=COS(ANGL*DEG)
         SA=SIN(ANGL*DEG)
         CALL MOVE(R1*CA+12.8,R1*SA+9.0)
         CALL MARK(R2*CA+12.8,R2*SA+9.0)
 1000 CONTINUE
      RETURN
      END
C
      SUBROUTINE XTEXT
C
      CALL SETCHH(0.4,0.0)
      DO 100 I=2,7
      DO 100 J=0,15
         X=J+5.0
         Y=18.0-I
         CALL MOVE(X,Y)
         CALL TEXT(CHAR(16*I+J),1)
  100 CONTINUE
      CALL SETCHS(0.4,0.0)
      DO 200 I=0,7
      DO 200 J=0,15
         X=J+5.0
         Y=9.0-I
         CALL MOVE(X,Y)
         CALL TEXT(CHAR(16*I+J),1)
  200 CONTINUE
      RETURN
      END
C
      SUBROUTINE XLINE
C
      DO 100 ILNDO=0,7
         ILN=ILNDO
      DO 100 IBLDO=0,4
         IBL=IBLDO
      DO 100 ICLDO=0,7
         ICL=ICLDO
         X=2*ICL+5
         Y=2*ILN+0.3*IBL+1
         CALL SETLIN(ILN,IBL,ICL)
         CALL MOVE(X,Y)
         CALL DRAW(X+1.5,Y)
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE XCHAR(KID)
C
      CHARACTER KID*1
C
      IF(KID.EQ.'S') THEN
         CALL SETFNT(0)
      ELSEIF(KID.EQ.'H') THEN
         CALL SETFNT(1)
      ELSE
         CALL SETFNT(2)
      ENDIF
C
      DO 100 I=1,13
         ANGL=30*(I-1)
         CALL SETCHR(0.6,0.4,0.6,ANGL,0.0)
         X=I+6.0
         Y=15.0
         CALL MOVE(X,Y)
         CALL TEXT('A',1)
  100 CONTINUE
C
      DO 200 I=1,13
         TILT=8*(I-7)
         CALL SETCHR(0.6,0.4,0.6,0.0,TILT)
         X=I+6.0
         Y=13.0
         CALL MOVE(X,Y)
         CALL TEXT('A',1)
  200 CONTINUE
C
      DO 300 I=1,20
         CHH=0.05*(I-1)
         CALL SETCHR(CHH,0.6666667,0.6,0.0,0.0)
         X=I+3.0
         Y=11.0
         CALL MOVE(X,Y)
         CALL TEXT('A',1)
  300 CONTINUE
C
      DO 400 I=1,20
         CHW=0.05*(I-1)
         CALL SETCHR(1.0,CHW*0.6666667,0.6,0.0,0.0)
         X=I+3.0
         Y=9.0
         CALL MOVE(X,Y)
         CALL TEXT('A',1)
  400 CONTINUE
C
      DO 500 I=1,20
         CHH=0.05*(I-1)
         CALL SETCHR(CHH,CHH*0.6666667,0.6,0.0,0.0)
         X=I+3.0
         Y=7.0
         CALL MOVE(X,Y)
         CALL TEXT('A',1)
  500 CONTINUE
C
      CALL SETCHR(0.6,0.4,0.6,0.0,0.0)
      CALL MOVE(3.0,5.0)
      CALL TEXT('NORMAL$$+SUPER$$=N.',19)
      CALL MOVE(14.0,5.0)
      CALL TEXT('NORMAL$+SUPER$=N.',17)
      CALL MOVE(3.0,3.5)
      CALL TEXT('NORMAL$$-SUB$$=N.',17)
      CALL MOVE(14.0,3.5)
      CALL TEXT('NORMAL$-SUB$=N.',15)
      CALL MOVE(3.0,2.0)
      CALL TEXT('NORMAL$$#abc$$#N.',17)
      CALL MOVE(14.0,2.0)
      CALL TEXT('NORMAL$#abc$#N.',15)
      CALL MOVE(3.0,0.5)
      CALL TEXT('NORMAL$$*BACK.',14)
      CALL MOVE(14.0,0.5)
      CALL TEXT('NORMAL$*BACK.',13)
      RETURN
      END
C
      SUBROUTINE XXYIN
C
      CHARACTER K*8
C
         CALL XYIN(X,Y)
         CALL MOVE(X,Y)
         CALL SETCHS(0.6,45.)
         CALL TEXT('GRAPHIC PACKAGE',15)
         CALL MOVE(X+1.,Y)
         CALL SETCHS(0.6,0.0)
         CALL TEXT('INPUT 8 CHAR',12)
         CALL MOVE(X+1.,Y-1.)
         CALL CHIN(K,8)
         CALL SETCHS(0.6,-45.0)
         CALL TEXT(K,8)
      RETURN
      END
