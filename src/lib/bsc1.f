C     $Id$C
C
C     ****************************************************
C     ****** GSAF BASIC ROUTINES V3.8 : INTERFACE 1 ******
C     ****************************************************
C
C     ****** SYSTEM INITIALIZE ******
C
      SUBROUTINE GSOPEN
C
      CALL GS_OPENX(50,51)
      RETURN
      END
C
C     ****** SYSTEM INITIALIZE ******
C
      SUBROUTINE GS_OPENX(NFCS1,NFCT1)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFFS/ NPAGES,NWORDS,NFCS,NFCT
      COMMON /GSAFLF/ LOPENS,LOPENT,LSAVES,LSAVET
      COMMON /GSAFRD/ NPAGER
      COMMON /GSGRPL/ NGRPL,NGRPM
      COMMON /GSAFMD/ ITMODE
      COMMON /GSAFTM/ TOPEN
      COMMON /GSAFBL/ LBELL
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
      CHARACTER KID*1
      DATA LGOPEN/.FALSE./
C
      IF(LGOPEN.AND.LGSAF) RETURN
C
      CALL GUDATE(NDY,NDM,NDD,NTH,NTM,NTS)
      CALL GU_SRAND(NTH*3600+NTM*60+NTS)
      CALL GUTIME(TOPEN)
      LGOPEN = .TRUE.
      LGSAF  = .TRUE.
      LPAGE  = .FALSE.
      LFIL   = .FALSE.
      LKEEP  = .FALSE.
      NPAGE  = 0
      NPAGES = 0
      NWORDS = 0
      NFCS   = NFCS1
      NFCT   = NFCT1
      LOPENS = .FALSE.
      LOPENT = .FALSE.
      LSAVES = .FALSE.
      LSAVET = .FALSE.
      NPAGER = 0
      NGRPL  = 0
      NGRPM  = 0
      ITMODE = 1
      LBELL  = .TRUE.
      PXOFFSET = 0.0
      PYOFFSET = 0.0
      PXSCALE  = 1.0
      PYSCALE  = 1.0
      CALL GSTOPN
      CALL GSTON
      CALL GSSIZE(25.6,18.1)
      CALL GSTITL('/ /')
C
      CALL DVTYPE(ICH)
C
C     ***** ICH.EQ.1: Resolution must be given *****
C
      IF(ICH.EQ.1) THEN
    1    WRITE(6,600)
  600    FORMAT(' # INPUT DISPLAY TYPE : ',
     &          '1)512x380 2)640x475 3)768x570 4)896x665 5)1024x760'/
     &          '                        ',
     &          '6)1280x950 7)xterm 8)versaterm 9)online 0)quiet)')
         READ(5,'(A1)',END=9000,ERR=1) KID
         CALL CHRASC(KID,ICH,1)
      ENDIF
C
C     ***** ICH.EQ.0: Non Interactive device *****
C     ***** ICH.LT.0: Interactive device but be quiet *****
C     ***** ICH.GT.0: Interactive device *****
C     ***** ABS(ICH): Key input *****
C
      CALL DVOPEN(ICH)
      IF(ICH.LE.0) ITMODE=0
C
      IF(ICH.NE.0) THEN
   10    CONTINUE
         ID=ABS(ICH)
         IF(ID.EQ.1) THEN
            WRITE(6,601)
            CALL DVFLSH
            READ(5,'(A1)',END=9000,ERR=10) KID
         ELSE
            CALL ASCCHR(ID,KID,1)
         ENDIF
         CALL GUCPTL(KID)
         IF(KID.EQ.'C') THEN
            GOTO 20
         ELSEIF(KID.EQ.'O') THEN
            CALL SFOPTN
         ELSEIF(KID.EQ.'F') THEN
            CALL SFFILE
            CALL GSFON
         ELSEIF(KID.EQ.'H') THEN
            CALL SFHELP
         ELSEIF(KID.EQ.'Q') THEN
            CALL GSCLOS
            STOP
         ENDIF
         GOTO 10
      ENDIF
   20 CONTINUE
      RETURN
  601 FORMAT(1H ,'# INPUT : (C)ONTINUE,(O)PTION,(F)ILE,(H)ELP,(Q)UIT')
C
 9000 STOP
      END
C
C     ****** SYSTEM TERMINATE ******
C
      SUBROUTINE GSCLOS
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFLF/ LOPENS,LOPENT,LSAVES,LSAVET
      COMMON /GSAFTM/ TOPEN
      COMMON /GSGRPL/ NGRPL,NGRPM
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE) CALL PAGEE
C
      CALL GSTCLS
      IF(LOPENS) CALL GSFCLS
      CALL DVCLOS(ICH)
      LGSAF=.FALSE.
      IF(NGRPL.NE.0) THEN
         WRITE(6,*) 'XX GSAF GROUP LEVEL ERROR: NGRPL=',NGRPL
         WRITE(6,*) '   EPS FILE MAY BE BROKEN.'
      ENDIF
      CALL GUTIME(T)
      IF(ICH.NE.0) WRITE(6,601) T-TOPEN
  601 FORMAT(1H ,'# GSAF V3.83 : ',
     &           'Copyright (C) 1983-2004 A. Fukuyama and T. Akutsu'/
     &       1H ,'#              CLOSED.  USED CPU TIME =',
     &           F10.3,' SEC')
      RETURN
      END
C
C     ****** SET PAGE SIZE ******
C
      SUBROUTINE GSSIZE(SIZEX,SIZEY)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE) RETURN
C
      IF(SIZEX.LE.0.0.OR.SIZEY.LE.0.0) GOTO 9001
      SIZEXS=SIZEX
      SIZEYS=SIZEY
      IF(25.6*SIZEY.LT.18.1*SIZEX) THEN
         DSIZE = 32768/SIZEX
      ELSE
         DSIZE = 23168/SIZEY
      ENDIF
      RETURN
C
 9001 WRITE(6,*) 'XX SIZEX AND SIZEY MUST BE POSITIVE VALUE.'
      RETURN
      END
C
C     ****** SET PAGE TITLE ******
C
      SUBROUTINE GSTITL(KTITL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFHD/ KHEAD
      CHARACTER KHEAD*40,KTITL*42
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE) RETURN
C
      KHEAD='                                        '
      CALL GUDSTR(KTITL,42,KHEAD,NHEAD)
      RETURN
      END
C
C     ****** SET OPTION ******
C
      SUBROUTINE GSOPTN(KOPTDL,IOPT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      CHARACTER KOPTDL*4,KOPT*2
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE) RETURN
C
      CALL GUDSTR(KOPTDL,4,KOPT,IKOPT)
      IF(IKOPT.EQ.0) THEN
         CALL SFOPTN
      ELSE
         CALL DVOPTN(KOPT,IOPT)
      ENDIF
      RETURN
      END
C
C     ****** PRINT GRAPHIC HARDCOPY ******
C
      SUBROUTINE GSPRNT
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE) RETURN
C
      CALL DVPRNT
      RETURN
      END
C
C     ****** PAGE INITIALIZE ******
C
      SUBROUTINE PAGES
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE) CALL PAGEE
C
      LPAGE=.TRUE.
      NPAGE=NPAGE+1
      IF(LFIL.AND..NOT.LKEEP) CALL FLPAGS
      CALL DVPAGS(NPAGE,SIZEXS,SIZEYS,LKEEP)
      CALL GF_SET_GCFUNC(3)
      PXOFFSET=0.0
      PYOFFSET=0.0
      PXSCALE= 1.0
      PYSCALE= 1.0
      CALL OFFVEW
      CALL OFFCLP
      CALL SETLIN(0,2,7)
      CALL SETCHH(0.35,0.0)
      CALL SETMKS(0,0.2)
      CALL SETFNT(0)
      CALL MOVE(0.0,0.0)
      IF(.NOT.LKEEP) CALL SFHEAD
      RETURN
      END
C
C     ****** PAGE TERMINATE ******
C
      SUBROUTINE PAGEE
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFLF/ LOPENS,LOPENT,LSAVES,LSAVET
      COMMON /GSAFBL/ LBELL
      CHARACTER*1 KID
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      IF(LBELL) CALL DVBELL
      ICH=1
      CALL DVPAGE(ICH)
C
C     ***** ICH.EQ.0: Non Interactive device *****
C     ***** ABS(ICH): Key input *****
C
C
      LPAGE=.FALSE.
      LKEEP=.FALSE.
      LANS=.FALSE.
      LSAVE=LSAVES
      IF(ICH.NE.0) THEN
         IF(ICH.GT.32) THEN
            CALL ASCCHR(ICH,KID,1)
            CALL GUCPTL(KID)
   10       IF(KID.EQ.'C') THEN
               GOTO 30
            ELSEIF(KID.EQ.'K') THEN
               LKEEP=.TRUE.
            ELSEIF(KID.EQ.'P') THEN
               CALL DVPRNT
            ELSEIF(KID.EQ.'O') THEN
               CALL SFOPTN
               LSAVE=LSAVES
            ELSEIF(KID.EQ.'F') THEN
               CALL SFFILE
               CALL GSFON
               LSAVE=LSAVES
            ELSEIF(KID.EQ.'X') THEN
               IF(LSAVES) THEN
                  CALL GSFOFF
               ELSE
                  CALL GSFON
               ENDIF
            ELSEIF(KID.EQ.'S') THEN
               IF(.NOT.LOPENS) CALL SFFILE
               LANS=.TRUE.
               LSAVE=.TRUE.
               GOTO 30
            ELSEIF(KID.EQ.'Y'.AND.LOPENS) THEN
               LANS=.TRUE.
               LSAVE=.TRUE.
               GOTO 30
            ELSEIF(KID.EQ.'N'.AND.LOPENS) THEN
               LANS=.TRUE.
               LSAVE=.FALSE.
               GOTO 30
            ELSEIF(KID.EQ.'B') THEN
               LBELL=.NOT.LBELL
            ELSEIF(KID.EQ.'H') THEN
               CALL SFHELP
            ELSEIF(KID.EQ.'Q') THEN
   18          WRITE(6,*) '# ARE YOU SURE TO QUIT ? (Y/N)'
               CALL DVFLSH
               READ(5,'(A1)',END=19,ERR=18) KID
               CALL GUCPTL(KID)
               IF(KID.EQ.'Y') THEN
                  CALL GSCLOS
                  STOP
               ENDIF
   19          CONTINUE
            ENDIF
C
   20       IF(LSAVES) THEN
               WRITE(6,*) '# INPUT : (C)ONTINUE,(F)ILE,(S)AVE,',
     &                    '(Y/N/X/B/D/K),(O)PTION,(H)ELP,(Q)UIT'
            ELSE
               WRITE(6,*) '# INPUT : (C)ONTINUE,(F)ILE,(S)AVE,',
     &                    '(X/B/D/K),(O)PTION,(H)ELP,(Q)UIT'
            ENDIF
            CALL DVFLSH
            READ(5,'(A1)',END=30,ERR=20) KID
            CALL GUCPTL(KID)
            GOTO 10
         ENDIF
C
   30    IF(LSAVES.AND..NOT.LANS) THEN
   40       WRITE(6,*) '# SAVE GRAPHIC DATA IN FILE (Y)ES OR (N)O ?'
            CALL DVFLSH
            READ(5,'(A1)',END=50,ERR=40) KID
            CALL GUCPTL(KID)
            IF(KID.EQ.'Y') THEN
               LSAVE=.TRUE.
            ELSEIF(KID.EQ.'N') THEN
               LSAVE=.FALSE.
            ELSE
               GOTO 40
            ENDIF
   50       CONTINUE
         ENDIF
      ENDIF
C
      IF(LFIL.AND..NOT.LKEEP) CALL FLPAGE
      IF(LSAVE) CALL FLSAVE(NPAGE)
C
      RETURN
      END
C
C     ****** PAGE QUICKLY TERMINATE WITH FILE SAVE ******
C
      SUBROUTINE PAGEY
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFLF/ LOPENS,LOPENT,LSAVES,LSAVET
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      ICH=0
      CALL DVPAGE(ICH)
C
      LPAGE=.FALSE.
C
      IF(LFIL) CALL FLPAGE
      IF(LSAVES) CALL FLSAVE(NPAGE)
C
      RETURN
      END
C
C     ****** PAGE QUICKLY TERMINATE WITHOUT FILE SAVE ******
C
      SUBROUTINE PAGEN
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      ICH=0
      CALL DVPAGE(ICH)
C
      LPAGE=.FALSE.
C
      IF(LFIL) CALL FLPAGE
C
      RETURN
      END
C
C     ****** SET PAGE OFFSET AND SCALE ******
C
      SUBROUTINE SETPAGE(XOFFSET,YOFFSET,XSCALE,YSCALE)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      PXOFFSET=XOFFSET
      PYOFFSET=YOFFSET
      PXSCALE=XSCALE
      PYSCALE=YSCALE
C
      CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      CALL SETVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      CALL OFFCLP
      CALL SETLIN(0,2,7)
      CALL SETCHH(0.35,0.0)
      CALL SETMKS(0,0.2)
      CALL SETFNT(0)
      CALL MOVE(0.0,0.0)
      RETURN
      END
C
C     ****** INQ PAGE OFFSET AND SCALE ******
C
      SUBROUTINE INQPAGE(XOFFSET,YOFFSET,XSCALE,YSCALE)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      XOFFSET=PXOFFSET
      YOFFSET=PYOFFSET
      XSCALE=PXSCALE
      YSCALE=PYSCALE
      RETURN
      END
C
C     ****** SET VIEW PORT ******
C
      SUBROUTINE SETVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFS2/ PRECT(4),RECT(4),CLRECT(4)
C
      IF(.NOT.LPAGE) RETURN
C
      IF(ABS( XMAX- XMIN).LT.1.E-32) GO TO 9000
      IF(ABS( YMAX- YMIN).LT.1.E-32) GO TO 9000
      IF(PXMAX.LT.PXMIN) GO TO 9100
      IF(PYMAX.LT.PYMIN) GO TO 9100
C
      PRECT(1)=PXMIN
      PRECT(2)=PXMAX
      PRECT(3)=PYMIN
      PRECT(4)=PYMAX
      RECT(1) = XMIN
      RECT(2) = XMAX
      RECT(3) = YMIN
      RECT(4) = YMAX
C
      XLI=DSIZE/(XMAX-XMIN)
      YLI=DSIZE/(YMAX-YMIN)
      XDEL=(PXMAX-PXMIN)*PXSCALE*XLI
      YDEL=(PYMAX-PYMIN)*PYSCALE*YLI
      XORG=(XMAX*(PXMIN*PXSCALE+PXOFFSET)
     &     -XMIN*(PXMAX*PXSCALE+PXOFFSET))*XLI
      YORG=(YMAX*(PYMIN*PYSCALE+PYOFFSET)
     &     -YMIN*(PYMAX*PYSCALE+PYOFFSET))*YLI
      CALL OFFCLP
      RETURN
C
 9000 WRITE(6,691) XMIN,XMAX,YMIN,YMAX
      RETURN
 9100 WRITE(6,692) PXMIN,PXMAX,PYMIN,PYMAX
      RETURN
C
  691 FORMAT(1H ,'XX SETVEW PARAMETER ERROR :'/
     &       1H ,'#   XMIN,XMAX,YMIN,YMAX = '/
     &       1H ,'#     ',1P4E15.7)
  692 FORMAT(1H ,'XX SETVEW PARAMETER ERROR :'/
     &       1H ,'#   PXMIN,PXMAX,PYMIN,PYMAX = '/
     &       1H ,'#     ',1P4E15.7)
      END
C
C     ****** CANCEL VIEW PORT ******
C
      SUBROUTINE OFFVEW
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
C
      IF(.NOT.LPAGE) RETURN
C
      CALL SETVEW(0.0,SIZEXS,0.0,SIZEYS,0.0,SIZEXS,0.0,SIZEYS)
      RETURN
      END
C
C     ****** INQUIRE VIEW PORT ******
C
      SUBROUTINE INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS2/ PRECT(4),RECT(4),CLRECT(4)
C
      IF(.NOT.LPAGE) RETURN
C
      PXMIN=PRECT(1)
      PXMAX=PRECT(2)
      PYMIN=PRECT(3)
      PYMAX=PRECT(4)
      XMIN =RECT(1)
      XMAX =RECT(2)
      YMIN =RECT(3)
      YMAX =RECT(4)
      RETURN
      END
C
C     ****** SET CLIP REGION ******
C
      SUBROUTINE SETCLP(XMIN,XMAX,YMIN,YMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      COMMON /GSAFS2/ PRECT(4),RECT(4),CLRECT(4)
C
      IF(.NOT.LPAGE) RETURN
C
      CLRECT(1)=XMIN
      CLRECT(2)=XMAX
      CLRECT(3)=YMIN
      CLRECT(4)=YMAX
C
      IF(RECT(1).LT.RECT(2)) THEN
         XL=MIN(XMIN,XMAX)-0.000001
         XR=MAX(XMIN,XMAX)+0.000001
      ELSE
         XL=MAX(XMIN,XMAX)+0.000001
         XR=MIN(XMIN,XMAX)-0.000001
      ENDIF
      IF(RECT(3).LT.RECT(4)) THEN
         YL=MIN(YMIN,YMAX)-0.000001
         YR=MAX(YMIN,YMAX)+0.000001
      ELSE
         YL=MAX(YMIN,YMAX)-0.000001
         YR=MIN(YMIN,YMAX)+0.000001
      ENDIF
      X1=XL
      Y1=YL
      RETURN
      END
C
C     ****** CANCEL CLIP REGION ******
C
      SUBROUTINE OFFCLP
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFS2/ PRECT(4),RECT(4),CLRECT(4)
C
      IF(.NOT.LPAGE) RETURN
C
      GX   = (RECT(2)-RECT(1))/(PRECT(2)-PRECT(1))
      XMIN = GX*(0.0   -PRECT(1))+RECT(1)
      XMAX = GX*(SIZEXS-PRECT(1))+RECT(1)
      GY   = (RECT(4)-RECT(3))/(PRECT(4)-PRECT(3))
      YMIN = GY*(0.0   -PRECT(3))+RECT(3)
      YMAX = GY*(SIZEYS-PRECT(3))+RECT(3)
      CALL SETCLP(XMIN,XMAX,YMIN,YMAX)
      RETURN
      END
C
C     ****** INQUIRE CLIP REGION ******
C
      SUBROUTINE INQCLP(XMIN,XMAX,YMIN,YMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS2/ PRECT(4),RECT(4),CLRECT(4)
C
      IF(.NOT.LPAGE) RETURN
C
      XMIN=CLRECT(1)
      XMAX=CLRECT(2)
      YMIN=CLRECT(3)
      YMAX=CLRECT(4)
      RETURN
      END
C
C     ****** MOVE TO ******
C
      SUBROUTINE MOVE(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
C
      IF(.NOT.LPAGE) RETURN
C
      X1=X
      Y1=Y
      LMV=.TRUE.
      RETURN
      END
C
C     ****** DRAW LINE ******
C
      SUBROUTINE DRAW(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
C
      IF(.NOT.LPAGE) RETURN
C
      DX=XR-XL
      DY=YR-YL
      JX1=0
      IF((X1-XL)*DX.LT.0.0) JX1=-1
      IF((XR-X1)*DX.LT.0.0) JX1=+1
      JY1=0
      IF((Y1-YL)*DY.LT.0.0) JY1=-1
      IF((YR-Y1)*DY.LT.0.0) JY1=+1
      X2=X
      Y2=Y
      JX2=0
      IF((X2-XL)*DX.LT.0.0) JX2=-1
      IF((XR-X2)*DX.LT.0.0) JX2=+1
      JY2=0
      IF((Y2-YL)*DY.LT.0.0) JY2=-1
      IF((YR-Y2)*DY.LT.0.0) JY2=+1
      LKP1=.TRUE.
C
 1000 IF(JX1.NE.0.OR.JY1.NE.0.OR.JX2.NE.0.OR.JY2.NE.0) THEN
         IF(JX1*JX2.EQ.1.OR.JY1*JY2.EQ.1) THEN
            X1=X
            Y1=Y
            RETURN
         ELSE
            IF(JX1.EQ.0.AND.JY1.EQ.0) THEN
               JX=JX2
               JY=JY2
               IND=2
            ELSE
               JX=JX1
               JY=JY1
               IND=1
            ENDIF
C
            IF(JY.EQ.1) THEN
               XN=X1+(X2-X1)*(YR-Y1)/(Y2-Y1)
               YN=YR
            ELSEIF(JY.EQ.-1) THEN
               XN=X1+(X2-X1)*(YL-Y1)/(Y2-Y1)
               YN=YL
            ELSEIF(JX.EQ.1) THEN
               YN=Y1+(Y2-Y1)*(XR-X1)/(X2-X1)
               XN=XR
            ELSEIF(JX.EQ.-1) THEN
               YN=Y1+(Y2-Y1)*(XL-X1)/(X2-X1)
               XN=XL
            ELSE
               WRITE(6,690)
  690          FORMAT(1H ,'XX LOGICAL ERROR IN CLIP PROCEDURE.')
               RETURN
            ENDIF
C
            JXN=0
            IF((XN-XL)*DX.LT.0.0) JXN=-1
            IF((XR-XN)*DX.LT.0.0) JXN=+1
            JYN=0
            IF((YN-YL)*DY.LT.0.0) JYN=-1
            IF((YR-YN)*DY.LT.0.0) JYN=+1
            IF(IND.EQ.1) THEN
               X1=XN
               Y1=YN
               JX1=JXN
               JY1=JYN
               LKP1=.FALSE.
            ELSE
               X2=XN
               Y2=YN
               JX2=JXN
               JY2=JYN
            ENDIF
            GO TO 1000
         ENDIF
      ENDIF
C
      IF(LMV.OR..NOT.LKP1) THEN
         IX=NINT(X1*XDEL+XORG)
         IY=NINT(Y1*YDEL+YORG)
         IF(IX.LT.0)     IX=0
         IF(IX.GT.32767) IX=32767
         IF(IY.LT.0)     IY=0
         IF(IY.GT.32767) IY=32767
         CALL DVMOVE(IX,IY)
         IF(LFIL) CALL BUFFXY(0,IX,IY)
      ENDIF
      IX=NINT(X2*XDEL+XORG)
      IY=NINT(Y2*YDEL+YORG)
      IF(IX.LT.0)     IX=0
      IF(IX.GT.32767) IX=32767
      IF(IY.LT.0)     IY=0
      IF(IY.GT.32767) IY=32767
      CALL DVDRAW(IX,IY)
      IF(LFIL) CALL BUFFXY(1,IX,IY)
      LMV=.FALSE.
      X1=X
      Y1=Y
      RETURN
      END
C
C     ****** DRAW MARK ******
C
      SUBROUTINE MARK(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      COMMON /GSAFIM/ IMK,IXMKX,IXMKY,IYMKX,IYMKY
C
      IF(.NOT.LPAGE) RETURN
C
      IF(IMK.LT.0) CALL DRAW(X,Y)
      IF((X-XL)*(XR-XL).GE.0.0.AND.
     &   (XR-X)*(XR-XL).GE.0.0.AND.
     &   (Y-YL)*(YR-YL).GE.0.0.AND.
     &   (YR-Y)*(YR-YL).GE.0.0) THEN
         IX=NINT(X*XDEL+XORG)
         IY=NINT(Y*YDEL+YORG)
         CALL GS_SFMARK(IX,IY)
         IF(LFIL) CALL BUFFXY(2,IX,IY)
      ENDIF
      X1=X
      Y1=Y
      LMV=.TRUE.
      RETURN
      END
C
C     ****** INQUIRE POSITION ******
C
      SUBROUTINE INQPOS(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
C
      IF(.NOT.LPAGE) RETURN
C
      X=X1
      Y=Y1
      RETURN
      END
C
C     ****** DRAW TEXT ******
C
      SUBROUTINE TEXT(KTEXT,NCHAR1)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      COMMON /GSPGC2/ TFXX,TFXY,TFYX,TFYY,TFSPX,TFSPY
      COMMON /GSAFS7/ IFNTS
      CHARACTER KTEXT*256
      DIMENSION IASC(256),IASCS(256)
C
      IF(.NOT.LPAGE) RETURN
C
      NCHAR=MIN(NCHAR1,256)
      CALL CHRASC(KTEXT,IASC,NCHAR)
      CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
      FACX=(XMAX-XMIN)/(PXMAX-PXMIN)
      FACY=(YMAX-YMIN)/(PYMAX-PYMIN)
C
      ISUB=0
      IFNT=-1
      IESC=0
      ISET=0
      I=0
C
      DO N=1,NCHAR
         INS=MOD(IASC(N),128)
         IF(IESC.EQ.0) THEN
            IF(INS.EQ.92.OR.INS.EQ.36) THEN
               IESC=1
            ELSE
               I=I+1
               IASCS(I)=INS
            ENDIF
         ELSE
            IF(ISET.EQ.0) THEN
               CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
               ISET=1
            ENDIF
            CALL GS_TEXT(X1,Y1,IASCS,I)
            IF(INS.EQ.43.OR.
     &         INS.EQ.45.OR.
     &         INS.EQ.61) THEN
               IF(ISUB.NE.0) THEN
                  CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
                  IF(ISUB.EQ.1) THEN
                     X1=X1-TFXY*CHH*0.666667*FACX
                     Y1=Y1-TFYY*CHH*0.666667*FACY
                  ELSE
                     X1=X1+TFXY*CHH*0.333333*FACX
                     Y1=Y1+TFYY*CHH*0.333333*FACY
                  ENDIF
                  ISUB=0
               ENDIF
               IF(INS.EQ.43) THEN
                  X1=X1+TFXY*CHH*0.666667*FACX
                  Y1=Y1+TFYY*CHH*0.666667*FACY
                  CALL SETCHR(0.7*CHH,0.7*CHW,0.7*CHSP,ANGL,TILT)
                  ISUB=1
               ELSEIF(INS.EQ.45) THEN
                  X1=X1-TFXY*CHH*0.333333*FACX
                  Y1=Y1-TFYY*CHH*0.333333*FACY
                  CALL SETCHR(0.7*CHH,0.7*CHW,0.7*CHSP,ANGL,TILT)
                  ISUB=-1
               ENDIF
            ELSEIF(INS.EQ.35) THEN
               IF(IFNT.EQ.-1) THEN
                  IFNT=IFNTS
                  IF(IFNTS.LT.32) THEN
                     CALL SETFNT(11)
                  ELSE
                     CALL SETFNT(44)
                  ENDIF
               ELSE
                  CALL SETFNT(IFNT)
                  IFNT=-1
               ENDIF
            ELSEIF(INS.GE.48.AND.INS.LE.57) THEN
               IF(IFNT.EQ.-1) IFNT=IFNTS
               IF(IFNTS.LT.32) THEN
                  CALL SETFNT(INS-48)
               ELSE
                  IF(INS-48.EQ.0) CALL SETFNT(0)
                  IF(INS-48.EQ.1) CALL SETFNT(40)
                  IF(INS-48.EQ.2) CALL SETFNT(32)
                  IF(INS-48.EQ.3) CALL SETFNT(33)
                  IF(INS-48.EQ.4) CALL SETFNT(34)
                  IF(INS-48.EQ.5) CALL SETFNT(35)
                  IF(INS-48.EQ.6) CALL SETFNT(36)
                  IF(INS-48.EQ.7) CALL SETFNT(37)
                  IF(INS-48.EQ.8) CALL SETFNT(38)
                  IF(INS-48.EQ.9) CALL SETFNT(39)
               ENDIF
            ELSEIF(INS.EQ.44) THEN
               X1=X1+TFSPX*0.1666667*CHSP*FACX
               Y1=Y1+TFSPY*0.1666667*CHSP*FACY
            ELSEIF(INS.EQ.59) THEN
               X1=X1+TFSPX*0.2777777*CHSP*FACX
               Y1=Y1+TFSPY*0.2777777*CHSP*FACY
            ELSEIF(INS.EQ.33) THEN
               X1=X1-TFSPX*0.1666667*CHSP*FACX
               Y1=Y1-TFSPY*0.1666667*CHSP*FACY
            ELSEIF(INS.EQ.63) THEN
               X1=X1-TFSPX*0.2777777*CHSP*FACX
               Y1=Y1-TFSPY*0.2777777*CHSP*FACY
            ELSEIF(INS.EQ.42) THEN
               X1=X1-TFSPX*CHSP*FACX
               Y1=Y1-TFSPY*CHSP*FACY
            ELSEIF(INS.EQ.47) THEN
               I=I+1
               IASCS(I)=92
            ELSE
               I=I+1
               IASCS(I)=INS
            ENDIF
            IESC=0
         ENDIF
      ENDDO
C
      CALL GS_TEXT(X1,Y1,IASCS,I)
      IF(ISUB.NE.0) CALL SETCHR(CHH,CHW,CHSP,ANGL,TILT)
      IF(IFNT.NE.-1) CALL SETFNT(IFNT)
      LMV=.TRUE.
      RETURN
      END
C
C     ****** DRAW TEXT ******
C
      SUBROUTINE GS_TEXT(X1,Y1,IASCS,NMAX)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFFN/ IFNTR,LSOFT
      COMMON /GSPGC2/ TFXX,TFXY,TFYX,TFYY,TFSPX,TFSPY
      DIMENSION IASCS(256)
C
      IF(NMAX.GT.0) THEN
         CALL INQVEW(PXMIN,PXMAX,PYMIN,PYMAX,XMIN,XMAX,YMIN,YMAX)
         CALL INQPAGE(XOFFSET,YOFFSET,XSCALE,YSCALE)
         FACX=(XMAX-XMIN)/((PXMAX-PXMIN)*XSCALE)
         FACY=(YMAX-YMIN)/((PYMAX-PYMIN)*YSCALE)
         CALL INQCHR(CHH,CHW,CHSP,ANGL,TILT)
         IX=NINT(X1*XDEL+XORG)
         IY=NINT(Y1*YDEL+YORG)
         IF(LSOFT) THEN
            IF(IFNTR.GE.2) THEN
               CALL INQLNW(WS)
               CALL SETLNW(0.08*MIN(CHH,1.5*CHW))
            ENDIF
            IF(LFIL) CALL BUFFTX(IX,IY,IASCS,NMAX)
            CALL GS_SFTEXT(IX,IY,IASCS,NMAX)
            IF(IFNTR.GE.2) THEN
               CALL SETLNW(WS)
            ENDIF
         ELSE
            IF(LFIL) CALL BUFFTX(IX,IY,IASCS,NMAX)
            CALL DVTEXT(IX,IY,IASCS,NMAX)
         ENDIF
         SIZE=0.0
         DO N=1,NMAX
            CALL GS_GRSIZE(IASCS(N),DX)
            SIZE=SIZE+DX
         ENDDO
         SIZESP=CHSP-1.5*CHW
         X1=X1+(TFXX*SIZE+TFSPX*SIZESP*NMAX)*FACX
         Y1=Y1+(TFYX*SIZE+TFSPY*SIZESP*NMAX)*FACY
      ENDIF
      NMAX=0
      RETURN
      END
C
C     ****** INQUIRE TEXT LENGTH ******
C
      SUBROUTINE INQTSZ(KTEXT,NCHAR1,SIZE)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS4/ CHARS(5)
      COMMON /GSAFS7/ IFNTS
      CHARACTER KTEXT*256
      DIMENSION IASC(256)
C
      IF(.NOT.LPAGE) RETURN
C
      CHW=CHARS(2)
      CHSP=CHARS(3)
      NCHAR=MIN(NCHAR1,256)
      CALL CHRASC(KTEXT,IASC,NCHAR)
C
      IFNT=-1
      IESC=0
      SIZE=0.0
      FACTOR=1.0
C
      DO N=1,NCHAR
         INS=MOD(IASC(N),128)
         IF(IESC.EQ.0) THEN
            IF(INS.EQ.92.OR.INS.EQ.36) THEN
               IESC=1
            ELSE
               CALL GS_GRSIZE(INS,DX)
               SIZE=SIZE+DX*FACTOR+CHSP-1.5*CHW
            ENDIF
         ELSE
            IF(INS.EQ.43.OR.INS.EQ.45) THEN
               FACTOR=0.7
            ELSEIF(INS.EQ.61) THEN
               FACTOR=1.0
            ELSEIF(INS.EQ.35) THEN
               IF(IFNT.EQ.-1) THEN
                  IFNT=IFNTS
                  IF(IFNTS.LT.32) THEN
                     CALL SETFNT(11)
                  ELSE
                     CALL SETFNT(44)
                  ENDIF
               ELSE
                  CALL SETFNT(IFNT)
                  IFNT=-1
               ENDIF
            ELSEIF(INS.GE.48.AND.INS.LE.57) THEN
               IF(IFNT.EQ.-1) IFNT=IFNTS
               IF(IFNTS.LT.32) THEN
                  CALL SETFNT(INS-48)
               ELSE
                  IF(INS-48.EQ.0) CALL SETFNT(0)
                  IF(INS-48.EQ.1) CALL SETFNT(40)
                  IF(INS-48.EQ.2) CALL SETFNT(32)
                  IF(INS-48.EQ.3) CALL SETFNT(33)
                  IF(INS-48.EQ.4) CALL SETFNT(34)
                  IF(INS-48.EQ.5) CALL SETFNT(35)
                  IF(INS-48.EQ.6) CALL SETFNT(36)
                  IF(INS-48.EQ.7) CALL SETFNT(37)
                  IF(INS-48.EQ.8) CALL SETFNT(38)
                  IF(INS-48.EQ.9) CALL SETFNT(39)
               ENDIF
            ELSEIF(INS.EQ.44) THEN
               SIZE=SIZE+0.1666667*CHSP*FACTOR
            ELSEIF(INS.EQ.59) THEN
               SIZE=SIZE+0.2777777*CHSP*FACTOR
            ELSEIF(INS.EQ.33) THEN
               SIZE=SIZE-0.1666667*CHSP*FACTOR
            ELSEIF(INS.EQ.63) THEN
               SIZE=SIZE-0.2777777*CHSP*FACTOR
            ELSEIF(INS.EQ.42) THEN
               SIZE=SIZE-CHSP*FACTOR
            ELSE
               CALL GS_GRSIZE(INS,DX)
               SIZE=SIZE+DX*FACTOR+(CHSP-1.5*CHW)*FACTOR
            ENDIF
            IESC=0
         ENDIF
      ENDDO
      IF(IFNT.NE.-1) CALL SETFNT(IFNT)
      IF(IFNT.EQ.0) SIZE=SIZE-(CHSP-CHW)*FACTOR
      RETURN
      END
C
C     ****** SET LINE ATTRIBUTE ******
C
      SUBROUTINE SETLIN(ILN,IBL,ICL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS3/ ILNS,IBLS,ICLS
      COMMON /GSAFS6/ WS,RS,GS,BS
C
      IF(.NOT.LPAGE) RETURN
C
      IF(ILN.GE.0) ILNS=ILN
      IF(IBL.GE.0) THEN
         WS=-1.0
         IBLS=IBL
      ENDIF
      IF(ICL.GE.0) THEN
         RS=-1.0
         ICLS=ICL
      ENDIF
      CALL DVSTLN(ILNS,IBLS,ICLS)
      IF(LFIL) CALL BUFFST(1,ILNS,IBLS,ICLS,0.0,0.0)
      RETURN
      END
C
C     ****** INQUIRE LINE ATRIBUTE ******
C
      SUBROUTINE INQLIN(ILN,IBL,ICL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS3/ ILNS,IBLS,ICLS
C
      IF(.NOT.LPAGE) RETURN
C
      ILN=ILNS
      IBL=IBLS
      ICL=ICLS
      RETURN
      END
C
C     ****** SET MARK ATTRIBUTE (FULL) ******
C
      SUBROUTINE SETMRK(IMRK,HMRK,WMRK,ANGL,TILT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFS5/ SMRKS(4),IMRKS
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
      DATA DEG/0.0174533/
C
      IF(.NOT.LPAGE) RETURN
C
      IMRKS   =IMRK
      SMRKS(1)=HMRK
      SMRKS(2)=WMRK
      SMRKS(3)=ANGL
      SMRKS(4)=TILT
C
      CA=COS(ANGL*DEG)
      SA=SIN(ANGL*DEG)
      CB=COS((ANGL+TILT)*DEG)
      SB=SIN((ANGL+TILT)*DEG)
      P1=SA*PYSCALE
      P2=CA*PXSCALE
      P3=SB*PXSCALE
      P4=CB*PYSCALE
      ANGLS=ATAN2(P1,P2)/DEG
      ANGLT=ATAN2(P3,P4)/DEG
      TILTS=ANGLT-ANGLS
      HSCAL=SQRT(P1**2+P2**2)
      VSCAL=SQRT(P3**2+P4**2)
C
      IMKH =NINT(SMRKS(1)*DSIZE*VSCAL)
      IMKW =NINT(SMRKS(2)*DSIZE*HSCAL)
C
      CALL GS_SFSTMK(IMRK,IMKH,IMKW,ANGLS,TILTS)
      IF(LFIL) CALL BUFFST(4,IMRK,IMKH,IMKW,ANGLS,TILTS)
C
      RETURN
      END
C
C     ****** SET MARK ATTRIBUTE (PARTIAL) ******
C
      SUBROUTINE SETMKS(IMRK,SMRK)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LPAGE) RETURN
C
      CALL SETMRK(IMRK,SMRK,SMRK,0.0,0.0)
      RETURN
      END
C
C     ****** INQUIRE MARK ATTRIBUTE ******
C
      SUBROUTINE INQMRK(IMRK,HMRK,WMRK,ANGL,TILT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS5/ SMRKS(4),IMRKS
C
      IF(.NOT.LPAGE) RETURN
C
      IMRK=IMRKS
      HMRK=SMRKS(1)
      WMRK=SMRKS(2)
      ANGL=SMRKS(3)
      TILT=SMRKS(4)
      RETURN
      END
C
C     ****** SET CHARACTER ATTRIBUTE (FULL) ******
C
      SUBROUTINE SETCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS4/ CHARS(5)
C
      IF(.NOT.LPAGE) RETURN
C
      CHARS(1)=CHH
      CHARS(2)=CHW
      CHARS(3)=CHSP
      CHARS(4)=ANGL
      CHARS(5)=TILT
      CALL GS_SETCHAR
C
      RETURN
      END
C
C     ****** SET CHARCTER ATTRIBUTE (PARTIAL) ******
C
      SUBROUTINE SETCHS(CHH,ANGL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS4/ CHARS(5)
      COMMON /GSAFS7/ IFNTS
C
      IF(.NOT.LPAGE) RETURN
C
      CHARS(1)=CHH
      CHARS(2)=CHH*0.6666667
      CHARS(3)=CHH
      CHARS(4)=ANGL
      IF(IFNTS.EQ.37.OR.
     &   IFNTS.EQ.39.OR.
     &   IFNTS.EQ.41.OR.
     &   IFNTS.EQ.43) THEN
         CHARS(5)=-10.0
      ELSE
         CHARS(5)=0.0
      ENDIF
      CALL GS_SETCHAR
      RETURN
      END
C
C     ****** SET CHARACTER ATTRIBUTE (HARDWARE) ******
C
      SUBROUTINE SETCHH(CHH,ANGL)
C
      CALL SETCHS(CHH,ANGL)
      CALL SETFNT(1)
      RETURN
      END
C
C     ****** SET FONT TYPE ******
C
      SUBROUTINE SETFNT(IFNT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LPAGE) RETURN
C
      CALL GS_SETFNT(IFNT)
      IF(LFIL) CALL BUFFST(7,IFNT,0,0,0.0,0.0)
      CALL GS_SETCHAR
      RETURN
      END
C
C     ****** SET FONT TYPE ******
C
      SUBROUTINE GS_SETFNT(IFNT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFS4/ CHARS(5)
      COMMON /GSAFS7/ IFNTS
      COMMON /GSAFFN/ IFNTR,LSOFT
C
      IFNTS=IFNT
      IF(IFNT.EQ.1.OR.IFNT.GE.32) THEN
         LSOFT=.FALSE.
         IF(IFNT.EQ.1) THEN
            CALL DVFONT(0,IND)
            IF(IND.EQ.1) LSOFT=.TRUE.
            IFNTR=0
         ELSE
            CALL DVFONT(IFNT,IND)
            IF(IND.EQ.1) LSOFT=.TRUE.
            IF(IFNT.GE.32.AND.IFNT.LE.35) THEN
               IF(IFNT.EQ.32) IFNTR=2
               IF(IFNT.EQ.33) IFNTR=3
               IF(IFNT.EQ.34) IFNTR=4
               IF(IFNT.EQ.35) IFNTR=5
            ELSEIF(IFNT.GE.36.AND.IFNT.LE.39) THEN
               IFNTR=6
               IF(IND.EQ.1) THEN
                  IF(IFNT.EQ.37) CHARS(5)=-10.0
                  IF(IFNT.EQ.39) CHARS(5)=-10.0
               ENDIF
            ELSEIF(IFNT.GE.40.AND.IFNT.LE.43) THEN
               IFNTR=0
               IF(IND.EQ.1) THEN
                  IF(IFNT.EQ.41) CHARS(5)=-10.0
                  IF(IFNT.EQ.43) CHARS(5)=-10.0
               ENDIF
            ELSE
               IF(IFNT.EQ.44) THEN
                  IFNTR=11
               ELSE
                  IFNTR=0
               ENDIF
            ENDIF
         ENDIF
      ELSE
         LSOFT=.TRUE.
         IFNTR=IFNT
      ENDIF
      RETURN
      END
C
C     ****** SET FONT ATTRIBUTE ******
C
      SUBROUTINE GS_SETCHAR
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFS4/ CHARS(5)
      COMMON /GSAFFN/ IFNTR,LSOFT
      COMMON /GSAFS8/ PXOFFSET,PYOFFSET,PXSCALE,PYSCALE
      COMMON /GSPGC2/ TFXX,TFXY,TFYX,TFYY,TFSPX,TFSPY
      DATA DEG/0.0174533/
C
      ANGL =CHARS(4)
      TILT =CHARS(5)
C
      CA=COS(ANGL*DEG)
      SA=SIN(ANGL*DEG)
      CB=COS((ANGL+TILT)*DEG)
      SB=SIN((ANGL+TILT)*DEG)
      P1=SA*PYSCALE
      P2=CA*PXSCALE
      P3=SB*PXSCALE
      P4=CB*PYSCALE
      ANGLS=ATAN2(P1,P2)/DEG
      ANGLT=ATAN2(P3,P4)/DEG
      TILTS=ANGLT-ANGLS
      HSCAL=SQRT(P1**2+P2**2)
      VSCAL=SQRT(P3**2+P4**2)
C
      ICHH =NINT(CHARS(1)*DSIZE*VSCAL)
      ICHW =NINT(CHARS(2)*DSIZE*HSCAL)
      ICHSP=NINT(CHARS(3)*DSIZE*HSCAL)
C
      IF(LFIL) CALL BUFFST(2,ICHH,ICHW,ICHSP,ANGLS,TILTS)
      IF(LSOFT) THEN
         CALL GS_SFSTCH(ICHH,ICHW,ICHSP,ANGLS,TILTS)
      ELSE
         CALL DVSTCH(ICHH,ICHW,ICHSP,ANGLS,TILTS,IND)
      ENDIF
C
      IF(ICHH.EQ.0) THEN
         FACT=1.0
      ELSE
         FACT=1.5*REAL(ICHW)/REAL(ICHH)
      ENDIF
      TFXX= CA*FACT
      TFYX= SA*FACT
      TFXY=-SB
      TFYY= CB
      TFSPX=CA
      TFSPY=SA
      RETURN
      END
C
C     ****** INQUIRE CHARACTER ATTRIBUTE ******
C
      SUBROUTINE INQCHR(CHH,CHW,CHSP,ANGL,TILT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS4/ CHARS(5)
C
      IF(.NOT.LPAGE) RETURN
C
      CHH =CHARS(1)
      CHW =CHARS(2)
      CHSP=CHARS(3)
      ANGL=CHARS(4)
      TILT=CHARS(5)
      RETURN
      END
C
C     ****** INQUIRE FONT TYPE ******
C
      SUBROUTINE INQFNT(IFNT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS7/ IFNTS
C
      IF(.NOT.LPAGE) RETURN
C
      IFNT=IFNTS
      RETURN
      END
C
C     ****** GET INPUT CHARACTERS ******
C
      SUBROUTINE CHIN(KTEXT,NCHAR1)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
      CHARACTER KTEXT*256
      DIMENSION IASC(256)
C
      IF(.NOT.LPAGE) RETURN
C
      NCHAR=MIN(NCHAR1,256)
      IX=NINT(X1*XDEL+XORG)
      IY=NINT(Y1*XDEL+YORG)
      IF(IX.LT.0)     IX=0
      IF(IX.GT.32767) IX=32767
      IF(IY.LT.0)     IY=0
      IF(IY.GT.32767) IY=32767
      CALL DVMOVE(IX,IY)
      LMV=.FALSE.
C
      CALL DVCHIN(IASC,NCHAR)
      CALL ASCCHR(IASC,KTEXT,NCHAR)
      RETURN
      END
C
C     ****** GET CURSOR POSITION ******
C
      SUBROUTINE XYIN(X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
C
      IF(.NOT.LPAGE) RETURN
C
      IX=NINT(X1*XDEL+XORG)
      IY=NINT(Y1*XDEL+YORG)
      IF(IX.LT.0)     IX=0
      IF(IX.GT.32767) IX=32767
      IF(IY.LT.0)     IY=0
      IF(IY.GT.32767) IY=32767
      CALL DVMOVE(IX,IY)
      LMV=.FALSE.
C
      CALL DVXYIN(IX,IY)
      X=(REAL(IX)+0.5-XORG)/XDEL
      Y=(REAL(IY)+0.5-YORG)/YDEL
      RETURN
      END
C
C     ****** ERASE SCREEN ******
C
      SUBROUTINE ERAS
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LPAGE) RETURN
C
      CALL DVERAS
      RETURN
      END
C
C     ****** BEEP SOUND ******
C
      SUBROUTINE BELL
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LPAGE) RETURN
C
      CALL DVBELL
      RETURN
      END
C
C     ****** CHANGE TO GRAPHIC SCREEN ******
C
      SUBROUTINE GRMODE
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LPAGE) RETURN
C
      CALL DVGRMD
      RETURN
      END
C
C     ******CHANGE TO CHARACTER SCREEN ******
C
      SUBROUTINE CHMODE
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LPAGE) RETURN
C
      CALL DVCHMD
      RETURN
      END
C
C     ++++++ CONVERT STRING WITH DELIM. TO STRING ++++++
C
      SUBROUTINE GUDSTR(KIN,NIN,KOUT,NOUT)
C
      CHARACTER KIN*256,KOUT*256,KDL*1
C
      KDL=KIN(1:1)
      I=2
    1 IF(KIN(I:I).EQ.KDL.OR.I.EQ.NIN) GOTO 2
         I=I+1
      GOTO 1
C
    2 IF(KIN(I:I).EQ.KDL) THEN
         KOUT(1:I-2)=KIN(2:I-1)
         NOUT=I-2
      ENDIF
      RETURN
      END
C
C     ++++++ CAPITALIZE CHAR ++++++
C
      SUBROUTINE GUCPTL(KID)
C
      CHARACTER KID*1
C
      CALL CHRASC(KID,ID,1)
C
      IF(ID.GE.97.AND.ID.LE.122) ID=ID-32
C
      CALL ASCCHR(ID,KID,1)
C
      RETURN
      END
C
C     ****** START GROUP ******
C
      SUBROUTINE GUGRPS
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSGRPL/ NGRPL,NGRPM
C
      IF(.NOT.LPAGE) RETURN
      CALL DVGRPS
      IF(LFIL) CALL BUFFST(8,0,0,0,0.0,0.0)
      NGRPL=NGRPL+1
      IF(NGRPL.GT.NGRPM) NGRPM=NGRPL
      RETURN
      END
C
C     ****** END GROUP ******
C
      SUBROUTINE GUGRPE
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSGRPL/ NGRPL,NGRPM
C
      IF(.NOT.LPAGE) RETURN
      CALL DVGRPE
      IF(LFIL) CALL BUFFST(8,1,0,0,0.0,0.0)
      NGRPL=NGRPL-1
      RETURN
      END
C
C     ****** INQUIRE GROUP LEVEL ******
C
      SUBROUTINE INQGRP(N1,N2)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSGRPL/ NGRPL,NGRPM
C
      IF(.NOT.LGSAF) RETURN
C
      N1=NGRPL
      N2=NGRPM
      RETURN
      END
C
C     ****** AVOID REAL*4 UNDERFLOW ******
C
      REAL FUNCTION GUCLIP(D)
C
      REAL*8 D
C
      IF(ABS(D).LT.1.D-30) then
         GUCLIP=0.0
      ELSEIF(D.GT. 1.D30) THEN
         GUCLIP= 1.E30
      ELSEIF(D.lT.-1.D30) THEN
         GUCLIP=-1.E30
      ELSE
         GUCLIP=SNGL(D)
      ENDIF
      RETURN
      END
C
C     ***** OPTIMUM NUM LENGTH FOR GVALUE *****
C
      FUNCTION NGULEN(GSTEP)
C
      GXL = SNGL(LOG10(DBLE(GSTEP*0.11)))
      IF(GXL.LT.0.0) THEN
         NGX = -INT(-GXL)
      ELSE
         NGX =  INT(GXL)
      ENDIF
      IF(NGX.LT.-4.OR.NGX.GT.4)THEN
         GX = GXL-NGX
         IF(GX.LT.0.0) GX=GX+1.0
         IF(GX.LE.0.15) THEN
            NGX=100
         ELSE
            NGX=101
         ENDIF
      ELSEIF(NGX.GE.0) THEN
         NGX=0
      ENDIF
      NGULEN=-NGX
      RETURN
      END
