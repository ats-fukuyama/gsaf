C     $Id$
C
C     **************************************
C     ****** GSAF DRIVER V3.5 : DUMMY ******
C     **************************************
C
C     ++++++ INITIALIZE DEVICE ++++++
C
      SUBROUTINE DVTYPE(ICH)
C
      WRITE(6,*) '# Welcome to GSAF '
      ICH=0
      RETURN
      END
C
      SUBROUTINE DVOPEN(ICH)
C
      ICH=-1
      RETURN
      END
C
C     ++++++ TERMINATE DEVICE ++++++
C
      SUBROUTINE DVCLOS(ICH)
C
      ICH=0
      RETURN
      END
C
C     ++++++ SET DEVICE OPTIONS ++++++
C
      SUBROUTINE DVOPTN(KOPT,IOPT)
C
      CHARACTER KOPT*2,K*1
C
      K=KOPT(1:1)
      I=IOPT
      RETURN
      END
C
C     ++++++ INITIALIZE PAGE ++++++
C
      SUBROUTINE DVPAGS(NPAGE,SIZEX,SIZEY,LKEEP)
C
      IMPLICIT LOGICAL(L)
C
      N=NPAGE
      S=SIZEX
      S=SIZEY
      L=LKEEP
      RETURN
      END
C
C     ++++++ TERMINATE PAGE ++++++
C
      SUBROUTINE DVPAGE(ICH)
C
      ICH=0
      RETURN
      END
C
C     ++++++ CHANGE TO GRAPHIC SCREEN ++++++
C
      SUBROUTINE DVGRMD
C
      RETURN
      END
C
C     ++++++ CHANGE TO CHARACTER SCREEN ++++++
C
      SUBROUTINE DVCHMD
C
      RETURN
      END
C
C     ++++++ START GROUP ++++++
C
      SUBROUTINE DVGRPS
C
      RETURN
      END
C
C     ++++++ END GROUP ++++++
C
      SUBROUTINE DVGRPE
C
      RETURN
      END
C
C     ++++++ MOVE CURSOR ++++++
C
      SUBROUTINE DVMOVE(IX,IY)
C
      I=IX
      I=IY
      RETURN
      END
C
C     ++++++ DRAW LINE ++++++
C
      SUBROUTINE DVDRAW(IX,IY)
C
      I=IX
      I=IY
      RETURN
      END
C
C     ++++++ DRAW LINEC ++++++
C
      SUBROUTINE DVLINS(IX,IY,NP)
C
      DIMENSION IX(*),IY(*)
      I=IX(1)
      I=IY(1)
      I=NP
      RETURN
      END
C
C     ++++++ DRAW POLY ++++++
C
      SUBROUTINE DVPOLY(IX,IY,NP)
C
      DIMENSION IX(*),IY(*)
      I=IX(1)
      I=IY(1)
      I=NP
      RETURN
      END
C
C     ++++++ DRAW TRIANGLE ++++++
C
      SUBROUTINE DVRGBTRG(IX,IY,IR,IG,IB)
C
      DIMENSION IX(*),IY(*),IR(*),IG(*),IB(*)
      I=IX(1)
      I=IY(1)
      I=IR(1)
      I=IG(1)
      I=IB(1)
      RETURN
      END
C
C     ++++++ DRAW TEXT ++++++
C
      SUBROUTINE DVTEXT(IX,IY,IASC,NCHAR)
C
      DIMENSION IASC(256)
C
      I=IX
      I=IY
      I=IASC(1)
      I=NCHAR
      RETURN
      END
C
C     ++++++ SET LINE ATTRIBUTE ++++++
C
      SUBROUTINE DVSTLN(ILN,IBL,ICL)
C
      I=ILN
      I=IBL
      I=ICL
      RETURN
      END
C
C     ++++++ SET LINE WIDTH ++++++
C
      SUBROUTINE DVLWDT(IW)
C
      I=IW
      RETURN
      END
C
C     ++++++ SET RGB COLOR ++++++
C
      SUBROUTINE DVCRGB(IR,IG,IB)
C
      I=IR
      I=IG
      I=IB
      RETURN
      END
C
C     ++++++ SET CHARACTER ATTRIBUTE ++++++
C
      SUBROUTINE DVSTCH(ICHH,ICHW,ICHSP,ANGL,TILT,IND)
C
      I=ICHH
      I=ICHW
      I=ICHSP
      X=ANGL
      X=TILT
      IND=0
      RETURN
      END
C
C     ++++++ SET DEVICE FONT ++++++
C
      SUBROUTINE DVFONT(IFNT,IND)
C
      I=IFNT
      IND=0
      RETURN
      END
C
C     ++++++ GET INPUT CHARACTERS ++++++
C
      SUBROUTINE DVCHIN(IASC,NCHAR)
C
      DIMENSION IASC(256)
C
      DO 10 I=1,NCHAR
         IASC(I)=32
   10 CONTINUE
      RETURN
      END
C
C     ++++++ GET CURSOR POSITION ++++++
C
      SUBROUTINE DVXYIN(IX,IY)
C
      IX=0
      IY=0
      RETURN
      END
C
C     ++++++ SET EVENT TYPE ++++++
C
      SUBROUTINE DVSETV(ID)
C
      I=ID
      RETURN
      END
C
C     ++++++ GET EVENT DATA ++++++
C
      SUBROUTINE DVGETV(ID,IX,IY,KID,KEY)
C
      ID=0
      IX=0
      IY=0
      KID=0
      KEY=0
      RETURN
      END
C
C     ++++++ ERASE SCREEN ++++++
C
      SUBROUTINE DVERAS
C
      RETURN
      END
C
C     ++++++ PRINT GRAPHIC HARDCOPY ++++++
C
      SUBROUTINE DVPRNT
C
      RETURN
      END
C
C     ++++++ BEEP SOUND ++++++
C
      SUBROUTINE DVBELL
C
      RETURN
      END
C
C     ++++++ SET GCFUNC TYPE ++++++
C
      SUBROUTINE DVGCFUNC(ID)
C
      I=ID
      RETURN
      END
C
C     ++++++ SYNC ++++++
C
      SUBROUTINE DVSYNC
C
      RETURN
      END
