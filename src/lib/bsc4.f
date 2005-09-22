C     $Id$
C
C     ****** DEFINE IMAGE ******
C
      SUBROUTINE GF_DEFIMAGE(ID,IWIDTH,IHEIGHT,IMAGE)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      DIMENSION IMAGE(IWIDTH,IHEIGHT)
C
      IF(.NOT.LGSAF) RETURN
C
      CALL DVDEFIMAGE(ID,IWIDTH,IHEIGHT,IMAGE)
      RETURN
      END
C
C     ****** UNDEFINE IMAGE ******
C
      SUBROUTINE GF_UNDEFIMAGE(ID)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LGSAF) RETURN
C
      CALL DVUNDEFIMAGE(ID)
      RETURN
      END
C
C     ****** PUT IMAGE ******
C
      SUBROUTINE GF_PUTIMAGE(ID,X,Y)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
      COMMON /GSAFCL/ LMV,X1,Y1,XL,XR,YL,YR
C
      IF(.NOT.LPAGE) RETURN
C
      IX=NINT(X*XDEL+XORG)
      IY=NINT(Y*YDEL+YORG)
      IF(IX.LT.0)     IX=0
      IF(IX.GT.32767) IX=32767
      IF(IY.LT.0)     IY=0
      IF(IY.GT.32767) IY=32767
C
      CALL DVPUTIMAGE(ID,IX,IY)
      RETURN
      END