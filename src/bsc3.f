C     $Id$C
C
C     ****** SET EVENT_MASK TYPE ******
C
      SUBROUTINE GF_SET_EVENT(ID)
C
C        ID = 0 : NO EVENT
C        ID = 1 : KEY PRESSED
C        ID = 2 : MOUSE PRESSED
C        ID = 4 : MOUSE RELEASED
C        ID = 8 : MOUSE MOTION
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFEV/ ID_EVENT
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      CALL DVSETV(ID)
      ID_EVENT=ID
      RETURN
      END
C
C     ****** INQUIRE EVENT_MASK TYPE ******
C
      SUBROUTINE GF_INQ_EVENT(ID)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFEV/ ID_EVENT
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      ID=ID_EVENT
      RETURN
      END
C
C     ****** GET EVENT DATA ******
C
      SUBROUTINE GF_GET_EVENT(ID,X,Y,KID,KEY)
C
C        ID : 1 KeyPressed
C        ID : 2 MousePressed
C        ID : 4 MouseReleased
C        ID : 8 MouseMoution
C
C        X   : Mouse X Positon
C        Y   : Mouse Y Positon
C        KID : Key   : Character Code in ASCII
C              Mouse : Mouse Button Number
C        KEY : Key   : Key Number
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFXY/ XDEL,YDEL,XORG,YORG
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      CALL DVGETV(ID,IX,IY,KEY,KID)
      X=(REAL(IX)+0.5-XORG)/XDEL
      Y=(REAL(IY)+0.5-YORG)/YDEL
C
      RETURN
      END
C
C     ****** SET GC FUNCTION ******
C
      SUBROUTINE GF_SET_GCFUNC(ID)
C
C        ID =  0 : Clear             0
C        ID =  1 : And               S  &  D
C        ID =  2 : AndReverse        S  & !D
C        ID =  3 : Copy (Default)    S 
C        ID =  4 : AndInverted      !S  &  D
C        ID =  5 : Noop                    D
C        ID =  6 : Xor               S XOR D
C        ID =  7 : Or                S  |  D
C        ID =  8 : Nor              !S  & !D
C        ID =  9 : Equiv            !S XOR D
C        ID = 10 : Invert                 !D
C        ID = 11 : OrReverse         S  | !D
C        ID = 12 : CopyInverted     !S
C        ID = 13 : OrInverted       !S  |  D
C        ID = 14 : Nand             !S  | !D
C        ID = 15 : Set               1
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFGC/ ID_GCFUNC
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      CALL DVGCFUNC(ID)
      IF(LFIL) CALL BUFFST(9,ID,0,0,0.0,0.0)
      ID_GCFUNC=ID
      RETURN
      END
C
C     ****** INQ GC FUNCTION ******
C
      SUBROUTINE GF_INQ_GCFUNC(ID)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFGC/ ID_GCFUNC
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      ID=ID_GCFUNC
      RETURN
      END
C
C     ****** SYNC XWINDOW ******
C
      SUBROUTINE GF_SYNC
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LGSAF) RETURN
      IF(.NOT.LPAGE) RETURN
C
      CALL DVSYNC
      RETURN
      END
