C     $Id$
      SUBROUTINE ANIMES
      RETURN
      END
C
      SUBROUTINE ANIMEE
      RETURN
      END
C
      SUBROUTINE gsglProjImgTranslate (X, Y)
      RETURN
      END
C
      SUBROUTINE gsglProjImgScale (FX, FY)
      RETURN
      END
C
      SUBROUTINE gsglProjImgRotate (ANGLE)
      RETURN
      END
C
      SUBROUTINE gsglProjImgIdentity
      RETURN
      END
C
      SUBROUTINE gsglVecrotText ()
      RETURN
      END
C
      SUBROUTINE gsglPolygonText ()
      RETURN
      END
C
      SUBROUTINE SetFreeTypeNodeNum (N)
      RETURN
      END
C
      SUBROUTINE gsgl_inqcharmath (CHH, CHW)
      RETURN
      END
C
      SUBROUTINE inqtextbbox (ktext, nchar, size, height, depth)
      CHARACTER*256 KTEXT
      CALL INQTSZ(KTEXT, NCHAR, SIZE)
      CALL INQCHR(HEIGHT,CHW,CHSP,ANGL,TILT)
      HEIGHT=HEIGHT*8.0/9.0
      DEPTH=0.0
      RETURN
      END
C
