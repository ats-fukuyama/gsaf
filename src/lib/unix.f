C     $Id$
C
C     ***************************************************
C     ****** GSAF DEPENDENT V3.5 :  MACHINE DEPEND ******
C     ***************************************************
C
C     ++++++ CONVERT FROM CHARACTER TO INTEGER (ASCII) ++++++
C
      SUBROUTINE CHRASC(KTEXT,IASC,NCHAR)
C
      CHARACTER KTEXT*256
      DIMENSION IASC(256)
      IF(NCHAR.LE.0) RETURN
      DO 1000 I=1,NCHAR
         IASC(I)=ICHAR(KTEXT(I:I))
 1000 CONTINUE
      RETURN
      END
C
C     ++++++ CONVERT FROM INTEGER TO CHARACTER (ASCII) ++++++
C
      SUBROUTINE ASCCHR(IASC,KTEXT,NCHAR)
C
      CHARACTER KTEXT*256
      DIMENSION IASC(256)
      IF(NCHAR.LE.0) RETURN
      DO 1000 I=1,NCHAR
         KTEXT(I:I)=CHAR(IASC(I))
 1000 CONTINUE
      RETURN
      END
C
C     ****** GET DATE AND TIME ******
C
      SUBROUTINE GUDATE(NDY,NDM,NDD,NTH,NTM,NTS)
C
      CALL DVDATE(NDY,NDM,NDD,NTH,NTM,NTS)
      RETURN
      END
C
C     ****** GET CPUTIME ******
C
      SUBROUTINE GUTIME(T)
C
      CALL DVTIME(NT,NTICK)
      T=REAL(DBLE(NT)/DBLE(NTICK))
      RETURN
      END
C
C     ****** RANDOM REAL VALUE ******
C
      FUNCTION GURAND(X)
C
      CALL DVRAND(I,K)
      Y=REAL(I)/REAL(K)
      GURAND=Y*X
      RETURN
      END
C
C     ****** INITIALIZE RANDOM GENERATOR ******
C
      SUBROUTINE GU_SRAND(I)
C
      CALL DVSRAND(I)
      RETURN
      END
C
C     ****** FLUSH STDOUT ******
C
      SUBROUTINE GUFLSH
C
      CALL DVFLSH
      RETURN
      END
C
C     ****** FLUSH X ******
C
      SUBROUTINE GU_XFLUSH
C
      CALL DVXFLUSH
      RETURN
      END
C
C     ****** SLEEP in SEC ******
C
      SUBROUTINE GU_SLEEP(T)
C
      IT=NINT(T*1000000)
      CALL DVSLEEP(IT)
      RETURN
      END
