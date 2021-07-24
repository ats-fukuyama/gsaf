
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
      CHARACTER KTEXT*(*)
      DIMENSION IASC(NCHAR)
      IF(NCHAR.LE.0) RETURN
      DO 1000 I=1,NCHAR
         IASC(I)=ICHAR(KTEXT(I:I))
 1000 CONTINUE
      RETURN
      END
C
      SUBROUTINE CHRASC1(KTEXT,IASC)
C
      CHARACTER*1 KTEXT
      INTEGER IASC
      IASC=ICHAR(KTEXT)
      RETURN
      END
C
C     ++++++ CONVERT FROM INTEGER TO CHARACTER (ASCII) ++++++
C
      SUBROUTINE ASCCHR(IASC,KTEXT,NCHAR)
C
      CHARACTER KTEXT*(NCHAR)
      DIMENSION IASC(NCHAR)
      IF(NCHAR.LE.0) RETURN
      DO 1000 I=1,NCHAR
         KTEXT(I:I)=CHAR(IASC(I))
 1000 CONTINUE
      RETURN
      END
C
      SUBROUTINE ASCCHR1(IASC,KTEXT)
C
      CHARACTER KTEXT*1
      INTEGER IASC
      KTEXT=CHAR(IASC)
      RETURN
      END
C
C     ****** GET DATE AND TIME ******
C
      SUBROUTINE GUDATE(NDY,NDM,NDD,NTH,NTM,NTS)
C
C      CALL DVDATE(NDY,NDM,NDD,NTH,NTM,NTS)
      NDY=0
      NDM=0
      NDD=0
      NTH=0
      NTM=0
      NTS=0
      RETURN
      END
C
C     ****** GET CPUTIME ******
C
      SUBROUTINE GUTIME(T)
      REAL T
      INTEGER NT,NTICK
C
C      CALL DVTIME(NT,NTICK)
      NT=0
      NTICK=1
      T=REAL(DBLE(NT)/DBLE(NTICK))
      RETURN
      END
C
C     ****** GET CPUTIMES ******
C
      SUBROUTINE GUTIMES(TU,TS,TCU,TCS,TELP)
      IMPLICIT NONE
      REAL*8 TU,TS,TCU,TCS,TELP
      INTEGER NTU,NTS,NTCU,NTCS,NTICK,ITS,ITUS
C
C      CALL DVTIMES(NTU,NTS,NTCU,NTCS,NTICK,ITS,ITUS)
      NTU=0
      NTS=0
      NTCU=0
      NTCS=0
      NTICK=1
      ITS=0
      ITUS=0
      TU=DBLE(NTU)/DBLE(NTICK)
      TS=DBLE(NTS)/DBLE(NTICK)
      TCU=DBLE(NTCU)/DBLE(NTICK)
      TCS=DBLE(NTCS)/DBLE(NTICK)
      TELP=DBLE(ITS)+1.D-6*DBLE(ITUS)
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
C      CALL DVFLSH
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
