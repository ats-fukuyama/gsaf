C     $Id$
C
C     ****** SET DATA COLOR ******
C
      SUBROUTINE R2G2P(R,RGB)
C
      DIMENSION RGB(3)
C
      IR=INT(200.0*R)
      IF (IR.LE.40) THEN
         RGB(1)=1.0-REAL(IR)/40.0
         RGB(2)=0.0
         RGB(3)=1.0
      ELSE IF (IR.LE.80) THEN
         RGB(1)=0.0
         RGB(2)=REAL(IR-40)/40.0
         RGB(3)=1.0
      ELSE IF (IR.LE.120) THEN
         RGB(1)=0.0
         RGB(2)=1.0
         RGB(3)=1.0-REAL(IR-80)/40.0
      ELSE IF (IR.LE.160) THEN
         RGB(1)=REAL(IR-120)/40.0
         RGB(2)=1.0
         RGB(3)=0.0
      ELSE
         RGB(1)=1.0
         RGB(2)=1.0-REAL(IR-160)/40.0
         RGB(3)=0.0
      ENDIF
C
      RETURN
      END
C
C     ****** SET DATA COLOR ******
C
      SUBROUTINE WHITE(R,RGB)
C
      DIMENSION RGB(3)
C
      RGB(1)=1.0
      RGB(2)=1.0
      RGB(3)=1.0
C
      RETURN
      END
C
C     ****** SET DATA COLOR ******
C
      SUBROUTINE BLACK(R,RGB)
C
      DIMENSION RGB(3)
C
      RGB(1)=0.0
      RGB(2)=0.0
      RGB(3)=0.0
C
      RETURN
      END
C
C     ****** SET DATA COLOR ******
C
      SUBROUTINE R2G2B(R,RGB)
C
      DIMENSION RGB(3)
C
      IR=INT(160.0*R)
      IF (IR.LE.40) THEN
         RGB(1)=0.0
         RGB(2)=REAL(IR)/40.0
         RGB(3)=1.0
      ELSE IF (IR.LE.80) THEN
         RGB(1)=0.0
         RGB(2)=1.0
         RGB(3)=1.0-REAL(IR-40)/40.0
      ELSE IF (IR.LE.120) THEN
         RGB(1)=REAL(IR-80)/40.0
         RGB(2)=1.0
         RGB(3)=0.0
      ELSE
         RGB(1)=1.0
         RGB(2)=1.0-REAL(IR-120)/40.0
         RGB(3)=0.0
      ENDIF
C
      RETURN
      END
C
C     ****** SET DATA COLOR ******
C
      SUBROUTINE R2W2B(R,RGB)
C
      DIMENSION RGB(3)
C
      IR=INT(80.0*R)
      IF (IR.LE.40) THEN
         RGB(1)=REAL(IR)/40.0
         RGB(2)=REAL(IR)/40.0
         RGB(3)=1.0
      ELSE
         RGB(1)=1.0
         RGB(2)=1.0-REAL(IR-40)/40.0
         RGB(3)=1.0-REAL(IR-40)/40.0
      ENDIF
C
      RETURN
      END   
C
C     ****** SET DATA COLOR ******
C
      SUBROUTINE R2G2BW(R,RGB)
C
      DIMENSION RGB(3)
C
      IR=INT(160.0*R)
      IF (IR.EQ.0) THEN
         RGB(1)=1.0
         RGB(2)=1.0
         RGB(3)=1.0
      ELSE IF (IR.LE.40) THEN
         RGB(1)=0.0
         RGB(2)=REAL(IR)/40.0
         RGB(3)=1.0
      ELSE IF (IR.LE.80) THEN
         RGB(1)=0.0
         RGB(2)=1.0
         RGB(3)=1.0-REAL(IR-40)/40.0
      ELSE IF (IR.LE.120) THEN
         RGB(1)=REAL(IR-80)/40.0
         RGB(2)=1.0
         RGB(3)=0.0
      ELSE
         RGB(1)=1.0
         RGB(2)=1.0-REAL(IR-120)/40.0
         RGB(3)=0.0
      ENDIF
C
      RETURN
      END
