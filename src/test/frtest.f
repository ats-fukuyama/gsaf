C     $Id$
C
      COMMON /GSAFRB/ NFCR,IRBUFF,IRDATA(150)
C
      NFCR=21
      OPEN(NFCR,FILE='test1',IOSTAT=ISTAT,STATUS='OLD',ERR=10)
   10 WRITE(6,*) 'ISTAT=',ISTAT
C
    1 CONTINUE
         CALL FLREADX(IEND)
         WRITE(6,'(5I14)') (IRDATA(I),I=1,150)
      IF(IEND.NE.0) GOTO 1
      CLOSE(NFCR)
C
      STOP
      END
C
      SUBROUTINE FLREADX(IEND)
C
      COMMON /GSAFRB/ NFCR,IRBUFF,IRDATA(150)
      CHARACTER KK*80
      DIMENSION IK(80),I(3)
C
      IEND=0
      DO 1000 N=1,10
         READ(NFCR,501,END=9000) KK
  501    FORMAT(A80)
         CALL CHRASC(KK,IK,80)
         DO 100 M=1,5
            J=16*M-15
            DO 20 L=1,3
               IL=0
               DO 10 LL=5,1,-1
                  K=IK(J+L*5+LL-5)-32
                  IL=IL*64+K
   10          CONTINUE
               I(L)=IL
   20       CONTINUE
            K=IK(J)-32
            IF(MOD(K,8).GE.4) I(1)=I(1)+1073741824
            IF(MOD(K,4).GE.2) I(2)=I(2)+1073741824
            IF(MOD(K,2).GE.1) I(3)=I(3)+1073741824
            IF(MOD(K,64).GE.32) I(1)=-I(1)
            IF(MOD(K,32).GE.16) I(2)=-I(2)
            IF(MOD(K,16).GE.8)  I(3)=-I(3)
            IRDATA(15*N+3*M-17)=I(1)
            IRDATA(15*N+3*M-16)=I(2)
            IRDATA(15*N+3*M-15)=I(3)
  100    CONTINUE
 1000 CONTINUE
      RETURN
C
 9000 IEND=1
      RETURN
      END
