C
      SUBROUTINE DEF_IMAGE
C
      INCLUDE 'plasma.inc'
C
      DO IX=1,11
      DO IY=1,11
         X=0.2*(IX-6)
         Y=0.2*(IY-6)
         R1=SQRT(X**2+Y**2)
         R2=SQRT((X+0.3)**2+(Y+0.3)**2)
C
         IF(R2.LT.0.5) THEN
            IB=255-NINT(255*4*R2*R2)
            IR=255
         ELSE
            IR=255-NINT(255*(R2-0.5)**2)
            IF(IR.LE.0) IR=0
            IB=0
         ENDIF
C         
         IF(R1.GE.1.0) THEN
            IR=255
            IB=255
         ELSEIF(R1.GE.0.9) THEN
            IR=IR+(255-IR)*(R1-0.9)/0.1
            IB=IB+(255-IB)*(R1-0.9)/0.1
         ENDIF
C
         IDATA1R(IX,IY)=(IB*256*256+IB*256+IR)*256
         IDATA1G(IX,IY)=(IB*256*256+IR*256+IB)*256
         IDATA1B(IX,IY)=(IR*256*256+IB*256+IB)*256
      ENDDO
      ENDDO
C
      DO IX=1,21
      DO IY=1,21
         X=0.1*(IX-11)
         Y=0.1*(IY-11)
         R1=SQRT(X**2+Y**2)
         R2=SQRT((X+0.3)**2+(Y+0.3)**2)
C
         IF(R2.LT.0.5) THEN
            IB=255-NINT(255*4*R2*R2)
            IR=255
         ELSE
            IR=255-NINT(255*(R2-0.5)**2)
            IF(IR.LE.0) IR=0
            IB=0
         ENDIF
C         
         IF(R1.GE.1.0) THEN
            IR=255
            IB=255
         ELSEIF(R1.GE.0.9) THEN
            IR=IR+(255-IR)*(R1-0.9)/0.1
            IB=IB+(255-IB)*(R1-0.9)/0.1
         ENDIF
C
         IDATA2R(IX,IY)=(IB*256*256+IB*256+IR)*256
         IDATA2G(IX,IY)=(IB*256*256+IR*256+IB)*256
         IDATA2B(IX,IY)=(IR*256*256+IB*256+IB)*256
      ENDDO
      ENDDO
C
      DO IX=1,41
      DO IY=1,41
         X=0.05*(IX-21)
         Y=0.05*(IY-21)
         R1=SQRT(X**2+Y**2)
         R2=SQRT((X+0.3)**2+(Y+0.3)**2)
C
         IF(R2.LT.0.5) THEN
            IB=255-NINT(255*4*R2*R2)
            IR=255
         ELSE
            IR=255-NINT(255*(R2-0.5)**2)
            IF(IR.LE.0) IR=0
            IB=0
         ENDIF
C         
         IF(R1.GE.1.0) THEN
            IR=255
            IB=255
         ELSEIF(R1.GE.0.9) THEN
            IR=IR+(255-IR)*(R1-0.9)/0.1
            IB=IB+(255-IB)*(R1-0.9)/0.1
         ENDIF
C
         IDATA4R(IX,IY)=(IB*256*256+IB*256+IR)*256
         IDATA4G(IX,IY)=(IB*256*256+IR*256+IB)*256
         IDATA4B(IX,IY)=(IR*256*256+IB*256+IB)*256
      ENDDO
      ENDDO
C
      RETURN
      END
