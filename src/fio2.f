C     $Id$
C
C     ****** READ GRAPHIC DATA FILE ******
C
      SUBROUTINE GSREAD(NFC,NPR)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE)      RETURN
      IF(NFC.LT.1.OR.NFC.GT.63) RETURN
C
      IF(NPR.GT.0) THEN
         CALL PGRLOC(NFC,NPR,IEND)
         IF(IEND.EQ.0) THEN
            CALL PGCOMB(NFC,1,1)
          ELSE
            WRITE(6,*) 'XX PAGE ',NPR,' NOT FOUND.'
         ENDIF
      ELSEIF(NPR.EQ.0) THEN
    1    CALL PGRLOC(NFC,0,IEND)
         IF(IEND.EQ.0) THEN
            CALL PGCOMB(NFC,1,1)
            GOTO 1
         ENDIF
      ELSE
         DO 10 I=1,-NPR
            CALL PGRLOC(NFC,0,IEND)
            IF(IEND.NE.0) THEN
               CALL PGCOMB(NFC,1,1)
            ENDIF
   10    CONTINUE
      ENDIF
      RETURN
      END
C
C     ****** COMBINE MULTIPLE PAGES IN A FILE ******
C
      SUBROUTINE GSCOMB(NFC,I,ITL)
C
    1 CALL PGRLOC(NFC,0,IEND)
      IF(IEND.EQ.0) THEN
         CALL PGCOMB(NFC,I,ITL)
         GOTO 1
      ENDIF
      RETURN
      END
C
C     ****** GET LIST OF GRAPHIC PAGE IN FILE ******
C
      SUBROUTINE GSLIST(NFC,NPAGEA,NA,NP)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFRB/ NFCR,IRBUFF,IRDATA(150)
      DIMENSION NPAGEA(NA)
C
      IF(.NOT.LGSAF) RETURN
      IF(LPAGE)      RETURN
C
      IF(NFC.LT.1.OR.NFC.GT.63) RETURN
C
      NFCR=NFC
      NP=0
      NPAGEP=0
C
    1 CONTINUE
         CALL FLREAD(IEND)
         IF(IEND.NE.0) GOTO 9900
         NPAGEF=IRDATA(1)/65536
         IF(NPAGEF.NE.NPAGEP) THEN
            NP=NP+1
            NPAGEA(NP)=NPAGEF
            NPAGEP=NPAGEF
            IF(NP.EQ.NA) GOTO 9900
         ENDIF
      GOTO 1
C
 9900 REWIND NFC
      RETURN
      END
C
C     ****** COMBINE MULTIPLE PAGES IN A FILE ******
C
      SUBROUTINE PGCOMB(NFC,I,ITL)
C
      IF(ITL.EQ.1) THEN
         CALL GSTITL('//')
         FACT2=0.666
         FACT8=0.333
         ITLL=1
      ELSEIF(ITL.EQ.-1) THEN
         CALL GSTITL('//')
         FACT2=0.701
         FACT8=0.3505
         ITLL=0
      ELSE
         FACT2=0.701
         FACT8=0.3505
         ITLL=0
      ENDIF
C
      CALL PAGES
C
      IF(I.EQ.1) THEN
         CALL PGREAD(NFC,1.0,1.0,0.0,0.0,0.0,ITLL)
C
      ELSEIF(I.LE.2) THEN
         CALL PGREAD(NFC,FACT2,FACT2,12.8,18.1,-90.0,ITLL)
         CALL PGRLOC(NFC,0,IEND)
         IF(IEND.NE.0) GOTO 9000
         CALL PGREAD(NFC,FACT2,FACT2, 0.0,18.1,-90.0,ITLL)
C
      ELSEIF(I.LE.4) THEN
         DO 210 IX=0,1
         DO 210 IY=0,1
            IP=2*IX+IY+1
            CALL PGREAD(NFC,0.5,0.5,
     &                  IX*12.8,9.05-IY*9.05,0.0,ITLL)
            CALL PGRLOC(NFC,0,IEND)
            IF(IEND.NE.0) GOTO 9000
            IF(IP.EQ.I) GOTO 211
  210    CONTINUE
  211    CONTINUE
C
      ELSEIF(I.LE.8) THEN
         DO 310 IX=0,1
         DO 310 IY=0,3
            IP=4*IX+IY+1
            CALL PGREAD(NFC,FACT8,FACT8,
     &                  19.2-IY*6.4,18.1-IX*9.05,-90.0,ITLL)
            CALL PGRLOC(NFC,0,IEND)
            IF(IEND.NE.0) GOTO 9000
            IF(IP.EQ.I) GOTO 311
  310    CONTINUE
  311    CONTINUE
C
      ELSE
         DO 410 IX=0,3
         DO 410 IY=0,3
            IP=4*IX+IY+1
            CALL PGREAD(NFC,0.25,0.25,
     &                  IX*6.4,13.575-IY*4.525,0.0,ITLL)
            CALL PGRLOC(NFC,0,IEND)
            IF(IEND.NE.0) GOTO 9000
            IF(IP.EQ.I) GOTO 411
  410    CONTINUE
  411    CONTINUE
      ENDIF
C
 9000 CALL PAGEE
      IF(ITL.EQ.1.OR.ITL.EQ.-1) THEN
         CALL GSTITL('/ /')
      ENDIF
      RETURN
      END
C
C     ****** READ GRAPHIC PAGE DATA ******
C
      SUBROUTINE PGREAD(NFC,XFACT1,YFACT1,XOFS,YOFS,ROT,ITL)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFRB/ NFCR,IRBUFF,IRDATA(150)
      COMMON /GSAFRD/ NPAGER
      COMMON /GSAFFN/ IFNTR,LSOFT
      DIMENSION IASC(256),IXA(1024),IYA(1024)
      DIMENSION IXTR(3),IYTR(3),IR(3),IG(3),IB(3)
C
      IF(.NOT.LPAGE) RETURN
C
      IF(NFC.LT.1.OR.NFC.GT.63) RETURN
      NFCR=NFC
C
C      NPAGEF=IRDATA(1)/65536
      PX=IRDATA(2)*0.00001
      PY=IRDATA(3)*0.00001
      IRBUFF=3
      FACT=MIN(25.6/PX,18.1/PY)
      XFACT=FACT*XFACT1
      YFACT=FACT*YFACT1
      IYTL=NINT(PY*FACT*23168/18.1)+1
      IXOFS=NINT(XOFS*DSIZE)
      IYOFS=NINT(YOFS*DSIZE)
      COSR=COS(ROT*3.141592/180.0)
      SINR=SIN(ROT*3.141592/180.0)
      XFC=XFACT*COSR
      XFS=XFACT*SINR
      YFC=YFACT*COSR
      YFS=YFACT*SINR
      XYFACT=SQRT(XFACT*YFACT)
C
  100 CONTINUE
      IRBUFF=IRBUFF+1
      ID=IRDATA(IRBUFF)
      IF(ID.GE.0) THEN
         IX=ID/65536
         IY=ID-IX*65536
         IP=IY/32768
         IY=IY-IP*32768
         IF(ITL.NE.0.OR.IY.LE.IYTL) THEN
            IXF=NINT(IX*XFC-IY*YFS)+IXOFS
            IYF=NINT(IX*XFS+IY*YFC)+IYOFS
            IF(IP.EQ.0) THEN
               CALL DVMOVE(IXF,IYF)
            ELSEIF(IP.EQ.1) THEN
               CALL DVDRAW(IXF,IYF)
            ENDIF
            IF(LFIL) CALL BUFFXY(IP,IXF,IYF)
         ENDIF
      ELSE
         ID=-ID
         IF(ID.EQ.1) THEN
            GO TO 9000
         ELSE
            IX=ID/65536
            IY=ID-IX*65536
            IP=IY/32768
            IY=IY-IP*32768
            IF(IP.EQ.1) THEN
               IF(ITL.NE.0.OR.IY.LE.IYTL) THEN
                  IXF=NINT(IX*XFC-IY*YFS)+IXOFS
                  IYF=NINT(IX*XFS+IY*YFC)+IYOFS
                  CALL GS_SFMARK(IXF,IYF)
                  IF(LFIL) CALL BUFFXY(2,IXF,IYF)
               ENDIF
            ELSE
               IND=IX
               IF(IND.LT.10) THEN
                  I1=IY-16384
                  IF(IRBUFF.GE.150) CALL FLBUFR
                  IRBUFF=IRBUFF+1
                  ID=IRDATA(IRBUFF)
                  I2=ID/65536
                  I3=ID-I2*65536
                  I2=I2-16384
                  I3=I3-16384
                  IF(IRBUFF.GE.150) CALL FLBUFR
                  IRBUFF=IRBUFF+1
                  R1=IRDATA(IRBUFF)*0.00001
                  IF(IRBUFF.GE.150) CALL FLBUFR
                  IRBUFF=IRBUFF+1
                  R2=IRDATA(IRBUFF)*0.00001
                  IF(IND.EQ.1) THEN
                     I2=NINT(I2*XYFACT)
                     CALL DVSTLN(I1,I2,I3)
                  ELSEIF(IND.EQ.2.OR.IND.EQ.3) THEN
                     I1=NINT(I1*YFACT)
                     I2=NINT(I2*XFACT)
                     I3=NINT(I3*XFACT)
                     R1=R1+ROT
                     IF(LSOFT) THEN
                        CALL GS_SFSTCH(I1,I2,I3,R1,R2)
                     ELSE
                        CALL DVSTCH(I1,I2,I3,R1,R2,INDD)
                     ENDIF
                  ELSEIF(IND.EQ.4) THEN
                     I2=NINT(I2*YFACT)
                     I3=NINT(I3*XFACT)
                     R1=R1+ROT
                     CALL GS_SFSTMK(I1,I2,I3,R1,R2)
                  ELSEIF(IND.EQ.5) THEN
                     CALL DVCRGB(I1,I2,I3)
                  ELSEIF(IND.EQ.6) THEN
                     I1=NINT(I1*XYFACT)
                     CALL DVLWDT(I1)
                  ELSEIF(IND.EQ.7) THEN
                     CALL GS_SETFNT(I1)
                  ELSEIF(IND.EQ.8) THEN
                     IF(I1.EQ.0) THEN
                        CALL DVGRPS
                     ELSE
                        CALL DVGRPE
                     ENDIF
                  ELSEIF(IND.EQ.9) THEN
                     CALL DVGCFUNC(I1)
                  ENDIF
                  IF(LFIL) CALL BUFFST(IND,I1,I2,I3,R1,R2)
               ELSEIF(IND.EQ.10) THEN
                  NCHAR=IY
                  IF(IRBUFF.GE.150) CALL FLBUFR
                  IRBUFF=IRBUFF+1
                  ID=IRDATA(IRBUFF)
                  IX=ID/65536
                  IY=ID-IX*65536
                  IXF=NINT(IX*XFC-IY*YFS)+IXOFS
                  IYF=NINT(IX*XFS+IY*YFC)+IYOFS
                  DO 300 I=1,(NCHAR+3)/4
                     IF(IRBUFF.GE.150) CALL FLBUFR
                     IRBUFF=IRBUFF+1
                     IT=IRDATA(IRBUFF)
                     II=4*(I-1)
                     IASC(II+1)=IT/16777216
                     IT=IT-16777216*IASC(II+1)
                     IASC(II+2)=IT/65536
                     IT=IT-65536*IASC(II+2)
                     IASC(II+3)=IT/256
                     IASC(II+4)=IT-256*IASC(II+3)
  300             CONTINUE
                  IF(ITL.NE.0.OR.IY.LE.IYTL) THEN
                     IF(LFIL) CALL BUFFTX(IXF,IYF,IASC,NCHAR)
                     IF(LSOFT) THEN
                        CALL GS_SFTEXT(IXF,IYF,IASC,NCHAR)
                     ELSE
                        CALL DVTEXT(IXF,IYF,IASC,NCHAR)
                     ENDIF
                  ENDIF
               ELSEIF(IND.EQ.11.OR.IND.EQ.12.OR.IND.EQ.13) THEN
                  NDATA=IY
                  J=0
                  DO 400 I=1,NDATA
                     IF(IRBUFF.GE.150) CALL FLBUFR
                     IRBUFF=IRBUFF+1
                     ID=IRDATA(IRBUFF)
                     IX=ID/65536
                     IY=ID-IX*65536
                     IF(ITL.NE.0.OR.IY.LE.IYTL) THEN
                        J=J+1
                        IXA(J)=NINT(IX*XFC-IY*YFS)+IXOFS
                        IYA(J)=NINT(IX*XFS+IY*YFC)+IYOFS
                     ENDIF
  400             CONTINUE
                  IF(IND.EQ.11) THEN
                     CALL DVLINS(IXA,IYA,J)
                  ELSEIF(IND.EQ.12) THEN
                     CALL DVPOLY(IXA,IYA,J)
                  ELSEIF(IND.EQ.13) THEN
                     IXTR(1)=IXA(1)
                     IXTR(2)=IXA(4)
                     IXTR(3)=IXA(7)
                     IYTR(1)=IYA(1)
                     IYTR(2)=IYA(4)
                     IYTR(3)=IYA(7)
                     IR(1)=IXA(2)
                     IG(1)=IYA(2)
                     IB(1)=IXA(3)
                     IR(2)=IXA(5)
                     IG(2)=IYA(5)
                     IB(2)=IXA(6)
                     IR(3)=IXA(8)
                     IG(3)=IYA(8)
                     IB(3)=IXA(9)
C                     DO NN=1,3
C                        WRITE(6,*) IXTR(NN),IYTR(NN),IR(NN),IG(NN),IB(NN)
C                     ENDDO
                     CALL DVRGBTRG(IXTR,IYTR,IR,IG,IB)
                  ENDIF
                  IF(LFIL) CALL BUFFXN(IND,IXA,IYA,J)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF(IRBUFF.GE.150) CALL FLBUFR
      GO TO 100
C
 9000 CONTINUE
      CALL FLREAD(IEND)
      IF(IEND.EQ.0) THEN
         NPAGER=IRDATA(1)/65536
      ELSE
         NPAGER=-1
      ENDIF
      RETURN
      END
C
C     ****** LOCATE PAGE IN A FILE ******
C
      SUBROUTINE PGRLOC(NFC,NP,IEND)
C
      COMMON /GSAFRB/ NFCR,IRBUFF,IRDATA(150)
      COMMON /GSAFRD/ NPAGER
C
      NFCR=NFC
      NPR=NP
      IF(NPR.EQ.-1) THEN
         REWIND NFC
         NPAGER=0
         NPR=0
      ENDIF
C
      IF(NPAGER.EQ.0) THEN
         CALL FLREAD(IEND)
         IF(IEND.EQ.0) THEN
            NPAGER=IRDATA(1)/65536
    1       IF(NPR.NE.0.AND.NPR.NE.NPAGER) THEN
               CALL FLREAD(IEND)
               IF(IEND.EQ.0) THEN
                  NPAGER=IRDATA(1)/65536
                  GOTO 1
               ELSE
                  NPAGER=-1
               ENDIF
            ENDIF
         ELSE
            NPAGER=-1
         ENDIF
      ELSE
    2    IF(NPR.NE.0.AND.NPR.NE.NPAGER) THEN
            CALL FLREAD(IEND)
            IF(IEND.EQ.0) THEN
               NPAGER=IRDATA(1)/65536
               GOTO 2
            ELSE
               REWIND NFC
    3          IF(NPR.NE.NPAGER) THEN
                  CALL FLREAD(IEND)
                  IF(IEND.EQ.0) THEN
                     NPAGER=IRDATA(1)/65536
                     GOTO 3
                  ELSE
                     NPAGER=-1
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF(NPAGER.EQ.-1) THEN
         IEND=1
      ELSE
         IEND=0
      ENDIF
      RETURN
      END
C
C     ++++++ READ FROM FILE BUFFER ++++++
C
      SUBROUTINE FLBUFR
C
      COMMON /GSAFRB/ NFCR,IRBUFF,IRDATA(150)
C
      CALL FLREAD(IEND)
      IF(IEND.NE.0) GOTO 9900
      IRBUFF=1
      RETURN
C
 9900 CALL PAGEE
      WRITE(6,601) NFCR
      CALL GSCLOS
      STOP
  601 FORMAT(1H ,'XX ABNORMAL END OF READ FILE : FC = ',I2)
      END
C
C     ++++++ CONVERT FROM TEXT DATA TO BINARY DATA ++++++
C
      SUBROUTINE FLREAD(IEND)
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
