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

C     
      SUBROUTINE INITDRAW_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      CALL GF_SET_GCFUNC(6)
      DO NP=1,NPMAX
         CALL GF_PUTIMAGE(IPD(NP),PX(1,NP),PX(2,NP))
      ENDDO
      CALL GF_SET_GCFUNC(3)
      CALL GU_XFLUSH
C
      RETURN
      END
C
      SUBROUTINE DRAW_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      CALL GF_SET_GCFUNC(6)
      DO NP=1,NPMAX
         CALL GF_PUTIMAGE(IPD(NP),PX(1,NP),PX(2,NP))
         PX(1,NP)=PXN(1,NP)
         PX(2,NP)=PXN(2,NP)
         PV(1,NP)=PVN(1,NP)
         PV(2,NP)=PVN(2,NP)
         CALL GF_PUTIMAGE(IPD(NP),PX(1,NP),PX(2,NP))
      ENDDO
      CALL GF_SET_GCFUNC(3)
      CALL GU_XFLUSH
C
      RETURN
      END
C
      SUBROUTINE INIT_GAS_PARTICLES
C
      INCLUDE 'plasma.inc'
      REAL*8 R(2)
C
      IF(MRSEED.EQ.0) THEN
         CALL GUDATE(NDY1,NDM1,NDD1,NTH1,NTM1,NTS1)
         IR=((NDD1*24+NTH1)*60+NTM1)*60+NTS1
      ELSE
         IR=MRSEED
      ENDIF
      CALL WFRNDI(IR)
C
      DO NP=1,NPMAX
         IPD(NP)=0
         CALL WFRNDU(2,R)
         PX(1,NP)=XMIN+GUCLIP(R(1))*XLEN
         PX(2,NP)=YMIN+GUCLIP(R(2))*YLEN
         CALL WFRNDN(2,R)
         PV(1,NP)=GUCLIP(R(1))*VT
         PV(2,NP)=GUCLIP(R(2))*VT
      ENDDO
      RETURN
      END
C
      SUBROUTINE INIT_PLASMA_PARTICLES
C
      INCLUDE 'plasma.inc'
      REAL*8 R(2)
C
      IF(MRSEED.EQ.0) THEN
         CALL GUDATE(NDY1,NDM1,NDD1,NTH1,NTM1,NTS1)
         IR=((NDD1*24+NTH1)*60+NTM1)*60+NTS1
      ELSE
         IR=MRSEED
      ENDIF
      CALL WFRNDI(IR)
C
      DO NP=1,NPMAX
         IF(NP.LE.NPMAX/2) THEN
            IPD(NP)=0
            VTP=VT
         ELSE
            IPD(NP)=1
            VTP=VT/3.0
         ENDIF
         CALL WFRNDU(2,R)
         PX(1,NP)=XMIN+GUCLIP(R(1))*XLEN
         PX(2,NP)=YMIN+GUCLIP(R(2))*YLEN
         CALL WFRNDN(2,R)
         PV(1,NP)=GUCLIP(R(1))*VTP
         PV(2,NP)=GUCLIP(R(2))*VTP
      ENDDO
      RETURN
      END
C
      SUBROUTINE PUSH_GAS_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      DO NP=1,NPMAX
         PVN(1,NP)=PV(1,NP)
         PVN(2,NP)=PV(2,NP)
      ENDDO
C
      DO NP=1,NPMAX
         DO NP1=NP+1,NPMAX
            XD=PX(1,NP1)-PX(1,NP)
            YD=PX(2,NP1)-PX(2,NP)
            R=SQRT(XD*XD+YD*YD)
            IF(R.LT.0.02) THEN
               FX=-FG*XD/R
               FY=-FG*YD/R
               FXP=FX
               FYP=FY
               PVN(1,NP)=PVN(1,NP)+FXP*DT
               PVN(2,NP)=PVN(2,NP)+FYP*DT
               PVN(1,NP1)=PVN(1,NP1)-FXP*DT
               PVN(2,NP1)=PVN(2,NP1)-FYP*DT
            ENDIF
         ENDDO
      ENDDO
C
      DO NP=1,NPMAX
         PXN(1,NP)=PX(1,NP)+PVN(1,NP)*DT
         PXN(2,NP)=PX(2,NP)+PVN(2,NP)*DT
         IF(PXN(1,NP).LT.XMIN) THEN
            PXN(1,NP)=2*XMIN-PXN(1,NP)
            PVN(1,NP)=-PVN(1,NP)
         ELSEIF(PXN(1,NP).GT.XMAX) THEN
            PXN(1,NP)=2*XMAX-PXN(1,NP)
            PVN(1,NP)=-PVN(1,NP)
         ENDIF
         IF(PXN(2,NP).LT.YMIN) THEN
            PXN(2,NP)=2*YMIN-PXN(2,NP)
            PVN(2,NP)=-PVN(2,NP)
         ELSEIF(PXN(2,NP).GT.YMAX) THEN
            PXN(2,NP)=2*YMAX-PXN(2,NP)
            PVN(2,NP)=-PVN(2,NP)
         ENDIF
      ENDDO
      RETURN
      END
C
      SUBROUTINE PUSH_PLASMA_PARTICLES
C
      INCLUDE 'plasma.inc'
C
      DO NP=1,NPMAX
         FBX=0.50*FB*DT
         IF(IPD(NP).EQ.0) THEN 
            FBX=-FBX
         ELSE
            FBX=FBX/9.0
         ENDIF
         FB1=(1-0.25*FBX*FBX)/(1+0.25*FBX*FBX)
         FB2=FBX/(1+0.25*FBX*FBX)
         PVN(1,NP)= FB1*PV(1,NP)+FB2*PV(2,NP)
         PVN(2,NP)=-FB2*PV(1,NP)+FB1*PV(2,NP)
      ENDDO
C
      DO NP=1,NPMAX
         DO NP1=NP+1,NPMAX
            XD=PX(1,NP1)-PX(1,NP)
            YD=PX(2,NP1)-PX(2,NP)
            R=SQRT(XD*XD+YD*YD)
            FX=FE*XD/(R*R)
            FY=FE*YD/(R*R)
            IF(IPD(NP).EQ.IPD(NP1)) THEN
               FX=-FX
               FY=-FY
            ENDIF
            IF(IPD(NP).EQ.0) THEN
               FXP=FX
               FYP=FY
            ELSE
               FXP=FX/9.0
               FYP=FY/9.0
            ENDIF
            PVN(1,NP)=PVN(1,NP)+FXP*DT
            PVN(2,NP)=PVN(2,NP)+FYP*DT
            IF(IPD(NP1).EQ.0) THEN
               FXP=FX
               FYP=FY
            ELSE
               FXP=FX/9.0
               FYP=FY/9.0
            ENDIF
            PVN(1,NP1)=PVN(1,NP1)-FXP*DT
            PVN(2,NP1)=PVN(2,NP1)-FYP*DT
         ENDDO
         XPL=PX(1,NP)
         ARG=(PX(1,NP)-0.50*(XMIN+XMAX))/XLE
         FXP=FEX*EXP(-ARG**2)
         FYP=0.0
         PVN(1,NP)=PVN(1,NP)+FXP*DT
         PVN(2,NP)=PVN(2,NP)+FYP*DT
      ENDDO
C
      DO NP=1,NPMAX
         PV(1,NP)=PVN(1,NP)
         PV(2,NP)=PVN(2,NP)
      ENDDO
      DO NP=1,NPMAX
         FBX=0.50*FB*DT
         IF(IPD(NP).EQ.0) THEN 
            FBX=-FBX
         ELSE
            FBX=FBX/9.0
         ENDIF
         FB1=(1-0.25*FBX*FBX)/(1+0.25*FBX*FBX)
         FB2=FBX/(1+0.25*FBX*FBX)
         PVN(1,NP)= FB1*PV(1,NP)+FB2*PV(2,NP)
         PVN(2,NP)=-FB2*PV(1,NP)+FB1*PV(2,NP)
      ENDDO
C
      DO NP=1,NPMAX
         PXN(1,NP)=PX(1,NP)+PVN(1,NP)*DT
         PXN(2,NP)=PX(2,NP)+PVN(2,NP)*DT
         IF(PXN(1,NP).LT.XMIN) THEN
            PXN(1,NP)=2*XMIN-PXN(1,NP)
            PVN(1,NP)=-PVN(1,NP)
         ELSEIF(PXN(1,NP).GT.XMAX) THEN
            PXN(1,NP)=2*XMAX-PXN(1,NP)
            PVN(1,NP)=-PVN(1,NP)
         ENDIF
         IF(PXN(2,NP).LT.YMIN) THEN
            PXN(2,NP)=2*YMIN-PXN(2,NP)
            PVN(2,NP)=-PVN(2,NP)
         ELSEIF(PXN(2,NP).GT.YMAX) THEN
            PXN(2,NP)=2*YMAX-PXN(2,NP)
            PVN(2,NP)=-PVN(2,NP)
         ENDIF
      ENDDO
      RETURN
      END
