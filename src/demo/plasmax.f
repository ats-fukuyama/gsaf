C     $Id$
C
      IMPLICIT NONE
      INTEGER NBM
      PARAMETER (NBM=4)
      REAL*4 BUTTON_XMIN(NBM),BUTTON_XMAX(NBM)
      REAL*4 BUTTON_YMIN(NBM),BUTTON_YMAX(NBM)
      REAL*4 BUTTON_RED(NBM),BUTTON_GREEN(NBM),BUTTON_BLUE(NBM)
      CHARACTER BUTTON_TEXT(NBM)*32
      INTEGER BUTTON_CLICK(NBM)
      REAL*4 XR(5),YR(5),PX,PY,PW,PH,X,Y,MODE
      INTEGER NB,NBMAX,IW,IH,ID,KID,KEY
      CHARACTER KIN*(1)
C
      CALL GSOPEN
C
      NBMAX=4
      PX=10.0
      PY=16.0
      PW= 2.0
      PH= 1.0
      DO NB=1,NBMAX
         BUTTON_XMIN(NB)=PX
         BUTTON_YMIN(NB)=PY
         PX=PX+PW
         BUTTON_XMAX(NB)=PX
         BUTTON_YMAX(NB)=PY+PH
         IF(NB.EQ.1) THEN
            BUTTON_RED(NB)=    0.9
            BUTTON_GREEN(NB)=  0.5
            BUTTON_BLUE(NB)=   0.5
            BUTTON_TEXT(NB)= 'A'
         ELSEIF(NB.EQ.2) THEN
            BUTTON_RED(NB)=    0.5
            BUTTON_GREEN(NB)=  0.9
            BUTTON_BLUE(NB)=   0.5
            BUTTON_TEXT(NB)= 'B'
         ELSEIF(NB.EQ.3) THEN
            BUTTON_RED(NB)=    0.5
            BUTTON_GREEN(NB)=  0.5
            BUTTON_BLUE(NB)=   0.9
            BUTTON_TEXT(NB)= 'C'
         ELSEIF(NB.EQ.4) THEN
            BUTTON_RED(NB)=    0.9
            BUTTON_GREEN(NB)=  0.5
            BUTTON_BLUE(NB)=   0.9
            BUTTON_TEXT(NB)= 'D'
         ENDIF
         BUTTON_CLICK(NB)=0
      ENDDO
C
      CALL PAGES
      CALL DVINQRES(IW,IH)
      WRITE(6,*) IW,IH
C
      MODE=1
    1 CONTINUE
C
      DO NB=1,NBMAX
         CALL SETRGB(BUTTON_RED(NB),BUTTON_GREEN(NB),BUTTON_BLUE(NB))
         XR(1)=BUTTON_XMIN(NB)
         YR(1)=BUTTON_YMIN(NB)
         XR(2)=BUTTON_XMAX(NB)
         YR(2)=BUTTON_YMIN(NB)
         XR(3)=BUTTON_XMAX(NB)
         YR(3)=BUTTON_YMAX(NB)
         XR(4)=BUTTON_XMIN(NB)
         YR(4)=BUTTON_YMAX(NB)
         XR(5)=BUTTON_XMIN(NB)
         YR(5)=BUTTON_YMIN(NB)
         CALL POLY(XR,YR,4)
         CALL SETRGB(0.0,0.0,0.0)
         CALL LINES(XR,YR,5)
         CALL GTEXT(0.5*(BUTTON_XMIN(NB)+BUTTON_XMAX(NB)),
     &              0.5*(BUTTON_YMIN(NB)+BUTTON_YMAX(NB)),
     &              BUTTON_TEXT(NB),1,2)
      ENDDO
C
      CALL GF_SET_EVENT(7)
C
   10 CONTINUE
C
         CALL GF_GET_EVENT(ID,X,Y,KID,KEY)
         WRITE(6,'(I5,1P2E12.4,I5)') ID,X,Y,KID
         IF(ID.EQ.-1) THEN
            GOTO 9000
         ELSEIF(ID.EQ.1) THEN
            CALL ASCCHR(KID,KIN,1)
            CALL GUCPTL(KIN)
            IF(KIN.EQ.'Q') GOTO 9000
         ELSEIF(ID.EQ.2) THEN
            DO NB=1,NBMAX
               IF(BUTTON_CLICK(NB).EQ.1) THEN
                  IF((X.GT.BUTTON_XMIN(NB)).AND.
     &               (X.LT.BUTTON_XMAX(NB)).AND.
     &               (Y.GT.BUTTON_YMIN(NB)).AND.
     &               (Y.LT.BUTTON_YMAX(NB))) THEN
                     BUTTON_CLICK(NB)=0
                     XR(1)=BUTTON_XMIN(NB)
                     YR(1)=BUTTON_YMIN(NB)
                     XR(2)=BUTTON_XMAX(NB)
                     YR(2)=BUTTON_YMIN(NB)
                     XR(3)=BUTTON_XMAX(NB)
                     YR(3)=BUTTON_YMAX(NB)
                     XR(4)=BUTTON_XMIN(NB)
                     YR(4)=BUTTON_YMAX(NB)
                     CALL SETRGB(0.0,0.0,0.0)
                     CALL GF_SET_GCFUNC(6)
                     CALL POLY(XR,YR,4)
                     CALL GF_SET_GCFUNC(3)
                     MODE=NB
                     CALL ERAS
                     GOTO 1
                  ELSE
                     BUTTON_CLICK(NB)=0
                     XR(1)=BUTTON_XMIN(NB)
                     YR(1)=BUTTON_YMIN(NB)
                     XR(2)=BUTTON_XMAX(NB)
                     YR(2)=BUTTON_YMIN(NB)
                     XR(3)=BUTTON_XMAX(NB)
                     YR(3)=BUTTON_YMAX(NB)
                     XR(4)=BUTTON_XMIN(NB)
                     YR(4)=BUTTON_YMAX(NB)
                     CALL SETRGB(0.0,0.0,0.0)
                     CALL GF_SET_GCFUNC(6)
                     CALL POLY(XR,YR,4)
                     CALL GF_SET_GCFUNC(3)
                  ENDIF
               ENDIF
            ENDDO
         ELSEIF(ID.EQ.4) THEN
            DO NB=1,NBMAX
               IF((X.GT.BUTTON_XMIN(NB)).AND.
     &            (X.LT.BUTTON_XMAX(NB)).AND.
     &            (Y.GT.BUTTON_YMIN(NB)).AND.
     &            (Y.LT.BUTTON_YMAX(NB))) THEN
                  BUTTON_CLICK(NB)=1
                  XR(1)=BUTTON_XMIN(NB)
                  YR(1)=BUTTON_YMIN(NB)
                  XR(2)=BUTTON_XMAX(NB)
                  YR(2)=BUTTON_YMIN(NB)
                  XR(3)=BUTTON_XMAX(NB)
                  YR(3)=BUTTON_YMAX(NB)
                  XR(4)=BUTTON_XMIN(NB)
                  YR(4)=BUTTON_YMAX(NB)
                  CALL SETRGB(0.0,0.0,0.0)
                  CALL GF_SET_GCFUNC(6)
                  CALL POLY(XR,YR,4)
                  CALL GF_SET_GCFUNC(3)
                  GOTO 10
               ENDIF
            ENDDO
         ENDIF
         WRITE(6,*) MODE
      GOTO 10
C
 9000 CALL GF_SET_EVENT(0)
      CALL PAGEY
C
      CALL GSCLOS
      STOP
      END
