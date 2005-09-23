C
      SUBROUTINE BUTTON1
C
      INCLUDE 'button.inc'
C
      REAL*4 PX,PY,PW,PH
      INTEGER NB,NBMAX
C
      NBMAX=10
      PX= 2.8
      PY=17.0
      PW= 2.0
      PH= 1.0
      DO NB=1,NBMAX
         BUTTON_XMIN(NB)=PX
         BUTTON_YMIN(NB)=PY
         PX=PX+PW
         BUTTON_XMAX(NB)=PX
         BUTTON_YMAX(NB)=PY+PH
         IF(NB.EQ.1) THEN
            BUTTON_RED(NB)=   1.0
            BUTTON_GREEN(NB)= 0.7
            BUTTON_BLUE(NB)=  0.7
            BUTTON_TEXT(NB)= 'G'
            BUTTON_NCHAR(NB)= 1
         ELSEIF(NB.EQ.2) THEN
            BUTTON_RED(NB)=   1.0
            BUTTON_GREEN(NB)= 0.7
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'P1'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.3) THEN
            BUTTON_RED(NB)=   1.0
            BUTTON_GREEN(NB)= 0.7
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'P2'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.4) THEN
            BUTTON_RED(NB)=   1.0
            BUTTON_GREEN(NB)= 0.7
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'P3'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.5) THEN
            BUTTON_RED(NB)=   1.0
            BUTTON_GREEN(NB)= 0.7
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'P4'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.6) THEN
            BUTTON_RED(NB)=   1.0
            BUTTON_GREEN(NB)= 0.7
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'P5'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.7) THEN
            BUTTON_RED(NB)=   0.7
            BUTTON_GREEN(NB)= 1.0
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'M1'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.8) THEN
            BUTTON_RED(NB)=   0.7
            BUTTON_GREEN(NB)= 1.0
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'M2'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.9) THEN
            BUTTON_RED(NB)=   0.7
            BUTTON_GREEN(NB)= 1.0
            BUTTON_BLUE(NB)=  1.0
            BUTTON_TEXT(NB)= 'M32'
            BUTTON_NCHAR(NB)= 2
         ELSEIF(NB.EQ.10) THEN
            BUTTON_RED(NB)=   0.8
            BUTTON_GREEN(NB)= 0.8
            BUTTON_BLUE(NB)=  0.8
            BUTTON_TEXT(NB)= 'Q'
            BUTTON_NCHAR(NB)= 1
         ENDIF
         BUTTON_CLICK(NB)=0
      ENDDO
      RETURN
      END
C
      SUBROUTINE SET_BUTTON
C
      INCLUDE 'button.inc'
C
      REAL*4 XR(5),YR(5)
      INTEGER NB
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
     &              0.5*(BUTTON_YMIN(NB)+BUTTON_YMAX(NB))-0.175,
     &              BUTTON_TEXT(NB),BUTTON_NCHAR(NB),2)
      ENDDO
      RETURN
      END
C
      SUBROUTINE REVERSE_BUTTON(NB)
C
      INCLUDE 'button.inc'
C
      REAL*4 XR(4),YR(4)
C
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
      RETURN
      END
