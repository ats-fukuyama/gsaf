*
*#RUN:OPT=2 L=/GP/PLOT L=/GP/GSP L=/GP/GDP
C
      DIMENSION XA(33),YA(33)
C
      AMP=10.0
      ISYM=0
      ICODE=1
C
      WRITE(6,*) '# INPUT : AMPLITUDE,ISYMBOL(0,1,-1),ICODE'
      READ(5,*,END=9999) AMP,ISYM,ICODE
C
      CALL PLOTS('PAGE001$',0.0)
C      CALL FACTOR(0.5)
      CALL PLOT(1.0,1.0,3)
      CALL PLOT(24.6,1.0,2)
      CALL PLOT(24.6,17.1,2)
      CALL PLOT(1.0,17.1,2)
      CALL PLOT(1.0,1.0,-2)
      CALL SYMBOL(1.0,1.0,0.5,'TEST PLOT',45.0,9)
      DO 10 I=0,15
         CALL SYMBOL(5.0+I,1.0,0.5,I+0,0.0,-1)
   10 CONTINUE
      DO 20 N=1,31
         XA(N)=2*3.141592*(N-1)/30
         YA(N)=AMP*SIN(XA(N))
   20 CONTINUE
      CALL SCALE(XA,10.0,31,1)
      CALL SCALE(YA,10.0,31,1)
      CALL PLOT(5.0,5.0,-3)
      CALL LINE(XA,YA,31,1,ISYM,ICODE)
      CALL AXIS(0.0,0.0,'X AXIS',-6,10.0, 0.0,XA(32),XA(33))
      CALL AXIS(0.0,0.0,'Y AXIS', 6,10.0,90.0,YA(32),YA(33))
      CALL PLOT(0.0,0.0,999)
 9999 STOP
      END

