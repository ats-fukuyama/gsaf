C
C     ****** UNIFORM RANDOM NUMBER GENERATION ******
C
      SUBROUTINE WFRNDU(N,R)
C
      DOUBLE PRECISION R(N)
      COMMON /WFRND1/ IRSEED
C
      IR=IRSEED
      CALL URAND1(N,R,IR)
      IRSEED=IR
C
      RETURN
      END
C
C     ****** NORMAL RANDOM NUMBER GENERATION ******
C
      SUBROUTINE WFRNDN(N,R)
C
      DOUBLE PRECISION R(N)
      COMMON /WFRND1/ IRSEED
C
      IR=IRSEED
      CALL NRAND(N,R,IR)
      IRSEED=IR
      RETURN
      END
C
C     ****** RANDOM NUMBER INTIALIZATION ******
C
      SUBROUTINE WFRNDI(IR)
C
      COMMON /WFRND1/ IRSEED
C
      IRSEED=MOD(IR,1664501)
      RETURN
      END
C
C     ****** UNIFORM RANDOM NUMBER FUNCTION ******
C
      FUNCTION DURAND(D)
C
      REAL*8 D,R,DURAND
      DIMENSION R(1)
C
      CALL WFRNDU(1,R)
      DURAND=D*R(1)
      RETURN
      END
C
      SUBROUTINE URAND1(N, X, IR)
C***********************************************************************
C UNIFORM RANDOM NUMBER GENERATOR (MIXED CONGRUENTIAL METHOD)          *
C     PORTABLE BUT SLOW.  THE PERIOD IS ONLY 1664501.                  *
C PARAMETERS                                                           *
C   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
C                  (INPUT)                                             *
C   (2) X      (D) UNIFORM RANDOM NUMBERS (OUTPUT)                     *
C   (3) IR     (I) THE INITIAL SEED  (INPUT)                           *
C                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
C COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
C***********************************************************************
C
       DOUBLE PRECISION X(N), INVM
       PARAMETER (M = 1664501, LAMBDA = 1229, MU = 351750)
       PARAMETER (INVM = 1.0D0 / M)
CPAREMETER CHECK
      IF( N .LE. 0) THEN
       WRITE(6,*) '(SUBR.URAND1) PARAMETER ERROR. N = ', N
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      END IF
      IF( IR .LT. 0 .OR. IR .GE. M) THEN
       WRITE(6,*) '(SUBR.URAND1) WARNING. IR = ', IR
      END IF
CMAIN LOOP
      DO 10 I = 1, N
       IR = MOD( LAMBDA * IR + MU, M)
       X(I) = IR * INVM
   10 CONTINUE
      RETURN
      END
C
       SUBROUTINE NRAND(N, X, IR)
C***********************************************************************
C NORMAL RANDOM NUMBER GENERATOR (BOX-MULLER METHOD)                   *
C PARAMETERS                                                           *
C   (1) N      (I) THE NUMBER OF RANDOM NUMBERS TO BE GENERATED        *
C                  (INPUT)                                             *
C   (2) X      (D) NORMAL RANDOM NUMBERS (OUTPUT)                      *
C   (3) IR     (I) THE INITIAL SEED FOR UNIFORM RANDOM NUMBER GENERATOR*
C                  (INPUT)                                             *
C                  THE SEED FOR THE NEXT CALL (OUTPUT)                 *
C COPYRIGHT: Y. OYANAGI, JUNE 30, 1989  V.1                            *
C***********************************************************************
       DOUBLE PRECISION X(N), R, T, PI2
       PARAMETER(PI2=2.D0*3.14159265358979D0)
CPAREMETER CHECK
      NN = N
      IF( NN .LE. 0) THEN
       WRITE(6,*) '(SUBR.NRAND) PARAMETER ERROR. N = ', NN
       WRITE(6,*) 'RETURN WITH NO FURTHER CALCULATION.'
       RETURN
      ELSE IF( MOD(N, 2) .NE. 0) THEN
       WRITE(6,*) '(SUBR.NRAND) WARNING. N IS ODD'
       NN=N+1
      END IF
CMAIN LOOP
      CALL URAND1(NN, X, IR)
      DO 10 I = 1, NN, 2
       R=SQRT(-2.0D0*LOG(X(I)))
       T=PI2*X(I+1)
       X(I  ) = R * SIN(T)
       X(I+1) = R * COS(T)
   10 CONTINUE
      RETURN
      END
