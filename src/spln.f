C     $Id$
C
C     ********************************************
C     ****** GSAF APLLICATION V3.5 : SPLINE ******
C     ********************************************
C
C     ##########     SPLINE 1D     ##########
C
      SUBROUTINE  SPLN1D(X,F,C,NX,XN,FN,NXN,IP)
C
C     X(NX)           : ORIGINAL COORDINATES
C     F(NX)           : ORIGINAL DATA
C     C(NX,3)         : WORKING AREA
C                     : DF/DX (ON BOUNDARY NX=1 OR NI, MAY BE GIVEN)
C     NX              : NUMBER OF ORIGINAL DATA IN X OR Y DIMENSION
C     XN(NXN)         : NEW COORDINATES
C     FN(NXN)         : SPLINED NEW DATA
C     NXN             : NUMBER OF NEW DATA IN X OR Y DIMENSION
C     IP              : 0 : CALCULATE DF INTERNALLY
C                       1 : DF IS GIVEN IN C(1,1) AND C(NX,1)
C                       2 : DDF IS GIVEN IN C(1,1) AND C(NX,1)
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  X(NX),F( NX)
      DIMENSION  C(NX,3)
      DIMENSION  XN(NXN),FN(NXN)
      DIMENSION  DF(2),IOPT(2)
C
      IF(IP.EQ.0) THEN
         IOPT(1)=3
         IOPT(2)=3
      ELSEIF(IP.EQ.1) THEN
         IOPT(1)=2
         IOPT(2)=2
         DF(1)=C(1,1)
         DF(2)=C(NX,1)
      ELSEIF(IP.EQ.2) THEN
         IOPT(1)=1
         IOPT(2)=1
         DF(1)=C(1,1)
         DF(2)=C(NX,1)
      ENDIF
      CALL GUSPLC(X, NX, F, DF, IOPT, C, NX, IER)
      CALL GUSPLF(X, NX, F, C, NX, XN, NXN, FN, IER)
      RETURN
      END
C
      SUBROUTINE GUSPLC(X, N, Y, DF, IOPT, C, NC, IER)
C***********************************************************************
C  COMPUTE THE COEFFICIENTS OF THE CUBIC SPLINE.                       *
C  PARAMETERS                                                          *
C    (1) X: 1-DIM. ARRAY FOR KNOWN POINTS                              *
C    (2) N: NUMBER OF KNOWN POINTS                                     *
C    (3) Y: 1-DIM. ARRAY FOR FUNCTION'S VALUES ON KNOWN POINTS         *
C    (4) DF: 1-DIM. ARRAY FOR DIFFERENTIALS AT END POINTS              *
C    (5) IOPT: 1-DIM. ARRAY SPECIFYING THE CONTENT OF DF               *
C    (6) C: 2-DIM. WORKING ARRAY                                       *
C    (7) NC: ROW SIZE OF THE ARRAY (C)                                 *
C    (8) IER: ERROR CODE                                               *
C  COPYRIGHT   T. OGUNI   JUNE 30 1989    VERSION 1.0                  *
C***********************************************************************
       IMPLICIT REAL*4(A-H,O-Z)
       DIMENSION X(N), Y(N), DF(2), IOPT(2), C(NC,3), EC(4), D(2)
C
      IF (N.LT.2 .OR. NC.LT.N-1 .OR. IOPT(1).LT.1 .OR. IOPT(1).GT.3
     &    .OR. IOPT(2).LT.1 .OR. IOPT(2).GT.3) THEN
       IER = 2
       WRITE(*,*) '(SUBR. SPLC) INVALID ARGUMENT.',N,NC,IOPT(1),IOPT(2)
       RETURN
      ENDIF
      DO 5 I=1,N-1
       IF (X(I) .GE. X(I+1)) THEN
        IER = 1
        WRITE(*,*) '(SUBR. SPLC) X SHOULD SATISFY UPWARD ORDER.'
        RETURN
       ENDIF
    5 CONTINUE
      IER = 0
C  SET THE END CONDITIONS.
      II = 2
      KS = 1
      KE = MIN0(4,N)
      IDER = 1
      DO 70 I=1,2
       I1 = 2 * I - 1
       I2 = 2 * I
       IB = IOPT(I)
       GO TO (10, 20, 30), IB
   10  EC(I1) = 0.0
       EC(I2) = 2.0 * DF(I)
       GO TO 70
   20  D(I) = DF(I)
   25  IF (I .EQ. 2) II = N
       H = X(II) - X(II-1)
       EC(I1) = 1.0
       HY = Y(II) - Y(II-1)
       EC(I2) = 6.0 * (HY / H - D(I)) / H
       IF (I .EQ. 2) EC(I2) = - EC(I2)
       GO TO 70
   30  IF (I .NE. 1) THEN
        KS = MAX0(1,N-3)
        KE = N
        IDER = N
       ENDIF
       A2 = 0.0
       D(I) = 0.0
       DO 60 K=KS,KE
        IF (IDER .NE. K) THEN
         A1 = 1.0
         DO 50 J=KS,KE
          IF (J .NE. IDER .AND. J .NE. K) THEN
           X1 = X(IDER) - X(J)
           X2 = X(K) - X(J)
           A1 = A1 * X1 / X2
          ENDIF
   50    CONTINUE
         X3 = X(K) - X(IDER)
         D(I) = D(I) + A1 * Y(K) / X3
         A2 = A2 - 1.0 / X3
        ENDIF
   60  CONTINUE
       D(I) = D(I) + Y(IDER) * A2
       GO TO 25
   70 CONTINUE
C  SET THE ELEMENTS FOR THE SYMMETRIC TRIDIAGONAL EQUATION.
      IF (N .NE. 2) THEN
       H1 = X(2) - X(1)
       Y1 = Y(2) - Y(1)
       DO 80 I=2,N-1
        H2 = X(I+1) - X(I)
        Y2 = Y(I+1) - Y(I)
        HH = H1 + H2
        C(I,1) = H2 / HH
        C(I,2) = 1.0 - C(I,1)
        C(I,3) = 6.0 * (Y2 / H2 - Y1 / H1) / HH
        H1 = H2
   80   Y1 = Y2
      ENDIF
C  SOLVE THE EQUATION
      C(1,1) = - EC(1) * 0.5
      C(1,2) =   EC(2) * 0.5
      IF (N .NE. 2) THEN
       DO 100 K=2,N-1
        PIV = 2.0 + C(K,2) * C(K-1,1)
        C(K,1) = - C(K,1) / PIV
  100   C(K,2) = (C(K,3) - C(K,2) * C(K-1,2)) / PIV
      ENDIF
      DY1 = (EC(4) - EC(3) * C(N-1,2)) / (2.0 + EC(3) * C(N-1,1))
      DO 120 I=1,N-1
       K = N - I
       DY2 = C(K,1) * DY1 + C(K,2)
       H = X(K+1) - X(K)
       C(K,3) = (DY1 - DY2) / (6.0 * H)
       C(K,2) = 0.5 * DY2
       C(K,1) = (Y(K+1) - Y(K)) / H - (C(K,2) + C(K,3) * H) * H
  120  DY1 = DY2
C
      RETURN
      END
C
      SUBROUTINE GUSPLF(X, N, Y, C, NC, V, M, F, IER)
C***********************************************************************
C  INTERPOLATION BY THE CUBIC SPLINE.                                  *
C  PARAMETERS                                                          *
C    (1) X: 1-DIM. ARRAY FOR KNOWN POINTS                              *
C    (2) N: NUMBER OF KNOWN POINTS                                     *
C    (3) Y: 1-DIM. ARRAY FOR FUNCTION'S VALUES ON KNOWN POINTS         *
C    (4) C: 2-DIM. WORKING ARRAY                                       *
C    (5) NC: ROW SIZE OF THE ARRAY (C)                                 *
C    (6) V: 1-DIM. ARRAY FOR POINTS WHICH INTERPOLATION MUST BE MADE   *
C    (7) M: NUMBER OF POINTS FOR WHICH INTERPOLATION MUST BE MADE      *
C    (8) F: 1-DIM. WORKING ARRAY                                       *
C    (9) IER: ERROR CODE                                               *
C  COPYRIGHT   T. OGUNI   JUNE 30 1989   VERSION 1.0                   *
C***********************************************************************
       IMPLICIT REAL*4(A-H,O-Z)
       DIMENSION X(N), Y(N), C(NC,3), V(M), F(M)
C
      IF (N .LT. 2 .OR. M .LT. 1 .OR. NC .LT. N-1) THEN
       IER = 2
       WRITE(*,*) '(SUBR. SPLF) INVALID ARGUMENT. ', N, NC, M
       RETURN
      ENDIF
      IER = 0
      I = 1
      DO 90 K=1,M
       V1 = V(K) - X(I)
       IF (V1) 10, 30, 40
   10  IF (I .GT. 1) GO TO 20
       IER = 1
       GO TO 80
   20  I = I - 1
       V1 = V(K) - X(I)
       IF (V1) 10, 30, 80
   30  F(K) = Y(I)
       GO TO 90
   40  IF (I .LT. N) GO TO 50
       IER = 1
       I = N - 1
       GO TO 80
   50  V2 = V(K) - X(I+1)
       IF (V2) 80, 60, 70
   60  I = I + 1
       GO TO 30
   70  I = I + 1
       V1 = V2
       GO TO 40
   80  F(K) = Y(I) + V1 * (C(I,1) + V1 * (C(I,2) + V1 * C(I,3)))
   90 CONTINUE
C
      RETURN
      END
C
C
C     ##########     SPLINE 2D     ##########
C
      SUBROUTINE  SPLN2D(X,Y,F,P,Q,S,A,IM,NI,NJ,XG,YG,FG,IN,NNI,NNJ,IP)
C
C     X(NI),Y(NJ)     : ORIGINAL COORDINATES
C     F(IM,NJ)        : ORIGINAL DATA
C     P(IM,NJ)        : DF/DX (ON BOUNDARY NX=1 OR NI, MAY BE GIVEN)
C     Q(IM,NJ)        : DF/DY (ON BOUNDARY NY=1 OR NJ, MAY BE GIVEN)
C     R(IM,NJ)        : DDF/DXDY     (AT FOUR CORNERS, MAY BE GIVEN)
C     A(4,MAX(NI,NJ)) : WORK AREA
C     IM              : FIRST DIMENSION OF ORIGINAL DATA AND WORK AREA
C     NI,NJ           : NUMBER OF ORIGINAL DATA IN X OR Y DIMENSION
C     XG(NNI),YG(NNJ) : NEW COORDINATES
C     FG(IN,NNJ)      : SPLINED NEW DATA
C     IM              : FIRST DIMENSION OF NEW DATA
C     NNI, NNJ        : NUMBER OF NEW DATA IN X OR Y DIMENSION
C     IP              : 0 : CALCULATE P,Q,S INTERNALLY
C                       1 : P,Q,S IS GIVEN
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  X( * ),Y( * ),F( IM,* )
      DIMENSION  XG( * ),YG( * ),FG( IN,* )
      DIMENSION  P( IM,* ),Q( IM,* ),S( IM,* )
      DIMENSION  GAM( 4,4 ),A( 4,* )
      REAL*8 DXY
C
      IF( NI.LT.4 .OR. NJ.LT.4 )  GO TO 9100
C
      CALL GUSPLN( X,Y,F,NI,NJ,P,Q,S,A,IM,IP )
C
      IF( XG( 1 ).LT.X( 1 ) .OR. XG( NNI ).GT.X( NI ) .OR.
     &    YG(1).LT.Y(1) .OR. YG( NNJ ).GT.Y( NJ ) )  WRITE( 6,1000 )
C
      DO 10  IDO = 2 , NI
         I=IDO
C
        IXL = 1
  500   IF( X( I-1 ) .LE. XG( IXL ) )  GO TO 510
        IXL = IXL + 1
        IF( IXL.GT.NNI ) GO TO 10
        GO TO 500
  510   IXU = IXL
  520   IF( X( I ) .LE. XG( IXU ) )  GO TO 530
        IXU = IXU + 1
        IF( IXU .GT. NNI ) GO TO 530
        GO TO 520
  530   IF(ABS(X(I)-XG(IXU)).LE.1.D-72)  IXU = IXU + 1
C
        DO 40  JDO = 2 , NJ
         J=JDO
C
          IYL = 1
  540     IF( Y( J-1 ) .LE. YG( IYL ) )  GO TO 550
          IYL = IYL + 1
          IF( IYL .GT. NNJ )  GO TO 40
          GO TO 540
  550     IYU = IYL
  560     IF( Y( J ) .LE. YG( IYU ) )  GO TO 570
          IYU = IYU + 1
          IF( IYU .GT. NNJ )  GO TO 570
          GO TO 560
  570     IF(ABS(Y(J)-YG(IYU)).LE.1.D-72)  IYU = IYU + 1
C
          CALL GUCALG( X,Y,F,P,Q,S,GAM,IM,I,J )
C
          DO 1  IX = IXL , IXU-1
            DO 2  IY = IYL , IYU-1
              FG( IX,IY ) = 0.E0
    2       CONTINUE
    1     CONTINUE
C
          DO 700  IX = IXL , IXU-1
            DO 710  IY = IYL , IYU-1
              DO 130  IOM = 0 , 3
                IF( IOM .NE. 0 ) THEN
                  XX = ( XG( IX ) - X( I-1 ) ) ** IOM
                ELSE
                  XX = 1.E0
                ENDIF
                DO 140  ION = 0 , 3
                  IF( ION .NE. 0 ) THEN
                    YY = ( YG( IY ) - Y( J-1 ) ) ** ION
                  ELSE
                    YY = 1.E0
                  ENDIF
                  DXY=DBLE(XX)*DBLE(YY)
                  IF(ABS(DXY).LT.1.E-30) DXY=0.0
                  FG( IX,IY ) = FG( IX,IY )+GAM( IOM+1,ION+1 )*DXY
  140           CONTINUE
  130         CONTINUE
  710       CONTINUE
  700     CONTINUE
C
   40   CONTINUE
C
   10 CONTINUE
C
      RETURN
C
 9100 WRITE( 6,3000 )
      RETURN
 1000 FORMAT( /,'   INTERPOLATION IS OVER SEGMENT' )
 3000 FORMAT( /,'   NI < 4   OR   NJ < 4' )
      END
C
C
C     ##########     GUSPLN     ##########
C
      SUBROUTINE  GUSPLN( X,Y,F,NI,NJ,P,Q,S,A,IM,IP )
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  X( * ),Y( * ),F( IM,* ),
     &           P( IM,* ),Q( IM,* ),S( IM,* ),
     &           A( 4,* )
C
      IF( IP .EQ. 0 )  CALL GUENCN( P,Q,S,X,Y,F,IM,NI,NJ )
C
      DO 30  J = 1 , NJ
C
         DX1 = X( 2 ) - X( 1 )
         DX2 = X( 3 ) - X( 2 )
         AS = 3.E0 * ( DX1 / DX2 * ( F( 3,J ) - F( 2,J ) )
     &             +   DX2 / DX1 * ( F( 2,J ) - F( 1,J ) ) )
         A( 4,1 ) = AS - DX2 * P( 1,J )
         A( 2,1 ) = 2.E0 * ( DX1 + DX2 )
         A( 3,1 ) = DX1
         DO 40  I = 3 , NI-2
            DXI1 = X( I ) - X( I-1 )
            DXI = X( I+1 ) - X( I )
            AS = 3.0 * (DXI1 / DXI * ( F( I+1,J ) - F( I  ,J ) )
     &               +  DXI / DXI1 * ( F( I  ,J ) - F( I-1,J ) ) )
            A( 3,I-1 ) = DXI1
            A( 2,I-1 ) = 2.E0 * ( DXI1 + DXI )
            A( 1,I-1 ) = DXI
            A( 4,I-1 ) = AS
   40    CONTINUE
         I = NI-1
         DXI1 = X( I ) - X( I-1 )
         DXI = X( I+1 ) - X( I )
         AS = 3.E0 * ( DXI1 / DXI * ( F( I+1,J ) - F( I  ,J ) )
     &             +   DXI / DXI1 * ( F( I  ,J ) - F( I-1,J ) ) )
         A( 4,I-1 ) = AS - DXI1 * P( NI,J )
         A( 2,I-1 ) = 2.E0 * ( DXI1 + DXI )
         A( 1,I-1 ) = DXI
C
         CALL GUBNDA( A,NI-2 )
         DO 41  I = 2 , NI-1
            P( I,J ) = A( 4,I-1 )
   41   CONTINUE
C
   30 CONTINUE
C
      DO 50  I = 1 , NI
C
         DY1 = Y( 2 ) - Y( 1 )
         DY2 = Y( 3 ) - Y( 2 )
         AS = 3.E0 * ( DY1 / DY2 * ( F( I,3 ) - F( I,2 ) )
     &             +   DY2 / DY1 * ( F( I,2 ) - F( I,1 ) ) )
         A( 4,1 ) = AS - DY2 * Q( I,1 )
         A( 2,1 ) = 2.E0 * ( DY1 + DY2 )
         A( 3,1 ) = DY1
         DO 60  J = 3 , NJ-2
            DYJ1 = Y( J ) - Y( J-1 )
            DYJ = Y( J+1 ) - Y( J )
            AS = 3.0 * ( DYJ1 / DYJ * ( F( I,J+1 ) - F( I,J   ) )
     &               +   DYJ / DYJ1 * ( F( I,J   ) - F( I,J-1 ) ) )
            A( 3,J-1 ) = DYJ1
            A( 2,J-1 ) = 2.E0 * ( DYJ1 + DYJ )
            A( 1,J-1 ) = DYJ
            A( 4,J-1 ) = AS
   60    CONTINUE
         J = NJ-1
         DYJ1 = Y( J ) - Y( J-1 )
         DYJ = Y( J+1 ) - Y( J )
         AS = 3.E0 * ( DYJ1 / DYJ * ( F( I,J+1 ) - F( I,J   ) )
     &             +   DYJ / DYJ1 * ( F( I,J   ) - F( I,J-1 ) ) )
         A( 4,J-1 ) = AS - DYJ1 * Q( I,NJ )
         A( 2,J-1 ) = 2.E0 * ( DYJ1 + DYJ )
         A( 1,J-1 ) = DYJ
C
         CALL GUBNDA( A,NJ-2 )
         DO 61  J = 2 , NJ-1
            Q( I,J ) = A( 4,J-1 )
   61    CONTINUE
C
   50 CONTINUE
C
      DO 70  J = 1 , NJ , NJ-1
C
         DX1 = X( 2 ) - X( 1 )
         DX2 = X( 3 ) - X( 2 )
         AS = 3.E0 * ( DX1 / DX2 * ( Q( 3,J ) - Q( 2,J ) )
     &             +   DX2 / DX1 * ( Q( 2,J ) - Q( 1,J ) ) )
         A( 4,1 ) = AS - DX2 * S( 1,J )
         A( 2,1 ) = 2.E0 * ( DX1 + DX2 )
         A( 3,1 ) = DX1
         DO 80  I = 3 , NI-2
            DXI1 = X( I ) - X( I-1 )
            DXI = X( I+1 ) - X( I )
            AS = 3.0 * (DXI1 / DXI * ( Q( I+1,J ) - Q( I  ,J ) )
     &               +  DXI / DXI1 * ( Q( I  ,J ) - Q( I-1,J ) ) )
            A( 3,I-1 ) = DXI1
            A( 2,I-1 ) = 2.E0 * ( DXI1 + DXI )
            A( 1,I-1 ) = DXI
            A( 4,I-1 ) = AS
   80    CONTINUE
         I = NI-1
         DXI1 = X( I ) - X( I-1 )
         DXI = X( I+1 ) - X( I )
         AS = 3.E0 * ( DXI1 / DXI * ( Q( I+1,J ) - Q( I  ,J ) )
     &             +   DXI / DXI1 * ( Q( I  ,J ) - Q( I-1,J ) ) )
         A( 4,I-1 ) = AS - DXI1 * S( NI,J )
         A( 2,I-1 ) = 2.E0 * ( DXI1 + DXI )
         A( 1,I-1 ) = DXI
C
         CALL GUBNDA( A,NI-2 )
         DO 81  I = 2 , NI-1
            S( I,J ) = A( 4,I-1 )
   81    CONTINUE
C
   70 CONTINUE
C
      DO 90  I = 1 , NI
C
         DY1 = Y( 2 ) - Y( 1 )
         DY2 = Y( 3 ) - Y( 2 )
         AS = 3.E0 * ( DY1 / DY2 * ( P( I,3 ) - P( I,2 ) )
     &             +   DY2 / DY1 * ( P( I,2 ) - P( I,1 ) ) )
         A( 4,1 ) = AS - DY2 * S( I,1 )
         A( 2,1 ) = 2.E0 * ( DY1 + DY2 )
         A( 3,1 ) = DY1
         DO 100  J = 3 , NJ-2
            DYJ1 = Y( J ) - Y( J-1 )
            DYJ = Y( J+1 ) - Y( J )
            AS = 3.0 * ( DYJ1 / DYJ * ( P( I,J+1 ) - P( I,J   ) )
     &               +   DYJ / DYJ1 * ( P( I,J   ) - P( I,J-1 ) ) )
            A( 3,J-1 ) = DYJ1
            A( 2,J-1 ) = 2.E0 * ( DYJ1 + DYJ )
            A( 1,J-1 ) = DYJ
            A( 4,J-1 ) = AS
  100    CONTINUE
         J = NJ-1
         DYJ1 = Y( J ) - Y( J-1 )
         DYJ = Y( J+1 ) - Y( J )
         AS = 3.E0 * ( DYJ1 / DYJ * ( P( I,J+1 ) - P( I,J   ) )
     &             +   DYJ / DYJ1 * ( P( I,J   ) - P( I,J-1 ) ) )
         A( 4,J-1 ) = AS - DYJ1 * S( I,NJ )
         A( 2,J-1 ) = 2.E0 * ( DYJ1 + DYJ )
         A( 1,J-1 ) = DYJ
C
         CALL GUBNDA( A,NJ-2 )
         DO 101  J = 2 , NJ-1
            S( I,J ) = A( 4,J-1 )
  101    CONTINUE
C
   90 CONTINUE
C
      RETURN
      END
C
C
C     ##########     GUENCN     ##########
C
      SUBROUTINE  GUENCN( P,Q,S,X,Y,F,IM,NI,NJ )
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  P( IM,* ),Q( IM,* ),S( IM,* )
      DIMENSION  X( * ),Y( * ),F( IM,* )
C
      DX1 = X( 2 ) - X( 1 )
      DXI = X( NI ) - X( NI-1 )
      DY1 = Y( 2 ) - Y( 1 )
      DYJ = Y( NJ ) - Y( NJ-1 )
      DO 10  I = 1 , NI
        Q( I,1  ) = ( F( I,2  ) - F( I,1    ) ) / DY1
        Q( I,NJ ) = ( F( I,NJ ) - F( I,NJ-1 ) ) / DYJ
   10 CONTINUE
C
      DO 20  J = 1 , NJ
        P( 1 ,J ) = ( F( 2 ,J ) - F( 1   ,J ) ) / DX1
        P( NI,J ) = ( F( NI,J ) - F( NI-1,J ) ) / DXI
   20 CONTINUE
C
      S( 1,1   ) = ( P(  1,2  ) - P( 1 ,1    ) ) / DY1
      S( 1,NJ  ) = ( P(  1,NJ ) - P( 1 ,NJ-1 ) ) / DYJ
      S( NI,1  ) = ( P( NI,2  ) - P( NI,1    ) ) / DY1
      S( NI,NJ ) = ( P( NI,NJ ) - P( NI,NJ-1 ) ) / DYJ
C
      RETURN
      END
C
C
C     ##########     GUBNDA     ##########
C
      SUBROUTINE  GUBNDA( A,L1 )
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  A( 4,* )
C
      DO 10  I = 1 , L1-1
         CO = A( 2,I )
         PA = A( 1,I+1 )
         A( 2,I+1 ) = A( 2,I+1 ) - A( 3,I )*PA/CO
         A( 4,I+1 ) = A( 4,I+1 ) - A( 4,I )*PA/CO
   10 CONTINUE
C
      A( 4,L1 ) = A( 4,L1 ) / A( 2,L1 )
      DO 20  II = 1 , L1-1
         I = L1-II
         A( 4,I ) = ( A( 4,I ) - A( 3,I )*A( 4,I+1 ) ) / A( 2,I )
   20 CONTINUE
C
      RETURN
      END
C
C
C     ##########     GUCALG     ##########
C
      SUBROUTINE  GUCALG( X,Y,F,P,Q,S,GAM,IM,I,J )
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  X( * ),Y( * ),F( IM,* )
      DIMENSION  P( IM,* ),Q( IM,* ),S( IM,* )
      DIMENSION  GAM( 4,4 ),GSM( 4,4 ),AX( 4,4 ),AY( 4,4 )
      DIMENSION  FK( 4,4 )
C
      DO 3  I1 = 1 , 4
         DO 4  J1 = 1 , 4
            GAM( I1,J1 ) = 0.0
            GSM( I1,J1 ) = 0.0
    4    CONTINUE
    3 CONTINUE
C
      CALL GUCALA( X( I )-X( I-1 ),AX )
      CALL GUCALA( Y( J )-Y( J-1 ),AY )
      CALL GUCALK( P,Q,S,F,FK,IM,I,J )
C
      DO 70  M1 = 1 , 4
         DO 80  M2 = 1 , 4
            DO 90  M3 = 1 , 4
               GSM( M1,M2 ) = GSM( M1,M2 ) + AX( M1,M3 )*FK( M3,M2 )
   90       CONTINUE
   80    CONTINUE
   70 CONTINUE
C
      DO 100  M1 = 1 , 4
         DO 110  M2 = 1 , 4
            DO 120  M3 = 1 , 4
               GAM( M1,M2 ) = GAM( M1,M2 ) + GSM( M1,M3 )*AY( M2,M3 )
  120       CONTINUE
  110    CONTINUE
  100 CONTINUE
C
      RETURN
      END
C
C
C     ##########     GUCALA     ##########
C
      SUBROUTINE  GUCALA( H,A )
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  A( 4,4 )
C
      DO 10  I = 1 , 4
         DO 20  J = 1 , 4
            A( I,J ) = 0.0
   20    CONTINUE
   10 CONTINUE
C
      A( 1,1 ) = 1.0
      A( 2,2 ) = 1.0
      A( 3,1 ) = -3.0/(H*H)
      A( 3,2 ) = -2.0/H
      A( 3,3 ) =  3.0/(H*H)
      A( 3,4 ) = -1.0/H
      A( 4,1 ) =  2.0/(H*H*H)
      A( 4,2 ) =  1.0/(H*H)
      A( 4,3 ) = -2.0/(H*H*H)
      A( 4,4 ) =  1.0/(H*H)
C
      RETURN
      END
C
C
C     ##########     GUCALK     ##########
C
      SUBROUTINE  GUCALK( P,Q,S,F,FK,IM,I,J )
C
      IMPLICIT REAL * 4 ( A - H , O - Z )
      DIMENSION  P( IM,* ),Q( IM,* ),S( IM,* ),F( IM,* )
      DIMENSION  FK( 4,4 )
C
      FK( 1,1 ) = F( I-1,J-1 )
      FK( 1,2 ) = Q( I-1,J-1 )
      FK( 1,3 ) = F( I-1, J  )
      FK( 1,4 ) = Q( I-1, J  )
      FK( 2,1 ) = P( I-1,J-1 )
      FK( 2,2 ) = S( I-1,J-1 )
      FK( 2,3 ) = P( I-1, J  )
      FK( 2,4 ) = S( I-1, J  )
      FK( 3,1 ) = F(  I ,J-1 )
      FK( 3,2 ) = Q(  I ,J-1 )
      FK( 3,3 ) = F(  I , J  )
      FK( 3,4 ) = Q(  I , J  )
      FK( 4,1 ) = P(  I ,J-1 )
      FK( 4,2 ) = S(  I ,J-1 )
      FK( 4,3 ) = P(  I , J  )
      FK( 4,4 ) = S(  I , J  )
C
      RETURN
      END
