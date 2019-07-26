C     $Id$
C
C     SAMPLE PROGRAM
C
      DIMENSION Z(101,101),X(101),Y(101),KA(8,101,101)
      EXTERNAL R2G2B,R2W2B
C
      IXMAX=31
      IYMAX=31
      CALL GSOPEN
      DO I=1,IXMAX
      DO J=1,IYMAX
         X(I)=0.2*FLOAT(I-1)
         Y(J)=0.1*FLOAT(J-1)
         Z(I,J)=COS(X(I))*SIN(Y(J))
      ENDDO
      ENDDO
      ZMIN= -1.
      ZMAX= 1.
      XL=10.0
      YL=12.0
      ZL=5.0
      ID=1
      PHI=50.0
      THETA=60.0
      RADIUS=100.0
    1 CONTINUE

    2 write(6,*) 'Input id(1..2),phi,theta  (id=0 for end)'
      read(5,*,END=9999,ERR=2) id,phi,theta
      if(id.eq.0) goto 9999
C
      if(id.eq.1) then
         CALL PAGES
         CALL SETLIN(0,0,7)
         CALL SETCHS(0.20,0.0)
         CALL SETFNT(32)
C
         CALL GMNMX1(X,1,IXMAX,1,XMIN,XMAX)
         CALL GMNMX1(Y,1,IYMAX,1,YMIN,YMAX)
         CALL GQSCAL(XMIN,XMAX,GXMIN,GXMAX,XSCAL)
         CALL GQSCAL(YMIN,YMAX,GYMIN,GYMAX,YSCAL)
         CALL GQSCAL(ZMIN,ZMAX,GZMIN,GZMAX,ZSCAL)
C
         CALL GDEFIN3D(2.,24.,1.,17.5,XL,YL,ZL)
         CALL GVIEW3D(PHI,THETA,RADIUS,0.9,1,0.0,0.0,0.0)
         CALL GDATA3D1(Z,101,IXMAX,IYMAX,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX)
C
         CALL GDRWBK(0.3,0.3,0.3)
C     
         CALL GSCALE3DX(XMIN,XSCAL,0.2,0)
         CALL GSCALE3DY(YMIN,YSCAL,0.2,0)
         CALL GSCALE3DZ(0.0,ZSCAL,0.2,0)
         CALL GVALUE3DX(XMIN,XSCAL,-2,1)
         CALL GVALUE3DY(YMIN,YSCAL,-2,1)
         CALL GVALUE3DZ(0.0,ZSCAL,-2,1)
C
         CALL CPLOT3D1(7,R2W2B)
         CALL CONTQ3D1(ZMIN,0.1*(ZMAX-ZMIN),11,0,0,KA,R2W2B,2)
         CALL GAXIS3D(2)
C
         CALL PAGEE
      elseif(id.eq.2) then
         CALL PAGES
         CALL SETLIN(0,0,7)
         CALL SETCHS(0.20,0.0)
         CALL SETFNT(32)
C
         CALL GMNMX1(X,1,IXMAX,1,XMIN,XMAX)
         CALL GMNMX1(Y,1,IYMAX,1,YMIN,YMAX)
         CALL GQSCAL(XMIN,XMAX,GXMIN,GXMAX,XSCAL)
         CALL GQSCAL(YMIN,YMAX,GYMIN,GYMAX,YSCAL)
         CALL GQSCAL(ZMIN,ZMAX,GZMIN,GZMAX,ZSCAL)
C
         CALL GDEFIN3D(2.,24.,1.,17.5,XL,YL,ZL)
         CALL GVIEW3D(PHI,THETA,RADIUS,0.9,1,0.0,0.0,0.0)
         CALL GDATA3D2(Z,X,Y,101,IXMAX,IYMAX,ZMIN,ZMAX)
C
         CALL GDRWBK(0.3,0.3,0.3)
C     
         CALL GSCALE3DX(XMIN,XSCAL,0.2,0)
         CALL GSCALE3DY(YMIN,YSCAL,0.2,0)
         CALL GSCALE3DZ(0.0,ZSCAL,0.2,0)
         CALL GVALUE3DX(XMIN,XSCAL,-2,1)
         CALL GVALUE3DY(YMIN,YSCAL,-2,1)
         CALL GVALUE3DZ(0.0,ZSCAL,-2,1)
C
         CALL CPLOT3D1(7,R2W2B)
         CALL CONTQ3D1(ZMIN,0.1*(ZMAX-ZMIN),11,0,0,KA,R2W2B,2)
         CALL GAXIS3D(2)
C
         CALL PAGEE
      endif
      goto 1
C
 9999 CALL GSCLOS
      STOP
      END



