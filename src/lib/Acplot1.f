C     $Id$
C     *************************************************
C     ********  GSAF 3D ROUTINES : Color plot  ********
C     *************************************************
C
C     言葉の説明：
C        3次元座標での値．．．ユーザが与えた座標での値
C        3次元での cm   ．．．gdefine3d で与えられる
C                       xl1,yl1,zl1 を元にした長さ(位置)
C        2次元での cm   ．．．実際に描画される長さ(位置)
C
C ------------------------------------------
C ------------------------------------------
C ---- 四角形をグラデーションで塗る --------
C ------------------------------------------
C ------------------------------------------
      subroutine gtGradationColor_Square(i,j,rgbfunc)
C
      INCLUDE 'A3dcomm.inc'
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA1/ XDATA(NXDMP),YDATA(NYDMP),ZDATA(NXDMP,NYDMP)
      dimension  x(5),y(5),z(5)
      dimension  xtmp(5),ytmp(5),ztmp(5)
      dimension  rgb(3)
      external   rgbfunc
C
      x(1)=xdata(i)
      y(1)=ydata(j)
      z(1)=zdata(i,j)
      x(2)=xdata(i+1)
      y(2)=ydata(j)
      z(2)=zdata(i+1,j)
      x(3)=xdata(i)
      y(3)=ydata(j+1)
      z(3)=zdata(i,j+1)
      x(4)=xdata(i+1)
      y(4)=ydata(j+1)
      z(4)=zdata(i+1,j+1)
C
      call INQRGB(cr,cg,cb)
      if (z(1).eq.z(2).and.z(1).eq.z(3).and.z(1).eq.z(4)) then
         call rgbfunc((z(1)-zmin)/(zmax-zmin),rgb)
         call setrgb(rgb(1),rgb(2),rgb(3))
         x(5)=x(3)
         y(5)=y(3)
         z(5)=z(3)
         x(3)=x(4)
         y(3)=y(4)
         z(3)=z(4)
         x(4)=x(5)
         y(4)=y(5)
         z(4)=z(5)
         x(5)=x(1)
         y(5)=y(1)
         z(5)=z(1)
         do m=1,5
            call gtttb(x(m),y(m),z(m),xtmp(m),ytmp(m))
         enddo
         call poly(xtmp,ytmp,4)
      else
         xm=(x(1)+x(2)+x(3)+x(4))/4
         ym=(y(1)+y(2)+y(3)+y(4))/4
         zm=(z(1)+z(2)+z(3)+z(4))/4
         xtmp(1)=x(1)
         ytmp(1)=y(1)
         ztmp(1)=z(1)
         xtmp(2)=x(2)
         ytmp(2)=y(2)
         ztmp(2)=z(2)
         xtmp(3)=xm
         ytmp(3)=ym
         ztmp(3)=zm
         call gtGradationColor_Triangle(xtmp,ytmp,ztmp,rgbfunc)
         xtmp(1)=x(2)
         ytmp(1)=y(2)
         ztmp(1)=z(2)
         xtmp(2)=x(4)
         ytmp(2)=y(4)
         ztmp(2)=z(4)
         xtmp(3)=xm
         ytmp(3)=ym
         ztmp(3)=zm       
         call gtGradationColor_Triangle(xtmp,ytmp,ztmp,rgbfunc)
         xtmp(1)=x(4)
         ytmp(1)=y(4)
         ztmp(1)=z(4)
         xtmp(2)=x(3)
         ytmp(2)=y(3)
         ztmp(2)=z(3)
         xtmp(3)=xm
         ytmp(3)=ym
         ztmp(3)=zm
         call gtGradationColor_Triangle(xtmp,ytmp,ztmp,rgbfunc)
         xtmp(1)=x(3)
         ytmp(1)=y(3)
         ztmp(1)=z(3)
         xtmp(2)=x(1)
         ytmp(2)=y(1)
         ztmp(2)=z(1)
         xtmp(3)=xm
         ytmp(3)=ym
         ztmp(3)=zm
         call gtGradationColor_Triangle(xtmp,ytmp,ztmp,rgbfunc)
      endif
      call setrgb(cr,cg,cb)

      RETURN
      END
C ------------------------------------------
C ---- 三角形をグラデーションで塗る --------
C ------------------------------------------
      subroutine gtGradationColor_Triangle(x,y,z,rgbfunc)
C
      INCLUDE 'A3dcomm.inc'
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA1/ XDATA(NXDMP),YDATA(NYDMP),ZDATA(NXDMP,NYDMP)
      dimension  rgb(3),r(3),g(3),b(3)
      dimension  x(3),y(3),z(3),xt(3),yt(3)
      external   rgbfunc
C
C      CALL INQRGB(CR,CG,CB)
      DO I=1,3
         call gtttb(x(I),y(I),z(I),xt(I),yt(I))
         call rgbfunc((z(I)-zmin)/(zmax-zmin),rgb)
         r(I)=rgb(1)
         g(I)=rgb(2)
         b(I)=rgb(3)
      ENDDO
      call rgbtrg(xt,yt,r,g,b)
C      CALL SETRGB(CR,CG,CB)
C
      return
      end
C -------------------------------------------    
C ----            バッファー            -----
C -------------------------------------------
      SUBROUTINE gtMkZBuffer

      INCLUDE 'A3dcomm.inc'
      COMMON /FRAG3D/ NTRN,NDFN,NZBUF
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA1/ XDATA(NXDMP),YDATA(NYDMP),ZDATA(NXDMP,NYDMP)
      COMMON /TDATA2/ WORK(NXDMP,NYDMP,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /CPLTHD/ NF,IXMIN,IYMIN,IHXY(NXDM,NYDM),DPMX(2560,1810),
     &     NNMX(2560,2),AFACTOR
      COMMON /ZBUFUR/ IXINDEX(NXYDM),IYINDEX(NXYDM),ZVALUE(NXDM,NYDM)
C
      IF (NZBUF.EQ.1) RETURN 
      NZBUF=1
C
      AFACTOR=50.0
      IXLENG=NINT(AFACTOR*(XTMAX-XTMIN))+2
      IYLENG=NINT(AFACTOR*(YTMAX-YTMIN))+2
      IXMIN =NINT(AFACTOR* XTMIN)-1
      IYMIN =NINT(AFACTOR* YTMIN)-1
C
      DO I=1,NX-1
      DO J=1,NY-1
         IHXY(I,J)=0
      ENDDO
      ENDDO
      NYMIN=NINT(AFACTOR*YTMAX)+1
      NYMAX=NINT(AFACTOR*YTMIN)-1
      DO I=1,IXLENG
         NNMX(I,1)=NYMIN
         NNMX(I,2)=NYMAX
         DO J=1,IYLENG
            DPMX(I,J)=-1.E+30
         ENDDO
      ENDDO
      CALL INQRGB(CR,CG,CB)
C
      DO I=(NX-1)*(NY-1),1,-1
         IX=IXINDEX(I)
         IY=IYINDEX(I)
         DO J=1,4
            IF ( J.EQ.1 ) THEN
               IX1=NINT(AFACTOR*WORK(IX,IY,1))
               IX2=NINT(AFACTOR*WORK(IX+1,IY,1))
               IY1=NINT(AFACTOR*WORK(IX,IY,2))
               IY2=NINT(AFACTOR*WORK(IX+1,IY,2))
               IXPMIN=MIN(IX1,IX2)
               IXPMAX=MAX(IX1,IX2)
               IYPMIN=MIN(IY1,IY2)
               IYPMAX=MAX(IY1,IY2)
            ELSE IF ( J.EQ.2 ) THEN
               IX2=NINT(AFACTOR*WORK(IX,IY+1,1))
               IY2=NINT(AFACTOR*WORK(IX,IY+1,2))
               IXPMIN=MIN(IX2,IXPMIN)
               IXPMAX=MAX(IX2,IXPMAX)
               IYPMIN=MIN(IY2,IYPMIN)
               IYPMAX=MAX(IY2,IYPMAX)
            ELSE IF ( J.EQ.3 ) THEN
               IX1=NINT(AFACTOR*WORK(IX+1,IY,1))
               IX2=NINT(AFACTOR*WORK(IX+1,IY+1,1))
               IY1=NINT(AFACTOR*WORK(IX+1,IY,2))
               IY2=NINT(AFACTOR*WORK(IX+1,IY+1,2))
               IXPMIN=MIN(IX1,IX2,IXPMIN)
               IXPMAX=MAX(IX1,IX2,IXPMAX)
               IYPMIN=MIN(IY1,IY2,IYPMIN)
               IYPMAX=MAX(IY1,IY2,IYPMAX)
            ELSE IF ( J.EQ.4 ) THEN
               IX1=NINT(AFACTOR*WORK(IX,IY+1,1))
               IY1=NINT(AFACTOR*WORK(IX,IY+1,2))
               IXPMIN=MIN(IX1,IXPMIN)
               IXPMAX=MAX(IX1,IXPMAX)
               IYPMIN=MIN(IY1,IYPMIN)
               IYPMAX=MAX(IY1,IYPMAX)
            ENDIF
            CALL GTZBUFP(IX1,IX2,IY1,IY2)
         ENDDO
         CALL GTZBUF(IX,IY,IXPMIN,IXPMAX)
         DO IN=IXPMIN,IXPMAX
            NNMX(IN-IXMIN,1)=NYMIN
            NNMX(IN-IXMIN,2)=NYMAX
         ENDDO
      ENDDO
C     
      RETURN
      END
C
C     ****** HIDE ******
C
      SUBROUTINE GTZBUF(I,J,IXPMIN,IXPMAX)
C
      INCLUDE 'A3dcomm.inc'
      COMMON /CPLTHD/ NF,IXMIN,IYMIN,IHXY(NXDM,NYDM),DPMX(2560,1810),
     &     NNMX(2560,2),AFACTOR
      COMMON /ZBUFUR/ IXINDEX(NXYDM),IYINDEX(NXYDM),ZVALUE(NXDM,NYDM)
C     
      IFLAG=0
      DO IX=IXPMIN,IXPMAX
      DO IY=NNMX(IX-IXMIN,1),NNMX(IX-IXMIN,2)
         IF (ZVALUE(I,J).GE.DPMX(IX-IXMIN,IY-IYMIN)) THEN
            IFLAG=1
            DPMX(IX-IXMIN,IY-IYMIN)
     &           =MAX(ZVALUE(I,J),DPMX(IX-IXMIN,IY-IYMIN))
         ENDIF
      ENDDO
      ENDDO
      IF (IFLAG.EQ.0) IHXY(I,J)=1
C
      RETURN
      END
C
C     ****** HIDE ******
C
      SUBROUTINE GTZBUFP(IX1,IX2,IY1,IY2)
C
      INCLUDE 'A3dcomm.inc'
      COMMON /CPLTHD/ NF,IXMIN,IYMIN,IHXY(NXDM,NYDM),DPMX(2560,1810),
     &     NNMX(2560,2),AFACTOR
C
      IF (IX2.EQ.IX1) THEN
         NNMX(IX1-IXMIN,1)=MIN(NNMX(IX1-IXMIN,1),IY1,IY2)
         NNMX(IX1-IXMIN,2)=MAX(NNMX(IX1-IXMIN,2),IY1,IY2)
         RETURN
      ENDIF
      D1X=FLOAT(IX2-IX1)
      DYDX=FLOAT(IY2-IY1)/D1X
      IF (IX1.GT.IX2) GOTO 200
      DO IX=IX1,IX2
         IYTMP=IY1+NINT(DYDX*(IX-IX1))
         NNMX(IX-IXMIN,1)=MIN(NNMX(IX-IXMIN,1),IYTMP)
         NNMX(IX-IXMIN,2)=MAX(NNMX(IX-IXMIN,2),IYTMP)
      ENDDO
      RETURN
 200  DO IX=IX1,IX2,-1
         IYTMP=IY1+NINT(DYDX*(IX-IX1))
         NNMX(IX-IXMIN,1)=MIN(NNMX(IX-IXMIN,1),IYTMP)
         NNMX(IX-IXMIN,2)=MAX(NNMX(IX-IXMIN,2),IYTMP)
      ENDDO
C
      RETURN
      END
C
C     ****** PLOT 3D DATA WITH COLORED PLANES ******
C                 グラフをカラー表示する
C
      SUBROUTINE CPLOT3D1(IND,RGBFUNC)
C
C     if IND=0, draw poly and lines
C     if IND=1, draw poly only
C     if IND=2, draw poly and x-lines only
C     if IND=3, draw poly and y-lines only
C
      INCLUDE 'A3dcomm.inc'
      COMMON /GLNGTH/ XL,YL,ZL,ZMIN,ZMAX,NX,NY
      COMMON /TDATA1/ XDATA(NXDMP),YDATA(NYDMP),ZDATA(NXDMP,NYDMP)
      COMMON /TDATA2/ WORK(NXDMP,NYDMP,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /ANGL3D/ PHI,THETA
      COMMON /CPLTHD/ NF,IXMIN,IYMIN,IHXY(NXDM,NYDM),DPMX(2560,1810),
     &                NNMX(2560,2),AFACTOR
      COMMON /ZBUFUR/ IXINDEX(NXYDM),IYINDEX(NXYDM),ZVALUE(NXDM,NYDM)
      DIMENSION RGB(3,NXDMP,NYDMP)
      EXTERNAL  RGBFUNC
C
      CALL GTTTDATA
      CALL gtMkZBuffer
      CALL INQRGB(CR,CG,CB)
      CALL SETRGB(0.0,0.0,0.0)
      IF (IND.GE.4) THEN
         NF=IND-4
         DO I=1,(NX-1)*(NY-1)
            IX=IXINDEX(I)
            IY=IYINDEX(I)
            IF (IHXY(IX,IY).NE.1) THEN
               call gtGradationColor_Square(IX,IY,rgbfunc)
               call gtCLines1(ix,iy)
            END IF
         ENDDO
      ELSE
         NF=IND
         DO I=1,(NX-1)*(NY-1)
            IX=IXINDEX(I)
            IY=IYINDEX(I)
            IF (IHXY(IX,IY).NE.1) THEN
               ZC=ZDATA(IX,IY)+ZDATA(IX+1,IY)+ZDATA(IX,IY+1)
     &           +ZDATA(IX+1,IY+1)
               ZC=0.25*ZC-ZMIN
               R=ZC/(ZMAX-ZMIN)
               CALL RGBFUNC(R,RGB(1,IX,IY))
               CALL SETRGB(RGB(1,IX,IY),RGB(2,IX,IY),RGB(3,IX,IY))
               CALL GTCPOLY1(IX,IY)
            END IF
         ENDDO
      ENDIF
C
      CALL SETRGB(CR,CG,CB)
      RETURN
      END
C     
C     ****** Paint and Draw Frame of POLY  ******
C     
      SUBROUTINE GTCPOLY1(I,J)
C
C     if I=0, draw poly and lines
C     if I=1, draw poly only
C     if I=2, draw poly and x-lines only
C     if I=3, draw poly and y-lines only
C
      INCLUDE 'A3dcomm.inc'
      COMMON /TDATA2/ WORK(NXDMP,NYDMP,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /CPLTHD/ NF,IXMIN,IYMIN,IHXY(NXDM,NYDM),DPMX(2560,1810),
     &     NNMX(2560,2),AFACTOR
C
      DIMENSION X1(5),Y1(5)
C     
      X1(1)=WORK(I,J,1)
      X1(2)=WORK(I+1,J,1)
      X1(3)=WORK(I+1,J+1,1)
      X1(4)=WORK(I,J+1,1)
      X1(5)=X1(1)
      Y1(1)=WORK(I,J,2)
      Y1(2)=WORK(I+1,J,2)
      Y1(3)=WORK(I+1,J+1,2)
      Y1(4)=WORK(I,J+1,2)
      Y1(5)=Y1(1)
      CALL POLY(X1,Y1,4)

      CALL SETRGB(0.0,0.0,0.0)
      SELECT CASE(NF)
      CASE(1)
         CALL MOVE(X1(1),Y1(1))
         CALL DRAW(X1(2),Y1(2))
         CALL MOVE(X1(4),Y1(4))
         CALL DRAW(X1(3),Y1(3))
      CASE(2)
         CALL MOVE(X1(1),Y1(1))
         CALL DRAW(X1(4),Y1(4))
         CALL MOVE(X1(2),Y1(2))
         CALL DRAW(X1(3),Y1(3))
      CASE(3)
         CALL LINES(X1,Y1,4)
      END SELECT
      RETURN
      END
C     
C     ****** DRAW LINES ******
C     
      SUBROUTINE GTCLINES1(I,J)
C
C     if NF=0, draw poly only
C     if NF=1, draw poly and x-lines only
C     if NF=2, draw poly and y-lines only
C     if NF=3, draw poly and lines
C     
      INCLUDE 'A3dcomm.inc'
      COMMON /TDATA2/ WORK(NXDMP,NYDMP,8),XTMIN,XTMAX,YTMIN,YTMAX
      COMMON /CPLTHD/ NF,IXMIN,IYMIN,IHXY(NXDM,NYDM),DPMX(2560,1810),
     &     NNMX(2560,2),AFACTOR
C
      DIMENSION X1(5),Y1(5)
C     
      X1(1)=WORK(I,J,1)
      X1(2)=WORK(I+1,J,1)
      X1(3)=WORK(I+1,J+1,1)
      X1(4)=WORK(I,J+1,1)
      X1(5)=X1(1)
      Y1(1)=WORK(I,J,2)
      Y1(2)=WORK(I+1,J,2)
      Y1(3)=WORK(I+1,J+1,2)
      Y1(4)=WORK(I,J+1,2)
      Y1(5)=Y1(1)

      SELECT CASE(NF)
      CASE(1)
         CALL MOVE(X1(1),Y1(1))
         CALL DRAW(X1(2),Y1(2))
         CALL MOVE(X1(4),Y1(4))
         CALL DRAW(X1(3),Y1(3))
      CASE(2)
         CALL MOVE(X1(1),Y1(1))
         CALL DRAW(X1(4),Y1(4))
         CALL MOVE(X1(2),Y1(2))
         CALL DRAW(X1(3),Y1(3))
      CASE(3)
         CALL LINES(X1,Y1,4)
      END SELECT
      RETURN
      END

