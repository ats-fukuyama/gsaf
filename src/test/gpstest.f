C gpstest.f

      REAL:: x,y

      CALL gsopen
      CALL pages

      x=1.0
      y=1.0
      CALL setrgb(1.0,0.0,0.0)
      CALL move(x,y)
      CALL draw(x+4.0,y)
      CALL draw(x+4.0,y+4.0)
      CALL draw(x,y+4.0)
      CALL draw(x,y)

      x=9.0
      y=1.0
      CALL move(x,y)
      CALL setrgb(1.0,0.0,0.0)
      CALL draw(x+4.0,y)
      CALL setrgb(0.0,1.0,0.0)
      CALL draw(x+4.0,y+4.0)
      CALL setrgb(0.0,0.0,1.0)
      CALL draw(x,y+4.0)
      CALL setrgb(0.0,0.0,0.0)
      CALL draw(x,y)

      x=1.0
      y=9.0
      CALL move(x,y)
      CALL setlnw(0.1)
      CALL draw(x+4.0,y)
      CALL setlnw(0.2)
      CALL draw(x+4.0,y+4.0)
      CALL setlnw(0.3)
      CALL draw(x,y+4.0)
      CALL setlnw(0.4)
      CALL draw(x,y)
      CALL setlnw(0.0)

      x=9.0
      y=9.0
      CALL move(x,y)
      CALL setlin(0,0,7)
      CALL draw(x+4.0,y)
      CALL setlin(1,0,7)
      CALL draw(x+4.0,y+4.0)
      CALL setlin(2,0,7)
      CALL draw(x,y+4.0)
      CALL setlin(3,0,7)
      CALL draw(x,y)
      CALL setlin(0,0,0)

      CALL pagee
      CALL gsclos
      STOP
      END
