C gpstest.f90

      REAL:: x,y

      CALL gsopen
      CALL pages

      x=1.0
      y=1.0
      CALL setrgb(1.0,0.0,0.0)
      CALL move(x,y)
      CALL draw(x+1.0,y)
      CALL draw(x+1.0,y+1.0)
      CALL draw(x,y+1.0)
      CALL draw(x,y)

      x=3.0
      y=3.0
      CALL move(x,y)
      CALL setrgb(1.0,0.0,0.0)
      CALL draw(x+1.0,y)
      CALL setrgb(0.0,1.0,0.0)
      CALL draw(x+1.0,y+1.0)
      CALL setrgb(0.0,0.0,1.0)
      CALL draw(x,y+1.0)
      CALL setrgb(0.0,0.0,0.0)
      CALL draw(x,y)

      CALL pagee
      CALL gsclos
      STOP
      END
