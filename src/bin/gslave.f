C     $Id$
C
C     ****** GSFILE OUTPUT DRIVER ******
C
      PARAMETER  (NPAGEMAX=1024)
      DIMENSION NPAGEA(NPAGEMAX)
      DIMENSION IPARM(256)
      CHARACTER KPARM*256,FLNAME*256
C
      CALL DVRENV(IPARM,256)
      CALL ASCCHR(IPARM,KPARM,256)
      READ(KPARM,*) FLNAME,INTACT,ISTART,IEND,INUM,ITITLE,IROTAT,ICOLOR
C
      NF=52
      OPEN(NF,FILE=FLNAME,STATUS='OLD',IOSTAT=IST,ERR=9200)
C
      CALL GSOPEN
      IF(MOD(INTACT,2).EQ.0) THEN
         IF(ISTART.EQ.0) THEN
            CALL GSCOMB(NF,INUM,ITITLE)
         ELSE
            DO 100 I=ISTART,IEND,INUM
               CALL PGRLOC(NF,I,IEND)
               IF(IEND.EQ.0) THEN
                  CALL PGCOMB(NF,INUM,ITITLE)
               ELSE
                  GOTO 101
               ENDIF
  100       CONTINUE
  101       CONTINUE
         ENDIF
      ELSE
         CALL GSLIST(NF,NPAGEA,NPAGEMAX,NP)
         IF(NP.LE.10) THEN
            WRITE(6,601) (NPAGEA(N),N=1,NP)
         ELSE
            WRITE(6,601) (NPAGEA(N),N=1,10)
            WRITE(6,602) (NPAGEA(N),N=11,NP)
         ENDIF
  601    FORMAT(1H ,'## PAGES : ',10(I3:','))
  602    FORMAT((1H ,'           ',10(I3:',')))
C     
  200    CONTINUE
         WRITE(6,*) '## INPUT : PAGE NUMBER ',
     &        '(0:ALL -1:REWIND CR/^D:END)'
         READ(5,*,ERR=200,END=9000) NPAGE
         IF(NPAGE.EQ.-1) THEN
            CALL PGRLOC(NF,-1,IEND)
         ELSEIF(NPAGE.EQ.0) THEN
            CALL GSCOMB(NF,INUM,ITITLE)
         ELSE
            CALL PGRLOC(NF,NPAGE,IEND)
            IF(IEND.EQ.0) THEN
               CALL PGCOMB(NF,INUM,ITITLE)
            ELSE
               WRITE(6,*) '## PAGE ',NPAGE,' NOT FOUND'
            ENDIF
         ENDIF
         GOTO 200
      ENDIF
 9000 CONTINUE
      CALL GSCLOS
      CLOSE(NF)
      STOP
C     
 9100 CONTINUE
      WRITE(6,*) 'XX PARM FILE OPEN ERROR : IOSTAT = ',IST
      STOP
 9200 CONTINUE
      WRITE(6,*) 'XX DATA FILE OPEN ERROR : IOSTAT = ',IST
      STOP
      END
