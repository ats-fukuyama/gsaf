C     $Id$
C
C     *************************************************
C     ****** GSAF BASIC ROUTINES V3.5 : INTERNAL ******
C     *************************************************
C
C     ++++++ INTERACTIVE OPTION SETTING ++++++
C
      SUBROUTINE SFOPTN
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFHD/ KHEAD
      COMMON /GSAFLF/ LOPENS,LOPENT,LSAVES,LSAVET
      CHARACTER KHEAD*40,KTITL*42
C
    1 WRITE(6,610) SIZEXS,SIZEYS
      CALL DVFLSH
      READ(5,*,END=1000,ERR=1) SIZEXS,SIZEYS
      CALL GSSIZE(SIZEXS,SIZEYS)
 1000 CONTINUE
C
      WRITE(6,620) KHEAD
      CALL DVFLSH
      READ(5,501,END=2000,ERR=1000) KTITL
      CALL GSTITL(KTITL)
 2000 CONTINUE
C
      WRITE(6,630) LSAVES
      CALL DVFLSH
      READ(5,*,END=4000,ERR=2000) LSAVEN
      IF(LSAVEN) THEN
         CALL SFFILE
         CALL GSFON
      ELSE
         CALL GSFOFF
      ENDIF
 4000 CONTINUE
C
      CALL DVOPTN('  ',0)
      RETURN
C
  501 FORMAT(A42)
  610 FORMAT(1H ,'# PAGE SIZE (CM)  : [ ',F6.2,',',F6.2,' ]')
  620 FORMAT(1H ,'# TITLE /40CHARS/ : [/',A40,'/]'/
     &       1H ,'#   INPUT A STRING ENCLOSED WITH DELIMITERS')
  630 FORMAT(1H ,'# FILE SAVE  (L)  : [ ',L1,' ]')
      END
C
C     ++++++ FILE NAME INPUT ++++++
C
      SUBROUTINE SFFILE
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLF/ LOPENS,LOPENT,LSAVES,LSAVET
      CHARACTER FCSNAM*80,KID*1
C
      IF(.NOT.LOPENT) CALL GSTOPN
      IF(LOPENS) THEN
    1    WRITE(6,*) '# CURRENT FILE IS GOING TO BE CLOSED.  ',
     &              '  ARE YOU SURE, (Y) OR (N) ?'
         CALL DVFLSH
         READ(5,'(A1)',END=10,ERR=1) KID
         CALL GUCPTL(KID)
         IF(KID.EQ.'Y') THEN
            CALL GSFOFF
            CALL GSFCLS
         ENDIF
      ENDIF
C      
   10 IF(.NOT.LOPENS) THEN
  100    CONTINUE
            CALL FLNAME(FCSNAM)
            CALL GSFOPN(FCSNAM,IND)
         IF(IND.NE.0) GOTO 100
      ENDIF
      RETURN
      END
C
C     ++++++ SHOW HELP INFORMATIONS ++++++
C
      SUBROUTINE SFHELP
C
      WRITE(6,601)
      RETURN
C
  601 FORMAT(1H ,'### GSAF INFORMATION ###'/
     &       1H ,'# YOU CAN INPUT ONE OF THE FOLLOWING CHARACTERS ',
     &           'AT THE END OF EACH PAGE.'/
     &       1H ,'#    C OR CR : CONTINUE.'/
     &       1H ,'#    F       : OPEN FILE AND START TO SAVE PAGES'/
     &       1H ,'#    S       : SAVE THIS PAGE, OPEN FILE IF NOT YET.'/
     &       1H ,'#    Y       : SAVE THIS PAGE IN FILE. '/
     &       1H ,'#    N       : DO NOT SAVE THIS PAGE IN FILE. '/
     &       1H ,'#    X       : ON/OFF SAVING PAGES '/
     &       1H ,'#    B       : ON/OFF BELL SOUND.'/
     &       1H ,'#    D       : DUMP DOT IMAGE. (X)'/
     &       1H ,'#    K       : KEEP SCREEN.'/
     &       1H ,'#    O       : SELECT OPTIONS.'/
     &       1H ,'#    H       : INFORMATIONS.'/
     &       1H ,'#    Q       : QUIT RUN.')
      END
C
C     ++++++ DRAW PAGE HEADER ++++++
C
      SUBROUTINE SFHEAD
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFLG/ LGSAF,LPAGE,LFIL,LKEEP,NPAGE,NHEAD
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFHD/ KHEAD
      CHARACTER KHEAD*40,KH1*18,KH2*4,KP*4
      CHARACTER*3 K1,K2,K3,K4,K5,K6
C
      IF(NHEAD.EQ.0) RETURN
C
      CALL GUDATE(NDY,NDM,NDD,NTH,NTM,NTS)
      WRITE(K1,'(I3)') 100+NDY
      WRITE(K2,'(I3)') 100+NDM
      WRITE(K3,'(I3)') 100+NDD
      WRITE(K4,'(I3)') 100+NTH
      WRITE(K5,'(I3)') 100+NTM
      WRITE(K6,'(I3)') 100+NTS
      WRITE(KH1,601) K1(2:3),K2(2:3),K3(2:3),K4(2:3),K5(2:3),K6(2:3)
  601 FORMAT(A2,'-',A2,'-',A2,'  ',A2,':',A2,':',A2)
      WRITE(KP,'(I4)') 1000+NPAGE
      WRITE(KH2,602) KP(2:4)
  602 FORMAT('#',A3)
C
      IF(SIZEXS.GT.25.7) CALL SETCHS(0.30,0.0)
      CALL MOVE(0.1,SIZEYS+0.25)
      CALL TEXT(KH1,18)
      CALL MOVE(8.15,SIZEYS+0.25)
      CALL TEXT(KHEAD,NHEAD)
      CALL MOVE(SIZEXS-1.5,SIZEYS+0.25)
      CALL TEXT(KH2,4)
      IF(SIZEXS.GT.25.7) CALL SETCHH(0.35,0.0)
      RETURN
      END
C
C     ++++++ DRAW MARK ++++++
C
      SUBROUTINE GS_SFMARK(IX,IY)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFIM/ IMK,IXMKX,IXMKY,IYMKX,IYMKY
C
      IXP=IX-IXMKX*2-IXMKY*3
      IYP=IY-IYMKX*2-IYMKY*3
      CALL DVGRPS
      CALL GS_SFCHAR(IXP,IYP,ABS(IMK),1)
      CALL DVGRPE
      RETURN
      END
C
C     ++++++ DRAW TEXT ++++++
C
      SUBROUTINE GS_SFTEXT(IX1,IY1,IASC,NCHAR)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFFN/ IFNTR,LSOFT
      DIMENSION IASC(*)
C
      IX=IX1
      IY=IY1
      IF(NCHAR.NE.0) THEN
         CALL DVGRPS
         DO 1000 N=1,ABS(NCHAR)
            INS=IASC(N)
            CALL DVGRPS
            IF(IFNTR.EQ.0) THEN
               CALL GS_SFCHAR(IX,IY,INS,0)
            ELSE
               CALL GS_GRCHAR(IX,IY,INS,0)
            ENDIF
            CALL DVGRPE
 1000    CONTINUE
         CALL DVGRPE
      ENDIF
C
      RETURN
      END
C
C     ++++++ DRAW ONE CHARACTER ++++++
C
      SUBROUTINE GS_SFCHAR(IX,IY,INS,IND)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFIC/ IXCHX,IXCHY,IYCHX,IYCHY,IXCHSP,IYCHSP
      COMMON /GSAFIM/ IMK,IXMKX,IXMKY,IYMKX,IYMKY
      DIMENSION IS(530),NS(128)
C
      DATA (IS(N),N=1,40)/
     &     200 000, 128 024, 123 022, 200 000,
     &     118 016, 138 036, 200 000, 106 046,
     &     144 004, 118 012, 132 038, 200 000,
     &     103 033, 044 035, 015 006, 017 047,
     &     128 022, 200 000, 048 118, 007 016,
     &     027 018, 132 043, 034 023, 032 200,
     &     143 022, 012 003, 004 027, 018 007,
     &     042 200, 128 026, 200 000, 128 016,
     &     014 022, 200 000, 128 036, 034 022,
     &     200 000, 103 046, 143 006, 127 025/
      DATA (IS(N),N=41,80)/
     &     124 022, 200 000, 105 045, 127 023,
     &     200 000, 123 011, 200 000, 105 045,
     &     200 000, 123 012, 113 022, 200 000,
     &     048 200, 103 012, 032 043, 047 038,
     &     018 007, 003 200, 000 000, 117 028,
     &     022 200, 107 018, 038 047, 046 003,
     &     002 042, 200 000, 107 018, 038 047,
     &     046 035, 015 135, 044 043, 032 012,
     &     003 200, 132 038, 004 044, 200 000,
     &     103 012, 032 043, 044 035, 005 008/
      DATA (IS(N),N=81,120)/
     &     048 200, 104 015, 035 044, 043 032,
     &     012 003, 007 018, 038 047, 200 000,
     &     108 048, 012 200, 115 004, 003 012,
     &     032 043, 044 035, 015 006, 007 018,
     &     038 047, 046 035, 200 000, 103 012,
     &     032 043, 047 038, 018 007, 005 014,
     &     034 045, 200 000, 116 025, 126 015,
     &     113 022, 123 012, 200 000, 116 025,
     &     126 015, 123 011, 200 000, 148 038,
     &     005 032, 042 200, 106 046, 104 044/
      DATA (IS(N),N=121,160)/
     &     200 000, 012 045, 018 008, 200 000,
     &     107 018, 038 047, 046 025, 024 123,
     &     022 200, 134 036, 026 015, 014 044,
     &     047 038, 018 007, 003 012, 042 200,
     &     005 028, 045 042, 104 044, 200 000,
     &     008 038, 047 046, 035 005, 135 044,
     &     043 032, 002 200, 147 038, 018 007,
     &     003 012, 032 043, 200 000, 008 028,
     &     046 044, 022 002, 200 000, 148 008,
     &     002 042, 105 035, 200 000, 008 048/
      DATA (IS(N),N=161,200)/
     &     105 035, 200 000, 147 038, 018 007,
     &     003 012, 032 043, 045 025, 200 000,
     &     008 105, 045 148, 042 200, 118 038,
     &     128 022, 112 032, 200 000, 103 012,
     &     032 043, 048 200, 008 148, 004 115,
     &     042 200, 108 002, 042 200, 008 025,
     &     048 042, 200 000, 008 042, 048 200,
     &     103 012, 032 043, 047 038, 018 007,
     &     003 200, 008 038, 047 046, 035 005,
     &     200 000, 103 012, 032 043, 047 038/
      DATA (IS(N),N=201,240)/
     &     018 007, 003 124, 042 200, 008 038,
     &     047 046, 035 005, 125 042, 200 000,
     &     103 012, 032 043, 044 035, 015 006,
     &     007 018, 038 047, 200 000, 108 048,
     &     128 022, 200 000, 108 003, 012 032,
     &     043 048, 200 000, 108 022, 048 200,
     &     108 006, 012 026, 032 046, 048 200,
     &     048 108, 042 200, 108 025, 148 025,
     &     022 200, 108 048, 002 042, 200 000,
     &     138 018, 012 032, 200 000, 108 042/
      DATA (IS(N),N=241,280)/
     &     200 000, 118 038, 032 012, 200 000,
     &     106 028, 046 200, 100 040, 200 000,
     &     128 036, 200 000, 135 026, 016 005,
     &     003 012, 022 033, 136 032, 042 200,
     &     108 018, 012 115, 026 036, 045 043,
     &     032 022, 013 200, 136 016, 005 003,
     &     012 032, 043 200, 135 026, 016 005,
     &     003 012, 022 033, 128 038, 032 042,
     &     200 000, 104 034, 045 036, 016 005,
     &     003 012, 032 043, 200 000, 105 025/
      DATA (IS(N),N=281,320)/
     &     137 028, 017 012, 200 000, 101 010,
     &     020 031, 036 046, 135 026, 016 005,
     &     003 012, 022 033, 200 000, 108 018,
     &     012 115, 026 036, 045 042, 200 000,
     &     128 027, 116 026, 022 112, 032 200,
     &     101 010, 020 031, 036 026, 138 037,
     &     200 000, 108 018, 012 146, 014 042,
     &     200 000, 118 028, 022 112, 032 200,
     &     006 105, 016 025, 022 125, 036 045,
     &     042 200, 112 016, 006 115, 026 036/
      DATA (IS(N),N=321,360)/
     &     045 042, 200 000, 103 012, 032 043,
     &     045 036, 016 005, 003 200, 100 010,
     &     016 006, 115 026, 036 045, 043 032,
     &     022 013, 200 000, 133 022, 012 003,
     &     005 016, 026 035, 146 036, 030 040,
     &     200 000, 112 016, 006 115, 026 036,
     &     045 200, 103 012, 032 043, 034 014,
     &     005 016, 036 045, 200 000, 106 036,
     &     118 013, 022 032, 043 200, 106 003,
     &     012 022, 033 136, 032 042, 200 000/
      DATA (IS(N),N=361,393)/
     &     106 022, 046 200, 106 012, 024 032,
     &     046 200, 046 106, 042 200, 101 010,
     &     020 031, 036 046, 106 003, 012 022,
     &     033 200, 106 046, 002 042, 200 000,
     &     138 027, 026 015, 024 023, 032 200,
     &     128 025, 124 021, 200 000, 118 027,
     &     026 035, 024 023, 012 200, 105 016,
     &     025 034, 045 200, 042 048, 008 002,
     &     200 000/
      DATA (IS(N),N=401,440)/
     &     125 025, 200 000, 114 034, 036 016,
     &     014 200, 105 045, 127 023, 200 000,
     &     103 047, 107 043, 200 000, 104 046,
     &     144 006, 127 023, 200 000, 103 047,
     &     107 043, 145 005, 127 023, 200 000,
     &     103 043, 027 003, 200 000, 103 043,
     &     047 007, 003 200, 105 023, 045 027,
     &     005 200, 104 013, 033 044, 046 037,
     &     017 006, 004 200, 103 043, 027 003,
     &     125 025, 200 000, 103 043, 047 007/
      DATA (IS(N),N=441,462)/
     &     003 125, 025 200, 105 023, 045 027,
     &     005 125, 025 200, 104 013, 033 044,
     &     046 037, 017 006, 004 125, 025 200,
     &     103 043, 047 007, 003 047, 107 043,
     &     200 000, 105 023, 045 027, 005 045,
     &     127 023, 200 000/
      DATA (IS(N),N=471,510)/
     &     125 027, 047 043, 003 007, 027 200,
     &     125 027, 037 046, 044 033, 013 004,
     &     006 017, 027 200, 125 027, 043 003,
     &     027 200, 105 045, 127 023, 200 000,
     &     147 003, 107 043, 200 000, 125 027,
     &     045 023, 005 027, 200 000, 123 027,
     &     045 005, 027 200, 103 047, 007 043,
     &     200 000, 107 047, 003 043, 115 035,
     &     200 000, 107 025, 023 147, 025 200,
     &     125 047, 107 016, 014 034, 036 016/
      DATA (IS(N),N=511,530)/
     &     114 003, 134 043, 200 000, 103 047,
     &     107 043, 123 027, 105 045, 200 000,
     &     103 047, 007 043, 003 200, 127 023,
     &     200 000, 125 027, 004 044, 027 123,
     &     046 006, 023 200, 125 045, 200 000/
      DATA (NS(N),N=1,32)/
     &     401,403,406,409,412,416,421,424,
     &     427,430,435,439,443,447,453,401,
     &     471,475,481,484,487,490,494,497,
     &     500,504,507,514,519,522,524,529/
      DATA (NS(N),N=33,128)/
     &       1,  2,  5,  8, 13, 19, 25, 30,
     &      32, 35, 38, 43, 46, 48, 50, 53,
     &      54, 60, 62, 67, 74, 77, 82, 89,
     &      91,100,107,112,116,119,122,125,
     &     130,137,141,147,152,156,160,163,
     &     169,172,176,179,182,184,187,189,
     &     194,198,204,209,216,219,223,225,
     &     229,231,234,237,240,242,245,247,
     &     249,251,257,263,267,274,280,284,
     &     292,297,301,306,310,313,318,323,
     &     328,335,342,346,352,356,361,363,
     &     366,368,374,377,381,384,388,391/
C
      J=NS(INS+1)
      LMV=.TRUE.
      IF(IND.EQ.0) THEN
         IXX=IXCHX
         IXY=IXCHY
         IYX=IYCHX
         IYY=IYCHY
         IXSP=NINT(0.166667*IXCHSP)
         IYSP=NINT(0.166667*IYCHSP)
      ELSE
         IXX=IXMKX
         IXY=IXMKY
         IYX=IYMKX
         IYY=IYMKY
         IXSP=0
         IYSP=0
      ENDIF
      IX=IX+IXSP
      IY=IY+IYSP
C
  100 CONTINUE
         IL=IS(J)
         IL1=IL/1000
         IL2=IL-IL1*1000
         IL1P=IL1/100
         IF(IL1P.EQ.2) GO TO 200
         IL1=IL1-IL1P*100
         IL1X=IL1/10
         IL1Y=IL1-IL1X*10-2
         IF(LMV.AND.IL1P.EQ.0) THEN
            IXT=IX
            IYT=IY
            IF(IXT.LT.0)     IXT=0
            IF(IXT.GT.32767) IXT=32767
            IF(IYT.LT.0)     IYT=0
            IF(IYT.GT.32767) IYT=32767
            CALL DVMOVE(IXT,IYT)
         ENDIF 
         LMV=.FALSE.
         IXT=IX+IXX*IL1X+IXY*IL1Y
         IYT=IY+IYX*IL1X+IYY*IL1Y
         IF(IXT.LT.0)     IXT=0
         IF(IXT.GT.32767) IXT=32767
         IF(IYT.LT.0)     IYT=0
         IF(IYT.GT.32767) IYT=32767
         IF(IL1P.EQ.0) CALL DVDRAW(IXT,IYT)
         IF(IL1P.EQ.1) CALL DVMOVE(IXT,IYT)
         IL2P=IL2/100
         IF(IL2P.EQ.2) GO TO 200
         IL2=IL2-IL2P*100
         IL2X=IL2/10
         IL2Y=IL2-IL2X*10-2
         IXT=IX+IXX*IL2X+IXY*IL2Y
         IYT=IY+IYX*IL2X+IYY*IL2Y
         IF(IXT.LT.0)     IXT=0
         IF(IXT.GT.32767) IXT=32767
         IF(IYT.LT.0)     IYT=0
         IF(IYT.GT.32767) IYT=32767
         IF(IL2P.EQ.0) CALL DVDRAW(IXT,IYT)
         IF(IL2P.EQ.1) CALL DVMOVE(IXT,IYT)
         J=J+1
      GO TO 100
C
  200 IX=IX+IXCHSP-IXSP
      IY=IY+IYCHSP-IYSP
      RETURN
      END
C
C     ++++++ SET MARK ATTRIBUTE ++++++
C
      SUBROUTINE GS_SFSTMK(IMRK,IMKH,IMKW,ANGL,TILT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFIM/ IMK,IXMKX,IXMKY,IYMKX,IYMKY
      DATA DEG/0.0174533/
C
      IMK=IMRK
      A=DEG*ANGL
      T=DEG*TILT
      B=A+T
      CA=COS(A)
      SA=SIN(A)
      CB=COS(B)
      SB=SIN(B)
      CC=COS(T)
C
      CHX=0.25*REAL(IMKW)
      CHY=0.25*REAL(IMKH)
      IXMKX=NINT( CHX*CA)
      IYMKX=NINT( CHX*SA)
      IXMKY=NINT(-CHY*SB)
      IYMKY=NINT( CHY*CB)
      RETURN
      END
C
C     ++++++ SET CHARACTER ATTRIBUTE ++++++
C
      SUBROUTINE GS_SFSTCH(ICHH,ICHW,ICHSP,ANGL,TILT)
C
      IMPLICIT LOGICAL(L)
      COMMON /GSAFS1/ SIZEXS,SIZEYS,DSIZE
      COMMON /GSAFIC/ IXCHX,IXCHY,IYCHX,IYCHY,IXCHSP,IYCHSP
      COMMON /GSPGC3/ THXX,THYX,THXY,THYY,THLX,THLY,THSP
      DATA DEG/0.0174533/
C
      A=DEG*ANGL
      T=DEG*TILT
      B=A+T
      CA=COS(A)
      SA=SIN(A)
      CB=COS(B)
      SB=SIN(B)
      CC=COS(T)
C
      CHX=0.25*REAL(ICHW)
      CHY=0.166667*REAL(ICHH)
      IXCHX=NINT( CHX*CA)
      IYCHX=NINT( CHX*SA)
      IXCHY=NINT(-CHY*SB/CC)
      IYCHY=NINT( CHY*CB/CC)
C
      CHSP=REAL(ICHSP)
      IXCHSP=NINT(CHSP*CA)
      IYCHSP=NINT(CHSP*SA)
C
      THLX=    1.5*ICHW       /DSIZE
      THLY=    ICHH           /DSIZE
      THSP=   (ICHSP-1.5*ICHW)/DSIZE
C
      THXX= CA
      THYX= SA
      THXY=-SB/CC
      THYY= CB/CC
C
      RETURN
      END
