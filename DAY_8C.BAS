1000 REM Advent of Code 2022 Day 8 - Treetop Tree House
1010 REM GRID stores the tree map digits in an integer
1020 REM Visibility is marked on the same map for space reasons
1030 REM Height fits in the lower nibble, visibility above that 
1040 '
1050 DEFINT A-Z
1060 DEF FNGH(R,C)=GRID(R,C) AND 15: REM Extract Height from GRID
1070 DEF FNGV(R,C)=GRID(R,C) > 15:   REM Extract Visibility bool from GRID
1090 GOTO 1200: REM Main
1100 '
1110 REM Subroutine Set Visisble(R,C)
1120  GRID(R,C)=GRID(R,C) OR 16
1130 RETURN
1140 '
1150 REM Subroutine Accumulate VIS(R,C) against H
1160  GRC=GRID(R,C)
1170  IF GRC<16 AND GRC>H THEN H=GRC: GRID(R,C)=H OR 16
1180 RETURN
1190 '
1200 REM Main
1210 INPUT "File";F$
1220 PRINT TIME$,"Start"
1230 OPEN F$ FOR INPUT AS #1
1240 '
1250 REM Read Forest
1260 LINE INPUT #1,L$
1270 SZ=LEN(L$): REM Data is square, but the size varies
1280 DIM GRID(SZ,SZ)
1290 FOR R=1 TO SZ
1300  FOR C=1 TO SZ: GRID(R,C)=VAL(MID$(L$,C,1)): NEXT C
1310  IF NOT EOF(1) THEN LINE INPUT #1,L$
1320 NEXT R
1330 PRINT TIME$,"Size = ";SZ;"x";SZ,FRE(0);" bytes free"
1340 '
1350 REM Mark all edge trees as visible
1360 FOR I=1 TO SZ
1370   R=I: C=1: GOSUB 1110: C=SZ: GOSUB 1110:
1380   C=I: R=1: GOSUB 1110: R=SZ: GOSUB 1110
1390 NEXT I
1400 '
1410 REM Horizontal scan
1420 FOR R=2 TO SZ-1
1430  H=FNGH(R,1): FOR C=2 TO SZ-1 STEP 1: GOSUB 1150: NEXT C
1440  H=FNGH(R,SZ): FOR C=SZ-1 TO 2 STEP -1: GOSUB 1150: NEXT C
1450 NEXT R
1460 REM Vertical scan
1470 FOR C=2 TO SZ-1
1480  H=FNGH(1,C): FOR R=2 TO SZ-1 STEP 1: GOSUB 1150: NEXT R
1490  H=FNGH(SZ,C): FOR R=SZ-1 TO 2 STEP -1: GOSUB 1150: NEXT R
1500 NEXT C
1510 '
1520 REM REPORT
1530 FOR R=1 TO SZ
1540  FOR C=1 TO SZ
1550   IF FNGV(R,C) THEN T=T+1
1560  NEXT C
1570 NEXT R
1580 PRINT TIME$,"Tree Count = ";T
1590 END
