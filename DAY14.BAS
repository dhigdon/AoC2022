1010 REM Advent of Code 2022, Day 14 - Regolith Reservoir
1020 REM
1030 REM From analyzing the data, the "working set" extends
1040 REM from 483,13 -> 569,166
1050 REM We'll choose a width for 100x170, which seems to fid
1100 REM It also seems that there is a LOT of overlap in the
1110 REM specified scan data.
1120 REM Field uses 0 for empty space, 1 for rock, and 2 for sand.
1130 REM
1140 DEFINT A-Z: ROWS=160: COLS=100: XORG=482: YORG=8
1150 DIM FLD(ROWS,COLS):K$=".#O"
1160 '
2000 REM Read the data and fill in the field
2010 INPUT "File";F$
2020 OPEN F$ FOR INPUT AS #1
2030 WHILE NOT EOF(1)
2040  LINE INPUT #1,L$
2050  REM Parse X,Y[;X,Y]*
2070  SC=VAL(L$)-XORG
2080  N=INSTR(L$,",")
2090  SR=VAL(MID$(L$,N+1))-YORG
2110  N=INSTR(N+1,L$,";")
2120  IF N=0 THEN 2400
2140  C=VAL(MID$(L$,N+1))-XORG
2150  N=INSTR(N+1,L$,",")
2160  R=VAL(MID$(L$,N+1))-YORG
2170  IF SC=C THEN FOR I=R TO SR STEP SGN(SR-R): FLD(I,C)=1: NEXT I
2180  IF SR=R THEN FOR I=C TO SC STEP SGN(SC-C): FLD(R,I)=1: NEXT I
2200  SC=C: SR=R: GOTO 2110
2400 WEND
2410 CLOSE #1
2420 GOSUB 3000 
2430 END

2900 ' RETURN
2999 '
3000 REM Dump Field
3010 OPEN "day14.map" FOR OUTPUT AS #1
3020 FOR R=1 TO ROWS: PRINT #1,R;: FOR C=1 TO COLS
3030  PRINT #1,MID$(K$,FLD(R,C)+1,1);
3040 NEXT C: PRINT #1,"": NEXT R
3050 CLOSE #1
3060 RETURN

