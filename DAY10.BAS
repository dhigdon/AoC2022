1000 REM Advent of Code 2022, Day 10 - Cathode-Ray Tube
1010 GOTO 2000
1020 '
1100 REM Clock Tick
1110 X=((C-1) MOD 40)
1120 IF ABS(X-V)<2 THEN PRINT "#"; ELSE PRINT ".";
1130 IF X=39 THEN PRINT
1140 C=C+1
1160 RETURN
1170 '
2000 REM Main
2010 INPUT "File";f$
2015 PRINT TIME$,"Start"
2020 OPEN F$ FOR INPUT AS #1
2025 V=1: C=1
2030 IF EOF(1) THEN GOTO 2110
2040  LINE INPUT #1,L$
2045  O$=LEFT$(L$,4)
2050  IF O$="noop" THEN GOSUB 1100: GOTO 2030
2060  IF O$="addx" THEN GOSUB 1100: GOSUB 1100: V=V+VAL(MID$(L$,5)): GOTO 2030
2100  PRINT "Invalid Opcode",O$: END 
2110 REM END I 
2120 PRINT TIME$,"End"
2130 END
