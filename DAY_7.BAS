1000 REM Advent of Code 2022, Day 7 - No Space Left On Device
1010 DIM PATHS$(200): DIM PN#(200): DIM PT#(200): CWD$=""
1020 GOTO 1410
1030 '
1040 REM FindPath(T$) -> P%, Error if not found
1050  P%=1
1060  WHILE P%<=NP% AND PATHS$(P%)<>T$: P%=P%+1: WEND
1070  IF P%>NP% THEN PRINT T$;" not found": END
1080 RETURN 
1090 '
1100 REM CD - Change CWD$ accordingly
1110  IF ARG$="/" THEN CWD$="!": RETURN
1120  IF ARG$=".." THEN CWD$=MID$(CWD$,INSTR(CWD$,"/")+1): RETURN
1130  CWD$=ARG$+"/"+CWD$
1140 RETURN
1150 '
1160 REM LS - Process all data from LS statement, Update PN(CWD)
1170  TOTAL#=0
1180  LINE INPUT #1,L$
1190  WHILE INSTR(L$,"$")=0
1200   TOTAL#=TOTAL#+VAL(L$): REM "dir x" will have val=0
1210   IF NOT EOF(1) THEN LINE INPUT #1,L$ ELSE L$="$"
1220  WEND
1230  REM Save file total (note, ls commands don't repeat folders)
1240  NP%=NP%+1: PATHS$(NP%)=CWD$: PN#(NP%)=TOTAL#
1250 RETURN
1260 '
1270 REM Tabulate
1280  FOR I%=1 TO NP%
1290   CWD$=PATHS$(I%):TOTAL#=PN#(I%)
1300   PT#(I%)=PN#(I%)
1310   J%=INSTR(CWD$,"/")
1320   WHILE J%>0
1330    T$=MID$(CWD$,J%+1)
1340    GOSUB 1040: REM P% = FindPath(T$)
1350    PT#(P%) = PT#(P%) + PN#(I%)
1360    J%=INSTR(J%+1,CWD$,"/")
1370   WEND
1380  NEXT I%
1390 RETURN
1400 '
1410 REM Main
1420  INPUT "File";F$
1430  OPEN F$ FOR INPUT AS #1
1440  LINE INPUT #1,L$
1450  WHILE NOT EOF(1)
1460   CMD$=MID$(L$,3,2):X%=INSTR(3,L$," ")
1470   IF X%=0 THEN ARG$="" ELSE ARG$=MID$(L$,X%+1)
1480   IF CMD$="cd" THEN GOSUB 1100: LINE INPUT #1,L$
1490   IF CMD$="ls" THEN GOSUB 1160: REM Reads next line
1500  WEND
1510  CLOSE #1
1520  '
1530  GOSUB 1270: REM Tabulate
1540  TOTAL=0
1550  FOR I%=1 TO NP%
1560   IF PT#(I%)<=100000! THEN TOTAL=TOTAL+PT#(I%)
1570  NEXT I%
1580  PRINT "Total Under 100000 = ";TOTAL
1590  '
1600  REM Part 2 - find smallest path we could delete
1610  MX#=PT#(1): X#=1: FS#=70000000#-MX#: GOAL#=30000000#: FIND#=GOAL#-FS#
1620  PRINT "Free Space ";FS#
1630  PRINT "Need to free up ";FIND#
1640  FOR I%=1 TO NP%
1650   D#=PT#(I%)
1660   IF D#>=FIND# AND D#<MX# THEN MX#=D#
1670  NEXT I%
1680  PRINT "Smallest = ";MX#
1690 END
