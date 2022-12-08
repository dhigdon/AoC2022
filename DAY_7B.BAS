1000 REM Advent of Code 2022, Day 7 - No Space Left On Device
1005 REM Experimental version - uses binary search for path resolution
1010 DEFINT H-L: DIM PS$(200): DIM PN#(200): DIM PT#(200)
1020 GOTO 1530
1030 '
1040 REM FindPath(T$) -> P%, Error if not found
1050  REM Binary Search, since PS$ is sorted
1060  L=0: H=NP%: P%=0
1070  IF L > H THEN PRINT T$;" not found": END
1080  K=(H+L)\2: K$=PS$(K)
1090  IF K$ > T$ THEN H=K-1: GOTO 1070
1100  IF K$ < T$ THEN L=K+1: GOTO 1070
1110  P%=K
1120 RETURN 
1130 '
1140 REM CD - Change CWD$ accordingly
1150  IF ARG$="/" THEN CWD$="!": RETURN
1160  IF ARG$=".." THEN CWD$=MID$(CWD$,INSTR(CWD$,"/")+1): RETURN
1170  CWD$=ARG$+"/"+CWD$
1180 RETURN
1190 '
1200 REM LS - Process all data from LS statement, Update PN(CWD)
1210  D#=0
1220  LINE INPUT #1,L$
1230  WHILE INSTR(L$,"$")=0
1240   D#=D#+VAL(L$): REM "dir x" will have val=0
1250   IF NOT EOF(1) THEN LINE INPUT #1,L$ ELSE L$="$"
1260  WEND
1270  REM Insertion sort CWD$ into PS$
1280  NP%=NP%+1: I=NP%
1290  WHILE I>1 AND PS$(I-1)>CWD$
1300   J=I-1
1310   PS$(I)=PS$(J): PN#(I)=PN#(J): I=J
1320  WEND
1330  PS$(I)=CWD$: PN#(I)=D#
1340 RETURN
1350 '
1360 REM Tabulate PT# from PN#
1370  FOR I=1 TO NP%
1380   CWD$=PS$(I)
1390   D#=PN#(I): PT#(I)=D#
1400   J=INSTR(CWD$,"/")
1410   WHILE J>0
1420    T$=MID$(CWD$,J+1)
1430    GOSUB 1040: REM P% = FindPath(T$)
1440    PT#(P%) = PT#(P%) + D#
1450    J=INSTR(J+1,CWD$,"/")
1460   WEND
1470  NEXT I
1480 ' FOR I = 1 TO NP%
1490 '  PRINT PS$(I),PN#(I),PT#(I)
1500 ' NEXT I
1510 RETURN
1520 '
1530 REM Main
1540  INPUT "File";F$
1550  PRINT TIME$
1560  OPEN F$ FOR INPUT AS #1
1570  LINE INPUT #1,L$
1580  WHILE NOT EOF(1)
1590   CMD$=MID$(L$,3,2):X%=INSTR(3,L$," ")
1600   IF X%=0 THEN ARG$="" ELSE ARG$=MID$(L$,X%+1)
1610   IF CMD$="cd" THEN GOSUB 1140: LINE INPUT #1,L$
1620   IF CMD$="ls" THEN GOSUB 1200: REM Reads next line
1630  WEND
1640  CLOSE #1
1650  '
1660  GOSUB 1360: REM Tabulate
1665  '
1670  D# = 0
1680  FOR I=1 TO NP%
1690   IF PT#(I)<=100000 THEN D# = D# + PT#(I)
1700  NEXT I
1710  PRINT "Total Under 100000 = ";D#
1720  '
1730  REM Part 2 - find smallest path we could delete
1740  MX#=PT#(1): X#=1: FS#=70000000#-MX#: GOAL#=30000000#: FIND#=GOAL#-FS#
1750  PRINT "Free Space ";FS#
1760  PRINT "Need to free up ";FIND#
1770  FOR I=1 TO NP%
1780   D#=PT#(I)
1790   IF D#>=FIND# AND D#<MX# THEN MX#=D#
1800  NEXT I
1810  PRINT "Smallest = ";MX#
1820  PRINT TIME$
1830 END
