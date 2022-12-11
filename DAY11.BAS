1000 REM Advent of Code 2022, Day 11 - Monkey in the Middle
1010 REM Data Structures:
1020 REM There are 7 monkeys and 36 items. We have two parallel arrays
1030 REM that tell for each item what its worry factor is and which
1040 REM monkey is holding it.
1050 REM For a given monkey's turn, we iterate over the array and
1060 REM run any values that match the monkey's index.
1070 REM
1080 REM For each monkey, we store the operation and test
1090 REM Operation: new = old * B + C, where a -1 means "old"
1100 REM   Thus new = old * 19  is encoded (19, 0)
1110 REM   and  new = old + 6   is encoded (1, 6)
1120 REM   and  new = old * old is encoded (-1, 0)
1130 REM Test is stored as 3 numbers,
1140 REM   MOD, PASS, FAIL
1150 REM   So "Test: divisible by 17
1160 REM         If true: throw to monkey 0
1170 REM         If false: throw to monkey 1"
1180 REM   is encoded (17, 0, 1)
1190 REM   and executed by changing the current item index
1200 REM   by IF (WORRY(I) MOD 17) THEN MONKEY(I)=0 ELSE MONKEY(I)=1
1210 REM
2000 DEFDBL W: NITEMS%=0: NMONKEYS%=0
2010 DIM WORRY(40), MONKEY%(40), OPS(8,1), TESTS(8,2), ACTIVE%(8)
2020 GOTO 5000
2030 '
2100 REM Read Monkey(M)
2101  REM Monkey N:
2102  REM   Starting items: N[,N]*
2103  REM   Operation: new = old (+|*) (old|[0-9]+)
2104  REM   Test: divisible by N
2105  REM     If true: throw to monkey N
2106  REM     If false: throw to monkey N
2110  LINE INPUT #1,L$: M%=VAL(MID$(L$,8))
2120  LINE INPUT #1,L$: I%=INSTR(L$,":")
2130  WHILE I%<>0
2140   WORRY(NITEMS%)=VAL(MID$(L$,I%+1)): MONKEY%(NITEMS%)=M%
2150   NITEMS%=NITEMS%+1: I%=INSTR(I%+1,L$,",")
2160  WEND
2170  LINE INPUT #1,L$: OP$=MID$(L$,24,1):T$=MID$(L$,26)
2180  IF T$="old" THEN I%=-1 ELSE I%=VAL(T$)
2190  IF OP$="+" THEN OPS(M%,0)=1: OPS(M%,1)=I%
2195  IF OP$="*" THEN OPS(M%,0)=I%: OPS(M%,1)=0
2200  LINE INPUT #1,L$: TESTS(M%,0)=VAL(MID$(L$,22))
2220  LINE INPUT #1,L$: TESTS(M%,1)=VAL(MID$(L$,30))
2230  LINE INPUT #1,L$: TESTS(M%,2)=VAL(MID$(L$,31))
2240  NMONKEYS%=NMONKEYS%+1
2250 RETURN
2260 '
2300 REM Dump Inventory
2310  FOR I%=0 TO NITEMS%-1
2320   PRINT "Item:";I%;" Monkey:";MONKEY%(I%);" Worry:";WORRY(I%)
2330  NEXT I%
2340 RETURN
2350 '
2400 REM Dump Monkeys
2410  FOR M%=0 TO NMONKEYS%-1
2420   PRINT "Monkey ";M%;": ";
2430   FOR I%=0 TO NITEMS%-1
2440    IF MONKEY%(I%)=M% THEN PRINT WORRY(I%);" ";
2450   NEXT I%
2455   PRINT
2460  NEXT M%
2470 RETURN
2480 '
2500 REM Process Monkey 'M'
2510  FOR I%=0 TO NITEMS%-1
2520   IF MONKEY%(I%)<>M% THEN GOTO 2600
2530   W=WORRY(I%)
2540   L=OPS(M%,0): IF L=-1 THEN L=W
2550   R=OPS(M%,1): IF R=-1 THEN R=W
2560   W=W*L+R
2570   W=FIX(W/3): WORRY(I%)=W: REM Decay and Store worry level
2580   T=TESTS(M%,0): T=W/T - FIX(W/T)
2590   IF T=0 THEN MONKEY%(I%)=TESTS(M%,1) ELSE MONKEY%(I%)=TESTS(M%,2)
2595   ACTIVE%(M%)=ACTIVE%(M%)+1
2600  NEXT I%
2610 RETURN
2620 '
3000 REM Run One Turn
3010  FOR M%=0 TO NMONKEYS%-1
3020   GOSUB 2500: rem Process Monkey M%
3030  NEXT M%
3040 RETURN
3050 '
5000 REM Main
5010 OPEN "DAY11.TXT" FOR INPUT AS #1
5020 PRINT TIME$,"Start"
5030 WHILE NOT EOF(1)
5040  GOSUB 2100: REM Read Monkey
5050  IF NOT EOF(1) THEN LINE INPUT #1,T$
5060 WEND
5070 CLOSE #1
5080 '
5090 REM Challenge 1
5100 FOR TURN%=1 TO 20
5110  GOSUB 3000: REM Run One Turn
5115  PRINT "Turn",TURN%
5120 NEXT TURN%
5125 MA=0: MB=0
5130 FOR M%=0 TO NMONKEYS%-1
5140  PRINT "Monkey";M%;" inspected items";ACTIVE%(M%);"times."
5145  MX=ACTIVE%(M%): IF MX>=MA THEN MB=MA: MA=MX ELSE IF MX>MB THEN MB=MX
5150 NEXT M%
5160 PRINT TIME$,MA*MB
5170 END
