100 REM Advent of Code 2022, Day 5, Part 1
110 DEFINT A-Z
120 INPUT "File";F$
130 OPEN F$ FOR INPUT AS #1
140 INPUT #1,PADS
150 DIM A$(PADS)
160 FOR I = 1 TO PADS: LINE INPUT #1,A$(I): NEXT I
170 WHILE NOT EOF(1)
180  INPUT #1,MV,SRC,DST
190  S$ = A$(SRC): SLEN=LEN(S$)
200  T$ = ""
210  FOR J = 1 TO MV
220   T$=T$+MID$(S$,SLEN-J+1,1)
230  NEXT J
240  A$(SRC) = LEFT$(S$,SLEN-MV)
250  A$(DST) = A$(DST)+T$
260 WEND
270 CLOSE #1
280 PRINT "Tops = ";
290 FOR I = 1 TO PADS: PRINT RIGHT$(A$(I),1); : NEXT I
300 END
