; ýúÞÕ File: ZXMon.p
; ASSEMBLY & BASIC FILE FOR ZX-IDE
; GET ZX-IDE 1.71.01v at https://forum.tlienhard.com/phpBB3/viewtopic.php?f=2&t=802
; TUTORIAL at https://www.sinclairzxworld.com/viewtopic.php?f=6&t=1064
;
; PRESS CTRL+F9 TO BUILD .P FILE

// Entry    20 LET MOVECODE=17504 //4460h
//          30 LET ZXMON=31802    //7C3Ah


format zx81
;labelusenumeric
;LISTON

;ROM calls used by the app
; 07BDH           ; FINDCHR
; 0A2AH           ; CLEAR SCREEN

NOAUTORUN EQU 0
HZ_50	  EQU 55
HZ_60	  EQU 31
SLOW_MODE EQU $40
FAST_MODE EQU $00

ORG	 $4009
; ZX81 MEMORY MAP(bytes 4000 - 4008 not saved to tape)
ERR_NR	  EQU $4000; 16384
FLAGS	  EQU $4001; 16385
ERR_SP	  EQU $4002; 16386
;RAMTOP    EQU $4004; 16388 ;BASIC Line 40 uses this name
MODE	  EQU $4006; 16390
PPC	  EQU $4007; 16391

VERSN	 db $00 	   ;16393 ZX81 BASIC program
E_PPC	 dw $0079	   ;16394 
D_FILE	 dw DFILE_ADDR	   ;16396 
DF_CC	 dw DFILE_ADDR+1   ;16398 
VARS	 dw VARS_ADDR	   ;16400 
DEST	 dw $4A23	   ;16402 
E_LINE	 dw WORKSPACE	   ;16404 
CH_ADD	 dw $49F1	   ;16406 
X_PTR	 dw $0000	   ;16408 
STKBOT	 dw PROGEND+9	   ;16410 
STKEND	 dw PROGEND+9	   ;16412 
BERG	 db $00 	   ;16414 
MEM	 dw MEMBOT	   ;16415 
UNUSED1  db $31 	   ;16417 
DF_SZ	 db $02 	   ;16418 
S_TOP	 dw $0010	   ;16419 
LAST_K	 dw $FFFF	   ;16421 
DEBOUNCE db $FF 	   ;16423 
MARGIN	 db HZ_50	   ;16424
NXTLIN	 dw DFILE_ADDR	   ;16425 
OLDPPC	 dw $FFFE	   ;16427 
FLAGX	 db $00 	   ;16429 
STRLEN	 dw $E271	   ;16430 
T_ADDR	 dw $0C8D	   ;16432 
SEED	 dw $0000	   ;16434 
FRAMES	 dw $9364	   ;16436 
COORDS	 db $00 	   ;16438 
	 db $00 	   ;16439 
PR_CC	 db $BC 	   ;16440 
S_POSN	 db $21 	   ;16441 
	 db $18 	   ;16442 
CDFLAG	 db SLOW_MODE	   ;16443 
PRBUFF	 db $00,$00,$00,$00,$00,$00,$00,$00
	 db $00,$00,$00,$00,$00,$00,$00,$00
	 db $00,$00,$00,$00,$00,$00,$00,$00
	 db $00,$00,$00,$00,$00,$00,$00,$00
	 db $76
MEMBOT	 db $00,$00,$00,$00,$00,$00,$85,$00
	 db $00,$00,$84,$A0,$00,$00,$00,$80
	 db $80,$80,$80,$85,$92,$98,$98,$91
	 db $90
MEM_5	 db $00,$00,$00,$00,$00
UNUSED2  dw $494A	   ;16507

AUTORUN:
   10 REM _asm
      // 984 bytes @ $4082 (16514)

      //ZXMON app variables
      CHAR	   EQU $7FEF
      M_CURSOR	   EQU $7FF0
      D_CURSOR	   EQU $7FF2
      D_END	   EQU $7FF4
      D_START	   EQU $7FF6
      ENDADDR	   EQU $7FF8
      STRTADDR	   EQU $7FFA
      JUMPADDR	   EQU $7FFC
      DISPADDR	   EQU $7FFE


    dbzx 'COPYRIGHT DAVID GONZALES 1983      '
    ORG $7C3A
    ZXMON:   LD      HL,0000H
	     LD      (STRTADDR),HL
	     LD      (ENDADDR),HL
	     LD      HL,7C3AH	     ; ZXMON HIGH ADDRESS
	     LD      (JUMPADDR),HL
	     LD      HL,(D_FILE)
	     LD      DE,016BH	     ; START DISPLAY MID-SCREEN
	     ADD     HL,DE
	     LD      (D_START),HL
	     LD      DE,0108H
	     ADD     HL,DE
	     LD      (D_END),HL
    INIT:    LD      HL,(DISPADDR)
	     LD      (M_CURSOR),HL
	     LD      HL,(D_START)
	     INC     HL
	     LD      (D_CURSOR),HL
	     XOR     A
	     LD      (CHAR),A
    MAIN:    LD      HL,(D_FILE)
	     LD      DE,0294H	     ; DRAW LINE AT 660 CHARS/20 ROWS
	     ADD     HL,DE
	     CALL    DRAWLINE
	     LD      HL,(D_FILE)
	     LD      DE,014AH	     ; DRAW LINE AT 330 CHARS/10 ROWS
	     ADD     HL,DE
	     CALL    DRAWLINE
    MAIN_E2: CALL    D_LEGEND
	     LD      HL,(D_FILE)
	     LD      DE,016BH
	     ADD     HL,DE
	     EX      DE,HL
	     LD      HL,(DISPADDR)
	     LD      BC,1008H
    MAIN_LP1:PUSH    BC
	     CALL    DISP1BYT
	     POP     BC
	     INC     HL
	     DJNZ    MAIN_LP1
	     INC     DE
	     LD      B,10H
	     DEC     C
	     JR      NZ,MAIN_LP1
	     LD      HL,(D_FILE)
	     LD      DE,0273H	     ; 19 ROWS DOWN (627 CHARS)
	     ADD     HL,DE
	     EX      DE,HL
	     LD      HL,M_CURSOR+1
	     CALL    DISPWORD
	     INC     DE
	     LD      HL,(M_CURSOR)
	     CALL    DISP1BYT
	     LD      HL,(D_CURSOR)
	     SET     7,(HL)
	     CALL    GETKEY
	     OR      A
	     RET     Z		     ; EXIT IF SPACE ($0) PRESSED
    PRESSED: CP      0DFH	     ; 'TO'
	     JP      Z,JUMPTO_Q
	     CP      12H	     ; '>'
	     CALL    Z,PAGEUP
	     CP      13H	     ; '<'
	     CALL    Z,PAGEDOWN
	     LD      C,06H	     ; OFFSET -6 (DISP) FOR E_DISP
	     CP      0E4H	     ; 'SLOW' (SHIFT D)
	     JP      Z,E_DISP
    NUMS:    CP      1CH	     ; '0'
	     JR      C,CONT1
	     CP      2CH	     ; 'G'
	     JR      NC,CONT1
	     PUSH    AF
	     CALL    DIGIT
	     POP     AF
    CONT1:   LD      C,00H	     ; OFFSET 0 (END)
	     CP      0E0H	     ; 'STEP' (SHIFT E)
	     JR      Z,ENTER_WD
	     LD      C,02H	     ; OFFSET -2 (START)
	     CP      0E1H	     ; 'LPRINT' (SHIFT S)
	     JR      Z,ENTER_WD
	     LD      C,04H	     ; OFFSET -4 (JUMP)
	     CP      16H	     ; '-' (SHIFT J)
	     JR      NZ,CONT2
    ENTER_WD:PUSH    AF
	     CALL    E_PROMPT
	     POP     AF
    CONT2:   PUSH    AF
	     CP      75H	     ; 'EDIT' (SHIFT 1)
	     JR      Z,4
	     CP      77H	     ; 'RUBOUT' (SHIFT 0)
	     JR      NZ,3
	     CALL    BLOCKMOD
	     POP     AF
	     CP      70H	     ; UP
	     CALL    Z,UP
	     CP      71H	     ; DOWN
	     CALL    Z,DOWN
	     CP      73H	     ; RIGHT
	     PUSH    AF
	     CALL    Z,RIGHT
	     POP     AF
	     CP      72H	     ; LEFT
	     CALL    Z,LEFT
	     JP      MAIN_E2

    PAGEUP:  LD      HL,(M_CURSOR)
	     LD      DE,0080H	     ; M-LENGTH
	     ADD     HL,DE
	     LD      (M_CURSOR),HL
	     LD      HL,(DISPADDR)
	     ADD     HL,DE
	     LD      (DISPADDR),HL
	     RET

    PAGEDOWN:LD      HL,(M_CURSOR)
	     LD      DE,0080H
	     OR      A
	     SBC     HL,DE
	     LD      (M_CURSOR),HL
	     LD      HL,(DISPADDR)
	     OR      A
	     SBC     HL,DE
	     LD      (DISPADDR),HL
	     RET

    DOWN:    LD      HL,(D_CURSOR)
	     LD      DE,0021H	     ; ROWLNGTH
	     ADD     HL,DE
	     EX      DE,HL
	     LD      HL,(D_END)
	     OR      A
	     SBC     HL,DE
	     RET     C
	     LD      (D_CURSOR),DE
	     LD      HL,(M_CURSOR)
	     LD      DE,0010H	     ; M-ROWLEN
	     ADD     HL,DE
	     LD      (M_CURSOR),HL
	     RET

    UP:      LD      HL,(D_CURSOR)
	     LD      DE,0021H	     ; ROWLNGTH
	     OR      A
	     SBC     HL,DE
	     EX      DE,HL
	     LD      HL,(D_START)
	     OR      A
	     SBC     HL,DE
	     RET     NC
	     LD      (D_CURSOR),DE
	     LD      HL,(M_CURSOR)
	     LD      DE,0010H	     ; M-ROWLEN
	     OR      A
	     SBC     HL,DE
	     LD      (M_CURSOR),HL
	     RET

    LEFT:    LD      DE,(D_START)
	     LD      HL,(D_CURSOR)
	     DEC     HL
	     LD      A,(HL)
	     CP      76H	     ; 'NEWLINE'
	     JR      NZ,1
	     DEC     HL 	     ; NEW LINE
	     PUSH    HL
	     OR      A
	     SBC     HL,DE
	     POP     HL
	     JR      NC,SAMELINE
    NEWLINE: LD      HL,(DISPADDR)
	     LD      DE,0080H
	     OR      A
	     SBC     HL,DE
	     LD      (DISPADDR),HL
	     LD      HL,(D_END)
	     DEC     HL
    SAMELINE:LD      (D_CURSOR),HL
	     LD      HL,CHAR
	     LD      A,01H
	     XOR     (HL)
	     LD      (HL),A
	     RET     Z
	     LD      HL,(M_CURSOR)
	     DEC     HL
	     LD      (M_CURSOR),HL
	     RET

    RIGHT:   LD      DE,(D_END)
	     LD      HL,(D_CURSOR)
	     INC     HL
	     LD      A,(HL)
	     CP      76H	     ; 'NEWLINE'
	     JR      NZ,1
	     INC     HL
	     PUSH    HL
	     OR      A
	     SBC     HL,DE
	     POP     HL
	     JR      C,0EH
	     LD      HL,(DISPADDR)
	     LD      DE,0080H
	     ADD     HL,DE
	     LD      (DISPADDR),HL
	     LD      HL,(D_START)
	     INC     HL
     L424B:  LD      (D_CURSOR),HL
	     LD      HL,CHAR
	     LD      A,01H
	     XOR     (HL)
	     LD      (HL),A
	     RET     NZ
	     LD      HL,(M_CURSOR)
	     INC     HL
	     LD      (M_CURSOR),HL
	     RET

    GETKEY:  LD      A,(403BH)	     ; CDFLAG FAST/SLOW FLAGS
	     BIT     7,A
	     JR      NZ,SCANKEY
    PUISM:   LD      HL,(D_FILE)     ; PLEASE USE IN SLOW MODE
	     LD      DE,02D7H	     ; STATUS LINE ~22 LINES DOWN
	     ADD     HL,DE
	     EX      DE,HL
	     LD      BC,0017H	     ; LENGTH L_PUISM
	     LD      HL,L_PUISM
	     LDIR		     ; COPY
	     XOR     A
	     RET
    SCANKEY: LD      A,(4025H)	     ; LAST_K
	     INC     A
	     JR      NZ,SCANKEY
    GOTKEY:  LD      BC,(4025H)      ; LAST_K
	     LD      A,C
	     INC     A
	     JR      Z,GOTKEY
	     NOP
	     NOP
    TRANSLT: CALL    07BDH	     ; FINDCHR
	     JR      NC,GOTKEY
	     LD      A,(HL)
	     RET

    BLOCKMOD:LD      HL,(M_CURSOR)
    BOUNDCHK:LD      DE,(STRTADDR)
	     OR      A
	     SBC     HL,DE
	     JR      C,CNIB
	     LD      HL,(M_CURSOR)
	     LD      DE,(ENDADDR)
	     OR      A
	     SBC     HL,DE
	     JR      NC,CNIB
    E_BEGIN: LD      HL,(ENDADDR)
	     LD      BC,(M_CURSOR)
	     OR      A
	     SBC     HL,BC
	     PUSH    HL
	     POP     BC
    E_OR_R:  CP      75H	     ; 'EDIT'
	     JR      Z,E_EDIT
    E_RUBOUT:LD      HL,(M_CURSOR)
	     PUSH    HL
	     POP     DE
	     INC     HL
	     LDIR		     ; DELETE BYTE, MOVE BLOCK DOWN
	     JR      E_FINALLY
    E_EDIT:  LD      HL,(ENDADDR)
	     PUSH    HL
	     POP     DE
	     DEC     HL
	     LDDR		     ; INSERT BYTE, MOVE BLOCK UP
   E_FINALLY:XOR     A		     ; ZERO OUT A REGISTER
	     LD      (DE),A	     ; INSERT BYTE 00
	     RET
    CNIB:    LD      HL,(D_FILE)
	     LD      DE,02F8H	     ; 23 LINES DOWN
	     ADD     HL,DE
	     EX      DE,HL
	     PUSH    DE
	     LD      HL,L_CNIB	     ; CURSOR NOT IN BLOCK
	     LD      BC,0013H	     ; LENGTH L_CNIB
	     LDIR		     ; COPY TO (DE)
	     LD      DE,3800H
    CNIB_LP1:DEC     DE
	     LD      A,D
	     OR      E
	     JR      NZ,CNIB_LP1
	     POP     HL
	     LD      B,13H
    CNIB_LP2:LD      (HL),00H
	     INC     HL
	     DJNZ    CNIB_LP2
	     RET

    JUMPTO_Q:LD      HL,(D_FILE)
	     PUSH    HL
	     LD      DE,02F8H	     ; STATUS LINE 2
	     ADD     HL,DE
	     EX      DE,HL
	     LD      HL,L_JUMPTO
	     LD      BC,0011H	     ; LENGTH L_JUMPTO
	     LDIR		     ; COPY
	     POP     HL
	     LD      DE,02FFH
	     ADD     HL,DE
	     EX      DE,HL
	     LD      HL,JUMPADDR+1
	     CALL    DISPWORD
    YN_Q:    CALL    GETKEY
	     CP      33H	     ; 'N'
	     JR      Z,04
	     CP      3EH	     ; 'Y'
	     JR      NZ,-12; YN_Q
	     PUSH    AF
	     CALL    0A2AH	     ; CLEAR SCREEN
	     POP     AF
	     CP      33H	     ; 'N'
	     JP      Z,MAIN
	     LD      HL,(JUMPADDR)
	     JP      (HL)

    GET_RNIB:LD      B,04H
	     SRL     A
	     DJNZ    -4
	     RET

    MULTX16: LD      B,04H	     ; SHIFT LEFT 4 TIMES
	     SLA     A
	     DJNZ    -4
	     RET

    DISP1BYT:INC     DE 	     ; HL=MEM-PTR, DE=DISP-PTR
	     LD      A,(HL)
	     CALL    GET_RNIB
	     ADD     A,1CH	     ; '0'
	     LD      (DE),A	     ; DISPLAY NIBBLE
	     INC     DE
	     LD      A,(HL)
	     AND     0FH
	     ADD     A,1CH	     ; '0'
	     LD      (DE),A	     ; DISPLAY NIBBLE
	     RET

    DIGIT:   SUB     1CH	     ; '0'
	     LD      HL,CHAR
	     BIT     0,(HL)
	     JR      Z,LNIB_D
	     LD      C,0F0H	     ; RNIB MASK
	     JR      PUTDIGIT
    LNIB_D:  CALL    MULTX16
	     LD      C,0FH	     ; LNIB MASK
    PUTDIGIT:LD      HL,(M_CURSOR)
	     LD      B,A
	     LD      A,(HL)
	     AND     C
	     OR      B
	     LD      (HL),A
	     CALL    RIGHT
	     RET

    DRAWLINE:INC     HL
	     LD      A,(HL)
	     CP      76H	     ; 'NEWLINE'
	     RET     Z
	     LD      (HL),80H	     ; BLACK SQUARE
	     JR      DRAWLINE

    DISPWORD:CALL    DISP1BYT
	     DEC     HL
	     CALL    DISP1BYT
	     DEC     HL
	     RET

    D_LEGEND:LD      HL,(D_FILE)
	     LD      DE,012AH	     ; 298 CHARS INTO DISPLAY
	     ADD     HL,DE	     ; (9 LINES + 1 CHAR)
	     EX      DE,HL
	     LD      HL,DISPADDR + 1
	     LD      BC,L_DJSE
	     LD      A,4	     ; 4 TIMES FOR 4 LABELS
    REPEAT_: PUSH    BC
	     PUSH    AF
	     LD      A,(BC)
	     INC     DE
	     INC     DE
	     LD      (DE),A	     ; LEGEND (ONE OF D,J,S OR E)
	     INC     DE
	     CALL    DISPWORD	     ; (DECS HL)
	     POP     AF
	     POP     BC
	     INC     BC 	     ; NEXT LEGEND
	     DEC     A
	     JR      NZ,REPEAT_
	     RET

    ENTER:   ;ENTER ADDR ROUTINES
    E_DISP:  CALL    E_PROMPT
	     JP      INIT
    E_PROMPT:LD      HL,(D_FILE)     ; ENTER JUMP ADDR
	     LD      DE,02D7H	     ; STATUS LINE
	     ADD     HL,DE
	     EX      DE,HL
	     LD      B,00H
	     LD      HL,ENDADDR+1
	     ADD     HL,BC
	     PUSH    HL
	     SRL     C
	     LD      A,C
	     RLCA
	     RLCA
	     ADD     A,C
	     LD      C,A
   CPY_LEGND:LD      HL,L_LEGEND
	     ADD     HL,BC	     ; ADD OFFSET
	     LD      C,05H	     ; ONE LABEL
	     LDIR
   CPY_PRMPT:LD      HL,L_????
	     LD      C,06H	     ; LENGTH L_????
	     PUSH    DE
	     LDIR
	     POP     DE
	     INC     DE
	     LD      HL,(D_CURSOR)
	     LD      (D_CURSOR),DE
	     EX      (SP),HL
	     CALL    E_GETBYT
	     CALL    E_GETBYT
	     POP     HL
	     LD      (D_CURSOR),HL
	     RET

    E_GETBYT:PUSH    HL
	     CALL    E_GETNIB
	     POP     HL
	     SUB     1CH	     ; '0'
	     CALL    MULTX16
	     LD      (HL),A
	     PUSH    HL
	     CALL    E_GETNIB
	     POP     HL
	     SUB     1CH	     ; '0'
	     OR      (HL)
	     LD      (HL),A
	     DEC     HL
	     RET

    E_GETNIB:CALL    GETKEY
	     CP      1CH	     ; '0'
	     JR      C,E_GETNIB
	     CP      2CH	     ; 'F'+1
	     JR      NC,E_GETNIB
	     LD      DE,(D_CURSOR)
	     INC     DE
	     LD      (D_CURSOR),DE
	     LD      (DE),A
	     RET

	     dw $0000
	     L_JUMPTO	dbzx 'JUMP TO XXXX Y/N?'
	     L_PUISM	dbzx 'PLEASE USE IN SLOW MODE'
	     L_CNIB	dbzx 'CURSOR NOT IN BLOCK'
	     L_????	dbzx ': ????'
	     L_DJSE	dbzx 'DJSE'
	     L_LEGEND	dbzx '  ENDSTART JUMP DISP'
      endZXMON:
      ORG $445A
      END _asm

   16 REM _asm
      // 20 bytes @ $4460 (17504)
    MOVER:   LD      DE,7C3AH	     ;ZXMON HIGH MEMORY
	     LD      HL,40A5H	     ;ZXMON IN REM 10
	     LD      BC,03C0H	     ;LENGTH OF ZXMON
	     LDIR		     ;COPY
	     RET     
      END _asm

   20 LET MOVECODE=17504
   30 LET ZXMON=31802
   40 LET RAMTOP=PEEK 16388+PEEK 16389*256
   50 IF RAMTOP>ZXMON THEN GOTO 100
   60 LET L=USR MOVECODE
   70 PRINT AT 8,0;"TO USE ZXMON at ANY TIME, TYPE"
   80 PRINT AT 10,0;"LET L= USR 31802"
   90 PRINT AT 20,0;"-EVEN AFTER NEW-"
   99 STOP 
  100 PRINT "MEMORY  MUST  BE  RESERVED FOR"
  110 PRINT "ZXMON. THIS IS DONE BY CHANGING"
  120 PRINT "THE SYSTEM VARIABLE RAMTOP"
  130 PRINT AT 6,0;"FIRST TYPE POKE 16389,124"
  140 PRINT "THEN TYPE NEW"
  150 PRINT ,,"THEN LOAD ZXMON AND RUN"
  160 PRINT AT 19,0;"THIS SHOULD BE DONE  WHEN  FIRST"
  170 PRINT "TURNING  ON THE MACHINE--TO SAVE"
  180 PRINT "LOADING ZXMON TWICE."


DFILE_ADDR:    ;N/L (ASCII 13=CR, which ZX-IDE maps to ZX81 N/L when using dbzx)
  db $76
  dbzx '                                ',13	  ;line 0
  dbzx '                                ',13	  ;line 1
  dbzx '                                ',13	  ;line 2
  dbzx '                                ',13	  ;line 3
  dbzx '                                ',13	  ;line 4
  dbzx '                                ',13	  ;line 5
  dbzx '                                ',13	  ;line 6
  dbzx '                                ',13	  ;line 7
  dbzx '                                ',13	  ;line 8
  dbzx '                                ',13	  ;line 9
  dbzx '                                ',13	  ;line 10
  dbzx '                                ',13	  ;line 11
  dbzx '                                ',13	  ;line 12
  dbzx '                                ',13	  ;line 13
  dbzx '                                ',13	  ;line 14
  dbzx '                                ',13	  ;line 15
  dbzx '                                ',13	  ;line 16
  dbzx '                                ',13	  ;line 17
  dbzx '                                ',13	  ;line 18
  dbzx '                                ',13	  ;line 19
  dbzx '                                ',13	  ;line 20
  dbzx '                                ',13	  ;line 21
  dbzx '                                ',13	  ;line 22
  dbzx '                                ',13	  ;line 23
DFILE_END:
assert (DFILE_END-DFILE_ADDR)>24     ;full display = 793 bytes

VARS_ADDR:   ;$49E8 18920
  //Variables Resident in Memory:
  //none

lastbyte:    ;80MARK
db $80	     ;end of VARS

WORKSPACE:   ;$49E9 18921
;ENDBYTE:
PROGEND:
afterlast:
; end of program