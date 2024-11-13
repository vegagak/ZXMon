; ýúÞÕ File: ZXMon.p
; ASSEMBLY FILE FOR SJASMPLUS v1.20.3   build command: sjasmplus demo_sj.asm --raw=demo.p
;                or SJASM     v0.39j    build command: sjasm demo_sj.asm demo.p
; GET SJASM AT https://github.com/Konamiman/Sjasm
; GET SJASMPLUS AT https://github.com/z00m128/sjasmplus

// Entry    20 LET MOVECODE=17504 //4460h
//          30 LET ZXMON=31802    //7C3Ah

;ROM calls used by the app
; 07BDH           ; FINDCHR
; 0A2AH           ; CLEAR SCREEN

;defs
;ZX81 char codes/how to survive without ASCII
 include zx81charcodes.inc
;system variables
 include zx81sys.inc

; ===========================================================
; BASIC code begins
; ===========================================================

;  10 REM _asm
	  db 0,0010			  ;LINE NUMBER
	  dw Line0010end-Line0010+1	  ;LINE LENGTH
Line0010  db ZX_REM  ;984 bytes @ $4082 (16514)
//ZXMON app variables
CHAR	     EQU $7FEF
M_CURSOR     EQU $7FF0
D_CURSOR     EQU $7FF2
D_END	     EQU $7FF4
D_START      EQU $7FF6
ENDADDR      EQU $7FF8
STRTADDR     EQU $7FFA
JUMPADDR     EQU $7FFC
DISPADDR     EQU $7FFE
   ; dbzx 'COPYRIGHT DAVID GONZALES 1983      '
 db _C,_O,_P,_Y,_R,_I,_G,_H,_T,__,_D,_A,_V,_I,_D,__,_G,_O,_N,_Z,_A,_L,_E,_S,__,_1,_9,_8,_3,__,__,__,__,__,__
ZXMONlow:
 ORG $7C3A
ZXMON:	   LD	   HL,0000H
	   LD	   (STRTADDR),HL
	   LD	   (ENDADDR),HL
	   LD	   HL,7C3AH	   ; ZXMON HIGH ADDRESS
	   LD	   (JUMPADDR),HL
	   LD	   HL,(D_FILE)
	   LD	   DE,016BH	   ; START DISPLAY MID-SCREEN
	   ADD	   HL,DE
	   LD	   (D_START),HL
	   LD	   DE,0108H
	   ADD	   HL,DE
	   LD	   (D_END),HL
INIT:	   LD	   HL,(DISPADDR)
	   LD	   (M_CURSOR),HL
	   LD	   HL,(D_START)
	   INC	   HL
	   LD	   (D_CURSOR),HL
	   XOR	   A
	   LD	   (CHAR),A
MAIN:	   LD	   HL,(D_FILE)
	   LD	   DE,0294H	   ; DRAW LINE AT 660 CHARS/20 ROWS
	   ADD	   HL,DE
	   CALL    DRAWLINE
	   LD	   HL,(D_FILE)
	   LD	   DE,014AH	   ; DRAW LINE AT 330 CHARS/10 ROWS
	   ADD	   HL,DE
	   CALL    DRAWLINE
MAIN_E2:   CALL    D_LEGEND
	   LD	   HL,(D_FILE)
	   LD	   DE,016BH
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   HL,(DISPADDR)
	   LD	   BC,1008H
MAIN_LP1:  PUSH    BC
	   CALL    DISP1BYT
	   POP	   BC
	   INC	   HL
	   DJNZ    MAIN_LP1
	   INC	   DE
	   LD	   B,10H
	   DEC	   C
	   JR	   NZ,MAIN_LP1
	   LD	   HL,(D_FILE)
	   LD	   DE,0273H	   ; 19 ROWS DOWN (627 CHARS)
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   HL,M_CURSOR+1
	   CALL    DISPWORD
	   INC	   DE
	   LD	   HL,(M_CURSOR)
	   CALL    DISP1BYT
	   LD	   HL,(D_CURSOR)
	   SET	   7,(HL)
	   CALL    GETKEY
	   OR	   A
	   RET	   Z		   ; EXIT IF SPACE ($0) PRESSED
PRESSED:   CP	   0DFH 	   ; 'TO'
	   JP	   Z,JUMPTO_Q
	   CP	   12H		   ; '>'
	   CALL    Z,PAGEUP
	   CP	   13H		   ; '<'
	   CALL    Z,PAGEDOWN
	   LD	   C,06H	   ; OFFSET -6 (DISP) FOR E_DISP
	   CP	   0E4H 	   ; 'SLOW' (SHIFT D)
	   JP	   Z,E_DISP
NUMS:	   CP	   1CH		   ; '0'
	   JR	   C,CONT1
	   CP	   2CH		   ; 'G'
	   JR	   NC,CONT1
	   PUSH    AF
	   CALL    DIGIT
	   POP	   AF
CONT1:	   LD	   C,00H	   ; OFFSET 0 (END)
	   CP	   0E0H 	   ; 'STEP' (SHIFT E)
	   JR	   Z,ENTER_WD
	   LD	   C,02H	   ; OFFSET -2 (START)
	   CP	   0E1H 	   ; 'LPRINT' (SHIFT S)
	   JR	   Z,ENTER_WD
	   LD	   C,04H	   ; OFFSET -4 (JUMP)
	   CP	   16H		   ; '-' (SHIFT J)
	   JR	   NZ,CONT2
ENTER_WD:  PUSH    AF
	   CALL    E_PROMPT
	   POP	   AF
CONT2:	   PUSH    AF
	   CP	   75H		   ; 'EDIT' (SHIFT 1)
	   JR	   Z,C24
	   CP	   77H		   ; 'RUBOUT' (SHIFT 0)
	   JR	   NZ,C23
C24:	   CALL    BLOCKMOD
C23:	   POP	   AF
	   CP	   70H		   ; UP
	   CALL    Z,UP
	   CP	   71H		   ; DOWN
	   CALL    Z,DOWN
	   CP	   73H		   ; RIGHT
	   PUSH    AF
	   CALL    Z,RIGHT
	   POP	   AF
	   CP	   72H		   ; LEFT
	   CALL    Z,LEFT
	   JP	   MAIN_E2

PAGEUP:    LD	   HL,(M_CURSOR)
	   LD	   DE,0080H	   ; M-LENGTH
	   ADD	   HL,DE
	   LD	   (M_CURSOR),HL
	   LD	   HL,(DISPADDR)
	   ADD	   HL,DE
	   LD	   (DISPADDR),HL
	   RET

PAGEDOWN:  LD	   HL,(M_CURSOR)
	   LD	   DE,0080H
	   OR	   A
	   SBC	   HL,DE
	   LD	   (M_CURSOR),HL
	   LD	   HL,(DISPADDR)
	   OR	   A
	   SBC	   HL,DE
	   LD	   (DISPADDR),HL
	   RET

DOWN:	   LD	   HL,(D_CURSOR)
	   LD	   DE,0021H	   ; ROWLNGTH
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   HL,(D_END)
	   OR	   A
	   SBC	   HL,DE
	   RET	   C
	   LD	   (D_CURSOR),DE
	   LD	   HL,(M_CURSOR)
	   LD	   DE,0010H	   ; M-ROWLEN
	   ADD	   HL,DE
	   LD	   (M_CURSOR),HL
	   RET

UP:	   LD	   HL,(D_CURSOR)
	   LD	   DE,0021H	   ; ROWLNGTH
	   OR	   A
	   SBC	   HL,DE
	   EX	   DE,HL
	   LD	   HL,(D_START)
	   OR	   A
	   SBC	   HL,DE
	   RET	   NC
	   LD	   (D_CURSOR),DE
	   LD	   HL,(M_CURSOR)
	   LD	   DE,0010H	   ; M-ROWLEN
	   OR	   A
	   SBC	   HL,DE
	   LD	   (M_CURSOR),HL
	   RET

LEFT:	   LD	   DE,(D_START)
	   LD	   HL,(D_CURSOR)
	   DEC	   HL
	   LD	   A,(HL)
	   CP	   76H		   ; 'NEWLINE'
	   JR	   NZ,L1
	   DEC	   HL		   ; NEW LINE
L1:	   PUSH    HL
	   OR	   A
	   SBC	   HL,DE
	   POP	   HL
	   JR	   NC,SAMELINE
NEWLINE:   LD	   HL,(DISPADDR)
	   LD	   DE,0080H
	   OR	   A
	   SBC	   HL,DE
	   LD	   (DISPADDR),HL
	   LD	   HL,(D_END)
	   DEC	   HL
SAMELINE:  LD	   (D_CURSOR),HL
	   LD	   HL,CHAR
	   LD	   A,01H
	   XOR	   (HL)
	   LD	   (HL),A
	   RET	   Z
	   LD	   HL,(M_CURSOR)
	   DEC	   HL
	   LD	   (M_CURSOR),HL
	   RET

RIGHT:	   LD	   DE,(D_END)
	   LD	   HL,(D_CURSOR)
	   INC	   HL
	   LD	   A,(HL)
	   CP	   76H		   ; 'NEWLINE'
	   JR	   NZ,R1
	   INC	   HL
R1:	   PUSH    HL
	   OR	   A
	   SBC	   HL,DE
	   POP	   HL
	   JR	   C,RE
	   LD	   HL,(DISPADDR)
	   LD	   DE,0080H
	   ADD	   HL,DE
	   LD	   (DISPADDR),HL
	   LD	   HL,(D_START)
	   INC	   HL
RE:	   LD	   (D_CURSOR),HL ;L424B:
	   LD	   HL,CHAR
	   LD	   A,01H
	   XOR	   (HL)
	   LD	   (HL),A
	   RET	   NZ
	   LD	   HL,(M_CURSOR)
	   INC	   HL
	   LD	   (M_CURSOR),HL
	   RET

GETKEY:    LD	   A,(403BH)	   ; CDFLAG FAST/SLOW FLAGS
	   BIT	   7,A
	   JR	   NZ,SCANKEY
PUISM:	   LD	   HL,(D_FILE)	   ; PLEASE USE IN SLOW MODE
	   LD	   DE,02D7H	   ; STATUS LINE ~22 LINES DOWN
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   BC,0017H	   ; LENGTH L_PUISM
	   LD	   HL,L_PUISM
	   LDIR 		   ; COPY
	   XOR	   A
	   RET
SCANKEY:   LD	   A,(4025H)	   ; LAST_K
	   INC	   A
	   JR	   NZ,SCANKEY
GOTKEY:    LD	   BC,(4025H)	   ; LAST_K
	   LD	   A,C
	   INC	   A
	   JR	   Z,GOTKEY
	   NOP
	   NOP
TRANSLT:   CALL    07BDH	   ; FINDCHR
	   JR	   NC,GOTKEY
	   LD	   A,(HL)
	   RET

BLOCKMOD:  LD	   HL,(M_CURSOR)
BOUNDCHK:  LD	   DE,(STRTADDR)
	   OR	   A
	   SBC	   HL,DE
	   JR	   C,CNIB
	   LD	   HL,(M_CURSOR)
	   LD	   DE,(ENDADDR)
	   OR	   A
	   SBC	   HL,DE
	   JR	   NC,CNIB
E_BEGIN:   LD	   HL,(ENDADDR)
	   LD	   BC,(M_CURSOR)
	   OR	   A
	   SBC	   HL,BC
	   PUSH    HL
	   POP	   BC
E_OR_R:    CP	   75H		   ; 'EDIT'
	   JR	   Z,E_EDIT
E_RUBOUT:  LD	   HL,(M_CURSOR)
	   PUSH    HL
	   POP	   DE
	   INC	   HL
	   LDIR 		   ; DELETE BYTE, MOVE BLOCK DOWN
	   JR	   E_FINALLY
E_EDIT:    LD	   HL,(ENDADDR)
	   PUSH    HL
	   POP	   DE
	   DEC	   HL
	   LDDR 		   ; INSERT BYTE, MOVE BLOCK UP
E_FINALLY: XOR	   A		   ; ZERO OUT A REGISTER
	   LD	   (DE),A	   ; INSERT BYTE 00
	   RET
CNIB:	   LD	   HL,(D_FILE)
	   LD	   DE,02F8H	   ; 23 LINES DOWN
	   ADD	   HL,DE
	   EX	   DE,HL
	   PUSH    DE
	   LD	   HL,L_CNIB	   ; CURSOR NOT IN BLOCK
	   LD	   BC,0013H	   ; LENGTH L_CNIB
	   LDIR 		   ; COPY TO (DE)
	   LD	   DE,3800H
CNIB_LP1:  DEC	   DE
	   LD	   A,D
	   OR	   E
	   JR	   NZ,CNIB_LP1
	   POP	   HL
	   LD	   B,13H
CNIB_LP2:  LD	   (HL),00H
	   INC	   HL
	   DJNZ    CNIB_LP2
	   RET

JUMPTO_Q:  LD	   HL,(D_FILE)
	   PUSH    HL
	   LD	   DE,02F8H	   ; STATUS LINE 2
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   HL,L_JUMPTO
	   LD	   BC,0011H	   ; LENGTH L_JUMPTO
	   LDIR 		   ; COPY
	   POP	   HL
	   LD	   DE,02FFH
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   HL,JUMPADDR+1
	   CALL    DISPWORD
YN_Q:	   CALL    GETKEY
	   CP	   33H		   ; 'N'
	   JR	   Z,Y4
	   CP	   3EH		   ; 'Y'
	   JR	   NZ,YN_Q-1
Y4:	   PUSH    AF
	   CALL    0A2AH	   ; CLEAR SCREEN
	   POP	   AF
	   CP	   33H		   ; 'N'
	   JP	   Z,MAIN
	   LD	   HL,(JUMPADDR)
	   JP	   (HL)

GET_RNIB:  LD	   B,04H
G2:	   SRL	   A
	   DJNZ    G2
	   RET

MULTX16:   LD	   B,04H	   ; SHIFT LEFT 4 TIMES
M_4:	   SLA	   A
	   DJNZ    M_4
	   RET

DISP1BYT:  INC	   DE		   ; HL=MEM-PTR, DE=DISP-PTR
	   LD	   A,(HL)
	   CALL    GET_RNIB
	   ADD	   A,1CH	   ; '0'
	   LD	   (DE),A	   ; DISPLAY NIBBLE
	   INC	   DE
	   LD	   A,(HL)
	   AND	   0FH
	   ADD	   A,1CH	   ; '0'
	   LD	   (DE),A	   ; DISPLAY NIBBLE
	   RET

DIGIT:	   SUB	   1CH		   ; '0'
	   LD	   HL,CHAR
	   BIT	   0,(HL)
	   JR	   Z,LNIB_D
	   LD	   C,0F0H	   ; RNIB MASK
	   JR	   PUTDIGIT
LNIB_D:    CALL    MULTX16
	   LD	   C,0FH	   ; LNIB MASK
PUTDIGIT:  LD	   HL,(M_CURSOR)
	   LD	   B,A
	   LD	   A,(HL)
	   AND	   C
	   OR	   B
	   LD	   (HL),A
	   CALL    RIGHT
	   RET

DRAWLINE:  INC	   HL
	   LD	   A,(HL)
	   CP	   76H		   ; 'NEWLINE'
	   RET	   Z
	   LD	   (HL),80H	   ; BLACK SQUARE
	   JR	   DRAWLINE

DISPWORD:  CALL    DISP1BYT
	   DEC	   HL
	   CALL    DISP1BYT
	   DEC	   HL
	   RET

D_LEGEND:  LD	   HL,(D_FILE)
	   LD	   DE,012AH	   ; 298 CHARS INTO DISPLAY
	   ADD	   HL,DE	   ; (9 LINES + 1 CHAR)
	   EX	   DE,HL
	   LD	   HL,DISPADDR + 1
	   LD	   BC,L_DJSE
	   LD	   A,4		   ; 4 TIMES FOR 4 LABELS
REPEAT_:   PUSH    BC
	   PUSH    AF
	   LD	   A,(BC)
	   INC	   DE
	   INC	   DE
	   LD	   (DE),A	   ; LEGEND (ONE OF D,J,S OR E)
	   INC	   DE
	   CALL    DISPWORD	   ; (DECS HL)
	   POP	   AF
	   POP	   BC
	   INC	   BC		   ; NEXT LEGEND
	   DEC	   A
	   JR	   NZ,REPEAT_
	   RET

ENTER:	   ;ENTER ADDR ROUTINES
E_DISP:    CALL    E_PROMPT
	   JP	   INIT
E_PROMPT:  LD	   HL,(D_FILE)	   ; ENTER JUMP ADDR
	   LD	   DE,02D7H	   ; STATUS LINE
	   ADD	   HL,DE
	   EX	   DE,HL
	   LD	   B,00H
	   LD	   HL,ENDADDR+1
	   ADD	   HL,BC
	   PUSH    HL
	   SRL	   C
	   LD	   A,C
	   RLCA
	   RLCA
	   ADD	   A,C
	   LD	   C,A
CPY_LEGND: LD	   HL,L_LEGEND
	   ADD	   HL,BC	   ; ADD OFFSET
	   LD	   C,05H	   ; ONE LABEL
	   LDIR
CPY_PRMPT: LD	   HL,L_????
	   LD	   C,06H	   ; LENGTH L_????
	   PUSH    DE
	   LDIR
	   POP	   DE
	   INC	   DE
	   LD	   HL,(D_CURSOR)
	   LD	   (D_CURSOR),DE
	   EX	   (SP),HL
	   CALL    E_GETBYT
	   CALL    E_GETBYT
	   POP	   HL
	   LD	   (D_CURSOR),HL
	   RET

E_GETBYT:  PUSH    HL
	   CALL    E_GETNIB
	   POP	   HL
	   SUB	   1CH		   ; '0'
	   CALL    MULTX16
	   LD	   (HL),A
	   PUSH    HL
	   CALL    E_GETNIB
	   POP	   HL
	   SUB	   1CH		   ; '0'
	   OR	   (HL)
	   LD	   (HL),A
	   DEC	   HL
	   RET

E_GETNIB:  CALL    GETKEY
	   CP	   1CH		   ; '0'
	   JR	   C,E_GETNIB
	   CP	   2CH		   ; 'F'+1
	   JR	   NC,E_GETNIB
	   LD	   DE,(D_CURSOR)
	   INC	   DE
	   LD	   (D_CURSOR),DE
	   LD	   (DE),A
	   RET

	   dw $0000
L_JUMPTO   db _J,_U,_M,_P,__,_T,_O,__,_X,_X,_X,_X,__,_Y,_SL,_N,_QM
L_PUISM    db _P,_L,_E,_A,_S,_E,__,_U,_S,_E,__,_I,_N,__,_S,_L,_O,_W,__,_M,_O,_D,_E
L_CNIB	   db _C,_U,_R,_S,_O,_R,__,_N,_O,_T,__,_I,_N,__,_B,_L,_O,_C,_K
L_????	   db _CL,__,_QM,_QM,_QM,_QM
L_DJSE	   db _D,_J,_S,_E
L_LEGEND   db __,__,_E,_N,_D,_S,_T,_A,_R,_T,__,_J,_U,_M,_P,__,_D,_I,_S,_P
endZXMON:

  org ZXMONlow+endZXMON-ZXMON	     ; $445A

Line0010end db $76			  ;ZX_NEWLINE

;  16 REM _asm
	  db 0,0016			  ;LINE NUMBER
	  dw Line0016end-Line0016+1	  ;LINE LENGTH
Line0016  db $EA  ;ZX_REM	 ;20 bytes @ $4460 (17504)
MOVER:	 LD	 DE,7C3AH	 ;ZXMON HIGH MEMORY
	 LD	 HL,40A5H	 ;ZXMON IN REM 10
	 LD	 BC,03C0H	 ;LENGTH OF ZXMON
	 LDIR			 ;COPY
	 RET
	 db $3A,$7C
	 NOP
	 NOP
	 NOP
	 NOP
	 NOP
	 db $80
Line0016end db $76			  ;ZX_NEWLINE

;  20 LET MOVECODE=17504
	  db 0,0020			  ;LINE NUMBER
	  dw Line0020end-Line0020+1	  ;LINE LENGTH
Line0020  db ZX_LET
	  db _M,_O,_V,_E,_C,_O,_D,_E,_EQ,_1,_7,_5,_0,_4
	  db ZX_NUMBER
	  db $8F,$08,$C0,$00,$00
Line0020end db $76			  ;ZX_NEWLINE

;  30 LET ZXMON=31802
	  db 0,0030			  ;LINE NUMBER
	  dw Line0030end-Line0030+1	  ;LINE LENGTH
Line0030  db ZX_LET
	  db _Z,_X,_M,_O,_N,_EQ,_3,_1,_8,_0,_2
	  db ZX_NUMBER
	  db $8F,$78,$74,$00,$00
Line0030end db $76			  ;ZX_NEWLINE

;  40 LET RAMTOP=PEEK 16388+PEEK 16389*256
	  db 0,0040			  ;LINE NUMBER
	  dw Line0040end-Line0040+1	  ;LINE LENGTH
Line0040  db ZX_LET
	  db _R,_A,_M,_T,_O,_P,_EQ,ZX_PEEK,_1,_6,_3,_8,_8
	  db ZX_NUMBER,$8F,$00,$08,$00,$00
	  db _PL,ZX_PEEK,_1,_6,_3,_8,_9
	  db ZX_NUMBER,$8F,$00,$0A,$00,$00
	  db_AS,_2,_5,_6
	  db ZX_NUMBER,$89,$00,$00,$00,$00
Line0040end db $76			  ;ZX_NEWLINE

;  50 IF RAMTOP>ZXMON THEN GOTO 100
	  db 0,0050			  ;LINE NUMBER
	  dw Line0050end-Line0050+1	  ;LINE LENGTH
Line0050  db ZX_IF,_R,_A,_M,_T,_O,_P,_GT,_Z,_X,_M,_O,_N,ZX_THEN,ZX_GOTO,_1,_0,_0
	  db ZX_NUMBER,$87,$48,$00,$00,$00
Line0050end db $76			  ;ZX_NEWLINE

;  60 LET L=USR MOVECODE
	  db 0,0060			  ;LINE NUMBER
	  dw Line0060end-Line0060+1	  ;LINE LENGTH
Line0060  db ZX_LET,_L,_EQ,ZX_USR,_M,_O,_V,_E,_C,_O,_D,_E
Line0060end db $76			  ;ZX_NEWLINE

;  70 PRINT AT 8,0;"TO USE ZXMON at ANY TIME, TYPE"
	  db 0,0070			  ;LINE NUMBER
	  dw Line0070end-Line0070+1	  ;LINE LENGTH
Line0070  db ZX_PRINT,ZX_AT,_8
	  db ZX_NUMBER,$84,$00,$00,$00,$00
	  db _CM,_0
	  db ZX_NUMBER,$00,$00,$00,$00,$00
	  db ZX_SEMI,ZX_QUOTE,ZX_TO,_U,_S,_E,__,_Z,_X,_M,_O,_N,__,ZX_AT,_A,_N,_Y,__,_T,_I,_M,_E,_CM,__,_T,_Y,_P,_E,ZX_QUOTE
Line0070end db $76			  ;ZX_NEWLINE

;  80 PRINT AT 10,0;"LET L= USR 31802"
	  db 0,0080			  ;LINE NUMBER
	  dw Line0080end-Line0080+1	  ;LINE LENGTH
Line0080  db ZX_PRINT,ZX_AT,_1,_0
	  db ZX_NUMBER,$84,$20,$00,$00,$00
	  db _CM,_0
	  db ZX_NUMBER,$00,$00,$00,$00,$00
	  db ZX_SEMI,ZX_QUOTE,ZX_DBL_QT,ZX_LET,_L,_EQ,ZX_USR,_3,_1,_8,_0,_2,ZX_DBL_QT,ZX_QUOTE
Line0080end db $76			  ;ZX_NEWLINE

;  90 PRINT AT 20,0;"-EVEN AFTER NEW-"
	  db 0,0090			  ;LINE NUMBER
	  dw Line0090end-Line0090+1	  ;LINE LENGTH
Line0090  db ZX_PRINT,ZX_AT,_2,_0
	  db ZX_NUMBER,$85,$20,$00,$00,$00
	  db _CM,_0
	  db ZX_NUMBER,$00,$00,$00,$00,$00
	  db ZX_SEMI,ZX_QUOTE,_MI,_E,_V,_E,_N,__,_A,_F,_T,_E,_R,__,_N,_E,_W,_MI,ZX_QUOTE
Line0090end db $76			  ;ZX_NEWLINE

;  99 STOP
	  db 0,0099			  ;LINE NUMBER
	  dw Line0099end-Line0099+1	  ;LINE LENGTH
Line0099  db ZX_STOP
Line0099end db $76			  ;ZX_NEWLINE

; 100 PRINT "MEMORY  MUST  BE  RESERVED FOR"
	  db 0,0100			  ;LINE NUMBER
	  dw Line0100end-Line0100+1	  ;LINE LENGTH
Line0100  db ZX_PRINT,ZX_QUOTE,_M,_E,_M,_O,_R,_Y,__,__,_M,_U,_S,_T,__,__,_B,_E,__,__,_R,_E,_S,_E,_R,_V,_E,_D,__,ZX_FOR,ZX_QUOTE
Line0100end db $76			  ;ZX_NEWLINE

; 110 PRINT "ZXMON. THIS IS DONE BY CHANGING"
	  db 0,0110			  ;LINE NUMBER
	  dw Line0110end-Line0110+1	  ;LINE LENGTH
Line0110  db ZX_PRINT,ZX_QUOTE,_Z,_X,_M,_O,_N,_DT,__,_T,_H,_I,_S,__,_I,_S,__,_D,_O,_N,_E,__,_B,_Y,__,_C,_H,_A,_N,_G,_I,_N,_G,ZX_QUOTE
Line0110end db $76

; 120 PRINT "THE SYSTEM VARIABLE RAMTOP"
	  db 0,0120			  ;LINE NUMBER
	  dw Line0120end-Line0120+1	  ;LINE LENGTH
Line0120  db ZX_PRINT,ZX_QUOTE,_T,_H,_E,__,_S,_Y,_S,_T,_E,_M,__,_V,_A,_R,_I,_A,_B,_L,_E,__,ZX_DBL_QT,_R,_A,_M,_T,_O,_P,ZX_DBL_QT,ZX_QUOTE
Line0120end db $76

; 130 PRINT AT 6,0;"FIRST TYPE POKE 16389,124"
	  db 0,0130			  ;LINE NUMBER
	  dw Line0130end-Line0130+1	  ;LINE LENGTH
Line0130  db ZX_PRINT,ZX_AT,_6
	  db ZX_NUMBER,$83,$40,$00,$00,$00
	  db _CM,_0
	  db ZX_NUMBER,$00,$00,$00,$00,$00
	  db ZX_SEMI,ZX_QUOTE,_F,_I,_R,_S,_T,__,_T,_Y,_P,_E,__,ZX_DBL_QT,ZX_POKE,_1,_6,_3,_8,_9,_CM,_1,_2,_4,__,ZX_DBL_QT,ZX_QUOTE
Line0130end db $76			  ;ZX_NEWLINE

; 140 PRINT "THEN TYPE NEW"
	  db 0,0140			  ;LINE NUMBER
	  dw Line0140end-Line0140+1	  ;LINE LENGTH
Line0140  db ZX_PRINT,ZX_QUOTE,ZX_THEN,__,_T,_Y,_P,_E,__,ZX_DBL_QT,ZX_NEW,ZX_DBL_QT,ZX_QUOTE
Line0140end db $76

; 150 PRINT ,,"THEN LOAD ZXMON AND RUN"
	  db 0,0150			  ;LINE NUMBER
	  dw Line0150end-Line0150+1	  ;LINE LENGTH
Line0150  db ZX_PRINT,ZX_COMMA,ZX_COMMA,ZX_QUOTE,ZX_THEN,ZX_LOAD,_Z,_X,_M,_O,_N,ZX_AND,ZX_RUN,ZX_QUOTE
Line0150end db $76

; 160 PRINT AT 19,0;"THIS SHOULD BE DONE  WHEN  FIRST"
	  db 0,0160			  ;LINE NUMBER
	  dw Line0160end-Line0160+1	  ;LINE LENGTH
Line0160  db ZX_PRINT,ZX_AT,_1,_9
	  db ZX_NUMBER,$85,$18,$00,$00,$00
	  db _CM,_0
	  db ZX_NUMBER,$00,$00,$00,$00,$00
	  db ZX_SEMI,ZX_QUOTE,_T,_H,_I,_S,__,_S,_H,_O,_U,_L,_D,__,_B,_E,__,_D,_O,_N,_E,__,__,_W,_H,_E,_N,__,__,_F,_I,_R,_S,_T,ZX_QUOTE
Line0160end db $76

; 170 PRINT "TURNING  ON THE MACHINE--TO SAVE"
	  db 0,0170			  ;LINE NUMBER
	  dw Line0170end-Line0170+1	  ;LINE LENGTH
Line0170  db ZX_PRINT,ZX_QUOTE,_T,_U,_R,_N,_I,_N,_G,__,__,_O,_N,__,_T,_H,_E,__,_M,_A,_C,_H,_I,_N,_E,_MI,_MI,_T,_O,__,_S,_A,_V,_E,ZX_QUOTE
Line0170end db $76

; 180 PRINT "LOADING ZXMON TWICE."
	  db 0,0180			  ;LINE NUMBER
	  dw Line0180end-Line0180+1	  ;LINE LENGTH
Line0180  db ZX_PRINT,ZX_QUOTE,_L,_O,_A,_D,_I,_N,_G,__,_Z,_X,_M,_O,_N,__,_T,_W,_I,_C,_E,_DT,ZX_QUOTE
Line0180end db $76

; ===========================================================
; BASIC code ends
; ===========================================================

; Display file (ZX81 screen) - 2K-16K standard low res screen (full 32x24 display)
; this file can be pre-populated to have characters appear on-screen after loading
Display:	DEFB $76
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 0
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 1
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 2
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 3
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 4
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 5
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 6
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 7
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 8
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 9
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 10
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 11
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 12
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 13
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 14
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 15
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 16
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 17
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 18
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 19
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 20
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 21
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 22
		DEFB __,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,__,$76 ; Line 23
DisplayEnd:
 assert (DisplayEnd-Display) = 793
;close out the basic program
Variables:
VariablesEnd:	DEFB $80
BasicEnd:			       

END