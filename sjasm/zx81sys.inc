;system variables
;Origin of a ZX81 file is always 16393

NOAUTORUN EQU 0
HZ_50	  EQU 55
HZ_60	  EQU 31
SLOW_MODE EQU $40
FAST_MODE EQU $00

 ORG	  $4009
; ZX81 MEMORY MAP(bytes 4000 - 4008 not saved to tape)
ERR_NR	  EQU $4000; 16384
FLAGS	  EQU $4001; 16385
ERR_SP	  EQU $4002; 16386
;RAMTOP    EQU $4004; 16388 ;BASIC Line 40 uses this name
MODE	  EQU $4006; 16390
PPC	  EQU $4007; 16391

VERSN	 db $00 	   ;16393 ZX81 BASIC program
E_PPC	 dw $0079	   ;16394 
D_FILE	 dw Display	   ;16396
DF_CC	 dw Display+1	   ;16398
VARS	 dw Variables	   ;16400
DEST	 dw $4A23	   ;16402 
E_LINE	 dw BasicEnd	   ;16404
CH_ADD	 dw $49F1	   ;16406 
X_PTR	 dw $0000	   ;16408 
STKBOT	 dw BasicEnd+9	   ;16410
STKEND	 dw BasicEnd+9	   ;16412
BERG	 db $00 	   ;16414 
MEM	 dw MEMBOT	   ;16415 
UNUSED1  db $31 	   ;16417 
DF_SZ	 db $02 	   ;16418 
S_TOP	 dw $0010	   ;16419 
LAST_K	 dw $FFFF	   ;16421 
DEBOUNCE db $FF 	   ;16423 
MARGIN	 db HZ_50	   ;16424
NXTLIN	 dw Display	   ;16425
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
	 assert(MEMBOT-PRBUFF)=33
MEMBOT	 db $00,$00,$00,$00,$00,$00,$85,$00
	 db $00,$00,$84,$A0,$00,$00,$00,$80
	 db $80,$80,$80,$85,$92,$98,$98,$91
	 db $90
MEM_5	 db $00,$00,$00,$00,$00
UNUSED2  dw $494A	   ;16507

; End of system variables

