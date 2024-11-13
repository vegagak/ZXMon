; ZX81 character & token defines v1.0c

; abbreviated defines
__:   EQU  $00	;space
_QT:  EQU  $0B	;" quote
_PD:  EQU  $0C	;� pound symbol
_SD:  EQU  $0D	;$ dollar
_CL:  EQU  $0E	;: colon
_QM:  EQU  $0F	;? question
_OP:  EQU  $10	;( open paren
_CP:  EQU  $11	;) close paren
_GT:  EQU  $12	;> greater than
_LT:  EQU  $13	;< less than
_EQ:  EQU  $14	;= equal
_PL:  EQU  $15	;+ plus
_MI:  EQU  $16	;- minus
_AS:  EQU  $17	;* asterisk
_SL:  EQU  $18	;/ slash
_SC:  EQU  $19	;; semi-colon
_CM:  EQU  $1A	;, comma
_DT:  EQU  $1B	;. dot
_FS:  EQU  $1B	;. full stop/period
_NL:  EQU  $76	;NEWLINE
_0:   EQU  $1C
_1:   EQU  $1D
_2:   EQU  $1E
_3:   EQU  $1F
_4:   EQU  $20
_5:   EQU  $21
_6:   EQU  $22
_7:   EQU  $23
_8:   EQU  $24
_9:   EQU  $25
_A:   EQU  $26
_B:   EQU  $27
_C:   EQU  $28
_D:   EQU  $29
_E:   EQU  $2A
_F:   EQU  $2B
_G:   EQU  $2C
_H:   EQU  $2D
_I:   EQU  $2E
_J:   EQU  $2F
_K:   EQU  $30
_L:   EQU  $31
_M:   EQU  $32
_N:   EQU  $33
_O:   EQU  $34
_P:   EQU  $35
_Q:   EQU  $36
_R:   EQU  $37
_S:   EQU  $38
_T:   EQU  $39
_U:   EQU  $3A
_V:   EQU  $3B
_W:   EQU  $3C
_X:   EQU  $3D
_Y:   EQU  $3E
_Z:   EQU  $3F
_a:   EQU  $A6
_b:   EQU  $A7
_c:   EQU  $A8
_d:   EQU  $A9
_e:   EQU  $AA
_f:   EQU  $AB
_g:   EQU  $AC
_h:   EQU  $AD
_i:   EQU  $AE
_j:   EQU  $AF
_k:   EQU  $B0
_l:   EQU  $B1
_m:   EQU  $B2
_n:   EQU  $B3
_o:   EQU  $B4
_p:   EQU  $B5
_q:   EQU  $B6
_r:   EQU  $B7
_s:   EQU  $B8
_t:   EQU  $B9
_u:   EQU  $BA
_v:   EQU  $BB
_w:   EQU  $BC
_x:   EQU  $BD
_y:   EQU  $BE
_z:   EQU  $BF

;descriptive defines
ZX_SPACE       EQU 00H	;0   ' ' SPACE
ZX_NW	       EQU 01H	;1   '�' NW DOT
ZX_NE	       EQU 02H	;2   '�' NE DOT
ZX_NORTH       EQU 03H	;3   '�' NORTH BAR
ZX_SW	       EQU 04H	;4   '�' SW DOT
ZX_WEST        EQU 05H	;5   '�' WEST BAR
ZX_NE_CHK      EQU 06H	;6   '�' NE CHECKER
ZX_INV_SE      EQU 07H	;7   '�' INVERSE SE DOT
ZX_GRAY        EQU 08H	;8   '�' GRAY SQUARE
ZX_GRAY_S      EQU 09H	;9   '�' GRAY SOUTH BAR
ZX_GRAY_N      EQU 0AH	;10  '�' GRAY NORTH BAR
ZX_QUOTE       EQU 0BH	;11  '"' QUOTATION MARK
ZX_POUND       EQU 0CH	;12  '�' BRITISH POUND
ZX_DOLLAR      EQU 0DH	;13  '$' DOLLAR SIGN
ZX_COLON       EQU 0EH	;14  ':' COLON SIGN
ZX_QMARK       EQU 0FH	;15  '?' QUESTION MARK
ZX_LPAR        EQU 10H	;16  '(' LEFT PAREN
ZX_RPAR        EQU 11H	;17  ')' RIGHT PAREN
ZX_GT	       EQU 12H	;18  '>' GREATER THAN
ZX_LT	       EQU 13H	;19  '<' LESS THAN
ZX_EQUAL       EQU 14H	;20  '=' EQUAL SIGN
ZX_PLUS        EQU 15H	;21  '+' PLUS SIGN
ZX_MINUS       EQU 16H	;22  '-' MINUS SIGN
ZX_HYPHEN      EQU 16H	;    '-' HIPHEN
ZX_ASTER       EQU 17H	;23  '*' ASTERISK
ZX_SLASH       EQU 18H	;24  '/' SLASH/STROKE/SOLIDUS
ZX_SEMI        EQU 19H	;25  ';' SEMICOLON
ZX_COMMA       EQU 1AH	;26  ',' COMMA
ZX_PERIOD      EQU 1BH	;27  '.' PERIOD/FULL STOP
ZX_0	       EQU 1CH	;28  '0'
ZX_1	       EQU 1DH	;29  '1'
ZX_2	       EQU 1EH	;30  '2'
ZX_3	       EQU 1FH	;31  '3'
ZX_4	       EQU 20H	;32  '4'
ZX_5	       EQU 21H	;33  '5'
ZX_6	       EQU 22H	;34  '6'
ZX_7	       EQU 23H	;35  '7'
ZX_8	       EQU 24H	;36  '8'
ZX_9	       EQU 25H	;37  '9'
ZX_A	       EQU 26H	;38  'A'
ZX_B	       EQU 27H	;39  'B'
ZX_C	       EQU 28H	;40  'C'
ZX_D	       EQU 29H	;41  'D'
ZX_E	       EQU 2AH	;42  'E'
ZX_F	       EQU 2BH	;43  'F'
ZX_G	       EQU 2CH	;44  'G'
ZX_H	       EQU 2DH	;45  'H'
ZX_I	       EQU 2EH	;56  'I'
ZX_J	       EQU 2FH	;47  'J'
ZX_K	       EQU 30H	;48  'K'
ZX_L	       EQU 31H	;49  'L'
ZX_M	       EQU 32H	;50  'M'
ZX_N	       EQU 33H	;51  'N'
ZX_O	       EQU 34H	;52  'O'
ZX_P	       EQU 35H	;53  'P'
ZX_Q	       EQU 36H	;54  'Q'
ZX_R	       EQU 37H	;55  'R'
ZX_S	       EQU 38H	;56  'S'
ZX_T	       EQU 39H	;57  'T'
ZX_U	       EQU 3AH	;58  'U'
ZX_V	       EQU 3BH	;59  'V'
ZX_W	       EQU 3CH	;60  'W'
ZX_X	       EQU 3DH	;61  'X'
ZX_Y	       EQU 3EH	;62  'Y'
ZX_Z	       EQU 3FH	;63  'Z'
; 40-42 LOW TOKENS
ZX_RND	       EQU 40H	;64  'RND'
ZX_INKEY       EQU 41H	;65  'INKEY$'
ZX_PI	       EQU 42H	;66  'PI'
; 43-6F UNUSED ZX81
; 70-79 CONTROL CODES
ZX_U_ARROW     EQU 70H	;112 CURSOR UP
ZX_D_ARROW     EQU 71H	;113 CURSOR DOWN
ZX_L_ARROW     EQU 72H	;114 CURSOR LEFT
ZX_R_ARROW     EQU 73H	;115 CURSOR RIGHT
ZX_GRAPHIC     EQU 74H	;116 GRAPHICS
ZX_SHIFT_1     EQU 75H	;117 'EDIT'
ZX_EDIT        EQU 75H	;117 'EDIT'
ZX_NEWLINE     EQU 76H	;118 NEWLINE/ENTER KEY
L83_NEWLINE    EQU 76H	;118 NEWLINE/ENTER KEY
ZX_EOL	       EQU 76H	;118 NEWLINE/END OF LINE
ZX_SHIFT_0     EQU 77H	;119 'RUBOUT'
ZX_RUBOUT      EQU 77H	;119 'RUBOUT'
ZX_DELETE      EQU 77H	;119 DELETE/RUBOUT
ZX_KL	       EQU 78H	;120 K/L MODE
ZX_FUNC        EQU 79H	;121 FUNCTION
;7A-7D UNUSED ZX81
ZX_NUMBER      EQU 7EH	;126 NUMBER CONTAINER MARKER
ZX_CURSOR      EQU 7FH	;127 CURSOR
;80-87 GRAPHICS
ZX_INV_SP      EQU 80H	;128 '�' INVERSE SPACE
ZX_INV_NW      EQU 81H	;129 '�' INVERSE NW DOT
ZX_INV_NE      EQU 82H	;130 '�' INVERSE NE DOT
ZX_SOUTH       EQU 83H	;131 '�' SOUTH BAR
ZX_INV_SW      EQU 84H	;132 '�' INVERSE SW DOT
ZX_EAST        EQU 85H	;133 '�' EAST BAR
ZX_SE_CHK      EQU 86H	;134 '�' SE CHECKER
ZX_SE	       EQU 87H	;135 '�' SE DOT
ZX_INV_GRY     EQU 88H	;136 '�' INVERSE GRAY SQUARE
ZX_INV_GRAY_S  EQU 89H	;137 '�' INVERSE GRAY SOUTH BAR
ZX_INV_GRAY_N  EQU 8AH	;138 '�' INVERSE GRAY NORTH BAR
ZX_INV_QUOTE   EQU 8BH	;139 '�' INVERSE QUOTATION MARK
ZX_INV_POUND   EQU 8CH	;140 '�' INVERSE POUND SIGN
ZX_INV_DOLLAR  EQU 8DH	;141 '�' INVERSE DOLLAR SIGN
ZX_INV_COLON   EQU 8EH	;142 '�' INVERSE COLON
ZX_INV_QMARK   EQU 8FH	;143 '�' INVERSE QUESTION MARK
ZX_INV_LPAR    EQU 90H	;144 '�' INVERSE LEFT PARENTHESIS
ZX_INV_RPAR    EQU 91H	;145 '�' INVERSE RIGHT PARENTHESIS
ZX_INV_GT      EQU 92H	;146 '�' INVERSE GREATER THAN
ZX_INV_LT      EQU 93H	;147 '�' INVERSE LESS THAN
ZX_INV_EQUAL   EQU 94H	;148 '�' INVERSE EQUAL SIGN
ZX_INV_SHIFT_L EQU 94H	;    '�' INVERSE EQUAL SIGN
ZX_INV_PLUS    EQU 95H	;149 '�' INVERSE PLUS SIGN
ZX_INV_MINUS   EQU 96H	;150 '�' INVERSE MINUS
ZX_INV_SHIFT_J EQU 96H	;    '�' INVERSE HIPHEN
ZX_INV_HYPHEN  EQU 96H	;    '�' INVERSE NEG. SIGN
ZX_INV_ASTER   EQU 97H	;151 '�' INVERSE ASTERISK
ZX_INV_SLASH   EQU 98H	;152 '�' INVERSE SLASH
ZX_INV_SHIFT_V EQU 98H	;    '�' INVERSE SHIFT V (SLASH)
ZX_INV_SEMI    EQU 99H	;153 '�' INVERSE SEMICOLON
ZX_INV_COMMA   EQU 9AH	;153 '�' INVERSE COMMA
ZX_INV_PERIOD  EQU 9BH	;155 '�' INVERSE PERIOD
ZX_INV_0       EQU 9CH	;156 '�' INVERSE 0
ZX_INV_1       EQU 9DH	;157 '�' INVERSE 1
ZX_INV_2       EQU 9EH	;158 '�' INVERSE 2
ZX_INV_3       EQU 9FH	;159 '�' INVERSE 3
ZX_INV_4       EQU 0A0H ;160 '�' INVERSE 4
ZX_INV_5       EQU 0A1H ;161 '�' INVERSE 5
ZX_INV_6       EQU 0A2H ;162 '�' INVERSE 6
ZX_INV_7       EQU 0A3H ;163 '�' INVERSE 7
ZX_INV_8       EQU 0A4H ;164 '�' INVERSE 8
ZX_INV_9       EQU 0A5H ;165 '�' INVERSE 9
ZX_INV_A       EQU 0A6H ;166 '�' INVERSE A
ZX_INV_B       EQU 0A7H ;167 '�' INVERSE B
ZX_INV_C       EQU 0A8H ;168 '�' INVERSE C
ZX_INV_D       EQU 0A9H ;169 '�' INVERSE D
ZX_INV_E       EQU 0AAH ;170 '�' INVERSE E
ZX_INV_F       EQU 0ABH ;17A '�' INVERSE F
ZX_INV_G       EQU 0ACH ;172 '�' INVERSE G
ZX_INV_H       EQU 0ADH ;173 '�' INVERSE H
ZX_INV_I       EQU 0AEH ;174 '�' INVERSE I
ZX_INV_J       EQU 0AFH ;175 '�' INVERSE J
ZX_INV_K       EQU 0B0H ;176 '�' INVERSE K
ZX_INV_L       EQU 0B1H ;177 '�' INVERSE L
ZX_INV_M       EQU 0B2H ;178 '�' INVERSE M
ZX_INV_N       EQU 0B3H ;179 '�' INVERSE N
ZX_INV_O       EQU 0B4H ;180 '�' INVERSE O
ZX_INV_P       EQU 0B5H ;181 '�' INVERSE P
ZX_INV_Q       EQU 0B6H ;182 '�' INVERSE Q
ZX_INV_R       EQU 0B7H ;183 '�' INVERSE R
ZX_INV_S       EQU 0B8H ;184 '�' INVERSE S
ZX_INV_T       EQU 0B9H ;185 '�' INVERSE T
ZX_INV_U       EQU 0BAH ;186 '�' INVERSE U
ZX_INV_V       EQU 0BBH ;187 '�' INVERSE V
ZX_INV_W       EQU 0BCH ;188 '�' INVERSE W
ZX_INV_X       EQU 0BDH ;189 '�' INVERSE X
ZX_INV_Y       EQU 0BEH ;190 '�' INVERSE Y
ZX_INV_Z       EQU 0BFH ;191 '�' INVERSE Z
; C0-FF HIGH TOKENS
ZX_DBL_QT      EQU 0C0H ;192 '""'   DOUBLE QUOTE MARKS
ZX_AT	       EQU 0C1H ;193 ' AT '
ZX_TAB	       EQU 0C2H ;194 'TAB '
ZX_C3	       EQU 0C3H ;195	    ZX81:undefined]
ZX_CODE        EQU 0C4H ;196 'CODE '
ZX_VAL	       EQU 0C5H ;197 'VAL '
ZX_LEN	       EQU 0C6H ;198 'LEN '
ZX_SIN	       EQU 0C7H ;199 'SIN '
ZX_COS	       EQU 0C8H ;200 'COS '
ZX_TAN	       EQU 0C9H ;201 'TAN '
ZX_ASN	       EQU 0CAH ;202 'ASN '	ARCSIN
ZX_ACS	       EQU 0CBH ;203 'ACS '	ARCCOS
ZX_ATN	       EQU 0CCH ;204 'ATN '	ARCTAN
ZX_LN	       EQU 0CDH ;205 'LN'	Natural Logarithm
ZX_EXP	       EQU 0CEH ;206 'EXP '	Exponential function
ZX_INT	       EQU 0CFH ;207 'INT '	Integer function
ZX_SQR	       EQU 0D0H ;208 'SQR '	Square Root function
ZX_SGN	       EQU 0D1H ;209 'SGN '	Sign function/Signum
ZX_ABS	       EQU 0D2H ;210 'ABS '	Absolute Value/Modulus
ZX_PEEK        EQU 0D3H ;211 'PEEK '
ZX_USR	       EQU 0D4H ;212 'USR '
ZX_STR	       EQU 0D5H ;213 'STR$ '
ZX_CHR	       EQU 0D6H ;214 'CHR$ '
ZX_NOT	       EQU 0D7H ;215 'NOT '
ZX_POWER       EQU 0D8H ;216 '**'	EXPONENTIATION/POWER OF
ZX_OR	       EQU 0D9H ;217 ' OR '
ZX_AND	       EQU 0DAH ;218 ' AND '
ZX_SHIFT_R     EQU 0DBH ;219 '<='	LESS THAN OR EQUAL
ZX_LTE	       EQU 0DBH ;219 '<='	LESS THAN OR EQUAL
ZX_GTE	       EQU 0DCH ;220 '>='	GREATER THAN OR EQUAL
ZX_NOTEQU      EQU 0DDH ;221 '<>'	NOT EQUAL
ZX_THEN        EQU 0DEH ;222 ' THEN '
ZX_TO	       EQU 0DFH ;223 ' TO '
ZX_STEP        EQU 0E0H ;224 ' STEP '
ZX_SHIFT_E     EQU 0E0H ;224 ' STEP '
ZX_LPRINT      EQU 0E1H ;225 ' LPRINT '
ZX_SHIFT_S     EQU 0E1H ;    ' LPRINT '
ZX_LLIST       EQU 0E2H ;226 ' LLIST '
ZX_STOP        EQU 0E3H ;227 ' STOP '
ZX_SLOW        EQU 0E4H ;228 ' SLOW '
ZX_SHIFT_D     EQU 0E4H ;    ' SLOW '
ZX_FAST        EQU 0E5H ;229 ' FAST '
ZX_NEW	       EQU 0E6H ;230 ' NEW '
ZX_SCROLL      EQU 0E7H ;231 ' SCROLL '
ZX_CONT        EQU 0E8H ;232 ' CONT '
ZX_DIM	       EQU 0E9H ;233 ' DIM '
ZX_REM	       EQU 0EAH ;234 ' REM '
ZX_FOR	       EQU 0EBH ;235 ' FOR '
ZX_GOTO        EQU 0ECH ;236 ' GOTO '
ZX_GOSUB       EQU 0EDH ;237 ' GOSUB '
ZX_INPUT       EQU 0EEH ;238 ' INPUT '
ZX_LOAD        EQU 0EFH ;239 ' LOAD '
ZX_LIST        EQU 0F0H ;240 ' LIST '
ZX_LET	       EQU 0F1H ;241 ' LET '
ZX_PAUSE       EQU 0F2H ;242 ' PAUSE '
ZX_NEXT        EQU 0F3H ;243 ' NEXT '
ZX_POKE        EQU 0F4H ;244 ' POKE '
ZX_PRINT       EQU 0F5H ;245 ' PRINT '
ZX_PLOT        EQU 0F6H ;246 ' PLOT '
ZX_RUN	       EQU 0F7H ;247 ' RUN '
ZX_SAVE        EQU 0F8H ;248 ' SAVE '
ZX_RAND        EQU 0F9H ;249 ' RAND '
ZX_IF	       EQU 0FAH ;250 ' IF '
ZX_CLS	       EQU 0FBH ;251 ' CLS '
ZX_UNPLOT      EQU 0FCH ;252 ' UNPLOT'
ZX_CLEAR       EQU 0FDH ;253 ' CLEAR '
ZX_RETURN      EQU 0FEH ;254 ' RETURN '
ZX_COPY        EQU 0FFH ;255 ' COPY '