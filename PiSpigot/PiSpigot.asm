;=============================================================================
;	Spigot algorithm to compute PI
;=============================================================================
;
;	Build:
;		zmac PiSpigot.asm --od PiSpigot --oo cim,lst -c -s -g
;	ZMAC: 8080/Z-80 Cross-Assembler for Windows
;		http://48k.ca/zmac.html
;=============================================================================

;-----------------------------------------------------------------------------
; 	Title
$TITLE	MACRO
	DB	'Pi Spigot - Ver:1.00 - Sep-2019'
	ENDM

;-----------------------------------------------------------------------------
; 	Settings
DEBUG	EQU	0			; Debug mode
NDIGITS	EQU	1000			; Number of digits to compute (4837 max)

;-----------------------------------------------------------------------------

CR	EQU	0DH			; Carriage Return
LF	EQU	0AH			; Line Feed

;-----------------------------------------------------------------------------
;	Macro to break in emulator
$BREAK	MACRO
	IF	DEBUG
	DB	0EDH,0F5H		; Emulator $BREAK opcode
	ENDIF
	ENDM
					;

;-----------------------------------------------------------------------------
;	IPL entry
	ORG	0

BOOT	DI				; No interrupts
	JP	SPIGOT			;

;-----------------------------------------------------------------------------
;	Send one char to the terminal
PUTCH	EX	AF,AF'			; Save char to A'
	LD	A,1			; IOS:SERIAL_TX opcode
	OUT	(1),A			; send it
	EX	AF,AF'			; restore char in A
	OUT	(0),A			; Send it
	RET				; Done

;-----------------------------------------------------------------------------
;	Send a null-terminated string to the terminal
PUTS	LD	A,(HL)			; get char from string
	INC	HL			; bump ptr
	OR	A			; NULL terminator ?
	RET	Z			; Return if yes
	CALL	PUTCH			; Send char to terminal
	JR	PUTS			; Loop again

;-----------------------------------------------------------------------------
;	Display HL in decimal
PUTHL4:	LD	DE,1000			;
	CALL	PUT1HL			; Display thousands
	LD	DE,100			;
	CALL	PUT1HL			; Display hundreds
	LD	DE,10			;
	CALL	PUT1HL			; Display tens
	LD	A,L			;
	JR	PUTHL1			; Display units
PUT1HL	LD	A,0FFH			; Init counter
PUT1HL1	INC	A			; Inc counter
	SBC	HL,DE			; Try subtract
	JR	NC,PUT1HL1		; Loop while OK
	ADD	HL,DE			; Revert subtract
PUTHL1	ADD	A,'0'			; Adjust for ASCII digit
	JR	PUTCH			; Display it

;-----------------------------------------------------------------------------
;	Display A in decimal
PUTA2:	LD	B,10			;
	CALL	PUT1A			; Display tens
PUTA1:	ADD	A,'0'			; Adjust for ASCII digit
	JR	PUTCH			; Display units
PUT1A:	LD	C,0FFH			; Init counter
PUT1A1:	INC	C			; Inc counter
	SUB	A,B			; Try subtract
	JR	NC,PUT1A1		; Loop while OK
	ADD	A,B			; Revert subtract
	LD	B,A			; Save
	LD	A,C			; Get digit
	CALL	PUTA1			; Display it
	LD	A,B			; Restore
	RET				; Done

;-----------------------------------------------------------------------------
;	Read Time
READTIME:
	INC	HL
	INC	HL
	LD	A,84H			; IOS:OPCODE DATETIME
	OUT	(1),A			; Send opcode
	IN	A,(0)			; Read seconds
	LD	(HL),A			; Save
	DEC	HL			;
	IN	A,(0)			; Read minutes
	LD	(HL),A			; Save
	DEC	HL			;
	IN	A,(0)			; Read hours
	LD	(HL),A			; Save
	RET				; Done


;-----------------------------------------------------------------------------
;	Display Time
PUTTIME	LD	A,(HL)			; Get hours
	INC	HL
	CALL	PUTA2			; Display hours
	LD	A,':'			;
	CALL	PUTCH			; Display sep
	LD	A,(HL)			; Get mintues
	INC	HL
	CALL	PUTA2			; Display minutes
	LD	A,':'			;
	CALL	PUTCH			; Display sep
	LD	A,(HL)			; Get seconds
	JP	PUTA2			; Display seconds

;-----------------------------------------------------------------------------
;	Wait for a keypress from the terminal
WAITKI	IN	A,(1)			; Get char from terminal
	CP	0FFH			; No char ?
	JR	Z,WAITKI		; Loop if yes
	RET				; Done


;-----------------------------------------------------------------------------
;	Div HL by DE
DIVHLDE:
;	Scale divisor
	XOR	A			; Init loop counter
	EX	DE,HL			; Divisor to HL
DIVL1	INC	A			; Inc counter
	RET	Z			; Overflow: return
	ADD	HL,HL			; Shift left divisor
	JR	NC,DIVL1		; Loop while not past left bound
	RR	H			; Restore scaled divisor
	RR	L			;
	LD	B,H			; Scaled Divisor to BC
	LD	C,L			;
	EX	DE,HL			; Dividend to HL
	LD	DE,0			; Clear quotient
DIVL2:	EX	DE,HL			; Shift left quotient
	ADD	HL,HL			;
	EX	DE,HL			;
	SBC	HL,BC			; Try to subtract divisor from dividend (C is cleared)
	INC	DE			; Try to Add 1 to quotient
	JR	NC,DIVJ2		; If OK, go
	DEC	DE			; Cancel addition to quotient
	ADD	HL,BC			; Cancel subtraction
DIVJ2:	SRL	B			; Shift right divisor
	RR	C			;
	DEC	A			; Dec loop counter
	JR	NZ,DIVL2		; Continue while counter > 0
	EX	DE,HL			; Quotient to HL, remainder to DE
	RET				; Done

;-----------------------------------------------------------------------------
;	Long Div HL by DE
LDIVHLDE:
;	Scale divisor
	XOR	A			;
	EX	DE,HL			; Divisor to HL, low word
	EXX				;
	EX	DE,HL			; idem, high word
	EXX
LDIVL1	INC	A			; Inc counter
	RET	Z			; Overflow: return

	ADD	HL,HL			; Shift left divisor, low word
	EXX				;
	ADC	HL,HL			; Idem, high word
	EXX				;

	JR	NC,LDIVL1		; Loop while not past left bound

LDIVJ1:	EXX				;
	RR	H			; Restore scaled divisor, high word
	RR	L			;
	LD	B,H			; Scaled divisor to BC, high word
	LD	C,L			;
	EX	DE,HL			; Dividend to HL, high word
	LD	DE,0			; Clear quotient, high word
	EXX				;
	RR	H			; Restore scaled divisor, low word
	RR	L			;
	LD	B,H			; Scaled divisor to BC, low word
	LD	C,L			;
	EX	DE,HL			; Dividend to HL, low word
	LD	DE,0			; Clear quotient, low word

LDIVL2:	EX	DE,HL			; Shift left quotient, high word
	ADD	HL,HL			;
	EX	DE,HL			;
	EXX				;
	EX	DE,HL			; Idem, high word
	ADC	HL,HL			;
	EX	DE,HL			;
	EXX				;

	SBC	HL,BC			; Try to subtract divisor from dividend, low word (C is cleared)
	EXX
	SBC	HL,BC			; Idem, high word
	EXX

	INC	DE			; Try to Add 1 to quotient
	JR	NC,LDIVJ2		; If OK, go
	DEC	DE			; Cancel addition to quotient

	ADD	HL,BC			; Cancel subtraction, low word
	EXX				;
	ADC	HL,BC			; Idem, high word
	EXX				;

LDIVJ2:	EXX				;
	SRL	B			; Shift right divisor, high word
	RR	C			;
	EXX				;
	RR	B			; Idem, low word
	RR	C			;

	DEC	A			; Dec loop counter
	JR	NZ,LDIVL2		; Continue while counter > 0

	EX	DE,HL			; Quotient to HL, remainder to DE, low word
	EXX				;
	EX	DE,HL			; idem, high word
	EXX				;

	RET				; Done

;-----------------------------------------------------------------------------
;	Long MUL HL,DE
LMULHLDE:
	CALL	LLDBCHL			; Move multiplicand HL':HL to BC':BC
	LD	HL,0			; Product, HL':HL := 0
	EXX				;
	LD	HL,0			;
	EXX				;
	LD	A,32			; Loop counter
LMULL1:	EXX				; Multiplicand, BC':BC >>= 1
	SRL	B			;
	RR	C			;
	EXX				;
	RR	B			;
	RR	C			;
	CALL	C,LADDHLDE		; Product += Multiplicator if multiplicand's last bit was 1
	EX	DE,HL			; product, HL':HL <<= 1
	ADD	HL,HL			;
	EX	DE,HL			;
	EXX				;
	EX	DE,HL			;
	ADC	HL,HL			;
	EX	DE,HL			;
	EXX				;
	DEC	A			; Decrement loop counter
	JR	NZ,LMULL1		; Continue while counter > 0
	RET				; Done

;-----------------------------------------------------------------------------
;	Long LD BCx,HLx
;	BC':BC := HL':HL
LLDBCHL:
	LD	B,H			; BC := HL
	LD	C,L			;
	EXX				;
	LD	B,H			; BC' := HL'
	LD	C,L			;
	EXX				;
	RET				; Done

;-----------------------------------------------------------------------------
;	Long ADD HLx,BCx
;	HL':HL += BC':BC
LADDHLBC:
	ADD	HL,BC			; HL += BC
	EXX				;
	ADC	HL,BC			; HL' += BC' + carry
	EXX				;
	RET				; Done

;-----------------------------------------------------------------------------
;	Long ADD HLx,DEx
;	HL':HL += DE':DE
LADDHLDE:
	ADD	HL,DE			; HL += DE
	EXX				;
	ADC	HL,DE			; HL' += DE' + carry
	EXX				;
	RET				; Done

;-----------------------------------------------------------------------------
;	Long ADD HLx,HLx
;	HL':HL <<= 1
LADDHLHL:
	ADD	HL,HL			; HL <<= 1
	EXX				;
	ADC	HL,HL			; HL' <<= 1 with carry
	EXX				;
	RET				; Done


;-----------------------------------------------------------------------------
;	Long MUL HLx,10L
LMULHL10:
	CALL	LLDBCHL			; BC':BC := HL':HL
	CALL	LADDHLHL		; HL':HL <<= 1		=> 2 * HL':HL
	CALL	LADDHLHL		; HL':HL <<= 1		=> 4 * HL':HL
	CALL	LADDHLBC		; HL':HL += BC':BC 	=> 5 * HL':HL
	CALL	LADDHLHL		; HL':HL <<= 1		=> 10 * HL':HL
	RET				; Done


;-----------------------------------------------------------------------------
;	Pi-Spigot OneLoop routine
OneLoop:
;	I := I * 10 DIV 3 + 16
	CALL	LMULHL10		; I *= 10
	CALL	LDIVHLDE		; I /= 3 (3 was loaded in DE':DE before call)
	LD	BC,16			;
	ADD	HL,BC			; I += 16 (6 safety digits)
	LD	(I_),HL			; update I
	EXX				;
	LD	BC,0			; idem, high word
	ADC	HL,BC			;
	LD	(I_+2),HL		;
	EXX				;

;	IF ( I > LEN ) THEN I := LEN;
	EX	DE,HL			; I to DE
	LD	HL,(LEN_)		; LEN to HL
	OR	A			;
	SBC	HL,DE			; Compare I to LEN
	EX	DE,HL			; I to HL
	JR	NC,ILELEN		; if LEN < I then
	LD	HL,(LEN_)		;   I := LEN
ILELEN:	LD	(I_),HL			; update I
	ADD	HL,HL			; I *= 4
	ADD	HL,HL			;
	LD	DE,ARRAY		; ARRAY origin
	ADD	HL,DE			; ARRAY + 2 * I
	PUSH	HL			;
	POP	IX			; to IX, ARRAY pointer

;	RES := 0;
	LD	HL,0			; Clear RES
	LD	(RES_),HL		;
	LD	(RES_+2),HL		;

;	REPEAT
REPT1:
;	X := 10 * A[I] + RES * I;
	LD	HL,(RES_)		; RES to HL
	LD	DE,(I_)			; I to DE
	EXX				;
	LD	HL,(RES_+2)		;
	LD	DE,0			;   low words only
	EXX				;
	CALL	LMULHLDE		; HL':HL := HL * DE == RES * I
	EX	DE,HL			; to DE':DE (low word)
	LD	L,(IX+0)		; HL := ARRAY[I]
	LD	H,(IX+1)		;
	EXX				;
	EX	DE,HL			; to DE':DE (high word)
	LD	L,(IX+2)		;
	LD	H,(IX+3)		;
	EXX				;
	CALL	LMULHL10		; HL':HL *= 10 		( == 10 * ARRAY[I] )
	CALL	LADDHLDE		; HL':HL += DE':DE 	( += RES * I )
	PUSH	HL			; Save HL (no need to save HL')

;	K := 2*I - 1
	LD	HL,(I_)			; I to HL (low word only because I < 8000H)
	ADD	HL,HL			; HL <<= 1		( 2*I )
	DEC	HL			; HL -= 1		( 2*I - 1 )
	EX	DE,HL			; to DE
	POP	HL			; restore HL
	EXX				;
	LD	DE,0			; DE' := 0
	EXX				;

;	RES := X DIV K;
;	A[I] := X MOD K;
	CALL	LDIVHLDE		; HL /= DE 		( X / ( 2*I - 1 ) )
	LD	(RES_),HL		; RES := quotient
	LD	(IX+0),E		; ARRAY[I] := remainder
	LD	(IX+1),D		;
	EXX				;
	LD	(RES_+2),HL		; idem, high word
	LD	(IX+2),E		;
	LD	(IX+3),D		;
	EXX

;	I := I - 1;
	DEC	IX			; Dec ARRAY pointer
	DEC	IX			;
	DEC	IX			;
	DEC	IX			;
	LD	HL,(I_)			; I -= 1
	DEC	HL			;
	LD	(I_),HL			;

;	UNTIL I <= 0;
	LD	A,H			; Check if I != 0
	OR	L			;
	JR	NZ,REPT1		; Continue if yes

	RET				; Done

;-----------------------------------------------------------------------------
;	Pi-Spigot Entry Point
Spigot:
	LD	HL,SPIGOT$		; Welcome message
	CALL	PUTS			; Display it
	LD	HL,TIME_		; Get and save start time
	CALL	READTIME		;
	LD	IY,IY0			; Initialize variables pointer

;	N := NDIGITS;
	LD	HL,NDIGITS		; HL := Number of decimals to compute
	INC	HL			; + unit digit
	LD	(N_),HL			; Save to N

;	LEN = N * 10 DIV 3
	LD	DE,3			; Init DE':DE := Divisor (3)
	EXX				;
	LD	HL,0			; HL' := DE' := 0
	LD	DE,0			;
	EXX				;
	CALL	LMULHL10		; HL := 10 * N
	CALL	LDIVHLDE		; HL /= 3		( HL := 10 * N DIV 3 )
	LD	(LEN_),HL		; save to LEN

;	FOR J:=0 TO LEN DO A[J] := 2;
	INC	HL			; HL := LEN + 1
	LD	B,H			; Loop counter, BC := HL == LEN + 1
	LD	C,L			;
	ADD	HL,HL			; HL := ( LEN + 1 ) * 4
	ADD	HL,HL			;
	LD	DE,ARRAY		; DE := ARRAY origin
	ADD	HL,DE			; DE += ( LEN + 1 ) * 4
	EXX				;
	LD	HL,0			; Save SP to HL'
	ADD	HL,SP			;
	EXX				;
	LD	SP,HL			; SP := HL
	LD	HL,0			;
	LD	DE,2			; 2 to write
SPL01:	PUSH	HL			; Write 0 at SP ; SP -= 4
	PUSH	DE			; Write 2 at SP ; SP -= 4
	DEC	BC			; Dec loop counter
	LD	A,B			;
	OR	C			;
	JR	NZ,SPL01		; Loop while counter > 0
	EXX				;
	LD	SP,HL			; Restore SP from HL'

;	NINES := 0;
	XOR	A			;
	LD	(IY+NINES$),A		; Counter of 9s := 0

;	PREDIGIT := 0;
	LD	(IY+PREDIG$),A		; Predigit := 0 (maybe useless)

;	GROUPS := 10
	LD	(IY+GROUPS$),10		; Groups of 5 digits counter (for NL)

;	GROUP := 6
	LD	(IY+GROUP$),6		; Digits counter

;	DOT := '.'
	LD	(IY+DOT$),'.'		; Dot to display after 1st digit


;	FOR J := 0 TO N DO
	LD	HL,0			; J := 0
	LD	(J_),HL			;
	DEC	HL			; Init decimals counter
	LD	(COUNT_),HL		;

FORJ:
	LD	HL,(J_)			; HL := J

;	QI := OneLoop ( N - J );

;	I := N - J;
	EX	DE,HL			; DE := J
	LD	HL,(N_)			; HL := N
	OR	A			;
	SBC	HL,DE			; HL -= DE	( N - J )
	LD	DE,3			; Load divisor 3 to DE':DE
	EXX				;
	LD	HL,0			; HL' := 0	( N - J < 10000H )
	LD	DE,0			; DE' := 0
	EXX				;

	CALL	OneLoop			; Base conversion fot 1 digit

;	Q := RES DIV 10;
;	A[1] := RES MOD 10;
	LD	HL,(RES_)		; RES == 10 * digit + remainder
	LD	DE,10			;
	CALL	DIVHLDE			; RES /= 10 (in HL with H == 0)
	LD	(ARRAY+4),DE		; Remainder in DE to ARRAY[1]
	LD	DE,0			;
	LD	(ARRAY+6),DE		;

;	IF Q = 9 THEN INC(NINES);
	LD	A,L			; Examine Digit
	CP	9			; If not 9
	JR	NZ,QNOT9		;   go
	INC	(IY+NINES$)		; else count 9s
	JR	ENDIF1			; Done

;	ELSE IF Q = 10 THEN
QNOT9:	CP	10			; If not 10
	JR	NZ,QNOT10		;   go

;	OUTDIG( 1 + PREDIGIT );
;	PREDIGIT := 0;
	LD	A,(IY+PREDIG$)		; Else get digit preceding 9s
	LD	(IY+PREDIG$),0		; Clear stored value
	INC	A			; Increment digit
	CALL	OUTDIG			; Display it

;	WHILE NINES > 0 DO
;	BEGIN
;	  OUT( '0' );
;	  NINES := NINES - 1;
;	END;

	LD	A,(IY+NINES$)		; Check for 9s
	OR	A			;
	JR	Z,NOZEROS		; Go if none
WZEROS:	XOR	A			; Display 0s instead of 9s
	CALL	OUTDIG			;
	DEC	(IY+NINES$)		; Dec 9s counter
	JR	NZ,WZEROS		; Loop until all 0s displayed
NOZEROS:
	JR	ENDIF1			; Done

;	ELSE
QNOT10:
;	IF J > 0 THEN OUTDIG( PREDIGIT );
;	PREDIGIT := Q
	LD	A,(IY+J$)		; Get J
	OR	(IY+J$+1)		; Is it 0 (first loop) ?
	LD	A,(IY+PREDIG$)		; Load digit preceding 9s
	LD	(IY+PREDIG$),L		; Store new digit
	CALL	NZ,OUTDIG		; Display if not first loop

;	WHILE NINES > 0 DO
;	BEGIN
;	  OUT( '9' );
;	  NINES := NINES - 1;
;	END;
	LD	A,(IY+NINES$)		; Check for 9s
	OR	A			;
	JR	Z,NONINES		; Go if none
WNINES:	LD	A,9			; Display 9s
	CALL	OUTDIG			;
	DEC	(IY+NINES$)		; Dec 9s counter
	JR	NZ,WNINES		; Loop until all 9s displayed
NONINES:

ENDIF1:	LD	HL,(J_)			; Get J, main loop counter
	INC	HL			; Increment it
	LD	(J_),HL			; Store new value
	LD	DE,(N_)			; Compare with N
	OR	A			;
	SBC	HL,DE			; J == N ?
	LD	A,H			;
	OR	L			;
	JP	NZ,FORJ			; Loop until yes

	LD	A,(IY+PREDIG$)		; Display last digit preceding 8s
	CALL	OUTDIG			;

;	WHILE NINES > 0 DO
;	BEGIN
;	  OUT( '9' );
;	  NINES := NINES - 1;
;	END;
	LD	A,(IY+NINES$)		; Display following 9s if any
	OR	A			;
	JR	Z,NONINES2		;
WNINES2	LD	A,9			;
	CALL	OUTDIG			;
	DEC	(IY+NINES$)		;
	JR	NZ,WNINES2		;
NONINES2:

	LD	HL,DONE$		;
	CALL	PUTS			; Display "Done. Started:"
	LD	HL,TIME_		;
	PUSH	HL			;
	CALL	PUTTIME			; Display start time
	LD	HL,ENDED$		;
	CALL	PUTS			; Display " - Ended: "
	POP	HL			;
	CALL	READTIME		; Read end time
	CALL	PUTTIME			; Display it
	LD	HL,CRLF			;
	CALL	PUTS			; do CR-LF

	HALT				; All digits displayed => HALT
	JR	$			;

;-----------------------------------------------------------------------------
;	Display each digit of Pi, with grouping and new lines
OUTDIG:	ADD	A,'0'			; Convert to ASCII digit
	CALL	PUTCH			; Display char
	LD	A,(IY+DOT$)		; Get dot if any
	LD	(IY+DOT$),0		; Only once
	OR	A			; Is there a dot ?
	CALL	NZ,PUTCH		; If yes, display it
	LD	HL,(COUNT_)		; Get counter
	INC	HL			; Increment it
	LD	(COUNT_),HL		; Save it
	DEC	(IY+GROUP$)		; Done with group of digits ?
	RET	NZ			; ret if yes
	LD	(IY+GROUP$),5		; reload in-group digits counter
	LD	A,' '			; Display ' ' separator
	CALL	PUTCH			;
	DEC	(IY+GROUPS$)		; Done with line of groups ?
	RET	NZ			; ret if yes
	LD	(IY+GROUPS$),10		; Reload groups counter
	LD	HL,COLSEP$		;
	CALL	PUTS			; Display ': '
	LD	HL,(COUNT_)		; Get loop counter
	CALL	PUTHL4			; Display it
	LD	HL,NL2SPC$		; Display NL + 2 spaces
	CALL	PUTS			;
	RET				; Done

;-----------------------------------------------------------------------------
;	Dialog Messages
SPIGOT$	$TITLE				; Title message
LFCRLF:	DB	LF
CRLF:	DB	CR,LF,0			;

DONE$	DB	CR,LF
	DB	"Done - Started: ",0	; Done message

ENDED$	DB	" - Ended: ",0		;

NL2SPC$	DB	CR,LF,'  ',0		; New line + 2 spaces

COLSEP$	DB	': ',0			; Colon + space

IY0	EQU	$			; Base value for IY

N$	EQU	$-IY0			; N = Number of Digits
N_	DS	2			;
LEN$	EQU	$-IY0			; LEN = array length
LEN_	DS	2			;
I$	EQU	$-IY0			; I sub loop counter
I_	DS	4			;
J$	EQU	$-IY0			; J main loop counter
J_	DS	2			;
RES$	EQU	$-IY0			; RES = quotient
RES_	DS	4			;
NINES$	EQU	$-IY0			; NINES = 9s counter
NINES_	DS	1			;
PREDIG$	EQU	$-IY0			; PREDIG = digit preceding 9s
PREDIG_	DS	1			;
DOT$	EQU	$-IY0			; DOT = dot to display after 1st digit
DOT_	DS	1			;
GROUP$	EQU	$-IY0			; GROUP = Digits in group counter
GROUP_	DS	1			;
GROUPS$	EQU	$-IY0			; GROUPS = Groups in line counter
GROUPS_	DS	1			;
COUNT_	DS	2			; COUNT = decimals counter
TIME_	DS	3			; Hold the time


	DS	1 + low not $		; go to next 256 bytes boundary
ARRAY	EQU	$
ARREND	EQU	4*NDIGITS*10/3+ARRAY	; ensure that enough memory is avalilable

	ASSERT	ARREND < 0FFF0H

	END	BOOT

