!cpu w65c02
!src "../cx16stuff/cx16.inc"
!src "../cx16stuff/vera0.9.inc"
+SYS_LINE
	jmp	main		; Jump over variables to the main program

BANKRAM_END	= $BFFF

Init_str:	!pet	"  welcome to banked ram tester",13
 		!pet	"     for commander x16",13,13
		!pet	"by jimmy dansbo (jimmy@dansbo.dk)",13
		!pet	"    http://jnz.dk/rtst/",0
Num_banks_str:	!pet	"number of ram banks: $",0
Cur_bank_str	!pet	"currently testing bank# $  ",0
Err_str1:	!pet	PET_RED,"!!! error found at bank# $",0
Err_str2:	!pet	" address $",0
No_err_str:	!pet	13,13,"all tests completed, no errors found",0
Hex_tbl:	!pet	"0123456789abcdef"

Num_banks	= TMP7

; This macro just outputs the character stored in A register.
; It exists to make it easy to alter the code to write directly to VERA
; instead of using a KERNAL call.
!macro PRINT {
	jsr	CHROUT
}

; Print a single immediate character to screen.
!macro PRINT_CHR .chr {
	lda	#.chr
	+PRINT
}

; ****************************************************************************
; Main program start
; ****************************************************************************
main:
	; Print the welcome message
	+PRINT_CHR 13
	lda	#<Init_str
	ldy	#>Init_str
	jsr	Print_str
	; Print the number of banks detected
	+PRINT_CHR 13
	jsr	CHROUT
	lda	#<Num_banks_str
	ldy	#>Num_banks_str
	jsr	Print_str
	sec
	jsr	MEMTOP		; Detect number of banks (by setting Carry bit)
	sta	Num_banks
	bne	+		; If number of banks = 0, then memory is fully
	tax			; populated and number of banks is 256 = $100
	+PRINT_CHR "1"		; .. so write 1 before the rest of the number
	txa
+	jsr	Print_hex

	; Print information about bank being tested
	+PRINT_CHR 13
	jsr	CHROUT
	lda	#<Cur_bank_str
	ldy	#>Cur_bank_str
	jsr	Print_str

	stz	RAM_BANK
@bank_loop:
	+PRINT_CHR $9D		; left-arrow twice to overwrite bank number
	jsr	CHROUT

	inc	RAM_BANK	; Go to next RAM bank
	lda	RAM_BANK	; If it is = number of banks, we are done
	cmp	Num_banks
	beq	@end		; So jump to end.

	jsr	Print_hex	; Print the current bank number

	lda	#>RAM_BANK_START; Test the memory in bank $A0 to $BF
	ldx	#>BANKRAM_END	; .A = Start page, .X = End page
	jsr	Fast_RAM_test

	cpx	#$C0		; If Fast_RAM_Test did not get to page $C0
	bne	@err		; it has encountered an error.
	bra	@bank_loop	; Otherwise loop back and test next bank.
@err:
	+PRINT_CHR 13		; 2x New line
	jsr	CHROUT
	sty	TMP2		; Save faulty address for later use.
	stx	TMP3
	; Print Error string
	lda	#<Err_str1
	ldy	#>Err_str1
	jsr	Print_str
	; Print the failing RAM bank number
	lda	RAM_BANK
	jsr	Print_hex
	; Print the rest of the Error string
	lda	#<Err_str2
	ldy	#>Err_str2
	jsr	Print_str
	; Load and print faulty address
	lda	TMP3
	jsr	Print_hex
	lda	TMP2
	jsr	Print_hex
	+PRINT_CHR PET_WHITE	; Error string is with red chars, return to white
	+PRINT_CHR 13
	rts			; End the program
@end:
	; Print All OK string
	lda	#<No_err_str
	ldy	#>No_err_str
	jsr	Print_str
	+PRINT_CHR 13
	rts

; ****************************************************************************
; Print a 0-terminated string
; ****************************************************************************
; INPUTS:	.A & .Y = pointer to start of string. .A=low-byte, .Y=high-byte
; USES:		.A, .Y & TMP0-TMP1
; ****************************************************************************
Print_str:
@ptr	= TMP0
	sta	@ptr		; Store the pointer to string in zero-page
	sty	@ptr+1
	ldy	#0		; Initialize .Y as index
@loop:
	lda	(@ptr),y	; Load character from string
	beq	@end		; If it is 0, we are done
	+PRINT			; Output the caracter
	iny			; Increment .Y to get next character
	bra	@loop		; Loop back to get next character
@end:	rts

; ****************************************************************************
; Print the hexadecimal value of a byte to screen
; ****************************************************************************
; INPUTS:	.A = the byte to print
; USES:		.A, .X & .Y
; ****************************************************************************
Print_hex:
	tay			; Save byte in .Y for later use
	lsr			; Move high-nibble to low-nibble
	lsr
	lsr
	lsr
	tax			; Use Low nibble as index into Hex_tbl
	lda	Hex_tbl,x	; Load character to print
	+PRINT			; Print the character
	tya			; Restore original byte from .Y
	and	#$0F		; Zero out high-nibble
	tax			; Use low nibble as index into Hex_tbl
	lda	Hex_tbl,x	; Load character to print
	+PRINT			; Print the character
	rts

; ****************************************************************************
; Function to test an area of RAM (usually banked RAM) It tests at minium
; an entire page - WARNING, memory will be overwritten.
; ****************************************************************************
; INPUTS:	.A = Page to start on (usually $A0 for banked RAM)
;		.X = Last page to test (usually $BF for banked RAM)
; ****************************************************************************
; OUTPUTS:	.X(high-byte) & .Y(low-byte) =
;		address of fault or highest location tested + 1 for no fault
; ****************************************************************************
; USES:		.A, .X & .Y registers
;		zero page, TMP0-TMP6 = $0022-$0028
; ****************************************************************************
; NOTE:		Information about function can be found here:
;		https://cx16.dk/fastramtest/
; ****************************************************************************
Fast_RAM_test:
@ptr_l		= TMP0
@ptr_h		= TMP1
@start_page	= TMP2
@end_page	= TMP3
@flag		= TMP4
@flip		= TMP5
@mod		= TMP6
	sta	@start_page
	stx	@end_page
	lda	#0		; zero pointers..
	tay			; into low-order
	sta	@ptr_l		; addresses
@big_loop:
	sta	@flag		; =00 first time, = FF second
	ldx	#2
	stx	@mod		; 3 tests in each major loop
@pass:	lda	@start_page	; set pointer to..
	sta	@ptr_h		; .. start of test area
	ldx	@end_page
	lda	@flag
	eor	#$FF		; reverse FLAG
	sta	@flip		; =ff first time, =00 second
@clear:	sta	(@ptr_l),y	; write above value..
	iny			; .. into all locations
	bne	@clear
	inc	@ptr_h
	cpx	@ptr_h
	bcs	@clear
; FLIP in all locations: now change 1 in 3
	ldx	@mod
	lda	@start_page	; set pointer..
	sta	@ptr_h		; .. back to start
@fill:	lda	@flag		; change value
@top:	dex
	bpl	@skip		; skip 2 out of 3
	ldx	#2		; restore 3-counter
	sta	(@ptr_l),y	; change 1 out of 3
@skip:	iny
	bne	@top
	inc	@ptr_h		; new page
	lda	@end_page
	cmp	@ptr_h		; end of test area?
	bcs	@fill		; no, keep going
; memory set up - now test it
	lda	@start_page	; set pointer..
	sta	@ptr_h		; .. back to start
	ldx	@mod		; sinchronize 3-counter
@pop:	lda	@flip		; test for FLIP value..
	dex			; ..2 out of 3 times
	bpl	@slip		;  - else -
	ldx	#2		; reset 3-counter
	lda	@flag		; & test for FLAG value
@slip:	cmp	(@ptr_l),y	; make the test
	bne	@out		; branch if failed
	iny
	bne	@pop
	inc	@ptr_h
	lda	@end_page
	cmp	@ptr_h
	bcs	@pop
; above test OK - change & repeat
	dec	@mod		; change 1 in 3 position
	bpl	@pass		; .. & do next third
	lda	@flag		; invert flag..
	eor	#$FF		; .. for part 2
	bmi	@big_loop
@out:	ldx	@ptr_h		; low order adds to display
	rts			; ..and exit to KIM
