!cpu w65c02
!src "../cx16stuff/cx16.inc"
!src "../cx16stuff/vera0.9.inc"
+SYS_LINE
	jmp	main

BANKRAM_START	= $A000
BANKRAM_END	= $BFFF

!macro PRINT {
	jsr	CHROUT
}

!macro PRINT_CHR .chr {
	lda	#.chr
	+PRINT
}

Init_str:	!pet	"  welcome to banked ram tester",13
 		!pet	"     for commander x16",13,13
		!pet	"by jimmy dansbo (jimmy@dansbo.dk)",13
		!pet	" https://github.com/jimmydansbo/",0
Num_banks_str:	!pet	"number of ram banks: $",0
Cur_bank_str	!pet	"currently testing bank# $  ",0
Err_str1:	!pet	$1C,"!!! error found at bank# $",0
Err_str2:	!pet	" address $",0
No_err_str:	!pet	13,13,"all tests completed, no errors found",0

Num_banks	= TMP7

main:
	+PRINT_CHR 13
	lda	#<Init_str
	ldy	#>Init_str
	jsr	Print_str
	+PRINT_CHR 13
	jsr	CHROUT
	lda	#<Num_banks_str
	ldy	#>Num_banks_str
	jsr	Print_str

	sec
	jsr	MEMTOP
	sta	Num_banks
	bne	+
	tax
	+PRINT_CHR "1"
	txa
+	jsr	Print_hex
	+PRINT_CHR 13
	jsr	CHROUT
	lda	#<Cur_bank_str
	ldy	#>Cur_bank_str
	jsr	Print_str

	stz	VIA1PA
@bank_loop:
	+PRINT_CHR $9D
	jsr	CHROUT

	inc	VIA1PA
	lda	VIA1PA
	cmp	Num_banks
	beq	@end
	jsr	Print_hex

	lda	#>BANKRAM_START
	ldx	#>BANKRAM_END
	jsr	Fast_RAM_test

	cpx	#$C0
	bne	@err
	bra	@bank_loop
@err:
	+PRINT_CHR 13
	jsr	CHROUT
	sty	TMP2
	stx	TMP3
	lda	#<Err_str1
	ldy	#>Err_str1
	jsr	Print_str
	lda	VIA1PA
	jsr	Print_hex
	lda	#<Err_str2
	ldy	#>Err_str2
	jsr	Print_str
	lda	TMP3
	jsr	Print_hex
	lda	TMP2
	jsr	Print_hex
	+PRINT_CHR $05
	+PRINT_CHR 13
	rts
@end:
	lda	#<No_err_str
	ldy	#>No_err_str
	jsr	Print_str
	+PRINT_CHR 13
	rts

Print_str:
@ptr	= TMP0
	sta	@ptr
	sty	@ptr+1
	ldy	#0
@loop:
	lda	(@ptr),y
	beq	@end
	+PRINT
	iny
	bra	@loop
@end:	rts

Print_hex:
	tay
	lsr
	lsr
	lsr
	lsr
	tax
	lda	Hex_tbl,x
	+PRINT
	tya
	and	#$0F
	tax
	lda	Hex_tbl,x
	+PRINT
	rts

;****************************************************************************
;* Function to test an area of RAM (usually banked RAM) It tests at minium
;* an entire page - WARNING, memory will be overwritten.
;****************************************************************************
;* INPUTS:	.A = Page to start on (usually $A0 for banked RAM)
;*		.X = Last page to test (usually $BF for banked RAM)
;****************************************************************************
;* OUTPUTS:	.X(high-byte) & .Y(low-byte) =
;*		address of fault or highest location tested + 1 for no fault
;****************************************************************************
;* USES:	.A, .X & .Y registers
;*		zero page, TMP0-TMP6 = $0022-$0028
;****************************************************************************
;* NOTE:	Information about function can be found here:
;*		https://cx16.dk/fastramtest/
;****************************************************************************
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

Hex_tbl:	!pet	"0123456789abcdef"
