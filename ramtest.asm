!cpu w65c02
!src "../cx16stuff/cx16.inc"
!src "../cx16stuff/vera0.9.inc"
+SYS_LINE

MEMTOP		= $FF99

BANKRAM_START	= $A000
BANKRAM_END	= $BFFF

!macro PRINT_CHR {
	sta	VERA_DATA0
}

main:!byte $db
	sec
	jsr	MEMTOP

	lda	#1
	sta	VIA1PA

	lda	#>BANKRAM_START
	ldx	#>BANKRAM_END
	jsr	Fast_RAM_test
	rts

Print_hex:
	tay
	lsr
	lsr
	lsr
	lsr
	tax
	lda	Hex_tbl,x
	+PRINT_CHR
	tya
	and	#$0F
	tax
	lda	Hex_tbl,x
	+PRINT_CHR
	rts

;****************************************************************************
;* Function to test an area of RAM (usually banked RAM) It tests at minium
;* an entire page - WARNING, memory will be overwritten.
;****************************************************************************
;* INPUTS:	.A = Page to start on (usually $A0 for banked RAM)
;*		.X = Last page to test (usually $BF for banked RAM)
;****************************************************************************
;* OUTPUTS:	.A(high-byte) & .Y(low-byte) =
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
@out:	lda	@ptr_h		; low order adds to display
	rts			; ..and exit to KIM

Hex_tbl:	!pet	"0123456789ABCDEF"
