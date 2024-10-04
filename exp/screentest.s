;screentest.s Example program from VIC 20 programmer's manual with addl changes

	processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2

;ORIGIN
	org $1001

;BASIC STUB
	dc.w nextstmt
	dc.w 10				;line number
	dc.b $9e, "4109", 0		;SYS 4109, end of statement
nextstmt
	dc.w 0				;end of basic stub

;PROGRAM START
	ldx #%0101110			;Playing with VIC options
	stx $900F			;Screen colour
	ldx #$09
	stx $9000			;Horizontal origin
	ldx #$4B
	stx $9001			;Vertical origin
	ldx #%10010110
	stx $9002			;Video columns
	ldx #$AE
	stx $9003			;Video rows
	ldx #%00011010
	stx $9004			;Raster
	ldx #%00110011
	stx $9008			;Paddle 1
	ldx #%00100011
	stx $9009			;Paddle 2

	lda #$0				;Load A with base address $00 (will change with Y)
	sta $01				;Store low byte of $1E00 (screen RAM) at zero page $01
	sta $fe				;Store low byte of $9600 (colour RAM) at zero page $fe

	lda #$1e			;Load A with high byte of $1E00 at zero page $02
	sta $02				;Store at zero page $02

	lda #$96			;Load A with high byte of $9600 at zero page $ff
	sta $ff

	ldy #$0				;Y is the index

loop
	lda #$66 			;Load A with a liney thing corresponding to chr code 66
	sta ($01),Y			;

	lda #$0a
	sta ($fe),Y

	iny
	bne loop

	rts	

