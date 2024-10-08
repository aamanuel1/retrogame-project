;basicmov.s Shows basic movement on a blank board.

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

	
init:					;Initialize video settings on VIC chip
	ldy #16				;Iterator
init_loop:
	dey
	lda videosettings,Y
	sta $9000,Y
	tya
	bne init_loop

clearscreen:
	lda #$00			;Load low byte of address
	sta $01
	lda #$1e			;Load high byte of address
	sta $02
	ldy #$0				;Counter = 0
clearscreen_loop:
	lda #$20			;Load with space (32 dec)
	sta ($01),Y
	iny				;Loop until we loop back to 0
	bne clearscreen_loop

main:	
	lda #$00			;load low-byte base address for screen and colour memory
	sta $01
	sta $fe
	lda #$1e			;Load high-byte base address for screen memory
	sta $02
	lda #$96			;Load high-byte base address for colour memory
	sta $ff
	ldy #$0				;Set increment to 0
main_loop:
	dey				;Go back one
	lda #32				;Load with space to clear previous screen
	sta ($01),Y
	iny				;Go forward one to replace blank with non-blank
	lda #2
	sta ($fe),Y
	lda #81
	sta ($01),Y
	iny
	sty $04
	jsr delay
	ldy $04
	bne main_loop
	jmp clearscreen

delay:
	ldx #$ff	
delay_loop:
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	dex
	bne delay_loop
	rts

videosettings:
	dc.b %00001100			;9000 $05 Interlace off, screen origin horiz 12  (def 5, lower value left
	dc.b %00011110			;9001 $19 Screen origin vertical 5, lower value up by 2pix to 1
	dc.b %10010110			;9002 $96 bit 7 Screen mem location, rest columns (current default 22 max 27)
	dc.b %10101110			;9003 $AE bit 7 is screen raster, bit 6-1  rows (default 23 max 23)
					;bit 0 is character size 0 8x8 chars, 1 8x16 chars
	dc.b %01111010			;9004 $7A 122 TV raster beam line
	dc.b %11110000			;9005 $F0 bits 4-7 video address, bit 0-3 character memory loc.
	dc.b %0				;9006 $0 Horizontal light pen location
	dc.b %0				;9007 $0 Vertical light pen location
	dc.b %11111111			;9008 $FF Paddle X Digitized variable resistance
	dc.b %11111111			;9009 $FF Paddle Y Digitized variable resistance
	dc.b %0				;900A $0 Bass oscillator
	dc.b %0				;900B $0 Alto oscillator
	dc.b %0				;900C $0 Soprano oscillator
	dc.b %0				;900D $0 Noise source
	dc.b %0				;900E $0 Bit 4-7 auxilary colour, bit 0-3 sound loudness
	dc.b %00001110			;900F 
