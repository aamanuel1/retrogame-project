
;pollinput.s Change screen colour when you press an arrow key.

	processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f

;VIC CHIP
BRDR_SCR_COLOUR = $900f

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
	lda $02
	cmp #$1f
	beq main
	inc $02
	jmp clearscreen_loop	

main:		
	;jsr SCNKEY
	jsr GETIN
	cmp #0
	;lda #%00011110
	;sta $900e
	beq no_move
	cmp #87
	beq move_up
	cmp #65
	beq move_left
	cmp #83
	beq move_down
	cmp #68
	beq move_right
	
	jmp main

no_move:
	lda BRDR_SCR_COLOUR
	and #%00001111
	ora #%00000000
	;lda #%00001110
	sta BRDR_SCR_COLOUR	
	jmp main
	
move_left:
	lda BRDR_SCR_COLOUR
	and #%00011111
	ora #%00010000
	;lda #%00011110
	sta BRDR_SCR_COLOUR
	jmp main

move_right:
	lda BRDR_SCR_COLOUR
	and #%01101111
	ora #%01100000
	;lda #%01101110
	sta BRDR_SCR_COLOUR
	jmp main

move_up:
	lda BRDR_SCR_COLOUR
	and #%01011111
	ora #%01010000
	;lda #%01011110
	sta BRDR_SCR_COLOUR
	jmp main

move_down:
	lda BRDR_SCR_COLOUR
	and #%00101111
	ora #%00100000
	;lda #%00101110
	sta BRDR_SCR_COLOUR
	jmp main

shoot:

delay:
	ldx #$ff			;Delay 255 times
delay_loop:
	nop				;NOP is 2 clock cycles, 12 nops, 2 * 255 = 510 clock cycles
	nop				;12 nops is about right for this animation to work...
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
	bne delay_loop			;Done delaying 255 times, go back.
	rts

videosettings:
	dc.b %00001100			;9000 $05 Interlace off, screen origin horiz 12  (def 5, lower value left
	dc.b %00100110			;9001 $19 Screen origin vertical 5, currently 37, lower value up by 2pix to 1
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
