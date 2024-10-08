;scrollingball.s A red ball zips across and down the screen to no end or reason.

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
	lda $02
	cmp #$1f
	beq main
	inc $02
	jmp clearscreen_loop	

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
	lda #2				;Load A with red
	sta ($fe),Y			;Store red in the colour map
	lda #81				;Load ball character code into A
	sta ($01),Y			;Store red ball on screen map
	iny				;Move to the right by 1
	sty $04				;Store Y temporarily in zero page
	jsr delay			;Call delay, Y will be lost.
	ldy $04				;Reload the incrementor back to Y
	bne main_loop			;Loop around until Y loops from $FF to $00
	lda $ff				;Load high-byte base address to A for checking
	cmp #$97			;Check if we're only halfway done.
	beq clearscreen			;If we're fully done, go back to clearscreen
	
	dey				;Otherwise erase the ball to the left
	lda #32
	sta ($01),Y
	iny				;Go forward one
	inc $02				;Increment high-byte screen addr base from $1E to $1F
	inc $ff				;Increment high-byte colour addr base from $96 to $97
	jmp main_loop			;Keep drawing balls.

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
