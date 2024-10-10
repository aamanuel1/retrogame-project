
;pollinput.s Change screen colour when you press WASD
;Note: Using GETIN, and SCNKEY are too slow, tap current key location in zero page instead

	processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f
CURKEY = $c5

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
	cmp #$1f			;If we're only halfway done ($1e) then
	beq main
	inc $02				;Increment 1e to 1f then continue
	jmp clearscreen_loop	

main:		
	lda CURKEY			;CURKEY is $c5 in zero page
	cmp #$40			;$40 is 64 dec, see pg 179 of VIC20 ref
	beq no_move			;64 is no button pressed
	cmp #$09			;$09 is 9 dec, W button
	beq move_up			
	cmp #$11			;$11 is 17 dec, A button
	beq move_left
	cmp #$29			;$29 is 41 dec, S button
	beq move_down
	cmp #$12			;$12 is 18 dec, D button
	beq move_right
	cmp #$0F			;$0F is 15 dec, return button
	beq shoot
	jmp main			;End of "game" loop

no_move:
	lda BRDR_SCR_COLOUR		;Border screen colour is $900f
	and #%00001111			;AND the existing to turn off bits
	ora #%00000000			;OR the result with 1 in the bits you want on
	;lda #%00001110			;or just load A with the bitmask you want.
	sta BRDR_SCR_COLOUR		;and forget the rest.	
	jmp main			;Similar code for rest.
	
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
	lda BRDR_SCR_COLOUR
	and #%11111111
	ora #%11110000
	sta BRDR_SCR_COLOUR
	jmp main

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
