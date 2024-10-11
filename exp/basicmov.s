;basicmov.s Combine the game loop in pollinput.s with scrollingball.s

	processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f
CURKEY = $c5
SCRMEM = $1e00
COLMEM = $9600
PLRSTRT = $1ee6
PLRCOLR = $96e6
PLRX = $07
PLRY = $08

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
	lda #<SCRMEM			;Load low byte of address
	sta $01
	lda #>SCRMEM			;Load high byte of address
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
	;Establish starting point
	ldx #0
	lda #<PLRSTRT
	sta $01
	lda #>PLRSTRT			;Player starts at 7910, $1ee6
	sta $02
	lda #10				
	sta PLRX			;Offset x by 10, $0a TODO: better way to do this?
	lda #10				;Range 0-22
	sta PLRY			;Offset y by 10
	lda #<PLRCOLR			;Player colour map at 38638, $96e6
	sta $05
	lda #>PLRCOLR
	sta $06
	lda #2				;Load character and colour into low bytes of maps
	sta ($05,X)
	lda #81
	sta ($01,X)


game_loop:		
	lda CURKEY			;CURKEY is $c5 in zero page
	cmp #$40			;$40 is 64 dec, see pg 179 of VIC20 ref
	beq game_loop			;64 is no button pressed
	cmp #$09			;$09 is 9 dec, W button
	bne poll_left 
	jmp move_up
poll_left:				
	cmp #$11			;$11 is 17 dec, A button
	bne poll_down
	jmp move_left
poll_down:	
	cmp #$29			;$29 is 41 dec, S button
	bne poll_right
	jmp move_down
poll_right:	
	cmp #$12			;$12 is 18 dec, D button
	bne poll_shoot
	jmp move_right
poll_shoot:
	cmp #$0F			;$0F is 15 dec, return button
	bne end_poll 
	jmp shoot
end_poll:
	jsr delay
	jmp game_loop			;End of "game" loop
	
move_left:
	lda #32				;Clear current location with SPACE
	sta ($01,X)
	lda $01				;Load current location for operations
	sec				;DON'T forget to set this.
	sbc #1				;subtract 1 to move left
	sta $01				;Store in low byte of screen map
	lda $05				;Load current colour map location
	sec
	sbc #1				;Subtract by 1 to colour left tile
	sta $05				;Store in low byte of screen map
	bcs check_left			;Do a check of the location if didn't roll over
	lda $02				;If we did roll over (carry clear)
	cmp #$1E			;Do a check if we're in $1F
	beq check_left			;If we're not then continue the ckeck
	dec $02				;Otherwise, turn $1F to $1E
	dec $06				;turn $97 to $96
check_left:
	dec PLRX			;decrement player X position
	jsr global_collision		;Check if it's bumping against edge of map
	beq draw_left			;If it's 0 then keep going
	inc $01				;If it's 1 then reset everything.
	inc $05
	inc PLRX			;Reset the X counter
	jmp draw_left			;Draw it not moving.
draw_left:
	lda #81				;Draw a circle...
	sta ($01,X)
	lda #2				;That is red.
	sta ($05,X)
end_move_left:
	jsr delay			;Runs fast, need to figure that out.
	jmp game_loop

move_right:
	lda #32				;Same as above but we're adding 1 to move right
	sta ($01,X)
	lda $01
	clc
	adc #1
	sta $01
	lda $05
	clc
	adc #1
	sta $05
	bcc check_right
	lda $02
	cmp #$1F
	beq check_right
	inc $02
	inc $06
check_right:
	inc PLRX
	jsr global_collision
	beq draw_right
	dec $01
	dec $05
	dec PLRX
	jmp draw_right
draw_right:
	lda #81
	sta ($01,X)
	lda #2
	sta ($05,X) 
end_move_right:
	jsr delay
	jmp game_loop

move_up:
	lda #32				;Same as above, but we're subtracting by 22 to move up
	sta ($01,X)
	lda $01				;Load low byte of current location
	sec				;Set carry flag, carry clear means we borrowed
	sbc #22				;Subtract by 22 because the space above is offset by 22 (22 cols)
	sta $01				;Store new location in screen location
	lda $05				;Do the same with the colour map
	sec		
	sbc #22
	sta $05
	bcs check_up			;Carry set means we didn't roll over
	lda $02
	cmp #$1e			;The carry was clear (we rolled over) so check if it's already in 1e
	beq check_up			;If it's in $1exx then keep going (maybe we don't need this)
	dec $02				;Otherwise it's in $1f so bring it to $1e
	dec $06				;bring from $97 to $96
check_up:
	dec PLRY			;Decrement by 1
	jsr global_collision		;Check if it's collided with the edge
	beq draw_up			;If false, continue drawing
	lda $01				;If true then reset everything by adding it back or incrementing it back
	clc
	adc #22
	sta $01
	lda $05
	clc
	adc #22
	sta $05
	inc PLRY
	jmp draw_up			;Draw it not moving
draw_up:
	lda #81				;Draw a red circle
	sta ($01,X)
	lda #2
	sta ($05,X)
end_move_up:
	jsr delay			;Delay to make it less fast
	jsr delay
	jmp game_loop

move_down:				;Same as above but add 22 to move down
	lda #32					
	sta ($01,X)
	lda $01
	clc
	adc #22
	sta $01
	lda $05
	clc
	adc #22
	sta $05
	bcc check_down
	lda $02
	cmp #$1f
	beq check_down
	inc $02
	inc $06
check_down:
	inc PLRY
	jsr global_collision
	beq draw_down
	lda $01
	sec
	sbc #22
	sta $01
	lda $05
	sec
	sbc #22
	sta $05
	dec PLRY
	jmp draw_down
draw_down:
	lda #81
	sta ($01,X)
	lda #2
	sta ($05,X)
end_move_down:
	jsr delay
	jmp game_loop

shoot:
	lda BRDR_SCR_COLOUR		;Change the screen colour to yellow
	and #%11111111
	ora #%11110000
	sta BRDR_SCR_COLOUR
	jmp main

global_collision:
	lda PLRX			;Load player X position
	cmp #0
	bmi collide			;X < 0 we collided with left edge
	cmp #22
	bpl collide			;X > 22 we collided with right edge
	lda PLRY			;Load player Y position
	cmp #0	
	bmi collide			;X < 0 we collided with top edge
	cmp #23
	bpl collide			;X > 23 we collided with bottom edge
;	lda $02				;Some old stuff might delete later.
;	sec
;	sbc #$1e
;	bcc collide
;	lda $02
;	cmp #$1f
;	bpl collide
;	lda $01
;	cmp #$f9
;	bpl collide
	lda #0				;Return false
	jmp ret_global_collision
collide:
	lda #1				;Return true
	jmp ret_global_collision
ret_global_collision:
	rts				;Return result.

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
	dc.b %00001100			;9000 $05 Interlace off, screen origin horiz 12  (wef 5, lower value left
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
