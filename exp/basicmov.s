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
	lda #>PLRSTRT
	sta $02
	lda #<PLRCOLR
	sta $05
	lda #>PLRCOLR
	sta $06
	lda #2
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
	lda #32
	sta ($01,X)
	lda $01
	sec
	sbc #1
	sta $01
	lda $05
	sec
	sbc #1
	sta $05
	bcs draw_left
	lda $02
	cmp #$1E
	beq draw_left
	dec $02
	dec $06
check_left:
	jsr global_collision
	beq draw_left
	inc $01
	inc $05
	jmp draw_left
draw_left:
	lda #81
	sta ($01,X)
	lda #2
	sta ($05,X)
end_move_left:
	jsr delay
	jmp game_loop

move_right:
	lda #32
	sta ($01,X)
	lda $01
	clc
	adc #1
	sta $01
	lda $05
	clc
	adc #1
	sta $05
	bcc draw_right
	lda $02
	cmp #$1F
	beq draw_right
	inc $02
	inc $06
check_right:
	jsr global_collision
	beq draw_right
	dec $01
	dec $05
	jmp draw_right
draw_right:
	lda #81
	sta ($01,X)
	lda #2
	sta ($05,X)
	jsr delay
	jmp game_loop

move_up:
	lda #32
	sta ($01,X)
	lda $01
	sec
	sbc #22
	sta $01
	lda $05
	sec
	sbc #22
	sta $05
	bcs draw_up
	lda $02
	cmp #$1e
	beq draw_up
	dec $02
	dec $06
check_up:
	jsr global_collision
	beq draw_up
	lda $01
	clc
	adc #22
	sta $01
	lda $05
	clc
	adc #22
	sta $05
	jmp draw_up
draw_up:
	lda #81
	sta ($01,X)
	lda #2
	sta ($05,X)
end_move_up:
	jsr delay
	jsr delay
	jmp game_loop

move_down:
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
	bcc draw_down
	lda $02
	cmp #$1f
	beq draw_down
	inc $02
	inc $06
check_down:
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
	lda BRDR_SCR_COLOUR
	and #%11111111
	ora #%11110000
	sta BRDR_SCR_COLOUR
	jmp main

global_collision:
	lda $02
	sec
	sbc #$1e
	bcc collide
	lda $02
	cmp #$1f
	bpl collide
	lda $01
	cmp #$f9
	bpl collide
	lda #0
	jmp ret_global_collision
collide:
	lda #1
	jmp ret_global_collision
ret_global_collision:
	rts

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
