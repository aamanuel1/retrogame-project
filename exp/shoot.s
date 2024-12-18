
;shoot.s Enhance basicmov.s with shooting mechanic.

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
CLOCK = $a2
PREVDIR = $09
BULLETCOUNT = $10
MAX_BULLET = 8

;VIC CHIP
BRDR_SCR_COLOUR = $900f

;OBJECTS TABLE
	SEG.U objects
	ORG $12
numbullet ds 1
bullets ds 8
bulletdir ds 8
bulletx ds 8
bullety ds 8
bulletlow ds 8
bullethigh ds 8
bulletcolour ds 8
bulletaddr ds 2
	SEG
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
	;Establish bullets number
	lda #0
	sta numbullet
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
	ldx #0
	lda CURKEY			;CURKEY is $c5 in zero page
	jsr update_bullet

	cmp #$09			;$09 is 9 dec, W button
	bne poll_left 
	jmp turn
poll_left:				
	cmp #$11			;$11 is 17 dec, A button
	bne poll_down
	jmp turn
poll_down:	
	cmp #$29			;$29 is 41 dec, S button
	bne poll_right
	jmp turn
poll_right:	
	cmp #$12			;$12 is 18 dec, D button
	bne poll_shoot
	jmp turn
poll_shoot:
	cmp #$0F			;$0F is 15 dec, return button
	bne end_poll 
	jsr shoot
end_poll:
	cmp #$40			;$40 is 64 dec, see pg 179 of VIC20 ref
	beq game_loop			;64 is no button pressed //To remove?!?
	lda #20 
	jsr delay

	jmp game_loop			;End of "game" loop

turn:	
	sta PREVDIR			;Stub turn function, stores previous direction
	jmp game_loop

shoot:	
	ldx numbullet			;Load bullet index
	cpx MAX_BULLET
	beq no_shoot

	lda PREVDIR
	sta bulletdir,X			;load current key from A before it's wiped
	
	;get the player location
	lda PLRX			;Get player X position from zero page
	sta bulletx,X
	lda PLRY			;Get player Y position from zero page
	sta bullety,X
	ldy #0

	lda $01				;Load screen memory location (near centre of screen)
	sta bulletlow,X
	lda $02				;Load high bits of memory screen location
	sta bullethigh,X

	lda bulletdir,X			;Load bullet direction then redirect
	cmp #$09
	beq shoot_up
	cmp #$11
	beq shoot_left
	cmp #$29
	beq shoot_down
	cmp #$12
	beq shoot_right	
	jmp no_shoot			;If the bullet direction is not WASD then don't shoot

shoot_left:	
	lda #67				;Store line sprite
	sta bullets,X			;store the sprite in bullets list
	
	lda bulletlow,X			;Load the low byte of the bullet at X
	sec				;Set carry
	sbc #1				;subtract bullet position by 1 to move left

	dec bulletx,X			;Decrement the bullet's X position at bulletx[X]
	jmp shoot_ret

shoot_right:
	lda #67				;Similar to shoot left but move right by adding 1
	sta bullets,X

	lda bulletlow,X
	clc
	adc #1

	inc bulletx,X
	jmp shoot_ret

shoot_up:
	lda #93				;Similar to above but move up by subtracting screenmem by 22
	sta bullets,X

	lda bulletlow,X
	sec
	sbc #22

	dec bullety,X			;Decrement the Y position
	jmp shoot_ret

shoot_down:
	lda #93				;Similar to above but move down by adding 22 to screenmem
	sta bullets,X

	lda bulletlow,X
	clc
	adc #22

	inc bullety,X
	jmp shoot_ret	

shoot_ret:
	sta bulletlow,X			;Store shifted low byte of bullet screenmem in zero page
	lda bullets,X			;Load the object we'll draw into accumulator
	sta (bulletlow,X)		;Draw the object (indirect indexed deref)
	inx				;Increment bullets
	stx numbullet 			;Store bullet index
no_shoot:
	rts				;return

update_bullet:
	ldx #0				;load index

update_bullet_loop:			;Start of loop
	cpx numbullet			;for(i=0; i<numbullet && i<MAX_BULLET)
	bmi check_bulletdir
	jmp update_bullet_end
	cpx MAX_BULLET
	bmi check_bulletdir
	jmp update_bullet_end		;End for loop

check_bulletdir:
	lda #32				;Remove the bullet at current location
	ldy #0
	sta (bulletlow,X)		;Hacky stopgap measure to erase first draw from origin
	sta (bulletaddr),Y		;Better measure for removing bullet at full 16bit addr
	
	lda bulletdir,X			;Draw the bullet at the right direction similar to shoot
	cmp #$09
	beq draw_up
	cmp #$11
	beq draw_left
	cmp #$29
	beq draw_down
	cmp #$12
	beq draw_right	
	jmp no_draw

draw_left:
	lda bulletx,X			;Load A and Y for the global collision func 
	ldy bullety,X
	sec				
	sbc #1				;subtract bulletx in accumulator by 1
	sta bulletx,X			;store shifted bulletx into zero page location
	
	jsr global_collision		;Call global_collision
	beq no_collision_left		;If the call returns 0 false keep going
	jmp remove_bullet		;If it returns 1 true then remove the bullet.
	
no_collision_left:	
	lda #67				;Store circle "sprite"
	sta bullets,X			;store the sprite in bullets list
		
	lda bulletlow,X			;If there was no collison, let's draw the bullet
	sec				;Similar to shoot code
	sbc #1
	sta bulletlow,X	

	bcs continue_left		;Rollover check
	lda bullethigh,X
	cmp #$1E
	beq continue_left
	dec bullethigh,X
continue_left:
	jmp draw_bullet

draw_right:
	lda bulletx,X			;Similar to previous code above
	ldy bullety,X
	clc
	adc #1
	sta bulletx,X
	
	jsr global_collision
	bne remove_bullet
	
	lda #67
	sta bullets,X

	lda bulletlow,X
	clc
	adc #1
	sta bulletlow,X

	bcc continue_right
	lda bullethigh,X
	cmp #$1F
	beq continue_right
	inc bullethigh,X
continue_right:
	jmp draw_bullet

draw_up:
	lda bulletx,X
	ldy bullety,X
	dey

	jsr global_collision
	bne remove_bullet
	
	sty bullety,X
	
	lda #93
	sta bullets,X

	lda bulletlow,X
	sec
	sbc #22
	sta bulletlow,X	

	bcs continue_up
	lda bullethigh,X
	cmp #$1E
	beq continue_up
	dec bullethigh,X
continue_up:
	jmp draw_bullet

draw_down:
	lda bulletx,X
	ldy bullety,X
	iny

	jsr global_collision
	bne remove_bullet

	sty bullety,X	

	lda #93
	sta bullets,X

	lda bulletlow,X
	clc
	adc #22
	sta bulletlow,X
	
	bcc continue_down
	lda bullethigh,X
	cmp #$1f
	bpl continue_down
	inc bullethigh,X
continue_down:
	jmp draw_bullet	

remove_bullet:
	lda #$40			;Load the null direction no button pressed into
	sta bulletdir,X			;into bulletdir
	lda #0				;Load 0 into X and Y positions
	sta bulletx,X
	sta bullety,X
	;dex
	dec numbullet			;Decrement the number of bullets
	inx				;Increment X to continue the bullet update
	jmp update_bullet_loop

draw_bullet:
;	sta bulletlow,X			;Old code for drawing based on only bulletlow 
;	lda bullets,X
;	sta (bulletlow,X)
	lda bulletlow,X			;New code, take bulletlow and bullethigh
	sta bulletaddr			;Then store as 16 bit unit in two bytes of zero page
	lda bullethigh,X
	ldy #1
	sta bulletaddr,Y		;This is a little endian machine
	ldy #0
	lda bullets,X			;load and draw the bullet
	sta (bulletaddr),Y		;Draw the bullet (dereference with indirect indexed)

	
no_draw:	
	;increment and start again
	inx				;Loop through all bullets
	jmp update_bullet_loop

update_bullet_end:
	rts

global_collision:
	;lda PLRX			;Load player X position
	cmp #0
	bmi collide			;X < 0 we collided with left edge
	cmp #22
	bpl collide			;X > 22 we collided with right edge
	;lda PLRY			;Load player Y position
	cpy #0	
	bmi collide			;Y < 0 we collided with top edge
	cpy #23
	bpl collide			;Y > 23 we collided with bottom edge
	lda #0				;Return false
	jmp ret_global_collision
collide:
	lda #1				;Return true
	jmp ret_global_collision
ret_global_collision:
	rts				;Return result.

delay:
	clc				;Clear carry
	adc CLOCK			;A = A + CLOCK
delay_loop:
	cmp CLOCK			
	bne delay_loop			;if clock != clock + A keep going
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

