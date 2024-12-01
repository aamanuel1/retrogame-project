; jailbreak.s Jailbreak game source code

	processor 6502

;CONSTANTS
MAX_BULLET = $08
MAX_ENEMIES = 8
TRUE = 1
FALSE = 0
MAX_ENEMY_CYC = 10

;BUTTONS
NO_KEY = $40
UP_BUTTON = $09
LEFT_BUTTON = $11
DOWN_BUTTON = $29
RIGHT_BUTTON = $12
ENTER = $0F

; KERNAL ADDRESSES
CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f
PLOT = $fff0

;IMPORTANT ZERO PG ADDRESSESS
CURKEY = $c5
CLOCK = $a2

;MISC ADDRESSES
SCRMEM = $1e00
COLMEM = $9600
PLRSTRT = $1ee6
PLRCOLR = $96e6

;VRAM POINTERS
SCR_PTR_LO = $00
SCR_PTR_HI = $01
COLOUR_PTR_LO = $02
COLOUR_PTR_HI = $03

;PLAYER ATTRIBUTES
PLAYER_X = $04
PLAYER_Y = $05
PLAYER_DIR = $06
PLAYER_SPRITE = $07
PLAYER_LIVES = $08
PLAYER_ALIVE = $09
PLAYER_ADDR_LO = $0A
PLAYER_ADDR_HI = $0B
PLAYER_COLOUR_LO = $0C
PLAYER_COLOUR_HI = $0D

;OBJECTS
	SEG.U enemies
	ORG $0E
num_enemies ds 1
enemy_sprite ds 8
enemy_dir ds 8
enemy_x ds 8
enemy_y ds 8
enemy_low ds 8
enemy_high ds 8
enemy_alive ds 8

	SEG.U bullets
	ORG $47
numbullet ds 1
bullet_sprite ds 8
bullet_dir ds 8
bullet_x ds 8
bullet_y ds 8
bullet_low ds 8
bullet_high ds 8
bullet_colour ds 8
bullet_collide ds 8
	SEG

CUR_LEVEL = $80
CUR_SPRITE = $81
OBJECT_DIR = $82
COUNTER = $83
COLLISION_STATUS = $84
SCRATCH_LO = $85
SCRATCH_HI = $86
REMAINDER = $87
ENEMY_SHOOT_TIMER = $88
ENEMY_MOVE_TIMER = $89
ENEMY_CYCLE_CTR = $8A
ENEMY_DIFF_X = $8B
ENEMY_DIFF_Y = $8C
ENEMY_COUNTER = $8D

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

draw_title:
	; Load and store starting screen memory locations to draw title
        jsr load_screen_memory
	ldy #$00
	
draw_title_loop:
	lda title_chr,Y
	sta (SCR_PTR_LO),Y
	iny
	bne draw_title_loop

	inc SCR_PTR_HI
draw_title_loop_lower:
        ; lda #$20
        ; sta (SCR_PTR_LO),Y
	lda title_chr+$FF,Y
	sta (SCR_PTR_LO),Y
	iny
	bne draw_title_loop_lower

        ldy #$00
colour_title_loop:
	lda #$09
	sta (COLOUR_PTR_LO),Y
	inc COLOUR_PTR_LO
	bne skip_inc_title_colour
        inc COLOUR_PTR_HI
skip_inc_title_colour:
        lda COLOUR_PTR_HI
        cmp #$98
        beq main
        jmp colour_title_loop

main:
        lda CURKEY
        cmp #$20
        beq game_start
	jmp main

game_start:
	jsr load_screen_memory
	lda #$00
	sta numbullet
	sta num_enemies
	sta ENEMY_CYCLE_CTR
        jsr draw_level
	lda #$05
	sta ENEMY_MOVE_TIMER
	lda #$06
	sta ENEMY_SHOOT_TIMER
game_loop:
        jsr poll_input
	jsr update_bullet
	jsr enemy_hunt_player
	jsr enemy_shoot
	jsr enemy_update_timer
	lda #10
        jsr delay
	jmp game_loop			;End of "game" loop

draw_level:
        jsr load_screen_memory
	ldy #$00
        ldx #$00
draw_level_loop:
	lda level_1,Y
	sta (SCR_PTR_LO,X)
        cmp #$37			;Note this will stop working if not facing up.
        beq player_found
	cmp #$3C
	beq enemy_found
	jmp inc_level_draw
enemy_found:
	jsr store_enemy
	jmp inc_level_draw
player_found:
        jsr store_player
inc_level_draw:
        iny
	inc SCR_PTR_LO
	bne draw_level_loop

	inc SCR_PTR_HI
        iny


draw_level_loop_lower:
	lda level_1+$FF,Y
	sta (SCR_PTR_LO,X)
        cmp #$37
        bne skip_store_player_2
        jsr store_player
skip_store_player_2:
        iny
        inc SCR_PTR_LO
	bne draw_level_loop_lower
        rts

store_player:
        lda SCR_PTR_LO
        sta PLAYER_ADDR_LO
        lda SCR_PTR_HI
        sta PLAYER_ADDR_HI
        rts

store_enemy:
	ldx num_enemies			;There's no protection against index overrun fyi.
	lda SCR_PTR_LO
	sta enemy_low,X
	lda SCR_PTR_HI
	sta enemy_high,X
	inc num_enemies
	ldx #$00			;Set to 0 so the indexed indirect works outside of function.
	rts

load_screen_memory:
	lda #<SCRMEM
	sta SCR_PTR_LO
	lda #>SCRMEM
	sta SCR_PTR_HI
	lda #<COLMEM
	sta COLOUR_PTR_LO
	lda #>COLMEM
	sta COLOUR_PTR_HI
        rts

player_to_screen:
	lda PLAYER_ADDR_LO
        sta SCR_PTR_LO
	ldy PLAYER_ADDR_HI
        sty SCR_PTR_HI
        rts

screen_to_player:
	lda SCR_PTR_LO
	sta PLAYER_ADDR_LO
	lda SCR_PTR_HI
	sta PLAYER_ADDR_HI
	rts

poll_input:
	jsr player_to_screen
	lda CURKEY			;CURKEY is $c5 in zero page
	cmp #NO_KEY			;$40 is 64 dec, see pg 179 of VIC20 ref
	beq end_poll_shoot		;64 is no button pressed
	cmp #UP_BUTTON			;$09 is 9 dec, W button
	bne poll_left

	lda #UP_BUTTON
	sta PLAYER_DIR
	sta OBJECT_DIR

	lda #$37
	sta CUR_SPRITE
	jsr move_up
        jmp end_poll
poll_left:				
	cmp #LEFT_BUTTON		;$11 is 17 dec, A button
	bne poll_down
	
	lda #LEFT_BUTTON
	sta PLAYER_DIR
	sta OBJECT_DIR

	lda #$39
	sta CUR_SPRITE
	jsr move_left
        jmp end_poll
poll_down:	
	cmp #DOWN_BUTTON		;$29 is 41 dec, S button
	bne poll_right

	lda #DOWN_BUTTON
	sta PLAYER_DIR
	sta OBJECT_DIR

	lda #$38
	sta CUR_SPRITE
	jsr move_down
        jmp end_poll
poll_right:	
	cmp #RIGHT_BUTTON		;$12 is 18 dec, D button
	bne poll_shoot

	lda #RIGHT_BUTTON
	sta PLAYER_DIR
	sta OBJECT_DIR

	lda #$3A
	sta CUR_SPRITE
	jsr move_right
        jmp end_poll
poll_shoot:
	cmp #ENTER			;$0F is 15 dec, return button
	bne end_poll_shoot

	lda PLAYER_DIR
	sta OBJECT_DIR

	jsr shoot
	jmp end_poll_shoot
idle:
	lda #NO_KEY
	sta PLAYER_DIR
	sta OBJECT_DIR
	; lda #$20
	; sta CUR_SPRITE
	jmp end_poll_shoot
end_poll:
	lda CUR_SPRITE
	sta PLAYER_SPRITE
	jsr screen_to_player
end_poll_shoot:
        rts

move_left:
        ldx #$00
	lda #32				;Clear current location with SPACE
	sta (SCR_PTR_LO,X)
move_left_skip_blank:
	ldx #$00
	lda SCR_PTR_LO			;Load current location for operations
	sec				;DON'T forget to set this.
	sbc #1				;subtract 1 to move left
	sta SCR_PTR_LO	        	;Store in low byte of screen map
	bcs check_left			;Do a check of the location if didn't roll over
	lda SCR_PTR_HI			;If we did roll over (carry clear)
	cmp #$1E			;Do a check if we're in $1F NOTE: Remove later
	beq check_left			;If we're not then continue the ckeck
	dec SCR_PTR_HI			;Otherwise, turn $1F to $1E
check_left:
        ldy #$00
        lda (SCR_PTR_LO),Y
	jsr global_collision		;Check if it's bumping against edge of map
	sta COLLISION_STATUS
	beq draw_left			;If it's 0 then keep going
	inc SCR_PTR_LO			;If it's 1 then reset everything.
	jmp draw_left			;Draw it not moving.
draw_left:
        ldx #$00                        ;X will have the X value of the object from screen to xy
	lda CUR_SPRITE			;Draw the guy
	sta (SCR_PTR_LO,X)
end_move_left:
	rts

move_right:
        ldx #$00
	lda #32				;Same as above but we're adding 1 to move right
	sta (SCR_PTR_LO,X)
move_right_skip_blank:
	ldx #$00
        lda SCR_PTR_LO
	clc
	adc #1
	sta SCR_PTR_LO
	bcc check_right
	lda SCR_PTR_HI
	cmp #$1F
	beq check_right
	inc SCR_PTR_HI
check_right:
        ldy #$00
        lda (SCR_PTR_LO),Y
	jsr global_collision
	sta COLLISION_STATUS
	beq draw_right
	dec SCR_PTR_LO
	jmp draw_right
draw_right:
        ldx #$00
	lda CUR_SPRITE
	sta (SCR_PTR_LO,X)
end_move_right:
        rts

move_up:
	; lda CUR_SPRITE
	; cmp #$2E
	; beq skip_blank_up
	ldx #$00
	lda #32				;Same as above, but we're subtracting by 22 to move up
	sta (SCR_PTR_LO,X)
move_up_skip_blank:
	ldx #$00
	lda SCR_PTR_LO			;Load low byte of current location
	sec				;Set carry flag, carry clear means we borrowed
	sbc #22				;Subtract by 22 because the space above is offset by 22 (22 cols)
	sta SCR_PTR_LO			;Store new location in screen location
	bcs check_up			;Carry set means we didn't roll over
	lda SCR_PTR_HI
	cmp #$1e			;The carry was clear (we rolled over) so check if it's already in 1e
	beq check_up			;If it's in $1exx then keep going (maybe we don't need this)
	dec SCR_PTR_HI			;Otherwise it's in $1f so bring it to $1e
check_up:
	ldy #$00
	lda (SCR_PTR_LO),y
	jsr global_collision		;Check if it's collided with the edge
	sta COLLISION_STATUS
	beq draw_up			;If false, continue drawing
	lda SCR_PTR_LO			;If true then reset everything by adding it back or incrementing it back
	clc
	adc #22
        bcc skip_inc_up
        inc SCR_PTR_HI
skip_inc_up:
	sta SCR_PTR_LO
	jmp draw_up			;Draw it not moving
draw_up:
        ldx #$00
	lda CUR_SPRITE			;Draw a red circle
	sta (SCR_PTR_LO,X)
end_move_up:
	rts

move_down:				;Same as above but add 22 to move down
        ldx #$00
        lda #32
	sta (SCR_PTR_LO,X)
move_down_skip_blank:
	ldx #$00
	lda SCR_PTR_LO 
	clc
	adc #22
	sta SCR_PTR_LO
	bcc check_down
	lda SCR_PTR_HI
	cmp #$1f
	beq check_down
	inc SCR_PTR_HI
check_down:
        ldy #$00
        lda (SCR_PTR_LO),Y
	jsr global_collision
	sta COLLISION_STATUS
	beq draw_down
	lda SCR_PTR_LO
	sec
	sbc #22
        bcs skip_dec_down
        dec SCR_PTR_HI
skip_dec_down:
	sta SCR_PTR_LO
	jmp draw_down
draw_down:
        ldx #$00
        lda CUR_SPRITE
	sta (SCR_PTR_LO,X)
end_move_down:
	rts

shoot:
	ldx numbullet
	cpx #MAX_BULLET
	bpl add_bullet_start
	ldx #$00
	stx numbullet
add_bullet_start:
	lda OBJECT_DIR
	sta bullet_dir,X

	cmp #UP_BUTTON
	bne bullet_left
	lda #UP_BUTTON
	sta OBJECT_DIR
	sta bullet_dir,X
	lda #$34			;note LDA BULLET AND STA CURSPRITE COULD GO INTO SUBROUTINE
	sta CUR_SPRITE
	jsr move_up_skip_blank
	jmp shoot_end
bullet_left:
	cmp #LEFT_BUTTON
	bne bullet_down
	lda #LEFT_BUTTON
	sta OBJECT_DIR
	sta bullet_dir,X
	lda #$33
	sta CUR_SPRITE
	jsr move_left_skip_blank
	jmp shoot_end
bullet_down:
	cmp #DOWN_BUTTON
	bne bullet_right
	lda #DOWN_BUTTON
	sta OBJECT_DIR
	sta bullet_dir,X
	lda #$34
	sta CUR_SPRITE
	jsr move_down_skip_blank
	jmp shoot_end
bullet_right:
	cmp #RIGHT_BUTTON
	bne shoot_end
	lda #RIGHT_BUTTON
	sta OBJECT_DIR
	sta bullet_dir,X
	lda #$33
	sta CUR_SPRITE
	jsr move_right_skip_blank
shoot_end:
	ldx numbullet
	lda CUR_SPRITE
	sta bullet_sprite,X
	lda SCR_PTR_LO
	sta bullet_low,X
	lda SCR_PTR_HI
	sta bullet_high,X
	inc numbullet
        rts

update_bullet:
	ldx #0

update_bullet_loop:
	cpx numbullet
	beq update_bullet_end
	bpl update_bullet_end
	cpx #MAX_BULLET-1
	beq update_bullet_end
	stx COUNTER

	lda bullet_low,X
	sta SCR_PTR_LO

	lda bullet_high,X
	sta SCR_PTR_HI

	lda bullet_sprite,X
	sta CUR_SPRITE

	lda bullet_dir,X
	sta OBJECT_DIR

	cmp #UP_BUTTON
	bne update_bullet_left
	jsr move_up
	jmp update_bullet_inc
update_bullet_left:
	cmp #LEFT_BUTTON
	bne update_bullet_down
	jsr move_left
	jmp update_bullet_inc
update_bullet_down:
	cmp #DOWN_BUTTON
	bne update_bullet_right
	jsr move_down
	jmp update_bullet_inc
update_bullet_right:
	cmp #RIGHT_BUTTON
	bne update_bullet_inc
	jsr move_right
update_bullet_inc:
	lda SCR_PTR_LO
	sta bullet_low,X
	lda SCR_PTR_HI
	sta bullet_high,X
check_bullet_status:
	lda COLLISION_STATUS
	beq inc_bullet_counter
remove_bullet:
	lda #NO_KEY
	sta bullet_dir,X
	lda #32
	ldy #$00
	sta bullet_sprite,X
	sta (SCR_PTR_LO),Y
inc_bullet_counter:
	inc COUNTER
	ldx COUNTER
	jmp update_bullet_loop
update_bullet_end:
	rts

enemy_hunt_player:
	lda ENEMY_CYCLE_CTR
	cmp ENEMY_MOVE_TIMER
	bne enemy_hunt_player_ret
	jsr player_to_screen
	jsr screen_to_xy
	stx PLAYER_X
	sty PLAYER_Y
	ldx #$00
	stx ENEMY_COUNTER
enemy_hunt_player_loop:
	lda enemy_low,X
	sta SCR_PTR_LO
	lda enemy_high,X
	sta SCR_PTR_HI
	jsr screen_to_xy
	txa
	ldx ENEMY_COUNTER
	sty enemy_y,X
	sta enemy_x,X
	
	sec
	sbc PLAYER_X
	bcs enemy_move_left
enemy_move_right:
	lda #$3E
	sta CUR_SPRITE
	
	lda #RIGHT_BUTTON
	sta enemy_dir,X
	sta OBJECT_DIR
	jsr move_right
	jmp enemy_move_y
enemy_move_left:
 	lda #$3D
	sta CUR_SPRITE
	
	lda #LEFT_BUTTON
	sta enemy_dir,X
	sta OBJECT_DIR
	jsr move_left
enemy_move_y:
	lda enemy_y,X
	
	sec
	sbc PLAYER_Y
	bcc enemy_move_down
enemy_move_up:
	lda #$3B
	sta CUR_SPRITE
	
	lda #UP_BUTTON
	sta enemy_dir,X
	sta OBJECT_DIR
	jsr move_up
	jmp enemy_store
enemy_move_down:
	lda #$3C
	sta CUR_SPRITE

	lda #DOWN_BUTTON
	sta enemy_dir,X
	sta OBJECT_DIR
	jsr move_down
enemy_store:
	lda SCR_PTR_LO
	sta enemy_low,X
	lda SCR_PTR_HI
	sta enemy_high,X
	;jmp enemy_hunt_player_loop
enemy_hunt_player_ret:
	rts

enemy_shoot:
	lda ENEMY_CYCLE_CTR
	cmp ENEMY_SHOOT_TIMER
	bne enemy_shoot_ret

	ldx #$00
	stx ENEMY_COUNTER
enemy_shoot_player_loop:
	lda enemy_low,X
	sta SCR_PTR_LO
	lda enemy_high,X
	sta SCR_PTR_HI
	jsr screen_to_xy
	txa
	ldx ENEMY_COUNTER
	sty enemy_y,X
	sta enemy_x,X

	sec
	sbc PLAYER_X
	sta ENEMY_DIFF_X
	bcs enemy_shoot_pos_x
enemy_shoot_neg_x:
	tya 
	sec
	sbc PLAYER_Y
	sta ENEMY_DIFF_Y
	bcs enemy_shoot_neg_x_pos_y
enemy_shoot_neg_x_neg_y:
	lda ENEMY_DIFF_X
	cmp ENEMY_DIFF_Y
	bpl enemy_shoot_right
	beq enemy_shoot_right
	bmi enemy_shoot_up
enemy_shoot_neg_x_pos_y:
	lda ENEMY_DIFF_X
	cmp ENEMY_DIFF_Y
	bpl enemy_shoot_down
	beq enemy_shoot_right
	bmi enemy_shoot_right
enemy_shoot_pos_x:
	tya 
	sec
	sbc PLAYER_Y
	sta ENEMY_DIFF_Y
	bcs enemy_shoot_pos_x_pos_y
enemy_shoot_pos_x_neg_y:
	lda ENEMY_DIFF_X
	cmp ENEMY_DIFF_Y
	bpl enemy_shoot_left
	beq enemy_shoot_left
	bmi enemy_shoot_up
enemy_shoot_pos_x_pos_y:
	lda ENEMY_DIFF_X
	cmp ENEMY_DIFF_Y
	bpl enemy_shoot_down
	beq enemy_shoot_left
	bmi enemy_shoot_left
enemy_shoot_right:
	lda #RIGHT_BUTTON
	jmp enemy_shoot_dir_store
enemy_shoot_up:
	lda #UP_BUTTON
	jmp enemy_shoot_dir_store
enemy_shoot_down:
	lda #DOWN_BUTTON
	jmp enemy_shoot_dir_store
enemy_shoot_left:
	lda #LEFT_BUTTON
enemy_shoot_dir_store:
	sta OBJECT_DIR
	jsr shoot
enemy_shoot_ret:
	rts

enemy_update_timer:
	inc ENEMY_CYCLE_CTR
	lda ENEMY_CYCLE_CTR
	cmp #MAX_ENEMY_CYC
	bmi enemy_update_ret		;IF ENEMY_CYCLE_CTR < MAX_ENEMY_CYC keep going
	lda #00				;Otherwise reset counter
	sta ENEMY_CYCLE_CTR
enemy_update_ret:
	rts

global_collision:
	cmp #$3F			;Compare with wall
	beq collide			;X < 0 we collided with left edge
	jsr screen_to_xy
	cpx #0
	bmi collide
	cpx #21
	bpl collide
	cpy #0
	bmi collide
	cpy #22
	bpl collide
	lda #0				;Return false
	jmp ret_global_collision
collide:
	lda #1				;Return true
ret_global_collision:
	rts				;Return result.

screen_to_xy:
	lda SCR_PTR_HI
	sta SCRATCH_HI
	lda SCR_PTR_LO
	sta SCRATCH_LO

	ldx #$00
	ldy #$00
screen_to_xy_loop:
	sec
	sbc #22
	sta SCRATCH_LO
	bcs skip_xy_loop_dec_hi
	pha				;Push A onto stack before incrementing the high byte or remainder will be lost
	lda SCRATCH_HI
	cmp #$1E
	beq screen_to_xy_ret
	dec SCRATCH_HI
	pla
skip_xy_loop_dec_hi:	
	sta REMAINDER
	iny
	jmp screen_to_xy_loop
screen_to_xy_ret:
	pla
	ldx REMAINDER
	rts

delay:
	clc				;clear carry flag
	adc CLOCK			;Add existing time with lowest 24-bit 3byte jiffy clock value
delay_loop:
	cmp CLOCK			;Compare A + clock with clock not equal then we haven't finished yet
	bne delay_loop			
	rts

videosettings:
	dc.b %00001100			;9000 $05 Interlace off, screen origin horiz 12  (def 5, lower value left
	dc.b %00011110			;9001 $19 Screen origin vertical 5, lower value up by 2pix to 1
	dc.b %10010110			;9002 $96 bit 7 Screen mem location, rest columns (current default 22 max 27)
	dc.b %10101110			;9003 $AE bit 7 is screen raster, bit 6-1  rows (default 23 max 23)
					;bit 0 is character size 0 8x8 chars, 1 8x16 chars
	dc.b %01111010			;9004 $7A 122 TV raster beam line
	dc.b %11111111			;9005 $F0 bits 4-7 video address, bit 0-3 character memory loc (changed 0000 to 1110 $1800).
	dc.b %0				;9006 $0 Horizontal light pen location
	dc.b %0				;9007 $0 Vertical light pen location
	dc.b %11111111			;9008 $FF Paddle X Digitized variable resistance
	dc.b %11111111			;9009 $FF Paddle Y Digitized variable resistance
	dc.b %0				;900A $0 Bass oscillator
	dc.b %0				;900B $0 Alto oscillator
	dc.b %0				;900C $0 Soprano oscillator
	dc.b %0				;900D $0 Noise source
	dc.b %00010000			;900E $0 Bit 4-7 auxilary colour, bit 0-3 sound loudness
	dc.b %00000110			;900F Screen colour (black) border colour dark blue

; screen character data replace with compression later.
title_chr:
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $00, $21, $22, $23, $14, $15, $06, $07, $06, $18, $19, $1A, $1B, $1C, $14, $1D, $20, $20, $20
	dc.b	$20, $20, $20, $10, $02, $03, $04, $24, $15, $16, $17, $16, $28, $29, $2A, $2B, $2C, $0D, $0E, $20, $20, $20
	dc.b	$20, $20, $01, $11, $12, $13, $12, $05, $25, $26, $27, $08, $09, $0A, $0B, $0C, $12, $05, $1E, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $82, $99, $20, $81, $81, $92, $8F, $8E, $20, $8D, $81, $8E, $95, $85, $8C, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $90, $92, $85, $93, $93, $20, $93, $90, $81, $83, $85, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20

level_1:
        dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $3F, $3F, $3F, $3F, $3F, $3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $37, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $20, $20, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F

	org $1C00
; Character bitmap definitions only 63 chars, reverse mode set to retain the base alphanum set because the vic chip loops around
	dc.b	$3C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	dc.b	$00, $15, $15, $15, $15, $15, $00, $00
	dc.b	$14, $14, $14, $15, $15, $15, $14, $14
	dc.b	$1F, $1F, $1F, $5F, $5F, $5F, $1F, $1F
	dc.b	$14, $14, $14, $14, $14, $14, $14, $14
	dc.b	$17, $17, $17, $17, $17, $17, $17, $03
	dc.b	$00, $55, $55, $50, $50, $50, $50, $50
	dc.b	$0F, $4F, $5F, $5F, $1F, $1F, $1F, $1F
	dc.b	$50, $50, $50, $50, $50, $50, $50, $00
	dc.b	$17, $17, $17, $17, $17, $17, $07, $03
	dc.b	$D4, $D4, $D4, $D4, $D4, $D5, $D5, $C0
	dc.b	$00, $00, $00, $00, $00, $54, $54, $00
	dc.b	$14, $34, $F4, $F4, $F4, $F4, $F4, $F0
	dc.b	$17, $17, $15, $15, $15, $15, $17, $17
	dc.b	$40, $40, $40, $40, $40, $C0, $C0, $C0
	dc.b	$18, $24, $42, $42, $42, $24, $18, $00
	dc.b	$7C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	dc.b	$7C, $7C, $7C, $7C, $7C, $3C, $3C, $3C
	dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	dc.b	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $0F
	dc.b	$03, $17, $17, $17, $17, $17, $17, $17
	dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	dc.b	$50, $50, $50, $55, $55, $50, $50, $50
	dc.b	$1F, $1F, $1F, $5F, $5F, $1F, $1F, $1F
	dc.b	$03, $43, $53, $57, $17, $17, $17, $17
	dc.b	$C0, $D5, $D5, $D4, $D4, $D4, $D4, $D4
	dc.b	$00, $54, $54, $00, $00, $00, $00, $00
	dc.b	$F0, $F5, $F5, $F5, $F5, $F5, $F4, $F4
	dc.b	$00, $50, $50, $50, $50, $50, $14, $14
	dc.b	$C0, $C5, $C5, $D5, $D4, $D4, $50, $40
	dc.b	$C0, $D0, $D4, $D4, $D5, $C5, $C5, $C0
	dc.b	$08, $1C, $2A, $77, $2A, $08, $08, $00
	dc.b	$00, $00, $00, $00, $00, $00, $00, $00
	dc.b	$00, $05, $05, $05, $05, $05, $14, $14
	dc.b	$0F, $5F, $5F, $5F, $5F, $5F, $1F, $1F
	dc.b	$00, $14, $14, $14, $14, $14, $14, $14
	dc.b	$17, $17, $17, $17, $17, $17, $17, $17
	dc.b	$C0, $C0, $C0, $C0, $D4, $D4, $D4, $C0
	dc.b	$50, $50, $50, $50, $50, $55, $55, $00
	dc.b	$1F, $1F, $1F, $1F, $5F, $5F, $4F, $0F
	dc.b	$17, $17, $17, $57, $57, $57, $17, $17
	dc.b	$D4, $D4, $D4, $D5, $D5, $D4, $D4, $D4
	dc.b	$00, $00, $00, $50, $50, $00, $00, $00
	dc.b	$F4, $F4, $D4, $D5, $15, $15, $14, $14
	dc.b	$14, $14, $14, $54, $54, $54, $14, $14
	dc.b	$40, $40, $40, $40, $40, $C0, $C0, $C0
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$30, $30, $30, $30, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $30, $30, $30, $30
	dc.b	$C0, $C0, $30, $20, $0C, $08, $02, $02
	dc.b	$C0, $C0, $30, $20, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $FF, $00, $00, $00
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$C0, $C0, $30, $20, $0C, $08, $02, $02
	dc.b	$02, $02, $08, $0C, $20, $30, $C0, $C0
	dc.b	$30, $3C, $00, $FF, $00, $3C, $00, $3C
	dc.b	$0C, $3C, $00, $FF, $00, $3C, $00, $3C
	dc.b	$30, $3C, $00, $FC, $00, $3C, $00, $3C
	dc.b	$08, $3C, $00, $3F, $00, $3C, $00, $3C
	dc.b	$14, $3C, $14, $55, $14, $14, $14, $14
	dc.b	$14, $3C, $14, $55, $1C, $14, $14, $14
	dc.b	$14, $3C, $14, $54, $24, $14, $14, $14
	dc.b	$14, $3C, $14, $15, $1C, $14, $14, $14
	dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	dc.b	$00, $00, $00, $00, $00, $00, $00, $00
	; dc.b	$3C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	; dc.b	$00, $05, $05, $05, $05, $05, $14, $14
	; dc.b	$0F, $5F, $5F, $5F, $5F, $5F, $1F, $1F
	; dc.b	$00, $14, $14, $14, $14, $14, $14, $14
	; dc.b	$03, $17, $17, $17, $17, $17, $17, $17
	; dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	; dc.b	$00, $55, $55, $50, $50, $50, $50, $50
	; dc.b	$0F, $4F, $5F, $5F, $1F, $1F, $1F, $1F
	; dc.b	$00, $55, $55, $50, $50, $50, $50, $50
	; dc.b	$03, $43, $53, $57, $17, $17, $17, $17
	; dc.b	$C0, $D5, $D5, $D4, $D4, $D4, $D4, $D4
	; dc.b	$00, $54, $54, $00, $00, $00, $00, $00
	; dc.b	$F0, $F5, $F5, $F5, $F5, $F5, $F4, $F4
	; dc.b	$00, $50, $50, $50, $50, $50, $14, $14
	; dc.b	$03, $17, $17, $17, $17, $17, $17, $17
	; dc.b	$00, $00, $00, $00, $00, $00, $00, $00
	; dc.b	$7C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	; dc.b	$14, $14, $14, $15, $15, $15, $14, $14
	; dc.b	$1F, $1F, $1F, $5F, $5F, $5F, $1F, $1F
	; dc.b	$14, $14, $14, $14, $14, $14, $14, $14
	; dc.b	$17, $17, $17, $17, $17, $17, $17, $17
	; dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	; dc.b	$50, $50, $50, $55, $55, $50, $50, $50
	; dc.b	$1F, $1F, $1F, $5F, $5F, $1F, $1F, $1F
	; dc.b	$50, $50, $50, $55, $55, $54, $50, $50
	; dc.b	$17, $17, $17, $57, $57, $57, $17, $17
	; dc.b	$D4, $D4, $D4, $D5, $D5, $D4, $D4, $D4
	; dc.b	$00, $00, $00, $50, $50, $00, $00, $00
	; dc.b	$F4, $F4, $D4, $D5, $15, $15, $14, $14
	; dc.b	$14, $14, $14, $54, $54, $54, $14, $14
	; dc.b	$17, $17, $15, $15, $15, $15, $17, $17
	; dc.b	$00, $15, $15, $15, $15, $15, $00, $00
	; dc.b	$7C, $7C, $7C, $7C, $7C, $3C, $3C, $3C
	; dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	; dc.b	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $0F
	; dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	; dc.b	$17, $17, $17, $17, $17, $17, $17, $03
	; dc.b	$C0, $C0, $C0, $C0, $D4, $D4, $D4, $C0
	; dc.b	$50, $50, $50, $50, $50, $55, $55, $00
	; dc.b	$1F, $1F, $1F, $1F, $5F, $5F, $4F, $0F
	; dc.b	$50, $50, $50, $50, $50, $50, $50, $00
	; dc.b	$17, $17, $17, $17, $17, $17, $07, $03
	; dc.b	$D4, $D4, $D4, $D4, $D4, $D5, $D5, $C0
	; dc.b	$00, $00, $00, $00, $00, $54, $54, $00
	; dc.b	$14, $34, $F4, $F4, $F4, $F4, $F4, $F0
	; dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	; dc.b	$17, $17, $17, $17, $17, $17, $17, $03
	; dc.b	$C0, $C5, $C5, $D5, $D4, $D4, $50, $40
	; dc.b	$08, $08, $08, $08, $FF, $00, $00, $00
	; dc.b	$00, $00, $00, $00, $FF, $08, $08, $08
	; dc.b	$08, $08, $08, $08, $F8, $08, $08, $08
	; dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	; dc.b	$E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0
	; dc.b	$07, $07, $07, $07, $07, $07, $07, $07
	; dc.b	$FF, $FF, $00, $00, $00, $00, $00, $00
	; dc.b	$FF, $FF, $FF, $00, $00, $00, $00, $00
	; dc.b	$00, $00, $00, $00, $00, $FF, $FF, $FF
	; dc.b	$01, $01, $01, $01, $01, $01, $01, $FF
	; dc.b	$00, $00, $00, $00, $F0, $F0, $F0, $F0
	; dc.b	$0F, $0F, $0F, $0F, $00, $00, $00, $00
	; dc.b	$08, $08, $08, $08, $F8, $00, $00, $00
	; dc.b	$F0, $F0, $F0, $F0, $00, $00, $00, $00
	; dc.b	$F0, $F0, $F0, $F0, $0F, $0F, $0F, $0F
	; dc.b	$40, $40, $40, $40, $40, $C0, $C0, $C0
	; dc.b	$E7, $DB, $BD, $81, $BD, $BD, $BD, $FF
	; dc.b	$83, $DD, $DD, $C3, $DD, $DD, $83, $FF
	; dc.b	$E3, $DD, $BF, $BF, $BF, $DD, $E3, $FF
	; dc.b	$87, $DB, $DD, $DD, $DD, $DB, $87, $FF
	; dc.b	$81, $BF, $BF, $87, $BF, $BF, $81, $FF
	; dc.b	$81, $BF, $BF, $87, $BF, $BF, $BF, $FF
	; dc.b	$E3, $DD, $BF, $B1, $BD, $DD, $E3, $FF
	; dc.b	$BD, $BD, $BD, $81, $BD, $BD, $BD, $FF
	; dc.b	$E3, $F7, $F7, $F7, $F7, $F7, $E3, $FF
	; dc.b	$F1, $FB, $FB, $FB, $FB, $BB, $C7, $FF
	; dc.b	$BD, $BB, $B7, $8F, $B7, $BB, $BD, $FF
	; dc.b	$BF, $BF, $BF, $BF, $BF, $BF, $81, $FF
	; dc.b	$BD, $99, $A5, $A5, $BD, $BD, $BD, $FF
	; dc.b	$BD, $9D, $AD, $B5, $B9, $BD, $BD, $FF
	; dc.b	$E7, $DB, $BD, $BD, $BD, $DB, $E7, $FF
	; dc.b	$C0, $D0, $D4, $D4, $D5, $C5, $C5, $C0
	; dc.b	$E7, $DB, $BD, $BD, $B5, $DB, $E5, $FF
	; dc.b	$83, $BD, $BD, $83, $B7, $BB, $BD, $FF
	; dc.b	$C3, $BD, $BF, $C3, $FD, $BD, $C3, $FF
	; dc.b	$C1, $F7, $F7, $F7, $F7, $F7, $F7, $FF
	; dc.b	$BD, $BD, $BD, $BD, $BD, $BD, $C3, $FF
	; dc.b	$BD, $BD, $BD, $DB, $DB, $E7, $E7, $FF
	; dc.b	$BD, $BD, $BD, $A5, $A5, $99, $BD, $FF
	; dc.b	$BD, $BD, $DB, $E7, $DB, $BD, $BD, $FF
	; dc.b	$DD, $DD, $DD, $E3, $F7, $F7, $F7, $FF
	; dc.b	$81, $FD, $FB, $E7, $DF, $BF, $81, $FF
	; dc.b	$C3, $DF, $DF, $DF, $DF, $DF, $C3, $FF
	; dc.b	$FF, $BF, $DF, $EF, $F7, $FB, $FD, $FF
	; dc.b	$C3, $FB, $FB, $FB, $FB, $FB, $C3, $FF
	; dc.b	$FF, $F7, $E3, $D5, $F7, $F7, $F7, $F7
	; dc.b	$FF, $FF, $EF, $DF, $80, $DF, $EF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	; dc.b	$F7, $F7, $F7, $F7, $FF, $FF, $F7, $FF
	; dc.b	$DB, $DB, $DB, $FF, $FF, $FF, $FF, $FF
	; dc.b	$DB, $DB, $81, $DB, $81, $DB, $DB, $FF
	; dc.b	$F7, $E1, $D7, $E3, $F5, $C3, $F7, $FF
	; dc.b	$FF, $9D, $9B, $F7, $EF, $D9, $B9, $FF
	; dc.b	$CF, $B7, $B7, $CF, $B5, $BB, $C5, $FF
	; dc.b	$FB, $F7, $EF, $FF, $FF, $FF, $FF, $FF
	; dc.b	$FB, $F7, $EF, $EF, $EF, $F7, $FB, $FF
	; dc.b	$DF, $EF, $F7, $F7, $F7, $EF, $DF, $FF
	; dc.b	$F7, $D5, $E3, $C1, $E3, $D5, $F7, $FF
	; dc.b	$FF, $F7, $F7, $C1, $F7, $F7, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $F7, $F7, $EF
	; dc.b	$FF, $FF, $FF, $81, $FF, $FF, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $E7, $E7, $FF
	; dc.b	$FF, $FD, $FB, $F7, $EF, $DF, $BF, $FF
	; dc.b	$C3, $BD, $B9, $A5, $9D, $BD, $C3, $FF
	; dc.b	$F7, $E7, $D7, $F7, $F7, $F7, $C1, $FF
	; dc.b	$C3, $BD, $FD, $F3, $CF, $BF, $81, $FF
	; dc.b	$C3, $BD, $FD, $E3, $FD, $BD, $C3, $FF
	; dc.b	$FB, $F3, $EB, $DB, $81, $FB, $FB, $FF
	; dc.b	$81, $BF, $87, $FB, $FD, $BB, $C7, $FF
	; dc.b	$E3, $DF, $BF, $83, $BD, $BD, $C3, $FF
	; dc.b	$81, $BD, $FB, $F7, $EF, $EF, $EF, $FF
	; dc.b	$C3, $BD, $BD, $C3, $BD, $BD, $C3, $FF
	; dc.b	$C3, $BD, $BD, $C1, $FD, $FB, $C7, $FF
	; dc.b	$FF, $FF, $F7, $FF, $FF, $F7, $FF, $FF
	; dc.b	$FF, $FF, $F7, $FF, $FF, $F7, $F7, $EF
	; dc.b	$F1, $E7, $CF, $9F, $CF, $E7, $F1, $FF
	; dc.b	$FF, $FF, $81, $FF, $81, $FF, $FF, $FF
	; dc.b	$8F, $E7, $F3, $F9, $F3, $E7, $8F, $FF
	; dc.b	$C3, $BD, $FD, $F3, $EF, $FF, $EF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $00, $FF, $FF, $FF
	; dc.b	$F7, $E3, $C1, $80, $80, $E3, $C1, $FF
	; dc.b	$EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF
	; dc.b	$FF, $FF, $FF, $00, $FF, $FF, $FF, $FF
	; dc.b	$FF, $FF, $00, $FF, $FF, $FF, $FF, $FF
	; dc.b	$FF, $00, $FF, $FF, $FF, $FF, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $00, $FF, $FF
	; dc.b	$DF, $DF, $DF, $DF, $DF, $DF, $DF, $DF
	; dc.b	$FB, $FB, $FB, $FB, $FB, $FB, $FB, $FB
	; dc.b	$FF, $FF, $FF, $FF, $1F, $EF, $F7, $F7
	; dc.b	$F7, $F7, $F7, $FB, $FC, $FF, $FF, $FF
	; dc.b	$F7, $F7, $F7, $EF, $1F, $FF, $FF, $FF
	; dc.b	$7F, $7F, $7F, $7F, $7F, $7F, $7F, $00
	; dc.b	$7F, $BF, $DF, $EF, $F7, $FB, $FD, $FE
	; dc.b	$FE, $FD, $FB, $F7, $EF, $DF, $BF, $7F
	; dc.b	$00, $7F, $7F, $7F, $7F, $7F, $7F, $7F
	; dc.b	$00, $FE, $FE, $FE, $FE, $FE, $FE, $FE
	; dc.b	$FF, $C3, $81, $81, $81, $81, $C3, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $00, $FF
	; dc.b	$C9, $80, $80, $80, $C1, $E3, $F7, $FF
	; dc.b	$BF, $BF, $BF, $BF, $BF, $BF, $BF, $BF
	; dc.b	$FF, $FF, $FF, $FF, $FC, $FB, $F7, $F7
	; dc.b	$7E, $BD, $DB, $E7, $E7, $DB, $BD, $7E
	; dc.b	$FF, $C3, $BD, $BD, $BD, $BD, $C3, $FF
	; dc.b	$F7, $E3, $D5, $88, $D5, $F7, $F7, $FF
	; dc.b	$FD, $FD, $FD, $FD, $FD, $FD, $FD, $FD
	; dc.b	$F7, $E3, $C1, $80, $C1, $E3, $F7, $FF
	; dc.b	$F7, $F7, $F7, $F7, $00, $F7, $F7, $F7
	; dc.b	$5F, $AF, $5F, $AF, $5F, $AF, $5F, $AF
	; dc.b	$F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
	; dc.b	$FF, $FF, $FE, $C1, $AB, $EB, $EB, $FF
	; dc.b	$00, $80, $C0, $E0, $F0, $F8, $FC, $FE
	; dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	; dc.b	$0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
	; dc.b	$FF, $FF, $FF, $FF, $00, $00, $00, $00
	; dc.b	$00, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $00
	; dc.b	$7F, $7F, $7F, $7F, $7F, $7F, $7F, $7F
	; dc.b	$55, $AA, $55, $AA, $55, $AA, $55, $AA
	; dc.b	$FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE
	; dc.b	$FF, $FF, $FF, $FF, $55, $AA, $55, $AA
	; dc.b	$00, $01, $03, $07, $0F, $1F, $3F, $7F
	; dc.b	$FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
	; dc.b	$F7, $F7, $F7, $F7, $F0, $F7, $F7, $F7
	; dc.b	$FF, $FF, $FF, $FF, $F0, $F0, $F0, $F0
	; dc.b	$F7, $F7, $F7, $F7, $F0, $FF, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $07, $F7, $F7, $F7
	; dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $00, $00
	; dc.b	$FF, $FF, $FF, $FF, $F0, $F7, $F7, $F7
	; dc.b	$F7, $F7, $F7, $F7, $00, $FF, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $00, $F7, $F7, $F7
	; dc.b	$F7, $F7, $F7, $F7, $07, $F7, $F7, $F7
	; dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	; dc.b	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
	; dc.b	$F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
	; dc.b	$00, $00, $FF, $FF, $FF, $FF, $FF, $FF
	; dc.b	$00, $00, $00, $FF, $FF, $FF, $FF, $FF
	; dc.b	$FF, $FF, $FF, $FF, $FF, $00, $00, $00
	; dc.b	$FE, $FE, $FE, $FE, $FE, $FE, $FE, $00
	; dc.b	$FF, $FF, $FF, $FF, $0F, $0F, $0F, $0F
	; dc.b	$F0, $F0, $F0, $F0, $FF, $FF, $FF, $FF
	; dc.b	$F7, $F7, $F7, $F7, $07, $FF, $FF, $FF
	; dc.b	$0F, $0F, $0F, $0F, $FF, $FF, $FF, $FF
	; dc.b	$0F, $0F, $0F, $0F, $F0, $F0, $F0, $F0
