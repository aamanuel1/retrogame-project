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
SPACE = $20

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
; PLAYER_COLOUR_LO = $0C
; PLAYER_COLOUR_HI = $0D

;OBJECTS
	SEG.U enemies
	ORG $0C
num_enemies ds 1
enemy_sprite ds 8
enemy_dir ds 8
enemy_x ds 8
enemy_y ds 8
enemy_low ds 8
enemy_high ds 8
enemy_alive ds 8

	SEG.U bullets
	ORG $45
numbullet ds 1
bullet_sprite ds 8
bullet_dir ds 8
bullet_x ds 8
bullet_y ds 8
bullet_low ds 8
bullet_high ds 8
; bullet_colour ds 8			;To delete if needed
bullet_collide ds 8
	SEG

CUR_LEVEL = $76
CUR_SPRITE = $77
OBJECT_DIR = $78
COUNTER = $79
COLLISION_STATUS = $7A
SCRATCH_LO = $7B
SCRATCH_HI = $7C
REMAINDER = $7D
ENEMY_SHOOT_TIMER = $7E
ENEMY_MOVE_TIMER = $7F
ENEMY_CYCLE_CTR = $80
ENEMY_DIFF_X = $81
ENEMY_DIFF_Y = $82
ENEMY_COUNTER = $83
COLL_PTR_LO = $84
COLL_PTR_HI = $85
LEVEL_CHANGE = $86
LEVEL_OFFSET = $87
LEVEL_ADDR_LO = $88
LEVEL_ADDR_HI = $89
LEVEL_LOWERADDR_LO = $8A
LEVEL_LOWERADDR_HI = $8B
TEMP_BYTE = $8C

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
	; jsr clearscreen
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
	
decompress_title_loop:
	lda title_chr,Y
	tax
	iny

	lda title_chr,Y
	sta TEMP_BYTE
	iny

write_title_loop:
	tya
	pha
	ldy #$00
	lda TEMP_BYTE
	sta (SCR_PTR_LO),Y
	pla
	tay
	inc SCR_PTR_LO
	bne skip_inc_hi_title
	inc SCR_PTR_HI

skip_inc_hi_title:
	dex
	bne write_title_loop

	lda SCR_PTR_HI
	cmp #$1F
	bmi decompress_title_loop
	beq decompress_title_loop
	lda SCR_PTR_LO
	cmp #$FF
	bmi decompress_title_loop
; 	iny
; 	bne draw_title_loop

; 	inc SCR_PTR_HI
; draw_title_loop_lower:
;         ; lda #$20
;         ; sta (SCR_PTR_LO),Y
; 	lda title_chr+$FF,Y
; 	sta (SCR_PTR_LO),Y
; 	iny
; 	bne draw_title_loop_lower

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
	lda #<level_1
	sta LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	; sta draw_level_loop+1		;smod code example for changing the level.
	lda #>level_1
	sta LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_HI
	; sta draw_level_loop+2
	; jsr level_offset_store
	jsr load_level
        ; jsr draw_level

	lda #$D9			;Starting point for the player (X=11 Y=21)
	sta PLAYER_ADDR_LO
	sta SCR_PTR_LO
	lda #$1F
	sta PLAYER_ADDR_HI
	sta SCR_PTR_HI
	lda #$37
	ldy #$00
	sta CUR_SPRITE
	sta (SCR_PTR_LO),Y

	lda #$05
	sta ENEMY_MOVE_TIMER
	lda #$08
	sta ENEMY_SHOOT_TIMER
	lda #TRUE
	sta PLAYER_ALIVE
	lda #FALSE
	sta LEVEL_CHANGE
game_loop:
        jsr poll_input
	lda PLAYER_ALIVE
	bne player_still_alive
	jmp game_over
player_still_alive
	lda LEVEL_CHANGE
	beq continue_game_loop
	jsr change_level		;We'll have to go to game loop so no jsr
	jmp game_loop			;Start from scratch.
continue_game_loop:
	jsr enemy_hunt_player
	jsr enemy_shoot
	jsr update_bullet
	jsr remove_dead_enemies
	jsr enemy_update_timer
	lda #8
        jsr delay
	jmp game_loop			;End of "game" loop

draw_level:
        jsr load_screen_memory
	ldy #$00
        ldx #$00
draw_level_loop:
	lda level_1,Y
	sta (SCR_PTR_LO,X)
        ; cmp #$37			;Note this will stop working if not facing up.
        ; beq player_found
	cmp #$3C
	beq enemy_found
	jmp inc_level_draw
enemy_found:
	jsr store_enemy
; 	jmp inc_level_draw
; player_found:
;         jsr store_player
inc_level_draw:
        iny
	inc SCR_PTR_LO
	bne draw_level_loop

	inc SCR_PTR_HI
        iny
draw_level_loop_lower:
	lda level_1+$FF,Y
	sta (SCR_PTR_LO,X)
        ; cmp #$37
        ; bne skip_store_player_2
        ; jsr store_player
; skip_store_player_2:
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
	; lda SCR_PTR_LO
	; sta enemy_low,X
	; lda SCR_PTR_HI
	; sta enemy_high,X
	jsr screen_to_enemy
	lda #TRUE
	sta enemy_alive,X
	inc num_enemies
	ldx #$00			;Set to 0 so the indexed indirect works outside of function.
	rts

screen_to_enemy:
	lda SCR_PTR_LO
	sta enemy_low,X
	lda SCR_PTR_HI
	sta enemy_high,X
	rts

enemy_to_screen:
	lda enemy_low,X
	sta SCR_PTR_LO
	lda enemy_high,X
	sta SCR_PTR_HI
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
	jsr check_player_status
end_poll_shoot:
        rts

check_player_status:			;TODO combine with end_poll if it works to save a jsr and rts.
	lda COLLISION_STATUS
	cmp #5
	beq level_transition
	cmp #2
	bpl kill_player_flag
	jmp check_player_status_ret
kill_player_flag:
	lda #FALSE
	sta PLAYER_ALIVE
level_transition:
	lda #TRUE
	sta LEVEL_CHANGE
check_player_status_ret:
	rts

change_level:
	jsr player_to_screen
	jsr screen_to_xy
	stx PLAYER_X
	sty PLAYER_Y
	; cpx #00
	; beq exit_west
	; cpx #22
	; beq exit_east
	cpy #01
	beq exit_north
	cpy #21
	beq exit_south
; exit_west:
; 	lda #03
; 	jmp load_level
; exit_east:
; 	lda #01
; 	jmp load_level
exit_north:
	lda #$1F
	sta PLAYER_ADDR_HI
	lda #$CE
	; sta PLAYER_ADDR_LO
	clc
	adc PLAYER_X
	sta PLAYER_ADDR_LO

	lda #00
	jmp determine_level_adj
exit_south:
	lda #$1E
	sta PLAYER_ADDR_HI
	lda #$00
	; sta PLAYER_ADDR_LO
	clc
	adc PLAYER_X
	sta PLAYER_ADDR_LO

	lda #02
determine_level_adj:
	sta LEVEL_OFFSET
	tax
	lda LEVEL_ADDR_LO
	cmp #<level_1
	bne level_2_exit
level_1_exit:
	lda level_1_adj,X
	sta LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	inx
	lda level_1_adj,X
	sta LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_HI
	; jsr level_loweraddr_store
	jmp load_level
level_2_exit:
	lda LEVEL_OFFSET
	bne level_2_south
	lda level_2_adj,X
	sta LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	inx
	lda level_2_adj,X
	sta LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_HI
	; jsr level_loweraddr_store
	jmp load_level
level_2_south:
	lda level_2_adj,X
	sta LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	inx
	lda level_2_adj,X
	sta LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_HI
	; jsr level_loweraddr_store	;Something is happening here that breaks it if you jsr it for some reason

load_level:
	lda LEVEL_ADDR_LO
	sta draw_level_loop+1		;smod code example for changing the level.
	lda LEVEL_ADDR_HI
	sta draw_level_loop+2

	lda LEVEL_LOWERADDR_LO
	clc
	adc #$FF
	sta LEVEL_LOWERADDR_LO
	bcc skip_inc_level_addr_hi
	inc LEVEL_LOWERADDR_HI
	lda LEVEL_LOWERADDR_LO
skip_inc_level_addr_hi:
	sta draw_level_loop_lower+1
	lda LEVEL_LOWERADDR_HI
	sta draw_level_loop_lower+2
        jsr draw_level
	lda #FALSE
	sta LEVEL_CHANGE
	rts

level_loweraddr_store:
	lda LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	lda LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_LO
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
        ; ldy #$00
        ; lda (SCR_PTR_LO),Y
	; jsr global_collision		;Check if it's bumping against edge of map
	; sta COLLISION_STATUS
	jsr check_collision_status
	beq draw_left			;If it's 0 then keep going
	inc SCR_PTR_LO			;If it's 1 then reset everything.
	jmp draw_left			;Draw it not moving.
draw_left:
        ; ldx #$00                        ;X will have the X value of the object from screen to xy
	; lda CUR_SPRITE			;Draw the guy
	; sta (SCR_PTR_LO,X)
	jsr draw_sprite
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
        ; ldy #$00
        ; lda (SCR_PTR_LO),Y
	; jsr global_collision
	; sta COLLISION_STATUS
	jsr check_collision_status
	beq draw_right
	dec SCR_PTR_LO
	jmp draw_right
draw_right:
        ; ldx #$00
	; lda CUR_SPRITE
	; sta (SCR_PTR_LO,X)
	jsr draw_sprite
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
	; ldy #$00
	; lda (SCR_PTR_LO),y
	; jsr global_collision		;Check if it's collided with the edge
	; sta COLLISION_STATUS
	jsr check_collision_status
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
        ; ldx #$00
	; lda CUR_SPRITE			;Draw a red circle
	; sta (SCR_PTR_LO,X)
	jsr draw_sprite
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
        ; ldy #$00
        ; lda (SCR_PTR_LO),Y
	; jsr global_collision
	; sta COLLISION_STATUS
	jsr check_collision_status
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
        ; ldx #$00
        ; lda CUR_SPRITE
	; sta (SCR_PTR_LO,X)
	jsr draw_sprite
end_move_down:
	rts

check_collision_status:
        ldy #$00
        lda (SCR_PTR_LO),Y
	jsr global_collision
	sta COLLISION_STATUS
	rts

draw_sprite:
        ldx #$00
        lda CUR_SPRITE
	sta (SCR_PTR_LO,X)
	rts

shoot:
	ldx numbullet
	cpx #MAX_BULLET
	bne add_bullet_start
	ldx #$00
	stx numbullet
add_bullet_start:
	lda bullet_dir,X
	cmp #NO_KEY
	beq add_bullet_continue
	cmp #$00
	beq add_bullet_continue
	; lda bullet_low,X
	; sta SCR_PTR_LO
	; lda bullet_high,X
	; sta SCR_PTR_HI
	jsr bullet_to_screen
	jsr remove_bullet_func

add_bullet_continue:
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
	ldx numbullet			;I guess after a jsr I don't know what the X will be
	lda CUR_SPRITE
	sta bullet_sprite,X
	; lda SCR_PTR_LO
	; sta bullet_low,X
	; lda SCR_PTR_HI
	; sta bullet_high,X
	jsr screen_to_bullet
	inc numbullet
        rts

update_bullet:
	ldx #0

update_bullet_loop:
	; cpx numbullet					;Note remove later, was causing the bullet is stuck issue
	; beq update_bullet_end
	; bpl update_bullet_end
	cpx #MAX_BULLET
	beq update_bullet_end
	stx COUNTER

	; lda bullet_low,X
	; sta SCR_PTR_LO

	; lda bullet_high,X
	; sta SCR_PTR_HI
	jsr bullet_to_screen

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
	ldx COUNTER
	; lda SCR_PTR_LO
	; sta bullet_low,X
	; lda SCR_PTR_HI
	; sta bullet_high,X
	jsr screen_to_bullet
	stx COUNTER					;Probably should rename to bullet counter
	jsr check_bullet_status
	jmp update_bullet_loop
update_bullet_end:
	rts

check_bullet_status:
	lda COLLISION_STATUS
	beq inc_bullet_counter
	lda COLL_PTR_LO
	cmp PLAYER_ADDR_LO
	bne enemy_bullet_crosscheck
	lda COLL_PTR_HI
	cmp PLAYER_ADDR_HI
	bne enemy_bullet_crosscheck
	lda #FALSE
	sta PLAYER_ALIVE
	jmp remove_bullet
enemy_bullet_crosscheck:
	ldx #$00
	stx ENEMY_COUNTER 
enemy_bullet_crosscheck_loop:
	cpx #MAX_ENEMIES				;NOTE problem with enemy collision when this was refactored.
	beq remove_bullet				;Above is because I forgot what immediate was.
	lda COLL_PTR_LO
	cmp enemy_low,X
	bne enemy_bullet_crosscheck_inc
	lda COLL_PTR_HI
	cmp enemy_high,X
	bne enemy_bullet_crosscheck_inc
	lda #FALSE
	sta enemy_alive,X
	jmp remove_bullet				;This somehow makes it so the bullet doesn't kill the enemy.
enemy_bullet_crosscheck_inc:
	inx
	stx ENEMY_COUNTER
	ldx ENEMY_COUNTER
	jmp enemy_bullet_crosscheck_loop
remove_bullet:
	ldx COUNTER
	jsr remove_bullet_func				;Refactored remove_bullet that was here to be its own standalone function.
inc_bullet_counter:
	inc COUNTER
	ldx COUNTER
	rts

remove_bullet_func:
	lda #NO_KEY
	sta bullet_dir,X
	lda #32
	ldy #$00
	sta bullet_sprite,X
	; lda bullet_low,X				;Placing the bullet load here was causing weird stuff to happen
	; sta SCR_PTR_LO
	; lda bullet_high,X
	; sta SCR_PTR_HI
	sta (SCR_PTR_LO),Y
	rts

screen_to_bullet:
	lda SCR_PTR_LO
	sta bullet_low,X
	lda SCR_PTR_HI
	sta bullet_high,X
	rts

bullet_to_screen:
	lda bullet_low,X
	sta SCR_PTR_LO
	lda bullet_high,X
	sta SCR_PTR_HI
	rts

enemy_hunt_player:
	lda ENEMY_CYCLE_CTR
	cmp ENEMY_MOVE_TIMER
	beq enemy_hunt_player_continue
	jmp enemy_hunt_player_ret
enemy_hunt_player_continue:
	jsr player_to_screen
	jsr screen_to_xy
	stx PLAYER_X
	sty PLAYER_Y
	ldx #$00
	stx ENEMY_COUNTER
enemy_hunt_player_loop:
	ldx ENEMY_COUNTER
	cpx #MAX_ENEMIES
	beq enemy_hunt_player_ret

	lda enemy_alive,X
	beq skip_enemy_move
	cmp #$01				;For some strange reason zpg doesn't always init to 0 in a seg.u
	bne skip_enemy_move

	; lda enemy_low,X
	; sta SCR_PTR_LO
	; lda enemy_high,X
	; sta SCR_PTR_HI
	jsr enemy_to_screen
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
	ldx ENEMY_COUNTER
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
	jsr check_enemy_status
	ldx ENEMY_COUNTER 
	; lda SCR_PTR_LO
	; sta enemy_low,X
	; lda SCR_PTR_HI
	; sta enemy_high,X
	jsr screen_to_enemy
skip_enemy_move:
	inx
	stx ENEMY_COUNTER
	jmp enemy_hunt_player_loop			;Have to think about how this will work with more than one enemy.
enemy_hunt_player_ret:
	rts

enemy_shoot:
	lda ENEMY_CYCLE_CTR
	cmp ENEMY_SHOOT_TIMER
	bne enemy_shoot_ret

	lda num_enemies
	beq enemy_shoot_ret

	ldx #$00
	stx ENEMY_COUNTER
enemy_shoot_player_loop:
	lda enemy_alive,X
	beq enemy_shoot_ret
	; lda enemy_low,X
	; sta SCR_PTR_LO
	; lda enemy_high,X
	; sta SCR_PTR_HI
	jsr enemy_to_screen
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

check_enemy_status:
	lda COLLISION_STATUS
	cmp #4
	beq kill_enemy_flag
	cmp #3
	beq check_enemy_status_ret
	cmp #2
	beq enemy_kill_player_flag
	jmp check_enemy_status_ret
kill_enemy_flag:
	lda #FALSE
	sta enemy_alive,X		;This should still have the X from the counter. Hopefully.
	jmp check_enemy_status_ret
enemy_kill_player_flag:
	lda #FALSE
	sta PLAYER_ALIVE
check_enemy_status_ret:
	rts

remove_dead_enemies:
	ldx #$00
	stx ENEMY_COUNTER
remove_dead_enemies_loop:
	cpx #MAX_ENEMIES
	beq remove_dead_enemies_ret

	ldy #$00 			;This section needed to remove the dead enemy, need to refactor.
	; lda enemy_low,X
	; sta SCR_PTR_LO
	; lda enemy_high,X
	; sta SCR_PTR_HI
	jsr enemy_to_screen
	lda (SCR_PTR_LO),Y
	cmp #$20
	beq skip_remove_enemy
	cmp #$3B
	bmi skip_remove_enemy
	cmp #$3E
	bpl skip_remove_enemy

	lda enemy_alive,X
	bne skip_remove_enemy
	lda #NO_KEY
	sta enemy_dir,X
	; lda enemy_low,X
	; sta SCR_PTR_LO
	; lda enemy_high,X
	; sta SCR_PTR_HI
	jsr enemy_to_screen
	lda #32
	ldy #$00
	; sta enemy_sprite,X
	sta (SCR_PTR_LO),Y
	; lda #$00
	; sta enemy_low,X
	; sta enemy_high,X
	dec num_enemies			;There might be an issue with out-of-order removal
skip_remove_enemy:
	inx
	stx ENEMY_COUNTER
	jmp remove_dead_enemies_loop

remove_dead_enemies_ret:
	rts

global_collision:
	cmp #$3F			;Compare with wall
	beq collide			;X < 0 we collided with left edge
	cmp #$2F			;Compare with level change trigger
	beq level_change
bullet_collision_check:
	cmp #$33
	bmi enemy_collision_check
	cmp #$34
	bpl enemy_collision_check
	jmp bullet_collided
enemy_collision_check:
	cmp #$3B
	bmi player_collision_check
	cmp #$3E
	bpl player_collision_check
	jmp enemy_collide
player_collision_check:
	cmp #$37
	bmi global_bounds
	cmp #$3A
	bpl global_bounds
	jmp player_collide
global_bounds:
	; jsr screen_to_xy
	; cpx #0
	; bmi collide
	; cpx #22
	; bpl collide
	; cpy #0
	; bmi collide
	; cpy #23
	; bpl collide
	lda #0				;Return false
	jmp ret_global_collision
level_change:
	lda #5
	jmp store_collision_stats
bullet_collided:
	lda #4
	jmp store_collision_stats
enemy_collide:
	lda #3
	jmp store_collision_stats
player_collide:
	lda #2
	jmp store_collision_stats
collide:
	lda #1				;Return true
store_collision_stats:
	pha				;Push the flag onto the stack or you'll be stuck
	lda SCR_PTR_LO
	sta COLL_PTR_LO
	lda SCR_PTR_HI
	sta COLL_PTR_HI
	pla				;Pull it before returning.
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

; clearscreen:
; 	jsr load_screen_memory
; 	ldy #$00
; clearscreen_loop:
; 	lda #SPACE
; 	sta (SCR_PTR_LO),Y
; 	iny
; 	bne clearscreen_loop
; 	lda SCR_PTR_HI
; 	cmp #$1F
; 	beq clearscreen_ret
; 	inc SCR_PTR_HI
; 	jmp clearscreen_loop
; clearscreen_ret:
; 	rts

game_over:
	jsr player_to_screen
	lda #$1F
	ldy #$00
	sta (SCR_PTR_LO),Y
game_over_loop:
	lda CURKEY
	cmp #SPACE
	bne game_over_loop
	; jsr clearscreen
	lda #60
	jsr delay
	jsr clear_zero_pg
	jmp draw_title

clear_zero_pg:
	ldy #$00			;Will clear all objects from zero pg memory
	lda #$00			;Because phantom bullets were being drawn
clear_zero_pg_loop:
	sta $00,Y
	iny
	cpy #$8F
	bne clear_zero_pg_loop
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
	INCBIN "compressed_title.bin"
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $00, $21, $22, $23, $14, $15, $06, $07, $06, $18, $19, $1A, $1B, $1C, $14, $1D, $20, $20, $20
	; dc.b	$20, $20, $20, $10, $02, $03, $04, $24, $15, $16, $17, $16, $28, $29, $2A, $2B, $2C, $0D, $0E, $20, $20, $20
	; dc.b	$20, $20, $01, $11, $12, $13, $12, $05, $25, $26, $27, $08, $09, $0A, $0B, $0C, $12, $05, $1E, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $82, $99, $20, $81, $81, $92, $8F, $8E, $20, $8D, $81, $8E, $95, $85, $8C, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $90, $92, $85, $93, $93, $20, $93, $90, $81, $83, $85, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	; dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20

level_1:
    	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $2F, $2F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
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
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F

level_2:
    	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $2F, $2F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $3C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
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
	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $2F, $2F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F

level_1_adj:
	dc.b	<level_2, >level_2
	; dc.b	$00, $00
	dc.b	$00, $00
	; dc.b	$00, $00
level_2_adj:
	dc.b	<level_2, >level_2
	; dc.b	$00, $00
	dc.b	<level_1, >level_1
	; dc.b	$00, $00
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
	dc.b	$00, $00, $00, $00, $00, $00, $00, $00			;Level transition 47 $2F
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
	; dc.b	$00, $00, $00, $00, $00, $00, $00, $00
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
