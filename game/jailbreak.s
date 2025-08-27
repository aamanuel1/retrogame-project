; jailbreak.s Jailbreak game source code

	processor 6502
	
	include "constants.h"
	include "addresses.h"
	include "objects.h"

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
	jsr draw_screen
        ldy #$00

colour_screen_loop:			;TODO: turn into subroutine?
	lda #$09
	sta (COLOUR_PTR_LO),Y
	inc COLOUR_PTR_LO
	bne skip_inc_screen_colour
        inc COLOUR_PTR_HI
skip_inc_screen_colour:
        lda COLOUR_PTR_HI
        cmp #$98
        beq main
        jmp colour_screen_loop

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
	lda #<level_0
	sta LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	; sta draw_level_loop+1		;smod code example for changing the level.
	lda #>level_0
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
        jsr poll_input		;TODO investigate if we should put a draw all characters here.
	lda PLAYER_ALIVE		;to stop objects in game from "flickering"
	bne player_still_alive
	jmp game_over
player_still_alive:
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

draw_level:				;DONE reformat draw_level to use screen decompressor
        jsr load_screen_memory
	ldy #$00
        ldx #$00
	jsr draw_screen
	jsr find_enemies
	rts
;draw_level_loop:
;	lda level_1,Y
;	sta (SCR_PTR_LO,X)
;        ; cmp #$37			;Note this will stop working if not facing up.
;        ; beq player_found
;	cmp #$3C
;	beq enemy_found
;	jmp inc_level_draw
;enemy_found:				;DONE have to turn this into something that scans the decompressed screen
;	jsr store_enemy
;; 	jmp inc_level_draw
;; player_found:
;;         jsr store_player
;inc_level_draw:
;        iny
;	inc SCR_PTR_LO
;	bne draw_level_loop
;
;	inc SCR_PTR_HI
;        iny
;draw_level_loop_lower:
;	lda level_1+$FF,Y
;	sta (SCR_PTR_LO,X)
;					;This section used to have player detection code not needed anymore
;        iny
;        inc SCR_PTR_LO
;	bne draw_level_loop_lower
;        rts

find_enemies:
	ldy #$00
	jsr load_screen_memory
find_enemies_loop:
	lda (SCR_PTR_LO),Y
	cmp #$3C			;Enemy facing up = $3C
	bne enemy_not_found
	jsr store_enemy
enemy_not_found:
	inc SCR_PTR_LO
	bne skip_inc_find_enemy
        inc SCR_PTR_HI
skip_inc_find_enemy:
        lda SCR_PTR_HI
        cmp #$20
        bne find_enemies_loop
	rts

store_player:
        lda SCR_PTR_LO
        sta PLAYER_ADDR_LO
        lda SCR_PTR_HI
        sta PLAYER_ADDR_HI
        rts

store_enemy:
	ldx num_enemies			;There's no protection against index overrun fyi.
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
	cpx #00
	beq exit_west
	cpx #20
	beq exit_east
	cpy #01
	beq exit_north
	cpy #21
	beq exit_south
exit_west:			;TODO movement west and east at portals still off.
	lda #$1E
	sta PLAYER_ADDR_HI
	lda #$15
	sta PLAYER_ADDR_LO
	ldx PLAYER_Y
exit_west_loop:
	clc
	adc #22
	bcc exit_west_continue
	inc PLAYER_ADDR_HI
exit_west_continue:
	sta PLAYER_ADDR_LO
	dex
	stx PLAYER_Y
	cpx #0
	bpl exit_west_loop

 	lda #06
 	jmp determine_next_level

exit_east:
	lda #$1E
	sta PLAYER_ADDR_HI
	lda #$01
	sta PLAYER_ADDR_LO
	ldx PLAYER_Y
exit_east_loop:
	clc
	adc #22
	bcc exit_east_continue
	inc PLAYER_ADDR_HI
exit_east_continue:
	sta PLAYER_ADDR_LO
	dex
	stx PLAYER_Y
	cpx #0
	bpl exit_east_loop
 	lda #02
 	jmp determine_next_level

exit_north:
	lda #$1F
	sta PLAYER_ADDR_HI
	lda #$CE
	clc
	adc PLAYER_X
	sta PLAYER_ADDR_LO

	lda #00
	jmp determine_next_level
exit_south:
	lda #$1E
	sta PLAYER_ADDR_HI
	lda #$00
	clc
	adc PLAYER_X
	sta PLAYER_ADDR_LO
	lda #04

determine_next_level:		;DONE generalize level traversal, used smod
	sta LEVEL_OFFSET		;TODO something isn't working here. What is it?
	tax
	lda LEVEL_ADDR_LO
	sta next_level_marker+1
	sta next_level_marker+9
	lda LEVEL_ADDR_HI
	sta next_level_marker+2
	sta next_level_marker+10
next_level_marker:
	lda level_0,X
	sta LEVEL_ADDR_LO
	sta LEVEL_LOWERADDR_LO
	inx
	lda level_0,X
	sta LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_HI
	;jmp load_level
;	cmp #<level_1
;	bne level_2_exit
;level_1_exit:
;	lda level_1,X
;	sta LEVEL_ADDR_LO
;	sta LEVEL_LOWERADDR_LO
;	inx
;	lda level_1,X
;	sta LEVEL_ADDR_HI
;	sta LEVEL_LOWERADDR_HI
;	; jsr level_loweraddr_store
;	jmp load_level
;level_2_exit:
;	lda LEVEL_OFFSET
;	bne level_2_south
;	lda level_2,X
;	sta LEVEL_ADDR_LO
;	sta LEVEL_LOWERADDR_LO
;	inx
;	lda level_2,X
;	sta LEVEL_ADDR_HI
;	sta LEVEL_LOWERADDR_HI
;	; jsr level_loweraddr_store
;	jmp load_level
;level_2_south:
;	lda level_2,X
;	sta LEVEL_ADDR_LO
;	sta LEVEL_LOWERADDR_LO
;	inx
;	lda level_2,X
;	sta LEVEL_ADDR_HI
;	sta LEVEL_LOWERADDR_HI
	; jsr level_loweraddr_store	;Something is happening here that breaks it if you jsr it for some reason

load_level:				;DONE change location of self modifying code (smod) from draw_level_loop
	lda LEVEL_ADDR_LO
	clc
	adc #8
	bcc lvl_addr_lo_normal
	inc LEVEL_ADDR_HI
lvl_addr_lo_normal:
	sta LEVEL_LOWERADDR_LO
	sta decompress_screen_loop+1		;smod code example for changing the level.
	sta decompress_screen_loop+6
	lda LEVEL_ADDR_HI
	sta LEVEL_LOWERADDR_HI
	sta decompress_screen_loop+2
	sta decompress_screen_loop+7

	lda LEVEL_LOWERADDR_LO
	sec
	sbc #8
	bcs lvl_addr_reset_normal
	dec LEVEL_ADDR_HI
lvl_addr_reset_normal:
	sta LEVEL_LOWERADDR_LO


;	lda LEVEL_LOWERADDR_LO
;	clc
;	adc #$FF
;	sta LEVEL_LOWERADDR_LO
;	bcc skip_inc_level_addr_hi
;	inc LEVEL_LOWERADDR_HI
;	lda LEVEL_LOWERADDR_LO
;skip_inc_level_addr_hi:				;Note this part doesn't have a counterpart in draw_screen. 
;	sta draw_level_loop_lower+1		;TODO investigate
;	lda LEVEL_LOWERADDR_HI
;	sta draw_level_loop_lower+2
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
	jsr check_collision_status	;Replaced common code with subroutine.
	beq draw_left			;If it's 0 then keep going
	inc SCR_PTR_LO			;If it's 1 then reset everything.
	; jmp draw_left			;Draw it not moving. NOTE looks like unnecessary jump.
draw_left:
	jsr draw_sprite			;Common draw sprite code replaced with subroutine.
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
	jsr check_collision_status	;Common collision check replaced with subroutine.
	beq draw_right
	dec SCR_PTR_LO
	; jmp draw_right
draw_right:
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
	jsr check_collision_status	;Collision status code replaced with subroutine
	beq draw_up			;If false, continue drawing
	lda SCR_PTR_LO			;If true then reset everything by adding it back or incrementing it back
	clc
	adc #22
        bcc skip_inc_up
        inc SCR_PTR_HI
skip_inc_up:
	sta SCR_PTR_LO
	; jmp draw_up			;Draw it not moving
draw_up:
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
	jsr check_collision_status
	beq draw_down
	lda SCR_PTR_LO
	sec
	sbc #22
        bcs skip_dec_down
        dec SCR_PTR_HI
skip_dec_down:
	sta SCR_PTR_LO
draw_down:
	jsr draw_sprite			;Replaced a jmp and common code with subr.
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
	jsr screen_to_bullet
	inc numbullet
        rts

update_bullet:
	ldx #0

update_bullet_loop:
	cpx #MAX_BULLET
	beq update_bullet_end
	stx COUNTER
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
	; bpl collide			;Might need this bounding box.
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
	lda #<title_chr
	sta decompress_screen_loop+1	;self modify decompress screen loop
	sta decompress_screen_loop+6	;to title screen
	lda #>title_chr
	sta decompress_screen_loop+2
	sta decompress_screen_loop+7
	jmp init

clear_zero_pg:
	ldy #$00			;Will clear all objects from zero pg memory
	lda #$00			;Because phantom bullets were being drawn
clear_zero_pg_loop:
	sta $00,Y
	iny
	cpy #$8F
	bne clear_zero_pg_loop
	rts

draw_screen:
	; Load and store starting screen memory locations to draw title
        jsr load_screen_memory
	ldy #$00
	
decompress_screen_loop:
	lda title_chr,Y			;May not actually be title_chr, possible smod
	tax
	iny

	lda title_chr,Y			;smod location
	sta TEMP_BYTE			;NOTE I forgot to write to bottom half? Investigate
	iny

write_to_screen_loop:
	tya
	pha
	ldy #$00
	lda TEMP_BYTE
	sta (SCR_PTR_LO),Y
	pla
	tay
	inc SCR_PTR_LO
	bne skip_inc_hi_screen
	inc SCR_PTR_HI

skip_inc_hi_screen:
	dex
	bne write_to_screen_loop

	lda SCR_PTR_HI
	cmp #$1F
	bmi decompress_screen_loop
	beq decompress_screen_loop
	lda SCR_PTR_LO
	cmp #$FF
	bmi decompress_screen_loop
						;This section contained commented uncompressed code for some reason.
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

;DONE change this to an include to levels file.

	include "levels.h"
; level_0:
; 	dc.b	<level_1, >level_1
; 	dc.b	$00, $00
; 	dc.b	$00, $00
; 	dc.b	$00, $00
;    	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $2F, $2F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $3F, $3F, $3F, $3F, $3F, $3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F

; level_1:
; 	dc.b	<level_1, >level_1
; 	dc.b	$00, $00
; 	dc.b	<level_0, >level_0
; 	dc.b	$00, $00
;    	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $2F, $2F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $3C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3C, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $3F, $3F, $3F, $3F, $3F, $3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $3F
; 	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $2F, $2F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F

;level_1_adj:
;
;level_2_adj:

	org $1C00
	
; Character bitmap definitions only 63 chars, reverse mode set to retain the base alphanum set because the vic chip loops around
	include "characters.h"

