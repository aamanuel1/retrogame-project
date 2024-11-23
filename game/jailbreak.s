; jailbreak.s Jailbreak game source code

	processor 6502

;CONSTANTS
MAX_BULLET = 16
MAX_ENEMIES = 8
TRUE = 1
FALSE = 0

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

;IMPORTANT ZERO PG ADDRESSESS
CURKEY = $c5
CLOCK = $a2

;MISC ADDRESSES
SCRMEM = $1e00
COLMEM = $9600
PLRSTRT = $1ee6
PLRCOLR = $96e6

;VRAM POINTERS
SCR_PTR_LO = $01
SCR_PTR_HI = $02
COLOUR_PTR_LO = $03
COLOUR_PTR_HI = $04

;PLAYER ATTRIBUTES
PLAYER_X = $05
PLAYER_Y = $06
PLAYER_DIR = $07
PLAYER_SPRITE = $08
PLAYER_LIVES = $09
PLAYER_ALIVE = $0A
PLAYER_ADDR_LO = $0B
PLAYER_ADDR_HI = $0C
PLAYER_COLOUR_LO = $0D
PLAYER_COLOUR_HI = $0E

;OBJECTS
	SEG.U enemies
	ORG $0F
num_enemies ds 1
enemy_x ds 8
enemy_y ds 8
enemy_sprite ds 8
enemy_alive ds 8
enemy_dir ds 8
enemyaddr ds 2

	SEG.U bullets
	ORG $3A
num_bullet ds 1
bullet_sprite ds 8
bullet_dir ds 8
bullet_x ds 8
bullet_y ds 8
bullet_low ds 8
bullet_high ds 8
bullet_colour ds 8
bulletaddr ds 2
	SEG

CUR_LEVEL = $72

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
        lda #$20
        sta (SCR_PTR_LO),Y
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
        cmp #$97
        bpl main
        jmp colour_title_loop
main:
        lda CURKEY
        cmp #ENTER
        beq game_start
	jmp main

game_start:
        jsr draw_level
game_loop:
        jsr poll_input
	jmp game_loop			;End of "game" loop

draw_level:
        jsr load_screen_memory
	ldy #$00
        ldx #$00
draw_level_loop:
	lda level_1,Y
	sta (SCR_PTR_LO,X)
        cmp #$37
        bne skip_store_player
        jsr store_player
skip_store_player:
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
        sta PLAYER_ADDR_HI
        rts

store_enemy:

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

load_screen_location:
        sta SCR_PTR_LO
        sty SCR_PTR_HI
        rts

poll_input:
	lda CURKEY			;CURKEY is $c5 in zero page
	cmp #NO_KEY			;$40 is 64 dec, see pg 179 of VIC20 ref
	beq game_loop			;64 is no button pressed
	cmp #UP_BUTTON			;$09 is 9 dec, W button
	bne poll_left 
	jsr move_up
        jmp end_poll
poll_left:				
	cmp #LEFT_BUTTON			;$11 is 17 dec, A button
	bne poll_down
	jsr move_left
        jmp end_poll
poll_down:	
	cmp #DOWN_BUTTON			;$29 is 41 dec, S button
	bne poll_right
	jsr move_down
        jmp end_poll
poll_right:	
	cmp #RIGHT_BUTTON			;$12 is 18 dec, D button
	bne poll_shoot
	jsr move_right
        jmp end_poll
poll_shoot:
	cmp #ENTER			;$0F is 15 dec, return button
	bne end_poll 
	jsr shoot
end_poll:
        rts

move_left:
        ldx #$00
        lda PLAYER_ADDR_LO
        ldy PLAYER_ADDR_HI
        jsr load_screen_location
	lda #32				;Clear current location with SPACE
	sta (SCR_PTR_LO,X)
	lda PLAYER_ADDR_LO		;Load current location for operations
	sec				;DON'T forget to set this.
	sbc #1				;subtract 1 to move left
	sta PLAYER_ADDR_LO	        ;Store in low byte of screen map
	; lda $05			;Load current colour map location
	; sec
	; sbc #1			;Subtract by 1 to colour left tile
	; sta $05			;Store in low byte of screen map
	bcs check_left			;Do a check of the location if didn't roll over
	lda PLAYER_ADDR_HI		;If we did roll over (carry clear)
	cmp #$1E			;Do a check if we're in $1F
	beq check_left			;If we're not then continue the ckeck
	dec PLAYER_ADDR_HI				;Otherwise, turn $1F to $1E
	; dec $06				;turn $97 to $96
check_left:
        ldy #$00
        lda (PLAYER_ADDR_LO),Y
	jsr global_collision		;Check if it's bumping against edge of map
	beq draw_left			;If it's 0 then keep going
	inc PLAYER_ADDR_LO				;If it's 1 then reset everything.
	; inc $05
	; inc PLRX			;Reset the X counter
	jmp draw_left			;Draw it not moving.
draw_left:
        lda PLAYER_ADDR_LO
        ldy PLAYER_ADDR_HI
        jsr load_screen_location
	lda #$37				;Draw the guy
	sta (SCR_PTR_LO,X)
	; lda #2				;That is red.
	; sta ($05,X)
end_move_left:
        lda #60
        jsr delay
	rts

move_right:
        rts

move_up:
        rts

move_down:
        rts

shoot:
        rts


global_collision:
        ; ldx #$00
        ; tay
        ; lda (Y,X)
	cmp #$3F
	beq collide			;X < 0 we collided with left edge
	; cmp #22
	; bpl collide			;X > 22 we collided with right edge
	; ;lda PLRY			;Load player Y position
	; cpy #0	
	; bmi collide			;Y < 0 we collided with top edge
	; cpy #23
	; bpl collide			;Y > 23 we collided with bottom edge
	lda #0				;Return false
	jmp ret_global_collision
collide:
	lda #1				;Return true
	jmp ret_global_collision
ret_global_collision:
	rts				;Return result.

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
	dc.b %11111110			;9005 $F0 bits 4-7 video address, bit 0-3 character memory loc (changed 0000 to 1111 $1c00).
	dc.b %0				;9006 $0 Horizontal light pen location
	dc.b %0				;9007 $0 Vertical light pen location
	dc.b %11111111			;9008 $FF Paddle X Digitized variable resistance
	dc.b %11111111			;9009 $FF Paddle Y Digitized variable resistance
	dc.b %0				;900A $0 Bass oscillator
	dc.b %0				;900B $0 Alto oscillator
	dc.b %0				;900C $0 Soprano oscillator
	dc.b %0				;900D $0 Noise source
	dc.b %00010000			;900E $0 Bit 4-7 auxilary colour, bit 0-3 sound loudness
	dc.b %00001110			;900F Screen colour (black) border colour dark blue

; screen character data replace with compression later.
title_chr:
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $70, $20, $20, $20
	dc.b	$20, $20, $20, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A, $5B, $5C, $5D, $5E, $5F, $71, $20, $20, $20
	dc.b	$20, $20, $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6A, $6B, $6C, $6D, $6E, $6F, $72, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $02, $19, $20, $01, $01, $12, $0F, $0E, $20, $0E, $01, $0E, $15, $05, $0C, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $10, $12, $05, $13, $13, $20, $05, $0E, $14, $05, $12, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20

; screen color data
; title_colour:
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $09, $09, $0E, $09, $09, $09, $09, $09, $0E, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $09, $09, $09, $09, $09, $0E, $09, $09, $09, $09, $09, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $09, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
; 	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

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

	org $1800
; Character bitmap definitions only 128 chrs, 63 left to 1dff.
	dc.b	$1C, $22, $4A, $56, $4C, $20, $1E, $00
	dc.b	$18, $24, $42, $7E, $42, $42, $42, $00
	dc.b	$7C, $22, $22, $3C, $22, $22, $7C, $00
	dc.b	$1C, $22, $40, $40, $40, $22, $1C, $00
	dc.b	$78, $24, $22, $22, $22, $24, $78, $00
	dc.b	$7E, $40, $40, $78, $40, $40, $7E, $00
	dc.b	$7E, $40, $40, $78, $40, $40, $40, $00
	dc.b	$1C, $22, $40, $4E, $42, $22, $1C, $00
	dc.b	$42, $42, $42, $7E, $42, $42, $42, $00
	dc.b	$1C, $08, $08, $08, $08, $08, $1C, $00
	dc.b	$0E, $04, $04, $04, $04, $44, $38, $00
	dc.b	$42, $44, $48, $70, $48, $44, $42, $00
	dc.b	$40, $40, $40, $40, $40, $40, $7E, $00
	dc.b	$42, $66, $5A, $5A, $42, $42, $42, $00
	dc.b	$42, $62, $52, $4A, $46, $42, $42, $00
	dc.b	$18, $24, $42, $42, $42, $24, $18, $00
	dc.b	$7C, $42, $42, $7C, $40, $40, $40, $00
	dc.b	$18, $24, $42, $42, $4A, $24, $1A, $00
	dc.b	$7C, $42, $42, $7C, $48, $44, $42, $00
	dc.b	$3C, $42, $40, $3C, $02, $42, $3C, $00
	dc.b	$3E, $08, $08, $08, $08, $08, $08, $00
	dc.b	$42, $42, $42, $42, $42, $42, $3C, $00
	dc.b	$42, $42, $42, $24, $24, $18, $18, $00
	dc.b	$42, $42, $42, $5A, $5A, $66, $42, $00
	dc.b	$42, $42, $24, $18, $24, $42, $42, $00
	dc.b	$22, $22, $22, $1C, $08, $08, $08, $00
	dc.b	$7E, $02, $04, $18, $20, $40, $7E, $00
	dc.b	$00, $08, $08, $3E, $08, $08, $00, $00
	dc.b	$00, $3C, $7E, $7E, $7E, $7E, $3C, $00
	dc.b	$81, $42, $24, $18, $18, $24, $42, $81
	dc.b	$00, $3C, $42, $42, $42, $42, $3C, $00
	dc.b	$08, $1C, $2A, $77, $2A, $08, $08, $00
	dc.b	$00, $00, $00, $00, $00, $00, $00, $00
	dc.b	$3C, $42, $46, $5A, $62, $42, $3C, $00
	dc.b	$08, $18, $28, $08, $08, $08, $3E, $00
	dc.b	$3C, $42, $02, $0C, $30, $40, $7E, $00
	dc.b	$3C, $42, $02, $1C, $02, $42, $3C, $00
	dc.b	$04, $0C, $14, $24, $7E, $04, $04, $00
	dc.b	$7E, $40, $78, $04, $02, $44, $38, $00
	dc.b	$1C, $20, $40, $7C, $42, $42, $3C, $00
	dc.b	$7E, $42, $04, $08, $10, $10, $10, $00
	dc.b	$3C, $42, $42, $3C, $42, $42, $3C, $00
	dc.b	$3C, $42, $42, $3E, $02, $04, $38, $00
	dc.b	$00, $00, $00, $00, $FF, $00, $00, $00
	dc.b	$00, $00, $00, $00, $F0, $00, $00, $00
	dc.b	$00, $00, $00, $00, $0A, $00, $00, $00
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$30, $30, $30, $30, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $30, $30, $30, $30
	dc.b	$C0, $C0, $30, $20, $0C, $08, $02, $02
	dc.b	$C0, $C0, $30, $20, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $0C, $08, $02, $02
	dc.b	$02, $02, $08, $0C, $20, $30, $C0, $C0
	dc.b	$02, $02, $08, $0C, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $20, $30, $C0, $C0
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
	dc.b	$3C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	dc.b	$00, $05, $05, $05, $05, $05, $14, $14
	dc.b	$0F, $5F, $5F, $5F, $5F, $5F, $1F, $1F
	dc.b	$00, $14, $14, $14, $14, $14, $14, $14
	dc.b	$03, $17, $17, $17, $17, $17, $17, $17
	dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	dc.b	$00, $55, $55, $50, $50, $50, $50, $50
	dc.b	$0F, $4F, $5F, $5F, $1F, $1F, $1F, $1F
	dc.b	$00, $55, $55, $50, $50, $50, $50, $50
	dc.b	$03, $43, $53, $57, $17, $17, $17, $17
	dc.b	$C0, $D5, $D5, $D4, $D4, $D4, $D4, $D4
	dc.b	$00, $54, $54, $00, $00, $00, $00, $00
	dc.b	$F0, $F5, $F5, $F5, $F5, $F5, $F4, $F4
	dc.b	$00, $50, $50, $50, $50, $50, $14, $14
	dc.b	$03, $17, $17, $17, $17, $17, $17, $17
	dc.b	$00, $00, $00, $00, $00, $00, $00, $00
	dc.b	$7C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	dc.b	$14, $14, $14, $15, $15, $15, $14, $14
	dc.b	$1F, $1F, $1F, $5F, $5F, $5F, $1F, $1F
	dc.b	$14, $14, $14, $14, $14, $14, $14, $14
	dc.b	$17, $17, $17, $17, $17, $17, $17, $17
	dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	dc.b	$50, $50, $50, $55, $55, $50, $50, $50
	dc.b	$1F, $1F, $1F, $5F, $5F, $1F, $1F, $1F
	dc.b	$50, $50, $50, $55, $55, $54, $50, $50
	dc.b	$17, $17, $17, $57, $57, $57, $17, $17
	dc.b	$D4, $D4, $D4, $D5, $D5, $D4, $D4, $D4
	dc.b	$00, $00, $00, $50, $50, $00, $00, $00
	dc.b	$F4, $F4, $D4, $D5, $15, $15, $14, $14
	dc.b	$14, $14, $14, $54, $54, $54, $14, $14
	dc.b	$17, $17, $15, $15, $15, $15, $17, $17
	dc.b	$00, $15, $15, $15, $15, $15, $00, $00
	dc.b	$7C, $7C, $7C, $7C, $7C, $3C, $3C, $3C
	dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	dc.b	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $0F
	dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	dc.b	$17, $17, $17, $17, $17, $17, $17, $03
	dc.b	$C0, $C0, $C0, $C0, $D4, $D4, $D4, $C0
	dc.b	$50, $50, $50, $50, $50, $55, $55, $00
	dc.b	$1F, $1F, $1F, $1F, $5F, $5F, $4F, $0F
	dc.b	$50, $50, $50, $50, $50, $50, $50, $00
	dc.b	$17, $17, $17, $17, $17, $17, $07, $03
	dc.b	$D4, $D4, $D4, $D4, $D4, $D5, $D5, $C0
	dc.b	$00, $00, $00, $00, $00, $54, $54, $00
	dc.b	$14, $34, $F4, $F4, $F4, $F4, $F4, $F0
	dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	dc.b	$17, $17, $17, $17, $17, $17, $17, $03
	dc.b	$C0, $C5, $C5, $D5, $D4, $D4, $50, $40
	dc.b	$40, $40, $40, $40, $40, $C0, $C0, $C0
	dc.b	$C0, $D0, $D4, $D4, $D5, $C5, $C5, $C0
	dc.b	$08, $08, $08, $08, $F8, $08, $08, $08
	dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	dc.b	$E0, $E0, $E0, $E0, $E0, $E0, $E0, $E0
	dc.b	$07, $07, $07, $07, $07, $07, $07, $07
	dc.b	$FF, $FF, $00, $00, $00, $00, $00, $00
	dc.b	$FF, $FF, $FF, $00, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $00, $FF, $FF, $FF
	dc.b	$01, $01, $01, $01, $01, $01, $01, $FF
	dc.b	$00, $00, $00, $00, $F0, $F0, $F0, $F0
	dc.b	$0F, $0F, $0F, $0F, $00, $00, $00, $00
	dc.b	$08, $08, $08, $08, $F8, $00, $00, $00
	dc.b	$F0, $F0, $F0, $F0, $00, $00, $00, $00
	dc.b	$F0, $F0, $F0, $F0, $0F, $0F, $0F, $0F
	dc.b	$40, $40, $40, $44, $40, $C0, $C0, $C0