; jailbreak.s Jailbreak game source code

	processor 6502

;CONSTANTS
MAX_BULLET = 16
MAX_ENEMIES = 8
TRUE = 1
FALSE = 0

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

;OBJECTS
	SEG.U enemies
	ORG $0D
num_enemies ds 1
enemy_x ds 8
enemy_y ds 8
enemy_sprite ds 8
enemy_alive ds 8
enemy_dir ds 8
enemyaddr ds 2

	SEG.U bullets
	ORG $38
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
	

main:
	jmp main

videosettings:
	dc.b %00001100			;9000 $05 Interlace off, screen origin horiz 12  (def 5, lower value left
	dc.b %00011110			;9001 $19 Screen origin vertical 5, lower value up by 2pix to 1
	dc.b %10010110			;9002 $96 bit 7 Screen mem location, rest columns (current default 22 max 27)
	dc.b %10101110			;9003 $AE bit 7 is screen raster, bit 6-1  rows (default 23 max 23)
					;bit 0 is character size 0 8x8 chars, 1 8x16 chars
	dc.b %01111010			;9004 $7A 122 TV raster beam line
	dc.b %11111110			;9005 $F0 bits 4-7 video address, bit 0-3 character memory loc (changed 0000 to 1110).
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


