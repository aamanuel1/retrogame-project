;piano.s Transcription of the piano program in VIC-20
;programmer's reference manual

	processor 6502

;KERNAL AND IMPORTANT 0PG ADDRESSES

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

;VIC CHIP
BRDR_SCR_COLOUR = $900f
VOLUME = $900e
LOW_SPEAKER = $900a
MID_SPEAKER = $900b
HI_SPEAKER = $900c
NOISE = $900d

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
	lda #$0F			;200 POKE V,15
	sta VOLUME
main_loop:
	jsr SCNKEY			;220 GET A$...
	jsr GETIN
check_digit:
	cmp #0				;...IF A$="" then 220
	beq main_loop			
	cmp #49				;240 IF N=0
	bmi ending_module
	cmp #57				;OR N=9 THEN 300
	bpl ending_module
get_key:
	sec				;230 N=VAL(A$)
	sbc #49
play_keyboard:
	tay				;Transfer N to Y for absolute indexing
	lda #0				;250 POKE S2,0
	sta MID_SPEAKER
	lda #25				;260 FOR T=1 TO 25: NEXT T
	jsr delay
	lda b_major_scale,Y		;270 POKE S2,A(N)
	sta MID_SPEAKER
	jmp main_loop			;280 GOTO 220

ending_module:				;300 REM ENDING MODULE
	lda #0				;310 POKE S20,0
	sta MID_SPEAKER
	sta VOLUME
	rts				;Return control to OS.

delay:
	clc				;clear carry flag
	adc CLOCK			;Add existing time with lowest 24-bit 3byte jiffy clock value
delay_loop:
	cmp CLOCK			;Compare A + clock with clock not equal then we haven't finished yet
	bne delay_loop			
	rts

b_major_scale:
	dc.b 223			;B (offset 0)
	dc.b 227			;D
	dc.b 230			;D#
	dc.b 231			;E
	dc.b 234			;G
	dc.b 236			;G#
	dc.b 238			;A#
	dc.b 239			;B

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
	dc.b %00011011			;900F 
