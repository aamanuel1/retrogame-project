
;soundtest.s Playing around with sound, cycles through possible values in 
;low, mid, high speaker as well as the noise speaker.

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
	lda #$0F			;Set volume to 15 (max)
	sta VOLUME			;Store in $900e
	lda #128			;load  0 lowest note
low_loop:
	sta LOW_SPEAKER			;Store in $900a
	sta $03				;Store the note in zero page 03
	lda #15				;Load a with 60 (1 second for jiffy clock)
	jsr delay			;Delay will delay with value in accumulator
	inc $03				;Increment by 1
	lda $03
	cmp 255				;Go through suite of notes
	bne low_loop
	lda #0				;Turn low speaker off
	sta LOW_SPEAKER
	lda #128
mid_loop:
	sta MID_SPEAKER			;Store in $900b
	sta $03
	lda #15
	jsr delay
	inc $03
	lda $03
	cmp 255
	bne mid_loop
	lda #0
	sta MID_SPEAKER
	lda #128
hi_loop:
	sta HI_SPEAKER
	sta $03
	lda #15
	jsr delay
	inc $03
	lda $03
	cmp 255
	bne hi_loop
	lda #0
	sta HI_SPEAKER
	lda #128
noise_loop:
	sta NOISE
	sta $03
	lda #15
	jsr delay
	inc $03
	lda $03
	cmp 255
	bne noise_loop
	lda #0
	sta NOISE
	sta VOLUME
	rts				;Return control to OS

delay:
	clc				;clear carry flag
	adc CLOCK			;Add existing time with lowest 24-bit 3byte jiffy clock value
delay_loop:
	cmp CLOCK			;Compare A + clock with clock not equal then we haven't finished yet
	bne delay_loop			
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
	dc.b %00011011			;900F 
