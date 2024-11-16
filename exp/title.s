;title.s Uncompressed title screen for my awesome game.

    processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2
SCRMEM = $1e00
COLMEM = $9600
CHR_LOW = $01
CHR_HIGH = $02
COLOUR_LOW = $03
COLOUR_HIGH = $04
TITLE_ROW = $05

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

drawtitle:
	lda #<SCRMEM			;Load low byte of first screen addr
	sta CHR_LOW			;Store
	lda #>SCRMEM			;Load high byte of first screen addr
	sta CHR_HIGH			;Store
	lda #<COLMEM			;Load and store low byte of first colour addr
	sta COLOUR_LOW
	lda #>COLMEM			;Load and store high byte of first color addr
	sta COLOUR_HIGH
	ldy #$0				;Counter = 0
drawtitle_loop:
	lda title_chr,Y			;Load with title character in title_chr array
	sta (CHR_LOW),Y			;Store in screen memory
	lda title_colour,Y		;Load with title colour, 4 bit number + 8
	sta (COLOUR_LOW),Y		;Store in colour memory
	iny				;Loop until we loop back to 0
	bne drawtitle_loop		
	lda CHR_HIGH			;Load high character.
	cmp #$1e			;If we're only halfway done ($1e) then
	beq drawtitle_lower		;Draw some more.
	jmp main			;If we get here something's wrong.
drawtitle_lower:
	inc CHR_HIGH			;Increment 1e to 1f then continue
	inc COLOUR_HIGH
	ldy #$0				;Reload counter = 0
drawtitle_lower_loop:
	lda title_chr+$FF,Y		;Shift by 255 of start of title_chr + Y
	sta (CHR_LOW),Y			;Store in screen memory
	lda title_colour+$FF,Y		;Same thing with colour memory
	sta (COLOUR_LOW),Y
	iny				;Counter++
	cpy #$FB			;check if we've reached 506.
	bne drawtitle_lower_loop	;If not loop again

main:
	jmp main			;Loop until the end of time

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

	; screen character data
title_chr:
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C, $2D, $2E, $2F, $50, $20, $20, $20
	dc.b	$20, $20, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E, $3F, $60, $20, $20, $20
	dc.b	$20, $20, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F, $70, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20
	dc.b	$20, $20, $20, $02, $19, $20, $01, $01, $12, $0F, $0E, $20, $0D, $01, $0E, $15, $05, $0C, $20, $20, $20, $20
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
title_colour:
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $01, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $01, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $01, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $01, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $01, $0D, $0D, $0D, $0D, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E, $0E
	dc.b	$0E, $01, $0E, $0D, $0D, $0D, $0D, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0D, $0D, $0D, $0D, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$09, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $09, $0E
	dc.b	$0E, $0E, $0E, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $0E, $09, $09
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $09, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E, $0E

	org $1800
; Character bitmap definitions 2k
bitmap_def:
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
	dc.b	$3C, $20, $20, $20, $20, $20, $3C, $00
	dc.b	$00, $40, $20, $10, $08, $04, $02, $00
	dc.b	$3C, $04, $04, $04, $04, $04, $3C, $00
	dc.b	$00, $08, $1C, $2A, $08, $08, $08, $08
	dc.b	$00, $00, $10, $20, $7F, $20, $10, $00
	dc.b	$00, $00, $00, $00, $00, $00, $00, $00
	dc.b	$3C, $7C, $7C, $7C, $7C, $7C, $7C, $7C
	dc.b	$00, $05, $05, $05, $05, $05, $14, $14
	dc.b	$0F, $5F, $5F, $5F, $5F, $5F, $1F, $1F
	dc.b	$00, $14, $14, $14, $14, $14, $14, $14
	dc.b	$03, $17, $17, $17, $17, $17, $17, $17
	dc.b	$C0, $C0, $C0, $C0, $C0, $C0, $C0, $C0
	dc.b	$00, $55, $55, $50, $50, $50, $50, $50
	dc.b	$0F, $4F, $5F, $5F, $1F, $1F, $1F, $1E
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
	dc.b	$00, $00, $00, $40, $40, $00, $00, $00
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
	dc.b	$17, $17, $17, $17, $17, $07, $07, $03
	dc.b	$D4, $D4, $D4, $D4, $D4, $D5, $D5, $C0
	dc.b	$00, $00, $00, $00, $00, $54, $54, $00
	dc.b	$14, $34, $F4, $F4, $F4, $F4, $F4, $F0
	dc.b	$14, $14, $14, $14, $14, $14, $14, $00
	dc.b	$17, $17, $17, $17, $17, $17, $17, $03
	dc.b	$C0, $C5, $C5, $D5, $D4, $D4, $50, $40
	dc.b	$00, $3C, $7E, $7E, $7E, $7E, $3C, $00
	dc.b	$00, $00, $00, $00, $00, $00, $FF, $00
	dc.b	$36, $7F, $7F, $7F, $3E, $1C, $08, $00
	dc.b	$40, $40, $40, $40, $40, $40, $40, $40
	dc.b	$00, $00, $00, $00, $03, $04, $08, $08
	dc.b	$81, $42, $24, $18, $18, $24, $42, $81
	dc.b	$00, $3C, $42, $42, $42, $42, $3C, $00
	dc.b	$08, $1C, $2A, $77, $2A, $08, $08, $00
	dc.b	$02, $02, $02, $02, $02, $02, $02, $02
	dc.b	$08, $1C, $3E, $7F, $3E, $1C, $08, $00
	dc.b	$08, $08, $08, $08, $FF, $08, $08, $08
	dc.b	$A0, $50, $A0, $50, $A0, $50, $A0, $50
	dc.b	$08, $08, $08, $08, $08, $08, $08, $08
	dc.b	$00, $00, $01, $3E, $54, $14, $14, $00
	dc.b	$FF, $7F, $3F, $1F, $0F, $07, $03, $01
	dc.b	$40, $40, $40, $40, $40, $C0, $C0, $C0
	dc.b	$F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0
	dc.b	$00, $00, $00, $00, $FF, $FF, $FF, $FF
	dc.b	$FF, $00, $00, $00, $00, $00, $00, $00
	dc.b	$00, $00, $00, $00, $00, $00, $00, $FF
	dc.b	$80, $80, $80, $80, $80, $80, $80, $80
	dc.b	$AA, $55, $AA, $55, $AA, $55, $AA, $55
	dc.b	$01, $01, $01, $01, $01, $01, $01, $01
	dc.b	$00, $00, $00, $00, $AA, $55, $AA, $55
	dc.b	$FF, $FE, $FC, $F8, $F0, $E0, $C0, $80
	dc.b	$03, $03, $03, $03, $03, $03, $03, $03
	dc.b	$08, $08, $08, $08, $0F, $08, $08, $08
	dc.b	$00, $00, $00, $00, $0F, $0F, $0F, $0F
	dc.b	$08, $08, $08, $08, $0F, $00, $00, $00
	dc.b	$00, $00, $00, $00, $F8, $08, $08, $08
	dc.b	$00, $00, $00, $00, $00, $00, $FF, $FF
	dc.b	$C0, $D0, $D4, $D4, $C5, $C5, $C5, $C0
	dc.b	$08, $08, $08, $08, $FF, $00, $00, $00
	dc.b	$00, $00, $00, $00, $FF, $08, $08, $08
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
	dc.b	$E3, $DD, $B5, $A9, $B3, $DF, $E1, $FF
	dc.b	$E7, $DB, $BD, $81, $BD, $BD, $BD, $FF
	dc.b	$83, $DD, $DD, $C3, $DD, $DD, $83, $FF
	dc.b	$E3, $DD, $BF, $BF, $BF, $DD, $E3, $FF
	dc.b	$87, $DB, $DD, $DD, $DD, $DB, $87, $FF
	dc.b	$81, $BF, $BF, $87, $BF, $BF, $81, $FF
	dc.b	$81, $BF, $BF, $87, $BF, $BF, $BF, $FF
	dc.b	$E3, $DD, $BF, $B1, $BD, $DD, $E3, $FF
	dc.b	$BD, $BD, $BD, $81, $BD, $BD, $BD, $FF
	dc.b	$E3, $F7, $F7, $F7, $F7, $F7, $E3, $FF
	dc.b	$F1, $FB, $FB, $FB, $FB, $BB, $C7, $FF
	dc.b	$BD, $BB, $B7, $8F, $B7, $BB, $BD, $FF
	dc.b	$BF, $BF, $BF, $BF, $BF, $BF, $81, $FF
	dc.b	$BD, $99, $A5, $A5, $BD, $BD, $BD, $FF
	dc.b	$BD, $9D, $AD, $B5, $B9, $BD, $BD, $FF
	dc.b	$E7, $DB, $BD, $BD, $BD, $DB, $E7, $FF
	dc.b	$83, $BD, $BD, $83, $BF, $BF, $BF, $FF
	dc.b	$E7, $DB, $BD, $BD, $B5, $DB, $E5, $FF
	dc.b	$83, $BD, $BD, $83, $B7, $BB, $BD, $FF
	dc.b	$C3, $BD, $BF, $C3, $FD, $BD, $C3, $FF
	dc.b	$C1, $F7, $F7, $F7, $F7, $F7, $F7, $FF
	dc.b	$BD, $BD, $BD, $BD, $BD, $BD, $C3, $FF
	dc.b	$BD, $BD, $BD, $DB, $DB, $E7, $E7, $FF
	dc.b	$BD, $BD, $BD, $A5, $A5, $99, $BD, $FF
	dc.b	$BD, $BD, $DB, $E7, $DB, $BD, $BD, $FF
	dc.b	$DD, $DD, $DD, $E3, $F7, $F7, $F7, $FF
	dc.b	$81, $FD, $FB, $E7, $DF, $BF, $81, $FF
	dc.b	$C3, $DF, $DF, $DF, $DF, $DF, $C3, $FF
	dc.b	$FF, $BF, $DF, $EF, $F7, $FB, $FD, $FF
	dc.b	$C3, $FB, $FB, $FB, $FB, $FB, $C3, $FF
	dc.b	$FF, $F7, $E3, $D5, $F7, $F7, $F7, $F7
	dc.b	$FF, $FF, $EF, $DF, $80, $DF, $EF, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	dc.b	$F7, $F7, $F7, $F7, $FF, $FF, $F7, $FF
	dc.b	$DB, $DB, $DB, $FF, $FF, $FF, $FF, $FF
	dc.b	$DB, $DB, $81, $DB, $81, $DB, $DB, $FF
	dc.b	$F7, $E1, $D7, $E3, $F5, $C3, $F7, $FF
	dc.b	$FF, $9D, $9B, $F7, $EF, $D9, $B9, $FF
	dc.b	$CF, $B7, $B7, $CF, $B5, $BB, $C5, $FF
	dc.b	$FB, $F7, $EF, $FF, $FF, $FF, $FF, $FF
	dc.b	$FB, $F7, $EF, $EF, $EF, $F7, $FB, $FF
	dc.b	$DF, $EF, $F7, $F7, $F7, $EF, $DF, $FF
	dc.b	$F7, $D5, $E3, $C1, $E3, $D5, $F7, $FF
	dc.b	$FF, $F7, $F7, $C1, $F7, $F7, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $F7, $F7, $EF
	dc.b	$FF, $FF, $FF, $81, $FF, $FF, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $E7, $E7, $FF
	dc.b	$FF, $FD, $FB, $F7, $EF, $DF, $BF, $FF
	dc.b	$C3, $BD, $B9, $A5, $9D, $BD, $C3, $FF
	dc.b	$F7, $E7, $D7, $F7, $F7, $F7, $C1, $FF
	dc.b	$C3, $BD, $FD, $F3, $CF, $BF, $81, $FF
	dc.b	$C3, $BD, $FD, $E3, $FD, $BD, $C3, $FF
	dc.b	$FB, $F3, $EB, $DB, $81, $FB, $FB, $FF
	dc.b	$81, $BF, $87, $FB, $FD, $BB, $C7, $FF
	dc.b	$E3, $DF, $BF, $83, $BD, $BD, $C3, $FF
	dc.b	$81, $BD, $FB, $F7, $EF, $EF, $EF, $FF
	dc.b	$C3, $BD, $BD, $C3, $BD, $BD, $C3, $FF
	dc.b	$C3, $BD, $BD, $C1, $FD, $FB, $C7, $FF
	dc.b	$FF, $FF, $F7, $FF, $FF, $F7, $FF, $FF
	dc.b	$FF, $FF, $F7, $FF, $FF, $F7, $F7, $EF
	dc.b	$F1, $E7, $CF, $9F, $CF, $E7, $F1, $FF
	dc.b	$FF, $FF, $81, $FF, $81, $FF, $FF, $FF
	dc.b	$8F, $E7, $F3, $F9, $F3, $E7, $8F, $FF
	dc.b	$C3, $BD, $FD, $F3, $EF, $FF, $EF, $FF
	dc.b	$FF, $FF, $FF, $FF, $00, $FF, $FF, $FF
	dc.b	$F7, $E3, $C1, $80, $80, $E3, $C1, $FF
	dc.b	$EF, $EF, $EF, $EF, $EF, $EF, $EF, $EF
	dc.b	$FF, $FF, $FF, $00, $FF, $FF, $FF, $FF
	dc.b	$FF, $FF, $00, $FF, $FF, $FF, $FF, $FF
	dc.b	$FF, $00, $FF, $FF, $FF, $FF, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $00, $FF, $FF
	dc.b	$DF, $DF, $DF, $DF, $DF, $DF, $DF, $DF
	dc.b	$FB, $FB, $FB, $FB, $FB, $FB, $FB, $FB
	dc.b	$FF, $FF, $FF, $FF, $1F, $EF, $F7, $F7
	dc.b	$F7, $F7, $F7, $FB, $FC, $FF, $FF, $FF
	dc.b	$F7, $F7, $F7, $EF, $1F, $FF, $FF, $FF
	dc.b	$7F, $7F, $7F, $7F, $7F, $7F, $7F, $00
	dc.b	$7F, $BF, $DF, $EF, $F7, $FB, $FD, $FE
	dc.b	$FE, $FD, $FB, $F7, $EF, $DF, $BF, $7F
	dc.b	$00, $7F, $7F, $7F, $7F, $7F, $7F, $7F
	dc.b	$00, $FE, $FE, $FE, $FE, $FE, $FE, $FE
	dc.b	$FF, $C3, $81, $81, $81, $81, $C3, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $00, $FF
	dc.b	$C9, $80, $80, $80, $C1, $E3, $F7, $FF
	dc.b	$BF, $BF, $BF, $BF, $BF, $BF, $BF, $BF
	dc.b	$FF, $FF, $FF, $FF, $FC, $FB, $F7, $F7
	dc.b	$7E, $BD, $DB, $E7, $E7, $DB, $BD, $7E
	dc.b	$FF, $C3, $BD, $BD, $BD, $BD, $C3, $FF
	dc.b	$F7, $E3, $D5, $88, $D5, $F7, $F7, $FF
	dc.b	$FD, $FD, $FD, $FD, $FD, $FD, $FD, $FD
	dc.b	$F7, $E3, $C1, $80, $C1, $E3, $F7, $FF
	dc.b	$F7, $F7, $F7, $F7, $00, $F7, $F7, $F7
	dc.b	$5F, $AF, $5F, $AF, $5F, $AF, $5F, $AF
	dc.b	$F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7
	dc.b	$FF, $FF, $FE, $C1, $AB, $EB, $EB, $FF
	dc.b	$00, $80, $C0, $E0, $F0, $F8, $FC, $FE
	dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	dc.b	$0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F
	dc.b	$FF, $FF, $FF, $FF, $00, $00, $00, $00
	dc.b	$00, $FF, $FF, $FF, $FF, $FF, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $00
	dc.b	$7F, $7F, $7F, $7F, $7F, $7F, $7F, $7F
	dc.b	$55, $AA, $55, $AA, $55, $AA, $55, $AA
	dc.b	$FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE
	dc.b	$FF, $FF, $FF, $FF, $55, $AA, $55, $AA
	dc.b	$00, $01, $03, $07, $0F, $1F, $3F, $7F
	dc.b	$FC, $FC, $FC, $FC, $FC, $FC, $FC, $FC
	dc.b	$F7, $F7, $F7, $F7, $F0, $F7, $F7, $F7
	dc.b	$FF, $FF, $FF, $FF, $F0, $F0, $F0, $F0
	dc.b	$F7, $F7, $F7, $F7, $F0, $FF, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $07, $F7, $F7, $F7
	dc.b	$FF, $FF, $FF, $FF, $FF, $FF, $00, $00
	dc.b	$FF, $FF, $FF, $FF, $F0, $F7, $F7, $F7
	dc.b	$F7, $F7, $F7, $F7, $00, $FF, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $00, $F7, $F7, $F7
	dc.b	$F7, $F7, $F7, $F7, $07, $F7, $F7, $F7
	dc.b	$3F, $3F, $3F, $3F, $3F, $3F, $3F, $3F
	dc.b	$1F, $1F, $1F, $1F, $1F, $1F, $1F, $1F
	dc.b	$F8, $F8, $F8, $F8, $F8, $F8, $F8, $F8
	dc.b	$00, $00, $FF, $FF, $FF, $FF, $FF, $FF
	dc.b	$00, $00, $00, $FF, $FF, $FF, $FF, $FF
	dc.b	$FF, $FF, $FF, $FF, $FF, $00, $00, $00
	dc.b	$FE, $FE, $FE, $FE, $FE, $FE, $FE, $00
	dc.b	$FF, $FF, $FF, $FF, $0F, $0F, $0F, $0F
	dc.b	$F0, $F0, $F0, $F0, $FF, $FF, $FF, $FF
	dc.b	$F7, $F7, $F7, $F7, $07, $FF, $FF, $FF
	dc.b	$0F, $0F, $0F, $0F, $FF, $FF, $FF, $FF
	dc.b	$0F, $0F, $0F, $0F, $F0, $F0, $F0, $F0
