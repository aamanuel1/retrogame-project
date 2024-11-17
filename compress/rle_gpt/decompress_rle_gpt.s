;decompress_rle_gpt.s GPT version of a decompression program using its RLE algo.

    	processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2
SCRMEM = $1e00
COLMEM = $9600
CHR_LOW = $01
CHR_HIGH = $02
COLOUR_LOW = $03
COLOUR_HIGH = $04
TEMP_BYTE = $05

;ORIGIN
	org $1001

;BASIC STUB
    	dc.w nextstmt
   	dc.w 10                     ; line number
    	dc.b $9e, "4109", 0         ; SYS 4109, end of statement
nextstmt:
	dc.w 0                      ; end of basic stub

; Decompression routine starts here
; Assumes compressed data at address COMPRESSED_DATA
; Decompressed data will be written starting at DECOMPRESSED_DATA

init:					;Initialize video settings on VIC chip
	ldy #16				;Iterator
init_loop:
	dey
	lda videosettings,Y
	sta $9000,Y
	tya
	bne init_loop

decompress_rle:
	lda #<SCRMEM			;Load low byte of first screen addr
	sta CHR_LOW			;Store
	lda #>SCRMEM			;Load high byte of first screen addr
	sta CHR_HIGH			;Store
	lda #<COLMEM			;Load and store low byte of first colour addr
	sta COLOUR_LOW
	lda #>COLMEM			;Load and store high byte of first color addr
	sta COLOUR_HIGH
	ldy #$00                    	; Initialize index for compressed data

decompress_char_loop:
	lda compressed_title,Y       	; Load run length
    	tax                         	; Store run length in X register
	iny

    	lda compressed_title,Y       	; load data byte
    	sta TEMP_BYTE               	; Temporarily store data byte
    	iny

write_char_loop:
    	lda TEMP_BYTE               	; Load data byte
    	sta (CHR_LOW),Y    		; Store data
    	inc CHR_LOW
    	bne skip_inc_hi_char
	inc CHR_HIGH			
	
skip_inc_hi_char:
	dex                         	; Decrement run length	
	bne write_char_loop		; Repeat until run length is zero

	lda CHR_HIGH
	cmp #$1F
	bpl main
	lda CHR_LOW
	cmp #$FB

;drawtitle:
;	lda #<SCRMEM			;Load low byte of first screen addr
;	sta CHR_LOW			;Store
;	lda #>SCRMEM			;Load high byte of first screen addr
;	sta CHR_HIGH			;Store
;	lda #<COLMEM			;Load and store low byte of first colour addr
;	sta COLOUR_LOW
;	lda #>COLMEM			;Load and store high byte of first color addr
;	sta COLOUR_HIGH
;	ldy #$0				;Counter = 0
;drawtitle_loop:
;	lda title_chr,Y			;Load with title character in title_chr array
;	sta (CHR_LOW),Y			;Store in screen memory
;	lda title_colour,Y		;Load with title colour, 4 bit number + 8
;	sta (COLOUR_LOW),Y		;Store in colour memory
;	iny				;Loop until we loop back to 0
;	bne drawtitle_loop		
;	lda CHR_HIGH			;Load high character.
;	cmp #$1e			;If we're only halfway done ($1e) then
;	beq drawtitle_lower		;Draw some more.
;	jmp main			;If we get here something's wrong.
;drawtitle_lower:
;	inc CHR_HIGH			;Increment 1e to 1f then continue
;	inc COLOUR_HIGH
;	ldy #$0				;Reload counter = 0
;drawtitle_lower_loop:
;	lda title_chr+$FF,Y		;Shift by 255 of start of title_chr + Y
;	sta (CHR_LOW),Y			;Store in screen memory
;	lda title_colour+$FF,Y		;Same thing with colour memory
;	sta (COLOUR_LOW),Y
;	iny				;Counter++
;	cpy #$FB			;check if we've reached 506.
;	bne drawtitle_lower_loop	;If not loop again

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

compressed_title:
	INCBIN "compressed_screendump_rle_gpt.bin"

compressed_colours:
	INCBIN "compressed_colourdump_rle_gpt.bin"
