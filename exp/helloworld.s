;helloworld.s, prints hello world to basic VIC20 screen

	processor 6502

;KERNAL addresses
CHROUT = $ffd2

	org $1001

;BASIC stub
	dc.w nextstmt
	dc.w 10
	dc.b $9e, "4109", 0
nextstmt
	dc.w 0

;PROGRAM START	
	lda #>helloworld
	sta $21
	lda #<helloworld
	sta $20
	jsr printstring
	rts

printstring
	ldy #0
printstring_loop
	lda ($20),y
	cmp #255
	beq printstring_end
	jsr printchar
	iny
	jmp printstring_loop

printstring_end
	rts

printchar
	cmp #96
	bcc printchar_ok
	and #%11011111
printchar_ok
	jsr CHROUT
	rts

helloworld
	dc.b "Hello world",255

