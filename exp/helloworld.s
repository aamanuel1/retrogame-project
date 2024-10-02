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
helloworld
	dc.b "a",255
	lda >helloworld
	sta $21
	lda <helloworld
	sta $20

	ldy #0
	
printstring
	lda ($20),y
	cmp 255
	beq end
	jsr CHROUT
	iny
	jmp printstring

end:
	rts


