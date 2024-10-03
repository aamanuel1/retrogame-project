;helloworld.s, prints hello world to basic VIC20 screen

	processor 6502

;KERNAL addresses
CHROUT = $ffd2

	org $1001

;BASIC stub
	dc.w nextstmt		
	dc.w 10			;Line number
	dc.b $9e, "4109", 0	;sys 4109, end of statement
nextstmt
	dc.w 0			;End of basic stub, start of assembly

;PROGRAM START	
	lda #>helloworld	;Load high byte
	sta $21			;Store in 20, 21 (lower zero page addr)
	lda #<helloworld	;Load low byte
	sta $20			;store in zero page 20
	jsr printstring		;Jump but return from printstring
	rts			;return from printstring and continue

printstring
	ldy #0			;Load 0 to start increment
printstring_loop
	lda ($20),y		;Indirect indexed address, zero page $20 + y
	cmp #255		;Compare value of A with 255 terminator
	beq printstring_end	;Return to calling subroutine
	jsr printchar		;Otherwise print the character at A
	iny			;Increment Y to move down the string
	jmp printstring_loop	;loop through until the end

printstring_end
	rts			;The end

printchar
	cmp #96			;Check if the letter is a lowercase
	bcc printchar_ok	;Carry clear means that A <= 96, so it's uppercase already
	and #%11011111		;Flip bit 5 to offset down by 32 (-32)
printchar_ok
	jsr CHROUT		;Print the char
	rts			;return to calling subroute

helloworld
	dc.b "Hello world",255	;Data section, THE string.

