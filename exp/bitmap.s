;bitmap.s A first crack at printing out a bitmap on the VIC20

	processor 6502

;KERNAL ADDRESSES

CHROUT = $ffd2

;ORIGIN
	org $1001

;BASIC STUB
	dc.w nextstmt
	dc.w 10				;line number
	dc.b $9e, "4109", 0		;SYS 4109, end of statement
nextstmt
	dc.w 0				;end of basic stub

;PROGRAM START
	lda #'F				;stub for now
	jsr CHROUT
	rts
