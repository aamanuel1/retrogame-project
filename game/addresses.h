;This file includes KERNAL addresses used, zpg current key, CLOCK
;screen memory locations, and VRAM pointers on zpg for jailbreak.s

; KERNAL ADDRESSES
CHROUT = $ffd2
GETIN = $ffe4
SCNKEY = $ff9f
PLOT = $fff0

;IMPORTANT ZERO PG ADDRESSESS
CURKEY = $c5
CLOCK = $a2

;MISC ADDRESSES
SCRMEM = $1e00
COLMEM = $9600
PLRSTRT = $1ee6
PLRCOLR = $96e6

;VRAM POINTERS
SCR_PTR_LO = $00
SCR_PTR_HI = $01
COLOUR_PTR_LO = $02
COLOUR_PTR_HI = $03