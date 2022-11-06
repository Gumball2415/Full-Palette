; Displays entire 400+ color NTSC NES palette on screen.
; Disables PPU rendering so that current scanline color can be
; set directly by PPU address, then uses cycle-timed code to
; cycle through all colors in a clean grid.
;
; ca65 -o full_palette_persune.o full_palette.s
; ld65 -C nrom128.cfg full_palette_persune.o -o full_palette_persune.nes
;
; Shay Green <gblargg@gmail.com>

; Set to 1 for alternate palette arrangement
ALT_PALETTE = 0

; Height of each row, from 1 to 7 scanlines
row_height = 7

displayed_height = 241 ; affects timing; don't change

even_frame = <0

reset:	jsr init_nes
	
	lda #0
	sta even_frame
	
	jsr sync_vbl_long
	
	; Delay 84 clocks to center horizontally
	ldx #16
:	dex
	bne :-
	bit <0
	
loop:	jsr blacken_palette
	
	; Enable rendering so that we get short and long frames,
	; allowing image to shake less
	lda #$08
	sta $2001
	
	; Delay 2045 clocks
	ldy #150
	ldx #2
:	dey
	bne :-
	nop
	dex
	bne :-

	; Delay extra clock every other frame
	inc even_frame
	lda even_frame
	lsr a
	bcs :+
:
	; Draw palette from tables
	ldy #displayed_height
	lda #0
	clc
scanline:
	; Set address as early as possible, to extend first color all the
	; way off the left edge.
	ldx #$3F		; 10
	stx $2006
	stx $2006
	
	ldx tint_table,y
	stx $2001
	
	ldx palette_table,y
	
	; Write the 12 colors to palette. This will immediately increment
	; PPU address, so color won't be displayed until the next scanline.
	; This means the colors displayed now are from the previous scanline.
	stx $2007		; 82
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	stx $2007
	inx
	
	; Delay one clock less every third scanline
	adc #85
	bcc :+
:
	dey
	
	; Delay last write until as late as possible, so that its color
	; goes all the way into overscan
	stx $2007
	
	bne scanline
	
	jmp loop


;**** Palette/tint tables ****

; These tables determine the starting palette color for each scanline,
; and the tint values. The main color block is centered vertically.
; The tables are read from end to beginning, and the first byte isn't
; used.

height = 4 * 8 * row_height
border = (displayed_height - height) / 2

.align 256
palette_table:
	.res border+2,0
	.repeat height,i
		.byte (i/row_height/(1+ALT_PALETTE*7)) * $10 & $30
	.endrepeat
	.res border,$30

.align 256
tint_table:
	.res border+1,$E0
	.repeat height,i
		.byte (i/row_height/(4-ALT_PALETTE*3)) * $20 & $E0 ^ $E0
	.endrepeat
	.res border,0


;**** Utility routines ****
irq:
nmi:	jmp nmi

init_nes:
	sei
	jsr wait_vbl
	jsr wait_vbl
	lda #0
	sta $2000
	sta $2001
	jsr blacken_palette
	rts

wait_vbl:
	bit $2002
:	bit $2002
	bpl :-
	rts

.align 256


; Fills palette with black and leaves PPU addr at 0
blacken_palette:
	lda #$3F
	sta $2006
	lda #$E0
	sta $2006
	lda #$0F
	ldy #$20
:	sta $2007
	dey
	bne :-
	rts


; Synchronizes precisely with PPU so that next frame will be long.
sync_vbl_long:
	; Synchronize precisely to VBL. VBL occurs every 29780.67
	; CPU clocks. Loop takes 27 clocks. Every 1103 iterations,
	; the second LDA $2002 will read exactly 29781 clocks
	; after a previous read. Thus, the loop will effectively
	; read $2002 one PPU clock later each frame. It starts out
	; with VBL beginning sometime after this read, so that
	; eventually VBL will begin just before the $2002 read,
	; and thus leave CPU exactly synchronized to VBL.
	bit $2002
:	bit $2002
	bpl :-
:	nop
	pha
	pla
	lda $2002
	lda $2002
	pha
	pla
	bpl :-
	
	; Now synchronize with short/long frames.
	
	; Wait one frame with rendering off. This moves VBL time
	; earlier by 1/3 CPU clock.
	
	; Delay 29784 clocks
	ldx #24
	ldy #48
:	dey
	bne :-
	dex
	bne :-
	nop
	lda <0

	; Render one frame. This moves VBL time earlier by either
	; 1/3 or 2/3 CPU clock.
	lda #$08
	sta $2001
	
	; Delay 29752 clocks
	ldy #33
	ldx #24
:	dey
	bne :-
	nop
	dex
	bne :-

	lda #0
	sta $2001
	
	; VBL flag will read set if rendered frame was short
	bit $2002
	bmi @ret
	
	; Rendered frame was long, so wait another (long)
	; frame with rendering disabled. If rendering were enabled,
	; this would be a short frame, so we end up in same state
	; as if it were short frame above.
	
	; Delay 29782 clocks
	ldy #39
	ldx #24
:	dey
	bne :-
	nop
	dex
	bne :-

@ret:	; Now, if rendering is enabled, first frame will be long.
	rts

.segment "INESHDR"
	.byte "NES",26, 1,1, 0,0

.segment "VECTORS"
	.word nmi, reset, irq

.segment "CHR"
	.res 8192

.segment "CODE" ; avoids warning
