; Displays entire 400+ color NTSC NES palette on screen.
; Disables PPU rendering so that current scanline color can be
; set directly by PPU address, then uses cycle-timed code to
; cycle through all colors in a clean grid.
;
; ca65 -o full_palette_persune.o full_palette.s
; ld65 -C nrom128.cfg full_palette_persune.o -o full_palette_persune.nes
;
; Shay Green <gblargg@gmail.com>
; Modifications by Persune 2022

; Set to 1 for alternate palette arrangement
ALT_PALETTE = 0

; Height of each row, from 1 to 7 scanlines
row_height = 7

displayed_height = 241 ; affects timing; don't change

.segment "ZEROPAGE"
	counter:	.res 1
	system:		.res 1
	pointer:	.res 2

.segment "CODE"

reset:	jsr init_nes

	lda #0
	sta counter

	ldx system
	lda jmp_table_lo, x
	sta pointer
	lda jmp_table_hi, x
	sta pointer + 1
	jmp (pointer)

jmp_table_hi:
	.hibytes timing_ntsc, timing_pal, timing_dendy, null
jmp_table_lo:
	.lobytes timing_ntsc, timing_pal, timing_dendy, null

.align 256
timing_ntsc:
	jsr sync_vbl_long_ntsc

	; Delay 84 clocks to center horizontally
	ldx #16
:	dex
	bne :-
	bit <0

loop_ntsc:	jsr blacken_palette

	; Enable rendering so that we get short and long frames,
	; allowing image to shake less
	lda #$08
	sta $2001

	; Delay 2045 clocks
	; 4 + ((2 - 1) * ((((150 * 5) - 1) + 4) + ((((256 * (2 - 1) * 5) - 1) + 4)) + 6)) - 1
	ldy #150
	ldx #2
:	dey
	bne :-
	nop
	dex
	bne :-

	; Delay extra clock every other frame
	inc counter
	lda counter
	lsr a
	bcs :+
:
	; Draw palette from tables
	ldy #displayed_height
	lda #0
	clc
scanline_ntsc:
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

	bne scanline_ntsc

	jmp loop_ntsc

.align 256
timing_pal:
	jsr sync_vbl_long_pal

	; Delay 78 clocks to center horizontally
	ldx #14
:	dex
	bne :-
loop_pal:
	; Delay for a total of 7661 clocks
	jsr blacken_palette	; 315

	; Enable rendering
	lda #$08	; 6
	sta $2001

	; Delay 7334 clocks
	ldy #181
	ldx #6
:	dey
	bne :-
	dex
	bne :-
	nop

	; Draw palette from tables
	ldy #displayed_height	; 6
	lda #0
	clc
scanline_pal:
	; Set address as early as possible, to extend first color all the
	; way off the left edge.
	ldx #$3F		; 6
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
	
	bne scanline_pal

	jmp loop_pal

.align 256
timing_dendy:
	jsr sync_vbl_long_pal

	; Delay 78 clocks to center horizontally
	ldx #15
:	dex
	bne :-
	nop
loop_dendy:
	; Delay for a total of 2385 clocks
	; 315
	jsr blacken_palette
	; we don't need to enable rendering, since the dot skip only happens on NTSC
	; 2064
	nop
	nop
	nop
	nop
	ldy #153
	ldx #2
:	dey
	bne :-
	dex
	bne :-
	
	; Draw palette from tables
	; 6
	ldy #displayed_height
	lda #0
	clc
scanline_dendy:
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

	bne scanline_dendy

	; Delay 5683 clocks, approx. 50 scanlines
	ldy #108
	ldx #5
:	dey
	bne :-
	dex
	bne :-

	jmp loop_dendy

; unknown system, halt
null:
	jmp null

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
nmi:
	inc counter
	rti

init_nes:
	sei
	jsr wait_vbl
	jsr wait_vbl
	; enable NMI to detect TV system
	lda #$80
	sta $2000
	jsr getTVSystem
	sta system
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
sync_vbl_long_ntsc:
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

; Same as above but optimized for PAL timings.
; Since PAL systems do not skip a PPU cycle every odd frame,
; there's no need to determine whether the first frame is
; long or short.
sync_vbl_long_pal:
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
	rts



; taken from https://www.nesdev.org/wiki/Detect_TV_system
;
; NES TV system detection code
; Copyright 2011 Damian Yerrick
;
; Copying and distribution of this file, with or without
; modification, are permitted in any medium without royalty provided
; the copyright notice and this notice are preserved in all source
; code copies.  This file is offered as-is, without any warranty.
;

.align 32  ; ensure that branches do not cross a page boundary

;;
; Detects which of NTSC, PAL, or Dendy is in use by counting cycles
; between NMIs.
;
; NTSC NES produces 262 scanlines, with 341/3 CPU cycles per line.
; PAL NES produces 312 scanlines, with 341/3.2 CPU cycles per line.
; Its vblank is longer than NTSC, and its CPU is slower.
; Dendy is a Russian famiclone distributed by Steepler that uses the
; PAL signal with a CPU as fast as the NTSC CPU.  Its vblank is as
; long as PAL's, but its NMI occurs toward the end of vblank (line
; 291 instead of 241) so that cycle offsets from NMI remain the same
; as NTSC, keeping Balloon Fight and any game using a CPU cycle-
; counting mapper (e.g. FDS, Konami VRC) working.
;
; nmis is a variable that the NMI handler modifies every frame.
; - Renamed to counter
; Make sure your NMI handler finishes within 1500 or so cycles (not
; taking the whole NMI or waiting for sprite 0) while calling this,
; or the result in A will be wrong.
;
; @return A: TV system (0: NTSC, 1: PAL, 2: Dendy; 3: unknown
;         Y: high byte of iterations used (1 iteration = 11 cycles)
;         X: low byte of iterations used
getTVSystem:
	ldx #0
	ldy #0
	lda counter
nmiwait1:
	cmp counter
	beq nmiwait1
	lda counter

nmiwait2:
	; Each iteration takes 11 cycles.
	; NTSC NES: 29780 cycles or 2707 = $A93 iterations
	; PAL NES:  33247 cycles or 3022 = $BCE iterations
	; Dendy:    35464 cycles or 3224 = $C98 iterations
	; so we can divide by $100 (rounding down), subtract ten,
	; and end up with 0=ntsc, 1=pal, 2=dendy, 3=unknown
	inx
	bne :+
	iny
:
	cmp counter
	beq nmiwait2
	tya
	sec
	sbc #10
	cmp #3
	bcc notAbove3
	lda #3
notAbove3:
	rts

.segment "INESHDR"
	.byte "NES",26, 1,1, 0,0

.segment "VECTORS"
	.word nmi, reset, irq

.segment "CHR"
	.res 8192
