.ifndef macros_inc
macros_inc = 1

.include "x16.inc"

.macro MIDI_BORDER
    lda #15
    sta Vera::Reg::DCBorder
.endmacro

.macro KERNAL_BORDER
    lda #0
    sta Vera::Reg::DCBorder
.endmacro

.macro DONE_BORDER
    lda #0
    sta Vera::Reg::DCBorder
.endmacro

.macro API_BORDER
    lda #1
    sta Vera::Reg::DCBorder
.endmacro

.macro VIZ_BORDER
    lda #0
    sta Vera::Reg::DCBorder
.endmacro

.macro WAITVSYNC frames
.local endless
.local FRC
	jsr X16::Kernal::RDTIM
.ifnblank frames
	clc
	adc #frames
.endif
	sta FRC
endless:
	wai
	jsr X16::Kernal::RDTIM
	cmp #$ff
FRC = * -1
.ifnblank frames
	bne endless
.else
	beq endless
.endif
.endmacro

.endif
