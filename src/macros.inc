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
