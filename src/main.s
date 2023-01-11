.segment "LOADADDR"
    .word $0801
.segment "BASICSTUB"
    .word start-2
    .byte $00,$00,$9e
    .byte "2061"
    .byte $00,$00,$00
.segment "STARTUP"
start:
    jmp main
.segment "BSS"
.segment "CODE"

.include "x16.inc"
.include "audio.inc"

.import register_handler
.import deregister_handler
.import midi_parse
.import midi_play
.import midi_restart

.import setup_sprites
.import do_midi_sprites

.include "macros.inc"

main:
    lda #1
    ldx #$00
    ldy #$A0

    jsr midi_parse
    bcc :+
    jmp error
:

    ; set up raster bar thingy
    lda Vera::Reg::Ctrl
    ora #2
    sta Vera::Reg::Ctrl


    lda #($A0 - 3)
    sta Vera::Reg::DCHStop

    lda Vera::Reg::Ctrl
    and #%11111101
    sta Vera::Reg::Ctrl

    jsr setup_sprites

    jsr midi_restart

    lda #1
    jsr midi_play

    jsr register_handler

endless:
    DONE_BORDER
    wai
    DONE_BORDER
    
    jsr do_midi_sprites

    jmp endless

    jsr deregister_handler
    rts

error:
    lda #$45
    jsr X16::Kernal::CHROUT

    rts


print_hex:
    jsr byte_to_hex
    phx
    jsr X16::Kernal::CHROUT
    pla
    jsr X16::Kernal::CHROUT
    
    rts

byte_to_hex: ; converts a number to two ASCII/PETSCII hex digits: input A = number to convert, output A = most sig nybble, X = least sig nybble, affects A,X
    pha

    and #$0f
    tax
    pla
    lsr
    lsr
    lsr
    lsr
    pha
    txa
    jsr xf_hexify
    tax
    pla
xf_hexify:
    cmp #10
    bcc @nothex
    adc #$66
@nothex:
    eor #%00110000
    rts
