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
.scope AudioAPI
    .include "audio.inc"
.endscope

.import register_handler
.import deregister_handler
.import midi_parse
.import midi_play
.import midi_restart
.import midi_is_playing

.import setup_tiles
.import setup_sprites
.import setup_instruments
.import do_midi_sprites
.import update_instruments

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
    jsr setup_tiles
    jsr setup_instruments

    jsr midi_restart

    lda #1
    jsr midi_play

    jsr register_handler

endless:
    DONE_BORDER
    wai

    VIZ_BORDER
    jsr do_midi_sprites

    jsr update_instruments
    DONE_BORDER

    jsr X16::Kernal::STOP ; test stop key
    beq exit

    jsr midi_is_playing
    bne endless

exit:
    JSRFAR AudioAPI::ym_init, $0A

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
