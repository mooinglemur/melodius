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
.import t0delayF, t0delayL, t0delayM, t0delayH, t0delayU
.import t0bank, t0ptrH, t0ptrL

main:
    lda #1
    ldx #$00
    ldy #$A0

    jsr midi_parse
    bcs error

    jsr midi_restart

    lda #1
    jsr midi_play

    jsr register_handler

endless:
    wai
    lda t0delayU
    jsr print_hex
    lda t0delayH
    jsr print_hex
    lda t0delayM
    jsr print_hex
    lda t0delayL
    jsr print_hex
    lda t0delayF
    jsr print_hex
    lda #$20
    jsr X16::Kernal::CHROUT
    lda t0bank
    jsr print_hex
    lda t0ptrH
    jsr print_hex
    lda t0ptrL
    jsr print_hex

    lda #$0D
    jsr X16::Kernal::CHROUT


    bra endless

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
