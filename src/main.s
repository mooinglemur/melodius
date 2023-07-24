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
playback_mode:
    .res 1
.segment "CODE"

.include "x16.inc"
.scope AudioAPI
    .include "audio.inc"
.endscope

.export playback_mode

.import register_handler
.import deregister_handler
.import midi_parse
.import midi_play
.import midi_stop
.import midi_restart
.import midi_is_playing

.import setup_tiles
.import setup_sprites
.import hide_sprites
.import setup_instruments
.import do_midi_sprites
.import do_zsm_sprites
.import update_instruments

.import init_directory
.import load_directory
.import show_directory
.import dirlist_nav_down
.import dirlist_nav_up
.import dirlist_nav_home
.import dirlist_nav_end
.import dirlist_exec
.import dir_needs_refresh
.import dir_not_playing
.import files_full_size
.import draw_file_box
.import check_lazy_load

.include "macros.inc"

.scope zsmkit
.include "zsmkit.inc"
.endscope

main:
    ; screen mode 0
    lda #0
    clc
    jsr X16::Kernal::SCREEN_MODE

    ; set up raster bar thingy
    lda #2
    sta Vera::Reg::Ctrl

    lda #($A0 - 1)
    sta Vera::Reg::DCHStop

    stz Vera::Reg::Ctrl

    ; set ISO mode
    lda #$0f
    jsr X16::Kernal::CHROUT

    ; get skinny charset
    lda #6
    jsr X16::Kernal::SCREEN_SET_CHARSET

    jsr setup_sprites
    jsr setup_tiles
;    jsr setup_instruments

    stz playback_mode

    lda #1
    jsr zsmkit::zsm_init_engine

    jsr register_handler

    jsr init_directory

    ldx #34
    ldy #20
    jsr draw_file_box

    jsr load_directory
    jsr show_directory
endless:
    DONE_BORDER
    wai

rekey:
    jsr X16::Kernal::GETIN
    beq endkey

    cmp #$91 ; up arrow
    bne :+
    lda #1
    jsr dirlist_nav_up
    bra rekey
:

    cmp #$11 ; down arrow
    bne :+
    lda #1
    jsr dirlist_nav_down
    bra rekey
:

    cmp #$82 ; pgup
    bne :+
    lda files_full_size
    jsr dirlist_nav_up
    bra rekey
:

    cmp #$02 ; pgdn
    bne :+
    lda files_full_size
    jsr dirlist_nav_down
    bra rekey
:

    cmp #$13 ; home
    bne :+
    jsr dirlist_nav_home
    bra rekey
:

    cmp #$04 ; end
    bne :+
    jsr dirlist_nav_end
    bra rekey
:

    cmp #$20 ; space
    bne :+
    lda playback_mode
    cmp #1
    beq stopmidi
    cmp #2
    beq stopzsm
    bra rekey
:

    cmp #$0d
    bne :+
    jsr dirlist_exec
    bra rekey
:
endkey:
    lda dir_needs_refresh
    beq :+
    jsr show_directory
    stz dir_needs_refresh
:

    VIZ_BORDER

    lda playback_mode
    cmp #1
    beq ismidi
    cmp #2
    beq iszsm
    bra continue
ismidi:
    jsr do_midi_sprites
    jsr update_instruments
    jsr midi_is_playing
    bne continue
stopmidi:
    ldx #34
    ldy #20
    jsr draw_file_box
    inc dir_needs_refresh

    jsr midi_stop
    jsr dir_not_playing
    stz playback_mode
    jsr hide_sprites
    bra continue
iszsm:
    jsr check_lazy_load
    jsr do_zsm_sprites
    ldx #0
    jsr zsmkit::zsm_getstate
    bcs continue
stopzsm:
    ldx #34
    ldy #20
    jsr draw_file_box
    inc dir_needs_refresh

    ldx #0
    jsr zsmkit::zsm_close
    jsr dir_not_playing
    stz playback_mode
    jsr hide_sprites
continue:
    DONE_BORDER

    jsr X16::Kernal::STOP ; test stop key
    beq exit

;    bne endless

    jmp endless
exit:
    jsr deregister_handler

    JSRFAR AudioAPI::ym_init, $0A

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


domidi:
    lda #3
    ldx #$00
    ldy #$A0

    jsr midi_parse
    bcc :+
    jmp error
:
    jsr midi_restart

    lda #1
    jsr midi_play

    lda #1
    sta playback_mode

    rts
