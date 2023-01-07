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

main:
;    jsr register_handler

;    jsr deregister_handler
    rts
