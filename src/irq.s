.include "audio.inc"
.include "x16.inc"

.export register_handler
.export deregister_handler

.import midi_playtick

.segment "BSS"
old_irq_handler:
    .res 2
.segment "CODE"

.include "macros.inc"

register_handler:
    php
    sei
    lda X16::Vec::IRQVec
    sta old_irq_handler
    lda X16::Vec::IRQVec+1
    sta old_irq_handler+1

    lda #<handler
    sta X16::Vec::IRQVec
    lda #>handler
    sta X16::Vec::IRQVec+1

    ; use LINE IRQ instead of VBLANK
    lda Vera::Reg::IEN
    and #$FC
    ora #$02
    sta Vera::Reg::IEN
    stz Vera::Reg::IRQLineL

    plp
    rts

deregister_handler:
    php
    sei

    lda old_irq_handler
    sta X16::Vec::IRQVec
    lda old_irq_handler+1
    sta X16::Vec::IRQVec+1

    ; use VBLANK again
    lda Vera::Reg::IEN
    and #$FC
    ora #$01
    sta Vera::Reg::IEN

    plp
    rts


handler:
    lda X16::Reg::ROMBank
    pha
    lda #$0A
    sta X16::Reg::ROMBank

    lda X16::Reg::RAMBank
    pha
    
    MIDI_BORDER

    jsr midi_playtick

    pla
    sta X16::Reg::RAMBank

    pla
    sta X16::Reg::ROMBank

    KERNAL_BORDER

    lda #2
    sta Vera::Reg::ISR

    jmp (old_irq_handler)
