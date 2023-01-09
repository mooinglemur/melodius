.include "audio.inc"
.include "x16.inc"

.export register_handler
.export deregister_handler

.import midi_playtick

.segment "BSS"
old_irq_handler:
    .res 2
.segment "CODE"

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

    plp
    rts

deregister_handler:
    php
    sei

    lda old_irq_handler
    sta X16::Vec::IRQVec
    lda old_irq_handler+1
    sta X16::Vec::IRQVec+1

    plp
    rts


handler:
    lda X16::Reg::ROMBank
    pha
    lda #$0A
    sta X16::Reg::ROMBank

    lda X16::Reg::RAMBank
    pha
    
    jsr midi_playtick

    pla
    sta X16::Reg::RAMBank

    pla
    sta X16::Reg::ROMBank

    jmp (old_irq_handler)
