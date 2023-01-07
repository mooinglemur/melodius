
.include "x16.inc"

MAX_TRACKS = 16
MIDI_CHANNELS = 16
YM2151_CHANNELS = 8

.struct MIDIFile
    startbank   .byte
    offset      .word
    format      .byte
    ntracks     .byte
    divisions   .word
.endstruct

.struct MIDITrack
    startbank   .byte MAX_TRACKS
    startoffset .word MAX_TRACKS
    curbank     .byte MAX_TRACKS
    curoffset   .word MAX_TRACKS
    delay       .dword MAX_TRACKS
    playable    .byte MAX_TRACKS
.endstruct

.struct MIDIState
    tempo       .dword
    tickrate    .word
.endstruct

.struct MIDIChannel
    modwheel    .byte MIDI_CHANNELS
    volume      .byte MIDI_CHANNELS
    pan         .byte MIDI_CHANNELS
    expression  .byte MIDI_CHANNELS
    damper      .byte MIDI_CHANNELS
    pitchbend   .word MIDI_CHANNELS
.endstruct

.struct YMChannel
    midichannel .byte YM2151_CHANNELS
    note        .byte YM2151_CHANNELS ; 0 if released
    framecnt    .byte YM2151_CHANNELS ; frames since change of note
    instrument  .byte YM2151_CHANNELS ; patch number
.endstruct



.segment "ZEROPAGE"
midizp:
    .res 2

.segment "BSS"
miditracks:
    .res .sizeof(MIDITrack)*MAX_TRACKS
midifile:
    .tag MIDIFile
midistate:
    .tag MIDIState
chunklen:
    .res 4
track_iter:
    .res 1
variable_length:
    .res 4

.segment "CODE"

.macro COMPARE_BYTES s1, len
.scope
    lda #<s1
    sta MM
    lda #>s1
    sta MM+1

    ldx #0
loop:
    jsr fetch_indirect_byte
    cmp $ffff,x
MM = *-2
    bne fail
    inx
    cpx #len
    bcc loop
done:
    clc
    bra end
fail:
    sec
end:
.endscope
.endmacro

.proc get_chunklen: near
    jsr fetch_indirect_byte
    pha
    jsr fetch_indirect_byte
    pha    
    jsr fetch_indirect_byte
    pha
    jsr fetch_indirect_byte
    sta chunklen+0
    pla
    sta chunklen+1
    pla
    sta chunklen+2
    pla
    sta chunklen+3
    rts
.endproc

.proc fetch_indirect_byte: near
    jsr decrement_chunklen
    lda midizp 
    beq fetch   ; midizp is already page aligned
dec_y:          ; midizp is not yet page aligned
    cpy #0
    beq to_y    ; the Y register is already zero
    dey         ; the Y register is not yet zero, make it zero
    inc midizp
    bne dec_y
    lda midizp+1
    inc
    cmp #$C0
    bcc storezp_dec_y
    sbc #$20
    inc X16::Reg::RAMBank
storezp_dec_y:
    sta midizp+1
    bra dec_y
to_y:
    lda midizp  ; transfer the low byte of midizp to the y register
    tay
    stz midizp
fetch:
    lda (midizp),y
    iny
    bne done
    pha
    lda midizp+1
    inc
    cmp #$C0
    bcc storezp
    sbc #$20
    inc X16::Reg::RAMBank
storezp:
    sta midizp+1
    pla
done:
    rts
.endproc

.proc get_variable_length: near
    stz variable_length
    stz variable_length+1
    stz variable_length+2
    stz variable_length+3

fetchloop:
    jsr fetch_indirect_byte
    bit #$80
    bpl final
    and #$7F
    ora variable_length
    
    ; 32-bit shift 7 times :(
    ldx #7
shiftloop:
    asl
    rol variable_length+1
    rol variable_length+2
    rol variable_length+3
    dex
    bne shiftloop
    sta variable_length

    bra fetchloop
final:
    ora variable_length
    sta variable_length
    rts
.endproc

.proc decrement_chunklen: near
    lda chunklen
    bne d0
    lda chunklen+1
    bne d1
    lda chunklen+2
    bne d2
    dec chunklen+3
d2:
    dec chunklen+2
d1:
    dec chunklen+1
d0:
    dec chunklen
    rts
.endproc

.proc advance_to_end_of_chunk: near
    ; flush y
    sty midizp
    ldy #0

    ; Low byte first
    lda chunklen
    stz chunklen
    clc
    adc midizp
    sta midizp
    lda midizp+1
    adc #0
    cmp #$C0
    bcc store_m
    sbc #$20
    inc X16::Reg::RAMBank
store_m:
    sta midizp+1

    ; Middle byte
    lda chunklen+1
    stz chunklen+1
middleloop:
    cmp #$20
    bcc add_m
    sbc #$20
    inc X16::Reg::RAMBank
    bra middleloop
add_m:
    adc midizp+1
    cmp #$C0
    bcc store_m2
    sbc #$20
    inc X16::Reg::RAMBank
store_m2
    sta midizp+1

    ; High byte
    lda chunklen+2
    stz chunklen+2
highloop:
    beq done
    pha
    lda X16::Reg::RAMBank
    clc
    adc #8
    sta X16::Reg::RAMBank
    pla
    dec
    bra highloop
done:
    rts
.endproc

; input: .A = bank, .X .Y 16-bit address of beginning of midi
; returns: .C set if error, clear if success
.proc parsemidi: near
    ; lay down the parameters immediately
    sta midifile + MIDIFile::startbank
    stx midifile + MIDIFile::offset
    sty midifile + MIDIFile::offset+1

    sta X16::Reg::RAMBank
    stx midizp
    sty midizp+1
    
    ldy #0
    ; Header "MThd"
    COMPARE_BYTES MThd, 4
    bcs error

    ; 32-bit big-endian chunk length
    jsr get_chunklen

    ; Format, expect 0 or 1 for now
    jsr fetch_indirect_byte
    bne error
    jsr fetch_indirect_byte
    cmp #2
    bcs error
    sta midifile + MIDIFile::format

    ; Number of tracks
    jsr fetch_indirect_byte
    bne error
    jsr fetch_indirect_byte
    cmp #(MAX_TRACKS+1)
    bcs error
    sta midifile + MIDIFile::ntracks

    ; Divisions
    jsr fetch_indirect_byte
    bit #$80
    bmi error
    and #$7F
    sta midifile + MIDIFile::divisions+1
    jsr fetch_indirect_byte
    sta midifile + MIDIFile::divisions

    stz track_iter
trackloop:
    ; We should be at a track header now
    COMPARE_BYTES MTrk, 4
    bcs error

    jsr get_chunklen

    ; Where is our pointer? We store it in the current track's metadata
    ; Make X our offset into miditracks
    ldx track_iter

    lda X16::Reg::RAMBank
    sta miditracks + MIDITrack::startbank,x
    tya
    sta miditracks + MIDITrack::startoffset,x
    lda midizp+1
    sta miditracks + MIDITrack::startoffset+1,x
    
    ; pull the first delta value out
    jsr get_variable_length
    
    ; store the first event with the delta saved
    ldx track_iter
    lda variable_length
    sta miditracks + MIDITrack::delay,x
    lda variable_length+1
    sta miditracks + MIDITrack::delay+1,x
    lda variable_length+2
    sta miditracks + MIDITrack::delay+2,x
    lda variable_length+3
    sta miditracks + MIDITrack::delay+3,x

    lda X16::Reg::RAMBank
    sta miditracks + MIDITrack::curbank,x
    tya
    sta miditracks + MIDITrack::curoffset,x
    lda midizp+1
    sta miditracks + MIDITrack::curoffset+1,x

    lda #1
    sta miditracks + MIDITrack::playable,x

    jsr advance_to_end_of_chunk

    inc track_iter
    ldx track_iter
    cpx midifile + MIDIFile::ntracks
    bcc trackloop

    clc
    rts
MThd:
    .byte "MThd"
MTrk:
    .byte "MTrk"
error:
    sec
    rts
.endproc

