.macpack longbranch

.export midi_init
.export midi_parse
.export midi_play
.export midi_playtick
.export midi_restart
.export midi_is_playing
.export midi_stop
.export midi_set_external

.export midi_serial_init

.export ymnote, yminst, ymmidi, midibend, ympan, ymatten, midiinst
.export lyrics

.export vis_ext_ch, vis_ext_note

.export midibeat, midimeasure, midi_timesig_numerator, midi_timesig_denominator
.export midi_tempo, midi_keysig, midi_mode, midipan

.import divide40_24
.import multiply16x16
.import multiply8x8
.import bin2bcd

.import numerator
.import denominator
.import div_result
.import multiplicand
.import multiplier
.import mult_result
.import bcd_input
.import bcd_result

.import debug_byte

.include "x16.inc"

.scope AudioAPI
    .include "audio.inc"
.endscope

.include "macros.inc"

.define MAX_TRACKS 32
.define MIDI_CHANNELS 16
.define YM2151_CHANNELS 8
.define EXT_POLY 32

.define IO_BASE $9F00
.define MIDI_SERIAL_DIVISOR 32

; Define the registers of the TL16C2550PFBR
.define sRHR 0 ; Receive Holding Register
.define sTHR 0 ; Transmit Holding Register
.define sIER 1 ; Interrupt Enable Register
.define sFCR 2 ; FIFO Control Register
.define sLCR 3 ; Line Control Register
.define sMCR 4 ; Modem Control Register
.define sLSR 5 ; Line Status Register
.define sMSR 6 ; Modem Status Register
.define sDLL 0 ; Divisor Latch LSB
.define sDLM 1 ; Divisor Latch MSB

; Define some bit masks for the registers
.define LCR_DLAB $80 ; Divisor Latch Access Bit
.define LCR_WLS8 $03 ; Word Length Select: 8 bits
.define FCR_FIFOE $01 ; FIFO Enable
.define FCR_RFIFOR $02 ; Receiver FIFO Reset
.define FCR_XFIFOR $04 ; Transmitter FIFO Reset
.define MCR_DTR $01 ; Data Terminal Ready
.define MCR_RTS $02 ; Request To Send
.define LSR_THRE $20 ; Transmitter Holding Register Empty
.define LSR_DR $01 ; Data Ready


.struct MIDIFile
    startbank   .byte
    offset      .word
    format      .byte
    ntracks     .byte
    divisions   .word
.endstruct

.struct MIDITrack
    startbank    .byte MAX_TRACKS
    startoffsetL .byte MAX_TRACKS
    startoffsetH .byte MAX_TRACKS
    curbank      .byte MAX_TRACKS
    curoffsetL   .byte MAX_TRACKS
    curoffsetH   .byte MAX_TRACKS
    deltaF       .byte MAX_TRACKS
    deltaL       .byte MAX_TRACKS
    deltaM       .byte MAX_TRACKS
    deltaH       .byte MAX_TRACKS
    deltaU       .byte MAX_TRACKS
    prevstatus   .byte MAX_TRACKS
    playable     .byte MAX_TRACKS
.endstruct

.struct MIDIState
    tempo                 .dword
    tempo_bcd_bytes       .dword ; nnn.n
    deltas_per_call_frac  .byte
    deltas_per_call       .word
    calls_per_frame       .byte ; any value besides 2 behaves like 1
    playing               .byte
    tempochange           .byte ; flag to adjust deltas at the end of the tick
    deltas_processed_frac .byte
    deltas_processed      .dword ; how far along in the song are we?
    measure               .word  ; BCD, measure number
    beat                  .byte  ; (not BCD) beat number within measure
    beat_bcd              .byte  ; BCD version of above
    deltas_per_beat       .word
    deltas_til_beat_frac  .byte
    deltas_til_beat       .word
    timesig_numerator     .byte
    timesig_denominator   .byte
    timesig_denominator_shifts .byte
    timesig_clocks        .byte
    timesig_32nds_per_quarter  .byte
    keysig_sharps         .byte ; signed byte, negative for flats
    keysig_mode           .byte ; 0 for major, 1 for minor
.endstruct

.struct MIDIChannel
    modwheel    .byte MIDI_CHANNELS
    instrument  .byte MIDI_CHANNELS
    volume      .byte MIDI_CHANNELS
    pan         .byte MIDI_CHANNELS
    expression  .byte MIDI_CHANNELS
    damper      .byte MIDI_CHANNELS ; 0 normally, 1 if pedal is pressed
    pitchbend   .byte MIDI_CHANNELS ; signed 8 bit, +127 = +2 semitones normally
    pbdepth     .byte MIDI_CHANNELS ; depth, 0-12 of pitch bend
    rpnlsb      .byte MIDI_CHANNELS ; currently understood only for pitch bend depth
    rpnmsb      .byte MIDI_CHANNELS ; currently understood only for pitch bend depth
    ext_enable  .byte MIDI_CHANNELS ; means we route this to external MIDI
.endstruct

.struct YMChannel
    midichannel .byte YM2151_CHANNELS
    note        .byte YM2151_CHANNELS ; 0 if released
    callcnt     .byte YM2151_CHANNELS ; calls since change of note (255 max)
    instrument  .byte YM2151_CHANNELS ; patch number, drum = $FF
    undamped    .byte YM2151_CHANNELS ; 0 normally, 1 if releasing damper pedal would release note, so this is only set on release
    track       .byte YM2151_CHANNELS ; which track did this note come from?
    pan         .byte YM2151_CHANNELS
    velocity    .byte YM2151_CHANNELS
    atten       .byte YM2151_CHANNELS
.endstruct

.segment "ZEROPAGE"
midizp:
    .res 2

.segment "BSS"
miditracks:
    .tag MIDITrack
midichannels:
    .tag MIDIChannel
ymchannels:
    .tag YMChannel
midifile:
    .tag MIDIFile
midistate:
    .tag MIDIState
chunklen:
    .res 4
track_iter:
    .res 1
midichannel_iter:
    .res 1
ymchannel_iter:
    .res 1
note_iter:
    .res 1
cur_velocity:
    .res 1
variable_length:
    .res 4
tracks_playing:
    .res 1
lyrics:
    .res 32
last_serial_cmd:
    .res 1 ; to allow for continuations
tmp1:
    .res 1 ; assumed free to use at all times but unsafe after jsr
tmp2:
    .res 1 ; assumed free to use at all times but unsafe after jsr
; for visualizing external midi notes
vis_ext_note:
    .res EXT_POLY
vis_ext_ch:
    .res EXT_POLY

ymnote := ymchannels + YMChannel::note
yminst := ymchannels + YMChannel::instrument 
ymmidi := ymchannels + YMChannel::midichannel
ympan := ymchannels + YMChannel::pan
ymatten := ymchannels + YMChannel::atten
midibend := midichannels + MIDIChannel::pitchbend
midipan := midichannels + MIDIChannel::pan
midiinst := midichannels + MIDIChannel::instrument
midibeat := midistate + MIDIState::beat_bcd
midimeasure := midistate + MIDIState::measure
midi_timesig_numerator := midistate + MIDIState::timesig_numerator
midi_timesig_denominator := midistate + MIDIState::timesig_denominator
midi_tempo := midistate + MIDIState::tempo_bcd_bytes
midi_keysig := midistate + MIDIState::keysig_sharps
midi_mode := midistate + MIDIState::keysig_mode

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

.macro VELOCITY_TO_ATTEN
    eor #$7F
    lsr
    lsr
    sec
    sbc #$10
    bpl :+
    lda #0
:
.endmacro

.proc get_chunklen: near
    jsr fetch_indirect_byte
    sta chunklen+3
    jsr fetch_indirect_byte
    sta chunklen+2
    jsr fetch_indirect_byte
    sta chunklen+1
    jsr fetch_indirect_byte
    sta chunklen+0
    rts
.endproc

.proc fetch_indirect_byte_decchunk: near
    jsr decrement_chunklen
.endproc
.proc fetch_indirect_byte: near
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
    ldy midizp  ; transfer the low byte of midizp to the y register
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
    beq final
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
store_m2:
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


.proc midi_is_playing: near
    lda midistate + MIDIState::playing
    rts
.endproc

; input: .A = bank, .X .Y 16-bit address of beginning of midi
; returns: .C set if error, clear if success
.proc midi_parse: near
    ; lay down the parameters immediately
    sta midifile + MIDIFile::startbank
    stx midifile + MIDIFile::offset
    sty midifile + MIDIFile::offset+1

    sta X16::Reg::RAMBank
    stx midizp
    sty midizp+1

    stz midistate + MIDIState::playing
    lda #1
    sta midistate + MIDIState::calls_per_frame

    ldy #0
    ; Header "MThd"
    COMPARE_BYTES MThd, 4
    bcc :+
    jmp error
:

    ; 32-bit big-endian chunk length
    jsr get_chunklen

    ; Format, expect 0 or 1 for now
    jsr fetch_indirect_byte_decchunk
    cmp #0
    bne error
    jsr fetch_indirect_byte_decchunk
    cmp #2
    bcs error
    sta midifile + MIDIFile::format

    ; Number of tracks
    jsr fetch_indirect_byte_decchunk
    cmp #0
    bne error
    jsr fetch_indirect_byte_decchunk
    cmp #(MAX_TRACKS+1)
    bcs error
    sta midifile + MIDIFile::ntracks

    ; Divisions
    jsr fetch_indirect_byte_decchunk
    bit #$80
    bne error
    and #$7F
    sta midifile + MIDIFile::divisions+1
    jsr fetch_indirect_byte_decchunk
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
    sta miditracks + MIDITrack::startoffsetL,x
    lda midizp+1
    sta miditracks + MIDITrack::startoffsetH,x
    
    stz miditracks + MIDITrack::playable,x

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


; track_iter = track number
; midizp and Y are appropriately set
.proc get_delta: near
    ; pull the next delta value out
    jsr get_variable_length
    
    ; store the first event with the delta saved
    ldx track_iter
    clc
    lda variable_length
    adc miditracks + MIDITrack::deltaL,x
    sta miditracks + MIDITrack::deltaL,x
    lda variable_length+1
    adc miditracks + MIDITrack::deltaM,x
    sta miditracks + MIDITrack::deltaM,x
    lda variable_length+2
    adc miditracks + MIDITrack::deltaH,x
    sta miditracks + MIDITrack::deltaH,x
    lda variable_length+3
    adc miditracks + MIDITrack::deltaU,x
    sta miditracks + MIDITrack::deltaU,x

    rts
.endproc

; Input: .A = MIDI channel
;        .C = set for external, clear for internal
.proc midi_set_external: near
    php
    sei

    and #$0f
    tax
    lda #0
    ror
    sta midichannels + MIDIChannel::ext_enable,x

    lda X16::Reg::ROMBank
    pha
    lda #$0a
    sta X16::Reg::ROMBank

    stx midichannel_iter
    jsr release_channel_notes

    ; send the current program and pb range if we're toggling external on
    ldx midichannel_iter
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+
    lda midichannels + MIDIChannel::instrument,x
    jsr serial_send_progchange
    jsr serial_apply_pbrange
:

    pla
    sta X16::Reg::ROMBank

    plp
    rts
.endproc

.proc serial_apply_pbrange: near
    lda midichannel_iter
    ora #$b0 ; controller
    sta last_serial_cmd
    jsr serial_send_byte
    lda #$64
    jsr serial_send_byte
    lda #$00
    jsr serial_send_byte
    lda #$65
    jsr serial_send_byte
    lda #$00
    jsr serial_send_byte
    lda #$06
    jsr serial_send_byte
    ldx midichannel_iter
    lda midichannels + MIDIChannel::pbdepth,x
    jmp serial_send_byte
.endproc

.proc midi_restart: near
    ; We're resetting the instruments to $FF since we don't know
    ; What patches are loaded into the YM2151's channels
    ldx #0
    lda #$FF
iloop:
    sta ymchannels + YMChannel::instrument,x
    stz ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::callcnt,x
    stz ymchannels + YMChannel::undamped,x
    stz ymchannels + YMChannel::pan,x

    inx
    cpx #YM2151_CHANNELS
    bcc iloop

    ; We're resetting the controller state on all the MIDI channels
    ; to defaults
    ldx #0
mloop:
    lda #$7F
    stz midichannels + MIDIChannel::modwheel,x
    sta midichannels + MIDIChannel::volume,x
    stz midichannels + MIDIChannel::expression,x
    stz midichannels + MIDIChannel::damper,x
    stz midichannels + MIDIChannel::pitchbend,x
    stz midichannels + MIDIChannel::instrument,x
    lda #3
    sta midichannels + MIDIChannel::pan,x
    lda #2 
    sta midichannels + MIDIChannel::pbdepth,x

    lda midichannels + MIDIChannel::ext_enable,x
    beq mcont

    stx midichannel_iter

    txa
    ora #$b0 ; controller
    sta last_serial_cmd
    jsr serial_send_byte

    ; send all notes off
    lda #123
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    ; send reset all controllers
    lda #121
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    ; send bank 0
    lda #0
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    lda #$20
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    ; reset program to 0 (as needed after a bank change)
    lda #0
    jsr serial_send_progchange

    ; reset to default PB range
    jsr serial_apply_pbrange

    ldx midichannel_iter
mcont:

    inx
    cpx #MIDI_CHANNELS
    bcc mloop

    ldx #0
    lda #$ff
eloop:
    sta vis_ext_ch,x
    inx
    cpx #EXT_POLY
    bcc eloop

    jsr clear_lyrics

    stz track_iter
trackloop:
    ldx track_iter
    lda miditracks + MIDITrack::startbank,x
    sta X16::Reg::RAMBank

    ; we're loading directly into y since it is more work not to
    ldy miditracks + MIDITrack::startoffsetL,x
    stz midizp
    lda miditracks + MIDITrack::startoffsetH,x
    sta midizp+1
    
    ; pull the first delta value out
    jsr get_variable_length
    
    ; store the first event with the delta saved
    ldx track_iter
    stz miditracks + MIDITrack::deltaF,x
    lda variable_length
    sta miditracks + MIDITrack::deltaL,x
    lda variable_length+1
    sta miditracks + MIDITrack::deltaM,x
    lda variable_length+2
    sta miditracks + MIDITrack::deltaH,x
    lda variable_length+3
    sta miditracks + MIDITrack::deltaU,x

    lda X16::Reg::RAMBank
    sta miditracks + MIDITrack::curbank,x

    tya
    sta miditracks + MIDITrack::curoffsetL,x
    lda midizp+1
    sta miditracks + MIDITrack::curoffsetH,x

    lda #1
    sta miditracks + MIDITrack::playable,x

    inc track_iter
    ldx track_iter
    cpx midifile + MIDIFile::ntracks
    bcc trackloop

    ; set default tempo of 120 bpm
    DEFAULT_TEMPO = 500000
    
    lda #<(DEFAULT_TEMPO)
    sta midistate + MIDIState::tempo+0
    lda #>(DEFAULT_TEMPO)
    sta midistate + MIDIState::tempo+1
    lda #^(DEFAULT_TEMPO)
    sta midistate + MIDIState::tempo+2
    stz midistate + MIDIState::tempo+3

    stz midistate + MIDIState::deltas_processed_frac
    stz midistate + MIDIState::deltas_processed+0
    stz midistate + MIDIState::deltas_processed+1
    stz midistate + MIDIState::deltas_processed+2
    stz midistate + MIDIState::deltas_processed+3

    ; This routine clobbers Y but we're done using it
    jsr calc_deltas_per_call

    ; Defaults for these, events can override
    lda #4
    sta midistate + MIDIState::timesig_numerator
    sta midistate + MIDIState::timesig_denominator
    lda #2
    sta midistate + MIDIState::timesig_denominator_shifts
    lda #24
    sta midistate + MIDIState::timesig_clocks
    lda #8
    sta midistate + MIDIState::timesig_32nds_per_quarter

    lda #$80
    sta midistate + MIDIState::keysig_sharps
    stz midistate + MIDIState::keysig_mode

    lda #1
    sta midistate + MIDIState::measure
    stz midistate + MIDIState::measure+1
    sta midistate + MIDIState::beat
    sta midistate + MIDIState::beat_bcd

    jsr calc_deltas_per_beat

    clc
    rts
.endproc

.proc calc_deltas_per_beat: near
    lda midifile + MIDIFile::divisions
    sta tmp1
    lda midifile + MIDIFile::divisions+1
    sta tmp2

    lda midistate + MIDIState::timesig_denominator_shifts
    sec
    sbc #2
    tax
    bmi @l
@r:
    beq @e
    lsr tmp2
    ror tmp1
    dex
    bne @r
    bra @e
@l:
    beq @e
    asl tmp1
    rol tmp2
    inx
    bne @l
@e:
    stz midistate + MIDIState::deltas_til_beat_frac
    lda tmp1
    sta midistate + MIDIState::deltas_per_beat
    sta midistate + MIDIState::deltas_til_beat
    lda tmp2
    sta midistate + MIDIState::deltas_per_beat + 1
    sta midistate + MIDIState::deltas_til_beat + 1

    ; new time signature resets the beat to 1 and bumps the measure if it wasn't 1
    lda #1
    cmp midistate + MIDIState::beat
    beq @n
    sta midistate + MIDIState::beat
    sta midistate + MIDIState::beat_bcd
    sed
    lda midistate + MIDIState::measure
    clc
    adc #1
    sta midistate + MIDIState::measure
    lda midistate + MIDIState::measure+1
    adc #0
    sta midistate + MIDIState::measure+1
    cld
@n:
    rts
.endproc

; clobbers X and Y
.proc calc_deltas_per_call: near
    ; dq = MIDIFile::divisions is the number of deltas per quarter note
    ; mt = MIDIState::tempo is the number of microseconds per quarter note
    ; cf = MIDIState::calls_per_frame is the number of times
    ;    in a 1/60 second frame midi_playtick is called

    ; mf = 1000000/60; microseconds per frame
    ; dc = MIDIState::deltas_per_call, the number we're trying to find

    ; fq = mf/mt = fraction of a quarter note per frame
    ; 
    ; df = fq*dq = number of deltas per frame
    ; dc = df/cf = number of deltas per call
    ;   
    ; dc = ((mf*dq)/mt)/cf

    ; first multiply dq by 16667 (mf)
    lda #<16667
    sta multiplicand
    lda #>16667
    sta multiplicand+1
    lda midifile + MIDIFile::divisions
    sta multiplier
    lda midifile + MIDIFile::divisions+1
    sta multiplier+1
    
    jsr multiply16x16

    ; In our math routine memory,
    ; mult_result overlaps numerator like this,
    ; sharing memory, so the numerator is
    ; already multiplied by 256 coming out of
    ; the multiplication
    ; 
    ; mult_result   [ 0| 1| 2| 3]
    ; numerator  [ 0| 1| 2| 3| 4]
    ;
    ; We just have to initialize numerator+0
    stz numerator

    ; now divide that (mf*dq*256) by mt
    lda midistate + MIDIState::tempo
    sta denominator
    lda midistate + MIDIState::tempo+1
    sta denominator+1
    lda midistate + MIDIState::tempo+2
    sta denominator+2
    
    jsr divide40_24
    ; div_result now contains the deltas per frame
    ; multiplied by 256

save_deltas:
    lda div_result
    sta midistate + MIDIState::deltas_per_call_frac
    lda div_result+1
    sta midistate + MIDIState::deltas_per_call
    lda div_result+2
    sta midistate + MIDIState::deltas_per_call+1


    ; convert this result to a decimal tempo 
    ; shift one byte left for precision's sake
    stz div_result+4
    lda div_result+2
    sta div_result+3
    lda div_result+1
    sta div_result+2
    lda div_result+0
    sta div_result+1
    stz div_result+0

    ; (divide by divisions)
    lda midifile + MIDIFile::divisions
    sta denominator
    lda midifile + MIDIFile::divisions+1
    sta denominator+1
    stz denominator+2

    jsr divide40_24
    ; we now have the number of beats per tick*256, multiply it out to 
    ; beats per minute * 10 (so we can display tenths).  36000 is the number
    ; of 60Hz ticks per minute times 10

    lda div_result
    sta multiplicand
    lda div_result+1
    sta multiplicand+1
    lda #<36000
    sta multiplier
    lda #>36000
    sta multiplier+1

    jsr multiply16x16

    ; result is bpm * 10 (w/ fractional byte)
    lda mult_result

    ; now shift based on the time signature basis (is the tempo per quarter note or something else?)
    ; this shifts things in the opposite direction from the other place this is used
    
    lda midistate + MIDIState::timesig_denominator_shifts
    dec
    dec
    tax
    bpl @l
@r:
    beq @e
    lsr mult_result+3
    ror mult_result+2
    ror mult_result+1
    ror mult_result+0
    inx
    bne @r
    bra @e
@l:
    beq @e
    asl mult_result
    rol mult_result+1
    rol mult_result+2
    rol mult_result+3
    dex
    bne @l
@e:
    ; result is litte endian
    ; tt tt . ff ff

    ; we want it in BCD format tt tf
    ; while we're doing it, round the 10ths place up generously
    ; as we probably lost a little bit of precision
    
    lda mult_result+1
    clc
    adc #$cc
    lda mult_result+2
    adc #0
    sta bcd_input
    lda mult_result+3
    adc #0
    sta bcd_input+1

    jsr bin2bcd ; (uses bcd_input as input, bcd_result as output)
    lda bcd_result+3
    sta midistate + MIDIState::tempo_bcd_bytes
    lda bcd_result+2
    sta midistate + MIDIState::tempo_bcd_bytes+1
    lda bcd_result+1
    sta midistate + MIDIState::tempo_bcd_bytes+2
    lda bcd_result+0
    sta midistate + MIDIState::tempo_bcd_bytes+3

    lda midistate + MIDIState::tempo_bcd_bytes


    ; We support either 1 or 2 calls per frame.  If 2, we halve this
    lda midistate + MIDIState::calls_per_frame
    cmp #2
    bne end

    lsr midistate + MIDIState::deltas_per_call+1
    ror midistate + MIDIState::deltas_per_call
    ror midistate + MIDIState::deltas_per_call_frac

end:
    rts
.endproc

.proc midi_play: near
    sta midistate + MIDIState::playing
    rts
.endproc

.proc midi_set_calls_per_frame: near
    sta midistate + MIDIState::calls_per_frame
    rts
.endproc

.proc midi_stop: near
    stz midistate + MIDIState::playing

    php
    sei

    lda X16::Reg::ROMBank
    pha
    lda #$0a
    sta X16::Reg::ROMBank


    lda #YM2151_CHANNELS
:   jsr AudioAPI::ym_release
    dec
    bne :-

    lda #15
    sta midichannel_iter
extloop:
    jsr release_channel_notes
    dec midichannel_iter
    bpl extloop

    pla
    sta X16::Reg::ROMBank

    plp

    rts
.endproc

.proc midi_playtick: near
    ; are we in playback mode?
    ; bail out immediately if not
    lda midistate + MIDIState::playing
    bne :+
    jmp end
:
    stz tracks_playing
    stz track_iter


    ; calculate beat deltas
    sec
    lda midistate + MIDIState::deltas_til_beat_frac
    sbc midistate + MIDIState::deltas_per_call_frac
    sta midistate + MIDIState::deltas_til_beat_frac

    lda midistate + MIDIState::deltas_til_beat
    sbc midistate + MIDIState::deltas_per_call
    sta midistate + MIDIState::deltas_til_beat

    lda midistate + MIDIState::deltas_til_beat+1
    sbc midistate + MIDIState::deltas_per_call+1
    sta midistate + MIDIState::deltas_til_beat+1

    bpl not_beat
beat_it:
    ; reset the beat countdown
    clc
    lda midistate + MIDIState::deltas_per_beat
    adc midistate + MIDIState::deltas_til_beat
    sta midistate + MIDIState::deltas_til_beat

    lda midistate + MIDIState::deltas_per_beat+1
    adc midistate + MIDIState::deltas_til_beat+1
    sta midistate + MIDIState::deltas_til_beat+1

    ; bump the beat
    lda midistate + MIDIState::beat_bcd
    sed
    clc
    adc #1
    cld
    sta midistate + MIDIState::beat_bcd
    lda midistate + MIDIState::beat
    inc
    cmp midistate + MIDIState::timesig_numerator
    beq :+
    bcc :+
    lda midistate + MIDIState::measure
    sed
    adc #0 ; carry set, will add the one we want
    sta midistate + MIDIState::measure
    lda midistate + MIDIState::measure+1
    adc #0
    sta midistate + MIDIState::measure+1
    cld
    lda #1
    sta midistate + MIDIState::beat_bcd
:   sta midistate + MIDIState::beat

    ; edge-case check: tempo is so incredibly fast that more than one beat ticked in a frame
    lda midistate + MIDIState::deltas_til_beat+1
    bmi beat_it 

not_beat:

    ldx #0
trackloop:
    ; is track playable? if not, move on
    lda miditracks + MIDITrack::playable,x
    bne :+
    jmp nexttrack_nosave
:

    inc tracks_playing

    ; subtract a delta per call
    sec
    lda miditracks + MIDITrack::deltaF,x
    sbc midistate + MIDIState::deltas_per_call_frac
    sta miditracks + MIDITrack::deltaF,x

    lda miditracks + MIDITrack::deltaL,x
    sbc midistate + MIDIState::deltas_per_call
    sta miditracks + MIDITrack::deltaL,x

    lda miditracks + MIDITrack::deltaM,x
    sbc midistate + MIDIState::deltas_per_call+1
    sta miditracks + MIDITrack::deltaM,x

    lda miditracks + MIDITrack::deltaH,x
    sbc #0
    sta miditracks + MIDITrack::deltaH,x

    lda miditracks + MIDITrack::deltaU,x
    sbc #0
    sta miditracks + MIDITrack::deltaU,x

    ; if the MSB of the track's delta is positive, we're still
    ; in delay, so move on to the next track
    bmi :+
    jmp nexttrack_nosave
:

    ; set up pointers to track data
    lda miditracks + MIDITrack::curbank,x
    sta X16::Reg::RAMBank
    ldy miditracks + MIDITrack::curoffsetL,x
    stz midizp
    lda miditracks + MIDITrack::curoffsetH,x
    sta midizp+1

eventloop:
    ; if the MSB of the track's delta is positive, we're now
    ; in delay, so move on to the next track
    ldx track_iter
    lda miditracks + MIDITrack::deltaU,x
    bpl nexttrack

    ; we should be pointed directly to an event
    ; right after a delta
    jsr fetch_indirect_byte

    ; if it's below $80, the previous status byte is implied
    cmp #$80
    bcs normal_status

    jsr rewind_indirect_byte

    ; ldx track_iter  X should still be clean here
    lda miditracks + MIDITrack::prevstatus,x
normal_status:
    sta miditracks + MIDITrack::prevstatus,x

    cmp #$90
    bcc event_note_off    ; $80-$8F

    cmp #$A0
    bcc event_note_on     ; $90-$9F

    cmp #$B0
    bcc event_note_aft    ; $A0-$AF

    cmp #$C0
    bcc event_controller  ; $B0-$BF

    cmp #$D0
    bcc event_progchange  ; $C0-$CF

    cmp #$E0
    bcc event_ch_aft      ; $D0-$DF

    cmp #$F0
    bcc event_pitchbend   ; $E0-$EF

    beq event_sysex       ; $F0
    cmp #$F7
    beq event_sysex       ; $F7
    cmp #$FF
    beq event_meta        ; $FF 
    
event_error:
    stz miditracks + MIDITrack::playable,x
    bra nexttrack
event_note_off:
    jsr do_event_note_off
    bra next_event
event_note_on:
    jsr do_event_note_on
    bra next_event
event_note_aft:
    jsr do_event_note_aft
    bra next_event
event_controller:
    jsr do_event_controller
    bra next_event
event_progchange:
    jsr do_event_progchange
    bra next_event
event_ch_aft:
    jsr do_event_ch_aft
    bra next_event
event_pitchbend:
    jsr do_event_pitchbend
    bra next_event
event_sysex: ; $F0/$F7
    jsr do_event_sysex
    bra next_event
event_meta: ; $FF
    jsr do_event_meta
    ; Track could have ended during this tick, so we need to check here
    ldx track_iter
    lda miditracks + MIDITrack::playable,x
    beq nexttrack
    ; bra next_event (no need)
next_event:
    jsr get_delta ; this loads X with track_iter
    bra eventloop

nexttrack:
    ; save track state first
    ldx track_iter

    lda X16::Reg::RAMBank
    sta miditracks + MIDITrack::curbank,x

    tya
    clc
    adc midizp
    sta miditracks + MIDITrack::curoffsetL,x
    lda midizp+1
    sta miditracks + MIDITrack::curoffsetH,x

nexttrack_nosave:
    inc track_iter
    ldx track_iter
    cpx midifile + MIDIFile::ntracks
    bcs tickcallcnt
    jmp trackloop
tickcallcnt:
    ldx #0
tccloop:
    inc ymchannels + YMChannel::callcnt,x
    bne :+
    dec ymchannels + YMChannel::callcnt,x
:
    inx
    cpx #YM2151_CHANNELS
    bcc tccloop

; cumulative deltas
    lda midistate + MIDIState::deltas_per_call_frac
    clc
    adc midistate + MIDIState::deltas_processed_frac
    sta midistate + MIDIState::deltas_processed_frac
    lda midistate + MIDIState::deltas_per_call
    adc midistate + MIDIState::deltas_processed
    sta midistate + MIDIState::deltas_processed
    lda midistate + MIDIState::deltas_per_call+1
    adc midistate + MIDIState::deltas_processed+1
    sta midistate + MIDIState::deltas_processed+1
    bcc end_cumu_delta
    inc midistate + MIDIState::deltas_processed+2
    bne end_cumu_delta
    inc midistate + MIDIState::deltas_processed+3

end_cumu_delta:
    ; adjust tempo if it changed on this tick
    lda midistate + MIDIState::tempochange
    beq check_if_any_playing
    jsr calc_deltas_per_call
    stz midistate + MIDIState::tempochange

check_if_any_playing:
    ; if no tracks are playing, end the song
    lda tracks_playing
    bne :+
    stz midistate + MIDIState::playing
:


end:
    rts
.endproc


; finds an empty YM channel or steals one
; current strategy is in this order:
; 1) return channel with the current MIDI channel and note
; 2) oldest non-playing with current instrument (skipped if MIDI Ch 10)
; 3) oldest non-playing
; 4) oldest playing in MIDI channel 10 if held at least 4 ticks
; 5) oldest playing with the same instrument number (held at least one tick)
; 6) oldest playing that has been held at least 4 ticks
; 7) oldest playing where two channels are playing the same note (same-tick steals okay)
; 8) reject, no channel available
; input - note to play in note_iter
; returns channel to use in X
; trashes Y, unfortunately
.proc find_ymchannel: near
    lda #$ff
    sta tmp2

    ldx #0
samenoteloop:
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne :+
    lda ymchannels + YMChannel::note,x
    cmp note_iter
    bne :+
    stx tmp2
:
    inx
    cpx #YM2151_CHANNELS
    bcc samenoteloop

    ldx tmp2
    bmi :+
    jmp end
:
    ; drums skip the next search
    lda midichannel_iter
    cmp #9
    beq nonplaying

    ; oldest nonplaying w/ current instrument
    stz tmp1 ; callcnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
npciloop:
    lda ymchannels + YMChannel::note,x
    bne :+ ; channel is playing

    lda ymchannels + YMChannel::instrument,x
    ldy midichannel_iter
    cmp midichannels + MIDIChannel::instrument,y
    bne :+ ; channel is using a different instrument

    lda ymchannels + YMChannel::callcnt,x
    cmp tmp1
    bcc :+ ; callcnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc npciloop

    ldx tmp2
    bmi :+
    jmp end
:

nonplaying:
    ; oldest nonplaying
    stz tmp1 ; callcnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
nploop:
    lda ymchannels + YMChannel::note,x
    bne :+ ; channel is playing

    lda ymchannels + YMChannel::callcnt,x
    cmp tmp1
    bcc :+ ; callcnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc nploop

    ldx tmp2
    bmi playing10
    jmp end


playing10:
    ; oldest in MIDI channel 10 held for over $40 ticks
    stz tmp1 ; callcnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
p10loop:
    lda ymchannels + YMChannel::midichannel,x
    cmp #9 ; MIDI channel 10
    bne :+


    lda ymchannels + YMChannel::callcnt,x
    cmp #4
    bcc :+ ; callcnt is less than 4

    cmp tmp1
    bcc :+ ; callcnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc p10loop

    ldx tmp2
    bmi :+
    jmp end
:



playingch:
    ; oldest with same instrument (on any midi channel) playing for at least one tick
    stz tmp1 ; callcnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
pchloop:
    lda ymchannels + YMChannel::instrument,x
    ldy midichannel_iter
    cmp midichannels + MIDIChannel::instrument,y
    bne :+ ; channel is using a different instrument

    lda ymchannels + YMChannel::callcnt,x
    cmp tmp1
    bcc :+ ; callcnt is less than the saved one
    beq :+ ; callcnt is 0 (can't replace on same tick)

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc pchloop

    ldx tmp2
    bpl end



playing:
    ; oldest playing but only older than 4 calls
    stz tmp1 ; callcnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
ploop:
    lda ymchannels + YMChannel::callcnt,x
    cmp #4
    bcc :+ ; callcnt is less than 4

    cmp tmp1
    bcc :+ ; callcnt is less than the current one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc ploop

    ldx tmp2
    bpl end

samenote:
    ; steal oldest playing where two channels are already playing the same note as each other (not necessarily that of the incoming note) MIDI channel 10 is not considered
    ; steals within the same tick are okay here
    stz tmp1
    lda #$ff
    sta tmp2

    ldx #0
snloop1:
    txa
    tay
    iny

    lda ymchannels + YMChannel::midichannel,x
    cmp #9
    beq sl1end
snloop2:
    lda ymchannels + YMChannel::midichannel,y
    cmp #9
    beq sl2end
    lda ymchannels + YMChannel::note,x
    cmp ymchannels + YMChannel::note,y
    bne sl2end

    ; which of these two are oldest
    lda ymchannels + YMChannel::callcnt,x
    cmp ymchannels + YMChannel::callcnt,y
    bcs x_older
y_older:
    lda ymchannels + YMChannel::callcnt,y
    cmp tmp1
    bcc sl2end
    sty tmp2
    sta tmp1
    bra sl2end
x_older: ; or same
    cmp tmp1
    bcc sl2end
    stx tmp2
    sta tmp1
sl2end:
    iny
    cpy #YM2151_CHANNELS
    bcc snloop2

sl1end:
    inx
    cpx #(YM2151_CHANNELS-1)
    bcc snloop1

    ldx tmp2
end:
    rts
.endproc

.proc do_event_note_off: near
    and #$0F
    sta midichannel_iter

    jsr fetch_indirect_byte
    sta note_iter ; note value
    jsr fetch_indirect_byte
    sta cur_velocity ; note volume
from_on:
    sty midizp ; stash Y until the end, we're done reading until the end of this routine

    ldx midichannel_iter
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+

    jsr serial_send_noteoff
    jmp end
:

    ldx #0
ymloop:
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne nextym

    lda ymchannels + YMChannel::note,x
    cmp note_iter
    bne nextym

    ldy midichannel_iter
    lda midichannels + MIDIChannel::damper,y

    beq nopedal
    ; damper pedal is held down, sustain note
    lda #1
    sta ymchannels + YMChannel::undamped,x
    bra end
nopedal:
    ; if it's a drum, don't actually release, except for certain notes
    ; because many midi files have really short note times that make the
    ; drums sound awful
;    cpy #9
;    bne release
;    lda ymchannels + YMChannel::note,x
;    cmp #25 ; drum roll
;    beq release
;    cmp #46 ; open hat
;    beq release
;    bra after_release
release:
    phx
    ; release note
    API_BORDER
    txa
    jsr AudioAPI::ym_release
    MIDI_BORDER
    plx
after_release:
    ; mark the note as released
    stz ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::callcnt,x


    bra end


nextym:
    inx
    cpx #YM2151_CHANNELS
    bcc ymloop

end:
    ldy #0
    rts
.endproc


.proc do_event_note_on: near
    and #$0F
    sta midichannel_iter

    jsr fetch_indirect_byte
    sta note_iter ; note value
    jsr fetch_indirect_byte
    sta cur_velocity ; note volume

    ora #0
    beq do_event_note_off::from_on

    sty midizp ; stash Y until the end, we're done reading until the end of this routine

    ldx midichannel_iter
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+

    jsr serial_send_noteon
    jmp end
:

    ; ignore notes out of range of the YM2151
    lda note_iter
    cmp #109
    jcs end
    cmp #13
    jcc end


    ; find a channel for this note
    jsr find_ymchannel ; returns the one to use in X
    cpx #$FF ; we were rejected
    bne :+
    jmp end
:

    stx ymchannel_iter ; YM channel

    ; set channel attenuation based on velocity
    lda cur_velocity
    sta ymchannels + YMChannel::velocity,x

    VELOCITY_TO_ATTEN

    sta tmp1
    ldx midichannel_iter
    lda midichannels + MIDIChannel::volume,x
    eor #$7F
    lsr
    lsr
    clc
    adc tmp1

    ldx ymchannel_iter
    sta ymchannels + YMChannel::atten,x
    tax
    API_BORDER
    lda ymchannel_iter
    jsr AudioAPI::ym_setatten
    MIDI_BORDER

    ; if it's a drum, we handle differently
    lda midichannel_iter
    cmp #9
    bne checkinst
    jmp drum

checkinst:
    ; check currently-loaded instrument
    ldx midichannel_iter
    lda midichannels + MIDIChannel::instrument,x
    ldx ymchannel_iter
    cmp ymchannels + YMChannel::instrument,x
   
    beq nopatchload ; if instruments are the same, skip load

    ; swap A and X
    tax ; instrument
    API_BORDER
    lda ymchannel_iter ; YM channel
    
    sec
    jsr AudioAPI::ym_loadpatch 
    MIDI_BORDER
nopatchload:
    ; trigger the note
    ldx midichannel_iter
    lda midichannels + MIDIChannel::pitchbend,x

    ldx note_iter
    jsr get_pb_kc_kf

    API_BORDER
    lda ymchannel_iter ; YM channel
;    ldy #0
    clc
    jsr AudioAPI::ym_playnote
    MIDI_BORDER

check_set_pan:
    ldx midichannel_iter
    lda midichannels + MIDIChannel::pan,x

    ldx ymchannel_iter
    cmp ymchannels + YMChannel::pan,x

    beq :+
    sta ymchannels + YMChannel::pan,x

    tax
    API_BORDER
    lda ymchannel_iter
    jsr AudioAPI::ym_setpan
    MIDI_BORDER
    
:

set_ymchannel:
    ; set the YMChannel params for this fresh note
    ldx midichannel_iter
    lda midichannels + MIDIChannel::instrument,x

set_ymchannel_cont:
    ldx ymchannel_iter ; YM channel
    sta ymchannels + YMChannel::instrument,x

    lda midichannel_iter
    sta ymchannels + YMChannel::midichannel,x
    lda note_iter
    sta ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::callcnt,x
    stz ymchannels + YMChannel::undamped,x
    lda track_iter
    sta ymchannels + YMChannel::track,x
end:
    ldy #0
    rts
drum:
    API_BORDER
    lda ymchannel_iter
    ldx note_iter

    jsr AudioAPI::ym_playdrum
    MIDI_BORDER

    lda #3
    ldx ymchannel_iter
    cmp ymchannels + YMChannel::pan,x
    
    beq :+
    sta ymchannels + YMChannel::pan,x

    ldx #3
    API_BORDER
    lda ymchannel_iter
    jsr AudioAPI::ym_setpan
    MIDI_BORDER
:

    lda #$ff
    bra set_ymchannel_cont

.endproc

.proc do_event_sysex: near
    ; stub, skips over it
    jsr get_variable_length ; length

    lda variable_length
    sta chunklen
    lda variable_length+1
    sta chunklen+1
    lda variable_length+2
    sta chunklen+2
    lda variable_length+3
    sta chunklen+3

    jsr advance_to_end_of_chunk
    rts
.endproc


.proc do_event_meta: near
    ; stub, skips over most
    jsr fetch_indirect_byte ; meta type
    sta tmp1
    jsr get_variable_length ; length

    lda variable_length
    sta chunklen
    lda variable_length+1
    sta chunklen+1
    lda variable_length+2
    sta chunklen+2
    lda variable_length+3
    sta chunklen+3

    lda tmp1 ; meta-event-type
    cmp #$2F
    beq end_of_track
    cmp #$51
    beq tempo
    cmp #$05
    beq lyric
    cmp #$01
    beq lyric
    cmp #$58
    jeq timesig
    cmp #$59
    jeq keysig

    bra end
end_of_track:
    ldx track_iter
    stz miditracks + MIDITrack::playable,x
    ; release all notes held by this track's events
    ldx #0
eotloop:
    lda ymchannels + YMChannel::track,x
    cmp track_iter
    bne :+
    stz ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::callcnt,x
    stz ymchannels + YMChannel::undamped,x
    phx
    API_BORDER
    txa
    jsr AudioAPI::ym_release
    MIDI_BORDER
    plx
:
    inx
    cpx #YM2151_CHANNELS
    bcc eotloop


    bra end
tempo:
    stz midistate + MIDIState::tempo+3
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::tempo+2
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::tempo+1
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::tempo+0

    ; handle this at the end of the tick since we need to advance all
    ; the tracks the same on this tick
    inc midistate + MIDIState::tempochange
end:
    jmp advance_to_end_of_chunk
lyric:
    ; don't process first-tick text events
    lda midistate + MIDIState::deltas_processed
    ora midistate + MIDIState::deltas_processed+1
    ora midistate + MIDIState::deltas_processed+2
    ora midistate + MIDIState::deltas_processed+3
    beq end
@l1:
    lda chunklen
    ora chunklen+1
    ora chunklen+2
    beq end

    jsr shift_lyrics
    jsr fetch_indirect_byte_decchunk
    cmp #$20
    bcc @space
    cmp #$7f
    bcc @gol
    lda #'?'
    bra @gol
@space:
    lda #' '
@gol:
    sta lyrics+31
    bra @l1
timesig:
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::timesig_numerator
    jsr fetch_indirect_byte_decchunk ; denominator shifts (2 = quarter note)
    sta midistate + MIDIState::timesig_denominator_shifts
    tax
    lda #1
    cpx #0
    beq :++
:   asl
    dex
    bne :-
:   sta midistate + MIDIState::timesig_denominator
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::timesig_clocks
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::timesig_32nds_per_quarter
    jsr calc_deltas_per_beat
    bra end
keysig:
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::keysig_sharps
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::keysig_mode
    bra end
.endproc

.proc shift_lyrics: near
    ldx #0
@llp:
    lda lyrics+1,x
    sta lyrics,x
    inx
    cpx #31
    bcc @llp
    rts
.endproc

.proc clear_lyrics: near
    ; clear lyrics buffer
    ldx #32
    lda #' '
lyrloop:
    sta lyrics-1,x
    dex
    bne lyrloop

    rts
.endproc

.proc do_event_progchange: near
    and #$0F
    sta midichannel_iter

    jsr fetch_indirect_byte
    ldx midichannel_iter
    sta midichannels + MIDIChannel::instrument,x
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+
    jmp serial_send_progchange
:   rts
.endproc

.proc do_event_note_aft: near
    and #$0F
    sta midichannel_iter

    tax
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+
    jmp serial_send_note_aft
:   jmp do_event_stub
.endproc

.proc do_event_controller: near
    and #$0F
    sta midichannel_iter

    tax

    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+
    jsr serial_send_controller
    jsr rewind_indirect_byte
    jsr rewind_indirect_byte
:
    jsr fetch_indirect_byte
    pha
    jsr fetch_indirect_byte
    sta tmp1
    pla

    sty midizp

    ldx midichannel_iter ; X as midi channel is most useful so do it once

    cmp #$01 ; Modulation wheel
    beq modwheel

    cmp #$06 ; Data entry MSB
    beq dataentry

    cmp #$07 ; Volume
    bne :+
    jmp volume
:

    cmp #$0A ; Pan MSB
    bne :+
    jmp pan
:

    cmp #$40 ; Damper/sustain pedal
    beq damper

    cmp #$64 ; rpn lsb
    beq rpnlsb

    cmp #$65 ; rpn msb
    beq rpnmsb

    cmp #$7F ; All notes off
    bne :+
    jmp all_notes_off
:

end:
    ldy #0
    rts
modwheel:
    lda tmp1
    sta midichannels + MIDIChannel::modwheel,x
    jsr apply_modwheel
    bra end

dataentry:
    ; pitch bend depth
    lda midichannels + MIDIChannel::rpnmsb,x
    bne end 
    lda midichannels + MIDIChannel::rpnlsb,x
    bne end

    lda tmp1
    cmp #25 ; capped at 24, found some midi file requesting something much higher and it was obviously wrong, should have been 2 in that midi file.
    bcs end
    sta midichannels + MIDIChannel::pbdepth,x
    bra end
rpnlsb:
    lda tmp1
    sta midichannels + MIDIChannel::rpnlsb,x
    bra end
rpnmsb:
    lda tmp1
    sta midichannels + MIDIChannel::rpnmsb,x
    bra end


damper:
    lda tmp1
    rol
    rol
    rol
    and #1
    sta midichannels + MIDIChannel::damper,x
    ora #0
    bne end
    
    ; damper changed to 0, release notes if they should be
    ldx #0
@rellp:
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne @rlend

    lda ymchannels + YMChannel::undamped,x
    beq @rlend

    stz ymchannels + YMChannel::undamped,x
    stz ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::callcnt,x
    phx
    API_BORDER
    txa
    jsr AudioAPI::ym_release
    MIDI_BORDER
    plx
@rlend:
    inx
    cpx #YM2151_CHANNELS
    bcc @rellp

    bra end
volume:
    ldx midichannel_iter
    lda tmp1
    sta midichannels + MIDIChannel::volume,x
    ; apply volumes to existing notes
    ldx #0
@vollp:
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne @vlend

    lda ymchannels + YMChannel::velocity,x

    VELOCITY_TO_ATTEN
    
    sta tmp2
    lda tmp1
    eor #$7F
    lsr
    lsr
    clc
    adc tmp2

    sta ymchannels + YMChannel::atten,x

    phx
    pha
    API_BORDER
    txa
    plx
    jsr AudioAPI::ym_setatten
    MIDI_BORDER
    plx

@vlend:
    inx
    cpx #YM2151_CHANNELS
    bcc @vollp
    jmp end

pan:
    ldx midichannel_iter
    lda tmp1
    cmp #$60
    bcs @right
    cmp #$20
    bcc @left

    lda #3
@setpan:
    sta midichannels + MIDIChannel::pan,x
    jmp end
@left:
    lda #1
    bra @setpan
@right:
    lda #2
    bra @setpan

all_notes_off:
    jsr release_channel_notes   
    jmp end
.endproc

; input: midichannel_iter = channel
.proc release_channel_notes: near
    php
    sei

    ldx #0
@anoloop:
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne :+

    lda ymchannels + YMChannel::note,x
    beq :+ ; released already

    stz ymchannels + YMChannel::callcnt,x
    stz ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::undamped,x

    phx
    API_BORDER
    txa
    jsr AudioAPI::ym_release
    MIDI_BORDER
    plx
:
    inx
    cpx #YM2151_CHANNELS
    bcc @anoloop    

    ldx #0
extloop:
    lda vis_ext_ch,x
    cmp midichannel_iter
    bne extend

    phx
    lda #$ff
    sta vis_ext_ch,x
    lda vis_ext_note,x
    sta note_iter
    stz cur_velocity
    jsr serial_send_noteoff
    plx   
extend:
    inx
    cpx #EXT_POLY
    bcc extloop

    lda midichannels + MIDIChannel::ext_enable,x
    beq end

    lda midichannel_iter
    ora #$b0 ; controller
    sta last_serial_cmd
    jsr serial_send_byte

    ; send all notes off
    lda #123
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    ; send damper off
    lda #$40
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

end:
    plp

    rts
.endproc


.proc do_event_ch_aft: near
    and #$0F
    sta midichannel_iter

    tax
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+
    jmp serial_send_ch_aft
:   jmp do_event_stub
.endproc

; A = PB value
; X = midi note
.proc get_pb_kc_kf: near
    ora #0
    beq even

    ; get PB depth
    sta pitch_bend
    stx midi_note
    ldx midichannel_iter
    lda midichannels + MIDIChannel::pbdepth,x
    asl
    sta multiplier

    lda pitch_bend ; load PB value
    cmp #$7F
    beq fullbendup
    sta multiplicand
    
    jsr multiply8x8

    ldy mult_result ; low byte is true KF (8-bit)

    lda mult_result+1 ; overflow into note value
    clc
    adc midi_note
    ldx pitch_bend ; check PB value for negative
    bpl :+
    ldx midichannel_iter
    sec
    sbc midichannels + MIDIChannel::pbdepth,x
    sbc midichannels + MIDIChannel::pbdepth,x
:
    tax
    bra end
fullbendup:
    lda midi_note
    clc
    adc midichannels + MIDIChannel::pbdepth,x
    tax
    lda #0
even:
    tay
end:
    API_BORDER
    jsr AudioAPI::notecon_midi2fm
    MIDI_BORDER
    rts
midi_note:
    .byte $00
pitch_bend:
    .byte $00
.endproc

.proc do_event_pitchbend: near
    and #$0F
    sta midichannel_iter

    tax
    bit midichannels + MIDIChannel::ext_enable,x
    bpl :+
    jmp serial_send_pitchbend
:
    jsr fetch_indirect_byte ; LSB
    rol
    rol
    and #$01
    sta tmp1
    jsr fetch_indirect_byte ; MSB
    sty midizp ; lay down Y, reset at end of routine

    asl
    ora tmp1
    sec
    sbc #$80
    ldx midichannel_iter
    sta midichannels + MIDIChannel::pitchbend,x
    sta tmp2

    ldx #0
bendloop:
    stx tmp1
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne blpend

    ; a pitch-bent note resets this, might help
    ; channel-stealing strategy as bendy notes are likely
    ; lead notes
    stz ymchannels + YMChannel::callcnt,x

    lda ymchannels + YMChannel::note,x

    tax
    lda tmp2
    jsr get_pb_kc_kf ; does not disturb tmp1/tmp2

    API_BORDER
    lda tmp1
    jsr AudioAPI::ym_setnote    
    MIDI_BORDER
blpend:
    ldx tmp1
    inx
    cpx #YM2151_CHANNELS
    bcc bendloop

    ldy #0
    rts
.endproc

.proc do_event_stub: near
    jsr fetch_indirect_byte
    jmp fetch_indirect_byte
.endproc

.proc rewind_indirect_byte: near
    dey
    cpy #$ff
    bne end

    lda midizp+1
    cmp #$A0
    bne decit
    dec X16::Reg::RAMBank 
    lda #$BF
    sta midizp+1
    bra end
decit:
    dec midizp+1
end:
    rts
.endproc

.proc apply_modwheel: near
    ; let's just set the bas_fmvib style here
    stz tmp1
    ldx #0
modloop:
    cpx #9 ; MIDI channel 10 mod doesn't matter
    beq :+
    lda midichannels + MIDIChannel::modwheel,x
    cmp tmp1
    bcc :+
    sta tmp1
:
    inx
    cpx #MIDI_CHANNELS
    bcc modloop
    
    lda tmp1
    lsr
    lsr
    lsr
    tax
    beq write_mod
    clc
    adc #200
write_mod:
    tay
    API_BORDER
    tya
    jsr AudioAPI::bas_fmvib
    MIDI_BORDER
    rts
.endproc

.proc midi_init: near
    ; clear the visual indicators
    ldx #EXT_POLY
    lda #$ff
:   sta vis_ext_ch-1,x
    dex
    bne :-

    ; clear the external flags
    ldx #MIDI_CHANNELS
:   stz midichannels + MIDIChannel::ext_enable - 1,x
    dex
    bne :-

    rts
.endproc

.proc serial_send_byte: near
    pha
    lda IOsTHR
    cmp #$60
    bcc plarts
    lda #LSR_THRE
    ldx #0
:   dex
    beq timeout
    bit $9f00
IOsLSR = * - 2
    beq :-
plasta:
    pla
    sta $9f00
IOsTHR = * - 2
    rts
timeout:
    lda IOsLSR
    sta TO
    lda $9f00
TO = * - 2
    sta debug_byte
plarts:
    pla
    rts

.endproc

.proc midi_serial_init: near
    php
    sei

    tax
    beq :+ ; don't initialize the zero device

    lda #LCR_DLAB
    sta IO_BASE + sLCR, x

    lda #<MIDI_SERIAL_DIVISOR
    sta IO_BASE + sDLL, x

    lda #>MIDI_SERIAL_DIVISOR
    sta IO_BASE + sDLM, x

    lda #LCR_WLS8
    sta IO_BASE + sLCR, x

    lda #(FCR_FIFOE | FCR_RFIFOR | FCR_XFIFOR)
    sta IO_BASE + sFCR, x

    lda #(MCR_DTR | MCR_RTS)
    sta IO_BASE + sMCR, x

    ; disable interrupts
    stz IO_BASE + sIER, x
:
    txa
    clc
    adc #sLSR
    sta serial_send_byte::IOsLSR
    txa
    clc
    adc #sTHR
    sta serial_send_byte::IOsTHR
    
    stz last_serial_cmd

    plp
    rts
.endproc

.proc serial_send_noteoff: near
    lda midichannel_iter
    ora #$80
    ldx cur_velocity
    beq make_on ; note off with vel 0 can be note on
    cpx #$40    ; note off with vel 0x40 can also be note on vel 0
    bne cont_off
    stz cur_velocity
make_on:
    ora #$10
cont_off:
    cmp last_serial_cmd
    sta last_serial_cmd
    beq :+
    jsr serial_send_byte
:   
    ldx #0
duploop:
    lda vis_ext_ch,x
    cmp midichannel_iter
    bne dupend

    lda vis_ext_note,x
    cmp note_iter
    bne dupend

    lda #$ff
    sta vis_ext_ch,x
    bra found
dupend:
    inx
    cpx #EXT_POLY
    bcc duploop

found:
    lda note_iter
    jsr serial_send_byte

    lda cur_velocity
    jmp serial_send_byte
.endproc

.proc serial_send_noteon: near
    lda midichannel_iter
    ora #$90
    cmp last_serial_cmd
    sta last_serial_cmd
    beq :+
    jsr serial_send_byte
:   
    ldx #0
duploop:
    lda vis_ext_ch,x
    cmp midichannel_iter
    bne dupend

    lda vis_ext_note,x
    cmp note_iter
    beq found
dupend:
    inx
    cpx #EXT_POLY
    bcc duploop

    ldx #0
visloop:
    lda vis_ext_ch,x
    cmp #$ff
    bne visend

    lda midichannel_iter
    sta vis_ext_ch,x
    lda note_iter
    sta vis_ext_note,x
    bra found    
visend:
    inx
    cpx #EXT_POLY
    bcc visloop

    lda note_iter
found:
    jsr serial_send_byte

    lda cur_velocity
    jmp serial_send_byte
.endproc

.proc serial_send_note_aft: near
    lda midichannel_iter
    ora #$a0
    cmp last_serial_cmd
    sta last_serial_cmd
    beq :+
    jsr serial_send_byte
:   
    jsr fetch_indirect_byte
    jsr serial_send_byte

    jsr fetch_indirect_byte
    jmp serial_send_byte
.endproc

.proc serial_send_controller: near
    lda midichannel_iter
    ora #$b0
    cmp last_serial_cmd
    sta last_serial_cmd
    beq :+
    jsr serial_send_byte
:   
    jsr fetch_indirect_byte
    jsr serial_send_byte

    jsr fetch_indirect_byte
    jmp serial_send_byte
.endproc

.proc serial_send_progchange: near
    pha
    lda midichannel_iter
    ora #$c0
    sta last_serial_cmd
    jsr serial_send_byte

    pla
    jmp serial_send_byte
.endproc

.proc serial_send_ch_aft: near
    lda midichannel_iter
    ora #$d0
    sta last_serial_cmd
    jsr serial_send_byte

    jsr fetch_indirect_byte
    jsr serial_send_byte

    jsr fetch_indirect_byte
    jmp serial_send_byte
.endproc

.proc serial_send_pitchbend: near
    lda midichannel_iter
    ora #$e0
    sta last_serial_cmd
    jsr serial_send_byte

    jsr fetch_indirect_byte ; LSB

    tax
    rol
    rol
    and #$01
    sta tmp1
    txa

    jsr serial_send_byte

    jsr fetch_indirect_byte ; MSB
    pha
    asl
    ora tmp1
    sec
    sbc #$80
    ldx midichannel_iter
    sta midichannels + MIDIChannel::pitchbend,x
    pla

    jmp serial_send_byte
.endproc
