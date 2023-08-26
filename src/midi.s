.macpack longbranch

.export midi_parse
.export midi_play
.export midi_playtick
.export midi_restart
.export midi_is_playing
.export midi_stop

.export ymnote, yminst, ymmidi, midibend, ympan, ymatten, midiinst
.export lyrics

.import divide40_24
.import multiply16x16
.import multiply8x8

.import numerator
.import denominator
.import div_result
.import multiplicand
.import multiplier
.import mult_result

.include "x16.inc"

.scope AudioAPI
    .include "audio.inc"
.endscope

.include "macros.inc"

.define MAX_TRACKS 32
.define MIDI_CHANNELS 16
.define YM2151_CHANNELS 8

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
    deltas_per_call_frac  .byte
    deltas_per_call       .word
    calls_per_frame       .byte ; any value besides 2 behaves like 1
    playing               .byte
    tempochange           .byte ; flag to adjust deltas at the end of the tick
    deltas_processed_frac .byte
    deltas_processed      .dword ; how far along in the song are we?
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
tmp1:
    .res 1 ; assumed free to use at all times but unsafe after jsr
tmp2:
    .res 1 ; assumed free to use at all times but unsafe after jsr

ymnote := ymchannels + YMChannel::note
yminst := ymchannels + YMChannel::instrument 
ymmidi := ymchannels + YMChannel::midichannel
ympan := ymchannels + YMChannel::pan
ymatten := ymchannels + YMChannel::atten
midibend := midichannels + MIDIChannel::pitchbend
midiinst := midichannels + MIDIChannel::instrument

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

    inx
    cpx #MIDI_CHANNELS
    bcc mloop

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

    clc
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
    
    ; We support either 1 or 2 calls per frame.  If 2, we halve this
    lda midistate + MIDIState::calls_per_frame
    cmp #2
    bne save_deltas

    lsr div_result+2
    ror div_result+1
    ror div_result

save_deltas:

    lda div_result
    sta midistate + MIDIState::deltas_per_call_frac
    lda div_result+1
    sta midistate + MIDIState::deltas_per_call
    lda div_result+2
    sta midistate + MIDIState::deltas_per_call+1

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


.proc do_event_note_on: near
    and #$0F
    sta midichannel_iter

    jsr fetch_indirect_byte
    sta note_iter ; note value
    jsr fetch_indirect_byte
    sta cur_velocity ; note volume

    ora #0
    bne :+
    ; this is a note_off in disguise
    jsr rewind_indirect_byte
    jsr rewind_indirect_byte
    lda midichannel_iter
    jmp do_event_note_off
:


    sty midizp ; stash Y until the end, we're done reading until the end of this routine

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
    jsr advance_to_end_of_chunk
    rts
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

.proc do_event_note_off: near
    and #$0F
    sta midichannel_iter

    jsr fetch_indirect_byte
    sta note_iter ; note value
    jsr fetch_indirect_byte
    sta cur_velocity ; note volume
    sty midizp ; stash Y until the end, we're done reading until the end of this routine

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

.proc do_event_progchange: near
    and #$0F
    sta midichannel_iter

    jsr fetch_indirect_byte
    ldx midichannel_iter

    sta midichannels + MIDIChannel::instrument,x
    rts
.endproc

.proc do_event_note_aft: near
    jmp do_event_stub
.endproc

.proc do_event_controller: near
    and #$0F
    sta midichannel_iter

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
     

pan:
    ldx midichannel_iter
    lda tmp1
    cmp #$70
    bcs @right
    cmp #$10
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
    
    jmp end
.endproc

.proc do_event_ch_aft: near
    jmp do_event_stub
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
