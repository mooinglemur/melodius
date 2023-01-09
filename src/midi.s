

.export midi_parse
.export midi_play
.export midi_playtick
.export midi_restart

.export t0delayF, t0delayL, t0delayM, t0delayH, t0delayU
.export t0bank, t0ptrH, t0ptrL

.import divide40_24
.import multiply16x16

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

.define MAX_TRACKS 24
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
.endstruct

.struct MIDIChannel
    modwheel    .byte MIDI_CHANNELS
    instrument  .byte MIDI_CHANNELS
    volume      .byte MIDI_CHANNELS
    pan         .byte MIDI_CHANNELS
    expression  .byte MIDI_CHANNELS
    damper      .byte MIDI_CHANNELS ; 0 normally, 1 if pedal is pressed
    pitchbend   .byte MIDI_CHANNELS ; signed 8 bit, +127 = +2 semitones
.endstruct

.struct YMChannel
    midichannel .byte YM2151_CHANNELS
    note        .byte YM2151_CHANNELS ; 0 if released
    callcnt     .byte YM2151_CHANNELS ; calls since change of note (255 max)
    instrument  .byte YM2151_CHANNELS ; patch number, drum = $FF
    undamped    .byte YM2151_CHANNELS ; 0 normally, 1 if releasing damper pedal would release note, so this is only set on release
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
tmp1:
    .res 1 ; assumed free to use at all times but unsafe after jsr
tmp2:
    .res 1 ; assumed free to use at all times but unsafe after jsr

t0delayF := miditracks + MIDITrack::deltaF
t0delayL := miditracks + MIDITrack::deltaL
t0delayM := miditracks + MIDITrack::deltaM
t0delayH := miditracks + MIDITrack::deltaH
t0delayU := miditracks + MIDITrack::deltaU

t0bank := miditracks + MIDITrack::curbank
t0ptrH := miditracks + MIDITrack::curoffsetH
t0ptrL := miditracks + MIDITrack::curoffsetL

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
    ; We're resetting the instruments to $FF since we don't know
    ; What patches are loaded into the YM2151's channels
    ldx #0
    lda #$FF
iloop:
    sta ymchannels + YMChannel::instrument,x
    inx
    cpx #YM2151_CHANNELS
    bcc iloop
    rts
.endproc

.proc midi_set_calls_per_frame: near
    sta midistate + MIDIState::calls_per_frame
    rts
.endproc

.proc midi_playtick: near
    ; are we in playback mode?
    ; bail out immediately if not
    lda midistate + MIDIState::playing
    bne :+
    jmp end
:
    
    stz track_iter
    ldx #0
trackloop:
    ; is track playable? if not, move on
    lda miditracks + MIDITrack::playable,x
    bne :+
    jmp nexttrack
:

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
    stp
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
end:
    rts
.endproc


; finds an empty YM channel or steals one
; current strategy is in this order:
; 1) return channel with the current MIDI channel and note
; 2) oldest non-playing with current instrument (skipped if MIDI Ch 10)
; 3) oldest non-playing
; 4) oldest playing in the current MIDI channel
; 5) oldest playing
; input - note to play in note_iter
; returns channel to use in X
; trashes Y, unfortunately
.proc find_ymchannel: near
    ldx #$ff
    stx tmp2

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
    ; drums skip the first search
    lda midichannel_iter
    cmp #9
    beq nonplaying

    ; oldest nonplaying w/ current instrument
    stz tmp1 ; framecnt
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
    bcc :+ ; framecnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc npciloop

    ldx tmp2
    bpl end
nonplaying:
    ; oldest nonplaying
    stz tmp1 ; framecnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
nploop:
    lda ymchannels + YMChannel::note,x
    bne :+ ; channel is playing

    lda ymchannels + YMChannel::callcnt,x
    cmp tmp1
    bcc :+ ; framecnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc nploop

    ldx tmp2
    bpl end

playingch:
    ; oldest in same channel
    stz tmp1 ; framecnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
pchloop:
    lda ymchannels + YMChannel::midichannel,x
    cmp midichannel_iter
    bne :+

    lda ymchannels + YMChannel::callcnt,x
    cmp tmp1
    bcc :+ ; framecnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc pchloop

    ldx tmp2
    bpl end

playing:
    ; oldest in general
    stz tmp1 ; framecnt
    lda #$ff
    sta tmp2 ; ym channel
    ldx #0
ploop:
    lda ymchannels + YMChannel::callcnt,x
    cmp tmp1
    bcc :+ ; framecnt is less than the saved one

    stx tmp2
    sta tmp1
:
    inx
    cpx #YM2151_CHANNELS
    bcc ploop

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
    sty midizp ; stash Y until the end, we're done reading until the end of this routine

    ; find a channel for this note
    jsr find_ymchannel ; returns the one to use in X
    stx ymchannel_iter ; YM channel

    ; set channel attenuation based on velocity
    lda cur_velocity
    eor #$7F
    lsr
    sec
    sbc #$10
    bpl :+
    lda #0
:
    tax
    lda ymchannel_iter
    jsr AudioAPI::ym_setatten

    ; if it's a drum, we handle differently
    lda midichannel_iter
    cmp #9
    beq drum


checkinst:
    ; check currently-loaded instrument
    ldx midichannel_iter
    lda midichannels + MIDIChannel::instrument,x
    ldx ymchannel_iter
    cmp ymchannels + YMChannel::instrument,x
    
    beq nopatchload ; if instruments are the same, skip load

    ; swap A and X
    tax ; instrument
    lda ymchannel_iter ; YM channel
    
    sec
    jsr AudioAPI::ym_loadpatch 
nopatchload:
    ; trigger the note
    ldx midichannel_iter
    lda midichannels + MIDIChannel::pitchbend,x

    ldx note_iter
    jsr get_pb_kc_kf

    lda ymchannel_iter ; YM channel
;    ldy #0
    clc
    jsr AudioAPI::ym_playnote

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

end:
    ldy #0
    rts
drum:
    txa ; YM channel to A
    ldx note_iter

    jsr AudioAPI::ym_playdrum

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

    bra end
end_of_track:
    ldx track_iter
    stz miditracks + MIDITrack::playable,x
    bra end
tempo:
    stz midistate + MIDIState::tempo+3
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::tempo+2
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::tempo+1
    jsr fetch_indirect_byte_decchunk
    sta midistate + MIDIState::tempo+0

    phy
    jsr calc_deltas_per_call
    ply 
end:
    jsr advance_to_end_of_chunk
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
    ; mark the note as released
    stz ymchannels + YMChannel::note,x
    stz ymchannels + YMChannel::callcnt,x

    ; release note
    txa
    jsr AudioAPI::ym_release

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
    jmp do_event_stub
.endproc

.proc do_event_ch_aft: near
    jmp do_event_stub
.endproc

; A = PB value
; X = midi note
.proc get_pb_kc_kf: near
    ora #0
    bmi neg
    cmp #$7F
    beq u2
    cmp #$40
    bcc even
    inx
    bra even
u2:
    inx
    inx
    ldy #0
    bra end
neg:
    cmp #$C0
    bcs d1
d2:
    dex
d1:
    dex
even:
    asl
    asl
    tay
end:
    jmp AudioAPI::notecon_midi2fm
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

    lda ymchannels + YMChannel::note,x
    tax
    lda tmp2
    jsr get_pb_kc_kf

    lda tmp1
    jsr AudioAPI::ym_setnote    
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
