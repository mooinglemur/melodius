.macpack longbranch

.include "x16.inc"

.scope zsmkit
.include "zsmkit.inc"
.endscope

.scope AudioAPI
	.include "audio.inc"
.endscope

.import tileset
.import set_dir_box_size
.import loader_get_ptr
.import playback_mode
.import ticker_behavior
.import jukebox
.import show_directory

.import files_full_size
.import files_width
.import dir_needs_refresh
.import errornum
.import paused

.import ymnote, yminst, ymmidi, midibend, ympan, ymatten, midiinst
.import lyrics
.import midi_set_external
.import midi_serial_init
.import midimeasure, midibeat, midi_tempo, midi_keysig, midi_mode
.import vis_ext_ch, vis_ext_note
.export update_instruments
.export do_midi_sprites
.export do_zsm_sprites
.export setup_sprites
.export setup_tiles
.export setup_instruments
.export hide_sprites
.export draw_file_box
.export draw_pianos
.export draw_loader_ptr
.export draw_zsm_ptr
.export draw_zsm_loop_ptr
.export draw_lyric
.export draw_zsm_tuning
.export clear_zsm_tuning
.export zsm_tuning
.export zsm_tuning_update
.export loading_msg
.export update_midi_beat
.export flash_pause_midi
.export flash_pause_zsm
.export draw_zsm_viz
.export draw_midi_viz
.export legend_jukebox
.export menu_options
.export external_midi_io
.export midi_ext_enable


.segment "BSS"

iterator:
	.res 1
tmp1:
	.res 1
tmp2:
	.res 1
pitchdown:
	.res 1
panright:
	.res 1
instrument_cursor:
	.res 16
instrument:
	.res 16
callcnt:
	.res 1
midinote:
	.res 1
midifrac:
	.res 1
zsm_tuning:
	.res 1
zsm_tuning_update:
	.res 1
external_midi_io:
    .res 1
midi_ext_enable:
	.res 16

.segment "ZEROPAGE"
visptr:
	.res 2

.segment "CODE"

patchnames_l:
	.lobytes p000, p001, p002, p003, p004, p005, p006, p007
	.lobytes p008, p009, p010, p011, p012, p013, p014, p015
	.lobytes p016, p017, p018, p019, p020, p021, p022, p023
	.lobytes p024, p025, p026, p027, p028, p029, p030, p031
	.lobytes p032, p033, p034, p035, p036, p037, p038, p039
	.lobytes p040, p041, p042, p043, p044, p045, p046, p047
	.lobytes p048, p049, p050, p051, p052, p053, p054, p055
	.lobytes p056, p057, p058, p059, p060, p061, p062, p063
	.lobytes p064, p065, p066, p067, p068, p069, p070, p071
	.lobytes p072, p073, p074, p075, p076, p077, p078, p079
	.lobytes p080, p081, p082, p083, p084, p085, p086, p087
	.lobytes p088, p089, p090, p091, p092, p093, p094, p095
	.lobytes p096, p097, p098, p099, p100, p101, p102, p103
	.lobytes p104, p105, p106, p107, p108, p109, p110, p111
	.lobytes p112, p113, p114, p115, p116, p117, p118, p119
	.lobytes p120, p121, p122, p123, p124, p125, p126, p127 
	.lobytes p128, p129
patchnames_h:
	.hibytes p000, p001, p002, p003, p004, p005, p006, p007
	.hibytes p008, p009, p010, p011, p012, p013, p014, p015
	.hibytes p016, p017, p018, p019, p020, p021, p022, p023
	.hibytes p024, p025, p026, p027, p028, p029, p030, p031
	.hibytes p032, p033, p034, p035, p036, p037, p038, p039
	.hibytes p040, p041, p042, p043, p044, p045, p046, p047
	.hibytes p048, p049, p050, p051, p052, p053, p054, p055
	.hibytes p056, p057, p058, p059, p060, p061, p062, p063
	.hibytes p064, p065, p066, p067, p068, p069, p070, p071
	.hibytes p072, p073, p074, p075, p076, p077, p078, p079
	.hibytes p080, p081, p082, p083, p084, p085, p086, p087
	.hibytes p088, p089, p090, p091, p092, p093, p094, p095
	.hibytes p096, p097, p098, p099, p100, p101, p102, p103
	.hibytes p104, p105, p106, p107, p108, p109, p110, p111
	.hibytes p112, p113, p114, p115, p116, p117, p118, p119
	.hibytes p120, p121, p122, p123, p124, p125, p126, p127 
	.hibytes p128, p129

;..........."012345678901"
p000: .byte "GRAND PIANO "
p001: .byte "BRIGHT PIANO"
p002: .byte "ELEC GRAND  "
p003: .byte "HONKY-TONK  "
p004: .byte "ELEC PIANO 1"
p005: .byte "ELEC PIANO 2"
p006: .byte "HARPSICHORD "
p007: .byte "CLAVINET    "
p008: .byte "CELESTA     "
p009: .byte "GLOCKENSPIEL"
p010: .byte "MUSIC BOX   "
p011: .byte "VIBRAPHONE  "
p012: .byte "MARIMBA     "
p013: .byte "XYLOPHONE   "
p014: .byte "TUBULAR BELL"
p015: .byte "DULCIMER    "
p016: .byte "ELEC ORGAN  "
p017: .byte "PERC ORGAN  "
p018: .byte "ROCK ORGAN  "
p019: .byte "PIPE ORGAN  "
p020: .byte "REED ORGAN  "
p021: .byte "ACCORDION   "
p022: .byte "HARMONICA   "
p023: .byte "BANDONEON   "
p024: .byte "NYLON GUITAR"
p025: .byte "STEEL GUITAR"
p026: .byte "JAZZ GUITAR "
p027: .byte "ELEC GUITAR "
p028: .byte "MUTED GUITAR"
p029: .byte "OVERDRIVEN  "
p030: .byte "DIST GUITAR "
p031: .byte "GU HARMONICS"
p032: .byte "ACOU BASS   "
p033: .byte "FINGER BASS "
p034: .byte "PICKED BASS "
p035: .byte "FRETLESS    "
p036: .byte "SLAP BASS 1 "
p037: .byte "SLAP BASS 2 "
p038: .byte "SYNTH BASS 1"
p039: .byte "SYNTH BASS 2"
p040: .byte "VIOLIN      "
p041: .byte "VIOLA       "
p042: .byte "CELLO       "
p043: .byte "CONTRABASS  "
p044: .byte "TREMOLO STR "
p045: .byte "PIZZICATO   "
p046: .byte "HARP        "
p047: .byte "TIMPANI     "
p048: .byte "STRING ENS 1"
p049: .byte "STRING ENS 2"
p050: .byte "SYNTH STR 1 "
p051: .byte "SYNTH STR 2 "
p052: .byte "CHOIR AAHS  "
p053: .byte "VOICE DOOS  "
p054: .byte "SYNTH VOICE "
p055: .byte "ORCH HIT    "
p056: .byte "TRUMPET     "
p057: .byte "TROMBONE    "
p058: .byte "TUBA        "
p059: .byte "MUTE TRUMPET"
p060: .byte "FRENCH HORN "
p061: .byte "BRASS SECT'N"
p062: .byte "SYNTHBRASS 1"
p063: .byte "SYNTHBRASS 2"
p064: .byte "SOPRANO SAX "
p065: .byte "ALTO SAX    "
p066: .byte "TENOR SAX   "
p067: .byte "BARITONE SAX"
p068: .byte "OBOE        "
p069: .byte "ENGLISH HORN"
p070: .byte "BASSOON     "
p071: .byte "CLARINET    "
p072: .byte "PICCOLO     "
p073: .byte "FLUTE       "
p074: .byte "RECORDER    "
p075: .byte "PAN FLUTE   "
p076: .byte "BLOWN BOTTLE"
p077: .byte "SHAKUHACHI  "
p078: .byte "WHISTLE     "
p079: .byte "OCARINA     "
p080: .byte "SQUARE LEAD "
p081: .byte "SAW LEAD    "
p082: .byte "TRI LEAD    "
p083: .byte "CHIFF LEAD  "
p084: .byte "CHARANG LEAD"
p085: .byte "VOICE LEAD  "
p086: .byte "FIFTHS LEAD "
p087: .byte "SOLO LEAD   "
p088: .byte "FANTASIA PAD"
p089: .byte "WARM PAD    "
p090: .byte "POLY PAD    "
p091: .byte "CHOIR PAD   "
p092: .byte "BOWED PAD   "
p093: .byte "METALLIC PAD"
p094: .byte "HALO PAD    "
p095: .byte "SWEEP PAD   "
p096: .byte "RAINDROP    "
p097: .byte "SOUNDTRACK  "
p098: .byte "CRYSTAL     "
p099: .byte "ATMOSPHERE  "
p100: .byte "BRIGHTNESS  "
p101: .byte "GOBLINS     "
p102: .byte "ECHOES      "
p103: .byte "SCI-FI      "
p104: .byte "SITAR       "
p105: .byte "BANJO       "
p106: .byte "SHAMISEN    "
p107: .byte "KOTO        "
p108: .byte "KALIMBA     "
p109: .byte "BAGPIPE     "
p110: .byte "FIDDLE      "
p111: .byte "SHANAI      "
p112: .byte "TINKLE BELL "
p113: .byte "AGOGO       "
p114: .byte "STEEL DRUMS "
p115: .byte "WOODBLOCK   "
p116: .byte "TAIKO DRUM  "
p117: .byte "MELODIC TOM "
p118: .byte "SYNTH DRUM  "
p119: .byte "REV CYMBAL  "
p120: .byte "FRET NOISE  "
p121: .byte "BREATH NOISE"
p122: .byte "SEASHORE    "
p123: .byte "BIRD TWEET  "
p124: .byte "TELEPHONE   "
p125: .byte "HELICOPTER  "
p126: .byte "APPLAUSE    "
p127: .byte "GUNSHOT     "
p128: .byte "            "
p129: .byte "PERCUSSION  "

CURSOR_LINGER = $68
CURSOR_PETSCII = $AD

.proc draw_midi_viz
    ldx #15
    ldy #20
    jsr draw_file_box
    jsr show_directory
    jsr legend_jukebox

    ldx #56
    ldy #52
    jsr X16::Kernal::PLOT

    ldx #0
:   lda midilegend1,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra :-
:

    ldx #14
    ldy #47
    jsr X16::Kernal::PLOT

    ldx #0
:   lda midilegend2,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra :-
:


    ldx #21
    ldy #13
    lda #0
piano_loop3:
    jsr draw_pianos
    sta Vera::Reg::Data0 ; this puts the number under the pianos
    inx
    inc
    cmp #16
    bcc piano_loop3

	rts

midilegend1:
    .byte $90,$01,$05,"MIDI CHANNEL",0

midilegend2:
    .byte $9e,"  KEY TEMPO  BEAT   LOADED",0

.endproc

.proc draw_zsm_viz
    ldx #34
    ldy #7
    jsr draw_file_box
    jsr show_directory
    jsr legend_jukebox
    jsr clear_zsm_tuning

    ; draw pianos
    ldx #12
    ldy #13
    lda #0
piano_loop1:
    jsr draw_pianos
    sta Vera::Reg::Data0 ; this puts the number under the pianos
    inx
    inc
    cmp #16
    bcc piano_loop1

    lda #0
    ldx #3
piano_loop2:
    jsr draw_pianos
    sta Vera::Reg::Data0 ; this puts the number under the pianos
    inx
    inc
    cmp #8
    bcc piano_loop2

    ; draw window for PCM
    VERA_SET_ADDR $87ba, 8
    lda #$20
    ldx #8
:   sta Vera::Reg::Data0
    dex
    bne :-

    ; legend text
    ldy #7
    ldx #56 ; x/y swapped for plot
    clc
    jsr X16::Kernal::PLOT

    ldx #0
lloop1:
    lda legend1,x
    beq :+
    jsr X16::Kernal::CHROUT
    inx
    bne lloop1
:

    ldy #32
    ldx #56 ; x/y swapped for plot
    clc
    jsr X16::Kernal::PLOT

    ldx #0
lloop2:
    lda legend2,x
    beq :+
    jsr X16::Kernal::CHROUT
    inx
    bne lloop2
:


    ldy #61
    ldx #34 ; x/y swapped for plot
    clc
    jsr X16::Kernal::PLOT

    ldx #0
lloop3:
    lda legend3,x
    beq :+
    jsr X16::Kernal::CHROUT
    inx
    bne lloop3
:

    ldy #68
    ldx #30 ; x/y swapped for plot
    clc
    jsr X16::Kernal::PLOT

    ldx #0
lloop4:
    lda legend4,x
    beq :+
    jsr X16::Kernal::CHROUT
    inx
    bne lloop4
:
	rts
legend1:
    .byte $90,$01,$05,"YM2151 CHANNEL",0

legend2:
    .byte "VERA PSG CHANNEL",0

legend3:
    .byte 'V',$11,$9d
    .byte 'E',$11,$9d
    .byte 'R',$11,$9d
    .byte 'A',$11,$11,$9d
    .byte 'P',$11,$9d
    .byte 'C',$11,$9d
    .byte 'M',0               

legend4:
    .byte $9e,"LOOP",$11,$11,$11,$9d,$9d,$9d,$9d,$9d
    .byte "CURSOR",$11,$11,$11,$9d,$9d,$9d,$9d,$9d,$9d
    .byte "LOADED",$05,0
.endproc

.proc print_hex
	jsr byte_to_hex
	jsr X16::Kernal::CHROUT
	txa
	jsr X16::Kernal::CHROUT
	
	rts
.endproc

.proc byte_to_hex ; converts a number to two ASCII/PETSCII hex digits: input A = number to convert, output A = most sig nybble, X = least sig nybble, affects A,X
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
.endproc


.proc legend_jukebox
    ldx #4
    ldy #42
    clc
    jsr X16::Kernal::PLOT
    ldx #0
legloop:
    lda legend,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra legloop

:   lda jukebox
    asl
    tax
    lda lut,x
    sta TYP
    lda lut+1,x
    sta TYP+1
    ldx #0
typloop:
    lda $ffff,x
TYP = * - 2
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra typloop
:
    ldx #11
    ldy #6
    clc
    jsr X16::Kernal::PLOT

    ldx #0
banloop:
    lda banner,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra banloop
:

    ldx #6
    ldy #42
    clc
    jsr X16::Kernal::PLOT
    ldx #0
legbloop:
    lda legendb,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra legbloop
:
    ldx #7
    ldy #47
    clc
    jsr X16::Kernal::PLOT
    lda ticker_behavior
    asl
    tax
    lda lutb,x
    sta TYPB
    lda lutb+1,x
    sta TYPB+1
    ldx #0
typbloop:
    lda $ffff,x
TYPB = * - 2
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra typbloop
:

    ldx #9
    ldy #42
    clc
    jsr X16::Kernal::PLOT
    ldx #0
legcloop:
    lda legendc,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra legcloop
:
    ldx #10
    ldy #42
    clc
    jsr X16::Kernal::PLOT
    ldx #0

legdloop:
    lda legendd,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra legdloop

:
    ldx #11
    ldy #42
    clc
    jsr X16::Kernal::PLOT
    ldx #0

legeloop:
    lda legende,x
    beq :+
    jsr X16::Kernal::BSOUT
    inx
    bra legeloop

:
end:
    rts
banner:
    .byte "by MooingLemur, "
    .byte "built on "
    .incbin "releasedate.inc"
    .byte 0
legend:
    .byte $90,$01,$05,"[F1] Playback Mode: ",0
lut:
    .word type0, type1, type2
type0:
    .byte "Single ",0
type1:
    .byte "Sequential",0
type2:
    .byte "Shuffle   ",0
legendb:
    .byte "[F2] Use VIA1 timer for ZSM:",0
lutb:
    .word type0b, type1b, type2b
type0b:
    .byte "When ZSM rate is not 60 Hz",0
type1b:
    .byte "Always                    ",0
type2b:
    .byte "Never (only use VSYNC)    ",0
legendc:
    .byte "[Enter] Load  [Space] (Un)pause",0
legendd:
    .byte "[Up/Dn/PgUp/PgDn/Home/End] Move",0
legende:
    .byte "[Tab] Next   [Shift+Space] Stop",0



.endproc

; Input: .X = y coord, .Y = x coord
.proc draw_loader_ptr
	ldx #15
	ldy #66
	lda playback_mode
	cmp #$11
	beq :+
	cmp #1
	beq :+
	ldx #37
:	clc
	jsr X16::Kernal::PLOT
	; white on black
	lda #$90
	jsr X16::Kernal::BSOUT
	lda #$01
	jsr X16::Kernal::BSOUT
	lda #$05
	jsr X16::Kernal::BSOUT

	lda #'$'
	jsr X16::Kernal::BSOUT
	jsr loader_get_ptr
	phx
	jsr print_hex
	lda #':'
	jsr X16::Kernal::BSOUT
	tya
	jsr print_hex
	pla
	jmp print_hex
.endproc

; Input: .X = y coord, .Y = x coord
.proc draw_zsm_loop_ptr
	ldx #31
	ldy #66
	clc
	jsr X16::Kernal::PLOT

	; white on black
	lda #$90
	jsr X16::Kernal::BSOUT
	lda #$01
	jsr X16::Kernal::BSOUT
	lda #$05
	jsr X16::Kernal::BSOUT

	lda #'$'
	jsr X16::Kernal::BSOUT
	lda #1 ; zsmkit bank
	sta X16::Reg::RAMBank

	lda zsmkit::loop_enable ; prio 0
	beq blank
	lda zsmkit::zsm_loop_bank ; prio 0
	jsr print_hex
	lda #':'
	jsr X16::Kernal::BSOUT
	lda zsmkit::zsm_loop_h ; prio 0
	jsr print_hex
	lda zsmkit::zsm_loop_l ; prio 0
	jmp print_hex
blank:
	lda #'-'
	jsr X16::Kernal::BSOUT
	jsr X16::Kernal::BSOUT
	lda #':'
	jsr X16::Kernal::BSOUT
	lda #'-'
	jsr X16::Kernal::BSOUT
	jsr X16::Kernal::BSOUT
	jsr X16::Kernal::BSOUT
	jmp X16::Kernal::BSOUT
.endproc

.proc draw_zsm_ptr
	ldx #34
	ldy #66
	clc
	jsr X16::Kernal::PLOT

	; white on black
	lda #$90
	jsr X16::Kernal::BSOUT
	lda #$01
	jsr X16::Kernal::BSOUT
	lda #$05
	jsr X16::Kernal::BSOUT

	lda #'$'
	jsr X16::Kernal::BSOUT
	lda #1 ; zsmkit bank
	sta X16::Reg::RAMBank
	lda zsmkit::zsm_ptr_bank ; prio 0
	jsr print_hex
	lda #':'
	jsr X16::Kernal::BSOUT
	lda zsmkit::zsm_ptr_h ; prio 0
	jsr print_hex
	lda zsmkit::zsm_ptr_l ; prio 0
	jmp print_hex
.endproc

.proc clear_zsm_tuning
	ldx #52
	ldy #67
	clc
	jsr X16::Kernal::PLOT

	ldx #0
lloop:
    lda legend,x
    beq :+
    jsr X16::Kernal::CHROUT
    inx
    bne lloop
:	rts
legend:
	.byte "      ",$11,$9d,$9d,$9d,$9d,$9d,$9d,$9d
	.byte "        ",0
.endproc

.proc draw_zsm_tuning
	lda zsm_tuning_update
	beq end
	stz zsm_tuning_update
	ldx #52
	ldy #67
	clc
	jsr X16::Kernal::PLOT

    ldx #0
lloop:
    lda legend,x
    beq :+
    jsr X16::Kernal::CHROUT
    inx
    bne lloop
:	lda zsm_tuning
	bpl plus
	lda #'-'
	bra sign
plus:
	lda #'+'
sign:
	jsr X16::Kernal::CHROUT

	lda zsm_tuning
	bpl pos
	eor #$ff
	inc
pos:
	lsr
	lsr
	tax
	lda tuning,x
	jsr print_hex

    ldx #0
cloop:
    lda cents,x
    beq end
    jsr X16::Kernal::CHROUT
    inx
    bne cloop
end:
	rts
legend:
	.byte $90,$01,$9e,"TUNING",$11,$9d,$9d,$9d,$9d,$9d,$9d,$9d,$05,0
cents:
	.byte " CENT",0
tuning:
	.byte $00,$02,$03,$05,$06,$08,$09,$11
	.byte $13,$14,$16,$17,$19,$20,$22,$23
	.byte $25,$27,$28,$30,$31,$33,$34,$36
	.byte $38,$39,$41,$42,$44,$45,$47,$48,$50
.endproc

.proc flash_pause_zsm
	ldx #39
	ldy #67
	clc
	jsr X16::Kernal::PLOT

	lda paused
	beq blank

	jsr X16::Kernal::RDTIM
	and #$10
	beq blank

	ldx #0
:	lda pausetxt,x
	beq :+
	jsr X16::Kernal::CHROUT
	inx
	bra :-
:	rts

blank:
	ldx #0
:	lda blanktxt,x
	beq :+
	jsr X16::Kernal::CHROUT
	inx
	bra :-
:	rts
blanktxt:
	.byte $90,$01,$05,"      ",0
pausetxt:
	.byte $05,$01,$1f,"PAUSED",0
.endproc

.proc flash_pause_midi
	ldx #28
	ldy #55
	clc
	jsr X16::Kernal::PLOT

	lda paused
	beq blank

	jsr X16::Kernal::RDTIM
	and #$10
	beq blank

	ldx #0
:	lda pausetxt,x
	beq :+
	jsr X16::Kernal::CHROUT
	inx
	bra :-
:	rts

blank:
	ldx #0
:	lda blanktxt,x
	beq :+
	jsr X16::Kernal::CHROUT
	inx
	bra :-
:	rts
blanktxt:
	.byte $90,$01,$05,"      ",0
pausetxt:
	.byte $05,$01,$1f,"PAUSED",0
.endproc


.proc loading_msg
    ; 0 = loading, 1 = sorting, 2 = preloading pcm
	asl
	tay

	lda msg_idx,y
	sta MSG
	lda msg_idx+1,y
	sta MSG+1

	lda files_width
	lsr
	dec
	tay ; column number for PLOT

	lda #14
	tax ; row number for plot

	jsr X16::Kernal::PLOT

	ldx #0
p1:
	lda preamble,x
	beq p2
	jsr X16::Kernal::BSOUT
	inx
	bra p1

p2:
	ldx #0
p3:
	lda $ffff,x
MSG = * - 2
	beq p4
	jsr X16::Kernal::BSOUT
	inx
	bra p3
p4:
	lda errornum
	bne :+
	inc dir_needs_refresh
:
	rts
	
preamble:
	.byte $1c,$01,$05,0 ; red on white
msg_idx:
	.word loadn, sortn, preloadn, too_big, unrecognized
loadn:
	.byte "    LOADING     ",0
sortn:
	.byte "    SORTING     ",0
preloadn:
	.byte " PRELOADING PCM ",0
too_big:
	.byte " FILE TOO LARGE ",0
unrecognized:
	.byte "UNKNOWN FILETYPE",0
.endproc


; x/y dimensions of box (16 pixel tiles)
.proc draw_file_box
	; don't do it if it wouldn't change things
	cpy oldrows
	bne :+
	cpx oldcols
	bne :+

	rts

:   sty rows
	sty oldrows
	tya
	asl
	tay
	stx cols
	stx oldcols
	txa
	asl
	tax
	jsr set_dir_box_size

	; implies a clear screen (text)
	jsr clear_screen

	; clear most of screen
	VERA_SET_ADDR $8300, 1

	ldx #6
	ldy #0

cloop:
	lda #$12
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0
	dey
	bne cloop
	dex
	bne cloop

	; now start drawing the box
	VERA_SET_ADDR $8304, 1

	; legend and top
	ldx #0
tploop:
	lda top_preamble,x
	sta Vera::Reg::Data0
	inx
	cpx #6
	bne tploop

	; rest of top (border)
	ldy cols
	dey
	dey
tploop2:
	lda #$21
	sta Vera::Reg::Data0
	lda #$18
	sta Vera::Reg::Data0
	dey
	bne tploop2

	; upper right corner
	lda #$23
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0

mainloop:
	jsr eat_it

	; left side
	lda #$18
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0

	ldy cols
midloop:
	lda #$2f
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0
	dey
	bne midloop

	; right side
	lda #$24
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0

	dec rows
	bne mainloop


	jsr eat_it

	; bottom edge
	lda #$25
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0

	ldy cols
botloop:
	lda #$21
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0
	dey
	bne botloop

	lda #$22
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0

	rts

eat_it:
	lda #62
	sec
	sbc cols
	tay
@eatloop:
	lda Vera::Reg::Data0
	lda Vera::Reg::Data0
	dey
	bne @eatloop
	rts

top_preamble:
	.byte $23,$14,$1c,$10,$1d,$10
rows:
	.byte 0
cols:
	.byte 0
oldrows:
	.byte 0
oldcols:
	.byte 0
.endproc


.proc draw_lyric: near
	ldx #29
	ldy #42
	clc
	jsr X16::Kernal::PLOT

	lda #$90
	jsr X16::Kernal::BSOUT
	lda #$01
	jsr X16::Kernal::BSOUT
	lda #$9f
	jsr X16::Kernal::BSOUT

	ldx #0
:	lda lyrics,x
	jsr X16::Kernal::BSOUT
	inx
	cpx #32
	bcc :-

	; get out of possible quote mode
	lda #13
	jsr X16::Kernal::BSOUT

	rts
.endproc

.proc setup_instruments: near
	ldx #0
	lda #$90 ; black
	jsr X16::Kernal::CHROUT
	lda #$01 ; background
	jsr X16::Kernal::CHROUT
midiloop:
	lda #$FF
	sta instrument,x
	stz instrument_cursor,x

	jsr point_cursor

	lda #$9A ; light blue
	jsr X16::Kernal::CHROUT

	txa
	inc
	jsr get_decimal ; returns in A and Y
	pha
	tya
	jsr X16::Kernal::CHROUT
	pla
	jsr X16::Kernal::CHROUT

	lda #$05 ; white
	jsr X16::Kernal::CHROUT

	inx
	cpx #16
	bne midiloop

	; redefine cursor
	VERA_SET_ADDR (Vera::VRAM_charset+((CURSOR_PETSCII)*8)), 1
	lda #$7E
	sta Vera::Reg::Data0
	sta Vera::Reg::Data0
	sta Vera::Reg::Data0
	sta Vera::Reg::Data0
	sta Vera::Reg::Data0
	sta Vera::Reg::Data0
	sta Vera::Reg::Data0
	stz Vera::Reg::Data0
	rts

point_cursor:
	; position the text cursor
	phx ; save our midi channel iterator
	txa
	and #7
	clc
	adc #20
	pha ; Y coordinate
	lda #42
	cpx #8 ; second column
	bcc :+
	clc
	adc #16
:   tay ; X coordinate goes in Y register and
	plx ; Y coordinate goes in X register :(
	jsr X16::Kernal::PLOT ; carry is clear, set position
	plx ; restore midi channel iterator
	rts
.endproc

.proc get_decimal
	ldy #'0'
	cmp #10
	bcc :+
	sec
	sbc #10
	ldy #'1'
:   clc
	adc #'0'
	rts
.endproc

.proc update_instruments: near
	; make white on black
	lda #$90
	jsr X16::Kernal::CHROUT
	lda #1
	jsr X16::Kernal::CHROUT
	lda #$05
	jsr X16::Kernal::CHROUT

	inc callcnt
	ldx #0
instloop:
	lda midiinst,x
	cmp instrument,x
	bne instchange
	
	ldy instrument_cursor,x
	cpy #CURSOR_LINGER ; turn off cursor
	beq turn_off_cursor
	bcs ilend

	cpy #12 ; at the end, do nothing
	bcs blink_cursor

	lda instrument_cursor,x
	sta tmp1
	jsr point_cursor

	lda #129 ; drums
	cpx #9
	beq :+
	lda instrument,x
	cmp #128
	bcc :+
	lda #128
:   tay
	lda patchnames_l,y
	sta visptr
	lda patchnames_h,y
	sta visptr+1
	ldy instrument_cursor,x
	lda (visptr),y
	jsr X16::Kernal::CHROUT
	lda #CURSOR_PETSCII
	jsr X16::Kernal::CHROUT

cursor_end:
	inc instrument_cursor,x
	bra ilend
blink_cursor:
	lda callcnt
	and #$10
	beq turn_off_cursor
	lda #12
	sta tmp1
	jsr point_cursor
	lda #CURSOR_PETSCII
	jsr X16::Kernal::CHROUT
	bra cursor_end

turn_off_cursor:
	lda #12
	sta tmp1
	jsr point_cursor
	lda #$20
	jsr X16::Kernal::CHROUT
	inc instrument_cursor,x
ilend:
	inx
	cpx #16
	bcs :+
	jmp instloop
:
	rts
instchange:
	pha
	; remove any cursor before redrawing name
	lda instrument_cursor,x
	cmp #12
	bcc :+
	lda #12
:
	sta tmp1
	jsr point_cursor
	lda #$20
	jsr X16::Kernal::CHROUT

	pla
	sta instrument,x
	stz instrument_cursor,x
	bra ilend
point_cursor:
	; position the text cursor
	phx ; save our midi channel iterator
	txa
	and #7
	clc
	adc #20
	pha ; Y coordinate
	lda #45
	cpx #8 ; second column
	bcc :+
	clc
	adc #16
:   adc tmp1
	tay ; X coordinate goes in Y register and
	plx ; Y coordinate goes in X register :(
	jsr X16::Kernal::PLOT ; carry is clear, set position
	plx ; restore midi channel iterator
	rts
.endproc

.proc hide_sprites: near
	stz Vera::Reg::Ctrl
	lda #<Vera::VRAM_sprattr
	sta Vera::Reg::AddrL
	lda #>Vera::VRAM_sprattr
	sta Vera::Reg::AddrM
	lda #^Vera::VRAM_sprattr
	ora #$10 ; auto increment = 1
	sta Vera::Reg::AddrH
	ldy #0
loop:
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	dey
	bne loop

	rts
.endproc

.proc update_midi_beat: near
	ldx #15
	ldy #49
	jsr X16::Kernal::PLOT

	; make white on black
	lda #$90
	jsr X16::Kernal::CHROUT
	lda #1
	jsr X16::Kernal::CHROUT
	lda #$05
	jsr X16::Kernal::CHROUT

	; write keysig
	lda midi_keysig
	cmp #$80
	beq unspec_keysig
	clc
	adc #7

	ldx midi_mode
	beq :+
	adc #3 ; minor key offset in table

	; clamp to 0-14 (-7 to +7)
:	bpl :+
	adc #12
	bra :-
:	cmp #15
	bcc :+
	sbc #12
	bra :-

:	asl
	tax
	lda keysigtbl,x
	jsr X16::Kernal::CHROUT
	lda keysigtbl+1,x
	jsr X16::Kernal::CHROUT

	ldx #' '
	lda midi_mode
	beq :+
	ldx #'m'
:	txa
	jsr X16::Kernal::CHROUT
	bra dotempo
unspec_keysig:
	lda #'-'
	jsr X16::Kernal::CHROUT
	jsr X16::Kernal::CHROUT
	jsr X16::Kernal::CHROUT

dotempo:
	lda #' '
	jsr X16::Kernal::CHROUT

	ldx #0
	; write tempo
tl:
	lda midi_tempo,x
	clc
	adc #$30
	jsr X16::Kernal::CHROUT
	inx
	cpx #3
	bne tl
	lda #'.'
	jsr X16::Kernal::CHROUT
	lda midi_tempo+3
	clc
	adc #$30
	jsr X16::Kernal::CHROUT
	lda #' '
	jsr X16::Kernal::CHROUT

	lda midimeasure+1
	jsr byte_to_hex
	txa ; only print low byte
	jsr X16::Kernal::CHROUT
	lda midimeasure
	jsr print_hex
	lda #':'
	jsr X16::Kernal::CHROUT
	lda midibeat
	jsr print_hex
	rts
keysigtbl:
	.byte "Cb"
	.byte "Gb"
	.byte "Db"
	.byte "Ab"
	.byte "Eb"
	.byte "Bb"
	.byte " F"
	.byte " C"
	.byte " G"
	.byte " D"
	.byte " A"
	.byte " E"
	.byte " B"
	.byte "F#"
	.byte "C#"
.endproc

.proc do_midi_sprites: near
	stz iterator

	stz Vera::Reg::Ctrl
	lda #<Vera::VRAM_sprattr
	sta Vera::Reg::AddrL
	lda #>Vera::VRAM_sprattr
	sta Vera::Reg::AddrM
	lda #^Vera::VRAM_sprattr
	ora #$10 ; auto increment = 1
	sta Vera::Reg::AddrH

	ldx iterator
sploop:
	lda ymnote,x
	bne :+
	jmp hideit
:

	lda ymmidi,x
	tay

	stz pitchdown
	stz panright

	lda yminst,x
	lsr
	lsr
	lsr
	and #$0F
	inc
	cmp #$10
	bne :+
	lda #1
:
	asl
	asl

	sta tmp1
	stz tmp2

	cpy #9
	beq endbend ; channel 10, don't pan or pitch

chkpan:
	lda ympan,x
	cmp #3
	beq chkpitch

	cmp #1
	beq :+
	inc panright
:
	lda tmp1
	clc
	adc #64
	sta tmp1
	lda #0
	adc #0
	sta tmp2
chkpitch:
	lda midibend,y
	beq endbend
	bpl contbend

	ldy #2
	sty pitchdown

	cmp #$C0
	bcc hardbend
	bra softbend

contbend:
	cmp #$40
	bcs hardbend
softbend:
	lda tmp1
	clc
	adc #128
	sta tmp1
	lda #0
	adc #0
	sta tmp2
	bra endbend
hardbend:
	inc tmp2
endbend:
	lda tmp1
	sta Vera::Reg::Data0

	; no high bits, mode 0
	lda tmp2
	sta Vera::Reg::Data0

	; multiply MIDI channel by 16
	lda ymmidi,x
	asl
	asl
	asl
	asl
	
	; add #320 and drop the X coord
	clc
	adc #<(336)
	sta Vera::Reg::Data0
	lda #>(336)
	adc #0
	sta Vera::Reg::Data0

	; note is Y coord
	lda #255
	sec
	sbc ymnote,x
	sbc ymnote,x

	; bring it downward on the screen by 194
	clc
	adc #194
	sta Vera::Reg::Data0

	lda #0
	adc #0
	sta Vera::Reg::Data0

	; set the Z depth / flip
	lda #$0C
	ora pitchdown
	ora panright
	sta Vera::Reg::Data0

	; get channel volume (attenuation)
	lda ymatten,x
	cmp #$3f
	bcc :+
	lda #$3f
:	and #$30
	lsr
	lsr
	lsr

	; set 16x16
	adc #$52 ; and pal offset 2
	sta Vera::Reg::Data0
	bra splend
	

hideit:
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
splend:
	inc iterator
	ldx iterator
	cpx #8
	bcs ext
	jmp sploop

ext:
	; now we do external MIDI sprites
	stz iterator
ext_loop:
	ldx iterator
	lda vis_ext_ch,x
	bmi ext_hideit

	; address 0 contains the external sprite for now
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0

	; multiply MIDI channel by 16
	lda vis_ext_ch,x
	asl
	asl
	asl
	asl
	
	; add #320 and drop the X coord
	clc
	adc #<(336)
	sta Vera::Reg::Data0
	lda #>(336)
	adc #0
	sta Vera::Reg::Data0

	; note is Y coord
	lda #255
	sec
	sbc vis_ext_note,x
	sbc vis_ext_note,x

	; bring it downward on the screen by 194
	clc
	adc #194
	sta Vera::Reg::Data0

	lda #0
	adc #0
	sta Vera::Reg::Data0

	; set the Z depth / flip
	lda #$0C
	sta Vera::Reg::Data0

	; get channel volume (attenuation)
	lda #0 ; always 0 here
	; set 16x16
	adc #$52 ; and pal offset 2
	sta Vera::Reg::Data0
	bra ext_end

ext_hideit:
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
ext_end:
	inc iterator
	ldx iterator
	cpx #32
	bcs end
	jmp ext_loop
end:
	rts
.endproc

.proc do_zsm_sprites
	stz Vera::Reg::Ctrl
	lda #<Vera::VRAM_sprattr
	sta Vera::Reg::AddrL
	lda #>Vera::VRAM_sprattr
	sta Vera::Reg::AddrM
	lda #^Vera::VRAM_sprattr
	ora #$10 ; auto increment = 1
	sta Vera::Reg::AddrH

	lda #1
	sta X16::Reg::RAMBank

	jsr do_zsm_pcm_sprite
	jsr do_zsm_psg_sprites
	; fall through to do_zsm_ym_sprites
.endproc


.proc do_zsm_ym_sprites: near
	stz iterator

	lda #1
	sta X16::Reg::RAMBank

	ldx iterator
sploop:
	lda zsmkit::opm_key_shadow,x
	and #$38
	jeq hideit

	lda zsmkit::opm_shadow+$28,x
	tax

	JSRFAR AudioAPI::notecon_fm2midi, $0a
	bcs high
	stx midinote
	
	ldx iterator
	lda zsmkit::opm_shadow+$30,x

	bpl :+
	inc midinote
:   sta midifrac ; signed


	lda zsm_tuning
	bpl subtuning
	eor $ff
	inc
addtuning:
	clc
	adc midifrac
	sta midifrac
	bvc :+
	inc midinote
:	bra aftertuning
subtuning:
	lda midifrac
	sec
	sbc zsm_tuning
	sta midifrac
	bvc :+
	dec midinote
:
aftertuning:
	lda midinote
	cmp #108
high:
	bcc :+
	lda #108
	sta midinote
:   cmp #12
	bcs :+
	lda #12
	sta midinote
:

	stz pitchdown
	stz panright

	ldx iterator
	lda zsmkit::opm_shadow+$20,x
	and #$07
	tay
	lda wav2color,y
	asl
	asl

	sta tmp1
	stz tmp2

chkpan:
	lda zsmkit::opm_shadow+$20,x
	rol
	rol
	rol
	and #3
	cmp #3
	beq chkpitch

	cmp #1
	beq :+
	inc panright
:
	lda tmp1
	clc
	adc #64
	sta tmp1
	lda #0
	adc #0
	sta tmp2
chkpitch:
	lda midifrac
	beq endbend
	bpl contbend

	cmp #$C0
	bcs endbend

	ldy #2
	sty pitchdown
	bra softbend
contbend:
	cmp #$40
	bcc endbend
softbend:
	lda tmp1
	clc
	adc #128
	sta tmp1
	lda #0
	adc #0
	sta tmp2
	bra endbend
hardbend:
	inc tmp2
endbend:
	lda tmp1
	sta Vera::Reg::Data0

	; no high bits, mode 0
	lda tmp2
	sta Vera::Reg::Data0

	; multiply PSG channel by 16
	lda iterator
	asl
	asl
	asl
	asl
	
	; add #320 and drop the X coord
	clc
	adc #<(48)
	sta Vera::Reg::Data0
	lda #>(48)
	adc #0
	sta Vera::Reg::Data0

	; note is Y coord
	lda #255
	sec
	sbc midinote
	sbc midinote

	; bring it downward on the screen by 194
	clc
	adc #194
	sta Vera::Reg::Data0

	lda #0
	adc #0
	sta Vera::Reg::Data0

	; set the Z depth / flip
	lda #$0C
	ora pitchdown
	ora panright
	sta Vera::Reg::Data0

	; get the alg
	lda zsmkit::opm_shadow+$20,x
	and #$07
	tay
	lda alg2mask,y
	sta tmp1
	; set max atten we'll process
	lda #$3f
	sta tmp2
m1:
	lsr tmp1
	bcc m2
	lda zsmkit::opm_shadow+$60,x
	cmp tmp2
	bcs m2
	sta tmp2
m2:
	lsr tmp1
	bcc c1
	lda zsmkit::opm_shadow+$68,x
	cmp tmp2
	bcs c1
	sta tmp2
c1:
	lsr tmp1
	bcc c2
	lda zsmkit::opm_shadow+$70,x
	cmp tmp2
	bcs c2
	sta tmp2
c2: ; C2 is always a carrier
	lda zsmkit::opm_shadow+$78,x
	cmp tmp2
	bcs vol
	sta tmp2
vol:
	lda tmp2
	and #$30
	lsr
	lsr
	lsr
	; set 16x16
	adc #$52 ; and pal offset 2
	sta Vera::Reg::Data0
	bra splend
	
hideit:
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
splend:
	inc iterator
	ldx iterator
	cpx #8
	bcs end
	jmp sploop

end:
	rts
alg2mask:
	.byte $08,$08,$08,$08,$0c,$0e,$0e,$0f
wav2color:
	.byte 12,2,15,4,6,1,8,7
.endproc

.proc do_zsm_pcm_sprite: near
	lda zsmkit::pcm_busy
	beq hideit

	lda Vera::Reg::AudioCtrl
	lsr
	lsr
	lsr
	lsr
	and #$03
	tay
	lda wav2color,y
	asl
	asl

	sta Vera::Reg::Data0
	stz Vera::Reg::Data0

	; add offset and drop the X coord
	lda #<(464)
	sta Vera::Reg::Data0
	lda #>(464)
	sta Vera::Reg::Data0

	; note is Y coord
	lda #128
	sec
	sbc Vera::Reg::AudioRate

	; bring it downward on the screen
	clc
	adc #233
	sta Vera::Reg::Data0

	lda #0
	adc #0
	sta Vera::Reg::Data0

	; set the Z depth / flip
	lda #$0C
	sta Vera::Reg::Data0

	lda Vera::Reg::AudioCtrl
	eor #$ff
	and #$0c
	lsr
	; set 16x16
	adc #$52 ; and pal offset 2
	
	sta Vera::Reg::Data0

	rts
hideit:
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0

	rts
wav2color:
	.byte 1,8,15,14
.endproc

.proc do_zsm_psg_sprites: near
	stz iterator

	ldx iterator
sploop:
	txa
	asl
	asl
	tax
	lda zsmkit::vera_psg_shadow+2,x
	and #$3f
	jeq hideit

	lda zsmkit::vera_psg_shadow+1,x
	tay
	lda zsmkit::vera_psg_shadow+0,x
	tax

	stz midifrac
	JSRFAR AudioAPI::notecon_psg2midi, $0a
	bcc :+
	ldx #127
	ldy #$7f
:	stx midinote
	tya
	bpl :+
	inc midinote
:   sta midifrac ; signed


	lda zsm_tuning
	bpl subtuning
	eor $ff
	inc
addtuning:
	clc
	adc midifrac
	sta midifrac
	bvc :+
	inc midinote
:	bra aftertuning
subtuning:
	lda midifrac
	sec
	sbc zsm_tuning
	sta midifrac
	bvc :+
	dec midinote
:
aftertuning:
	; first bounds check
	lda midinote
    cmp #12
	bcs :+
	lda #12
	sta midinote
:
	
	stz pitchdown
	stz panright

	lda iterator
	asl
	asl
	tax
	lda zsmkit::vera_psg_shadow+3,x
	rol
	rol
	rol
	and #$03
	tay
	bne notsq ; colorful based on duty
	lda #$53
	sta PAL
	lda zsmkit::vera_psg_shadow+3,x
	and #$3c
	bne aftersq
	lda #$04
	bra aftersq
notsq:
	cpy #$03
	bne :+
	; bring noise waves down an octave visually
	lda midinote
	; carry already set for equal
	sbc #12
	sta midinote
:	lda #$52
	sta PAL
	lda wav2color,y
	asl
	asl
aftersq:
	sta tmp1
	stz tmp2

; recheck midinote bounds
	lda midinote
	cmp #108
high:
	bcc :+
	lda #108
	sta midinote
:   cmp #12
	bcs :+
	lda #12
	sta midinote
:


chkvol:
	lda zsmkit::vera_psg_shadow+2,x
	eor #$ff
	and #$30
	lsr
	lsr
	lsr
	adc PAL
	sta PAL


chkpan:
	lda zsmkit::vera_psg_shadow+2,x
	rol
	rol
	rol
	and #3
	cmp #3
	beq chkpitch

	cmp #1
	beq :+
	inc panright
:
	lda tmp1
	clc
	adc #64
	sta tmp1
	lda #0
	adc #0
	sta tmp2
chkpitch:
	lda midinote
	cmp #35 ; bottom of the PSG range, don't bother showing
	        ; the bend because even the best tuning shows
			; as bent for some notes here
	bcc endbend
	lda midifrac
	beq endbend
	bpl contbend

	cmp #$c0
	bcs endbend

	ldy #2
	sty pitchdown

	bra softbend

contbend:
	cmp #$40
	bcc endbend
softbend:
	lda tmp1
	clc
	adc #128
	sta tmp1
	lda #0
	adc #0
	sta tmp2
	bra endbend
hardbend:
	inc tmp2
endbend:
	lda tmp1
	sta Vera::Reg::Data0

	; no high bits, mode 0
	lda tmp2
	sta Vera::Reg::Data0

	; multiply PSG channel by 16
	lda iterator
	asl
	asl
	asl
	asl
	
	; add offset and drop the X coord
	clc
	adc #<(192)
	sta Vera::Reg::Data0
	lda #>(192)
	adc #0
	sta Vera::Reg::Data0

	; note is Y coord
	lda #255
	sec
	sbc midinote
	sbc midinote

	; bring it downward on the screen by 194
	clc
	adc #194
	sta Vera::Reg::Data0

	lda #0
	adc #0
	sta Vera::Reg::Data0

	; set the Z depth / flip
	lda #$0C
	ora pitchdown
	ora panright
	sta Vera::Reg::Data0

	; set 16x16
	lda #$52 ; and pal offset 2
PAL = * - 1 ; modified above
	sta Vera::Reg::Data0
	bra splend
	

hideit:
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
	stz Vera::Reg::Data0
splend:
	inc iterator
	ldx iterator
	cpx #16
	bcs end
	jmp sploop

end:
	rts

wav2color:
	.byte 1,6,12,8
.endproc



.proc setup_sprites: near
	; Create 16x16 4bpp sprite data, all starting at $00000

	stz Vera::Reg::Ctrl
	stz Vera::Reg::AddrL
	stz Vera::Reg::AddrM
	lda #$10 ; auto increment = 1
	sta Vera::Reg::AddrH

	; First sprite is gonna be for the "note blocked" sprite
	ldx #0
:
	lda note_blocked,x
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128 of them

	; These next 15 are gonna be straight lines in various indexes
	lda #$11
	sta iterator
noteloop:    
	
	ldx #0
:
	lda note,x
	and iterator
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128
	
	lda iterator
	clc
	adc #$11
	sta iterator
	cmp #$10 ; first overflow should land here
	bne noteloop


; blank sprite
	ldx #0
:
	lda note_blocked,x
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128 of them

	lda #$11
	sta iterator
notearrowloop:
	
	ldx #0
:
	lda note_arrow,x
	and iterator
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128

	lda iterator
	clc
	adc #$11
	sta iterator
	cmp #$10 ; first overflow should land here
	bne notearrowloop


	; enable sprites
	lda Vera::Reg::DCVideo
	ora #$40
	sta Vera::Reg::DCVideo


; blank sprite
	ldx #0
:
	lda note_blocked,x
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128 of them


; now do pitch bendy things
	lda #$11
	sta iterator
bendloop1:
	
	ldx #0
:
	lda bend_1,x
	and iterator
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128

	lda iterator
	clc
	adc #$11
	sta iterator
	cmp #$10 ; first overflow should land here
	bne bendloop1


; blank sprite
	ldx #0
:
	lda note_blocked,x
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128 of them

	lda #$11
	sta iterator
bendloop1arrow:
	
	ldx #0
:
	lda bend_1_arrow,x
	and iterator
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128

	lda iterator
	clc
	adc #$11
	sta iterator
	cmp #$10 ; first overflow should land here
	bne bendloop1arrow


; blank sprite
	ldx #0
:
	lda note_blocked,x
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128 of them

	lda #$11
	sta iterator
bendloop2:
	
	ldx #0
:
	lda bend_2,x
	and iterator
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128

	lda iterator
	clc
	adc #$11
	sta iterator
	cmp #$10 ; first overflow should land here
	bne bendloop2


; blank sprite
	ldx #0
:
	lda note_blocked,x
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128 of them

	lda #$11
	sta iterator
bendloop2arrow:
	
	ldx #0
:
	lda bend_2_arrow,x
	and iterator
	sta Vera::Reg::Data0
	inx
	bpl :- ; 128

	lda iterator
	clc
	adc #$11
	sta iterator
	cmp #$10 ; first overflow should land here
	bne bendloop2arrow



	; enable sprites
	lda Vera::Reg::DCVideo
	ora #$40
	sta Vera::Reg::DCVideo


	rts

bend_1:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$FF,$FF,$FF,$FF,$00,$00
	.byte $0F,$FF,$FF,$00,$00,$FF,$FF,$F0
	.byte $FF,$F0,$00,$00,$00,$00,$0F,$FF
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00

bend_1_arrow:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$FF,$00,$FF,$FF,$00,$00,$00
	.byte $0F,$F0,$FF,$FF,$FF,$FF,$00,$00
	.byte $FF,$FF,$FF,$00,$00,$FF,$FF,$F0
	.byte $FF,$F0,$00,$00,$00,$00,$0F,$FF
	.byte $0F,$F0,$00,$00,$00,$00,$00,$00
	.byte $00,$FF,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00


bend_2:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$0F,$F0,$00,$00,$00
	.byte $00,$00,$00,$FF,$FF,$00,$00,$00
	.byte $00,$00,$0F,$F0,$0F,$F0,$00,$00
	.byte $00,$FF,$FF,$00,$00,$FF,$FF,$00
	.byte $FF,$FF,$F0,$00,$00,$0F,$FF,$FF
	.byte $FF,$00,$00,$00,$00,$00,$0F,$FF
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00

bend_2_arrow:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$0F,$F0,$00,$00,$00
	.byte $00,$00,$00,$FF,$FF,$00,$00,$00
	.byte $00,$FF,$0F,$F0,$0F,$F0,$00,$00
	.byte $0F,$FF,$FF,$00,$00,$FF,$FF,$00
	.byte $FF,$FF,$F0,$00,$00,$0F,$FF,$FF
	.byte $FF,$00,$00,$00,$00,$00,$0F,$FF
	.byte $0F,$F0,$00,$00,$00,$00,$00,$00
	.byte $00,$FF,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00


note:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00

note_arrow:
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$FF,$00,$00,$00,$00,$00,$00
	.byte $0F,$F0,$00,$00,$00,$00,$00,$00
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	.byte $0F,$F0,$00,$00,$00,$00,$00,$00
	.byte $00,$FF,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00


note_blocked: ; repurposed for external midi
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $1c,$cc,$dd,$dd,$dd,$dd,$cc,$c1
	.byte $1c,$cc,$dd,$dd,$dd,$dd,$cc,$c1
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00

.endproc


.proc setup_tiles: near
	; load tiles to VRAM $4000
	ldx #<tileset
	ldy #>tileset

	stx TS
	sty TS+1

	VERA_SET_ADDR $4000, 1

	ldx #$30
	ldy #0
tsloop:
	lda $ffff,y
TS = * - 2
	sta Vera::Reg::Data0
	iny
	bne tsloop
	inc TS+1
	dex
	bne tsloop

	; clear $8000 of 64x32 tiles
	; blank tile is $12 with attribute byte $10

	VERA_SET_ADDR $8000, 1

	ldx #$08
	ldy #0
tbloop:
	lda #$12
	sta Vera::Reg::Data0
	lda #$10
	sta Vera::Reg::Data0
	iny
	bne tbloop
	dex
	bne tbloop

	; fill in Melodius logo
	; top row
	VERA_SET_ADDR $8106, 2

	ldy #$30
m1loop:
	sty Vera::Reg::Data0
	iny
	cpy #$3f
	bcc m1loop

	; middle row
	VERA_SET_ADDR $8186, 2

	ldy #$40
m2loop:
	sty Vera::Reg::Data0
	iny
	cpy #$4f
	bcc m2loop

	; bottom row
	VERA_SET_ADDR $8206, 2

	ldy #$50
m3loop:
	sty Vera::Reg::Data0
	iny
	cpy #$5f
	bcc m3loop


	; Set up Layer 0 to point to it
	lda #%00010010 ; 64x32 4bpp
	sta Vera::Reg::L0Config

	lda #($8000 >> 9)
	sta Vera::Reg::L0MapBase

	lda #(($4000 >> 11) << 2) | %00000011 ; 16x16
	sta Vera::Reg::L0TileBase

	stz Vera::Reg::Ctrl
	; enable layer 0
	lda Vera::Reg::DCVideo
	ora #%00010000
	sta Vera::Reg::DCVideo

	jsr clear_screen

	; set palette offset 1 up
	VERA_SET_ADDR (Vera::VRAM_palette + 32), 1
	ldx #0
:
	lda pal,x
	sta Vera::Reg::Data0
	inx
	cpx #32
	bcc :-

	ldx #0
:
	lda pal2,x
	sta Vera::Reg::Data0
	inx
	bne :-


	rts
pal:
	; Tileset
	;      bg    none  pno1  pno2  pnoC  none  none  skin
	.word $0000,$0FF0,$0222,$0333,$0554,$0555,$0555,$0334
	;      none  text
	.word $0555,$0FFF,$0AAA,$0BBB,$0CCC,$0DDD,$0EEE,$0FFF
pal2:
	; max volume 3/3
	; Sprites
	;      bg    pno   chpr  orgn  guit  bass  str   ens
	.word $0000,$0FFF,$08A3,$0DDF,$0F8A,$000F,$00F0,$00FF
	;      bras  reed  pipe  lead  pad   fx    eth   perc
	.word $0FF0,$0DFD,$0FDD,$0ABC,$06AF,$0FA6,$0AF6,$0A6F
	; Pulse waves
	.word $0000,$0F11,$0F22,$0F33,$0F44,$0F55,$0F66,$0F77
	.word $0F88,$0F99,$0FAA,$0FBB,$0FCC,$0FDD,$0FEE,$0FFF

	; volume 2/3
	; Sprites
	;      bg    pno   chpr  orgn  guit  bass  str   ens
	.word $0000,$0BBB,$0682,$0AAB,$0B68,$000B,$00B0,$00BB
	;      bras  reed  pipe  lead  pad   fx    eth   perc
	.word $0BB0,$0ABA,$0BAA,$0889,$058B,$0B85,$08B5,$085B
	; Pulse waves
	.word $0000,$0B11,$0B11,$0B22,$0B33,$0B44,$0B55,$0B55
	.word $0B66,$0B77,$0B88,$0B88,$0B99,$0BAA,$0BBB,$0BBB

	; volume 1/3
	; Sprites
	;      bg    pno   chpr  orgn  guit  bass  str   ens
	.word $0000,$0888,$0452,$0778,$0845,$0008,$0080,$0088
	;      bras  reed  pipe  lead  pad   fx    eth   perc
	.word $0880,$0787,$0877,$0566,$0358,$0853,$0583,$0538
	; Pulse waves
	.word $0000,$0811,$0811,$0822,$0822,$0833,$0833,$0844
	.word $0844,$0855,$0855,$0866,$0866,$0877,$0877,$0888

	; volume 0/3
	; Sprites
	;      bg    pno   chpr  orgn  guit  bass  str   ens
	.word $0000,$0666,$0231,$0334,$0423,$0006,$0060,$0066
	;      bras  reed  pipe  lead  pad   fx    eth   perc
	.word $0660,$0343,$0433,$0333,$0246,$0642,$0462,$0426
	; Pulse waves
	.word $0000,$0600,$0611,$0611,$0611,$0622,$0622,$0633
	.word $0633,$0633,$0644,$0644,$0655,$0655,$0655,$0666



.endproc

.proc clear_screen
	; clear the text screen with a black bg
	ldx #0
:
	lda color_seq,x
	jsr X16::Kernal::CHROUT
	inx
	cpx #4
	bcc :-

	rts
color_seq:
	.byte $90,$01,$05,$93
.endproc

.proc draw_pianos
	stx COL
	asl COL
	pha
	phx
	phy

	; $8000, half page per row
	tya
	lsr
	php

	adc #$80
	sta Vera::Reg::AddrM

	; carry top half of page in
	; and add column * 2
	plp
	lda #0
	ror
	ora #$00
COL = * - 1
	sta Vera::Reg::AddrL

	; bank 0, increment 128 (one row)
	lda #$80
	sta Vera::Reg::AddrH

	ldx #$04
rptloop:
	lda #$26
	sta Vera::Reg::Data0
	lda #$27
	sta Vera::Reg::Data0
	lda #$28
	sta Vera::Reg::Data0
	dex
	bne rptloop

	ply
	plx
	pla
	rts
rpt:
	.byte 0
.endproc

.proc menu_options
	jsr hide_sprites

	; draw the options menu
    ldx #25
    ldy #0
	clc
    jsr X16::Kernal::PLOT

	lda #$95
	jsr X16::Kernal::CHROUT
	lda #$01
	jsr X16::Kernal::CHROUT
	lda #$05
	jsr X16::Kernal::CHROUT

	ldy #10
boxloop:
	phy
	ldx #0
rleloop:
	ldy boxrle,x
	beq rleend
	inx
	lda boxrle,x
	inx
innerrle:
	jsr X16::Kernal::CHROUT
	dey
	bne innerrle
	bra rleloop
rleend:
	lda #13
	jsr X16::Kernal::CHROUT
	ply
	dey
	bne boxloop
redraw:
	lda #$95
	jsr X16::Kernal::CHROUT
	lda #$01
	jsr X16::Kernal::CHROUT
	lda #$05
	jsr X16::Kernal::CHROUT

    ldx #26
    ldy #13
	clc
    jsr X16::Kernal::PLOT

	lda #'I'
	jsr X16::Kernal::CHROUT
	lda #'O'
	jsr X16::Kernal::CHROUT
	lda #' '
	jsr X16::Kernal::CHROUT
	lda external_midi_io
	jsr print_hex

    ldx #27
    ldy #13
	clc
    jsr X16::Kernal::PLOT

	lda #'C'
	jsr X16::Kernal::CHROUT
	lda #'H'
	jsr X16::Kernal::CHROUT
	lda #' '
	jsr X16::Kernal::CHROUT
	ldx #0
chloop:
	lda #$05
	bit midi_ext_enable,x
	bpl :+
	lda #$96
:	jsr X16::Kernal::CHROUT
	txa
	inc
	jsr get_decimal
	pha
	tya
	jsr X16::Kernal::CHROUT
	pla
	jsr X16::Kernal::CHROUT
	lda #' '
	jsr X16::Kernal::CHROUT
	inx
	cpx #16
	bcc chloop

rekey:
	jsr X16::Kernal::GETIN
	cmp #13 ; enter
	beq exit_menu
	cmp #27 ; esc
	beq exit_menu
	cmp #$9d ; left arrow
	beq dec_io
	cmp #$1d ; right arrow
	beq inc_io
	cmp #$30
	bcc rekey
	cmp #$3a
	bcc midi_channel_toggle
	cmp #$61
	bcc rekey
	cmp #$67
	bcs rekey
midi_channel_toggle:
	sec
	sbc #$31
	cmp #$ff
	bne :+
	lda #9
:	cmp #10
	bcc :+
	sbc #6+32 ; bring a-f down to 10-15
:	tax
	lda #$80
	eor midi_ext_enable,x
	sta midi_ext_enable,x
	asl
	txa
	jsr midi_set_external
	jmp redraw
dec_io:
	lda external_midi_io
	jeq redraw
	cmp #$68
	bcs trydec
	stz external_midi_io
	jmp redraw
trydec:
	sec
	sbc #$08
	sta external_midi_io
	jmp redraw
inc_io:
	lda external_midi_io
	cmp #$60
	bcs tryinc
	lda #$60
	sta external_midi_io
	jmp redraw
tryinc:
	cmp #$f8
	jcs redraw
	adc #$08
	sta external_midi_io
	jmp redraw

exit_menu:
	; init serial
	lda external_midi_io
	jsr midi_serial_init

	lda #$90
	jsr X16::Kernal::CHROUT
	lda #$01
	jsr X16::Kernal::CHROUT
	lda #$05
	jsr X16::Kernal::CHROUT

	lda #$93
	jsr X16::Kernal::CHROUT
	lda playback_mode
	beq notplaying
	cmp #1
	beq midi
	cmp #2
	beq zsm
	rts
notplaying:
	jsr show_directory
	jsr legend_jukebox
	rts
midi:
	jsr draw_midi_viz
	jsr setup_instruments
	rts
zsm:
	jsr draw_zsm_viz
	rts
boxrle:
	.byte 12,$1d,56,$20,0
.endproc
