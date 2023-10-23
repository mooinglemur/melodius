.export load_directory
.export show_directory
.export init_directory
.export dirlist_nav_down
.export dirlist_nav_up
.export dirlist_nav_home
.export dirlist_nav_end
.export dirlist_exec
.export dir_needs_refresh
.export dir_not_playing
.export set_dir_box_size
.export check_lazy_load
.export loader_get_ptr
.export zsm_callback
.export legend_jukebox
.export loadnext
.export loadrandom
.export sortdir
.export scroll_active_file_if_needed
.export select_playing_song
.export apply_ticker_behavior
.export errornum

.export files_full_size
.export files_width
.export loopctr
.export jukebox
.export ticker_behavior
.export stopping
.export atten

.macpack longbranch

.import midi_stop
.import midi_play
.import midi_parse
.import midi_restart

.import playback_mode
.import paused

.import hide_sprites
.import draw_file_box
.import draw_pianos
.import setup_instruments
.import draw_loader_ptr
.import draw_zsm_loop_ptr
.import zsm_tuning
.import zsm_tuning_update
.import clear_zsm_tuning
.import loading_msg

.import clear_via_timer
.import setup_via_timer
.import use_via_timer

.import flash_pause_zsm
.import flash_pause_midi


.include "x16.inc"

.scope AudioAPI
.include "audio.inc"
.endscope

.scope zsmkit
.include "zsmkit.inc"
.endscope

DIR_BANK = 2
SORT_BANK = 3
LOAD_BANK = 4

.segment "ZEROPAGE"
dptr:
    .res 2
dptr2:
    .res 2
dptr3:
    .res 2
.segment "BSS"
mfiles:
    .res 2
menddir:
    .res 2
msortinto:
    .res 2
mendsort:
    .res 2
ctop:
    .res 2
cactive:
    .res 2
cvis:
    .res 2
cplaying:
    .res 2
filecount:
    .res 2
filesdropped:
    .res 2
hilighted_screen_row_addr:
    .res 1
reset_hilight_scroll:
    .res 1
preserved_name:
    .res 256
files_bottom_size:
    .res 1
files_top_size:
    .res 1
files_full_size:
    .res 1
files_width:
    .res 1
dir_needs_refresh:
    .res 1
tmp3:
    .res 3
is_lazy_loading:
    .res 1
loopctr:
    .res 1
jukebox:
    .res 1
ticker_behavior:
    .res 1
stopping:
	.res 1
atten:
	.res 1
.segment "CODE"
stat_cmd:
    .byte "$=T:"
fn_buf:
    .res 256
errornum:
    .res 1

.proc sortdir: near
    
    lda msortinto
    sta dptr2
    lda msortinto+1
    sta dptr2+1

nextpass:
    stz found_flag
    stz dptr
    lda #$a0
    sta dptr+1

    lda #SORT_BANK
    sta X16::Reg::RAMBank

    lda (dptr)
    jeq end ; empty sort dir
    
    bra nextfile
advance:
    lda (dptr),y
    beq @1
    iny
    bne advance
    dey
@1:
    iny
    tya
    beq @2
    clc
    adc dptr
    sta dptr
    bcc @3
@2:
    inc dptr+1
@3:   

nextfile:
    ldy #0
    lda (dptr)
    jeq endoflist ; end of dir
    cmp #','
    beq advance ; already parsed
    lda found_flag
    beq foundlow ; always populate buf if nothing is there yet
compareloop:
    lda fn_buf,y
    jsr case_fold
    sta CF
    lda (dptr),y
    jsr case_fold
    cmp #$ff    
CF = * - 1
    beq @1
    bcc foundlow
    bcs advance
@1:
    cmp #0
    beq foundlow ; doubled file in list? should never happen
    iny
    bne compareloop
foundlow:
    ; first, check for '.' directory
dotcheck:
    ldy #0
    lda (dptr)
    cmp #'.'
    bne nodot
    iny
    lda (dptr),y
    bne nodot
    lda #','
    sta (dptr)
    bra advance
nodot:
    lda #1
    sta found_flag
    ldy #0
sort2buflp:
    lda (dptr),y
    sta fn_buf,y
    beq aftercopy
    iny
    bne sort2buflp
    dey
aftercopy:
    ; save position so we can commaify the entry later
    lda dptr
    sta dptr3
    lda dptr+1
    sta dptr3+1
    
    bra advance
endoflist:
    lda found_flag
    beq end

    lda #DIR_BANK
    sta X16::Reg::RAMBank
    ldy #0
buf2dirlp:
    lda fn_buf,y
    sta (dptr2),y
    beq @1
    iny
    bne buf2dirlp
    dey
@1:
    iny
    tya
    beq @2
    clc
    adc dptr2
    sta dptr2
    bcc @3
@2:
    inc dptr2+1
@3:
    lda #SORT_BANK
    sta X16::Reg::RAMBank

    ; comma-ify the entry we just saved
    lda #','
    sta (dptr3)

    ; check to make sure we're not near the end of the bank
    lda dptr2+1
    cmp #$bf
    jcc nextpass
end:
    clc
    rts
found_flag:
    .byte 0
.endproc

.proc case_fold: near
    cmp #$61
    bcc end
    cmp #$7b
    bcc sub20
    cmp #$a8
    bcc end
    beq sub2
    cmp #$b8
    bcc end
    beq sub4
    cmp #$e0
    bcc end
    cmp #$ff
    bcc sub20
    lda #$be
    rts
sub20:
    sec
    sbc #$20
end:
    rts
sub4:
    dec
    dec
sub2:
    dec
    dec
    rts
.endproc

.scope loader
reset:
    stz PTR
    lda #$a0
    sta PTR+1
    lda X16::Reg::RAMBank
    sta BK
    rts

loadchr:
    pha
    lda BK
    sta X16::Reg::RAMBank
    pla
    sta $a000
PTR = * - 2
    inc PTR
    bne :+
    inc PTR+1
    lda PTR+1
    cmp #$c0
    bcc :+
    sbc #$20
    sta PTR+1
    lda BK
    inc
    sta X16::Reg::RAMBank
    sta BK
:   rts

set_lfn:
    sta LFN
    rts

load_block:
    jsr X16::Kernal::CLRCHN
    ldx #$00
LFN = * - 1
    jsr X16::Kernal::CHKIN
    lda #$00
BK = * - 1
    sta X16::Reg::RAMBank
    lda #0
    ldx PTR
    ldy PTR+1
    clc
    jsr X16::Kernal::MACPTR
    bcs @err
    txa
    adc PTR
    sta PTR
    tya
    adc PTR+1
    cmp #$c0
    bcc :+
    sbc #$20
:   sta PTR+1
    lda X16::Reg::RAMBank
    sta BK
    jsr X16::Kernal::READST
    and #$40
    pha
    jsr X16::Kernal::CLRCHN
    jsr draw_loader_ptr
    pla
    clc
    rts
@err:
    sec
    rts

load_remainder:
    jsr load_block
    beq load_remainder
    php
    lda LFN
    jsr X16::Kernal::CLOSE
    plp
    rts

loader_get_ptr:
    lda BK
    ldx PTR
    ldy PTR+1
    rts

loader_set_ptr:
    sta BK
    stx PTR
    sty PTR+1
    rts

.endscope

loader_get_ptr := loader::loader_get_ptr

.proc check_lazy_load
    lda is_lazy_loading
    beq done

    jsr loader::load_block
    beq done
    stz is_lazy_loading
    lda #2
    jsr X16::Kernal::CLOSE
done:
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

.proc select_playing_song
    lda cplaying+1
    beq end
    lda cplaying
    sta cactive
    sta cvis
    lda cplaying+1
    sta cactive+1
    sta cvis+1
    inc dir_needs_refresh
end:
    rts
.endproc

.proc dir_not_playing
    stz cplaying+1
    inc dir_needs_refresh
    rts
.endproc

.proc galois16
    lda seed
    ora seed+1
    bne :+
    jsr X16::Kernal::ENTROPY_GET
    sta seed
    sty seed+1  
    bra galois16

:	ldy #8
	lda seed+0
:
	asl        ; shift the register
	rol seed+1
	bcc :+
	eor #$39   ; apply XOR feedback whenever a 1 bit is shifted out
:
	dey
	bne :--
	sta seed+0
	cmp #0     ; reload flags
	rts
seed:
    .byte 0,0
.endproc

.proc loadrandom
    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda filecount
    ora filecount+1
    bne :+
    sec ; file count zero
    rts
:
    lda mfiles
    sta dptr
    lda mfiles+1
    sta dptr+1

    lda (dptr)
    bne rando
    sec ; file list empty
    rts
rando:
    jsr galois16
    sta advcnt
    jsr galois16
    sta advcnt+1
modulo:
    lda filecount+1
    cmp advcnt+1
    bcc advance
    beq modulo2
    bra findit
modulo2:
    lda advcnt
    cmp filecount
    bcc findit
advance:
    lda advcnt
    sec
    sbc filecount
    sta advcnt
    lda advcnt+1
    sbc filecount+1
    sta advcnt+1
    bra modulo
adv:
    inc dptr
    bne findit
    inc dptr+1
findit:
    ldy #0
    lda (dptr)
    bne adv
    ldy #1
    lda (dptr),y
    beq foundit ; we're at the last file, can't go any further
    lda advcnt
    bne :+
    lda advcnt+1
    beq foundit ; we're at the correct file (the null termination thereof)
    dec advcnt+1
:   dec advcnt
    bra adv
foundit:
    ; rewind to previous null
    lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    lda (dptr)
    bne foundit
    ; now advance by one byte
    inc dptr
    bne end
    inc dptr+1   
end:
    jsr dirlist_exec_dptr
    stz errornum
    bcs loadnext
    rts
advcnt:
    .byte 0,0
.endproc

.proc loadnext
    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda tries
    cmp #2
    bcc :+
    stz tries
    rts ; failout
:

    lda cplaying+1
    beq top
    sta dptr+1
    lda cplaying
    sta dptr

    ; find the next null
nul_loop:
    inc dptr
    bne :+
    inc dptr+1
:   lda (dptr)
    bne nul_loop

    inc dptr
    bne :+
    inc dptr+1
:   lda (dptr)
    beq top

    lda dptr
    lda dptr+1
    bra end

top:
    inc tries
    lda mfiles
    sta dptr
    lda mfiles+1
    sta dptr+1

    lda (dptr)
    bne end
    ; directory has no files whatsoever
    sec
    rts
end:
    jsr dirlist_exec_dptr
    stz errornum
    bcs loadnext
    stz tries
    rts
tries:
    .byte 0
.endproc

.proc dirlist_exec
    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda cactive
    sta dptr
    lda cactive+1
    sta dptr+1
    
    jsr dirlist_exec_dptr
    bcs :+
    rts
:
    jsr dir_not_playing
    stz playback_mode
;    lda #$07
;    jsr X16::Kernal::BSOUT
    sec
    rts
.endproc

.proc dirlist_exec_dptr
    lda dptr+1
    cmp mfiles+1
    bcc isdir
    beq :+
    bcs isfile
:   lda dptr
    cmp mfiles
    bcs isfile
isdir:
    ; before chdir, let's see where we are
    ; and store it in preserved_name
    ; but only if we're attempting to load ".."
    stz preserved_name

    ldy #2
dotdotck:
    lda (dptr),y
    cmp dotdot,y
    bne dochdir
    dey
    bpl dotdotck

    jsr loadcwd

dochdir:
    ; chdir
    lda #'C'
    sta fn_buf
    lda #'D'
    sta fn_buf+1
    lda #':'
    sta fn_buf+2
    ldy #0
:   lda (dptr),y
    sta fn_buf+3,y
    beq donedir
    iny
    bne :-
donedir:
    tya
    clc
    adc #3
    ldx #<fn_buf
    ldy #>fn_buf
    jsr X16::Kernal::SETNAM

    lda #15
    ldx #8
    ldy #15
    jsr X16::Kernal::SETLFS
    
    jsr X16::Kernal::OPEN

    ldx #15
    jsr X16::Kernal::CHKIN
eat_it:
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::READST
    and #$40
    bne eat_it

    jsr X16::Kernal::CLRCHN
    lda #15
    jsr X16::Kernal::CLOSE

    stz cplaying+1

    jsr load_directory
    jmp show_directory
isfile:
    lda playback_mode
    cmp #1
    jeq wasmidi
    cmp #2
    jeq waszsm
loadnewfile:
    lda is_lazy_loading
    beq :+
    stz is_lazy_loading
    lda #2
    jsr X16::Kernal::CLOSE
:
    ; set as playing file
    lda dptr
    sta cplaying
    sta cvis
    lda dptr+1
    sta cplaying+1
    sta cvis+1

    ; copy filename
    ldy #0
:   lda (dptr),y
    sta fn_buf,y
    beq fn_done
    iny
    bne :-
fn_done:
    sty tmp_len

    JSRFAR AudioAPI::ym_init, $0a

    lda #LOAD_BANK
    sta X16::Reg::RAMBank

    ; get the file's size

    lda tmp_len
    clc
    adc #4
    ldx #<stat_cmd
    ldy #>stat_cmd

    jsr X16::Kernal::SETNAM

    jsr check_size_from_listing
    bcc :+
    lda #3 ; too_big
    sta errornum
    rts

:   ; open the file to see what it is
    ; load it along the way
    lda #LOAD_BANK
    sta X16::Reg::RAMBank

    ldx #<fn_buf
    ldy #>fn_buf
    lda tmp_len

    jsr X16::Kernal::SETNAM

    lda #2
    ldx #8
    ldy #2
    
    jsr X16::Kernal::SETLFS

    jsr X16::Kernal::OPEN

    ldx #2
    jsr X16::Kernal::CHKIN

    jsr loader::reset
    lda #2
    jsr loader::set_lfn

    jsr X16::Kernal::CHRIN
    cmp #'z'
    beq maybe_zsm
    cmp #'M'
    beq maybe_midi
    bra err
plaerr:
    pla
err:
    jsr X16::Kernal::CLRCHN
    lda #2
    jsr X16::Kernal::CLOSE
    sec
    lda #4 ; not recognized
    sta errornum
    rts
maybe_midi:
    jsr loader::loadchr
    jsr X16::Kernal::CHRIN
    cmp #'T'
    bne err
    jsr loader::loadchr
    jsr X16::Kernal::CHRIN
    cmp #'h'
    bne err
    jsr loader::loadchr
    jmp load_midi
maybe_zsm:
    jsr loader::loadchr
    jsr X16::Kernal::CHRIN
    cmp #'m'
    bne err
    jsr loader::loadchr
    jsr X16::Kernal::CHRIN
    cmp #$01
    bne err
    jsr loader::loadchr

    ; assuming this is ZSM
    ; resize window as such
    ldx #34
    ldy #7
    jsr draw_file_box
    jsr show_directory
    jsr legend_jukebox
    jsr clear_zsm_tuning

    lda #0 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

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



    ; eat 3 bytes (loop point)
    ldx #3
eat_loop:
    jsr X16::Kernal::CHRIN
    jsr loader::loadchr
    jsr X16::Kernal::READST
    and #$40
    jne err
    dex
    bne eat_loop


    ; grab the PCM offset
    ldx #3
eat_pcmoff:
    jsr X16::Kernal::CHRIN
    sta tmp3-1,x
    jsr loader::loadchr
    jsr X16::Kernal::READST
    and #$40
    jne err
    dex
    bne eat_pcmoff

    lda tmp3
    ora tmp3+1
    ora tmp3+2
    bne zsm_preload_pcm

preload:
    lda #$80
    sta is_lazy_loading

    ldx #6
preload_loop:
    phx
    jsr loader::load_block
    plx
    bcc :+
    lda #2
    jsr X16::Kernal::CLOSE
    stz is_lazy_loading
    sec
    rts
:   cmp #0
    jne close_zsm_continue ; very short ZSM fully loaded
    dex
    bne preload_loop
    jmp zsm_continue

zsm_preload_pcm:
    ; ZSM has PCM data.  We will load the PCM
    ; data and then return to lazy load

    jsr clear_via_timer

    lda #2 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

    ; seek to PCM data in file
    lda tmp3+2
    sta seekfn+2
    lda tmp3+1
    sta seekfn+3
    lda tmp3+0
    sta seekfn+4
    stz seekfn+5

    lda #6
    ldx #<seekfn
    ldy #>seekfn
    jsr X16::Kernal::SETNAM

    lda #15
    ldx #8
    ldy #15
    jsr X16::Kernal::SETLFS
    
    jsr X16::Kernal::OPEN

    ldx #15
    jsr X16::Kernal::CHKIN
eat_seek_response:
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::READST
    and #$40
    bne eat_seek_response

    jsr X16::Kernal::CLRCHN
    lda #15
    jsr X16::Kernal::CLOSE

    ; high byte of pcm offset is bank times 32
    lda tmp3
    asl
    asl
    asl
    clc
    adc #LOAD_BANK
    sta tmp3

    ; medium byte adds to the bank if we're over $20
    lda tmp3+1
:   cmp #$20
    bcc :+
    sbc #$20
    inc tmp3
    bra :-
:   adc #$a0
    tay        ; medium byte
    ldx tmp3+2 ; low byte
    lda tmp3   ; bank
    
    jsr loader::loader_set_ptr ; for PCM

    jsr loader::load_remainder
    bcc zsm_relazy
    rts
zsm_relazy: ; reopen the file and lazy load it now that PCM is loaded
    lda #0 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

    ldx #<fn_buf
    ldy #>fn_buf
    lda tmp_len

    jsr X16::Kernal::SETNAM

    lda #2
    ldx #8
    ldy #2
    
    jsr X16::Kernal::SETLFS

    jsr X16::Kernal::OPEN

    lda #LOAD_BANK
    sta X16::Reg::RAMBank
    jsr loader::reset
    jmp preload

seekfn:
    .byte "P",$02,0,0,0,0

close_zsm_continue:
    lda #2
    jsr X16::Kernal::CLOSE
    stz is_lazy_loading

zsm_continue:


    lda #LOAD_BANK
    sta X16::Reg::RAMBank

    ldx #0
    lda #$00
    ldy #$a0
    jsr zsmkit::zsm_setmem

    stz paused
    jsr flash_pause_zsm
    stz loopctr
    stz stopping
    stz atten
    stz zsm_tuning
	stz zsm_tuning_update

	ldx #0
	lda #0
	jsr zsmkit::zsm_setatten

    jsr apply_ticker_behavior

@zplay:
    ldx #0
    jsr zsmkit::zsm_play

    lda #2
    sta playback_mode

    jsr draw_zsm_loop_ptr

    inc dir_needs_refresh

    clc
    rts
load_midi:
    ; is MIDI, resize window as such
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

    lda #0 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

    lda #$11
    sta playback_mode
    jsr loader::load_remainder

    jsr setup_instruments

    lda #LOAD_BANK
    ldx #$00
    ldy #$a0
    jsr midi_parse

    stz paused
    jsr flash_pause_midi

    jsr midi_restart
    lda #1
    jsr midi_play

    lda #1
    sta playback_mode

    inc dir_needs_refresh

    clc
    rts
wasmidi:
    jsr hide_sprites
    stz playback_mode
    jsr midi_stop
    jmp loadnewfile
waszsm:
    jsr hide_sprites
    stz playback_mode
    ldx #0
    jsr zsmkit::zsm_close
    jsr clear_via_timer
    jmp loadnewfile
dotdot:
    .byte "..",0
tmp_len:
    .byte 0
.endproc

.proc apply_ticker_behavior
    php
    sei

    ldx #0
    jsr zsmkit::zsm_getrate
    cpy #0
    bne @do60

    ldy ticker_behavior
    cpy #2 ; never use VIA
    beq @do60

    cmp #60
    beq @do60
    cmp #30
    beq @do60
    cmp #20
    beq @do60
    cmp #15
    beq @do60
    cmp #12
    beq @do60
    cmp #10
    beq @do60
    cmp #7
    bcc @do60

@dovia:
    pha
    ldy #0
    jsr zsmkit::zsm_set_int_rate
    pla
    jsr setup_via_timer
    lda #1
    sta use_via_timer

    plp
    rts

@do60:
    lda #60
    ldy ticker_behavior ; 0 = default, 1 = always use via, 2 = never use via
    cpy #1
    beq @dovia
    ldy #0
    jsr zsmkit::zsm_set_int_rate
    stz use_via_timer
    jsr clear_via_timer

    plp
    rts
.endproc

.proc dirlist_nav_up
    cmp #0
    beq exit
    tay
    lda #DIR_BANK
    sta X16::Reg::RAMBank
    lda cactive
    sta dptr
    lda cactive+1
    sta dptr+1

restart:
    lda dptr+1
    cmp #$a0
    bne findloop
    lda dptr
    beq done
findloop:
    lda (dptr)
    beq rewind
    lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    bra findloop
rewind:
    lda dptr+1
    cmp #$a0
    bne :+
    lda dptr
    beq done
:   lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    lda (dptr)
    bne rewind

    inc dptr
    bne done
    inc dptr+1
done:
    dey
    bne restart
    lda dptr
    sta cactive
    sta cvis
    lda dptr+1
    sta cactive+1
    sta cvis+1
    inc dir_needs_refresh
exit:
    rts
.endproc

.proc dirlist_nav_home
    lda #$a0
    sta cactive+1
    sta cvis+1
    stz cactive
    stz cvis
    inc dir_needs_refresh
    rts
.endproc

.proc dirlist_nav_end
    lda #255
    jsr dirlist_nav_down
    bcc dirlist_nav_end
    rts
.endproc

.proc dirlist_nav_down
    cmp #0
    beq exit
    tax    
    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda cactive
    sta dptr
    lda cactive+1
    sta dptr+1

findloop:
    lda (dptr)
    beq bottom_check
    inc dptr
    bne :+
    inc dptr+1
:   bra findloop
bottom_check:
    sec
    ldy #1
    lda (dptr),y
    beq done
    inc dptr
    bne :+
    inc dptr+1
:   lda dptr
    sta cactive
    sta cvis
    lda dptr+1
    sta cactive+1
    sta cvis+1
    dex
    bne findloop
    clc
done:
    inc dir_needs_refresh
exit:
    rts
.endproc

.proc show_directory
    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda #$80
    sta reset_hilight_scroll
    ; first go back from cvis and count the number of names that exist
    ; stopping at 12
    lda cvis
    sta dptr
    lda cvis+1
    sta dptr+1
    ldx #0
find_top_loop:
    lda dptr+1
    cmp #$a0
    bne :+
    lda dptr
    beq found_top
:   lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    lda (dptr)
    bne find_top_loop
    inx
    cpx files_full_size
    bcc find_top_loop
found_top:
    stx above

    ; go forward from cactive and count the number of names that exist
    ; stopping at 6
    lda cvis
    sta dptr
    lda cvis+1
    sta dptr+1
    ldx #0
find_bot_loop:
    lda (dptr)
    inc dptr
    bne :+
    inc dptr+1
:   cmp #0
    bne find_bot_loop
    ldy #0
    lda (dptr),y
    beq found_bot
    inx
    cpx files_bottom_size
    bcc find_bot_loop
found_bot:
    stx below

    ; if (above + below) < files_full_size, then set top to beginning of list
    
    lda below
    clc
    adc above
    cmp files_full_size
    bcc short_list

    ; if above < files_top_size, then set top to beginning of list

    lda above
    cmp files_top_size
    bcc short_list

    ; set top to files_full_size - below
    lda files_full_size
    sec
    sbc below

find_ctop:
    tax ; go up this many
    lda cvis
    sta dptr
    lda cvis+1
    sta dptr+1
ctop_loop:
    cpx #0
    beq ctop_found

    lda dptr+1
    cmp #$a0
    bne :+
    lda dptr
    beq ctop_done
:   lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    lda (dptr)
    bne ctop_loop
    dex
    bra ctop_loop

ctop_found:
    ; we still need to rewind to the top or until the next null
    lda dptr+1
    cmp #$a0
    bne :+
    lda dptr
    beq ctop_done

:   lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    lda (dptr)
    bne ctop_found

    inc dptr
    bne ctop_done
    inc dptr+1
ctop_done:
    lda dptr
    sta ctop
    lda dptr+1
    sta ctop+1
    bra displayit
short_list:
    stz ctop
    lda #$a0
    sta ctop+1
displayit:
    stz row
    lda ctop
    sta dptr
    lda ctop+1
    sta dptr+1

    ; print out the dir
    lda #$11
    sta Vera::Reg::AddrH
rowloop:
    ; set VERA pointer
    lda #(6 << 1) ; x coord, column 6
    sta Vera::Reg::AddrL
    ; Y coordinate
    lda row
    clc
    adc #(14 + $B0) ; start at row 14 on screen
    sta Vera::Reg::AddrM
    tax

    ; if this is the selected entry
    lda dptr+1
    cmp cactive+1
    bne notactive
    lda dptr
    cmp cactive
    bne notactive

    ; highlight bg color
    stx hilighted_screen_row_addr
    ldy #$60
    bra check_isdir
notactive:
    ; black bg color
    ldy #$00
check_isdir:
    lda dptr+1
    cmp mfiles+1
    bcc isdir
    beq :+
    bcs isfile
:   lda dptr
    cmp mfiles
    bcs isfile
isdir:
    ; yellow fg
    tya
    ora #$07
    tay
    bra printit
isfile:
    ; check to see if this is the one playing
    iny ; 0 to 1
    ldx dptr+1
    cpx cplaying+1
    bne :+
    ldx dptr
    cpx cplaying
    bne :+
    tya
    and #$f0
    ora #$04
    tay
:   
printit:
    ldx files_width
    lda (dptr)
    beq early_end
chrloop:
    lda (dptr)
    beq nextrow
    sta Vera::Reg::Data0
    sty Vera::Reg::Data0
    dex
    beq consumerow
    inc dptr
    bne chrloop
    inc dptr+1
    bra chrloop
consumerow:
    inc dptr
    bne :+
    inc dptr+1
:   lda (dptr)
    bne consumerow
nextrow:
    cpx #0
    beq padded_row
    lda #$20
    sta Vera::Reg::Data0
    sty Vera::Reg::Data0
    dex
    bra nextrow

padded_row:
    inc dptr
    bne :+
    inc dptr+1
:   inc row
    lda row
    cmp files_full_size
    jeq rowloop
    jcc rowloop
    clc
    rts
early_end:
    lda dptr
    bne :+
    dec dptr+1
:   dec dptr
    bra nextrow
above:
    .byte 0
below:
    .byte 0
row:
    .byte 0
.endproc

.proc scroll_active_file_if_needed
    lda cactive
    cmp cvis
    jne end
    sta dptr
    lda cactive+1
    cmp cvis+1
    bne end
    sta dptr+1

    lda reset_hilight_scroll
    beq scrollit
    stz reset_hilight_scroll
    ldy #0
findlength:
    lda (dptr),y
    beq :+
    iny
    bne findlength
:   cpy files_width
    bcs :+
    stz doscroll ; don't scroll if filename is less than the width of the viewport
    rts
:   tya
    sec
    sbc files_width
    sta doscroll
    lda #120 ; initial pause
    sta lingertimer
    stz scrolloffset
    ldy #0
    bra displayonce
scrollit:
    lda doscroll
    beq checklastlinger
    dec lingertimer
    beq advance
    rts
advance:
    dec doscroll
    beq lingerlong
    lda #15
    sta lingertimer
    bra display
lingerlong:
    lda #120
    sta lingertimer
display:
    inc scrolloffset
    ldy scrolloffset
displayonce:
    lda #(6 << 1)
    sta Vera::Reg::AddrL
    lda hilighted_screen_row_addr
    sta Vera::Reg::AddrM
    lda #$21
    sta Vera::Reg::AddrH ; leaving the attribute bytes alone
    ldx files_width
:   lda (dptr),y
    sta Vera::Reg::Data0
    iny
    dex
    bne :-    
    rts
checklastlinger:
    lda lingertimer
    beq end
    dec lingertimer
    bne end
    ; last tick, start over
    lda #1
    sta reset_hilight_scroll
end:
    rts
lingertimer:
    .byte 0
doscroll:
    .byte 0
scrolloffset:
    .byte 0
.endproc

.proc load_directory
    stz dptr
    stz msortinto
    lda #$a0
    sta dptr+1
    sta msortinto+1

    lda #SORT_BANK
    sta X16::Reg::RAMBank

    ; load directory

    lda #6
    ldx #<dirfn
    ldy #>dirfn

    jsr X16::Kernal::SETNAM

    lda #0 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

    jsr loaddir

    lda #0
    sta (dptr)

    lda #1 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg


    ; sort directory
    jsr sortdir

    ; set the write point
    lda dptr2
    sta msortinto
    sta mfiles
    lda dptr2+1
    sta msortinto+1
    sta mfiles+1

    stz dptr
    lda #$a0
    sta dptr+1

    lda #SORT_BANK
    sta X16::Reg::RAMBank

    lda #6
    ldx #<filefn
    ldy #>filefn

    jsr X16::Kernal::SETNAM

    lda #0 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

    stz filecount
    stz filecount+1

    jsr loaddir

    lda #0
    sta (dptr)

    lda #1 ; 0 = loading, 1 = sorting, 2 = preloading pcm
    jsr loading_msg

    ; sort directory
    jsr sortdir

    lda #DIR_BANK
    sta X16::Reg::RAMBank

    ; null terminate it
    lda #0
    sta (dptr2)
    lda dptr2
    sta menddir
    lda dptr2+1
    sta menddir+1

    ; look for preserved name
    stz dptr
    stz cactive
    stz cvis
    lda #$a0
    sta dptr+1
    sta cactive+1
    sta cvis+1

name_new_row:
    ldx #0
name_check:
    lda preserved_name,x
    cmp (dptr)
    bne name_next
    cmp #0
    beq name_found
    inc dptr
    bne :+
    inc dptr+1
:   inx
    bne name_check
name_notfound:
    rts
name_found:
    txa
    eor #$ff
    inc
    clc
    adc dptr
    sta cactive
    sta cvis
    lda dptr+1
    sbc #0
    sta cactive+1
    sta cvis+1
    rts
name_next:
    inc dptr
    bne :+
    inc dptr+1
:   lda (dptr)
    bne name_next
    ldy #1
    lda (dptr),y
    beq name_notfound
    inc dptr
    bne :+
    inc dptr+1
:   bra name_new_row

dirfn:
    .byte "$=T:=D"
filefn:
    .byte "$=T:=P"
.endproc


.proc loaddir
    lda #1
    ldx #8
    ldy #0

    jsr X16::Kernal::SETLFS
    jsr X16::Kernal::OPEN

    ldx #1
    jsr X16::Kernal::CHKIN

    ; eat the load address, line number, and next line
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; get past the header
    ; look for the first open quote mark
hoq_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    bne hoq_loop
    bra nul_loop
plaerr:
    pla
err:
    jsr X16::Kernal::CLRCHN
    lda #1
    jsr X16::Kernal::CLOSE
    rts

    ; now we keep going until EOL
nul_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    bne nul_loop

    ; eat the line number and next line
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; look for the open quote mark
oq_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    bne oq_loop

    inc filecount
    bne :+
    inc filecount+1
:
    ; absorb the name until the close quote
name_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    beq bank_full_check

    sta (dptr)
    inc dptr
    bne name_loop
    inc dptr+1
    bra name_loop

bank_full_check:
    lda #0
    sta (dptr)
    inc dptr
    bne :+
    inc dptr+1
:   lda dptr+1
    cmp #$bf
    beq maybe_stop_here
    bra nul_loop
maybe_stop_here:
    ; we're getting very close to the end of the bank
    ; let's just not load any more
    jmp err
    
.endproc

.proc loadcwd
    lda #3
    ldx #<cwdfn
    ldy #>cwdfn

    jsr X16::Kernal::SETNAM

    lda #1
    ldx #8
    ldy #0

    jsr X16::Kernal::SETLFS
    jsr X16::Kernal::OPEN

    ldx #1
    jsr X16::Kernal::CHKIN

    ; eat the load address, line number, and next line
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; get past the header
    ; look for the first open quote mark
hoq_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    bne hoq_loop

    ; now we keep going until EOL
nul_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    bne nul_loop

    ; eat the line number and next line
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::CHRIN

    ; look for the open quote mark
oq_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    bne oq_loop

    ; absorb the name until the close quote
    ldx #0
name_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    beq cq

    sta preserved_name,x
    inx
    bra name_loop
cq:
    pha
plaerr:
    stz preserved_name,x
    pla
    jsr X16::Kernal::CLRCHN
    lda #1
    jsr X16::Kernal::CLOSE
    rts
cwdfn:
    .byte "$=C"
.endproc

; Expects setnam to be done, will open and parse the directory
; checking the size of the file.  If it's larger than
; the number of free RAM banks, we return carry set
; which indicates not enough room for the file or there was
; another error, otherwise carry is clear.
;
; This routine parses the block size that comes back from the listing
.proc check_size_from_listing
    lda #1
    ldx #8
    ldy #0

    jsr X16::Kernal::SETLFS

    lda #0
    jsr X16::Kernal::OPEN

    ; eat the header until the first open quote
    ; get past the header
    ; look for the first open quote mark
    ldx #1
    jsr X16::Kernal::CHKIN

hoq_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    bne hoq_loop
    bra nul_loop
plaerr:
    pla
err:
    jsr X16::Kernal::CLRCHN
    lda #1
    jsr X16::Kernal::CLOSE
    sec
    rts

    ; now we keep going until EOL
nul_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    bne nul_loop

    ; eat the link
.repeat 2
    jsr X16::Kernal::CHRIN
    jsr X16::Kernal::READST
    and #$40
    bne err
.endrepeat

    ; here's the line number
    jsr X16::Kernal::CHRIN
    sta blk
    jsr X16::Kernal::READST
    and #$40
    bne err
    jsr X16::Kernal::CHRIN
    sta blk+1
    jsr X16::Kernal::READST
    and #$40
    bne err

    jsr X16::Kernal::CLRCHN
    lda #1
    jsr X16::Kernal::CLOSE

    ; blk has the block size
    ; we want number of 8 k
    ; banks
    lda blk+1
.repeat 5
    lsr
    ror blk
.endrepeat
    cmp #0
    bne no ; more than 256 8k banks worth
    ; get the number of 8k banks in the system
    sec
    jsr X16::Kernal::MEMTOP
    sec
    sbc #4 ; take away the number of banks used by the system and Melodius
           ; this has the nice side effect of 256 banks (0) becoming 252
    cmp blk
    bcc no
ok:
    clc
    rts
no:
    sec
    rts
blk:
    .word 0
.endproc

.proc init_directory
    stz preserved_name
    stz dir_needs_refresh
    stz cplaying+1
    stz is_lazy_loading
    stz loopctr
    stz jukebox
    stz ticker_behavior
    stz errornum
    rts
.endproc

.proc set_dir_box_size
    stx files_width
    tya
    lsr
    dec
    sta files_top_size
    sta files_bottom_size
    tya
    dec
    sta files_full_size
    rts
.endproc

.proc zsm_callback
    cpy #1
    bne :+
    sta loopctr
    bra end
:   cpy #3
    bne end
    sta zsm_tuning
    lda #$80
    sta zsm_tuning_update
end:
    rts
.endproc

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

midilegend1:
    .byte $90,$01,$05,"MIDI CHANNEL",0

midilegend2:
    .byte $9e,"  KEY TEMPO  BEAT   LOADED",0
