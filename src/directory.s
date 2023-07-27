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

.export files_full_size
.export loopctr
.export jukebox
.export stopping
.export atten

.macpack longbranch

.import midi_stop
.import midi_play
.import midi_parse
.import midi_restart

.import playback_mode

.import hide_sprites
.import draw_file_box
.import draw_pianos
.import setup_instruments
.import draw_loader_ptr
.import draw_zsm_loop_ptr

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
.segment "BSS"
mfiles:
    .res 2
menddir:
    .res 2
ctop:
    .res 2
cactive:
    .res 2
cplaying:
    .res 2
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
stopping:
	.res 1
atten:
	.res 1
.segment "CODE"
stat_cmd:
    .byte "$=T:"
fn_buf:
    .res 256

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
    beq off
    lda #'N'
    jsr X16::Kernal::BSOUT
    lda #' '
    jsr X16::Kernal::BSOUT
    bra end
off:
    lda #'F'
    jsr X16::Kernal::BSOUT
    jsr X16::Kernal::BSOUT
end:
    rts
legend:
    .byte $90,$01,$05,"[F1] TOGGLE JUKEBOX MODE: O",0
.endproc

.proc dir_not_playing
    stz cplaying+1
    inc dir_needs_refresh
    rts
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
    lda #$07
    jsr X16::Kernal::BSOUT
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
    lda dptr+1
    sta cplaying+1

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
    ldx #9
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
    bne zsm_load_remainder

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
    bne close_zsm_continue ; very short ZSM fully loaded
    dex
    bne preload_loop
    bra zsm_continue

zsm_load_remainder:
    ; ZSM has PCM data.  We will simply load it in full
    ; now before returning control

    jsr loader::load_remainder
    bcc zsm_continue
    rts

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

    stz loopctr
    stz stopping
    stz atten

	ldx #0
	lda #0
	jsr zsmkit::zsm_setatten

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

    lda #$11
    sta playback_mode
    jsr loader::load_remainder

    jsr setup_instruments

    lda #LOAD_BANK
    ldx #$00
    ldy #$a0
    jsr midi_parse

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
    jmp loadnewfile
dotdot:
    .byte "..",0
tmp_len:
    .byte 0
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
    lda dptr+1
    sta cactive+1
    inc dir_needs_refresh
exit:
    rts
.endproc

.proc dirlist_nav_home
    lda #$a0
    sta cactive+1
    stz cactive
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
    lda dptr+1
    sta cactive+1
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

    ; first go back from cactive and count the number of names that exist
    ; stopping at 12
    lda cactive
    sta dptr
    lda cactive+1
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
    lda cactive
    sta dptr
    lda cactive+1
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
    lda cactive
    sta dptr
    lda cactive+1
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
rowloop:
    ; X coordinate goes in Y register and
    ldy #6
    ; Y coordinate goes in X register :(
    lda row
    clc
    adc #14
    tax
    jsr X16::Kernal::PLOT ; carry is clear, set position

    ; if this is the selected entry
    lda dptr+1
    cmp cactive+1
    bne notactive
    lda dptr
    cmp cactive
    bne notactive

    ; highlight bg color
    lda #1
    jsr X16::Kernal::CHROUT
    lda #$1f
    jsr X16::Kernal::CHROUT
    lda #1
    jsr X16::Kernal::CHROUT
    bra check_isdir
notactive:
    ; black bg color
    lda #1
    jsr X16::Kernal::CHROUT
    lda #$90
    jsr X16::Kernal::CHROUT
    lda #1
    jsr X16::Kernal::CHROUT

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
    lda #$9e
    jsr X16::Kernal::CHROUT
    bra printit
isfile:
    ; check to see if this is the one playing
    lda #$05
    ldx dptr+1
    cpx cplaying+1
    bne :+
    ldx dptr
    cpx cplaying
    bne :+
    lda #$9c
:   jsr X16::Kernal::CHROUT
printit:
    ldx files_width
    lda (dptr)
    beq early_end
chrloop:
    lda (dptr)
    beq nextrow
    jsr X16::Kernal::CHROUT
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
    jsr X16::Kernal::CHROUT
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

.proc load_directory
    stz dptr
    lda #$a0
    sta dptr+1

    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda #6
    ldx #<dirfn
    ldy #>dirfn

    jsr X16::Kernal::SETNAM

    jsr loaddir

    lda dptr
    sta mfiles
    lda dptr+1
    sta mfiles+1

    lda #6
    ldx #<filefn
    ldy #>filefn

    jsr X16::Kernal::SETNAM

    jsr loaddir

    lda #0
    sta (dptr)
    lda dptr
    sta menddir
    lda dptr+1
    sta menddir+1

    ; look for preserved name
    stz dptr
    stz cactive
    lda #$a0
    sta dptr+1
    sta cactive+1

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
    lda dptr+1
    sbc #0
    sta cactive+1
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

    ; absorb the name until the close quote
name_loop:
    jsr X16::Kernal::CHRIN
    pha
    jsr X16::Kernal::READST
    and #$40
    bne plaerr
    pla
    cmp #$22
    beq dotcheck

    sta (dptr)
    inc dptr
    bne name_loop
    inc dptr+1
    bra name_loop

dotcheck:
    lda #0
    sta (dptr)
    ; skip filename if it's "."
    ; first, back up by 2
    lda dptr
    sec
    sbc #2
    sta dptr
    bcs :+
    dec dptr+1
    ; now check to see if we underflowed the pointer out of 
    ; banked ram
:   lda dptr+1
    cmp #$a0
    bcs :+
    ; we underflowed
    ; check to see if $a000 is a dot
    lda $a000
    cmp #'.'
    ; apparently a single character filename at the top of the directory
    ; that is not a dot
    bne wasok
    ; is a dot, reset
    stz dptr
    lda #$a0
    sta dptr+1
    bra nul_loop
:   ; if directory just read is ".", then the sequence will be "\0.\0"
    lda (dptr)
    bne wasok
    ldy #1
    lda (dptr),y
    cmp #'.'
    bne wasok
    ; next byte is definitely null since we're here
    ; we just placed "." as a directory name
    inc dptr
    bne :+
:   inc dptr+1
    bra nul_loop
wasok:    
    lda dptr
    clc
    adc #3
    sta dptr
    bcc :+
    inc dptr+1
:   lda dptr+1
    cmp #$bf
    beq maybe_stop_here
    jmp nul_loop
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
    bne end
    sta loopctr
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
