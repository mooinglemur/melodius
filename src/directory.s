.export load_directory
.export show_directory
.export init_directory
.export dirlist_nav_down
.export dirlist_nav_up
.export dirlist_exec
.export dir_needs_refresh
.export dir_not_playing

.export files_full_size

.macpack longbranch

.import midi_stop
.import midi_play
.import midi_parse
.import midi_restart

.import playback_mode

.import hide_sprites

.include "x16.inc"

.scope AudioAPI
.include "audio.inc"
.endscope

.scope zsmkit
.include "zsmkit.inc"
.endscope

DIR_BANK = 2
LOAD_BANK = 3

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
fn_buf:
    .res 256
files_bottom_size:
    .res 1
files_top_size:
    .res 1
files_full_size:
    .res 1
dir_needs_refresh:
    .res 1

.segment "CODE"

.proc dir_not_playing
    stz cplaying+1
    inc dir_needs_refresh
    rts
.endproc

.proc dirlist_exec
    lda #DIR_BANK
    sta X16::Reg::RAMBank

    lda cactive
    sta dptr
    lda cactive+1
    sta dptr+1
    cmp mfiles+1
    bcc isdir
    beq :+
    bcs isfile
:   lda cactive
    cmp mfiles
    bcs isfile
isdir:
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
    bra :-
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
    bra :-
fn_done:
    tya
    ldx #<fn_buf
    ldy #>fn_buf
    jsr X16::Kernal::SETNAM

    lda #1
    ldx #8
    ldy #2
    jsr X16::Kernal::SETLFS
    
    lda #LOAD_BANK
    sta X16::Reg::RAMBank

    lda #0
    ldx #$00
    ldy #$a0
    jsr X16::Kernal::LOAD

    lda #LOAD_BANK
    sta X16::Reg::RAMBank

    lda $a000
    cmp #$4d
    bne notmidi

    lda $a001
    cmp #$54
    bne notmidi

    lda $a002
    cmp #$68
    bne notmidi

    lda $a003
    cmp #$64
    bne notmidi

    lda #LOAD_BANK
    ldx #$00
    ldy #$a0
    jsr midi_parse

    JSRFAR AudioAPI::ym_init, $0a

    jsr midi_restart
    lda #1
    jsr midi_play

    lda #1
    sta playback_mode
    
    inc dir_needs_refresh

    rts
notmidi:
    lda $a000
    cmp #$7a
    bne notzsm

    lda $a001
    cmp #$6d
    bne notzsm

    lda $a002
    cmp #$01
    bne notzsm

    lda #LOAD_BANK
    sta X16::Reg::RAMBank

    ldx #0
    lda #$00
    ldy #$a0
    jsr zsmkit::zsm_setmem

    ldx #0
    jsr zsmkit::zsm_play

    lda #2
    sta playback_mode

    inc dir_needs_refresh

    rts
notzsm:
    stz cplaying+1
    stz playback_mode
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
    jsr zsmkit::zsm_stop
    jmp loadnewfile
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
    ldx #30
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
    ldy #0
    lda (dptr),y
    beq name_notfound
    bra name_new_row

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
    beq cq

    sta (dptr)
    inc dptr
    bne name_loop
    inc dptr+1
    bra name_loop
cq:
    lda #0
    sta (dptr)
    inc dptr
    bne nul_loop
    inc dptr+1
    bra nul_loop

plaerr:
    pla
    jsr X16::Kernal::CLRCHN
    lda #1
    jsr X16::Kernal::CLOSE
    rts

.endproc

.proc init_directory
    stz preserved_name
    stz dir_needs_refresh
    stz cplaying+1
    lda #19
    sta files_top_size
    lda #19
    sta files_bottom_size
    lda #39
    sta files_full_size
    rts
.endproc
