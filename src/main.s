;KIOSK = 1

.macpack longbranch

.segment "LOADADDR"
	.word $0801
.segment "BASICSTUB"
	.word start-2
	.byte $00,$00,$9e
	.byte "2061"
	.byte $00,$00,$00
.segment "STARTUP"
start:
	jmp main
.segment "BSS"
playback_mode:
	.res 1
paused:
	.res 1
controller_hold:
	.res 1
controller_last:
	.res 2
controller_edge:
	.res 2
zsmkit_lowram:
	.res 256

.segment "CODE"

.include "x16.inc"
.scope AudioAPI
	.include "audio.inc"
.endscope

ZSMKIT_BANK = 1

.export playback_mode
.export paused

.import __ZSMKITLIB_LOAD__

.import register_handler
.import deregister_handler
.import measure_machine_speed
.import midi_parse
.import midi_play
.import midi_stop
.import midi_restart
.import midi_is_playing
.import midi_playtick

.import midi_init
.import midi_serial_init

.import setup_tiles
.import setup_sprites
.import hide_sprites
.import setup_instruments
.import do_midi_sprites
.import do_zsm_sprites
.import update_instruments
.import update_midi_beat
.import update_midi_message_flash

.import init_directory
.import load_directory
.import show_directory
.import dirlist_nav_down
.import dirlist_nav_up
.import dirlist_nav_home
.import dirlist_nav_end
.import dirlist_exec
.import dir_needs_refresh
.import dir_not_playing
.import files_full_size
.import draw_file_box
.import check_lazy_load
.import draw_zsm_ptr
.import loopctr
.import loadnext
.import loadrandom
.import jukebox
.import legend_jukebox
.import zsm_callback
.import stopping
.import atten
.import draw_lyric
.import draw_zsm_tuning
.import scroll_active_file_if_needed
.import select_playing_song
.import ticker_behavior
.import apply_ticker_behavior
.import errornum
.import loading_msg
.import flash_pause_midi
.import flash_pause_zsm
.import menu_options

.import clear_via_timer

.include "macros.inc"

.scope zsmkit
.include "zsmkit.inc"
.endscope

main:
	; screen mode 0
	lda #0
	clc
	jsr X16::Kernal::SCREEN_MODE

	; set up raster bar thingy
	lda #2
	sta Vera::Reg::Ctrl

	lda #($A0 - 1)
	sta Vera::Reg::DCHStop

	stz Vera::Reg::Ctrl

	; set ISO mode
	lda #$0f
	jsr X16::Kernal::CHROUT

	; get skinny charset if VGA
	lda Vera::Reg::DCVideo
	and #$03
	cmp #1
	bne noskinny

	lda #6
	jsr X16::Kernal::SCREEN_SET_CHARSET

noskinny:
	lda #0
	jsr X16::Kernal::MOUSE_CONFIG

	jsr setup_sprites
	jsr setup_tiles
;    jsr setup_instruments

	stz playback_mode

	stz paused

	stz stopping

	stz controller_last
	stz controller_last+1

	stz controller_edge
	stz controller_edge+1

	stz controller_hold

	jsr copy_zsmkit_lib

    lda #ZSMKIT_BANK
    sta X16::Reg::RAMBank

	ldx #<zsmkit_lowram
	ldy #>zsmkit_lowram
	jsr zsmkit::zsm_init_engine

	ldx #0
	lda #<zsm_callback
	ldy #>zsm_callback
	jsr zsmkit::zsm_setcb

	jsr measure_machine_speed
	jsr register_handler

	jsr midi_init
	jsr init_directory

	ldx #34
	ldy #20
	jsr draw_file_box

	jsr load_directory
	jsr show_directory

.ifdef KIOSK
	lda #2
	sta jukebox
.endif
	jsr legend_jukebox
.ifdef KIOSK
	jsr loadrandom
	jsr scroll_active_file_if_needed
.endif
endless:
	jsr X16::Kernal::RDTIM
	sta FRC
	wai
	jsr X16::Kernal::RDTIM
	cmp #$ff
FRC = * -1
	beq endless

rekey:
	jsr X16::Kernal::GETIN
	beq check_controller

	ldx #(keytab_end-keytab)
keyloop:
	cmp keytab-1,x
	beq keymatch
	dex
	bne keyloop
	bra endkey

keymatch:
	dex
	txa
	asl
	tax
	jmp (keyopertab,x)

check_controller:
	lda #1
	jsr X16::Kernal::JOYSTICK_GET
	cpy #$ff ; no controller plugged in
	beq endkey
	eor #$ff
	tay
	eor controller_last
	sta controller_edge
	txa
	eor #$ff
	tax
	eor controller_last+1
	sta controller_edge+1
	sty controller_last
	stx controller_last+1
	ora controller_edge
	beq held

	lda #30
controller_repeat:
	sta controller_hold

	lda controller_last
	ldx #(joytab_end-joytab)
joy_a_loop:
	cmp joytab-1,x
	beq joy_a_match
	dex
	bne joy_a_loop

	lda controller_last+1
	ldx #(joytab_end-joytab)
joy_x_loop:
	cmp joytab-1,x
	beq joy_x_match
	dex
	bne joy_x_loop

	bra endkey

joy_a_match:
	dex
	txa
	asl
	tax
	jmp (joyopertab_a,x)

joy_x_match:
	dex
	txa
	asl
	tax
	jmp (joyopertab_x,x)

held:
	lda controller_hold
	dec
	bpl not_held
	cmp #$fe
	bcs not_held
	lda #0
	bra controller_repeat
not_held:
	sta controller_hold

endkey:
	lda dir_needs_refresh
	beq :+
	jsr show_directory
	jsr legend_jukebox
	stz dir_needs_refresh
	lda errornum
	beq :+
	jsr loading_msg
	stz errornum
:

	VIZ_BORDER

	lda playback_mode
	cmp #1
	beq ismidi
	cmp #2
	beq iszsm
	cmp #3
	beq iszcm
	jmp continue
ismidi:
	jsr do_midi_sprites
	jsr update_instruments
	jsr update_midi_message_flash
	jsr draw_lyric
	jsr update_midi_beat
	lda paused
	jne flashpause
	jsr midi_is_playing
	jne continue
	lda jukebox
	beq stopmidi
	cmp #2
	bne :+
	jsr loadrandom
	jcc continue
:	jsr loadnext
	jcc continue
stopmidi:

	jsr midi_stop
	jsr hide_sprites
	bra songstopped
iszsm:
	jsr check_lazy_load
	jsr draw_zsm_ptr
	jsr draw_zsm_tuning
	jsr do_zsm_sprites

    lda #ZSMKIT_BANK
    sta X16::Reg::RAMBank

	lda stopping
	beq loopck
	; we're stopping
	inc atten
	bne doatten
	; we've faded out
	stz stopping
	bra ckloadnext
doatten:
	lda atten
	lsr
	lsr
	ldx #0
	jsr zsmkit::zsm_setatten
	bra zsmck
iszcm:
	jsr do_zsm_sprites
	bit Vera::Reg::AudioCtrl
	bvc continue
	jsr hide_sprites
	bra songstopped
stopzcm:
	jsr zsmkit::zcm_stop
	bra continue
loopck:
	lda jukebox
	beq zsmck
	lda loopctr
	cmp #2
	bcc zsmck
	inc stopping
zsmck:
	lda paused
	jne flashpause
	ldx #0
	jsr zsmkit::zsm_getstate
	bcs continue

ckloadnext:
	lda jukebox
	beq stopzsm
	cmp #2
	bne :+
	jsr loadrandom
	bcc continue
:	jsr loadnext
	bcc continue
stopzsm:
	lda #ZSMKIT_BANK
	sta X16::Reg::RAMBank
	ldx #0
	jsr zsmkit::zsm_close
	jsr hide_sprites
	jsr clear_via_timer
songstopped:
	ldx #34
	ldy #20
	jsr draw_file_box
	inc dir_needs_refresh
	jsr dir_not_playing
	stz paused
	stz playback_mode
continue:
	jsr scroll_active_file_if_needed
	DONE_BORDER

	jsr X16::Kernal::STOP ; test stop key
	beq exit
	jmp endless
pausezsm:
	lda #ZSMKIT_BANK
	sta X16::Reg::RAMBank
	lda paused
	bne resumezsm
	ldx #0
	jsr zsmkit::zsm_stop
	inc paused
	jmp endless
resumezsm:
	ldx #0
	jsr zsmkit::zsm_play
	stz paused
	jmp flashpause ; unflash
pausemidi:
	lda paused
	bne resumemidi
	jsr midi_stop
	inc paused
	jmp endless
resumemidi:
	lda #1
	jsr midi_play
	stz paused
	jmp flashpause ; unflash


exit:
	jsr deregister_handler

	JSRFAR AudioAPI::ym_init, $0A

	jsr X16::Kernal::SCINIT

	rts

error:
	lda #$45
	jsr X16::Kernal::CHROUT

	rts


domidi:
	lda #3
	ldx #$00
	ldy #$A0

	jsr midi_parse
	bcc :+
	jmp error
:
	jsr midi_restart

	lda #1
	jsr midi_play

	lda #1
	sta playback_mode

	rts

flashpause:
	lda playback_mode
	jeq continue
	cmp #2
	beq @zsm
@midi:
	jsr flash_pause_midi
	jmp continue
@zsm:
	jsr flash_pause_zsm
	jmp continue

keytab:
	.byte $60 ; Backtick
	.byte $85 ; F1
	.byte $89 ; F2
	.byte $86 ; F3
	.byte $09 ; Tab
	.byte $18 ; Shift+Tab
	.byte $91 ; Up Arrow
	.byte $11 ; Down Arrow
	.byte $82 ; PgUp
	.byte $02 ; PgDn
	.byte $13 ; Home
	.byte $04 ; End
	.byte $a0 ; Shift+Space
	.byte $20 ; Space
	.byte $0d ; Return
	.byte $1d ; Right Arrow
keytab_end:

joytab:
	.byte $01 ; Right
	.byte $02 ; Left
	.byte $04 ; Down
	.byte $08 ; Up
	.byte $10 ; Start / R
	.byte $20 ; Select / L
	.byte $40 ; Y / X
	.byte $80 ; B / A
joytab_end:

joyopertab_a:
	.word RIGHTARROW_func
	.word endkey
	.word DOWNARROW_func
	.word UPARROW_func
	.word SPACE_func
	.word F1_func
	.word TAB_func
	.word RETURN_func

joyopertab_x:
	.word endkey
	.word endkey
	.word endkey
	.word endkey
	.word PGDN_func
	.word PGUP_func
	.word SH_TAB_func
	.word SH_SPACE_func

keyopertab:
	.word BACKTICK_func
	.word F1_func
	.word F2_func
	.word F3_func
	.word TAB_func
	.word SH_TAB_func
	.word UPARROW_func
	.word DOWNARROW_func
	.word PGUP_func
	.word PGDN_func
	.word HOME_func
	.word END_func
	.word SH_SPACE_func
	.word SPACE_func
	.word RETURN_func
	.word RIGHTARROW_func

F1_func:
	inc jukebox
	lda jukebox
	cmp #3
	bcc :+
	lda #0
:	sta jukebox
	jsr legend_jukebox
	jmp rekey

F2_func:
	inc ticker_behavior
	lda ticker_behavior
	cmp #3
	bcc :+
	lda #0
:	sta ticker_behavior
	jsr legend_jukebox
	lda playback_mode
	cmp #2
	jne rekey
	jsr apply_ticker_behavior
	jmp rekey

F3_func:
	jsr menu_options
	jmp rekey

TAB_func:
	lda playback_mode
	jeq rekey
tabkey:
	lda jukebox
	cmp #2
	beq :+
	jsr loadnext
	jmp continue
:
	jsr loadrandom
	jmp continue

SH_TAB_func:
	lda playback_mode
	jeq rekey
	cmp #1
	beq tabkey
	cmp #3
	beq tabkey
	inc stopping
	jmp rekey

UPARROW_func:
	lda #1
	jsr dirlist_nav_up
	jmp rekey

DOWNARROW_func:
	lda #1
	jsr dirlist_nav_down
	jmp rekey

PGUP_func:
	lda files_full_size
	jsr dirlist_nav_up
	jmp rekey

PGDN_func:
	lda files_full_size
	jsr dirlist_nav_down
	jmp rekey

HOME_func:
	jsr dirlist_nav_home
	jmp rekey

END_func:
	jsr dirlist_nav_end
	jmp rekey

SH_SPACE_func:
	lda playback_mode
	cmp #1
	jeq stopmidi
	cmp #2
	jeq stopzsm
	cmp #3
	jeq stopzcm
	jmp rekey

SPACE_func:
	lda playback_mode
	cmp #1
	jeq pausemidi
	cmp #2
	jeq pausezsm
	cmp #3
	jeq stopzcm
	jmp rekey

RETURN_func:
	jsr dirlist_exec
	jcs songstopped
	jmp rekey

RIGHTARROW_func:
	jsr select_playing_song
	jmp rekey

BACKTICK_func:
	lda playback_mode
	cmp #1
	beq ff_midi
	cmp #2
	beq ff_zsm
	jmp rekey
ff_midi:
	lda X16::Reg::ROMBank
	pha
	lda #$0A
	sta X16::Reg::ROMBank
	lda X16::Reg::RAMBank
	pha

	ldx #0
ff_midi_loop:
	phx
	sei
	jsr midi_playtick
	plx
	cli
	dex
	bne ff_midi_loop

	pla
	sta X16::Reg::RAMBank
	pla
	sta X16::Reg::ROMBank

	jmp rekey
ff_zsm:
	lda X16::Reg::ROMBank
	pha
	lda #$0A
	sta X16::Reg::ROMBank
	lda X16::Reg::RAMBank
	pha

	lda #ZSMKIT_BANK
	sta X16::Reg::RAMBank

	ldx #0
ff_zsm_loop:
	phx
	sei
	lda #2
	jsr zsmkit::zsm_tick
	plx
	cli
	dex
	bne ff_zsm_loop

	pla
	sta X16::Reg::RAMBank
	pla
	sta X16::Reg::ROMBank

	jmp rekey

.proc copy_zsmkit_lib: near
	lda #ZSMKIT_BANK
	sta X16::Reg::RAMBank

	lda #<__ZSMKITLIB_LOAD__
	sta X16::Reg::r0L

	lda #>__ZSMKITLIB_LOAD__
	sta X16::Reg::r0H

	stz X16::Reg::r1L
	lda #$a0
	sta X16::Reg::r1H

	jmp X16::Kernal::MEMORY_DECOMPRESS
.endproc

.segment "ZSMKITLIB"
.incbin "extern/zsmkit-a000.bin.lzsa"
