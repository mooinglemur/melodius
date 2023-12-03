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
.segment "CODE"

.include "x16.inc"
.scope AudioAPI
	.include "audio.inc"
.endscope

.export playback_mode
.export paused

.import register_handler
.import deregister_handler
.import midi_parse
.import midi_play
.import midi_stop
.import midi_restart
.import midi_is_playing

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

	lda #1
	sta X16::Reg::RAMBank
	jsr zsmkit::zsm_init_engine

	ldx #0
	lda #<zsm_callback
	ldy #>zsm_callback
	jsr zsmkit::zsm_setcb

	jsr register_handler

	jsr midi_init
	jsr init_directory

	ldx #34
	ldy #20
	jsr draw_file_box

	jsr load_directory
	jsr show_directory
	jsr legend_jukebox
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
	jeq endkey

	cmp #$85 ; F1
	bne :++
	inc jukebox
	lda jukebox
	cmp #3
	bcc :+
	lda #0
:	sta jukebox
	jsr legend_jukebox
	bra rekey
:

	cmp #$89 ; F2
	bne :++
	inc ticker_behavior
	lda ticker_behavior
	cmp #3
	bcc :+
	lda #0
:	sta ticker_behavior
	jsr legend_jukebox
	lda playback_mode
	cmp #2
	bne rekey
	jsr apply_ticker_behavior
	bra rekey
:

	cmp #$86 ; F3
	bne :+
	jsr menu_options
	bra rekey
:

	cmp #$09 ; tab
	bne :++
	lda playback_mode
	beq rekey
tabkey:
	lda jukebox
	cmp #2
	beq :+
	jsr loadnext
	jmp continue
:
	jsr loadrandom
	jmp continue
:

	cmp #$18 ; shift-tab
	bne :+
	lda playback_mode
	beq rekey
	cmp #1
	beq tabkey
	cmp #3
	beq tabkey
	inc stopping
	bra rekey
:	

	cmp #$91 ; up arrow
	bne :+
	lda #1
	jsr dirlist_nav_up
	jmp rekey
:

	cmp #$11 ; down arrow
	bne :+
	lda #1
	jsr dirlist_nav_down
	jmp rekey
:

	cmp #$82 ; pgup
	bne :+
	lda files_full_size
	jsr dirlist_nav_up
	jmp rekey
:

	cmp #$02 ; pgdn
	bne :+
	lda files_full_size
	jsr dirlist_nav_down
	jmp rekey
:

	cmp #$13 ; home
	bne :+
	jsr dirlist_nav_home
	jmp rekey
:

	cmp #$04 ; end
	bne :+
	jsr dirlist_nav_end
	jmp rekey
:

	cmp #$a0 ; shift+space
	bne :+
	lda playback_mode
	cmp #1
	jeq stopmidi
	cmp #2
	jeq stopzsm
	cmp #3
	jeq stopzcm
	jmp rekey
:

	cmp #$20 ; space
	bne :+
	lda playback_mode
	cmp #1
	jeq pausemidi
	cmp #2
	jeq pausezsm
	cmp #3
	jeq stopzcm
	jmp rekey
:

	cmp #$0d
	bne :+
	jsr dirlist_exec
	jcs songstopped
	jmp rekey
:

	cmp #$1d ; right arrow
	bne :+
	jsr select_playing_song
	jmp rekey
:
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
	lda zsmkit::pcm_busy
	bne continue
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

	lda #1
	jsr zsmkit::zsm_init_engine

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
