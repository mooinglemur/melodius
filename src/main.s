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
.segment "CODE"

.include "x16.inc"
.scope AudioAPI
	.include "audio.inc"
.endscope

.export playback_mode

.import register_handler
.import deregister_handler
.import midi_parse
.import midi_play
.import midi_stop
.import midi_restart
.import midi_is_playing

.import setup_tiles
.import setup_sprites
.import hide_sprites
.import setup_instruments
.import do_midi_sprites
.import do_zsm_sprites
.import update_instruments

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
.import jukebox
.import legend_jukebox
.import zsm_callback
.import stopping
.import atten
.import draw_lyric

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

	; get skinny charset
	lda #6
	jsr X16::Kernal::SCREEN_SET_CHARSET

	jsr setup_sprites
	jsr setup_tiles
;    jsr setup_instruments

	stz playback_mode

	stz stopping

	lda #1
	sta X16::Reg::RAMBank
	jsr zsmkit::zsm_init_engine

	ldx #0
	lda #<zsm_callback
	ldy #>zsm_callback
	jsr zsmkit::zsm_setcb

	jsr register_handler

	jsr init_directory

	ldx #34
	ldy #20
	jsr draw_file_box

	jsr load_directory
	jsr show_directory
	jsr legend_jukebox
endless:
	DONE_BORDER
	wai

rekey:
	jsr X16::Kernal::GETIN
	beq endkey

	cmp #$85 ; F1
	bne :+
	lda jukebox
	eor #1
	sta jukebox
	jsr legend_jukebox
	bra rekey
:

	cmp #$91 ; up arrow
	bne :+
	lda #1
	jsr dirlist_nav_up
	bra rekey
:

	cmp #$11 ; down arrow
	bne :+
	lda #1
	jsr dirlist_nav_down
	bra rekey
:

	cmp #$82 ; pgup
	bne :+
	lda files_full_size
	jsr dirlist_nav_up
	bra rekey
:

	cmp #$02 ; pgdn
	bne :+
	lda files_full_size
	jsr dirlist_nav_down
	bra rekey
:

	cmp #$13 ; home
	bne :+
	jsr dirlist_nav_home
	bra rekey
:

	cmp #$04 ; end
	bne :+
	jsr dirlist_nav_end
	bra rekey
:

	cmp #$20 ; space
	bne :+
	lda playback_mode
	cmp #1
	beq stopmidi
	cmp #2
	jeq stopzsm
	bra rekey
:

	cmp #$0d
	bne :+
	jsr dirlist_exec
	jcs songstopped
	bra rekey
:
endkey:
	lda dir_needs_refresh
	beq :+
	jsr show_directory
	jsr legend_jukebox
	stz dir_needs_refresh
:

	VIZ_BORDER

	lda playback_mode
	cmp #1
	beq ismidi
	cmp #2
	beq iszsm
	jmp continue
ismidi:
	jsr do_midi_sprites
	jsr update_instruments
	jsr draw_lyric
	jsr midi_is_playing
	bne continue
	lda jukebox
	beq stopmidi
	jsr loadnext
	bcc continue
stopmidi:

	jsr midi_stop
	jsr hide_sprites
	bra songstopped
iszsm:
	jsr check_lazy_load
	jsr draw_zsm_ptr
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

loopck:
	lda jukebox
	beq zsmck
	lda loopctr
	cmp #2
	bcc zsmck
	inc stopping
zsmck:
	ldx #0
	jsr zsmkit::zsm_getstate
	bcs continue

ckloadnext:
	lda jukebox
	beq stopzsm
	jsr loadnext
	bcc continue
stopzsm:
	ldx #0
	jsr zsmkit::zsm_close
	jsr hide_sprites
songstopped:
	ldx #34
	ldy #20
	jsr draw_file_box
	inc dir_needs_refresh
	jsr dir_not_playing
	stz playback_mode
continue:
	DONE_BORDER

	jsr X16::Kernal::STOP ; test stop key
	beq exit

;    bne endless

	jmp endless
exit:
	jsr deregister_handler

	JSRFAR AudioAPI::ym_init, $0A

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

