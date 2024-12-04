.include "audio.inc"
.include "x16.inc"

.export register_handler
.export deregister_handler
.export measure_machine_speed

.export use_via_timer
.export via_timer_loops
.export via_timer_iter

.export setup_via_timer
.export clear_via_timer

.import midi_playtick
.import playback_mode

ZSMKIT_BANK = 1

.segment "BSS"
old_irq_handler:
    .res 2
use_via_timer:
    .res 1
via_timer_loops:
    .res 1
via_timer_iter:
    .res 1
via_timer_latch:
    .res 2
machine_speed:
    .res 3
.segment "CODE"

.scope zsmkit
.include "zsmkit.inc"
.endscope

.include "macros.inc"

register_handler:
    php
    sei
    lda X16::Vec::IRQVec
    sta old_irq_handler
    lda X16::Vec::IRQVec+1
    sta old_irq_handler+1

    lda #<handler
    sta X16::Vec::IRQVec
    lda #>handler
    sta X16::Vec::IRQVec+1

    ; use LINE IRQ instead of VBLANK
    lda Vera::Reg::IEN
    and #$FC
    ora #$02
    sta Vera::Reg::IEN
    stz Vera::Reg::IRQLineL

    stz use_via_timer

    plp
    rts

deregister_handler:
    php
    sei

    lda old_irq_handler
    sta X16::Vec::IRQVec
    lda old_irq_handler+1
    sta X16::Vec::IRQVec+1

    ; use VBLANK again
    lda Vera::Reg::IEN
    and #$FC
    ora #$01
    sta Vera::Reg::IEN

    plp
    rts


handler:
    lda X16::Reg::ROMBank
    pha
    lda #$0A
    sta X16::Reg::ROMBank

    lda X16::Reg::RAMBank
    pha
    lda #ZSMKIT_BANK
    sta X16::Reg::RAMBank

    MIDI_BORDER

    ; is it a via timer?
    lda VIA1::Reg::IFR
    and #$40
    bne @via

    lda playback_mode
    beq @end

    cmp #1
    beq @midi
    cmp #2
    beq @zsm
    cmp #3
    beq @zcm
    bra @end

@midi:
    jsr midi_playtick
    bra @end
@via:
    lda VIA1::Reg::T1CL ; clear T1 interrupt flag on VIA
    lda use_via_timer
    beq @vend

    dec via_timer_iter
    bne @vend

    lda via_timer_loops
    sta via_timer_iter
    lda #2
    jsr zsmkit::zsm_tick
@vend:
    pla
    sta X16::Reg::RAMBank

    pla
    sta X16::Reg::ROMBank

    KERNAL_BORDER

    ; return to caller without invoking kernal ISR
    ply
    plx
    pla
    rti
@zcm:
@zsm:
    lda use_via_timer ; conveniently matches the input to zsm_tick
    jsr zsmkit::zsm_tick

@end:
    pla
    sta X16::Reg::RAMBank

    pla
    sta X16::Reg::ROMBank


    KERNAL_BORDER

    lda #2
    sta Vera::Reg::ISR

    jmp (old_irq_handler)


.proc measure_machine_speed: near
	WAITVSYNC
	; grab the least significant byte of the timer
	jsr X16::Kernal::RDTIM
	sta delta1

	lda #5
	ldx #0
	ldy #0
busyloop:
	dey
	bne busyloop
	dex
	bne busyloop
	dec
	bne busyloop

.assert (<busyloop) < 246, error, "measure_machine_speed busyloop crosses a page boundary within the loop, it must be moved"

	jsr X16::Kernal::RDTIM
	sec
	sbc delta1

	cmp #8
	bcc mhz14
	cmp #9
	bcc mhz12
	cmp #12
	bcc mhz10
	cmp #14
	bcc mhz8
	cmp #18
	bcc mhz6
	cmp #28
	bcc mhz4
	cmp #56
	bcc mhz2

mhz1:
	lda #<1000000
	sta machine_speed
	lda #>1000000
	sta machine_speed+1
	lda #^1000000
	sta machine_speed+2
	rts
mhz14:
	lda #<14000000
	sta machine_speed
	lda #>14000000
	sta machine_speed+1
	lda #^14000000
	sta machine_speed+2
	rts
mhz12:
	lda #<12000000
	sta machine_speed
	lda #>12000000
	sta machine_speed+1
	lda #^12000000
	sta machine_speed+2
	rts
mhz10:
	lda #<10000000
	sta machine_speed
	lda #>10000000
	sta machine_speed+1
	lda #^10000000
	sta machine_speed+2
	rts
mhz8:
	lda #<8000000
	sta machine_speed
	lda #>8000000
	sta machine_speed+1
	lda #^8000000
	sta machine_speed+2
	rts
mhz6:
	lda #<6000000
	sta machine_speed
	lda #>6000000
	sta machine_speed+1
	lda #^6000000
	sta machine_speed+2
	rts
mhz4:
	lda #<4000000
	sta machine_speed
	lda #>4000000
	sta machine_speed+1
	lda #^4000000
	sta machine_speed+2
	rts
mhz2:
	lda #<2000000
	sta machine_speed
	lda #>2000000
	sta machine_speed+1
	lda #^2000000
	sta machine_speed+2
	rts
delta1:
	.byte 0
.endproc


; .A = Hz
.proc setup_via_timer: near
    ; tmp1 = remainder
    ; tmp2 = dividend
    ; tmp3 = divisor

    sta IR

	; initialize remainder to 0
	stz tmp1
	stz tmp1+1
	
    lda machine_speed
	sta tmp2
	lda machine_speed+1
	sta tmp2+1
	lda machine_speed+2
	sta tmp2+2

	; initialize divisor to int_rate (default 60)
	lda #$ff
IR = * - 1
	sta tmp3
	stz tmp3+1

	; 24 bits in the dividend
	ldx #24
l1:
	asl tmp2
	rol tmp2+1
	rol tmp2+2
	rol tmp1
	rol tmp1+1
	lda tmp1
	sec
	sbc tmp3
	tay
	lda tmp1+1
	sbc tmp3+1
	bcc l2
	sta tmp1+1
	sty tmp1
	inc tmp2
l2:
	dex
	bne l1

    lda #1
    sta via_timer_loops
	lda tmp2+2
    beq l4    
l3:
	lda tmp2+2
    beq l4
    asl via_timer_loops
l3a:
    lsr tmp2+2
    ror tmp2+1
    ror tmp2
    bra l3
l4:
    lda tmp2
    sta via_timer_latch
	lda tmp2+1
	sta via_timer_latch+1

    lda via_timer_loops
    sta via_timer_iter
    ; set up the via
    php
    sei

    ; set T1 to freerunning mode
    lda #%01000000 
    sta VIA1::Reg::ACR

    ; enable T1 interrupts
    lda #%11000000
    sta VIA1::Reg::IER
    
    ; fill the timer (start it)
    lda via_timer_latch
    sta VIA1::Reg::T1CL
    lda via_timer_latch+1
    sta VIA1::Reg::T1CH

    lda #1
    sta use_via_timer

    plp

	rts
tmp1:
    .byte 0,0,0
tmp2:
    .byte 0,0,0
tmp3:
    .byte 0,0,0
prio:
	.byte 0
.endproc


.proc clear_via_timer: near
    php
    sei

    ; disable T1 interrupts
    lda #%01000000
    sta VIA1::Reg::IER
    
    stz use_via_timer

    plp
    rts
.endproc
