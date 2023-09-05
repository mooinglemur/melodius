
.export multiply16x16
.export multiply8x8
.export divide40_24
.export bin2bcd

.export numerator
.export denominator
.export remainder
.export div_result
.export multiplicand
.export multiplier
.export mult_result
.export bcd_input
.export bcd_result

.segment "BSS"

bcd_result:
div_result:
numerator:
dividend:
    .res 1
mult_result:
    .res 4
denominator:
divisor:
bcd_input:
multiplicand:
    .res 2
multiplier:
    .res 2
remainder:
    .res 3
tmp1:
    .res 1

.segment "CODE"

.proc bin2bcd: near ; 16 bit in, up to 5 bytes out
    BCD_BITS = 19

    curdigit = tmp1
    b = bcd_result

    stz bcd_result
    stz bcd_result+1
    stz bcd_result+2
    stz bcd_result+3
    stz bcd_result+4

    lda #$80 >> ((BCD_BITS - 1) & 3)
    sta curdigit
    ldx #(BCD_BITS - 1) >> 2
    ldy #BCD_BITS - 5    

@loop:
    ; Trial subtract this bit to A:b
    sec
    lda bcd_input
    sbc bcdTableLo,y
    sta b
    lda bcd_input+1
    sbc bcdTableHi,y

    ; If A:b > bcdNum then bcd_input = A:b
    bcc @trial_lower
    sta bcd_input+1
    lda b
    sta bcd_input

@trial_lower:
    ; Copy bit from carry into digit and pick up 
    ; end-of-digit sentinel into carry
    rol curdigit
    dey
    bcc @loop

    ; Copy digit into result
    lda curdigit
    sta bcd_result,x
    lda #$10  ; Empty digit; sentinel at 4 bits
    sta curdigit
    ; If there are digits left, do those
    dex
    bne @loop
    lda bcd_input
    sta bcd_result
    rts

bcdTableLo:
    .byte <10, <20, <40, <80
    .byte <100, <200, <400, <800
    .byte <1000, <2000, <4000, <8000
    .byte <10000, <20000, <40000

bcdTableHi:
    .byte >10, >20, >40, >80
    .byte >100, >200, >400, >800
    .byte >1000, >2000, >4000, >8000
    .byte >10000, >20000, >40000
.endproc

.proc multiply8x8: near
    stz multiplicand+1
    stz multiplier+1
    jmp multiply16x16
.endproc

.proc multiply16x16: near
    lda #0       ; Initialize part of mult_result to 0
    sta mult_result+2
    ldx #16      ; There are 16 bits in multiplier
l1:
    lsr multiplier+1   ;Get low bit of multiplier
    ror multiplier
    bcc l2        ;0 or 1?
    tay           ;If 1, add NUM1 (hi byte of mult_result is in A)
    clc
    lda multiplicand
    adc mult_result+2
    sta mult_result+2
    tya
    adc multiplicand+1
l2:
    ror
    ror mult_result+2
    ror mult_result+1
    ror mult_result
    dex
    bne l1
    sta mult_result+3
    rts
.endproc

.proc divide40_24: near
    stz remainder
    stz remainder+1
    stz remainder+2

    ldx #40	        ;repeat for each bit: ...
divloop:
    asl dividend	
    rol dividend+1	
    rol dividend+2
    rol dividend+3
    rol dividend+4
    rol remainder	
    rol remainder+1
    rol remainder+2
    lda remainder
    sec
    sbc divisor	;substract divisor to see if it fits in
    tay	        ;lb result -> Y, for we may need it later
    lda remainder+1
    sbc divisor+1
    sta tmp1
    lda remainder+2
    sbc divisor+2
    bcc skip	;if carry=0 then divisor didn't fit in yet

    sta remainder+2	;else save substraction result as new remainder,
    lda tmp1
    sta remainder+1
    sty remainder	
    inc dividend 	;and INCrement result cause divisor fit in 1 times
skip:
    dex
    bne divloop
    rts
.endproc
