
.export multiply16x16
.export multiply8x8
.export divide40_24

.export numerator
.export denominator
.export remainder
.export div_result
.export multiplicand
.export multiplier
.export mult_result

.segment "BSS"

div_result:
numerator:
dividend:
    .res 1
mult_result:
    .res 4
denominator:
divisor:
multiplicand:
    .res 2
multiplier:
    .res 2
remainder:
    .res 3
tmp1:
    .res 1

.segment "CODE"

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
