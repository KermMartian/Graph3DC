MaxHLDE:
	ld a,h
	xor d
	rla
	push af
		or a
		sbc hl,de
		add hl,de
		jr nc,MaxHLDE_Return
		ex de,hl
MaxHLDE_Return:
		pop af
	ret nc
	ex de,hl
	ret
	
MinHLDE:
	call MaxHLDE
	ex de,hl
	ret

cphlde_fp:
	ld a,h
	xor d
	rla
	jr nc,cphlde_fp_go
	ex de,hl
cphlde_fp_go:
	or a
	sbc hl,de
	add hl,de
	ret

;Input: A = Multiplier, DE = Multiplicand, HL = 0, C = 0
;Output: A:HL = Product
signed_multade:
	bit 7,a
	push af
		jr z,signed_multade_noflip1
		neg
signed_multade_noflip1:
		call multade
		pop af
	ret z
	ld de,0
	ex de,hl
	or a
	sbc hl,de
	ret

multade:
	bit 7,d
	push af
		jr z,multade_noflip1
		ld hl,0
		or a
		sbc hl,de
		ex de,hl
multade_noflip1:
		ld c,0
		ld l,c
		ld h,c
		add	a,a		; optimised 1st iteration
		jr	nc,$+4
		ld	h,d
		ld	l,e
		ld b,7
multade_loop:
		add	hl,hl		; unroll 7 times
		rla			; ...
		jr	nc,$+4		; ...
		add	hl,de		; ...
		adc	a,c		; ...
		djnz multade_loop
		pop af
	ret z
	ld de,0
	ex de,hl
	or a
	sbc hl,de
	ret

divabcde:
	ld hl,0
	ld ix,24
	push af
		push hl
divabcde_loop:
			pop hl
		pop af
	scf \ rl c		; unroll 24 times
	rl	b		; ...
	rla			; ...
	adc	hl,hl		; ...
	sbc	hl,de		; ...
	jr	nc,$+4		; ...
	add	hl,de		; ...
	dec	c		; ...
	push af
		push hl
			dec ix
			push ix
				pop hl
			ld a,l
			or a
			jr nz,divabcde_loop
			pop hl
		pop af
	ret
	
signed_divabcde:
	bit 7,d
	push de
		jr z,signed_divabcde_posd
		ld hl,0
		or a
		sbc hl,de
		ex de,hl
signed_divabcde_posd:
		bit 7,a
		push af
			jr z,signed_divabcde_posa
			push af
				ld a,b \ cpl \ ld b,a
				ld a,c \ cpl \ ld c,a
				pop af
			cpl
			inc c
			jr nz,{@}
			inc b
			jr nz,{@}
			inc a
@:
signed_divabcde_posa:
			call divabcde
			pop hl
		ld l,a
		pop af
	xor h
	and $80
	ld a,l
	ret z
	push af
		ld a,b \ cpl \ ld b,a
		ld a,c \ cpl \ ld c,a
		pop af
	cpl
	inc c
	ret nz
	inc b
	ret nz
	inc a
	ret

multbcde:
	ld hl,0
	sla	e		; optimised 1st iteration
	rl	d
	jr	nc,$+4
	ld	h,b
	ld	l,c
	ld a,15
multbcde_loop:
	add	hl,hl		; unroll 15 times
	rl	e		; ...
	rl	d		; ...
	jr	nc,$+6		; ...
	add	hl,bc		; ...
	jr	nc,$+3		; ...
	inc	de		; ...
	dec a
	jr nz,multbcde_loop
	ret
	
signed_multbcde:
	ld a,b
	and $80
	push af
		jr z,signed_multbcde_bpos
		ld hl,0
		or a
		sbc hl,bc
		ld b,h
		ld c,l
signed_multbcde_bpos:
		ld a,d
		and $80
		push af
			jr z,signed_multbcde_dpos
			ld hl,0
			or a
			sbc hl,de
			ex de,hl
signed_multbcde_dpos:
			pop hl
		pop af
	xor h
	push af
		call multbcde
		pop af
	ret z
negate_dehl:
	ld a,d \ cpl \ ld d,a
	ld a,e \ cpl \ ld e,a
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc l
	ret nz
	inc h
	ret nz
	inc e
	ret nz
	inc d
	ret
	
abshl_fp:
	bit 7,h
	ret z
negate_hl:
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	inc hl
	ret

;b.c*d.e -> de.hl, so returns e.h as de
signed_multbcde_fp:
	call signed_multbcde
	ld d,e
	ld e,h
	ret

; Input: hl = angle
.warn "We are cheating: sin and cos can only handle 4 angles"
mcosf:
	ld de,DELTA_ANGLE
	call cphlde
	jr z,mcosf_pos
	ld de,NEG_DELTA_ANGLE
	call cphlde
	jr z,mcosf_neg
	ld de,FP_PI
	call cphlde
	jr z,mcosf_pi
	ld hl,$0100					;cos(0) = 1
	ret
mcosf_pi:
	ld hl,COS_PI
	ret
mcosf_pos:
mcosf_neg:
	ld hl,COS_DELTA_ANGLE		;cos(DELTA_ANGLE) = COS_DELTA_ANGLE
	ret							;cos(-DELTA_ANGLE) = COS_DELTA_ANGLE

msinf:
	ld de,DELTA_ANGLE
	call cphlde
	jr z,msinf_pos
	ld de,FP_PI
	call cphlde
	jr z,msinf_pi
	ld de,NEG_DELTA_ANGLE
	call cphlde
	ret nz						;sin(0) = 0
msinf_neg:
	ld hl,SIN_NEG_DELTA_ANGLE	;sin(-DELTA_ANGLE) = -SIN_DELTA_ANGLE
	ret
msinf_pos:
	ld hl,SIN_DELTA_ANGLE
	ret
msinf_pi:
	ld hl,SIN_PI
	ret

cphlde:
	or a
	sbc hl,de
	add hl,de
	ret

; hl + de -> hl or hl - de -> hl
; Destroys flags and a
subhlde_fp:
	ld a,d \ cpl \ ld d,a		;hl-de => hl+(-de)
	ld a,e \ cpl \ ld e,a
	inc de
addhlde_fp:
	ld a,h
	xor d
	add hl,de
	bit 7,a
	ret nz						;Opposite sign: no danger of under/overflow
	ld a,d
	xor h						;If operands had same sign, and sum has same sign as well, no problem
	bit 7,a
	ret z						;0 ^ 0 or 1 ^ 1 (doesn't matter which operand, because same sign)
	bit 7,d
	jr nz,addhlde_fp_underflow
addhlde_fp_overflow:
	ld hl,FP_MAX
	ret
addhlde_fp_underflow:
	ld hl,FP_MIN
	ret
	
; Input: HL = Dividend, C = Divisor, A = 0
; Output: HL = Quotient, A = Remainder
g3dc_divhlc:
	ld b,16
	xor a
DivHLC_Loop:
	add	hl,hl		; unroll 16 times
	rla			; ...
	cp	c		; ...
	jr	c,$+4		; ...
	sub	c		; ...
	inc	l		; ...
	djnz DivHLC_Loop
	ret

; Input: signed 8.8 fixed-point number in HL
; Output: signed OS real in OP1
FPtoOP1:
	push hl
		call abshl_fp
		bcall(_SetXXXXOP2)
		bcall(_Op2toOp1)
		ld hl,256
		bcall(_SetXXXXOP2)
		bcall(_FPDiv)
		pop af
	and $80
	ret z
	bcall(_InvOP1S)
	ret

OP1toFP:
	ld hl,OP1
	ld a,(hl)
	bit 7,a				; sign bit
	push af
		and $7f			; make it positive
		ld (hl),a
		ld hl,256
		bcall(_SetXXXXOP2)
		bcall(_FPMult)
		ld hl,FP_MAX+1
		bcall(_SetXXXXOP2)
		bcall(_CpOP1OP2)
		jr z,OP1toFP_Trunc
		jr nc,OP1toFP_Trunc
		call ConvOP1C
OP1toFP_Complete:
		pop af
	ret z
	call negate_hl
	ret
OP1toFP_Trunc:
		ld hl,FP_MAX
		jr OP1toFP_Complete
		
;Attempts to convert the TI float in OP1 into an unsigned integer in HL.
;
;Throws:
; - A data type error if OP1 doesn't hold a nonnegative real number.
; - A domain error if the value cannot be exactly converted to an unsigned
;   16-bit integer.
;
; I: (OP1)=float
; O: A=0, BC=((uint)(OP1))%10, DE=OP1+8, HL=(uint)(OP1), (OP1)=(float)0
;FO: S=0, Z=1, H=0, P/V=0, N=1, C=0
;CC: 355 + 210*d
;    d = (OP1)!=0 ? floor(log10((OP1))) + 1 : 1
ConvOP1C:
;Throws an error if OP1 doesn't hold a nonnegative real number.
	ld   a,(OP1)
;   or   a
;   jr   nz,ErrDataType
;Initializes the 16-bit accumulator to 0.
	ld   h,a
	ld   l,a
ConvOP1_Loop:
;Multiplies the 16-bit accumulator by 10, checking for overflow.
	ld   b,h
	ld   c,l
	add   hl,hl
	adc   a,a
	add   hl,hl
	adc   a,a
	add   hl,bc
	adc   a,a
	add   hl,hl
	adc   a,a
	jr   nz,ErrDomain
;Rotates the first three mantissa bytes of OP1 left by a nibble, collecting the
;highest nibble/digit rotated out.
	ex   de,hl
	ld   hl,OP1+4
	rld
	dec   l
	rld
	dec   l
	rld
	dec   l
;Adds the highest nibble/digit rotated out to the 16-bit accumulator, checking
;for overflow. Decrements the exponent and continues looping if it doesn't
;become $7F. Doesn't care about bad exponents, as the 16-bit accumulator would
;overflow eventually.
	ld   c,a
	xor   a
	ld   b,a
	dec   (hl)
	ex   de,hl
	add   hl,bc
	jr   c,ErrDomain
	jp   po,ConvOP1_Loop
ConvOP1_CheckIntLoop:
;Returns successfully if the last byte of the mantissa has been checked.
	ld   a,(OP1+8)&$FF
	sub   e
	ret   z
;Continues if the next byte of the mantissa is zero.
	inc   e
	ld   a,(de)
	or   a
	jr   z,ConvOP1_CheckIntLoop
ErrDomain:
;Throws a domain error.
	;B_CALL(_ErrDomain)
	ret
ErrDataType:
;Throws a data type error.
	;B_CALL(_ErrDataType) 
	ret

cphlbc:
	or a
	sbc hl,bc
	add hl,bc
	ret
