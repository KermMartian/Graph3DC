;--------------------------------------------------
InitZEquations:
	ld b,MAX_EQS
InitZEquations_Loop:
	push bc
		ld hl,BaseZVarName
		rst 20h
		pop af
	push af
		add a,tZ1-1			;-1 because b goes from 6 to 1, not 5 to 0
		ld (OP1+2),a
		rst 10h
		jr nc,InitZEquations_Loop_Exists
		ld hl,0
		bcall(_CreateEqu)
InitZEquations_Loop_Exists:
		pop bc
	djnz InitZEquations_Loop
	ret

;--------------------------------------------------
CheckEnabled_Setup:
	xor a
	ld (counteqs),a
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,a
	ld c,tZ1
	ld hl,eq_en_cache
CheckEnabled_Setup_Loop:
	push bc
		push hl
			push bc
				ld hl,BaseZVarName
				rst 20h
				pop bc
			ld a,c
			ld (OP1+2),a
			rst 10h
			ld a,0
			jr c,CheckEnabled_Setup_Loop_Store
			bit 5,(hl)
			jr z,CheckEnabled_Setup_Loop_Store
			ld a,1
			ld hl,counteqs
			inc (hl)
CheckEnabled_Setup_Loop_Store:
			pop hl
		ld (hl),a
		pop bc
	inc hl
	inc c
	djnz CheckEnabled_Setup_Loop
	ret

CheckEnabledA:
	sub tZ1
	ld e,a
	ld d,0
	ld hl,eq_en_cache
	add hl,de
	ld a,(hl)
	or a
	ret

;--------------------------------------------------
SetFunctionMode:
	; Set Function mode - can only touch a!
	ld a,(iy+grfModeFlags)
	and $ff^((1 << grfPolarM) | (1 << grfParamM) | (1 << grfRecurM))
	or 0+(1 << grfFuncM)
	ld (iy+grfModeFlags),a
	ret

;--------------------------------------------------
; These functions swap the Y1..Y[2 or 5]
; functions into Z1..Z[2 or 5], and vice versa
; They work by directly modifying the VAT.
SwapZYFuncs_In:
	ld a,SETTINGS_AVOFF_EQSWAPPED
	call LTS_GetPtr
	ld a,(hl)
	or a
	ret nz							; Already swapped
	ld (hl),1
	jp SwapZYFuncs

SwapZYFuncs_Out:
	ld a,SETTINGS_AVOFF_EQSWAPPED
	call LTS_GetPtr
	ld a,(hl)
	or a
	ret z							; Not swapped
	ld (hl),0
SwapZYFuncs:
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,a							; Maximum equations to process
SwapZYFuncs_Loop:
	push bc
		ld a,b
		add a,tY1 - 1				; Save a dec a here
		ld hl,OP1
		ld (hl),EquObj
		inc hl
		ld (hl),tVarEqu
		inc hl
		ld (hl),a
		inc hl
		ld (hl),0
		rst 10h						; Find Yx
		jr c,SwapZYFuncs_Loop_Skip	; But this should never happen!!
		pop bc
	push bc
		ld de,-7
		add hl,de
		push hl							; Save offset to VAT for swapping
			ld a,b
			add a,tZ1 - 1				; Save a dec a here
			ld hl,OP1
			ld (hl),EquObj
			inc hl
			ld (hl),tVarEqu
			inc hl
			ld (hl),a
			inc hl
			ld (hl),0
			rst 10h						; Find Zx
			pop de
		jr c,SwapZYFuncs_Loop_Skip	; But this should never happen!!
		ld bc,-7
		add hl,bc
		ld b,(hl)
		ld a,(de)
		ex de,hl
		ld (de),a
		ld (hl),b
SwapZYFuncs_Loop_Skip:
		pop bc
	djnz SwapZYFuncs_Loop
	ret