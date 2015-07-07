; Long-Term Storage Functions

LTS_GetByte:
	call LTS_GetPtr
	ld a,(hl)
	ret
	
LTS_GetWord:
	call LTS_GetPtr
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ret

LTS_GetPtr:
	push de
		ld e,a
		ld d,0
		ld hl,(lts_av)
		add hl,de
		pop de
	ret

LTS_SetWord:
	push hl
		push de
			push hl
				call LTS_GetPtr
				pop de
			ld (hl),e
			inc hl
			ld (hl),d
			pop de
		pop hl
	ret
	
LTS_CacheAV:
	call OP1SwapOP6_Safe		; Save OP1
	ld hl,AVName
	rst 20h
	bcall(_chkfindsym)
	jr c,LTS_CreateAV
LTS_CacheAV_Exists:
	ld a,b
	or a
	jr z,LTS_CacheAV_Exists_RAM
	bcall(_Arc_Unarc)
	call OP1SwapOP6_Safe		; Restore OP1
	jr LTS_CacheAV
LTS_CacheAV_Exists_RAM:
	inc de
	inc de
	ld (lts_av),de
	call OP1SwapOP6_Safe		; Restore OP1
	ret

LTS_CreateAV:
	ld hl,AVName
	rst 20h
	ld hl,SETTINGS_AV_SIZE
	bcall(_CreateAppVar)
	inc de
	inc de
	
	ld (lts_av),de
	push de
		; Clear contents
		ld h,d
		ld l,e
		ld (hl),0
		inc de
		ld bc,SETTINGS_AV_SIZE-1				; Zero out the AppVar
		ldir
		pop hl

	; Load defaults
	xor a
	ld (hl),a				; Offset 0: 	Mode = 0
	inc hl
	ld (hl),1				; Offset 1:		Axis Mode = on
	inc hl
	ld (hl),a				; Offset 2:		Bounds Mode = Off
	inc hl
	ld (hl),$ff
	inc hl
	ld (hl),$ff				; Offset 3:		BGColor = $0000
	inc hl
	ld (hl),a				; Offset 5:		Color mode = 0
	ld de,SETTINGS_AVOFF_MAXEQS - SETTINGS_AVOFF_COLOR
	add hl,de
	ld (hl),MAX_EQS
	inc hl
	inc hl					; to SETTINGS_AVOFF_LABEL
	ld (hl),1
	
	call ZoomStandard3D

	ld hl,OP6
	rst 20h					; OP6 -> OP1
	jr LTS_CacheAV

AVName:
	.db AppVarObj,"G3DCS",0