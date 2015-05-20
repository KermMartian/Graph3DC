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
	ld e,a
	ld d,0
	ld hl,(lts_av)
	add hl,de
	ret

LTS_CacheAV:
	ld hl,AVName
	rst 20h
	bcall(_chkfindsym)
	jr c,LTS_CreateAV
LTS_CacheAV_Exists:
	ld a,b
	or a
	jr z,LTS_CacheAV_Exists_RAM
	bcall(_Arc_Unarc)
	jr LTS_CacheAV
LTS_CacheAV_Exists_RAM:
	inc de
	inc de
	ld (lts_av),de
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
	ld (hl),AXIS_MODE_A		; Offset 1:		Axis Mode = 0
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a				; Offset 2:		BGColor = $0000
	inc hl
	ld (hl),a				; Offset 4:		Color mode = 0
	inc hl
	
	call ZoomStandard3D
	jr LTS_CacheAV

AVName:
	.db AppVarObj,"G3DCS",0