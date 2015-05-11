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
	
	ex de,hl
	; Load defaults
	xor a
	ld (hl),a				; Offset 0: 	Mode = 0
	inc hl
	ld (hl),a				; Offset 1:		Axis Mode = 0
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a				; Offset 2:		BGColor = $0000
	inc hl
	ld (hl),a				; Offset 4:		Color mode = 0
	inc hl
	ld a,DEFAULT_XY_RES
	ld (hl),a				; Offset 5:		DimX = maximum possible
	inc hl
	ld (hl),a				; Offset 6:		DimY = maximum possible
	inc hl
	ld de,1.00*INT_TO_8P8
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 7:		ScaleFactor = 1
	inc hl
	ld de,0.75*INT_TO_8P8
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 9:		ZoomFactor = 0.75
	inc hl
	ld de,DEFAULT_XY_MIN
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 11:	MinX = default
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 13:	MinY = default
	inc hl
	ld de,DEFAULT_XY_MAX
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 15:	MaxX = default
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 17:	MaxY = default
	
	jr LTS_CacheAV
	
AVName:
	.db AppVarObj,"G3DCS",0