MenuHook:				;aka ZoomHook
	.db $83
	cp a				;set zero flag
	push hl
		ld hl,SETTINGS_HOOKBACK_MENU
		call HookChainer
	ret nz							;other hook said to do something /non-standard/
	or a
	jr nz,ZoomHook_Not0

	ld a,(menuCurrent)
	cp mZoom3D
	jr nz,MenuHook_NotSelected3D
	ld hl,PostZoomContextSwitch				; Return here after the following
	push hl
		ld a,b
		or a
		jp z,ZoomIn3D
		dec a
		jp z,ZoomOut3D
		jp ZoomStandard3D

MenuHook_NotSelected3D:
	ld a,(menuCurrent)						; On the second iteration through, with b=0,
	cp mZoom								; set our custom stuff
	jr nz,ZoomHook_RetZSet

	call LTS_CacheAV
	ld a,SETTINGS_AVOFF_MODE
	call LTS_GetByte
	or a
	jr z,ZoomHook_RetZSet					; Don't trigger our menu if 3D mode is not on
	
	ld hl,ZoomMenuTable
	ld de,RAMCode
	ld bc,ZoomMenuTableEnd-ZoomMenuTable
	push de
		ldir
		pop hl
ZoomHook_RetZReset:
	or $ff
	ret
ZoomHook_RetZSet:
	cp a
	ret
ZoomHook_Not0:
	dec a
	jr nz,ZoomHook_Not1
	ld a,(menuCurrent)
	cp mZoom
	jr nz,ZoomHook_RetZSet
	ld a,mZoom3D
	ld (menuCurrent),a
	jr ZoomHook_RetZSet
ZoomHook_Not1:
	cp a
	ret

ZoomStandard3D:
	ld a,SETTINGS_AVOFF_XDIM
	call LTS_GetPtr
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
	ret
	
ZoomIn3D:
	ld a,SETTINGS_AVOFF_SCALEF
	call LTS_GetPtr
	push hl
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		push hl
			ld a,SETTINGS_AVOFF_ZOOMF
			call LTS_GetWord
			ex de,hl
			pop bc
		call signed_multbcde				; returns dehl
		ld d,e
		ld e,h
		pop hl
	ld (hl),e
	inc hl
	ld (hl),d
	ret
	
ZoomOut3D:
	ld a,SETTINGS_AVOFF_SCALEF
	call LTS_GetPtr
	push hl
		ld b,(hl)
		inc hl
		ld h,(hl)
		ld c,0
		push hl
			push bc
				ld a,SETTINGS_AVOFF_ZOOMF
				call LTS_GetWord
				ex de,hl
				pop bc
			pop af
		call signed_divabcde			; abc/de -> abc remainder hl
		pop hl
	ld (hl),c
	inc hl
	ld (hl),b
	ret
	
PostZoomContextSwitch:
	ld a,kGraph
	bcall(_NewContext)
	bjump(_Mon)

ZOOM_MENU_CHOICES	.equ 3
ZoomMenuTable:
	.db 1	;how many headers?
	.db ZOOM_MENU_CHOICES 	;how many choices under 1st header?
	.db 5Eh	;string table entry for "ZOOM"
ZoomMenuTable_Entries:
	.db 0,kZIn
	.db 0,kZOut	;these are 2-byte keypresses of each entry
	.db 0,kStd
ZoomMenuTableEnd: