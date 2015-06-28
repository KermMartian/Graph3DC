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
	
	call DisplayAppTitle
	ld hl,ZoomMenuTable
	ld de,RAMCode
	ld bc,ZoomMenuTableEnd-ZoomMenuTable
	push de
		ldir
		pop hl
ZoomHook_RetZReset:
	or $ff
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
	sub 2
	jr nz,ZoomHook_Not3
	; The following code re-draws the graph if we left the graph via a menu
	; and are now returning to the graph
	ld a,b
	or a
	jr nz,ZoomHook_RetZSet
	bit grfSplit,(iy+sgrFlags)
	jr z,ZoomHook_2_NoSplit

	call LTS_CacheAV
	; Back up the current cxRedispHook
	ld a,SETTINGS_HOOKBACK_REDISP
	call LTS_GetPtr						;to hl
	ld de,cxRedispHookPtr
	ex de,hl
	ld bc,3
	ldir
	ld a,(flags + hookflags4)			; contains MenuHookActive
	and 1 << cxRedispHookActive
	ld (de),a

	; Set up a temporary cxRedispHook
	call GetCurrentPage
	ld hl,SplitscreenRedispHook
	bcall(_SetcxRedispHook)

ZoomHook_RetZSet:
	cp a
	ret

ZoomHook_2_NoSplit:
	push bc
		call LTS_CacheAV
		ld a,SETTINGS_AVOFF_MODE
		call LTS_GetByte
		pop bc
	or a
	ret z					; 3D mode is not enabled
	ld a,SETTINGS_AVOFF_CXCUR
	call LTS_GetByte
	cp kGraph
	jr nz,ZoomHook_RetZSet	; Current context is not our graph context

	;bcall(_MenCatRet)		; This has too many side effects; replaced with the following
	xor a
	ld hl,menuCurrent
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	
	call GraphRedisp
	bjump(_Mon)

ZoomHook_Not3:
	cp a
	ret

ZoomStandard3D:
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,DEFAULT_XY_RES
	cp MAX_EQS
	jr z,ZoomStandard3D_SetDim
	ld b,DEFAULT_XY_RES_HI
ZoomStandard3D_SetDim:
	ld a,SETTINGS_AVOFF_XDIM
	call LTS_GetPtr
	ld (hl),b				; Offset 7:		DimX = maximum possible
	inc hl
	ld (hl),b				; Offset 8:		DimY = maximum possible
	inc hl
	inc hl
	inc hl
	ld de,DEFAULT_XY_ZOOMF
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 11:		ZoomFactor = 0.75
	
	; Set minimum X and Y OS values...
	ld a,SETTINGS_AVOFF_MINXOS
	call LTS_GetPtr
	ld de,MinXYDefault
	ex de,hl
	push hl
		call OPXtoOPX
		ld a,SETTINGS_AVOFF_MINYOS
		call LTS_GetPtr
		pop hl
	call OPXtoOPX
	
	; Set maximum X and Y OS values...
	ld a,SETTINGS_AVOFF_MAXXOS
	call LTS_GetPtr
	ld de,MaxXYDefault
	ex de,hl
	push hl
		call OPXtoOPX
		ld a,SETTINGS_AVOFF_MAXYOS
		call LTS_GetPtr
		pop hl
	call OPXtoOPX
	
	; And now update the fixed-point version
	call windowAutoScale
	
	call DataChecksum_Reset
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
	call DataChecksum_Reset
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
	call DataChecksum_Reset
	ret
	
PostZoomContextSwitch:
	bcall(_MenCatRet)
	bjump(_JForceGraphNoKey)

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