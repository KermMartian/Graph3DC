windowHook:
	.db $83
	call SetSpeedFast
	push af
		push bc
			push ix
				push hl
					call LTS_CacheAV
					pop hl
				pop ix
			pop bc
		pop af
	; Do not chain, because that's silly.
	or a
	jr nz,windowHook_Not0
	or $ff
	ld a,(ix)
	ret
windowHook_Not0:
	dec a
	jr nz,windowHook_Not1
	or $ff
	ld a,(ix+3)
	ret
windowHook_Not1:
	dec a
	jr nz,windowHook_Not2
	or $ff
	ld a,(ix-3)
	ret
windowHook_Not2:
	dec a
	jr nz,windowHook_Not3
	push ix
		ld l,(ix+1)
		ld h,(ix+2)
		call PutsApp
		ld a,'='
		bcall(_PutC)
		bcall(_SetNumWindow)
		pop hl
	jp windowHook_LoadDisplayValue
windowHook_Not3:
	dec a
	jr nz,windowHook_Not4
	; Initialize things for the Window menu
	call LTS_CacheAV
	call DisplayAppTitle
	ld hl,winMenu_sWindow
	call PutsApp
	ld hl,winTop
	inc (hl)				; Make the title stay when things scroll
	or 1
	ret
windowHook_Not4:
	dec a
	jr nz,windowHook_Not5
windowHook_GetValueToOp1:
	ld a,(hl)
	sub WINDOW_ID_OFFSET
	; Set Xmin, Xmax, Ymin, Ymax to the dialog fields
	push af
		ld hl,Window_Field_Table
		ld e,a
		ld d,0
		add hl,de
		ld a,(hl)
		; Loaded offset into AppVar of value from table
		call LTS_GetPtr
		; Got actual pointer in AppVar
		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		; Loaded value from address of value
		pop af
	cp 2
	jr z,windowHook_LoadSimpleByte
	cp 5
	jr z,windowHook_LoadSimpleByte

	push af
		call FPtoOP1
		pop af
	cp 6
	jr z,windowHook_LoadDone			;ie, don't scale it
	bcall(_OP1toOP4)
	ld a,SETTINGS_AVOFF_SCALEF
	call LTS_GetWord					;ld hl,(scalefactor)			; Used to keep minx/maxx/miny/maxy sane
	call FPtoOP1
	bcall(_OP4toOP2)
	bcall(_FPMult)
	jr windowHook_LoadDone
windowHook_LoadSimpleByte:
	ld a,l
	bcall(_SetXXOP1)
windowHook_LoadDone:
	ld hl,OP1
	or $ff
	ret
windowHook_Not5:
	dec a
	jr nz,windowHook_Not6
	set graphDraw,(iy + graphFlags)
windowHook_SaveOp1toValue:
	ld a,(hl)
	sub WINDOW_ID_OFFSET
	; Set Xmin, Xmax, Ymin, Ymax to the dialog fields
	push af
		ld hl,Window_Field_Table
		ld e,a
		ld d,0
		add hl,de
		ld a,(hl)
		; Loaded offset into AppVar of value from table
		call LTS_GetPtr
		; Got actual pointer in AppVar
		pop af
	cp 2
	jr z,windowHook_SaveSimpleByte
	cp 5
	jr z,windowHook_SaveSimpleByte

	;val = (init * scalefactor) -> init = val / scalefactor
	push hl
		cp 6
		jr z,windowHook_SaveOp1toValue_NoScale
		bcall(_OP1toOP4)
		ld a,SETTINGS_AVOFF_SCALEF
		call LTS_GetWord					;ld hl,(scalefactor)			; Used to keep minx/maxx/miny/maxy sane
		call FPtoOP1
		bcall(_OP1toOP2)
		bcall(_OP4toOP1)
		bcall(_FPDiv)
windowHook_SaveOp1toValue_NoScale:
		call OP1toFP
		pop de
	ex de,hl
	ld (hl),e
	inc hl
	ld (hl),d
	jr windowHook_SaveDone
windowHook_SaveSimpleByte:
	push hl
		bcall(_Int)
		ld a,1
		bcall(_SetXXOP2)
		bcall(_CpOP1OP2)
		jr z,windowHook_SaveSimpleByte_Cancel	; value <= 1
		jr c,windowHook_SaveSimpleByte_Cancel
		ld b,MAX_XY_RES + 1
		ld a,SETTINGS_AVOFF_MAXEQS
		call LTS_GetByte
		cp MAX_EQS
		jr z,windowHook_SaveSimpleByte_HaveMaxVal	; Normal res
		ld b,MAX_XY_RES_HI + 1						; Hi res
windowHook_SaveSimpleByte_HaveMaxVal:
		ld a,b
		bcall(_SetXXOP2)
		bcall(_CpOP1OP2)
		jr z,windowHook_SaveSimpleByte_Cancel	; value >= 256
		jr nc,windowHook_SaveSimpleByte_Cancel
		call ConvOP1C				; Get OP1 into hl...
		ld a,l
		pop hl
	ld (hl),a						;...and save as single byte at (hl)
windowHook_SaveDone:
	or $ff
	ret
windowHook_SaveSimpleByte_Cancel:
		pop hl
	jr windowHook_SaveDone
windowHook_Not6:
	dec a
	jr nz,windowHook_Not7
	ld hl,windowTableStart
windowHook_Disallow:
	or $ff
	ret
windowHook_Not7:
	dec a
	ret z
windowHook_Not8:
	dec a
	jr nz,windowHook_Not9
windowHook_LoadDisplayValue:
	call windowHook_GetValueToOp1
	ld a,$15
	bcall(_FormEReal)
	ld hl,OP3
	bcall(_wputsEOL)
	or $ff
	ret
windowHook_Not9:
	dec a
	jr nz,windowHook_Not10
	ld a,b
	cp kLastEnt
	jr z,windowHook_Disallow
windowHook_Not10:
windowHook_Allow:
	xor a
	ret
