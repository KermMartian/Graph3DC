cxInit_Format:
	call setWindow_Full
	bcall(_ClrWindow)
	call MultiMenu_Init
	ld a,1
	ld (menuCurCol),a
	set saIndicForce,(iy+extraIndicFlags)	; Force 2nd/alpha to appear in status area
	set curAble,(iy+curFlags)				; Flash that cursor
	ret

cxRedisp_Format:
	call DisplayAppTitle
	call LTS_CacheAV
	ld ix,(lts_av)
	ld hl,FormatTable
	call MultiMenu_Redisplay
	call DrawFormatColorIcon
	ret

cxMain_Format:
	push af
		call LTS_CacheAV
		pop af
	ld ix,(lts_av)						; Settings pointer
	ld hl,FormatTable					; Menu table pointer
	ld de,cxMain_Format_Callback		; Called if a setting is changed
	call MultiMenu_HandleKey
	jp nz,KeyHook_OtherKey
cxMain_Format_NoKey:
	call DrawFormatColorIcon
	xor a
	ret
	
cxMain_Format_Callback:
	call DataChecksum_Reset
	ld a,(menuCurRow)
	cp 4								; Resolution
	ret nz
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	cp MAX_EQS
	jr nz,cxMain_Format_Callback_SetHighRes
	ld b,MAX_XY_RES
	jr cxMain_Format_Callback_SetRes
cxMain_Format_Callback_SetHighRes:
	ld b,MAX_XY_RES_HI
cxMain_Format_Callback_SetRes:
	ld a,SETTINGS_AVOFF_XDIM
	call LTS_GetPtr
	ld (hl),b
	inc hl
	ld (hl),b
	ret
	
cxPutaway_Format:
	call ResetColors
	ret
	
formatCursorHook:
	.db $83
	cp $22
	jr z,formatCursorHook_Flash
	cp $24
	jr nz,formatCursorHook_NoFlash
formatCursorHook_Flash:
	call LTS_CacheAV
	ld ix,(lts_av)
	ld hl,FormatTable
	call MultiMenu_DrawCursor
formatCursorHook_NoFlash:
	cp a
	ret

DrawFormatColorIcon:
	; Draw the graph color icon
	ld a,SETTINGS_AVOFF_COLOR
	call LTS_GetPtr
	ld e,(hl)
	ld d,0
	ld hl,GridIcon_Table
	add hl,de
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
		pop ix
	ld de,290
	ld hl,79
	call DrawSprite_4Bit
	jp Full_Window

FormatCxVectors:
	.dw cxMain_Format
	.dw SimpleRet
	.dw cxPutaway_Format
	.dw cxRedisp_Format
	.dw SimpleRet
	.dw SimpleRet
	.db 2
