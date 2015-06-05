cxInit_3DGraph:
	call LTS_CacheAV
	call Graph_Setup
	call DisplayOrg
	set graphDraw,(iy+graphFlags)
	ret

cxRedisp_3DGraph:
	call LTS_CacheAV
	call SetSpeedFast
	call Graph_Clear_Screen			; calls DisplayNormal
	call Graph_Recompute
	call Graph_Rerotate

	call DisplayNormal
	call Graph_Redraw
	call DisplayOrg
	res graphDraw,(iy+graphFlags)
	ret
	
;-----------------------------------
cxMain_3DGraph:
	push bc
		call LTS_CacheAV
		ld a,SETTINGS_AVOFF_TRACE
		call LTS_GetByte
		or a
		pop bc
	ld a,b
	jr nz,GraphKeyHook_Trace
GraphKeyHook_Graph:
	ld de,DELTA_ANGLE
	cp kUp
	jr z,KeyHook_Graph_StoreAlpha
	cp kLeft
	jr z,KeyHook_Graph_StoreBeta
	ld de,NEG_DELTA_ANGLE
	cp kDown
	jr z,KeyHook_Graph_StoreAlpha
	cp kRight
	jr z,KeyHook_Graph_StoreBeta
	cp kClear
	jr z,KeyHook_Graph_RetQuit
	cp kTrace
	jr nz,GraphKeyHook_OtherKey
	; To Trace mode from Graph mode
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetPtr
	ld (hl),1
	call DrawTraceCursor
	ret								; No key
KeyHook_Graph_RetQuit:
	bjump(_JForceCmdNoChar)
KeyHook_Graph_StoreAlpha:
	ld (alpha),de
	jr KeyHook_Graph_Rerotate
KeyHook_Graph_StoreBeta:
	ld (beta),de
KeyHook_Graph_Rerotate:
	set graphDraw,(iy+graphFlags)
	call SetSpeedFast

	call DisplayNormal
	call Graph_Erase
	call Graph_Rerotate
	call Graph_Redraw
	call DisplayOrg
	res graphDraw,(iy+graphFlags)
GraphKeyHook_OtherKey:
	cp kClear
	jr nz,KeyHook_Graph_NotClear
	bjump(_JForceCmdNoChar)
KeyHook_Graph_NotClear:
	cp echoStart1
	ret c
	bjump(_JForceCmd)
;-----------------------------------
GraphKeyHook_Trace:
	push af
		call EraseTraceCursor
		pop af
	cp kUp
	jr z,KeyHook_Trace_Up
	cp kLeft
	jr z,KeyHook_Trace_Left
	cp kDown
	jr z,KeyHook_Trace_Down
	cp kRight
	jr z,KeyHook_Trace_Right
	cp kClear
	jr z,KeyHook_Graph_RetQuit

	push af
		call DrawTraceCursor
		pop af
	cp kGraph
	jr nz,GraphKeyHook_OtherKey
	; To Graph mode from Trace mode
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetPtr
	ld (hl),0
	; No need to erase Trace cursor here
	call GraphRedisp
	ret											; No key
KeyHook_Trace_Up:
KeyHook_Trace_Down:
KeyHook_Trace_Left:
KeyHook_Trace_Right:
	call DrawTraceCursor
	ret
;-----------------------------------
GraphRedisp:
	call SetSpeedFast
	call Graph_Clear_Screen			; calls DisplayNormal
	call Graph_Redraw
	jp DisplayOrg
;-----------------------------------
GraphCursorHook:
	.db $83
	cp $22
	jr z,GraphCursorHook_Flash
	cp $24
	jr nz,GraphCursorHook_NoFlash
GraphCursorHook_Flash:
	call LTS_CacheAV
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetByte
	or a
	ret z
	bit curOn,(iy+curFlags)
	jr nz,EraseTraceCursor
	; It's off: draw the cursor
DrawTraceCursor:
	ld hl,traceCursorPalette
	push hl
		ld de,(bgcolor)
		ld (hl),d
		inc hl
		ld (hl),e
		inc hl
		ld de,(fgcolor)
		ld (hl),d
		inc hl
		ld (hl),e
		call GetTraceCoords			; pixel de->x, pixel hl->y
		pop bc
	di
	push iy
		ld iy,traceCursorBack
		ld ix,TraceCursor
		call DrawSprite_1Bit_SaveBuf
		pop iy
	ei
	set curOn,(iy+curFlags)
	jr GraphCursorHook_NoFlash

; Uses (trace_x) and (trace_y) to get sx->de and sy->hl
EraseTraceCursor:
	call GetTraceCoords
	ld ix,traceCursorBack
	call DrawSprite_16Bit
	res curOn,(iy+curFlags)
GraphCursorHook_NoFlash:
	cp a
	ret

GetTraceCoords:
	call TrashRAM_SwapIn
	ld a,(dim_x)
	push af
		ld e,a
		ld d,0
		ld a,(dim_y)
		call multade				;hl = dim_x * dim_y
		ex de,hl
		ld a,(ateq)
		call multade				;hl = ateq * dim_x * dim_y
		pop af
	push hl
		ld e,a
		ld d,0
		ld a,(trace_y)
		call multade
		pop de
	add hl,de						;hl = (ateq * dim_x * dim_y) + (dim_x * y)
	ld a,(trace_x)
	ld e,a
	ld d,0
	add hl,de						;hl = (ateq * dim_x * dim_y) + (dim_x * y) + x
	add hl,hl
	; Have the offset. Load the X, Y, Z pointers
	push hl
		ld de,grid_x
		add hl,de
		ld (pgrid_x),hl
		pop de
	push de
		ld hl,grid_y
		add hl,de
		ld (pgrid_y),hl
		pop de
	ld hl,grid_z
	add hl,de
	ld (pgrid_z),hl

	; Now load the screen coord pointers, and compute
	ld hl,axes_sy
	ld (pgrid_sy),hl
	push hl
		ld hl,axes_sx
		ld (pgrid_sx),hl
		push hl
			ld b,1
			call Map_B_Points
			pop hl
		ld e,(hl)
		inc hl
		ld d,(hl)
		dec de
		dec de
		dec de					; Move the pixel to the center of the 7x7 sprite
		pop hl
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	dec hl
	dec hl
	dec hl						; Move the pixel to the center of the 7x7 sprite
	jp TrashRAM_SwapOut
	
GraphCxVectors:
	.dw cxMain_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.dw cxRedisp_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.db 2
