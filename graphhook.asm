GraphHook:
	.db $83
	; Don't chain the graph hook here.  That would be silly.
	; It's only enabled if we're in 3D mode, anyway.
	push af
		push bc
			call LTS_CacheAV
			pop bc
		pop af
	or a
	jr nz,GraphHook_Not0
	call Graph_Setup
	call DisplayOrg
	set graphDraw,(iy+graphFlags)
	cp a
	ret

GraphHook_Not0:
	cp 6
	jr z,GraphKeyHook

	cp 7				; OS about to draw graph
	jr nz,GraphHook_RetZSet

	call SetSpeedFast
	call Graph_Clear_Screen			; calls DisplayNormal
	call Graph_Recompute
	call Graph_Rerotate

	call DisplayNormal
	call Graph_Redraw
	call DisplayOrg
	res graphDraw,(iy+graphFlags)
	
	or $ff
	ret
	
GraphHook_RetZSet:
	cp a
	ret
	
;-----------------------------------
GraphKeyHook:
	;Note that AV location is cached once we get here
	push bc
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
	jr KeyHook_Graph_NoKey
GraphKeyHook_OtherKey:
	cp a
	ret
	;jp Menu_4_Redraw
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
KeyHook_Graph_NoKey:
	or $ff						; Do not do default behavior for this key
	ret
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
	jr KeyHook_Graph_NoKey
KeyHook_Trace_Up:
KeyHook_Trace_Down:
KeyHook_Trace_Left:
KeyHook_Trace_Right:
	call DrawTraceCursor
	jr KeyHook_Graph_NoKey
;-----------------------------------
GraphRedisp:
	call SetSpeedFast
	call Graph_Clear_Screen			; calls DisplayNormal
	call Graph_Redraw
	jp DisplayOrg
;-----------------------------------
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
		call DrawSprite_1Bit_SaveBuf
		pop iy
	ei
	ret

; Uses (trace_x) and (trace_y) to get sx->de and sy->hl
EraseTraceCursor:
	call GetTraceCoords
	ld bc,traceCursorBack
	;call DrawSprite_16Bit
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
		pop hl
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	jp TrashRAM_SwapOut