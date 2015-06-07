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

	; Check if tracing is enabled and we have enabled equations
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetByte
	or a
	jr z,cxRedisp_3DGraph_PostInitGraph
	ld a,(counteqs)
	or a
	jr z,cxRedisp_3DGraph_PostInitGraph

cxRedisp_3DGraph_PostInitTrace:
	call DrawTraceEquation
	call DrawTraceCoords
	jp DrawTraceCursor
	
cxRedisp_3DGraph_PostInitGraph:
	jp DisplayAppTitle

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
	ld a,(counteqs)
	or a
	ret z							; Don't go to trace mode with 0 enabled equations
	; To Trace mode from Graph mode
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetPtr
	ld (hl),1
	call cxRedisp_3DGraph_PostInitTrace
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
		ld a,(counteqs)
		or a
		jr z,GraphKeyHook_Trace_NoEqs
		call EraseTraceCursor
		pop af
	ld hl,trace_y
	ld de,dim_y
	cp kUp
	jr z,KeyHook_Trace_Up
	cp kDown
	jr z,KeyHook_Trace_Down
	ld hl,trace_x
	ld de,dim_x
	cp kLeft
	jr z,KeyHook_Trace_Left
	cp kRight
	jr z,KeyHook_Trace_Right
	cp kTrace
	jr nz,GraphKeyHook_Trace_NotTrace
	ld hl,ateq
	inc (hl)
	ld a,(counteqs)
	cp (hl)
	jr nz,GraphKeyHook_Trace_Trace_Display
	ld (hl),0
GraphKeyHook_Trace_Trace_Display:
	call cxRedisp_3DGraph_InitTrace
	ret
GraphKeyHook_Trace_NotTrace:
	cp kClear
	jr z,KeyHook_Graph_RetQuit

	push af
		call DrawTraceCursor
		pop af
	cp kGraph
	jr nz,GraphKeyHook_OtherKey
	push af
GraphKeyHook_Trace_NoEqs:
		pop af
	; To Graph mode from Trace mode
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetPtr
	ld (hl),0
	; No need to erase Trace cursor here
	call cxRedisp_3DGraph_PostInitGraph
	call GraphRedisp
	ret											; No key
KeyHook_Trace_Up:
KeyHook_Trace_Right:
	ld a,(hl)
	or a
	jr z,KeyHook_Trace_Draw
	dec (hl)
	jr KeyHook_Trace_Draw
KeyHook_Trace_Down:
KeyHook_Trace_Left:
	ld a,(de)
	dec a
	cp (hl)
	jr z,KeyHook_Trace_Draw
	inc (hl)
KeyHook_Trace_Draw:
	call DrawTraceCoords
	jp DrawTraceCursor
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
	ld a,(dim_y)
	push af
		ld e,a
		ld d,0
		ld a,(dim_x)
		call multade				;hl = dim_x * dim_y
		ex de,hl
		ld a,(ateq)
		call multade				;hl = ateq * dim_x * dim_y
		pop af
	push hl
		ld e,a
		ld d,0
		ld a,(trace_x)
		call multade
		pop de
	add hl,de						;hl = (ateq * dim_x * dim_y) + (dim_y * x)
	ld a,(trace_y)
	ld e,a
	ld d,0
	add hl,de						;hl = (ateq * dim_x * dim_y) + (dim_y * x) + y
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

DrawTraceCoords:
	ld bc,(bgcolor)
	ld de,(fgcolor)
	call SetTextColors

	; Draw X
	ld hl,TRACE_COORDS_START_X
	ld (pencol),hl
	ld a,TRACE_COORDS_START_Y
	ld (penrow),a
	ld hl,sXEqu
	call VPutsApp
	ld hl,deltaX_OS
	rst 20h						;OP1 = deltaX
	ld a,(trace_x)
	bcall(_SetXXOP2)			;OP2 = trace_x
	bcall(_FPMult)				;OP1 = trace_x * deltaX
	call OP1toOP2
	ld hl,maxX_OS
	rst 20h
	bcall(_FPSub)				;OP1 = maxX - (trace_x * deltaX)
	bcall(_StoX)
	ld a,8
	res fracDrawLFont,(iy+fontFlags)
	bcall(_DispOP1A)

	; Draw Y
	ld hl,TRACE_COORDS_START_X
	ld (pencol),hl
	ld a,TRACE_COORDS_START_Y+12
	ld (penrow),a
	ld hl,sYEqu
	call VPutsApp
	ld hl,deltaY_OS
	rst 20h						;OP1 = deltaY
	ld a,(trace_y)
	bcall(_SetXXOP2)			;OP2 = trace_y
	bcall(_FPMult)				;OP1 = trace_y * deltaY
	call OP1toOP2
	ld hl,maxY_OS
	rst 20h
	bcall(_FPSub)				;OP1 = minY - (trace_y * deltaY)
	bcall(_StoY)
	ld a,8
	res fracDrawLFont,(iy+fontFlags)
	bcall(_DispOP1A)

	; Draw Z
	ld hl,TRACE_COORDS_START_X
	ld (pencol),hl
	ld a,TRACE_COORDS_START_Y+24
	ld (penrow),a
	ld hl,sZEqu
	call VPutsApp
	
	ld a,(ateq)
	call GetEnabledEq					; Gets the A'th equation token
	push af
		ld hl,BaseZVarName
		rst 20h
		pop af
	ld (OP1 + 2),a
	rst 10h
	jr c,Trace_Compute_EQ_Error
	AppOnErr(Trace_Compute_EQ_Error)
	bcall(_ParseInp)
	AppOffErr
	ld a,8
	res fracDrawLFont,(iy+fontFlags)
	bcall(_DispOP1A)
Trace_Compute_EQ_Error:					; All done with displaying the trace X/Y/Z values

	ret

; Inputs: hl -> tokens
;         bc =  length
;         de =  maxcol
VPutsTokenizedString:
	push de
		push bc
			push hl
				bcall(_Get_Tok_Strng)
				ld hl,OP3
				ld b,a
				call VPutsAppN
				pop hl
			ld a,(hl)
			inc hl
			bcall(_IsA2ByteTok)
			jr nz,VPutsTokenizedString_OneByte
			inc hl
VPutsTokenizedString_OneByte:
			pop bc
		pop de
	dec bc
	ld a,b
	or c
	ret z
	push de
		push hl
			ld hl,(pencol)
			ex de,hl
			or a
			sbc hl,de
			ld de,10
			call cphlde
			pop hl
		pop de
	jr nc,VPutsTokenizedString
	ld a,b
	or a
	jr nz,VPutsTokenizedString
	ld a,c
	dec a
	jr nz,VPutsTokenizedString
	ld hl,OP2
	ld (hl),$BB			;$BB $DB is an ellipsis
	inc hl
	ld (hl),$DB
	dec hl
	bcall(_Get_Tok_Strng)
	ld hl,OP3
	ld b,a
	call VPutsAppN
	ret

DrawTraceEquation:
	bcall(_ClearAppTitle)
	call SetupStatusText
	call SetTextColors
	ld hl,sZEqu
	call VPutsApp
	ld a,(ateq)
	call GetEnabledEq					; Gets the A'th equation token
	push af
		ld hl,BaseZVarName
		rst 20h
		pop af
	ld (OP1 + 2),a
	rst 10h
	ret c
	ex de,hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl
	ld de,TRACE_EQ_END_X
	call VPutsTokenizedString
	ret
	
GetEnabledEq:
	ld c,tZ1
	ld b,a
	ld hl,eq_en_cache
GetEnabledEqLoop:
	ld a,(hl)
	inc hl
	inc c
	or a
	jr z,GetEnabledEqLoop
	dec c
	ld a,b
	or a
	ld a,c
	ret z
	dec b
	inc c
	jr GetEnabledEqLoop

GraphCxVectors:
	.dw cxMain_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.dw cxRedisp_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.db 2
