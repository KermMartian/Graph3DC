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
GraphCxVectors:
	.dw cxMain_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.dw cxRedisp_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.db 2
