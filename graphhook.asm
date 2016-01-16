SplitscreenGraphHook:
	.db $83
	cp a									;set zero flag
	push af
		bit grfSplit,(iy+sgrFlags)			; We're enabled, but splitscreen mode is off. Why
		jr z,SplitscreenGraphHook_Chain		; would this fire? Eh, let's be safe.
		push bc
			push hl
				call LTS_CacheAV			; Already preserves OP1
				ld a,SETTINGS_AVOFF_MODE
				call LTS_GetByte
				pop hl
			pop bc
		or a
		jr z,SplitscreenGraphHook_Chain
		pop af
	or a
	jr z,SplitscreenGraphHook_Redraw
	cp a
	ret

SplitscreenGraphHook_Chain:
		pop af
	push hl
		ld hl,SETTINGS_HOOKBACK_REGR
		call HookChainer						; Let the other hook decide [now preserves OP1
	ret

SplitscreenRedispHook:
	.db $83
	call LTS_CacheAV
	call SplitscreenGraphHook_Redraw

	; Uninstall myself
	ld de,cxRedispHookPtr+2
	ld bc,($ff^(1 << cxRedispHookActive))*256 + SETTINGS_HOOKBACK_REDISP
	ld hl,flags + hookflags4
	call DisableHook
	
	cp a
	ret

SplitscreenGraphHook_Redraw:
	ld hl,(curRow)					; row and column
	push hl
		call SwapZYFuncs_IsSwapped
		push af
			call nz,SwapZYFuncs_Out
			
			; Fix the homescreen, because the OS seems to hate us.
			ld a,(cxCurApp)
			cp kQuit
			bcallz(_RstrShadow)
			
			; The actual drawing process starts here
			call SetRunIndic_Friendly
			call SetSpeedFast
			call Graph_Setup
			call Graph_Clear_Screen			; calls DisplayNormal
			
			; Draw the horizontal divider
			di
			push iy
				ld hl,(PxlMaxY)
				push hl
					ld hl,PXLMAXY_FULL
					ld (PxlMaxY),hl
					ld de,0
					ld hl,319
					ld bc,154
					push bc
						pop ix
					ld iy,COLOR_BLACK
					call ColorLine_Override
					pop hl
				ld (PxlMaxY),hl
				pop iy
			
			call DataChecksum_Check
			jr z,SplitscreenGraphHook_Redraw_NoRecompute
			call Graph_Recompute
			call Graph_Rerotate
SplitscreenGraphHook_Redraw_NoRecompute:
			call DisplayNormal
			call Graph_Redraw

			call DisplayOrg
			call SetRunIndic_Normal
			res graphDraw,(iy+graphFlags)
			call DataChecksum_Set
			pop af
		call nz,SwapZYFuncs_In
		call ResetColors
		call DisableTextColors
		pop hl
	ld (curRow),hl
	or $ff
	ret

cxInit_3DGraph:
	set appMenus,(iy+appFlags)				; Let menus be opened.
	call LTS_CacheAV
	call Graph_Setup
	call DisplayOrg
	;call DataChecksum_Reset
	ret

cxRedisp_3DGraph:
	set saIndicForce,(iy+extraIndicFlags)	; Force 2nd/alpha to appear in status area
	call LTS_CacheAV
	call SetSpeedFast
	call Graph_Clear_Screen			; calls DisplayNormal

	call DataChecksum_Check
	jr z,cxRedisp_3DGraph_NoRecompute
	call Graph_Recompute
	call Graph_Rerotate

cxRedisp_3DGraph_NoRecompute:
	call SetRunIndic_Friendly
	call DisplayNormal
	call Graph_Redraw
	call DisplayOrg
	call SetRunIndic_Normal
	call DataChecksum_Set

	; Check if tracing is enabled and we have enabled equations
	ld a,SETTINGS_AVOFF_TRACE
	call LTS_GetPtr
	ld a,(hl)
	ld (hl),0								; Tracing disabled
	or a
	jr z,cxRedisp_3DGraph_PostInitGraph
	ld a,(counteqs)
	or a
	jr z,cxRedisp_3DGraph_PostInitGraph
	ld (hl),1								; Tracing actually enabled

cxRedisp_3DGraph_PostInitTrace:
	call DrawTraceEquation
	call DrawTraceCoords
	jp DrawTraceCursor
	
cxRedisp_3DGraph_PostInitGraph:
	jp DisplayAppTitle

cxPutaway_3DGraph:
	res saIndicForce,(iy+extraIndicFlags)	; Do not force 2nd/alpha to appear in status area
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
	jp nz,GraphKeyHook_Trace
GraphKeyHook_Graph:
	ld de,DELTA_ANGLE
	cp kUp
	jr z,GraphKeyHook_Graph_StoreAlpha
	cp kLeft
	jr z,GraphKeyHook_Graph_StoreBeta
	ld de,NEG_DELTA_ANGLE
	cp kDown
	jr z,GraphKeyHook_Graph_StoreAlpha
	cp kRight
	jr z,GraphKeyHook_Graph_StoreBeta
	cp kClear
	jr z,GraphKeyHook_Graph_RetQuit
	cp kAdd
	jp z,GraphKeyHook_ZoomIn
	cp kSub
	jp z,GraphKeyHook_ZoomOut
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
	jp GraphKeyHook_NoKey								; No key
GraphKeyHook_Graph_RetQuit:
	bjump(_JForceCmdNoChar)

GraphKeyHook_Graph_StoreAlpha:
	ld (alpha),de
	jr GraphKeyHook_Graph_Rerotate
GraphKeyHook_Graph_StoreBeta:
	ld (beta),de
GraphKeyHook_Graph_Rerotate:
	call SetSpeedFast
	call SetRunIndic_Friendly
	call DisplayNormal
	call Graph_Erase
	call Graph_Rerotate
	call Graph_Redraw
	call DisplayOrg
	call SetRunIndic_Normal
	call DataChecksum_Set
	jr GraphKeyHook_NoKey

GraphKeyHook_OtherKey:
KeyHook_OtherKey:
	cp kClear
	jr nz,KeyHook_NotClear
	bjump(_JForceCmdNoChar)
KeyHook_NotClear:
	cp appStart
	jr c,KeyHook_OtherForceCmd
	cp echoStart1
	jr nc,KeyHook_OtherForceCmd
	ld sp,(onSP)
	; a still contains key -> new context to load
	ld b,a
	bcall(_NewContext0)
	xor a
	bjump(_SendKpress)

KeyHook_OtherForceCmd:
	bjump(_JForceCmd)
;-----------------------------------
GraphKeyHook_Trace:
	push af
		ld a,(counteqs)
		or a
		jr z,GraphKeyHook_Trace_NoEqs
		call EraseTraceCursor
		call EraseTraceCoords
		pop af
	ld hl,trace_y
	ld de,dim_y
	cp kUp
	jr z,GraphKeyHook_Trace_Up
	cp kDown
	jr z,GraphKeyHook_Trace_Down
	ld hl,trace_x
	ld de,dim_x
	cp kLeft
	jr z,GraphKeyHook_Trace_Left
	cp kRight
	jr z,GraphKeyHook_Trace_Right
	cp kTrace
	jr nz,GraphKeyHook_Trace_NotTrace
	ld hl,ateq
	inc (hl)
	ld a,(counteqs)
	cp (hl)
	jr nz,GraphKeyHook_Trace_Trace_Display
	ld (hl),0
GraphKeyHook_Trace_Trace_Display:
	call cxRedisp_3DGraph_PostInitTrace
	ret
GraphKeyHook_Trace_NotTrace:
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
GraphKeyHook_NoKey:
	xor a
	ret											; No key
GraphKeyHook_Trace_Up:
GraphKeyHook_Trace_Right:
	ld a,(hl)
	or a
	jr z,GraphKeyHook_Trace_Draw
	dec (hl)
	jr GraphKeyHook_Trace_Draw
GraphKeyHook_Trace_Down:
GraphKeyHook_Trace_Left:
	ld a,(de)
	dec a
	cp (hl)
	jr z,GraphKeyHook_Trace_Draw
	inc (hl)
GraphKeyHook_Trace_Draw:
	call DrawTraceCoords
	call DrawTraceCursor
	jr GraphKeyHook_NoKey
;-----------------------------------
GraphKeyHook_ZoomIn:
	call ZoomIn3D
	bjump(_JForceGraphNoKey)

GraphKeyHook_ZoomOut:
	call ZoomOut3D
	bjump(_JForceGraphNoKey)
;-----------------------------------
GraphRedisp:
	call SetRunIndic_Friendly
	call SetSpeedFast
	call Graph_Clear_Screen			; calls DisplayNormal
	call Graph_Redraw
	call DisplayOrg
	jp SetRunIndic_Normal
;-----------------------------------
GraphCxVectors:
	.dw cxMain_3DGraph
	.dw SimpleRet
	.dw cxPutaway_3DGraph
	.dw cxRedisp_3DGraph
	.dw SimpleRet
	.dw SimpleRet
	.db 2
