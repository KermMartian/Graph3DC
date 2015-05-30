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
	or $ff
	ret
	
GraphHook_RetZSet:
	cp a
	ret
	
;-----------------------------------
GraphKeyHook:
	;.db $83
	ld a,b
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
	call SetSpeedFast

	call DisplayNormal
	call Graph_Erase
	call Graph_Rerotate
	call Graph_Redraw
	call DisplayOrg

	or $ff						; Do not do default behavior for this key
	ret
;-----------------------------------
