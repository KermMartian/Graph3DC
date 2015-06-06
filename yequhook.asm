YEquHook:
	.db $83
	; Skip 0 and 1
	;bit grfFuncM,(iy + grfModeFlags)		; Only offer 3D mode in Function mode for now
	;ret z
	push af
		push bc
			push hl
				push de
					call LTS_CacheAV		; Our AV is cached for the entire hook duration now
					ld a,SETTINGS_AVOFF_MODE
					call LTS_GetByte
					pop de
				pop hl
			pop bc
		or a
		jp nz,YEquHook_Full
YEquHook_Partial:
		pop af
	cp 3
	jr nz,YEquHook_Partial_Not3

	ld hl,AppTitleOff
	call DisplayAppTitleText
	call ClearPlotLine
	ld hl,PlotLineText
	ld b,0
YEquHook_Partial_PlotLineLoop:
	push bc
		ld a,12-60
		push bc
			inc b
YEquHook_Partial_PlotLineLoop_AddXPos:
			add a,60
			djnz YEquHook_Partial_PlotLineLoop_AddXPos
			ld e,a
			ld d,0
			ld (pencol),de
			pop af
		ld a,36
		ld (penrow),a
		ld de,COLOR_BLACK
		ld bc,COLOR_WHITE
		push de
			cp 3
			jr nc,YEquHook_Partial_PlotLineLoop_DoDisplay
			ld e,a
			ld d,0
			push hl
				ld hl,cxMain_PlotLine_PlotTable
				add hl,de
				add hl,de
				ld a,(hl)
				inc hl
				ld h,(hl)
				ld l,a
				ld a,(hl)
				pop hl
			or a
			jr z,YEquHook_Partial_PlotLineLoop_DoDisplay
			pop de
		push bc
			ld c,e
			ld b,d
YEquHook_Partial_PlotLineLoop_DoDisplay:
			pop de
		call VPutsColored
		pop bc
	inc b
	ld a,4
	cp b
	jr nz,YEquHook_Partial_PlotLineLoop
	call ResetColors
	or 1				; Reset Z flag
	ret

YEquHook_Partial_Not3:
	cp 4								;Entering Plot area
	jr nz,YEquHook_SetZRet
YEquHook_SpecialPlotLine_Setup:
	call SaveMonVectors
	ld hl,SpecialPlotLineVectors
	bcall(_AppInit)
	
	; Back up the current cursor hook
	ld a,SETTINGS_HOOKBACK_CUR
	call LTS_GetPtr						;to hl
	ld de,CursorHookPtr
	ex de,hl
	ld bc,3
	ldir
	ld a,(flags + hookflags2)			; contains cursorHookActive
	and $ff^(1 << cursorHookActive)
	ld (de),a
	
; Set up new cursorhook
	xor a
	ld (menuCurCol),a
	call GetCurrentPage
	ld hl,yEquCursorHook
	bcall(_SetCursorHook)
	jr YEquHook_ResZRet

YEquHook_SetZRet:
	cp a
	ret

YEquHook_Full:
		ld a,$0f & (tZ1 + MAX_EQS)
		ld (EQS + 6),a

		pop af
	or a
	jr nz,yEquHook_Not0

	call InitZEquations
	call DisplayAppTitle
	ld hl,BaseZVarName
	ld de,parseVar
	ld bc,3
	ldir
	jr YEquHook_SetZRet

DisplayAppTitle:
	ld hl,AppTitle
DisplayAppTitleText:
	push hl
		bcall(_ClearAppTitle)
		pop hl
	call SetupStatusText
	call VPutsColored
	call ResetColors
	ret

SetupStatusText:
	ld de,2
	ld (pencol),de
	ld a,14
	ld (penrow),a
	ld de,$e71c
	ld bc,$52aa
	ret

yEquHook_Not0:
	dec a
	jr nz,yEquHook_Not1
	or a					; Reset z flag: not allowed to go to style icons (1 | 1 => z reset)
	ret

yEquHook_Not1:
	dec a
	jr nz,yEquHook_Not2

	ld a,$E9
	bcall(_PutC)
YEquHook_ResZRet:
	or 1
	ret
yEquHook_Not2:
	dec a
	jr nz,yEquHook_Not3
	call ClearPlotLine
	ld hl,PlotLineText2
	ld de,12
	ld (pencol),de
	ld a,36
	ld (penrow),a
	call vputsapp

	; Set max equations
	ld a,(EQS + 7)
	cp tZ1
	jr nc,yEquHook_3_NoEqSetup
	ld a,tZ1
	ld (EQS + 7),a
	ld (OP1 + 2),a
yEquHook_3_NoEqSetup:

	cp $ff
	ret

yEquHook_Not3:
	dec a
	jp z,YEquHook_SpecialPlotLine_Setup

yEquHook_Not4:
	dec a
	jr nz,yEquHook_Not5
#ifdef false
	ld hl,BaseZVarName
	rst 20h
	ld a,(EQS + 7)							; Currently-edited equation
	ld (OP1 + 2),a
	add a,SETTINGS_ZEQUENABLED-tZ1
	call LTS_GetPtr
	push hl
		rst 10h
		pop hl
	ret c
	ex de,hl
	ld a,(hl)
	inc hl
	or (hl)
	ld a,0
	ex de,hl
	jr z,yEquHook_3_Set
	ld a,(hl)
	xor $ff
yEquHook_3_Set:
	ld (hl),a
	ld c,a
#endif
	cp a									; -D-o-n-'-t- do the toggle
	ret

yEquHook_Not5:
	dec a
	jr nz,yEquHook_Not6
	ld a,b
	cp kUp
	jr nz,yEquHook_5_NotUp
	ld a,(EQS + 7)
	cp tZ1
	jr nz,yEquHook_Allow
	ld hl,(editCursor)
	ld de,(editTop)
	or a
	sbc hl,de
	jr nz,yEquHook_Allow
	bcall(_CloseEditEqu)
	jp YEquHook_SpecialPlotLine_Setup		; Go into Plot1...3 line
yEquHook_5_NotUp:
	cp kLeft
	jr nz,yEquHook_Allow
	ld a,(curcol)				; Are we to the right of the =?
	cp 4
	jr nc,yEquHook_Allow
	or $ff						; Don't let us go into the style area
	ret

yEquHook_Not6:
	sub 2
	jr nz,yEquHook_Not8

	push hl
		ld hl,CurCol
		ld (hl),1
		ld a,'Z'
		bcall(_PutC)
		ld a,(EQS + 7)
		add a,$81-tZ1
		bcall(_PutC)
		pop hl
	cp a
	ret

yEquHook_Not8:
yEquHook_Allow:
	cp a
	ret

;--------------------------------------------
ClearPlotLine:
	ld hl,(CurRow)
	push hl
		xor a
		ld (CurCol),a
		ld (CurRow),a
		ld de,COLOR_WHITE
		ld (textFGcolor),de
		ld (textBGcolor),de
		ld b,26
ClearPlotLine_Loop:
		ld a,' '
		push bc
			bcall(_putc)
			pop bc
		djnz ClearPlotLine_Loop
		pop hl
	ld (CurRow),hl
	ret
;--------------------------------------------
cxMain_PlotLine:
	push af
		call LTS_CacheAV
		ld a,SETTINGS_AVOFF_MODE
		call LTS_GetByte
		ld b,a
		or a
		jr nz,cxMain_PlotLine_Mode1
		pop af
	cp kRight
	jr nz,cxMain_PlotLine_NotRight
	ld a,(menuCurCol)
	cp 3
	ret z							; Already at last item
	inc a
cxMain_PlotLine_StoreMenuCurCol:
	ld (menuCurCol),a
	jp cxMain_PlotLine_Redraw
cxMain_PlotLine_NotRight:
	cp kLeft
	jr nz,cxMain_PlotLine_NotLeft
	ld a,(menuCurCol)
	or a
	ret z
	dec a
	jr cxMain_PlotLine_StoreMenuCurCol

cxMain_PlotLine_Mode1:
		pop af
cxMain_PlotLine_NotLeft:
	cp kDown
	jr nz,cxMain_PlotLine_NotDown
cxMain_PlotLine_RestoreApp:
	call ClearCursorHook
	call RestoreMonVectors
	bjump(_maybe_MonRestart)
	;bjump(_Mon)

cxMain_PlotLine_NotDown:
	jr z,cxMain_PlotLine_NotDown
	cp kAlphaEnter
	jr z,cxMain_PlotLine_Enter
	cp kEnter
	jr nz,cxMain_PlotLine_NotEnter
cxMain_PlotLine_Enter:
	ld a,b				; current Mode
	or a
	jr nz,cxMain_PlotLine_3DMode
	ld a,(menuCurCol)
	cp 3
	jr nc,cxMain_PlotLine_3DMode
	ld e,a
	ld d,0
	ld hl,cxMain_PlotLine_PlotTable
	add hl,de
	add hl,de
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	ld a,(hl)
	xor $1
	ld (hl),a			; Toggle plot
	
	; Was a subroutine in Transform
	res smartGraph,(iy + smartFlags)
	set smartGraph_inv,(iy + smartFlags)
	set graphDraw,(iy + graphFlags)
	set textEraseBelow,(iy + textFlags)
	
	;FIX ME XXX TODO
cxMain_PlotLine_Redraw:
	ret

cxMain_PlotLine_3DMode:
	ld a,$3
	sub b
	push af
		ld a,SETTINGS_AVOFF_MODE
		call LTS_GetPtr
		pop af
	ld (hl),a
	call SetFunctionMode
	jr cxMain_PlotLine_RestoreApp
cxMain_PlotLine_NotEnter:
	cp kClear
	jr nz,cxMain_PlotLine_NotClear
	bjump(_JForceCmdNoChar)
cxMain_PlotLine_NotClear:
	cp echoStart1
	ret c
	bjump(_JForceCmd)

yEquCursorHook:
	.db $83
	cp $22
	jr nz,yEquCursorHook_NotDispCursor
	ld bc,COLOR_BLACK		; background
	jr yEquCursorHook_DisplayBlock
yEquCursorHook_NotDispCursor:
	cp $24
	jr nz,yEquCursorHook_NotDispUnderCursor
	ld bc,COLOR_WHITE		; background
yEquCursorHook_DisplayBlock:
	push bc
		call LTS_CacheAV
		ld a,SETTINGS_AVOFF_MODE
		call LTS_GetByte
		pop bc
	ld de,COLOR_BLACK
	ld hl,PlotLineText2
	and 1
	ld a,(menuCurCol)
	push af
		push de
			jr nz,yEquCursorHook_DisplayBlock_NoCheckInvert
			ld e,a
			ld d,0
			push de
				ld h,d
				ld l,e
				add hl,hl				;2
				add hl,de				;3
				add hl,hl				;6
				add hl,de				;7
				ld de,PlotLineText
				add hl,de
				pop de
			cp 3
			jr nc,yEquCursorHook_DisplayBlock_NoCheckInvert
			push hl
				ld hl,cxMain_PlotLine_PlotTable
				add hl,de
				add hl,de
				ld a,(hl)
				inc hl
				ld h,(hl)
				ld l,a
				ld a,(hl)
				pop hl
			or a
			jr z,yEquCursorHook_DisplayBlock_NoCheckInvert
			pop de
		push bc
			 push de
				pop bc
yEquCursorHook_DisplayBlock_NoCheckInvert:
			pop de
		pop af
	call SetTextColors
	; now hl and colors are set. Pick some coordinates
	ld a,36
	ld (penrow),a
	ld b,a
	ld a,12-60
	inc b
yEquCursorHook_DisplayBlock_AddXLoop:
	add a,60
	djnz yEquCursorHook_DisplayBlock_AddXLoop
	ld e,a
	ld d,0
	ld (pencol),de
	call vputsapp
yEquCursorHook_NotDispUnderCursor:
	cp a
	ret

;--------------------------------------------
ClearCursorHook:
	call GetCurrentPage
	ld b,a
	ld a,(CursorHookPtr+2)
	cp b
	ret nz
	ld a,SETTINGS_HOOKBACK_CUR
	call LTS_GetPtr						;to hl
	ld de,cursorHookPtr
	ld bc,3
	ldir
	ld a,(flags + hookflags2)
	and $ff^(1 << cursorHookActive)
	or (hl)
	ld (flags + hookflags2),a
SimpleRet:
	ret
	
;--------------------------------------------
SaveMonVectors:
	ld a,SETTINGS_MONVECBACK
	call LTS_GetPtr
	ld de,monQueue
	ex de,hl
MonVectorCopy:
	ld bc,$0d
	ldir
	ret
RestoreMonVectors:
	ld a,SETTINGS_MONVECBACK
	call LTS_GetPtr
	ld de,monQueue
	jr MonVectorCopy
	
SpecialPlotLineVectors:
	.dw cxMain_PlotLine
	.dw SimpleRet
	.dw ClearCursorHook
	.dw SimpleRet
	.dw SimpleRet
	.dw SimpleRet
	.db 2

cxMain_PlotLine_PlotTable:
	.dw PlotEnabled1
	.dw PlotEnabled2
	.dw PlotEnabled3
	
AppTitle:
	.db "Graph3DC 3D Grapher",0
AppTitleOff:
	.db "Select 3D Mode for Graph3DC",0

PlotLineText:
	.db " Plot1",0
	.db " Plot2",0
	.db " Plot3",0
PlotLineText3:
	.db " 3D Mode",0
PlotLineText2:
	.db " 2D Mode",0
