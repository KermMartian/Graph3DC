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
					or a
					jp nz,YEquHook_Full
YEquHook_Partial:
					pop de
				pop hl
			pop bc
			
			set 1,(iy+$3a)					;These two are required to make the graph style cursor work
			set 7,(iy+$52)					;in the normal Y= menu
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
			pop bc
		call YEquHook_SetPenRow	; destroys a and f		
		ld (penrow),a
		ld a,b					; restore selected item
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
	ld (menuButtonNum),a
	call GetCurrentPage
	ld hl,yEquCursorHook
	bcall(_SetCursorHook)

	; Deal with flags
	set saIndicForce,(iy+extraIndicFlags)	;Make 2nd/alpha appear in status area
	res 0,(iy+$13)
	res 2,(iy+$44)					; Because this is what Transform does!
	res 1,(iy+$3a)					;Undo what we did to make the graph style cursor
	res 7,(iy+$52)					;work properly
	
	jr YEquHook_ResZRet

YEquHook_SetZRet:
	cp a
	ret

YEquHook_Full:
						call SwapZYFuncs_In
						ld a,SETTINGS_AVOFF_MAXEQS
						call LTS_GetByte
						add a,tY1
						and $0f								; was ld a,$0f & (tZ1 + MAX_EQS)
						ld (EQS + 6),a
					pop de
				pop hl
			pop bc
		pop af
	or a
	jr nz,yEquHook_Not0

	call InitZEquations
	call DisplayAppTitle
;	ld hl,BaseZVarName
;	ld de,parseVar
;	ld bc,3
;	ldir

	ld a,SETTINGS_AVOFF_ERRGOTOPEND
	call LTS_GetPtr
	xor a
	bit cmdGoto,(iy+cmdFlags)
	jr z,yEquHook_0_NoGoto
	
	push hl
		; Back up the current RawKeyHook
		ld a,SETTINGS_HOOKBACK_KEY
		call LTS_GetPtr						;to hl
		ld de,GetKeyHookPtr
		ex de,hl
		ld bc,3
		ldir
		ld a,(flags + hookflags2)			; contains rawKeyHookActive
		and 1 << GetCSCHookActive
		ld (de),a

		call GetCurrentPage
		ld hl,yEquKeyHook
		bcall(_SetGetKeyHook)		; Used to handle ERR:GOTO ourselves.
		pop hl
	
	ld a,1
yEquHook_0_NoGoto:
	ld (hl),a
	res cmdGoto,(iy+cmdFlags)
	jr YEquHook_SetZRet

yEquHook_Not0:
	dec a
	jr nz,yEquHook_Not1
	or $ff					; Reset z flag: not allowed to go to style icons
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
	call YEquHook_SetPenRow	; destroys a and f		
	call vputsapp
	
	; Display explanation, saving and restoring curRow and curCol
	call Yequ_DisplayExplanation

	cp $ff
	ret

yEquHook_Not3:
	dec a
	jp z,YEquHook_SpecialPlotLine_Setup

yEquHook_Not4:
	dec a
	jr nz,yEquHook_Not5

	cp a									; -D-o-n-'-t- do the toggle
	ret

yEquHook_Not5:
	dec a
	jr nz,yEquHook_Not6
	ld a,b
	; Check for keys that require re-drawing the explanation line
	cp kUp
	jr nz,yEquHook_5_NotUp
	ld a,(EQS + 7)
	cp tY1
	jr nz,yEquHook_Allow
	ld hl,(editCursor)
	ld de,(editTop)
	or a
	sbc hl,de
	jr nz,yEquHook_Allow
	bcall(_CloseEditEqu)
	jp YEquHook_SpecialPlotLine_Setup		; Go into Plot1...3 line
yEquHook_5_NotUp:
	; Important note: on the CSE, possibly others, the OS rampages over
	; an area starting at $8b0a when equations grow to cover more lines,
	; an area that stores the starting pixel coordinates of each equation.
	; If we don't perform this mapping, then the looped coordinate adjustment
	; the OS performs to correct coordinates below an equation that grew (or
	; shrank) will rampage over all memory (around $6a90 on page $06 in the OS).
	; I put code at the beginning of the full hook that undoes this after a key
	; is handled.
	;call YEquHook_MapEQStoY
	jr yEquHook_Allow

yEquHook_Not6:
	sub 2
	jr nz,yEquHook_Not8

	push hl
		ld hl,CurCol
		push hl
			ld (hl),1
			ld a,'Z'
			bcall(_PutC)
			pop hl
		inc (hl)
		pop hl
	jr yEquHook_Allow

yEquHook_Not8:
	dec a
	jr nz,yEquHook_Not9
	call DataChecksum_Reset

yEquHook_Not9:
yEquHook_Allow:
	cp a
	ret

;--------------------------------------------
Yequ_DisplayExplanation:
	call YEquHook_SetPenRow
	ld hl,YEQU_EXPLAIN_START_X
	ld (pencol),hl
	ld hl,COLOR_GRAY
	ld (drawFGColor),hl
	ld hl,sYequExplain
	call vPutsApp
	call ResetColors
	ret
;--------------------------------------------
yEquKeyHook:
	.db $83
	push af
		push bc
			; Do we even have something to be working on?
			call LTS_CacheAV
			ld a,SETTINGS_AVOFF_ERRGOTOPEND
			call LTS_GetPtr						;to hl
			ld a,(hl)
			or a
			jr z,yEquKeyHook_RemoveSelf
			
			inc hl								;to SETTINGS_AVOFF_PENDEQ
			ld a,(hl)
			dec hl
			ld b,a
			ld a,(EQS + 7)
			cp b
			jr z,yEquKeyHook_RemoveSelf
	
			; Do in fact need to send a key.
			pop bc
		pop af
	ld a,kDown
	cp a
	ret
yEquKeyHook_RemoveSelf:
			ld (hl),0				; No more pending stuff.
			
			;GetKeyHook
			ld de,GetKeyHookPtr+2
			ld bc,($ff^(1 << GetCSCHookActive))*256 + SETTINGS_HOOKBACK_KEY
			ld hl,flags + hookflags2
			call DisableHook
			pop bc
		pop af
	cp $1b
	ret nz
	ld a,b
	or a							; NZ if there's a real keycode. Whatever.
	ret
;--------------------------------------------
ClearYEquationArea:
	ld a,(winTop)
	dec a
	ld de,21
	call multade				; quotient in hl
	ld de,PXLMINY_WITHSTATUS
	add hl,de
	push hl
		ld a,239
		sub l
		ld de,320*2/4
		call multade
		ex de,hl
		pop hl
	jr FillAreaWhite
	
ClearPlotLine:
	ld a,(winTop)
	dec a
	ld de,21
	call multade				; quotient in hl
	ld de,PXLMINY_WITHSTATUS
	add hl,de
	ld de,320*21*2/4
	; hl = minimum Y coordinate
	; de = number of 2-pixel pairs
	; ix = color
FillAreaWhite:
	ld ix,$ffff

FillAreaColor:
	di
	push ix
		push de
			push hl
				call DisplayNormal
				call Full_Window
				pop hl
			ld a,$20
			call Write_Display_Control
			ld a,$50
			call Write_Display_Control

			ld hl,0
			ld a,$21				; set write X coordinate
			call Write_Display_Control
			ld a,$52
			call Write_Display_Control
			
			ld	a,$22
			out	($10),a
			out	($10),a
			pop de				; Number of loop iterations
		pop hl
	ld c,$11
ClearPlotLine_Loop:
	out	(c),h
	out	(c),l
	out	(c),h
	out	(c),l
	dec	de
	ld a,d
	or e
	jr nz,ClearPlotLine_Loop
	call DisplayOrg
	ei
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
	ld a,(menuButtonNum)
	cp 3
	ret z							; Already at last item
	inc a
cxMain_PlotLine_StoremenuButtonNum:
	ld (menuButtonNum),a
	jp cxMain_PlotLine_Redraw
cxMain_PlotLine_NotRight:
	cp kLeft
	jr nz,cxMain_PlotLine_NotLeft
	ld a,(menuButtonNum)
	or a
	ret z
	dec a
	jr cxMain_PlotLine_StoremenuButtonNum

cxMain_PlotLine_Mode1:
		pop af
cxMain_PlotLine_NotLeft:
	cp kDown
	jr nz,cxMain_PlotLine_NotDown
cxMain_PlotLine_RestoreApp:
	call cxPutAway_PlotLine
	call RestoreMonVectors
	call setWindow_OS					; If we're switching from 3D to 2D mode
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
	ld a,(menuButtonNum)
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
	push af
		call nz,SwapZYFuncs_In
		pop af
	call z,SwapZYFuncs_Out
	call SetFunctionMode
	call ClearYEquationArea
	set grfSChanged,(iy+sgrFlags)
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
	and 1							; SETTINGS_AVOFF_MODE byte
	ld a,(menuButtonNum)
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
	ld b,a							
	ld a,12-60
	inc b
yEquCursorHook_DisplayBlock_AddXLoop:
	add a,60
	djnz yEquCursorHook_DisplayBlock_AddXLoop
	ld e,a
	ld d,0
	ld (pencol),de
	call YEquHook_SetPenRow	; destroys a and f		
	call vputsapp							; hl was already set earlier in this routine
yEquCursorHook_NotDispUnderCursor:
	cp a
	ret

;--------------------------------------------
cxPutAway_PlotLine:
	res saIndicForce,(iy+extraIndicFlags)	;Do not force 2nd/alpha to appear in status area

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
YEquHook_SetPenRow:
	ld a,$24
	bit grfSplit,(iy+sgrFlags)
	jr z,YEquHook_SetPenRow_Set
	ld a,$9e
YEquHook_SetPenRow_Set:
	ld (penrow),a
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
	.dw cxPutAway_PlotLine
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
