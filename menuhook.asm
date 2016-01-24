MenuHook:				;aka MenuHook
	.db $83
	cp a				;set zero flag
	push hl
		ld hl,SETTINGS_HOOKBACK_MENU
		call HookChainer
	ret nz							;other hook said to do something /non-standard/
	push af
		push bc
			push de
				push hl
					call LTS_CacheAv
					pop hl
				pop de
			pop bc
		pop af
	or a
	jr nz,MenuHook_Not0

	ld a,SETTINGS_AVOFF_MENUTRIG
	call LTS_GetPtr
	ld (hl),1						;A menu has triggered, so things are dirty.		 
	
	ld a,(menuCurrent)
	cp mZoom3D
	jr nz,MenuHook_NotSelected3D
	ld hl,PostZoomContextSwitch				; Return here after the following
	push hl
		ld a,b
		or a
		jp z,ZoomIn3D
		dec a
		jp z,ZoomOut3D
		jp ZoomStandard3D

MenuHook_NotSelected3D:
	ld a,SETTINGS_AVOFF_MODE
	call LTS_GetByte
	or a
	jp z,MenuHook_RetZSet					; Don't trigger our menu if 3D mode is not on

	ld a,(menuCurrent)						; On the second iteration through, with b=0,
	ld hl,CalcMenuTable
	ld bc,CalcMenuTableEnd-CalcMenuTable
	cp mCalculate
	jr z,MenuHook_SetTable

	ld hl,ZoomMenuTable
	ld bc,ZoomMenuTableEnd-ZoomMenuTable
	cp mZoom								; set our custom stuff
	jp nz,MenuHook_RetZSet

MenuHook_SetTable:
	push hl
		push bc
			call DisplayAppTitle
			pop bc
		pop hl
	ld de,RAMCode
	push de
		ldir
		pop hl
MenuHook_RetZReset:
	or $ff
	ret
	
MenuHook_Not0:
	dec a
	jr nz,MenuHook_Not1

	ld a,SETTINGS_AVOFF_MODE
	call LTS_GetByte
	or a
	jp z,MenuHook_RetZSet					; Don't trigger our menu if 3D mode is not on

	ld hl,menuCurrent
	ld a,(hl)
	cp mZoom
	jr nz,MenuHook_RetZSet
	
	ld (hl),mZoom3D

	jr MenuHook_RetZSet
MenuHook_Not1:
	dec a
	jr nz,MenuHook_Not2
	
	ld a,SETTINGS_AVOFF_MODE
	push hl
		call LTS_GetByte
		pop hl
	or a
	jr z,MenuHook_RetZSet					; Don't trigger our menu if 3D mode is not on

	; Custom menu title for Calc menu
	ld a,(menuCurrent)
	cp mCalculate
	jr nz,MenuHook_RetZSet					; Only for calculate menu
	ld a,c
	or a
	jr nz,MenuHook_RetZSet					; Only for first header

	push hl									; Current title pointer
		call LTS_CacheAV
		ld a,SETTINGS_AVOFF_MODE
		call LTS_GetByte
		or a
		pop hl
	jr z,MenuHook_RetZSet					; Don't trigger our menu if 3D mode is not on
	
	ld hl,sCalcDisabled
	call PutsApp
	bcall(_EraseEOL)
	jr MenuHook_RetZReset
	
MenuHook_Not2:
	dec a
	jr nz,MenuHook_Not3
	; The following code re-draws the graph if we left the graph via a menu
	; and are now returning to the graph

	ld a,SETTINGS_AVOFF_MODE
	call LTS_GetByte
	or a
	jr z,MenuHook_RetZSet					; Don't trigger our menu if 3D mode is not on

	push bc
		push de
			ld a,b
			or a
			jr z,MenuHook_3_CheckRedispHook

			cp kExtApps					; Going to an external app?
			jr nz,MenuHook_3_RetZSet
			
			; Restore previous context
			ld a,SETTINGS_AVOFF_CXCUR
			call LTS_GetPtr
			ld a,(hl)
			ld (cxCurApp),a
			jr MenuHook_3_RetZSet
MenuHook_3_CheckRedispHook:
			bit grfSplit,(iy+sgrFlags)
			jr z,MenuHook_3_NoSplit
MenuHook_3_SetRedispHook:
			call LTS_CacheAV
			; Back up the current cxRedispHook
			ld a,SETTINGS_HOOKBACK_REDISP
			call LTS_GetPtr						;to hl
			ld de,cxRedispHookPtr
			ex de,hl
			ld bc,3
			ldir
			ld a,(flags + hookflags4)			; contains MenuHookActive
			and 1 << cxRedispHookActive
			ld (de),a

			; Set up a temporary cxRedispHook
			call GetCurrentPage
			ld hl,SplitscreenRedispHook
			bcall(_SetcxRedispHook)
			
MenuHook_3_RetZSet:
			pop de
		pop bc

MenuHook_RetZSet:
	cp a
	ret

MenuHook_3_NoSplit:
			call LTS_CacheAV
			ld a,SETTINGS_AVOFF_MODE
			call LTS_GetByte
			pop de
		pop bc
	or a
	ret z					; 3D mode is not enabled
	ld a,SETTINGS_AVOFF_CXCUR
	call LTS_GetByte
	cp kGraph
	jr nz,MenuHook_RetZSet	; Current context is not our graph context

	;bcall(_MenCatRet)		; This has too many side effects; replaced with the following
	xor a
	ld hl,menuCurrent
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	
MenuHook_3_Redisp:
	call cxRedisp_3DGraph
	bjump(_Mon)

MenuHook_Not3:
	cp a
	ret

ZoomStandard3D:
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,DEFAULT_XY_RES
	cp MAX_EQS
	jr z,ZoomStandard3D_SetDim
	ld b,DEFAULT_XY_RES_HI
ZoomStandard3D_SetDim:
	ld a,SETTINGS_AVOFF_XDIM
	call LTS_GetPtr
	ld (hl),b				; Offset 7:		DimX = maximum possible
	inc hl
	ld (hl),b				; Offset 8:		DimY = maximum possible
	inc hl
	inc hl
	inc hl
	ld de,DEFAULT_XY_ZOOMF
	ld (hl),e
	inc hl
	ld (hl),d				; Offset 11:		ZoomFactor = 0.75
	
	; Set minimum X and Y OS values...
	ld a,SETTINGS_AVOFF_MINXOS
	call LTS_GetPtr
	ld de,MinXYDefault
	ex de,hl
	push hl
		call OPXtoOPX
		ld a,SETTINGS_AVOFF_MINYOS
		call LTS_GetPtr
		pop de
	ex de,hl
	call OPXtoOPX
	
	; Set maximum X and Y OS values...
	ld a,SETTINGS_AVOFF_MAXXOS
	call LTS_GetPtr
	ld de,MaxXYDefault
	ex de,hl
	push hl
		call OPXtoOPX
		ld a,SETTINGS_AVOFF_MAXYOS
		call LTS_GetPtr
		pop de
	ex de,hl
	call OPXtoOPX
	
	; And now update the fixed-point version and mark the graph dirty
	call windowAutoScale
	call DataChecksum_Reset
	ret
	
ZoomIn3D:
	ld a,SETTINGS_AVOFF_ZOOMF
	call LTS_GetWord
	call FPtoOP1
	bcall(_OP1toOP5)

	call Zoom_GetCenter_X					; to OP1
	call OP1toOP4
	ld a,SETTINGS_AVOFF_MINXOS
	call Mult_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale

	ld a,SETTINGS_AVOFF_MAXXOS
	call Mult_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale
	
	call Zoom_GetCenter_Y					; to OP1
	call OP1toOP4
	ld a,SETTINGS_AVOFF_MINYOS
	call Mult_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale
	
	ld a,SETTINGS_AVOFF_MAXYOS
	call Mult_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale
	
	; And now update the fixed-point version and mark the graph dirty
	call windowAutoScale
	call DataChecksum_Reset
	ret
	
ZoomOut3D:
	ld a,SETTINGS_AVOFF_ZOOMF
	call LTS_GetWord
	call FPtoOP1
	bcall(_OP1toOP5)

	call Zoom_GetCenter_X					; to OP1
	call OP1toOP4
	ld a,SETTINGS_AVOFF_MINXOS
	call Div_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale

	ld a,SETTINGS_AVOFF_MAXXOS
	call Div_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale
	
	call Zoom_GetCenter_Y					; to OP1
	call OP1toOP4
	ld a,SETTINGS_AVOFF_MINYOS
	call Div_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale
	
	ld a,SETTINGS_AVOFF_MAXYOS
	call Div_PtedReal_ByOP4OP5			; OP4 = center, OP5 = scale
	
	; And now update the fixed-point version and mark the graph dirty
	call windowAutoScale
	call DataChecksum_Reset
	ret
	
PostZoomContextSwitch:
	bcall(_MenCatRet)
	bjump(_JForceGraphNoKey)

Zoom_GetCenter_X:
	call GetWindow_MinXMaxX
	jr Zoom_GetCenter
Zoom_GetCenter_Y:
	call GetWindow_MinYMaxY
Zoom_GetCenter:
	bcall(_FPAdd)
	bcall(_TimesPt5)
	ret

Mult_PtedReal_ByOP4OP5:
	call LTS_GetPtr
	push hl
		rst 20h
		call OP4toOP2
		bcall(_FPSub)						; Adjust for center
		bcall(_OP5toOP2)
		bcall(_FPMult)
		jr PtedReal_ByOP4OP5_Finish
	
Div_PtedReal_ByOP4OP5:
	call LTS_GetPtr
	push hl
		rst 20h
		call OP4toOP2
		bcall(_FPSub)						; Adjust for center
		bcall(_OP5toOP2)
		bcall(_FPDiv)
PtedReal_ByOP4OP5_Finish:
		call OP4toOP2
		bcall(_FPAdd)
		pop de
	jp OP1toOPX							; Store back to AppVar
	
