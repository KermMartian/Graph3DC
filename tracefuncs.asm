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
		call DisplayOrg
		pop iy
	ei
	set curOn,(iy+curFlags)
	jr GraphCursorHook_NoFlash

; Uses (trace_x) and (trace_y) to get sx->de and sy->hl
EraseTraceCursor:
	call GetTraceCoords
	ld ix,traceCursorBack
	di
	call DrawSprite_16Bit
	call DisplayOrg
	ei
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

	; Save coordinate area
	di
	call TraceCoordBackRAM_SwapIn
	call DisplayNormal
	ld de,TRACE_COORDS_START_X
	ld hl,TRACE_COORDS_START_Y
	ld bc,TRACE_COORDS_WIDTH
	push bc
		push bc
			ld c,TRACE_COORDS_HEIGHT
			call RectWindow
		call DrawSprite_SetDataMode
		pop de
	ld hl,trace_coord_back
	ld (hl),e
	inc hl
	ld a,TRACE_COORDS_HEIGHT
	ld (hl),a
	inc hl
	push hl
		call multade
		ld b,h
		ld c,l
		pop hl
DrawTraceCoords_SaveAreaLoop:
	in a,($11)
	in a,($11)
	in a,($11)
	ld (hl),a
	inc hl
	in a,($11)
	ld (hl),a
	dec hl
	ld a,(hl)
	out ($11),a
	inc hl
	ld a,(hl)
	out ($11),a
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,DrawTraceCoords_SaveAreaLoop
	call TraceCoordBackRAM_SwapOut
	call DisplayOrg							;Also sets full window
	ei

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

EraseTraceCoords:
	; Restore coordinate area
	di
	call TraceCoordBackRAM_SwapIn
	ld de,TRACE_COORDS_START_X
	ld hl,TRACE_COORDS_START_Y
	ld ix,trace_coord_back
	call DrawSprite_16Bit
	call TraceCoordBackRAM_SwapOut
	call DisplayOrg
	ei
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
			ld de,16
			call cphlde
			pop hl
		pop de
	jr nc,VPutsTokenizedString
	ld a,b
	or a
	jr nz,VPutsTokenizedString_Finish
	ld a,c
	dec a
	jr z,VPutsTokenizedString
VPutsTokenizedString_Finish:
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
	ld de,0
	bcall(_ClearAppTitle)
	call SetupStatusText
	call SetTextColors
	ld a,'Z'
	bcall(_VPutMap)
	ld a,(ateq)
	call GetEnabledEq					; Gets the A'th equation token
	push af
		ld hl,BaseZVarName
		rst 20h
		pop af
	ld (OP1 + 2),a
	add a,$81-tZ1						; Convert Z1-Z6 to sub1 to sub6
	bcall(_VPutMap)
	ld a,'='
	bcall(_VPutMap)
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
