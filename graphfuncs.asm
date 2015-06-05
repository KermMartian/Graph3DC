Graph_Setup:
	; Basic setup stuff
	call SetSpeedFast

	; Load graph constants
	ld a,SETTINGS_AVOFF_XDIM
	call LTS_GetPtr
	ld a,(hl)				; Offset SETTINGS_AVOFF_XDIM
	ld (dim_x),a
	srl a
	ld (trace_x),a
	inc hl
	ld a,(hl)				; Offset SETTINGS_AVOFF_YDIM
	ld (dim_y),a
	srl a
	ld (trace_y),a
	inc hl
	ld e,(hl)				; Offset SETTINGS_AVOFF_SCALEF
	inc hl
	ld d,(hl)
	ld (scalefactor),de
	inc hl
	inc hl
	inc hl
	ld e,(hl)				; Offset SETTINGS_AVOFF_MINX
	inc hl
	ld d,(hl)
	ld (min_x),de
	inc hl
	ld e,(hl)				; Offset SETTINGS_AVOFF_MINY
	inc hl
	ld d,(hl)
	ld (min_y),de
	inc hl
	ld e,(hl)				; Offset SETTINGS_AVOFF_MAXX
	inc hl
	ld d,(hl)
	ld (max_x),de
	inc hl
	ld e,(hl)				; Offset SETTINGS_AVOFF_MAXY
	inc hl
	ld d,(hl)
	ld (max_y),de
	xor a
	ld (ateq),a

	ld a,SETTINGS_AVOFF_AXISMODE
	call LTS_GetPtr
	ld a,(hl)
	ld (axismode),a
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (bgcolor),de
	inc hl
	ld a,(hl)
	ld (colormode),a
	
	; Load screen constants
	ld hl,30
	ld (pxlMinY),hl
	ld hl,(0.5*(5/4))*INT_TO_8P8
	ld (MapFactorX),hl
	ld hl,(240-30)/2
	ld (MapFactorY),hl
	
	; Load equation information
	call CheckEnabled_Setup

	ret

;-----------------------------------
Graph_Clear_Screen:	
	call DisplayNormal
	call Full_Window
	ld a,$20
	ld hl,(PxlMinY)		; set write Y coordinate
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

	ld hl,240
	ld de,(PxlMinY)
	or a
	sbc hl,de
	ld a,l
	ld de,320*2/4
	call multade
	ex de,hl
	ld  hl,(bgcolor)
	ld c,$11
blank_loop:
	out	(c),h
	out	(c),l
	out	(c),h
	out	(c),l
	dec	de
	ld	a,d
	or	e
	jr	nz,blank_loop
	call DisplayOrg
	ret

;-----------------------------------
Graph_Recompute:
Graph_Rezoom:
Graph_Recolor:
	call TrashRAM_SwapIn						; NB: CAN'T CALL OS ROUTINES UNTIL SWAPOUT!
	
	; Re-set up graph-related equations
	ld hl,FP_MAX
	ld (val_min_z),hl
	ld hl,FP_MIN
	ld (val_max_z),hl

	ld hl,(max_x)						;step_x = (max_x-min_x)/(float)(dim_x-1);
	ld de,(min_x)
	call subhlde_fp
	push hl
		ld a,(dim_x)
		ld c,a
		dec c
		call g3dc_divhlc
		ld (step_x),hl

		ld hl,(max_y)						;step_y = (max_y-min_y)/(float)(dim_y-1);
		ld de,(min_y)
		or a
		call subhlde_fp
		push hl
			ld a,(dim_y)
			ld c,a
			dec c
			call g3dc_divhlc
			ld (step_y),hl

			pop hl							;cam_radius = 2.0f*MAX(max_x-min_x, max_y-min_y);
		pop de
	call MaxHLDE
	ld e,l
	ld d,h
	call addhlde_fp
	ld (cam_radius),hl
	
	ld a,(dim_x)							;for tracing
	srl a
	ld (at_step_x),a
	ld a,(dim_y)
	srl a
	ld (at_step_y),a
	ld a,$ff
	ld (whichtrace),a

	ld hl,FP_PI
	ld (alpha),hl
	ld hl,0
	ld (beta),hl
	ld (gamma),hl

	; Figure out which equations are enabled
#ifdef DEBUG_GRAPH
	ld a,1
#else
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,a
	ld a,tZ1
	ld c,0
Graph_CountEQs_Loop:
	push af
		push bc
			call CheckEnabledA
			pop bc
		jr z,Graph_CountEQs_Loop_NotEnabled
		inc c
Graph_CountEQs_Loop_NotEnabled:
		pop af
	inc a
	djnz Graph_CountEQs_Loop
	ld a,c
#endif
	ld (counteqs),a
	ld e,a
	ld d,0
	ld a,(dim_x)
	call multade
	ld a,l
	ld (totalXiters),a
	xor a
	ld (completeXiters),a

	ld hl,(bgcolor)
	call negate_hl
	dec hl						;This makes negate_hl equivalent to cpl hl
	ld (fgcolor),hl
	ex de,hl					;Get fgcolor
	ld hl,(0 * 256) + 8
	ld (CurRow),hl
	ld bc,(bgcolor)
	ld hl,sTotalProgress
	push bc
		push de
			call PutsColored
			pop de
		pop bc
	ld hl,sPartialProgress
	call PutsColored

	; Initialize pointers into big stored data chunks
	ld hl,grid_x
	ld (pgrid_x),hl
	ld hl,grid_y
	ld (pgrid_y),hl
	ld hl,grid_z
	ld (pgrid_z),hl

	; Pre-compute deltaX and deltaY
	; - min X
	ld hl,(min_x)
	call FPtoOP1_scaled
	ld hl,OP1
	ld de,minX_OS
	call OPXtoOPX
	; - max X
	ld hl,(max_x)
	call FPtoOP1_scaled
	ld hl,minX_OS
	ld de,OP2
	call OPXtoOPX
	bcall(_FPSub)
	; - delta X
	bcall(_PushOP1)
	ld a,(dim_x)
	dec a
	ld h,a
	ld l,0
	call FPtoOP1		; Not scaled
	call OP1toOP2
	bcall(_PopOP1)
	bcall(_FPDiv)
	ld hl,OP1
	ld de,deltaX_OS
	call OPXtoOPX

	; - min Y
	ld hl,(min_y)
	call FPtoOP1_scaled
	ld hl,OP1
	ld de,minY_OS
	call OPXtoOPX
	; - max Y
	ld hl,(max_y)
	call FPtoOP1_scaled
	ld hl,minY_OS
	ld de,OP2
	call OPXtoOPX
	bcall(_FPSub)
	; - delta Y
	bcall(_PushOP1)
	ld a,(dim_Y)
	dec a
	ld h,a
	ld l,0
	call FPtoOP1		; Not scaled
	call OP1toOP2
	bcall(_PopOP1)
	bcall(_FPDiv)
	ld hl,OP1
	ld de,deltaY_OS
	call OPXtoOPX

	;Iterate over all equations
#ifdef DEBUG_GRAPH
	ld b,1
#else
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,a
#endif
	ld c,tZ1

Graph_Compute_EQ:
	push bc
		; Select the proper equation
		ld hl,BaseZVarName
		ld de,parseVar
		ld bc,9
		push de
			ldir
			pop hl
		pop bc
	push bc
		ld a,c
		ld (parseVar + 2),a
		rst 20h
		rst 10h
		jp c,Graph_Compute_EQ_Next
		bit 5,(hl)
		jp z,Graph_Compute_EQ_Next

		; Note! _PutC messes this up.
		call TrashRAM_SwapOut
		; Initialize progress bar
		ld de,(bgcolor)
		push de
			; Draw the subscripted number for this equation
			ld hl,(1*256) + 9
			ld (CurRow),hl
			ld a,(ParseVar + 2)
			add a,$81-tZ1
			bcall(_PutC)
			; Set foreground and background to black
			pop de
		push de
			ld c,e
			ld b,d
			call SetTextColors
			; Now do the erase
			ld a,0 + (25-PROGRESS_WIDTH)
			ld (CurCol),a
			ld b,PROGRESS_WIDTH
Graph_Compute_EQ_ClearProgressLoop:
			ld a,$E0
			push bc
				bcall(_PutC)
				pop bc
			djnz Graph_Compute_EQ_ClearProgressLoop
			pop bc
		ld de,(fgcolor)
		call SetTextColors								; To prep for displaying the actual progress
		xor a
		ld (thisXiters),a
		; Note! _PutC messes this up.
		
		; Must do this with normal OS RAM swapped in
		ld hl,minX_OS
		rst 20h
		bcall(_StoX)
		ld hl,minY_OS
		rst 20h
		bcall(_StoY)

		call TrashRAM_SwapIn
		
		; Begin the actual calculations
		ld a,(dim_x)
		ld c,a
Graph_Compute_EQ_Outer:
		push bc
			ld a,c
			dec a
			ld de,(step_x)
			call signed_multade
			ld de,(min_x)
			call addhlde_fp
			ld (val_x),hl
			ld de,(pgrid_x)
			ex de,hl
			ld (hl),e
			inc hl
			ld (hl),d
			pop bc

		ld a,(dim_y)
		ld b,a
Graph_Compute_EQ_Inner:
		push bc
			; Update the val_y and pgrid_y values
			ld a,b
			dec a
			ld de,(step_y)
			call signed_multade
			ld de,(min_y)
			call addhlde_fp
			ld (val_y),hl
			ld de,(pgrid_y)
			ex de,hl
			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
			ld (pgrid_y),hl
			
			; Update the stored x and pgrid_x values
			ld hl,(pgrid_x)
			ld de,(val_x)
			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
			ld (pgrid_x),hl

			; Compute output value
#ifdef DEBUG_GRAPH
			;ld hl,0
			ld hl,(val_x)
			ld de,(val_y)
			call addhlde_fp
			ex de,hl
			ld bc,$0080
			call signed_multbcde_fp
			ld b,d
			ld c,e
			call signed_multbcde_fp
			ex de,hl
#else
			; To fit into the constraints of our fixed-point numbers, we
			; need to pre-scale X and Y, feed to our equation and get Z,
			; then post-un-scale Z. Keeping our FP numbers within the bounds
			; of sanity is key.
			call TrashRAM_SwapOut

			; Generate Z value
			ld hl,ParseVar
			rst 20h
			AppOnErr(Graph_Compute_EQ_Error)
			bcall(_ParseInp)
			AppOffErr
			call OP1toOP4
			ld hl,(scalefactor)				; Used to keep minx/maxx/miny/maxy sane
			call FPtoOP1
			call OP1toOP2
			call OP4toOP1
			bcall(_FPDiv)
			call OP1toFP
Graph_Compute_EQ_Inner_SwapAndStore:
			push hl
				call TrashRAM_SwapIn						; NB: CAN'T CALL OS ROUTINES UNTIL SWAPOUT!
				pop hl
#endif

			; Store computed Z value and update pointer
			ld de,(pgrid_z)
			ex de,hl
			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
			ld (pgrid_z),hl
			; Deal with min and max. Check for truncation values.
			ld hl,FP_MAX
			or a
			sbc hl,de
			jr z,Graph_Compute_EQ_Inner_SkipMinMax
			ld hl,FP_MIN
			or a
			sbc hl,de
			jr z,Graph_Compute_EQ_Inner_SkipMinMax
			push de
				ld hl,(val_max_z)
				call MaxHLDE
				ld (val_max_z),hl
				pop de
			ld hl,(val_min_z)
			call MinHLDE
			ld (val_min_z),hl
Graph_Compute_EQ_Inner_SkipMinMax:
			; Update Y
			bcall(_RclY)
			ld hl,deltaY_OS
			ld de,OP2
			call OPXtoOPX
			bcall(_FPAdd)
			bcall(_StoY)

			pop bc
		dec b
		jp nz,Graph_Compute_EQ_Inner
		
		push bc
			; Note! _PutC messes this up.
			call TrashRAM_SwapOut
			
			; Update X
			bcall(_RclX)
			ld hl,deltaX_OS
			ld de,OP2
			call OPXtoOPX
			bcall(_FPAdd)
			bcall(_StoX)

			; Get Y ready
			ld hl,minY_OS
			call OPXtoOP1
			bcall(_StoY)

			; Display some progress for this equation
			ld hl,((25-PROGRESS_WIDTH) * 256) + 9
			ld (CurRow),hl
			ld hl,thisXiters
			inc (hl)
			ld e,(hl)
			ld d,0
			ld a,PROGRESS_WIDTH
			call multade
			ld a,(dim_x)
			ld c,a
			call g3dc_divhlc
			ld b,l
			inc b
Graph_Compute_EQ_SetProgressLoop:
			dec b
			jr z,Graph_Compute_EQ_SetProgressLoop_Done
			ld a,$E0
			push bc
				bcall(_PutC)
				pop bc
			jr Graph_Compute_EQ_SetProgressLoop
Graph_Compute_EQ_SetProgressLoop_Done:
			
			; Display some progress for overall equations
			ld hl,((25-PROGRESS_WIDTH) * 256) + 8
			ld (CurRow),hl
			ld hl,completeXiters
			inc (hl)
			ld e,(hl)
			ld d,0
			ld a,PROGRESS_WIDTH
			call multade
			ld a,(totalXiters)
			ld c,a
			call g3dc_divhlc
			ld b,l
			inc b
Graph_Compute_EQ_SetProgressLoop2:
			dec b
			jr z,Graph_Compute_EQ_SetProgressLoop2_Done
			ld a,$E0
			push bc
				bcall(_PutC)
				pop bc
			jr Graph_Compute_EQ_SetProgressLoop2
Graph_Compute_EQ_SetProgressLoop2_Done:
			; Note! _PutC messes this up.
			call TrashRAM_SwapIn						; NB: CAN'T CALL OS ROUTINES UNTIL SWAPOUT!

			pop bc

		dec c
		jp nz,Graph_Compute_EQ_Outer
Graph_Compute_EQ_Next:
		pop bc
	inc c				; next equation name
	dec b
	jp nz,Graph_Compute_EQ

	; Note! _PutC messes this up.
	call TrashRAM_SwapOut
	; Clear the screen of those silly progress bars
	ld de,(bgcolor)
	ld b,d
	ld c,e
	call SetTextColors
	ld hl,(0 * 256) + 8
	ld (CurRow),hl
	ld b,26*2
Graph_Compute_EQ_ClearProgress_Loop:
	ld a,$E0
	push bc
		bcall(_PutC)
		pop bc
	djnz Graph_Compute_EQ_ClearProgress_Loop
	call ResetColors
	call DisableTextColors
	call TrashRAM_SwapIn
	
	; Set up the axes and bounding box arrays here
	ld hl,(max_x)
	push hl
		ld de,(min_x)
		call subhlde_fp
		ex de,hl
		ld bc,$0010
		call signed_multbcde_fp
		ld (sub_min_x),de
		push de
			ex de,hl
			call negate_hl
			ld (negsub_min_x),hl
			pop de
		pop hl
	call subhlde_fp
	ld (sub_max_x),hl

	ld hl,(max_y)
	push hl
		ld de,(min_y)
		call subhlde_fp
		push hl
			ex de,hl
			ld bc,$0010
			call signed_multbcde_fp
			ld (sub_min_y),de
			push de
				ex de,hl
				call negate_hl
				ld (negsub_min_y),hl
				pop de
			pop bc
		pop hl
	call subhlde_fp
	ld (sub_max_y),hl

	ld hl,(val_max_z)
	push hl
		ld de,(val_min_z)
		call subhlde_fp
		ld e,c
		ld d,b
		call MaxHLDE
		ex de,hl
		ld bc,$0010
		call signed_multbcde_fp
		pop hl
	call subhlde_fp
	ld (sub_max_z),hl

	ld hl,$0000
	ld (val_zero),hl

	ld b,AXES_BOUND_COORDS
	ld hl,AxesBoundsX
	ld de,axes_x
	call CopyContainCoords

	ld b,AXES_BOUND_COORDS
	ld hl,AxesBoundsY
	ld de,axes_y
	call CopyContainCoords

	ld b,AXES_BOUND_COORDS
	ld hl,AxesBoundZ
	ld de,axes_z
	call CopyContainCoords	
	
	; Compute the colors for the graphs here
	ld hl,grid_z
	ld (pgrid_z),hl
	ld hl,grid_colors
	ld (pgrid_colors),hl

	; Iterate over all equations
#ifdef DEBUG_GRAPH
	ld b,1
#else
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,a
#endif
	ld c,tZ1

Graph_Color_EQ:
	push bc
		ld a,c
		call CheckEnabledA
		jp z,Graph_Color_EQ_Next

		ld a,(dim_x)
		ld c,a
Graph_Color_EQ_Outer:
		ld a,(dim_y)
		ld b,a
Graph_Color_EQ_Inner:
		push bc
			;float frac = (val_max_z==val_min_z)?0.5:((val_z-val_min_z)/(val_max_z-val_min_z));
			ld hl,(pgrid_z)
			ld e,(hl)
			inc hl
			ld d,(hl)
			inc hl
			ld (pgrid_z),hl
			; de now contains val_z
			push de
				ld bc,$0080					;0.5
				ld hl,(val_max_z)
				ld de,(val_min_z)
				call subhlde_fp
				pop de
			ld a,h
			or l
			jr z,Graph_Color_EQ_Inner_StoreFracBC
			push hl
				ld hl,(val_min_z)
				ex de,hl
				call subhlde_fp
				pop de
			; divide hl by de here
			ld c,0
			ld a,h
			ld b,l
			inc de							; Fudge the maximum to $FF instead of $100
			call signed_divabcde			; returns result in abc (but a.bc/d.e = ab.c)
Graph_Color_EQ_Inner_StoreFracBC:
			ld (frac),bc			
			ld a,(colormode)
			or a
			jp nz,Graph_Color_EQ_Inner_ColorMode1
Graph_Color_EQ_Inner_ColorMode0:
			;Spectrum render, red component
			ld hl,$0040						; 0.25
			push hl
				ld de,(frac)
				call subhlde_fp
				call abshl_fp
				pop de
			ex de,hl
			call subhlde_fp					;r = 0.25 - abs(0.25 - frac)
			ld bc,0
			bit 7,h
			jr nz,{@}
			ld bc,$0600
@:
			ex de,hl
			call signed_multbcde_fp
			ld a,d
			ld b,a
			ld c,e
			or a
			jr z,{@}
			ld bc,$00ff
@:
			ld hl,(frac)
			ld de,$0040						; 0.25
			call cphlde_fp
			jr nc,Graph_Color_EQ_Inner_ColorMode0_StoreR
			ld hl,$0100
			add hl,bc
			srl h
			rr l
			ld b,h
			ld c,l
Graph_Color_EQ_Inner_ColorMode0_StoreR:
			rrc b
			jr nc,{@}
			ld c,$ff
@:
			ld a,c
			ld (clr_r),a
			;Spectrum render, green component
			ld hl,$0080						; 0.5
			ld de,(frac)
			call subhlde_fp
			call abshl_fp
			ld de,$0055						; 1/3
			ex de,hl
			call subhlde_fp					;g = 1/3 - abs(0.5 - frac)
			ld bc,0
			bit 7,h
			jr nz,{@}
			ld bc,$0600
@:
			ex de,hl
			call signed_multbcde_fp
			ld a,d
			ld b,a
			ld c,e
			or a
			jr z,{@}
			ld bc,$00ff
@:
			rrc b
			jr nc,{@}
			ld c,$ff
@:
			ld a,c
			ld (clr_g),a
			;Spectrum render, blue component
			ld hl,$00C0						; 0.75
			ld de,(frac)
			call subhlde_fp
			call abshl_fp
			ld de,$0040
			ex de,hl
			call subhlde_fp					;b = 0.25 - abs(0.75 - frac)
			ld bc,0
			bit 7,h
			jr nz,{@}
			ld bc,$0600
@:
			ex de,hl
			call signed_multbcde_fp
			ld a,d
			ld b,a
			ld c,e
			or a
			jr z,{@}
			ld bc,$00ff
@:
			ld de,(frac)
			ld hl,$00C0						; 0.75
			call cphlde_fp
			jr nc,Graph_Color_EQ_Inner_ColorMode0_StoreB
			ld hl,$0100
			add hl,bc
			srl h
			rr l
			ld b,h
			ld c,l
Graph_Color_EQ_Inner_ColorMode0_StoreB:
			rrc b
			jr nc,{@}
			ld c,$ff
@:
			ld a,c
			ld (clr_b),a
			jr Graph_Color_EQ_Inner_ColorFromRGB
Graph_Color_EQ_Inner_ColorMode1:
			dec a
			jr nz,Graph_Color_EQ_Inner_ColorMode2
			;a is already 0
			ld (clr_r),a
			ld de,(frac)
			ld a,2
			call multade
			rrc h
			jr nc,Graph_Color_EQ_Inner_ColorMode1_StoreG
			ld hl,$00ff
Graph_Color_EQ_Inner_ColorMode1_StoreG:
			ld a,l
			ld (clr_g),a
			ld hl,$00ff
			ld de,(frac)
			or a
			sbc hl,de
			ex de,hl
			ld a,2
			call multade
			rrc h
			jr nc,Graph_Color_EQ_Inner_ColorMode1_StoreB
			ld hl,$00ff
Graph_Color_EQ_Inner_ColorMode1_StoreB:
			ld a,l
			ld (clr_b),a
			jr Graph_Color_EQ_Inner_ColorFromRGB
Graph_Color_EQ_Inner_ColorMode2:
			; Default "flame" color mode, red to yellow
			ld a,$ff
			ld (clr_r),a
			ld a,$00
			ld (clr_b),a
			ld a,(frac)					;LSB
			ld (clr_g),a

Graph_Color_EQ_Inner_ColorFromRGB:
			ld a,(clr_r)				;Get fractional part of red
			and $f8
			ld b,a
			ld a,(clr_g)
			push af
				rlc a
				rlc a
				rlc a
				and $07					;Upper three bits of green with red
				or b
				ld b,a
				pop af
			sla a
			sla a
			sla a
			and $e0						;Lower three bits of green to go with blue
			ld c,a
			ld a,(clr_b)
			srl a
			srl a
			srl a
			or c
			ld c,a

Graph_Color_EQ_Inner_StoreColor:
			; Now store bc into the actual color
			ld hl,(pgrid_colors)
			ld (hl),c
			inc hl
			ld (hl),b
			inc hl
			ld (pgrid_colors),hl
			
			pop bc
		dec b
		jp nz,Graph_Color_EQ_Inner
		dec c
		jp nz,Graph_Color_EQ_Outer
Graph_Color_EQ_Next:
		pop bc
	inc c									; Next equation
	dec b
	jp nz,Graph_Color_EQ
	call TrashRAM_SwapOut
	ret

;-----------------------------------
Graph_Compute_EQ_Error:
	cp E_Break					; Was it a break error?
	jr z,Graph_Compute_EQ_Error_DoError
Graph_Compute_EQ_Error_NotBreak:
	ld hl,FP_MAGICCULL
	cp E_NonReal
	jp c,Graph_Compute_EQ_Inner_SwapAndStore
	jr nz,Graph_Compute_EQ_Error_DoError
	; Apologies to Kirk and Graph3 for this check that I don't quite get
	ld a,(OP1)
	and $1F
	cp CplxObj
	jp nz,Graph_Compute_EQ_Inner_SwapAndStore
Graph_Compute_EQ_Error_DoError:
	push af
		ld a,tUn
		ld (parseVar + 2),a
		call TrashRAM_SwapOut
		pop af
	bjump(_JError)
;-----------------------------------
Graph_Rerotate:
	call TrashRAM_SwapIn						; NB: CAN'T CALL DCSE ROUTINES UNTIL SWAPOUT!
	
	; Time to handle the headache of rotation
	ld hl,(alpha)
	ld a,h
	or l
	ld hl,(beta)
	or h
	or l
#ifndef GAMMA_ZERO
	ld hl,(gamma)
	or h
	or l
#endif
	jp z,SkipRotate
	; Alpha, beta, and/or gamma are non-zero, so do some rotation

	; Compute costx/y/z, sintx/y/z, cx, cy, and cz
	ld hl,(max_x)
	ld de,(min_x)
	call addhlde_fp
	ld c,0
	ld a,h
	ld b,l
	ld de,FP_2
	call signed_divabcde			; returns result in abc (but a.bc/d.e = ab.c)
	ld (cx),bc				;cx = (max_x + min_x)/2
	ld (ex),bc

	ld hl,(max_y)
	ld de,(min_y)
	call addhlde_fp
	ld c,0
	ld a,h
	ld b,l
	ld de,FP_2
	call signed_divabcde			; returns result in abc (but a.bc/d.e = ab.c)
	ld (cy),bc				;cy = (max_y + min_y)/2
	ld (ey),bc
	
	ld hl,0
	ld (cz),hl				;cz = 0.f
	
	ld hl,(alpha)
	push hl
		call mcosf
		ld (costx),hl
		pop hl
	call msinf
	ld (sintx),hl
	ld hl,(beta)
	push hl
		call mcosf
		ld (costy),hl
		pop hl
	call msinf
	ld (sinty),hl
#ifdef GAMMA_ZERO
	ld hl,1*INT_TO_8P8
	ld (costz),hl
	ld hl,0*INT_TO_8P8
	ld (sintz),hl
#else
	ld hl,(gamma)
	push hl
		call mcosf
		ld (costz),hl
		pop hl
	call msinf
	ld (sintz),hl
#endif
	
	ld hl,grid_x
	ld (pgrid_x),hl
	ld hl,grid_y
	ld (pgrid_y),hl
	ld hl,grid_z
	ld (pgrid_z),hl

	; Iterate over all equations
#ifdef DEBUG_GRAPH
	ld b,1
#else
	ld a,SETTINGS_AVOFF_MAXEQS
	call LTS_GetByte
	ld b,a
#endif
	ld c,tZ1

Graph_Rotate_EQ:
	push bc
		ld a,c
		call CheckEnabledA
		jp z,Graph_Rotate_EQ_Next
	
		ld a,(dim_x)
		ld c,a
Graph_Rotate_EQ_Outer:
		ld a,(dim_y)
		ld b,a		
		call Rotate_B_Points		;uses the vals based on pgrid_x/y/z
		dec c
		jp nz,Graph_Rotate_EQ_Outer
Graph_Rotate_EQ_Next:
		pop bc
	inc c							; Next equation
	dec b
	jp nz,Graph_Rotate_EQ
	
Graph_Rotate_AxesBounds:
	ld hl,axes_x
	ld (pgrid_x),hl
	ld hl,axes_y
	ld (pgrid_y),hl
	ld hl,axes_z
	ld (pgrid_z),hl
	ld b,AXES_BOUND_COORDS
	call Rotate_B_Points		;uses the vals based on pgrid_x/y/z

	ld hl,$0000
	ld (alpha),hl
	ld (beta),hl
	ld (gamma),hl
	
SkipRotate:
	call TrashRAM_SwapOut
	ret

;-----------------------------------
Graph_Rerender:
Graph_Redraw:
	call TrashRAM_SwapIn						; NB: CAN'T CALL TI-OS ROUTINES UNTIL SWAPOUT!

	; Calculate camera position
	ld hl,$0000
	ld (cx),hl
	ld (cy),hl
	ld hl,(cam_radius)
	call negate_hl
	ld (cz),hl
	
	; Center was set above, (ex, ey); FP_EZ is a constant
	; Ready to transform 3D coordinates into screen coordinates and display

	xor a
	call Graph_Render
	
	ld hl,axes_x
	ld (pgrid_x),hl
	ld hl,axes_y
	ld (pgrid_y),hl
	ld hl,axes_z
	ld (pgrid_z),hl
	ld hl,axes_sx
	ld (pgrid_sx),hl
	ld hl,axes_sy
	ld (pgrid_sy),hl
	ld b,AXES_BOUND_COORDS
	call Map_B_Points
	
	ld a,(axismode)
	rrca
	push af
		jr nc,Graph_Draw_Redraw_NoAxes
		ld hl,(bgcolor)
		call negate_hl
		dec hl						;This makes negate_hl equivalent to cpl hl
		ld (fgcolor),hl
		ld hl,Offsets_Axes
		ld b,9
		call Graph_Render_FromOffsets
Graph_Draw_Redraw_NoAxes:
		pop af
	rrca
	jr nc,Graph_Draw_Redraw_NoBounds
	ld hl,COLOR_GRAY
	ld (fgcolor),hl
	ld hl,Offsets_Bounds
	ld b,12
	call Graph_Render_FromOffsets
Graph_Draw_Redraw_NoBounds:

	call TrashRAM_SwapOut
	ret

;-----------------------------------
Graph_Render:
	ld (erase_mode),a

	; Initialize pointers into big stored data chunks
	ld hl,grid_x
	ld (pgrid_x),hl
	ld hl,grid_y
	ld (pgrid_y),hl
	ld hl,grid_z
	ld (pgrid_z),hl

	ld hl,grid_colors
	ld (pgrid_colors),hl
	
	di
	push iy
#ifdef DEBUG_GRAPH
		ld b,1
#else
		ld a,SETTINGS_AVOFF_MAXEQS
		call LTS_GetByte
		ld b,a
#endif
		ld c,tZ1

Graph_Render_EQ:
		push bc
			ld a,c
			call CheckEnabledA
			jp z,Graph_Render_EQ_Next

			ld hl,grid_sx
			ld (pgrid_sx),hl
			ld hl,grid_sy
			ld (pgrid_sy),hl
	
			ld a,(dim_x)
			ld c,a
Graph_Map_EQ_Outer:
			ld a,(dim_y)
			ld b,a
			call Map_B_Points
			dec c
			jr nz,Graph_Map_EQ_Outer
Graph_Map_EQ_Next:

			ld hl,grid_sy
			ld (pgrid_sy),hl
			ld hl,grid_sx
			ld (pgrid_sx),hl
			;ld hl,(pgrid_sx)
			push hl
				ld hl,(pgrid_sy)
				push hl
					ld hl,(pgrid_colors)
					push hl
						ld a,(dim_x)
						ld c,a
Graph_Render_EQ_XMajor_Outer:
						ld a,(dim_y)
						ld b,a
						dec b
Graph_Render_EQ_XMajor_Inner:
						push bc
							; Load first endpoint's color
							ld hl,(pgrid_colors)
							ld e,(hl)
							inc hl
							ld d,(hl)
							inc hl
							ld (pgrid_colors),hl
							; Load second endpoint's color
							ld a,(hl)
							inc hl
							ld h,(hl)
							ld l,a
							; Average the two colors
							call AverageRGB565
							; Color is ready!
							push hl
								pop iy
							ld hl,(pgrid_sx)
							ld e,(hl)
							inc hl
							ld d,(hl)
							inc hl
							ld (pgrid_sx),hl
							ld c,(hl)
							inc hl
							ld b,(hl)
							; Check if either point is culled
							ld hl,FP_MAGICCULL
							or a
							sbc hl,de
							add hl,de
							jr z,Graph_Render_EQ_XMajor_Inner_Cull
							or a
							sbc hl,bc
							jr z,Graph_Render_EQ_XMajor_Inner_Cull
							push bc
								ld hl,(pgrid_sy)
								ld c,(hl)
								inc hl
								ld b,(hl)
								inc hl
								ld (pgrid_sy),hl
								ld a,(hl)
								inc hl
								ld h,(hl)
								ld l,a
								push hl
									pop ix
								pop hl
							;bc, de, hl, ix, iy all set up
							ld a,(erase_mode)
							or a
							jr z,Graph_Render_EQ_XMajor_Inner_Go
							ld iy,(bgcolor)
Graph_Render_EQ_XMajor_Inner_Go:
							call ColorLine
							jr Graph_Render_EQ_XMajor_Inner_Next
Graph_Render_EQ_XMajor_Inner_Cull:
							ld hl,(pgrid_sy)
							inc hl
							inc hl
							ld (pgrid_sy),hl
Graph_Render_EQ_XMajor_Inner_Next:
							pop bc
						dec b
						jp nz,Graph_Render_EQ_XMajor_Inner
						ld hl,(pgrid_sx)
						inc hl
						inc hl
						ld (pgrid_sx),hl
						ld hl,(pgrid_sy)
						inc hl
						inc hl
						ld (pgrid_sy),hl
						ld hl,(pgrid_colors)
						inc hl
						inc hl
						ld (pgrid_colors),hl
						dec c
						jp nz,Graph_Render_EQ_XMajor_Outer
						pop hl
					ld (pgrid_colors),hl
					pop hl
				ld (pgrid_sy),hl
				pop hl
			ld (pgrid_sx),hl
			
			ld a,(dim_x)
			ld c,a
			dec c
Graph_Render_EQ_YMajor_Outer:
			ld a,(dim_y)
			ld b,a
Graph_Render_EQ_YMajor_Inner:
			push bc
				; Render the other lines
				ld hl,(pgrid_colors)
				ld e,(hl)
				inc hl
				ld d,(hl)
				inc hl
				ld (pgrid_colors),hl
				; Load second endpoint's color
				ld a,(dim_x)
				ld c,a
				ld b,0
				push bc
					add hl,bc
					add hl,bc
					dec hl
					ld a,(hl)
					dec hl
					ld l,(hl)
					ld h,a
					; Average the two colors
					call AverageRGB565
					; Color is ready!
					push hl
						pop iy
					ld hl,(pgrid_sx)
					ld e,(hl)
					inc hl
					ld d,(hl)
					inc hl
					ld (pgrid_sx),hl
				pop bc
				add hl,bc
				add hl,bc
				dec hl
				ld b,(hl)
				dec hl
				ld c,(hl)
				; Check if either point is culled
				ld hl,FP_MAGICCULL
				or a
				sbc hl,de
				add hl,de
				jr z,Graph_Render_EQ_YMajor_Inner_Cull
				or a
				sbc hl,bc
				jr z,Graph_Render_EQ_YMajor_Inner_Cull
				; Now render
				push bc
					ld hl,(pgrid_sy)
					ld c,(hl)
					inc hl
					ld b,(hl)
					inc hl
					ld (pgrid_sy),hl
					push bc
						push af
							ld a,(dim_x)
							dec a
							ld c,a
							ld b,0
							add hl,bc
							add hl,bc
							pop af
						pop bc
					ld a,(hl)
					inc hl
					ld h,(hl)
					ld l,a
					push hl
						pop ix
					pop hl
				;bc, de, hl, ix, iy all set up
				ld a,(erase_mode)
				or a
				jr z,Graph_Render_EQ_YMajor_Inner_Go
				ld iy,(bgcolor)
Graph_Render_EQ_YMajor_Inner_Go:
				call ColorLine
				jr Graph_Render_EQ_YMajor_Inner_Next
Graph_Render_EQ_YMajor_Inner_Cull:
				ld hl,(pgrid_sy)
				inc hl
				inc hl
				ld (pgrid_sy),hl
Graph_Render_EQ_YMajor_Inner_Next:
				pop bc
			dec b
			jp nz,Graph_Render_EQ_YMajor_Inner
			dec c
			jp nz,Graph_Render_EQ_YMajor_Outer
			; Fix color pointer
			ld hl,(pgrid_colors)
			ld a,(dim_x)
			ld e,a
			ld d,0
			add hl,de
			add hl,de
			ld (pgrid_colors),hl
Graph_Render_EQ_Next:
			pop bc
		inc c							; Next equation
		dec b
		jp nz,Graph_Render_EQ
		pop iy
	ei
	ret

