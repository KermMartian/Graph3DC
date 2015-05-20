GraphHook:
	.db $83
	; Don't chain the graph hook here.  That would be silly.
	; It's only enabled if we're in 3D mode, anyway.
	or a
	jr nz,GraphHook_Not0
	call Graph_Setup
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
	ld b,kQuit
	cp a
	ret
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
Graph_Setup:
	; Basic setup stuff
	call SetSpeedFast
	call LTS_CacheAV

	; Load graph constants
	ld a,SETTINGS_AVOFF_XDIM
	call LTS_GetPtr
	ld a,(hl)				; Offset SETTINGS_AVOFF_XDIM
	ld (dim_x),a
	inc hl
	ld a,(hl)				; Offset SETTINGS_AVOFF_YDIM
	ld (dim_y),a
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
	xor a
	ld (counteqs),a

	ld hl,FP_PI
	ld (alpha),hl
	ld hl,0
	ld (beta),hl
	ld (gamma),hl

	; Figure out which equations are enabled
#ifdef DEBUG_GRAPH
	ld a,1
	ld (counteqs),a
#else
	.error "Don't know how to count equations yet!"
#endif

	.warn "Need to draw an hourglass or progress bar"

	;Iterate over all equations
#ifdef DEBUG_GRAPH
	ld b,1
#else
	ld b,MAX_EQS
#endif

	; Initialize pointers into big stored data chunks
	ld hl,grid_x
	ld (pgrid_x),hl
	ld hl,grid_y
	ld (pgrid_y),hl
	ld hl,grid_z
	ld (pgrid_z),hl

Graph_Compute_EQ:
	push bc
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
			.error "Need to actually compute something here!"
			; To fit into the constraints of our fixed-point numbers, we
			; need to pre-scale X and Y, feed to our equation and get Z,
			; then post-un-scale Z. Keeping our FP numbers within the bounds
			; of sanity is key.
#endif

			; Store computed Z value and update pointer
			ld de,(pgrid_z)
			ex de,hl
			ld (hl),e
			inc hl
			ld (hl),d
			inc hl
			ld (pgrid_z),hl
			push de
				ld hl,(val_max_z)
				call MaxHLDE
				ld (val_max_z),hl
				pop de
			ld hl,(val_min_z)
			call MinHLDE
			ld (val_min_z),hl

			pop bc
		dec b
		jp nz,Graph_Compute_EQ_Inner
		dec c
		jp nz,Graph_Compute_EQ_Outer
		pop bc
	dec b
	jp nz,Graph_Compute_EQ
	
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
	; Iterate over all equations
#ifdef DEBUG_GRAPH
	ld b,1
#else
	ld b,MAX_EQS
#endif

	ld hl,grid_z
	ld (pgrid_z),hl
	ld hl,grid_colors
	ld (pgrid_colors),hl

Graph_Color_EQ:
	push bc
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
		pop bc
	dec b
	jp nz,Graph_Color_EQ
	call TrashRAM_SwapOut
	ret

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
	ld b,MAX_EQS
#endif

Graph_Rotate_EQ:
	push bc
		ld a,(dim_x)
		ld c,a
Graph_Rotate_EQ_Outer:
		ld a,(dim_y)
		ld b,a		
		call Rotate_B_Points		;uses the vals based on pgrid_x/y/z
		dec c
		jp nz,Graph_Rotate_EQ_Outer
		pop bc
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

	; Initialize pointers into big stored data chunks
	ld hl,grid_x
	ld (pgrid_x),hl
	ld hl,grid_y
	ld (pgrid_y),hl
	ld hl,grid_z
	ld (pgrid_z),hl
	ld hl,grid_sx
	ld (pgrid_sx),hl
	ld hl,grid_sy
	ld (pgrid_sy),hl
	
#ifdef DEBUG_GRAPH
	ld b,1
#else
	ld b,MAX_EQS
#endif

Graph_Map_EQ:
	push bc
		ld a,(dim_x)
		ld c,a
Graph_Map_EQ_Outer:
		ld a,(dim_y)
		ld b,a
		call Map_B_Points
		dec c
		jp nz,Graph_Map_EQ_Outer
		pop bc
	dec b
	jp nz,Graph_Map_EQ

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
	
	call TrashRAM_SwapOut
Graph_Draw_Redraw:
	xor a
	call Graph_Render
	
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
	ret

;-----------------------------------
