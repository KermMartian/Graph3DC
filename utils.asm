;============================================================
; Copies b words from the memory locations pointed to
; by hl to the chunk of memory at de
CopyContainCoords:
	ld a,(hl)
	inc hl
	push hl
		ld h,(hl)
		ld l,a
		ld a,(hl)
		ld (de),a
		inc hl
		inc de
		ld a,(hl)
		ld (de),a
		pop hl
	inc de
	inc hl
	djnz CopyContainCoords
	ret
;============================================================
TrashRAM_SwapIn:
	di
	in a,($27)
	ld (high_bank_mask),a
	ld a,(16384 - trash_ram_fill)/64
	out ($27),a

	in a,(5)
	ld (high_bank_page),a
	ld a,TRASHABLE_RAM_PAGE | $80			;RAM page to trash with a buffer
	out (5),a
	ret

TrashRAM_SwapOut:
	ld a,(high_bank_page)		;Don't need to worry about port $D/E/F because
	out (5),a					;$C000-$FFFF can only hold RAM pages
	ld a,(high_bank_mask)
	out ($27),a
	ei
	ret

ColorLine_RetPopBCDEHL:
				pop bc
			pop de
		pop hl
	ret

ColorLine:
	; de = x0, bc = y0, hl = x1, ix = y1, iy = colour
	;
	ld (lineColour),iy
	ld a,1
	ld (xyinc+0),a
	ld (xyinc+1),a                          ; x/y inc
	ld (xstart),de                          ; de = x0, hl = x1
	
	; Check for fully-offscreen lines: X or Y are both < pxlMinY, or X or Y are both > 320/240.
	ld a,h
	and d
	and $80					;bit 7,a
	ret nz					; h and d are both negative
	; Since we know hl and de are positive, we can add them.
	push hl
		push de
			push bc
				ld bc,320
				or a
				sbc hl,bc
				jr c,ColorLine_NotOffscreenX
				ex de,hl
				sbc hl,bc
				jr nc,ColorLine_RetPopBCDEHL
ColorLine_NotOffscreenX:
				pop hl
				; Check if ys are both offscreen
			push hl
				push ix
					pop de
				ld bc,240
				or a
				sbc hl,bc
				add hl,bc
				jr c,ColorLine_NotOffBottomY
				ex de,hl
				sbc hl,bc
				add hl,bc
				jr nc,ColorLine_RetPopBCDEHL

ColorLine_NotOffBottomY:
				ld bc,(PxlMinY)
				or a
				sbc hl,bc
				ex de,hl
				or a
				sbc hl,bc
				ld a,h
				and d
				and $80					;bit 7,a
				pop bc
			pop de
		pop hl
	ret nz

	; Figure out the increments
	ld a,h
	xor $80
	ld h,a
	ld a,d
	xor $80
	ld d,a
	or a
	sbc hl,de
	ld e,l
	ld d,h                                  ; de = (x1 - x0)
	jr nc,$+13
	ld a,-1
	ld (xyinc+0),a                                  ; x inc
	xor a
	sub e
	ld e,a
	sbc a,a
	sub d
	ld d,a                                  ; de = -(x1 - x0)
	ld (ystart),bc                                  ; bc = y0, ix = y1
	push ix
		pop hl
	ld a,h
	xor $80
	ld h,a
	ld a,b
	xor $80
	ld b,a
	or a
	sbc hl,bc
	ld c,l
	ld b,h                                  ; bc = (y1 - y0)
	jr nc,{@}
	ld a,-1
	ld (xyinc+1),a                                  ; y inc
	xor a
	sub c
	ld c,a
	sbc a,a
	sub b
	ld b,a                                  ; bc = -(y1 - y0)
@:	ld l,e
	ld h,d
	push bc
		ld a,h
		xor $80
		ld h,a
		ld a,b
		xor $80
		ld b,a
		or a
		sbc hl,bc
		pop bc
	ld hl,0                                 ; error
	jp c,ColorLine_y
ColorLine_x:
	push de
		pop ix
	inc ix
ColorLine_Loop_x:
	push hl
		push de
			push bc
				ld hl,(xstart)
				ld de,(ystart)
				call ColorPixel
				ld a,(xyinc+0)
				ld c,a
				rlca
				sbc a,a
				ld b,a
				add hl,bc
				ld (xstart),hl
				pop bc
			pop de
		pop hl
	add hl,bc
	or a
	sbc hl,de
	add hl,de
	jr c,$+23
	or a
	sbc hl,de
	push bc
		ld a,(xyinc+1)
		ld c,a
		rlca
		sbc a,a
		ld b,a
		push hl
			ld hl,(ystart)
			add hl,bc
			ld (ystart),hl
			pop hl
		pop bc
	dec ix
	ld a,ixl
	or ixh
	jr nz,ColorLine_Loop_x
	ret
ColorLine_y:
	push de
		ld e,c
		ld d,b
		pop bc
	push de
		pop ix
	inc ix
ColorLine_Loop_y:
	push hl
		push de
			push bc
				ld hl,(xstart)
				ld de,(ystart)
				call ColorPixel
				ld a,(xyinc+1)
				ld c,a
				rlca
				sbc a,a
				ld b,a
				ex de,hl
				add hl,bc
				ld (ystart),hl
				pop bc
			pop de
		pop hl
	add hl,bc
	or a
	sbc hl,de
	add hl,de
	jr c,$+23
	or a
	sbc hl,de
	push bc
		ld a,(xyinc+0)
		ld c,a
		rlca
		sbc a,a
		ld b,a
		push hl
			ld hl,(xstart)
			add hl,bc
			ld (xstart),hl
			pop hl
		pop bc
	dec ix
	ld a,ixl
	or ixh
	jr nz,ColorLine_Loop_y
	ret

ColorPixel:
	; hl = x, de = y, (lineColour) = colour
	; needs re-writing
	;
	bit 7,h
	ret nz
	ex de,hl
	ld bc,(pxlMinY)
	or a
	sbc hl,bc
	add hl,bc
	ex de,hl
	ret c
	ld bc,320
	or a
	sbc hl,bc
	add hl,bc
	ret nc                                                                  ; return if offscreen
	ex de,hl
	ld bc,240
	or a
	sbc hl,bc
	add hl,bc
	ex de,hl
	ret nc                                                                  ; return if offscreen
	push de
		push hl
			ld a,$52
			call setLCDRegister_SaveC                                                     ; v-addr-start (x-start)
			ld a,$21
			call setLCDRegister_SaveC                                                     ; x pos
			ld a,$53
			call setLCDRegister_SaveC                                                     ; v-addr-end (x-end)
			ex de,hl
			ld a,$50
			call setLCDRegister_SaveC                                                     ; h-addr-start (y-start)
			ld a,$20
			call setLCDRegister_SaveC                                                     ; y pos
			inc hl
			ld a,$51
			call setLCDRegister_SaveC                                                     ; h-addr-end (y-end)
			ld a,$22
			out ($10),a \ out ($10),a                                               ; write data to GRAM
			ld a,(lineColour+1) \ out ($11),a
			ld a,(lineColour+0) \ out ($11),a
			pop hl
		pop de
	ret

setLCDRegister_SaveC:
	; a = register
	; hl = value
	out ($10),a \ out ($10),a
	ld a,h \ out ($11),a
	ld a,l \ out ($11),a
	ret

DisplayOrg:
	ld	a,3		; adjust origin drawing mode
	ld	hl,$1038
	call	Write_Display_Control
	jr	Full_Window
	
DisplayNormal:
	ld	a,3		; normal drawing mode
	ld	hl,$10B8
	call	Write_Display_Control
	
Full_Window:
	ld	a,$50		; Set minimum Y
	ld	hl,0
	call	Write_Display_Control
	
	inc	a		; Set maximum Y
	ld	l,239
	call	Write_Display_Control	
	
	ld	hl,0		; Set minimum X
	inc	a
	call	Write_Display_Control
	
	inc	a		; Set maximum X
	ld	hl,319	

; Write HL to display register A (trashes c)
Write_Display_Control:
	out	($10),a
	out	($10),a
	ld	c,$11
	out	(c),h
	out	(c),l
	ret
	
;############## Clear the screen (in black, of course!)

PutsColored:
	call SetTextColors
	set textEraseBelow,(iy+textFlags)
	set 4,(iy+$4a)							; Puts should listen to colored text
PutSApp:
	ld a,(hl)
	inc hl
	or a
	ret z
	bcall(_PutMap)
	ld a,(curCol)
	inc a
	ld (curCol),a
	cp 26
	jr nz,PutSApp
	xor a
	ld (curCol),a
	ld a,(curRow)
	cp 9
	jr z,PutSApp
	inc a
	ld (curRow),a
	jr PutSApp

DrawSprite_OffscreenCheck:				;Re-used by all DrawSprite routines
	push bc
		ld b,0
		ld c,(ix)
		push bc
			ld c,(ix+1)
			call RectWindow		;Lowers the stack level; ix is still intact		
		jp c,Abort_1
		ld b,(ix+1)
		cp b
		jp nz,Abort_1
		ld a,(ix)
		cp c
		jp nz,Abort_1
		ld a,d
		or e
		push ix
			pop de
		pop ix
	ret nz						;X clipped
	ld a,h
	or l
	ret nz						;Y clipped

	ld	a,$22
	out	($10),a
	out	($10),a
	xor a
	ret

DrawSprite_CheckAndLoad:
	ld bc,240
	call cphlbc			;Check Y coordinate
	ret nc
	ld bc,320
	ex de,hl
	call cphlbc			;Check X coordinate
	ret nc

	ex de,hl
	ld c,(ix)
	inc ix
	ld b,(ix)
	inc ix
	scf					;Set carry flag!
	ret

;OLD: ix -> palette, b = x, l = y, de -> sprite	
;NEW: de=x, hl=y, 
;NEW: ix=sprite (.dw palette \ .db width, height \ .db bitpacked_padded_rows)
DrawSprite_1Bit:
	call DrawSprite_CheckAndLoad
	ret nc

;IMPORTANT NOTE: DrawSprite_1Bit_Pal skips the X/Y validity check
DrawSprite_1Bit_Pal:
;bc=palette, de=x, hl=y, 
;ix=sprite (.db width, height \ .db bitpacked_padded_rows)

	call DrawSprite_OffscreenCheck
	ret c
	ret nz			;DrawSprite_OffscreenCheck returns z on success

	ld a,(de)
	ld c,a
;	dec c
;	srl c
;	srl c
;	srl c
;	inc c
	inc de
	ld a,(de)
	ld b,a
	inc de
Draw_Sprite_1Bit_PackedLoop:
	push bc
		ld b,c
		ld c,1
Draw_Sprite_1Bit_PackedLine:	
		dec c
		jr nz,Draw_Sprite_1Bit_PackedLine_Bit_NoNewByte
		ld a,(de)
		inc de
		ld c,8
Draw_Sprite_1Bit_PackedLine_Bit_NoNewByte:
		rlc a
		push ix
			pop hl
		bit 0,a
		jr z,Draw_Sprite_1Bit_PackedLine_BitLow
		inc hl
		inc hl
Draw_Sprite_1Bit_PackedLine_BitLow:
		push bc						;10
			ld c,$11				;7
			outi					;16
			outi					;16
			pop bc					;10
		djnz Draw_Sprite_1Bit_PackedLine ;13 if jump taken
		pop	bc							;10
	djnz Draw_Sprite_1Bit_PackedLoop	;13 if jump taken
	ret

;OLD: ix -> palette, b = x, l = y, de -> sprite	
;NEW: de=x, hl=y, 
;NEW: ix=sprite (.dw palette \ .db width, height \ .db bitpacked_padded_rows)
DrawSprite_2Bit:
	call DrawSprite_CheckAndLoad
	ret nc

;IMPORTANT NOTE: DrawSprite_2Bit_Pal skips the X/Y validity check
DrawSprite_2Bit_Pal:
;bc=palette, de=x, hl=y, 
;ix=sprite (.db width, height \ .db bitpacked_padded_rows)

	call DrawSprite_OffscreenCheck
	ret c
	ret nz			;DrawSprite_OffscreenCheck returns z on success

	ld a,(de)
	ld c,a
	dec c
	srl c
	srl c
;	srl c
	inc c
	inc de
	ld a,(de)
	ld b,a
	inc de
Draw_Sprite_2Bit_PackedLoop:
	push bc
		ld b,c
Draw_Sprite_2Bit_PackedLine:
		push bc						;11
			ld	a,(de)				;7
			rra						;4
			rra						;4
			rra						;4
			rra						;4
			rra						;4
			and	%110				;7
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
			ld	a,(de)				;7
			rra						;4
			rra						;4
			rra						;4
			and	%110				;7
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
			ld	a,(de)				;7
			rra						;4
			and	%110				;7
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
			ld	a,(de)				;7
			rla						;4
			and	%110				;7
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
			inc	de					;6
			pop bc					;10
		djnz Draw_Sprite_2Bit_PackedLine ;13 if jump taken
		pop	bc							;10
	djnz Draw_Sprite_2Bit_PackedLoop	;13 if jump taken
	ret
	
;OLD: ix -> palette, b = x, l = y, de -> sprite	
;NEW: de=x, hl=y, 
;NEW: ix=sprite (.dw palette \ .db width, height \ .db bitpacked_padded_rows)
DrawSprite_4Bit:
	call DrawSprite_CheckAndLoad
	ret nc

;IMPORTANT NOTE: DrawSprite_4Bit_Pal skips the X/Y validity check
DrawSprite_4Bit_Pal:
;bc=palette, de=x, hl=y, 
;ix=sprite (.db width, height \ .db bitpacked_padded_rows)

	call DrawSprite_OffscreenCheck
	ret c
	ret nz			;DrawSprite_OffscreenCheck returns z on success

	ld a,(de)
	ld c,a
	dec c
	srl c
;	srl c
;	srl c
	inc c
	inc de
	ld a,(de)
	ld b,a
	inc de
DrawSprite_4Bit_PackedLoop:
	push bc
		ld b,c
DrawSprite_4Bit_PackedLine:
		push bc						;11
			ld a,(de)				;7
			rra						;4
			rra						;4
			rra						;4
			and	%11110				;7
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
			ld a,(de)				;7
			rla						;4
			and	%11110				;7
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
			inc	de					;6
			pop bc					;10
		djnz DrawSprite_4Bit_PackedLine ;13 if jump taken
		pop	bc							;10
	djnz DrawSprite_4Bit_PackedLoop	;13 if jump taken
	ret

;OLD: ix -> palette, b = x, l = y, de -> sprite	
;NEW: de=x, hl=y, 
;NEW: ix=sprite (.dw palette \ .db width, height \ .db bitpacked_padded_rows)
DrawSprite_4Bit_Enlarge:
	call DrawSprite_CheckAndLoad
	ret nc

;IMPORTANT NOTE: DrawSprite_4Bit_Enlarge_Pal skips the X/Y validity check
DrawSprite_4Bit_Enlarge_Pal:
;bc=palette, de=x, hl=y, 
;ix=sprite (.db width, height \ .db bitpacked_padded_rows)

	call DrawSprite_OffscreenCheck
	ret c
	ret nz			;DrawSprite_OffscreenCheck returns z on success

	ld a,(de)
	ld c,a
	dec c
	srl c
	srl c
;	srl c
	inc c
	inc de
	ld a,(de)
	ld b,a
	inc de
DrawSprite_4Bit_Enlarge_PackedLoop:
	push bc
		push de
			call DrawSprite_4Bit_Enlarge_PackedLine_Sub
			pop de
		call DrawSprite_4Bit_Enlarge_PackedLine_Sub
		pop	bc						;10
	dec b
	djnz DrawSprite_4Bit_Enlarge_PackedLoop		;13 if jump taken
	ret

DrawSprite_4Bit_Enlarge_PackedLine_Sub:
	ld b,c
DrawSprite_4Bit_Enlarge_PackedLine:
	push bc						;11
		ld a,(de)				;7
		rra						;4
		rra						;4
		rra						;4
		and	%11110				;7
		push ix					;11
			pop hl				;10
		ld c,a					;4
		ld b,0					;7
		add hl,bc				;11
		ld c,$11
		outi					;16
		outd					;16		;First pixel copy
		outi					;16
		outi					;16		;Second pixel copy
		ld a,(de)				;7
		rla						;4
		and	%11110				;7
		push ix					;11
			pop hl				;10
		ld c,a					;4
		ld b,0					;7
		add hl,bc				;11
		ld c,$11
		outi					;16
		outd					;16		;First pixel copy
		outi					;16
		outi					;16		;Second pixel copy
		inc	de					;6
		pop bc					;10
	djnz DrawSprite_4Bit_Enlarge_PackedLine	;13 if jump taken
	ret

;OLD: ix -> palette, b = x, l = y, de -> sprite	
;NEW: de=x, hl=y, 
;NEW: ix=sprite (.dw palette \ .db width, height \ .db bitpacked_padded_rows)
DrawSprite_8Bit:
	call DrawSprite_CheckAndLoad
	ret nc

;IMPORTANT NOTE: DrawSprite_8Bit_Pal skips the X/Y validity check
DrawSprite_8Bit_Pal:
;bc=palette, de=x, hl=y, 
;ix=sprite (.db width, height \ .db bitpacked_padded_rows)

	ld a,b
	or c
	jr z,DrawSprite_8Bit_DefaultPal
	call DrawSprite_OffscreenCheck
	ret c
	ret nz			;DrawSprite_OffscreenCheck returns z on success

	ld a,(de)
	ld c,a
;	dec c
;	srl c
;	srl c
;	srl c
;	inc c
	inc de
	ld a,(de)
	ld b,a
	inc de
DrawSprite_8Bit_PackedLoop:
	push bc
		ld b,c
DrawSprite_8Bit_PackedLine:
		push bc						;11
			ld a,(de)				;7
			cp $57					;transparent
			jr z,DrawSprite_8Bit_PackedLine_Trans
			push ix					;11
				pop hl				;10
			ld c,a					;4
			ld b,0					;7
			add hl,bc				;11
			add hl,bc				;11
			ld c,$11
			outi					;16
			outi					;16
DrawSprite_8Bit_PackedLine_Continue:
			inc	de					;6
			pop bc					;10
		djnz DrawSprite_8Bit_PackedLine ;13 if jump taken
		pop	bc							;10
	djnz DrawSprite_8Bit_PackedLoop	;13 if jump taken
	ret

DrawSprite_8Bit_PackedLine_Trans:
			ld c,$11
			in h,(c)
			in l,(c)
			in h,(c)
			in l,(c)
			out (c),h
			out (c),l
			jr DrawSprite_8Bit_PackedLine_Continue

DrawSprite_8Bit_DefaultPal:
	call DrawSprite_OffscreenCheck
	ret c
	ret nz			;DrawSprite_OffscreenCheck returns z on success

	ld a,(de)
	ld c,a
;	dec c
;	srl c
;	srl c
;	srl c
;	inc c
	inc de
	ld a,(de)
	ld b,a
	inc de
DrawSprite_8Bit_DefaultPal_PackedLoop:
	push bc
		ld b,c
DrawSprite_8Bit_DefaultPal_PackedLine:
		push bc						;11
			ld a,(de)				;7
			cp $57
			jr z,DrawSprite_8Bit_DefaultPal_PackedLine_Trans
			ld c,$11
			out (c),a
			out (c),a
DrawSprite_8Bit_DefaultPal_PackedLine_Continue:
			inc	de					;6
			pop bc					;10
		djnz DrawSprite_8Bit_DefaultPal_PackedLine ;13 if jump taken
		pop	bc							;10
	djnz DrawSprite_8Bit_DefaultPal_PackedLoop	;13 if jump taken
	ret

DrawSprite_8Bit_DefaultPal_PackedLine_Trans:
			ld c,$11
			in h,(c)
			in l,(c)
			in h,(c)
			in l,(c)
			out (c),h
			out (c),l
			jr DrawSprite_8Bit_DefaultPal_PackedLine_Continue

RectWindow:
;bc=height, de=x, hl=y, (sp)=ret, (sp+2)=width
		ex (sp),hl
		pop af
	ex (sp),hl
	push bc
		push af
			ld a,$21*2
			ld bc,320
			call RectWindow_1
			pop de
		ex (sp),hl
		push bc
			ld a,$20*2
			ld bc,240
			call RectWindow_1
			ld a,l
			or h
			ld a,c
Abort_2:
			pop bc
Abort_1:
		pop de
	ret
;a=clipped height, bc=clipped width, de=x feedback, hl=y feedback

RectWindow_1:
;a=aport*2, bc=sdim, de=p1, hl=rdim
; DEF: aport=GRAM horizontal/vertical address set port ($20 or $21),
;      sdim=screen dimension (240 or 320), rdim=rectangle dimension (height
;      or width), p1=rectangle start (y1 or x1)
	dec hl
	add hl,de
	rra									;rotate carry into bit 7, set if rdim+p1-1 carried
	ex de,hl
	push hl
		or a
		sbc hl,bc
		add hl,bc
		jr c,RectWindow_1_P1Ok
		ld l,a
		and h							;The things we care about are the bit 7s here
		jp p,Abort_4					;Abort if bit 7 of a and h are nonzero
		ld a,l
		sbc hl,hl
RectWindow_1_P1Ok:
		and $7F
		call setLCDRegister_SaveC_SaveA
		add a,8
		add a,a
		call setLCDRegister_SaveC_SaveA
		ex de,hl
		dec c
		sbc hl,bc
		jr c,RectWindow_1_P2Ok
		sbc hl,hl
RectWindow_1_P2Ok:
		add hl,bc
		add a,1
		call setLCDRegister_SaveC
		inc hl
		sbc hl,de
		ld b,h
		ld c,l
		pop hl
	sbc hl,de
	ret
;a=weport, bc=clipped rdim, de=clipped p1, hl=p1 feedback
Abort_4:
			scf
			pop bc
Abort_3:
		pop bc
	jp Abort_2

setLCDRegister_SaveC_SaveA:
	push af
		out ($10),a \ out ($10),a
		ld a,h \ out ($11),a
		ld a,l \ out ($11),a
		pop af
	ret

GetCurrentPage:
	push bc
		in a,($0E)
		rrca
		and $80
		ld b,a
		in a,($06)
		or b
		pop bc
	ret
	
VPutsColored:
	call SetTextColors
VPutSApp:				;display text in small font
	ld a,(hl)
	inc	hl
	inc	a
	dec	a			;use inc and dec to preserve carry
	ret	z
	push hl           
		push de
			bcall(_VPutMap)
			pop de
		pop hl
	jr VPutSApp
	
;--------------------------------------------------
ResetColors:
	ld de,COLOR_BLACK
	ld bc,COLOR_WHITE
SetTextColors:
	ld (drawBGColor),bc
	ld (textBGColor),bc
	ld (drawFGColor),de
	ld (textFGColor),de
	ret

;--------------------------------------------------
SetSpeedFast:
	push af
		set fastSpeed,(iy+speedFlags)
		ld a,1
		jr SetSpeed
SetSpeedSlow:
	push af
		res fastSpeed,(iy+speedFlags)
		xor a
SetSpeed:
		out (20h), a
		pop af
	ret
;--------------------------------------------------
InitZEquations:
	ld b,MAX_EQS
InitZEquations_Loop:
	push bc
		ld hl,BaseZVarName
		rst 20h
		pop af
	push af
		add a,tZ1-1			;-1 because b goes from 6 to 1, not 5 to 0
		ld (OP1+2),a
		rst 10h
		jr nc,InitZEquations_Loop_Exists
		ld hl,0
		bcall(_CreateEqu)
InitZEquations_Loop_Exists:
		pop bc
	djnz InitZEquations_Loop
	ret
;--------------------------------------------------
SetFunctionMode:
	; Set Function mode - can only touch a!
	ld a,(iy+grfModeFlags)
	and $ff^((1 << grfPolarM) | (1 << grfParamM) | (1 << grfRecurM))
	or 0+(1 << grfFuncM)
	ld (iy+grfModeFlags),a
	ret