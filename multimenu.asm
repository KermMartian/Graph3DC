;-------------------------------------------
; This file contains routines for a special
; kind of multi-option menu, very similar to
; the TI-OS Format menu.
;-------------------------------------------

;===========MultiMenu_Init=======================
; Inputs: (None)
; Outputs:
; - Initializes state
;------------------------------------------------
MultiMenu_Init:
	xor a
	ld (menuCurCol),a
	ld (menuCurRow),a
	ret
	
;===========MultiMenu_Redisplay==================
; Inputs:
; - hl = pointer to specially-formatted table
; - ix = pointer to settings area
; Outputs:
; - Table drawn to screen
;------------------------------------------------
MultiMenu_Redisplay:
	bcall(_HomeUp)
MultiMenu_Redisplay_NextRow:
	ld a,(hl)					; 0 items means this "row" marks the end
	or a
	jp z,MultiMenu_Redisplay_Finish
	ld b,a
	inc hl
MultiMenu_Redisplay_Row:
	push bc
		ld a,(hl)					; What kind of item is it?
		inc hl
		cp MT_TEXT
		jr nz,MultiMenu_Redisplay_Row_NotText
		; Display a text item
		call PutsApp
		jr MultiMenu_Redisplay_NextColumn
MultiMenu_Redisplay_Row_NotText:
		cp MT_OPTION
		jr nz,MultiMenu_Redisplay_Row_NotOption
		; Display a one-byte option item
		push ix
			pop de
		push hl
			ld l,(hl)
			ld h,0
			add hl,de
			ld a,(hl)
			pop hl
		inc hl
		cp (hl)									; Is this item selected?
		inc hl
		jr nz,MultiMenu_Redisplay_Row_Option_Display
		set textInverse,(iy+textFlags)
		jr MultiMenu_Redisplay_Row_Option_Display
MultiMenu_Redisplay_Row_NotOption:
		cp MT_OPTIONW
		jr nz,MultiMenu_Redisplay_Row_NotOptionW
		; Display a two-byte option item
		push ix
			pop de
		push hl
			ld l,(hl)
			ld h,0
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			pop hl
		inc hl
		push hl
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			or a
			sbc hl,de
			pop hl
		inc hl
		inc hl
		jr nz,MultiMenu_Redisplay_Row_Option_Display
		set textInverse,(iy+textFlags)
MultiMenu_Redisplay_Row_Option_Display:
		call PutsApp
MultiMenu_Redisplay_Row_NotOptionW:
MultiMenu_Redisplay_NextColumn:
		res textInverse,(iy+textFlags)
		ld a,' '
		bcall(_PutC)
		pop bc
	djnz MultiMenu_Redisplay_Row
	push hl
		ld hl,CurRow								; Cheap equivalent of NewLine without scrolling
		inc (hl)									; Next row
		inc hl										; to CurCol
		ld (hl),0									; Left column
		pop hl
	jp MultiMenu_Redisplay_NextRow
MultiMenu_Redisplay_Finish:
	bcall(_EraseEOW)
	ret

;===========MultiMenu_HandleKey==================
; Inputs:
; - hl = pointer to specially-formatted table
; - ix = pointer to settings area
; - a  = key pressed
; Outputs:
; - if z set, key was handled
; - if z reset, key was not handled
;------------------------------------------------
MultiMenu_HandleKey:
	cp kLeft
	jr nz,MultiMenu_HandleKey_NotLeft
	; Handle [left]
	call MultiMenu_FindFirstCol
	ld hl,menuCurCol
	cp (hl)
	ret z
	dec (hl)
	cp a
	ret
MultiMenu_HandleKey_NotLeft:
	cp kUp
	jr nz,MultiMenu_HandleKey_NotUp
	; Handle [up]
	ld a,(menuCurRow)
	or a
	ret z												; Key handled
	dec a
	ld (menuCurRow),a
	jr MultiMenu_HandleKey_ClearColAndRet
MultiMenu_HandleKey_NotUp:
	cp kRight
	jr nz,MultiMenu_HandleKey_NotRight
	; Handle [right]
	call MultiMenu_SkipToRow
	ld a,(menuCurCol)
	inc a
	cp (hl)
	ret z												; Key handled
	ld (menuCurCol),a
	cp a
	ret
MultiMenu_HandleKey_NotRight:
	cp kDown
	jr nz,MultiMenu_HandleKey_NotDown
	; Handle [down]
	push hl
		call MultiMenu_SkipToRow
		call MultiMenu_AdvanceRow
		ld a,(hl)
		pop hl
	or a
	ret z												; Key handled
	push hl
		ld hl,menuCurRow
		inc (hl)
		pop hl
MultiMenu_HandleKey_ClearColAndRet
	call MultiMenu_FindFirstCol
	ld (menuCurCol),a
	cp a
	ret
MultiMenu_HandleKey_NotDown:
	cp kEnter
	ret nz												; Key not handled
	; Handle [ENTER]
	push hl
		push ix
			push de
				call MultiMenu_SkipToRowCol
				ld a,(hl)
				inc hl
				ld e,(hl)
				ld d,0
				inc hl
				cp MT_OPTIONW
				jr z,MultiMenu_HandleKey_Enter_TwoByte
				cp MT_OPTION
				jr nz,MultiMenu_HandleKey_HandledEnter				; ??? should not happen! If it does, this code is outdated
				add ix,de
				ld a,(hl)
				ld (ix),a
				jr MultiMenu_HandleKey_HandledEnter
MultiMenu_HandleKey_Enter_TwoByte:
				add ix,de
				ld a,(hl)
				ld (ix),a
				inc hl
				inc ix
				ld a,(hl)
				ld (ix),a
MultiMenu_HandleKey_HandledEnter:
				pop hl
			call callhl
			pop ix
		pop hl
	call MultiMenu_Redisplay
	cp a
	ret
	
callhl:
	jp (hl)

;===========MultiMenu_DrawCursor=================
; Inputs:
; - hl = pointer to specially-formatted table
; - ix = pointer to settings area
; Outputs:
; - Cursor drawn or erased
;------------------------------------------------
MultiMenu_DrawCursor:
	call MultiMenu_SkipToRowCol
	ld de,COLOR_BLACK
	ld bc,COLOR_WHITE
	bit curOn,(iy+curFlags)							; If z, switching to on
	jr nz,MultiMenu_DrawCursor_CheckHighlighted
	ld b,d
	ld c,e
	jr MultiMenu_DrawCursor_SetColors
MultiMenu_DrawCursor_CheckHighlighted:
	call MultiMenu_CheckHighlighted					; z = highlighted, nz = not.
	jr nz,MultiMenu_DrawCursor_SetColors
	ld a,b \ ld b,d \ ld d,a						; Swap b<->d through a
	ld a,c \ ld c,e \ ld e,a						; Swap c<->e through a
MultiMenu_DrawCursor_SetColors:
	call MultiMenu_AdvanceToText
	call PutsColored
	call ResetColors
	ret
	
;===========MultiMenu_SkipToRowCol===============
; Inputs:
; - hl = pointer to specially-formatted table
; - ix = pointer to settings area
; Outputs:
; - hl = pointer to currently-selected item
; - Also sets curRow and curCol to start of item
;------------------------------------------------
MultiMenu_SkipToRowCol:
	call MultiMenu_SkipToRow
	inc hl
	call MultiMenu_SkipToCol
	ret

MultiMenu_SkipToRow:
	ld a,(menuCurRow)
	ld (CurRow),a
	ld b,a
	or a
	jr z,MultiMenu_DrawCursor_RowSearchDone
MultiMenu_DrawCursor_RowSearch:
	call MultiMenu_AdvanceRow						; Moves HL to next row in table
	djnz MultiMenu_DrawCursor_RowSearch
MultiMenu_DrawCursor_RowSearchDone:
	ret

MultiMenu_SkipToCol:
	ld a,(menuCurCol)
	ld b,a
	ld d,0
	or a
	jr z,MultiMenu_DrawCursor_ColSearchDone
MultiMenu_DrawCursor_ColSearch:
	call MultiMenu_AdvanceCol						; Moves HL to next col in table, returns a=width (minus space)
	inc a
	add a,d
	ld d,a
	djnz MultiMenu_DrawCursor_ColSearch
MultiMenu_DrawCursor_ColSearchDone:
	ld a,d
	ld (CurCol),a
	ret

;===========MultiMenu_CheckHighlighted===========
; Inputs:
; - hl = pointer to item in table
; - ix = pointer to settings area
; Outputs:
; - z = selected, nz = not selected
; Destroyed:
; - af
;------------------------------------------------
MultiMenu_CheckHighlighted:
	ld a,(hl)
	cp MT_OPTION
	jr z,MultiMenu_CheckHighlighted_OneByte
	cp MT_OPTIONW
	jr z,MultiMenu_CheckHighlighted_TwoBytes
	ret		; nz flag set - but we should never get here.
MultiMenu_CheckHighlighted_OneByte:
	push de
		push hl
			push ix
				pop de
			inc hl
			ld l,(hl)
			ld h,0
			add hl,de
			ld a,(hl)
			pop hl
		push hl
			inc hl
			inc hl
			cp (hl)
			pop hl
		pop de
	ret
MultiMenu_CheckHighlighted_TwoBytes:
	push de
		push hl
			push ix
				pop de
			inc hl
			ld l,(hl)
			ld h,0
			add hl,de
			ld e,(hl)
			inc hl
			ld d,(hl)
			pop hl
		push hl
			inc hl
			inc hl
			ld a,(hl)
			inc hl
			ld h,(hl)
			ld l,a
			or a
			sbc hl,de
			pop hl
		pop de
	ret

;===========MultiMenu_AdvanceRow=================
; Inputs:
; - hl = pointer to row in table
; Outputs:
; - hl = pointer to next row
; Destroyed:
; - af
;------------------------------------------------
MultiMenu_AdvanceRow:
	ld a,(hl)
	or a
	ret z
	push bc
		ld b,a
		inc hl
MultiMenu_AdvanceRow_Loop:
		call MultiMenu_AdvanceCol
		djnz MultiMenu_AdvanceRow_Loop
		pop bc
	ret

;===========MultiMenu_AdvanceCol=================
; Inputs:
; - hl = pointer to item in table
; Outputs:
; - hl = pointer to next col or next row
; - a  = width of text in col, in characters
; Destroyed:
; - af
;------------------------------------------------
MultiMenu_AdvanceCol:
	push bc
		call MultiMenu_AdvanceToText
		ld b,0
MultiMenu_AdvanceCol_Text_Loop:
		inc b
		ld a,(hl)
		inc hl
		or a
		jr nz,MultiMenu_AdvanceCol_Text_Loop
		dec b
		ld a,b
		pop bc
	ret

MultiMenu_AdvanceToText:
	push de
		ld a,(hl)
		ld de,4									;4: type, offset, value
		cp MT_OPTIONW
		jr z,MultiMenu_AdvanceToText_Text
		dec de									;3: type, offset, value
		cp MT_OPTION
		jr z,MultiMenu_AdvanceToText_Text
		dec de
		dec de									;1: type
MultiMenu_AdvanceToText_Text:
		add hl,de
		pop de
	ret
	
MultiMenu_FindFirstCol:
	call MultiMenu_SkipToRow
	inc hl
	ld b,0
MultiMenu_FindFirstCol_Loop:
	ld a,(hl)
	cp MT_OPTION
	jr z,MultiMenu_FindFirstCol_Finish
	cp MT_OPTIONW
	jr z,MultiMenu_FindFirstCol_Finish
	call MultiMenu_AdvanceCol
	inc b
	jr MultiMenu_FindFirstCol_Loop
MultiMenu_FindFirstCol_Finish:
	ld a,b
	ret