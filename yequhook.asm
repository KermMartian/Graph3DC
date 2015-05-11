YEquHook:
	.db $83
	; Skip 0 and 1
	bit grfFuncM,(iy + grfModeFlags)		; Only offer 3D mode in Function mode for now
	ret z
	push af
		push bc
			call LTS_CacheAV
			ld a,SETTINGS_AVOFF_MODE
			call LTS_GetByte
			pop bc
		cp %00000011
		jr z,YEquHook_Full
YEquHook_Partial:
		pop af
	cp 6
	jr nz,YEquHook_SetZRet
	ld b,kYEqu
YEquHook_SetZRet:
	cp a
	ret

YEquHook_Full:
		pop af
	cp 2
	jr nz,yEquHook_Not2
YEquHook_ResZRet:
	cp $ff
	ret

yEquHook_Not2:
	dec a
	jr nz,yEquHook_Not3
	cp $ff
	ret

yEquHook_Not3:
	dec a
	jr nz,yEquHook_Not4
	cp $ff
	ret

yEquHook_Not4:
	or a
	ret
