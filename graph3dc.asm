; Graph3DC for the TI-84+CSE
; By Christopher Mitchell ("Kerm Martian")
; http://www.cemetech.net

; TODO: 
; [X] Fix line routine
; [ ] Create variable for saving state
.echo "-----------------------\n"

.binarymode intel                 ; TI-83+ Application

#DEFINE NOAPPHEADER
#DEFINE NOEND
#DEFINE TI83P
#DEFINE NOTOKENS

.nolist
#include "ti84pcse.inc"

NUM_PAGES = 1
.defpage 0, 16*1024, $4000          ; Page 0 definition

; Assembly-time flags
#define DEBUG_EQ
#define DEBUG_GRAPH

myflag .equ asm_flag1
someflagbit equ 0
temp1	.equ cmdShadow
temp2	.equ $8585+3	; part of textShadow; leave space for ISR

.varloc temp1, 260

.var fp8.8, min_x
.var fp8.8, max_x
.var fp8.8, min_y
.var fp8.8, max_y
.var fp8.8, scalefactor
.var fp8.8, step_x
.var fp8.8, step_y
.var fp8.8, zoomfactor
.var fp8.8, val_min_z
.var fp8.8, val_max_z			;TODO: Consider making these OS floats
.var fp8.8, cam_radius
.var fp8.8, alpha
.var fp8.8, beta
.var fp8.8, gamma
.var fp8.8, val_x
.var fp8.8, val_y
.var fp8.8, val_z

.var fp8.8, frac				;Value 0.0 to 1.0 for color computation
.var fp8.8, clr_r
.var fp8.8, clr_g
.var fp8.8, clr_b

.var fp8.8, costx				;Used for rotation
.var fp8.8, costy
.var fp8.8, costz
.var fp8.8, sintx
.var fp8.8, sinty
.var fp8.8, sintz
.var fp8.8, cx
.var fp8.8, cy
.var fp8.8, cz
.var fp8.8, ex
.var fp8.8, ey

.var fp8.8, sub_min_x
.var fp8.8, negsub_min_x
.var fp8.8, sub_max_x
.var fp8.8, sub_min_y
.var fp8.8, negsub_min_y
.var fp8.8, sub_max_y
.var fp8.8, sub_max_z
.var fp8.8, val_zero

.var word, sx
.var word, sy

.var byte, colormode
.var word, bgcolor
.var word, fgcolor				;Used for bounds and axis rendering
.var byte, axismode
.var byte, dim_x
.var byte, dim_y
.var byte, curmenu
.var byte, menushown
.var byte, ateq
.var byte, whichtrace
.var byte, high_bank_page
.var byte, at_step_x
.var byte, at_step_y
.var byte, counteqs
.var byte, erase_mode
; The following used for lines and points
.var word, xstart
.var word, ystart
.var word, xyinc
.var word, lineColour
.var word, lts_av

; The following are pointers to *not the beginning* that are moved as rendering progresses
.var word, pgrid_x				;Pointer to variable data	3*MAX_EQS*MAX_XY_RES*MAX_XY_RES*2
.var word, pgrid_y				;Pointer to variable data
.var word, pgrid_z				;Pointer to variable data
.var word, pgrid_sx				;Pointer to variable data	MAX_XY_RES*MAX_XY_RES*2
.var word, pgrid_sy				;Pointer to variable data	MAX_XY_RES*MAX_XY_RES*2
.var word, pgrid_colors			;Pointer to variable data	MAX_EQS*MAX_XY_RES*MAX_XY_RES*2
.var word, paxes_x				;Pointer to variable data	20*2
.var word, paxes_y				;Pointer to variable data	20*2
.var word, paxes_z				;Pointer to variable data	20*2

; Constants
#define OUT_LEFT 1
#define OUT_TOP 2
#define OUT_RIGHT 4
#define OUT_BOTTOM 8
#define INT_TO_8P8	256
#define TRASHABLE_RAM_PAGE 6							;0-7
#define MAX_XY_RES 17
#define MAX_EQS 6
#define MAX_COLOR_MODES 3
#define MAX_AXIS_MODES 4
#define AXES_BOUND_COORDS 20
#define COLOR_GRAY $8410
#define WINDOW_ID_OFFSET $80

#define DEFAULT_XY_RES MAX_XY_RES
#define DEFAULT_XY_MIN $F800
#define DEFAULT_XY_MAX $0800

#define FP_MAX	$7FFF
#define	FP_MIN	$8000
#define FP_PI	$0324
#define FP_2	$0200
#define FP_EZ	$0100		;1.f/mtanf(fov/2.f) = 1.0 where fov=pi/2
#define FP_MAGICCULL $8000
#define DELTA_ANGLE			$0043			;pi/12
#define NEG_DELTA_ANGLE		$FFBD			;-pi/12
#define	COS_DELTA_ANGLE		$00F7			;cos(pi/12)
#define SIN_DELTA_ANGLE		$0042			;sin(pi/12)
#define SIN_NEG_DELTA_ANGLE	$FFBE			;sin(-pi/12)
#define COS_PI				$FF00			;cos(pi) = -1
#define SIN_PI				$0000			;sin(pi) = 0

#define SETTINGS_AV_SIZE		128
#define SETTINGS_AVOFF_MODE		0				;1 byte
#define SETTINGS_AVOFF_AXISMODE	1				;1 byte
#define SETTINGS_AVOFF_BGCOLOR	2				;2 bytes
#define SETTINGS_AVOFF_COLOR	4				;1 byte
#define SETTINGS_AVOFF_XDIM		5				;1 byte
#define SETTINGS_AVOFF_YDIM		6				;1 byte
#define SETTINGS_AVOFF_SCALEF	7				;2 bytes
#define SETTINGS_AVOFF_ZOOMF	9				;2 bytes
#define SETTINGS_AVOFF_MINX		11				;2 bytes
#define SETTINGS_AVOFF_MINY		13				;2 bytes
#define SETTINGS_AVOFF_MAXX		15				;2 bytes
#define SETTINGS_AVOFF_MAXY		17				;2 bytes
#define SETTINGS_HOOKBACK_WIN	19				;4 bytes
#define SETTINGS_HOOKBACK_YEQU	23				;4 bytes

; "Dynamic" allocation for graph-drawing data
trash_ram_loc	.equ	$C000
grid_x			.equ	trash_ram_loc + 0
grid_y			.equ	grid_x + (MAX_EQS*MAX_XY_RES*MAX_XY_RES*2)
grid_Z			.equ	grid_y + (MAX_EQS*MAX_XY_RES*MAX_XY_RES*2)
grid_sx			.equ	grid_z + (MAX_EQS*MAX_XY_RES*MAX_XY_RES*2)
grid_sy			.equ	grid_sx + (MAX_XY_RES*MAX_XY_RES*2)
axes_sx			.equ	grid_sy + (MAX_XY_RES*MAX_XY_RES*2)
axes_sy			.equ	axes_sx + (AXES_BOUND_COORDS*2)
grid_colors		.equ	axes_sy + (AXES_BOUND_COORDS*2)
axes_x			.equ	grid_colors + (MAX_EQS*MAX_XY_RES*MAX_XY_RES*2)
axes_y			.equ	axes_x + (2*20)
axes_z			.equ	axes_y + (2*20)
;dialogCBRAM		.equ	axes_z + (2*20)
trash_ram_end	.equ	axes_z + (2*20)		;dialogCBRAM + 16		; 16 bytes for (windowMenuCallbackEnd - windowMenuCallback)
trash_ram_fill	.equ	(trash_ram_end - trash_ram_loc)
.echo "Trash RAM page has ", trash_ram_fill, "/16384 bytes allocated\n"
.if trash_ram_fill > $4000
.error "Trash RAM page has overflowed!
.endif

; As suggested by MicrOS. This restricts how much
; stack we can use.
;IvtLocation	.equ	080h ; vector table at 8000h
;IsrLocation	.equ	085h ; ISR at 8585h

; OS Equates
APIFlg			equ 28h
appAllowContext		equ 0           ;App wants context changes to happen
hookflags3		equ 35h ;also sysHookFlg1
_SetAppChangeHook = 5011h
_ClrAppChangeHook =	5014h
appChangeHookPtr .equ	09E91h
hookflags4		.equ	$36f
yEqualsHookPtr	.equ	09E79h
_SetYEquHook	.equ	4FB4h
_ClrYEquHook	.equ	4FB7h
yEquHookActive	.equ 	4		;1 = Y= hook active

.list

;-----------------------------------
;|       App code starts here.
;-----------------------------------
.page 0                             ; Start page 0
Page0Start:
.echoln "-Page 0----------------------"
	; Master Field
	.db	80h, 0Fh, 0, 0, 0, 0
	; Signing Key ID
	.db	80h, 12h, 1, 15 ; 15 for the TI-84+CSE
	;revision
	.db 80h,21h,8   ; 8 
	.db 80h,31h,1   ; Pre-release
	; Name
	.db	80h, 48h, "Graph3DC"
	; Disable TI splash screen.
	.db	80h, 90h
	; Pages
	.db	80h, 81h, NUM_PAGES
	; Date stamp.  Apparently, the calculator doesn't mind if you put
	; nothing in this.
	.db	03h, 22h, 09h, 00h
	; Date stamp signature.  Since nothing ever checks this, there's no
	; reason ever to update it.  Or even have data in it.
	.db	02h, 00
	; Doors CSE 8 Icon
	.db 03h, 0F2h
	.dw AppIcon
	; Final field
	.db	80h, 70h

ASMStart:
ProgramStart:
	; Initialize all the hooks

	; Save the appChangeHook
	.warn "Need to save and restore AppChangeHook"

	; Set up the appChangeHook
	call GetCurrentPage
	ld hl,appChangeHook						;the ACTUAL appChange hook.
	bcall(_SetAppChangeHook)
	bjump(_JForceCmdNoChar)

; b is current app, a is new app
appChangeHook:
	.db $83

	push af
		push bc
			push hl
			
				; Get the current mode so we know if we can start triggering our stuff
				ld c,a										; b is current app, c is new app
				push bc
					call LTS_CacheAV
					call CleanTempHooks					; Clean up Yequ, Zoom, Window hooks, if they're ours

					ld a,SETTINGS_AVOFF_MODE
					call LTS_GetByte
					ld d,a
					pop bc
				; b is current app, c is new app, a and d are mode

				ld a,c
				cp kYequ
				jr nz,appChangeHook_CheckMode
				push de
					ld a,SETTINGS_AVOFF_MODE
					call LTS_GetPtr
					pop af
				scf
				rl a
				and %00000011
				ld (hl),a

				;cp %00000011
				;jr nz,appChangeHook_Done
				
				; Back up the current Yequ hook
				ld a,SETTINGS_HOOKBACK_YEQU
				call LTS_GetPtr						;to hl
				ld de,yEqualsHookPtr
				ex de,hl
				ld bc,3
				ldir
				ld a,(flags + hookflags3)			; contains yEquHookActive
				and $ff^(1 << yEquHookActive)
				ld (de),a
				
				; Set up new Yequ hook
				call GetCurrentPage
				ld hl,yEquHook
				bcall(_SetYEquHook)
appChangeHook_Done:
				pop hl
			pop bc
		pop af
	ret

appChangeHook_CheckMode:
				ld a,d
				cp %00000011
				jr z,appChangeHook_CheckWindow				; If it's zero, we haven't triggered 3D mode yet
appChangeHook_Invalid:
				ld a,SETTINGS_AVOFF_MODE
				call LTS_GetPtr
				ld (hl),0
				jr appChangeHook_Done

appChangeHook_CheckWindow:
				ld a,c
				cp kWindow
				jr nz,appChangeHook_CheckZoom
				
				; Back up the current Window hook
				ld a,SETTINGS_HOOKBACK_WIN
				call LTS_GetPtr						;to hl
				ld de,windowHookPtr
				ex de,hl
				ld bc,3
				ldir
				ld a,(flags + hookflags3)			; contains windowHookActive
				and $ff^(1 << windowHookActive)
				ld (de),a
				
				; Set up new Window hook
				call GetCurrentPage
				ld hl,windowHook
				bcall(_SetWindowHook)
				jr appChangeHook_Done

appChangeHook_CheckZoom:
				ld a,c
				cp kWindow
				jr nz,appChangeHook_Invalid

CleanTempHooks:
	call GetCurrentPage
	ld b,a
CleanTempHooks_Window:
	ld a,(windowHookPtr+2)
	cp b
	jr nz,CleanTempHooks_YEqu
	push bc
		ld a,SETTINGS_HOOKBACK_WIN
		call LTS_GetPtr						;to hl
		ld de,windowHookPtr
		ld bc,3
		ldir
		ld a,(flags + hookflags3)
		and $ff^(1 << windowHookActive)
		or (hl)
		ld (flags + hookflags3),a
		pop bc
CleanTempHooks_YEqu:
	ld a,(yEqualsHookPtr+2)
	cp b
	jr nz,CleanTempHooks_Window_Zoom
	push bc
		ld a,SETTINGS_HOOKBACK_YEQU
		call LTS_GetPtr						;to hl
		ld de,yEqualsHookPtr
		ld bc,3
		ldir
		ld a,(flags + hookflags3)
		and $ff^(1 << yEquHookActive)
		or (hl)
		ld (flags + hookflags3),a
		pop bc
CleanTempHooks_Window_Zoom:
	ret

;-----------------------------------
Graph:
	; Variable allocation and initialization goes here
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
	ld (menushown),a

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

Graph_Rerotate:
	call TrashRAM_SwapIn						; NB: CAN'T CALL DCSE ROUTINES UNTIL SWAPOUT!
	
	; Time to handle the headache of rotation
	ld hl,(alpha)
	ld a,h
	or l
	ld hl,(beta)
	or h
	or l
	ld hl,(gamma)
	or h
	or l
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
	ld hl,(gamma)
	push hl
		call mcosf
		ld (costz),hl
		pop hl
	call msinf
	ld (sintz),hl
	
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
	
Graph_Rerender:
Graph_Redraw:
	call TrashRAM_SwapIn						; NB: CAN'T CALL DCSE ROUTINES UNTIL SWAPOUT!

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
Menu_4_Redraw:
	xor a
	call Graph_Render
	
	ld a,(axismode)
	rrca
	push af
		jr nc,Menu_4_Redraw_NoAxes
		ld hl,(bgcolor)
		call negate_hl
		dec hl						;This makes negate_hl equivalent to cpl hl
		ld (fgcolor),hl
		ld hl,Offsets_Axes
		ld b,9
		call Graph_Render_FromOffsets
Menu_4_Redraw_NoAxes:
		pop af
	rrca
	jr nc,Menu_4_Redraw_NoBounds
	ld hl,COLOR_GRAY
	ld (fgcolor),hl
	ld hl,Offsets_Bounds
	ld b,12
	call Graph_Render_FromOffsets
Menu_4_Redraw_NoBounds:
	
	; Draw the onscreen sprites and things
	ld a,(menushown)
	or a
	jr nz,Menu_4_Redraw_FullMenu
	ld ix,spriteup_black
	ld a,(bgcolor)
	or a
	jr z,Menu_4_Redraw_ShownSpriteUp
	ld ix,spriteup_white
Menu_4_Redraw_ShownSpriteUp:
	ld de,20
	ld hl,214
	call DrawSprite_1Bit
	ld de,86
	ld hl,214
	ld ix,switchaxes
	call DrawSprite_2Bit
	ld de,152
	ld hl,214
	ld ix,changecolor
	call DrawSprite_8Bit
	ld de,284
	ld hl,214
	ld ix,switchback
	call DrawSprite_1Bit
	jr Menu_4_Keys
Menu_4_Redraw_FullMenu:
	; TODO
Menu_4_Keys:
	bcall(_getcsc)
	or a
	jr z,Menu_4_Keys
	cp skClear
	ret z
	push af
		call Graph_Erase
		pop af
	ld bc,DELTA_ANGLE
	cp skUp
	jr z,Menu_4_Keys_StoreAlpha
	cp skLeft
	jr z,Menu_4_Keys_StoreBeta
	ld bc,NEG_DELTA_ANGLE
	cp skDown
	jr z,Menu_4_Keys_StoreAlpha
	cp skRight
	jr z,Menu_4_Keys_StoreBeta
	jp Menu_4_Redraw
Menu_4_Keys_StoreAlpha:
	ld (alpha),bc
	jp Graph_Rerotate
Menu_4_Keys_StoreBeta:
	ld (beta),bc
	jp Graph_Rerotate
	
Restart_Menu_Quit:
Quit:
	ret

Menu4_GraphModify_ShowAxes:
	call Graph_Erase
	ld a,(axismode)
	inc a
	cp MAX_AXIS_MODES
	jr nz,Menu4_GraphModify_ShowAxes_Save
	xor a
Menu4_GraphModify_ShowAxes_Save:
	ld (axismode),a
	jp Menu_4_Redraw

Menu4_GraphModify_GraphColor:
	call Graph_Erase
	ld a,(colormode)
	inc a
	cp MAX_COLOR_MODES
	jr nz,Menu4_GraphModify_GraphColor_Save
	xor a
Menu4_GraphModify_GraphColor_Save:
	ld (colormode),a
	jp Graph_Recolor

Menu4_GraphModify_ToggleAll:
	;TODO
	jp Menu_4_Redraw
Menu4_GraphModify_BGColor:
	ld hl,(bgcolor)
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	ld (bgcolor),hl
	call Clear_Screen
	jp Menu_4_Redraw
;============================================================
Graph_Erase:
	ld hl,(bgcolor)
	ld (fgcolor),hl
	ld a,(axismode)
	rrca
	push af
		jr nc,{@}
		ld hl,Offsets_Axes
		ld b,9
		call Graph_Render_FromOffsets
@:
		pop af
	rrca
	jr nc,{@}
	ld hl,Offsets_Bounds
	ld b,12
	call Graph_Render_FromOffsets
@:
	ld a,1
	jp Graph_Render
;============================================================
Graph_Render_FromOffsets:
	di
	push iy
		push bc
			push hl
				call TrashRAM_SwapIn
				pop hl
			pop bc
Graph_Render_FromOffsets_Inner:
		push bc
			push hl
				ld e,(hl)
				ld d,0
				push de
					ld hl,axes_sx
					add hl,de
					add hl,de
					ld e,(hl)
					inc hl
					ld d,(hl)
					pop bc
				ld hl,axes_sy
				add hl,bc
				add hl,bc
				ld c,(hl)
				inc hl
				ld b,(hl)
				pop hl
			inc hl
			push hl
				push de				;x0
					push bc			;y0
						ld e,(hl)
						ld d,0
						push de		;offset
							ld hl,axes_sy
							add hl,de
							add hl,de
							ld c,(hl)
							inc hl
							ld b,(hl)
							pop de
						ld hl,axes_sx
						add hl,de
						add hl,de
						ld a,(hl)
						inc hl
						ld h,(hl)
						ld l,a
						push bc
							pop ix
						;hl = x1, ix = y1
						pop bc
					pop de
				ld iy,(fgcolor)
				call ColorLine_SwapOutIn
				pop hl
			inc hl
			pop bc
		djnz Graph_Render_FromOffsets_Inner
		call TrashRAM_SwapOut
		pop iy
	ei
	ret

Graph_Render:
	ld (erase_mode),a
	call TrashRAM_SwapIn

	ld hl,grid_sx
	ld (pgrid_sx),hl
	ld hl,grid_sy
	ld (pgrid_sy),hl
	ld hl,grid_colors
	ld (pgrid_colors),hl
	
	di
	push iy
#ifdef DEBUG_GRAPH
		ld b,1
#else
		ld b,MAX_EQS
#endif

Graph_Render_EQ:
		push bc
			ld hl,(pgrid_sx)
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
							ld hl,(pgrid_colors)
							ld e,(hl)
							inc hl
							ld d,(hl)
							inc hl
							ld (pgrid_colors),hl
							push de
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
							call ColorLine_SwapOutIn
								
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
				push de
					pop iy
				ld hl,(pgrid_sx)
				ld e,(hl)
				inc hl
				ld d,(hl)
				inc hl
				ld (pgrid_sx),hl
				ld a,(dim_x)
				ld c,a
				ld b,0
				add hl,bc
				add hl,bc
				dec hl
				ld b,(hl)
				dec hl
				ld c,(hl)
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
				call ColorLine_SwapOutIn
				pop bc
			dec b
			jp nz,Graph_Render_EQ_YMajor_Inner
			dec c
			jp nz,Graph_Render_EQ_YMajor_Outer
			pop bc	
		dec b
		jp nz,Graph_Render_EQ
		pop iy
	ei

	jp TrashRAM_SwapOut
;============================================================
Rotate_B_Points:
	push bc
		; Get the X coordinate
		ld hl,(pgrid_x)
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld hl,(cx)
		ex de,hl
		call subhlde_fp			;val_x = (val_x - cx)
		ld (val_x),hl
		; Get the Y coordinate
		ld hl,(pgrid_y)
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld hl,(cy)
		ex de,hl
		call subhlde_fp			;val_y = (val_y - cy)
		ld (val_y),hl
		; Get the Z coordinate
		ld hl,(pgrid_z)
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld hl,(cz)
		ex de,hl
		call subhlde_fp			;val_z = (val_z - cz)
		ld (val_z),hl
		
		; Do the rotation. Ogod.
		ld bc,(val_x)				;dx =  (val_x-cx)*costy*costz + (val_y-cy)*(costy*sintz) + (val_z-cz)*(sinty);
		ld de,(costy)
		call signed_multbcde_fp		;de.hl as d.e
		ld bc,(costz)
		call signed_multbcde_fp
		push de
			ld bc,(val_y)
			ld de,(costy)
			call signed_multbcde_fp
			ld bc,(sintz)
			call signed_multbcde_fp
			push de
				ld bc,(val_z)
				ld de,(sinty)
				call signed_multbcde_fp
				pop hl
			call addhlde_fp
			pop de
		call addhlde_fp
		ld de,(cx)
		call addhlde_fp
		; Store the new x val
		ex de,hl
		ld hl,(pgrid_x)
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ld (pgrid_x),hl
		
		ld bc,(val_x)				;-(val_x-cx)*(costx*sintz+costz*sintx*sinty) + (val_y-cy)*(costx*costz-sintx*sinty*sintz) + (val_z-cz)*costy*sintx;
		push bc
			ld de,(costx)
			call signed_multbcde_fp
			ld bc,(sintz)
			call signed_multbcde_fp
			pop bc
		push de
			ld de,(costz)
			call signed_multbcde_fp
			ld bc,(sintx)
			call signed_multbcde_fp
			ld bc,(sinty)
			call signed_multbcde_fp
			push de
				ld bc,(val_y)
				push bc
					ld de,(costx)
					call signed_multbcde_fp
					ld bc,(costz)
					call signed_multbcde_fp
					pop bc
				push de
					ld de,(sintx)
					call signed_multbcde_fp
					ld bc,(sinty)
					call signed_multbcde_fp
					ld bc,(sintz)
					call signed_multbcde_fp
					push de
						ld bc,(val_z)
						ld de,(costy)
						call signed_multbcde_fp
						ld bc,(sintx)
						call signed_multbcde_fp
						pop hl
					call addhlde_fp
					pop de
				call addhlde_fp
				pop de
			call subhlde_fp
			pop de
		call subhlde_fp
		ld de,(cy)
		call addhlde_fp
		; Store the new y val
		ex de,hl
		ld hl,(pgrid_y)
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ld (pgrid_y),hl

		ld bc,(val_x)				;(val_x-cx)*(sintx*sintz-costx*costz*sinty) - (val_y-cy)*(costz*sintx+costx*sinty*sintz) + (val_z-cz)*costx*costy;
		push bc
			ld de,(sintx)
			call signed_multbcde_fp
			ld bc,(sintz)
			call signed_multbcde_fp
			pop bc
		push de
			ld de,(costx)
			call signed_multbcde_fp
			ld bc,(costz)
			call signed_multbcde_fp
			ld bc,(sinty)
			call signed_multbcde_fp
			push de
				ld bc,(val_y)
				push bc
					ld de,(costz)
					call signed_multbcde_fp
					ld bc,(sintx)
					call signed_multbcde_fp
					pop bc
				push de
					ld de,(costx)
					call signed_multbcde_fp
					ld bc,(sinty)
					call signed_multbcde_fp
					ld bc,(sintz)
					call signed_multbcde_fp
					push de
						ld bc,(val_z)
						ld de,(costx)
						call signed_multbcde_fp
						ld bc,(costy)
						call signed_multbcde_fp
						pop hl
					ex de,hl
					call subhlde_fp
					pop de
				call subhlde_fp
				pop de
			call subhlde_fp
			pop de
		call addhlde_fp
		ld de,(cz)
		call addhlde_fp
		; Store the new y val
		ex de,hl
		ld hl,(pgrid_z)
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ld (pgrid_z),hl
		pop bc
	dec b
	jp nz,Rotate_B_Points
	ret

;============================================================
Map_B_Points:
	push bc
		; Get the X coordinate
		ld hl,(pgrid_x)
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		ld (pgrid_x),hl
		ld hl,(cx)
		ex de,hl
		call subhlde_fp			;val_x = (val_x - cx)
		ld (val_x),hl
		; Get the Y coordinate
		ld hl,(pgrid_y)
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		ld (pgrid_y),hl
		ld hl,(cy)
		ex de,hl
		call subhlde_fp			;val_y = (val_y - cy)
		ld (val_y),hl
		; Get the Z coordinate
		ld hl,(pgrid_z)
		ld e,(hl)
		inc hl
		ld d,(hl)
		inc hl
		ld (pgrid_z),hl
		ld hl,(cz)
		ex de,hl
		call subhlde_fp			;val_z = (val_z - cz)
		ld (val_z),hl
		
		;hl = (val_z)
		ld de,FP_EZ
		call cphlde_fp
		jr c,Graph_Map_EQ_Inner_Cull
		ex de,hl		;de = (val_z)
		ld a,(FP_EZ*(5/4))>>8
		ld b,(FP_EZ*(5/4))&$ff
		ld c,0
		call signed_divabcde			; returns result in abc (a.bc/d.e = ab.c)
		push bc
			ld de,(val_x)
			call signed_multbcde_fp
			ld hl,(0.5*(5/4))*INT_TO_8P8		;256*[0.625+(dx-ex)/((5/4)*ez/dz)] = 256*(5/4)*[0.5+(dx-ex)/(ez/dz)] 
			call addhlde_fp						; = 320*[0.5+(dx-ex)/(ez/dz)]
			;de*256 is the int, so just take d.e and put it diretly in (sx)
			ld (sx),hl
			pop bc
		ld de,(val_y)
		call signed_multbcde_fp
		ld hl,(0.5*(5/4))*INT_TO_8P8
		call addhlde_fp
		ex de,hl
		sla e
		rl d									;double de
		ld bc,120
		call signed_multbcde_fp
		ld (sy),de								;d is the upper byte (int is in the bottom byte)
		jr Graph_Map_EQ_Inner_StoreReloop
Graph_Map_EQ_Inner_Cull:
		ld hl,FP_MAGICCULL
		ld (sx),hl
		ld (sy),hl
Graph_Map_EQ_Inner_StoreReloop:
		ld de,(sx)
		ld hl,(pgrid_sx)
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ld (pgrid_sx),hl
		ld de,(sy)
		ld hl,(pgrid_sy)
		ld (hl),e
		inc hl
		ld (hl),d
		inc hl
		ld (pgrid_sy),hl

		pop bc
	dec b
	jp nz,Map_B_Points
	ret

#include "g3dcmath.asm"
#include "utils.asm"
#include "ltstore.asm"
#include "winhook.asm"
#include "yequhook.asm"

;------------------------------------------------------------------------------- SPRITES
spriteup_black_palette:
	.db $00,$00,$ff,$ff
spriteup_black:
	.dw spriteup_black_palette
	.db 20,16
	.db $00,$f0,$00
	.db $01,$98,$00
	.db $03,$0c,$00
	.db $06,$66,$00
	.db $0c,$f3,$00
	.db $19,$f9,$80
	.db $33,$fc,$c0
	.db $67,$fe,$60
	.db $cf,$ff,$30
	.db $80,$00,$10
	.db $80,$00,$10
	.db $ff,$ff,$f0
	.db $ff,$ff,$f0
	.db $80,$00,$10
	.db $80,$00,$10
	.db $ff,$ff,$f0
spriteup_white_palette:
	.db $ff,$ff,$00,$00
spriteup_white:
	.dw spriteup_white_palette
	.db 20,16
	.db $00,$00,$00
	.db $00,$60,$00
	.db $00,$f0,$00
	.db $01,$98,$00
	.db $03,$0c,$00
	.db $06,$06,$00
	.db $0c,$03,$00
	.db $18,$01,$80
	.db $30,$00,$c0
	.db $7f,$ff,$e0
	.db $7f,$ff,$e0
	.db $00,$00,$00
	.db $00,$00,$00
	.db $7f,$ff,$e0
	.db $7f,$ff,$e0
	.db $00,$00,$00
switchaxes_palette:
	.db $00,$20,$ff,$ff,$4a,$69,$9c,$f3
switchaxes:
	.dw switchaxes_palette
	.db 16,16
	.db $00,$00,$00,$00
	.db $15,$55,$55,$54
	.db $15,$55,$95,$54
	.db $15,$56,$85,$54
	.db $15,$5b,$b1,$54
	.db $15,$55,$b5,$54
	.db $15,$55,$b5,$54
	.db $15,$55,$15,$54
	.db $15,$55,$35,$54
	.db $15,$57,$09,$54
	.db $15,$72,$5c,$c4
	.db $1b,$8d,$57,$04
	.db $18,$d5,$54,$04
	.db $1a,$b5,$55,$54
	.db $15,$55,$55,$54
	.db $00,$00,$00,$00
switchback_palette:
	.db $00,$00,$ff,$ff
switchback:
	.dw switchback_palette
	.db 16,16
	.db $00,$ff
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $7f,$01
	.db $00,$ff
changecolor_palette:
	.db $00,$00,$ff,$ff,$10,$1f,$00,$ff,$02,$7f,$03,$ff,$05,$7f,$06,$ff
	.db $07,$fe,$07,$f8,$07,$f2,$07,$ec,$07,$e6,$07,$e1,$06,$df,$27,$e0
	.db $57,$e0,$87,$e0,$b7,$e0,$e7,$e0,$2f,$e0,$ff,$60,$ff,$40,$fd,$e0
	.db $07,$e0,$fc,$60,$fa,$e0,$f9,$60,$f8,$00
changecolor:
	.dw changecolor_palette
	.db 16,16
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0
	.db 0,1,2,3,4,5,6,7,8,9,10,11,12,13,1,0
	.db 0,1,3,4,5,6,14,8,9,10,11,12,13,15,1,0
	.db 0,1,4,5,6,7,8,9,10,11,12,13,15,16,1,0
	.db 0,1,5,6,7,8,9,10,11,12,13,15,16,17,1,0
	.db 0,1,6,7,8,9,10,11,12,13,15,16,17,18,1,0
	.db 0,1,7,8,9,10,11,12,13,15,16,17,18,19,1,0
	.db 0,1,8,9,10,11,12,13,20,16,17,18,19,21,1,0
	.db 0,1,9,10,11,12,13,15,16,17,18,19,22,23,1,0
	.db 0,1,10,11,12,24,15,16,17,18,19,22,23,25,1,0
	.db 0,1,11,12,24,15,16,17,18,19,21,23,25,26,1,0
	.db 0,1,12,13,15,16,17,18,19,22,23,25,26,27,1,0
	.db 0,1,13,15,16,17,18,19,21,23,25,26,27,28,1,0
	.db 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0
	.db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
AxesBoundsX:
	.dw max_x, min_x, sub_max_x, sub_max_x
	.dw val_zero, val_zero, sub_min_x, negsub_min_x
	.dw val_zero, val_zero, val_zero, val_zero
	.dw max_x, min_x, max_x, max_x
	.dw min_x, max_x, min_x, min_x
AxesBoundsY:
	.dw val_zero, val_zero, sub_min_y, negsub_min_y
	.dw max_y, min_y, sub_max_y, sub_max_y
	.dw val_zero, val_zero, sub_min_y, negsub_min_y
	.dw max_y, max_y, min_y, max_y
	.dw min_y, min_y, max_y, min_y
AxesBoundZ:
	.dw val_zero, val_zero, val_zero, val_zero
	.dw val_zero, val_zero, val_zero, val_zero
	.dw val_max_z, val_min_z, sub_max_z, sub_max_z
	.dw val_max_z, val_max_z, val_max_z, val_min_z
	.dw val_min_z, val_min_z, val_min_z, val_max_z
Offsets_Axes:
	.db 0,1,0,2,0,3
	.db 4,5,4,6,4,7
	.db 8,9,8,10,8,11
Offsets_Bounds:
	.db 12,13,12,14,12,15
	.db 16,17,16,18,16,19
	.db 14,17,15,17,13,18
	.db 15,18,13,19,14,19

windowTable:
      .db   $FF
      .dw   0
windowTableStart:
      .db   WINDOW_ID_OFFSET+0
      .dw   winMenu_sMinX
      .db   WINDOW_ID_OFFSET+1
      .dw   winMenu_sMaxX
      .db   WINDOW_ID_OFFSET+2
      .dw   winMenu_sStepsX
      .db   WINDOW_ID_OFFSET+3
      .dw   winMenu_sMinY
      .db   WINDOW_ID_OFFSET+4
      .dw   winMenu_sMaxY
      .db   WINDOW_ID_OFFSET+5
      .dw   winMenu_sStepsY
      .db   WINDOW_ID_OFFSET+6
      .dw   winMenu_sZoomFact
      .db   $FF
winMenu_sWindow:	.db "3D WINDOW",0
winMenu_sMinX:		.db "Xmin",0
winMenu_sMaxX:		.db "Xmax",0
winMenu_sStepsX:	.db "Xsteps",0
winMenu_sMinY:		.db "Ymin",0
winMenu_sMaxY:		.db "Ymax",0
winMenu_sStepsY:	.db "Ysteps",0
winMenu_sZoomFact:	.db "ZoomFact",0

Window_Field_Table:
	.dw min_x
	.dw max_x
	.dw dim_x
	.dw min_y
	.dw max_y
	.dw dim_y
	
AppIcon:
	.db 2									;Icon type: 16x16, 4-bit color
	.db $ff,$ff,$a5,$34,$a4,$83,$b5,$68,$7c,$c4,$8b,$cb,$3c,$a5,$9a,$6f
	.db $53,$a8,$a6,$16,$7b,$0d,$a9,$96,$3b,$ae,$82,$75,$63,$74,$3c,$36
	.db 32,32								;Image dimensions, for the sprite routine
	.db $00,$00,$01,$11,$11,$00,$00,$00
	.db $00,$11,$10,$01,$23,$11,$10,$00
	.db $11,$00,$00,$01,$22,$44,$41,$11
	.db $10,$11,$00,$52,$24,$44,$11,$61
	.db $10,$00,$11,$52,$22,$11,$66,$61
	.db $10,$00,$05,$11,$11,$44,$66,$61
	.db $10,$07,$77,$51,$54,$86,$66,$91
	.db $a0,$b7,$77,$a1,$58,$68,$69,$01
	.db $1b,$b7,$7a,$a1,$cc,$8c,$90,$01
	.db $1b,$bd,$de,$e1,$cc,$00,$00,$01
	.db $1b,$bd,$de,$e1,$c1,$11,$00,$01
	.db $1b,$bd,$de,$f1,$f0,$00,$11,$01
	.db $1d,$bd,$de,$f1,$90,$00,$00,$11
	.db $01,$1d,$de,$f1,$00,$00,$11,$00
	.db $00,$01,$1f,$f1,$00,$11,$00,$00
	.db $00,$00,$01,$11,$11,$00,$00,$00
Header_Icon_End:

Page0End:
.echo "Page 0 has ", (Page0End-Page0Start), "/16384 bytes allocated\n"

.end