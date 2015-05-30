; Graph3DC for the TI-84+CSE
; By Christopher Mitchell ("Kerm Martian")
; http://www.cemetech.net

; TODO: 
; [/] Detect entirely-offscreen lines and do not draw
; [-] Implement better line-cropping as in Graph3DP?
; [X] Implement proper variable input at Y= menu
; [X] Add progress bar and text while computing graph
; [-] Add graph styles on Y= menu and proper storage
; [X] Fix color leak with 2+ simultaneous equations
; [X] Go somewhere handy after selecting a zoom option -> fixed via JForceGraphNoKey
; [X] Handle errors in ParseInp
; [X] Fix rendering in 3D even in 2D mode.
; [X] Display proper Zn value next to progress bars
; [X] Fix wrong RAM page swapped in when GraphRender ends
; [X] Fix Window menu not triggering starting from Graph screen -> Now fix no lines appearing
; [-] Fix spectrum colors coming out too dark -> Marked WONTFIX for now.
; [-] Is graph getting drawn X-flipped? Y-flipped? -> Looks good.
; [X] Try to improve precision when doing mapping to screen coords.
; ------v----New items----v------
; [X] Add ZoomFact to Window menu
; [-] Remove step_x / step_y? -> Still need for fixed-point X and Y computation
; [X] Try to optimize computation as much as possible: Pre-compute X and Y and X/Yinc, eg? -> Saved ~14% time for 289 points
; [X] Don't use ScaleFactor to scale ZoomFact in Window menu (x2)
; [X] Fix looping menu <> graph after using the Zoom menu
; [X] Consider averaging point colors for line colors -> Thanks to Runer112 for working through this with me and writing an optimized version
; [-] Experiment with the regraph hook for v--- this
; [X] Fix rendering when you leave via a menu and then return
; [ ] Make 2:Goto in syntax error go to proper equation somehow
; [ ] Add ability to label X, Y, and Z axes; add LabelOn/Off flag
; [ ] Explain what's happening while computation is underway
; [ ] Fix 2D graphing freezing after entering a 3D equation
; [ ] Implement Format menu
; [ ] Implement Tracing
; [ ] Add tip for equation entry in Y= menu
; [ ] Test interaction between Transform and G3DC in all menus
; [/] Add high-resolution, 2-equation mode -> set starting res properly based on mode
; [ ] Lots of beta-testing!
; [ ] Fix bug when Z= equation entry expands to second line -> related to blocking style editing?
; [ ] Add some kind of graphDirty flag for switching between trace and graph.
; [ ] Deal with split-screen flag.
; [ ] Erase progress bars using a fill
; [ ] Reset colors before possible error message in graph computation
; [ ] Set default res to 17/27 when switching modes
; [ ] Adjust MapFactorY and/or MapFactorX for splitscreen modes?

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
;#define DEBUG_EQ
;#define DEBUG_GRAPH
#define DEBUG_HIRES
#define GAMMA_ZERO					; Gamma is always zero in rotation

; Used for "normal" resolution mode
#define MAX_XY_RES 17
#define MAX_EQS 5

; Used for "high" resolution mode
#define MAX_XY_RES_HI 27
#define MAX_EQS_HI 2

#define sMAX(a,b) (((a) > (b))?0+(a):0+(b))		; Static max function

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
.var fp8.8, val_min_z
.var fp8.8, val_max_z			;TODO: Consider making these OS floats
.var fp8.8, cam_radius
.var fp8.8, alpha
.var fp8.8, beta
.var fp8.8, gamma
.var fp8.8, val_x
.var fp8.8, val_y
.var fp8.8, val_z
.var fp8.8, MapFactorX
.var word,  MapFactorY
.var word,  PxlMinY

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
.var byte[9], deltaX_OS
.var byte[9], deltaY_OS
.var byte[9], minX_OS
.var byte[9], minY_OS

.var word, sx
.var word, sy

.var byte, colormode
.var word, bgcolor
.var word, fgcolor				;Used for bounds and axis rendering
.var byte, axismode
.var byte, dim_x
.var byte, dim_y
.var byte, ateq

; The following used for lines and points
.var word, xstart
.var word, ystart
.var word, xyinc
.var word, lineColour

.var byte, curmenu
.var byte, whichtrace
.var byte, high_bank_page
.var byte, high_bank_mask
.var byte, at_step_x
.var byte, at_step_y
.var byte, counteqs
.var byte, erase_mode
.var word, lts_av
.var byte[MAX_EQS], eq_en_cache	; Cache of which Z functions are enabled
.var byte, totalXiters
.var byte, thisXiters
.var byte, completeXiters

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
#define MAX_COLOR_MODES 3
#define MAX_AXIS_MODES 4
#define AXES_BOUND_COORDS 20
#define COLOR_GRAY $8410
#define COLOR_WHITE $ffff
#define COLOR_BLACK $0000
#define WINDOW_ID_OFFSET $80
#define MIN_FILL_ERASE_SEGMENTS 600
#define PROGRESS_WIDTH 17

#define DEFAULT_XY_RES MAX_XY_RES
#define DEFAULT_XY_MIN $F800
#define DEFAULT_XY_MAX $0800

#define AXIS_MODE_NONE 0				; Neither axes nor bounds
#define AXIS_MODE_A 1					; Axes only
#define AXIS_MODE_B 2					; Bounds only
#define AXIS_MODE_AB 3					; Both axes and bounds

#define FP_MAX	$7FFF
#define	FP_MIN	$8000
#define FP_PI	$0324
#define FP_2	$0200
#define FP_EZ	$0100		;1.f/mtanf(fov/2.f) = 1.0 where fov=pi/2
#define FP_0	$0000
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
#define SETTINGS_AVOFF_HIRES	5				;1 byte
#define SETTINGS_AVOFF_XDIM		6				;1 byte
#define SETTINGS_AVOFF_YDIM		7				;1 byte
#define SETTINGS_AVOFF_SCALEF	8				;2 bytes
#define SETTINGS_AVOFF_ZOOMF	10				;2 bytes
#define SETTINGS_AVOFF_MINX		12				;2 bytes
#define SETTINGS_AVOFF_MINY		14				;2 bytes
#define SETTINGS_AVOFF_MAXX		16				;2 bytes
#define SETTINGS_AVOFF_MAXY		18				;2 bytes
#define SETTINGS_HOOKBACK_WIN	20				;4 bytes  - WindowHook backup
#define SETTINGS_HOOKBACK_YEQU	24				;4 bytes  - YEquHook backup
#define SETTINGS_HOOKBACK_CUR	28				;4 bytes  - CursorHook backup
#define SETTINGS_MONVECBACK		32				;13 bytes - Monitor vector backup
#define SETTINGS_HOOKBACK_APP	45				;4 bytes  - AppChangeHook backup
#define SETTINGS_HOOKBACK_MENU	49				;4 bytes  - MenuHook backup
#define SETTINGS_HOOKBACK_GRPH	53				;4 bytes  - GraphHook backup
#define SETTINGS_HOOKBACK_KEY	57				;4 bytes  - KeyHook backup
#define SETTINGS_HOOKBACK_REGR	61				;4 bytes  - RegraphHook backup
#define SETTINGS_HOOKBACK_TRACE	65				;4 bytes  - TraceHook backup
#define SETTINGS_AVOFF_MAXEQS	69				;1 byte
#define SETTINGS_AVOFF_TRACE	70				;1 byte   - 1 if tracing, 0 otherwise

capacity_3d_el = sMAX(MAX_EQS * MAX_XY_RES * MAX_XY_RES, MAX_EQS_HI * MAX_XY_RES_HI * MAX_XY_RES_HI)
capacity_2d_el = sMAX(MAX_XY_RES * MAX_XY_RES, MAX_XY_RES_HI * MAX_XY_RES_HI)

; "Dynamic" allocation for graph-drawing data
trash_ram_loc	.equ	$C000
grid_x			.equ	trash_ram_loc + 0
grid_y			.equ	grid_x + (capacity_3d_el * 2)
grid_z			.equ	grid_y + (capacity_3d_el * 2)
grid_sx			.equ	grid_z + (capacity_3d_el * 2)
grid_sy			.equ	grid_sx + (capacity_2d_el * 2)
axes_sx			.equ	grid_sy + (capacity_2d_el * 2)
axes_sy			.equ	axes_sx + (AXES_BOUND_COORDS*2)
grid_colors		.equ	axes_sy + (AXES_BOUND_COORDS*2)
axes_x			.equ	grid_colors + (capacity_3d_el * 2)
axes_y			.equ	axes_x + (2*20)
axes_z			.equ	axes_y + (2*20)
;dialogCBRAM		.equ	axes_z + (2*20)
trash_ram_end	.equ	axes_z + (2*20)		;dialogCBRAM + 16		; 16 bytes for (windowMenuCallbackEnd - windowMenuCallback)
trash_ram_fill	.equ	(trash_ram_end - trash_ram_loc)
.echo "Trash RAM page has ", trash_ram_fill, "/16384 bytes allocated\n"
.if trash_ram_fill > ($4000-$200)
.fail "Trash RAM page has overflowed!"
.endif

; As suggested by MicrOS. This restricts how much
; stack we can use.
;IvtLocation	.equ	080h ; vector table at 8000h
;IsrLocation	.equ	085h ; ISR at 8585h

; OS Equates - Hooks
hookflags2		.equ 	34h
hookflags3		.equ 	35h ;also sysHookFlg1
hookflags4		.equ	36h
_SetAppChangeHook = 5011h
_ClrAppChangeHook =	5014h
_SetYEquHook	.equ	4FB4h
_ClrYEquHook	.equ	4FB7h
_SetMenuHook	.equ	$5068
_ClrMenuHook	.equ	$506B
;_SetGraphHook	.equ	$4F9C
;_ClrGraphHook	.equ	$4F9F
;_SetTraceHook	.equ	$4FD8
;_ClrTraceHook	.equ	$4FDB
appChangeHookPtr .equ	09E91h
yEqualsHookPtr	.equ	09E79h
MenuHookPtr		.equ	$9EA1
GraphHookPtr	.equ	$9E75
TraceHookPtr	.equ	$9E89
appChangeHookActive .equ 2
yEquHookActive	.equ 	4		;1 = Y= hook active
MenuHookActive	.equ	6
GraphHookActive	.equ	3
traceHookActive	.equ	0					; In hookflags4

; OS Equates - Other
APIFlg			equ 28h
appAllowContext		equ 0           ;App wants context changes to happen
monQueue		.equ	$8669
PlotEnabled1	.equ	$9812
PlotEnabled2	.equ	$9824
PlotEnabled3	.equ	$9836
menuCurCol		.equ	$9d83
_ClearAppTitle	.equ	5056h
_maybe_MonRestart .equ	$4fba
mZoom			.equ	04h
mZoom3D			.equ	94h
fastSpeed		.equ	5
speedFlags		.equ	24h
_GetBytePaged	.equ	_LoadBIndPaged
tZ1				.equ	tY0+1
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
	call SetSpeedFast
	; Initialize all the hooks via a nice menu
	call LTS_CacheAV

	; Save the appChangeHook
	call GetCurrentPage
	ld b,a
	ld a,(appChangeHookPtr+2)
	sub b
	jr z,ProgramStart_appInstalled

	push bc
		ld a,SETTINGS_HOOKBACK_APP
		call LTS_GetPtr						;to hl
		ld de,appChangeHookPtr
		ex de,hl
		ld bc,3
		ldir
		ld a,(flags + hookflags4)			; contains windowHookActive
		and 1 << appChangeHookActive
		ld (de),a
		pop bc
	
	ld a,(MenuHookPtr+2)
	cp b
	jr z,ProgramStart_HookBackupDone

	; Back up the current Menu hook
	ld a,SETTINGS_HOOKBACK_MENU
	call LTS_GetPtr						;to hl
	ld de,MenuHookPtr
	ex de,hl
	ld bc,3
	ldir
	ld a,(flags + hookflags4)			; contains MenuHookActive
	and 1 << MenuHookActive
	ld (de),a

ProgramStart_HookBackupDone:
	ld a,1
ProgramStart_appInstalled:
	xor 1			; 1 <-> 0
	add a,2
	ld b,a			; b = number of menu options
	push bc
		bcall(_ClrLCDFull)
		bcall(_HomeUp)
		call DisplayAppTitle
		ld de,14
		ld hl,40
		ld ix,bigicon
		call DisplayNormal
		call DrawSprite_4Bit_Enlarge
		call DisplayOrg
		ld hl,1+(8*256)
		ld (currow),hl
		ld hl,Text_G3DCName
		call PutsApp						; Display App name
		ld hl,98
		ld (pencol),hl
		ld l,76								; d set to 0 by previous ld de
		ld (penrow),hl
		ld hl,Text_G3DCDesc
		call VPutsApp						; Display small description text below
		pop bc
	ld c,0									; b = # items; c = selected item
ProgramStart_MainMenuDisplay:
	ld hl,4+(0*256)
	ld (currow),hl

	ld a,b
	sub 2
	ld hl,MMenu_JTable
	add a,a
	add a,a
	ld e,a
	ld d,0
	add hl,de
	push hl
		push bc
ProgramStart_MainMenuDisplayLoop:
			push bc
				push hl
					ld a,(hl)
					inc hl
					ld h,(hl)
					ld l,a
					call PutsApp
					ld h,0
					ld a,(currow)
					inc a
					ld l,a
					ld (currow),hl	; 0->col, row+1->row
					pop hl
				inc hl
				inc hl
				pop bc
			djnz ProgramStart_MainMenuDisplayLoop
ProgramStart_MainMenuLoop_Outer:
			pop bc
ProgramStart_MainMenuLoop_OuterNoPopBC:
		pop de
	push de
		push bc
			ld h,0
			ld a,4
			add a,c
			ld l,a
			ld (currow),hl			; 0->col, c+4->row
			push hl
				ld l,c
				ld h,0
				ex de,hl
				add hl,de
				add hl,de
				ld a,(hl)
				inc hl
				ld h,(hl)
				ld l,a
				push hl
					ld de,COLOR_WHITE
					ld bc,COLOR_BLACK
					call PutsColored
					pop de
				pop hl
			ld (currow),hl
			push de
			
ProgramStart_MainMenuLoop_Inner:
				bcall(_getkey)
				or a
				jr z,ProgramStart_MainMenuLoop_Inner
				pop hl
			push af
				ld bc,COLOR_WHITE
				ld de,COLOR_BLACK
				call PutsColored
				pop af
			
			pop bc
		cp kUp
		jr nz,ProgramStart_MainMenuLoop_Inner_NotUp
		dec c
		bit 7,c
		jr z,ProgramStart_MainMenuLoop_OuterNoPopBC
		ld c,b
		dec c
		jr ProgramStart_MainMenuLoop_OuterNoPopBC
ProgramStart_MainMenuLoop_Inner_NotUp:
		cp kDown
		jr nz,ProgramStart_MainMenuLoop_Inner_NotDown
		inc c
		ld a,c
		cp b
		jr nz,ProgramStart_MainMenuLoop_OuterNoPopBC
		ld c,0
		jr ProgramStart_MainMenuLoop_OuterNoPopBC
ProgramStart_MainMenuLoop_Inner_NotDown:
		cp kClear
		jr z,ProgramStart_Quit
		cp k1
		jr z,ProgramStart_Install
		cp k2
		jr nz,ProgramStart_Not2
		ld a,b
		cp 2
		jr z,ProgramStart_Quit
		jr ProgramStart_Uninstall
ProgramStart_Not2:
		cp k3
		jr nz,ProgramStart_Not3
		ld a,b
		cp 3
		jr z,ProgramStart_Quit
		jr ProgramStart_MainMenuLoop_OuterNoPopBC
ProgramStart_Not3:
		cp kEnter
		jr z,ProgramStart_DoAndQuit
		cp kAlphaEnter
		jr nz,ProgramStart_MainMenuLoop_OuterNoPopBC
		
ProgramStart_DoAndQuit:
		pop hl
	ld a,c
	or a
	jr z,ProgramStart_Install
	ld a,b
	sub c
	dec a
	jr z,ProgramStart_Quit

ProgramStart_Uninstall:
	; Restore the AppChangeHook area
	ld de,appChangeHookPtr+2
	ld bc,($ff^(1 << appChangeHookActive))*256 + SETTINGS_HOOKBACK_APP
	ld hl,flags + hookflags4
	call DisableHook

	; Restore the MenuHook area
	ld de,MenuHookPtr+2
	ld bc,($ff^(1 << MenuHookActive))*256 + SETTINGS_HOOKBACK_MENU
	ld hl,flags + hookflags4
	call DisableHook

	jr ProgramStart_Quit

ProgramStart_Install:
	call InitZEquations

	; Set up the appChangeHook
	res 1,(iy+$3A)							;?????
	call GetCurrentPage
	ld hl,appChangeHook						;the ACTUAL appChange hook.
	bcall(_SetAppChangeHook)

	; Set up new Yequ hook
	call GetCurrentPage
	ld hl,MenuHook
	bcall(_SetMenuHook)

ProgramStart_Quit:
	; Cleanup/quit
	call DisplayNormal
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

				; Back up the current YequHook
				ld a,SETTINGS_HOOKBACK_YEQU
				call LTS_GetPtr						;to hl
				ld de,yEqualsHookPtr
				ex de,hl
				ld bc,3
				ldir
				ld a,(flags + hookflags3)			; contains yEquHookActive
				and 1 << yEquHookActive
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
				or a
				jr nz,appChangeHook_CheckWindow				; If it's zero, we haven't triggered 3D mode yet
appChangeHook_Invalid:
				jr appChangeHook_Done

appChangeHook_CheckWindow:
				call SetFunctionMode						; Needs to only touch a!
				ld a,c
				cp kWindow
				jr nz,appChangeHook_CheckGraph
				
				; Back up the current WindowHook
				ld a,SETTINGS_HOOKBACK_WIN
				call LTS_GetPtr						;to hl
				ld de,windowHookPtr
				ex de,hl
				ld bc,3
				ldir
				ld a,(flags + hookflags3)			; contains windowHookActive
				and 1 << windowHookActive
				ld (de),a
				
				; Set up new Window hook
				call GetCurrentPage
				ld hl,windowHook
				bcall(_SetWindowHook)
				jr appChangeHook_Done

appChangeHook_CheckGraph:
				ld b,1
				cp kTrace
				jr z,appChangeHook_GoGraph
				dec b
				cp kGraph
				jr nz,appChangeHook_Invalid
appChangeHook_GoGraph:
				push bc
					ld a,SETTINGS_AVOFF_TRACE
					call LTS_GetPtr					;to hl
					pop af
				ld (hl),a							;Whether tracing is enabled
				
				; Back up the current GraphHook
				ld a,SETTINGS_HOOKBACK_GRPH
				call LTS_GetPtr						;to hl
				ld de,GraphHookPtr
				ex de,hl
				ld bc,3
				ldir
				ld a,(flags + hookflags3)			; contains MenuHookActive
				and 1 << GraphHookActive
				ld (de),a
				
				; Set up new Graph hook
				call GetCurrentPage
				ld hl,GraphHook
				bcall(_SetGraphHook)

#ifdef false
				; Back up the current RawKeyHook
				ld a,SETTINGS_HOOKBACK_KEY
				call LTS_GetPtr						;to hl
				ld de,RawKeyHookPtr
				ex de,hl
				ld bc,3
				ldir
				ld a,(flags + hookflags2)			; contains MenuHookActive
				and 1 << RawKeyHookActive
				ld (de),a
				
				; Set up new RawKeyHook
				call GetCurrentPage
				ld hl,GraphKeyHook
				bcall(_SetRawKeyHook)
#endif

				jp appChangeHook_Done

CleanTempHooks:
	;WindowHook
	ld de,windowHookPtr+2
	ld bc,($ff^(1 << windowHookActive))*256 + SETTINGS_HOOKBACK_WIN
	ld hl,flags + hookflags3
	call DisableHook

	;YEqualsHook
	ld de,yEqualsHookPtr+2
	ld bc,($ff^(1 << yEquHookActive))*256 + SETTINGS_HOOKBACK_YEQU
	ld hl,flags + hookflags3
	call DisableHook

	;GraphHook
	ld de,GraphHookPtr+2
	ld bc,($ff^(1 << GraphHookActive))*256 + SETTINGS_HOOKBACK_GRPH
	ld hl,flags + hookflags3
	call DisableHook
	
	;RawKeyHook
	ld de,RawKeyHookPtr+2
	ld bc,($ff^(1 << RawKeyHookActive))*256 + SETTINGS_HOOKBACK_KEY
	ld hl,flags + hookflags2
	call DisableHook

	ret

;-----------------------------------
; Disables the given hook
; Inputs:
;   - de points to third byte of Ptr (eg, RawKeyHookPtr+2)
;   - b holds iy+hookflag mask
;   - hl holds iy+hookflag pointer
;   - c holds AppVar offset
DisableHook:
	push bc
		ld a,(de)
		ld b,a
		call GetCurrentPage
		cp b
		pop bc						;a now holds iy+hookflag mask
	ret nz							;it wasn't our hook
	ld a,(hl)
	and b
	push af
		push hl
			ld a,3
			add a,c
			call LTS_GetPtr			;a offset -> hl pointer into AppVar
			pop bc
		pop af
	or (hl)
	ld (bc),a						; ((iy+hookflag) & ~hookbit) | oldhookbit -> (iy+hookflag)
	dec hl
	ld bc,3
	lddr
	ret

;-----------------------------------
; Hook Chainer is used to chain hooks
; Arguments:
;  - hl = AV offset
;  - Format of hl-pointed memory: ADL, ADH, PG, EN
; Notes:
;  - You must push hl before calling this
;  - When this routine returns, the stack will be 4 bytes lower, because hl and de will be popped.
;  - You MUST CALL this routine, not jump to it
HookChainer:
		push de
			push af
				push bc
					push hl
						ld hl,AVName
						rst 20h
						bcall(_chkfindsym)
						pop hl
					jr c,HookChainer_Continue
					inc hl
					inc hl								;Account for size
					add hl,de
					ld e,(hl)
					inc hl
					ld d,(hl)
					inc hl
					ld b,(hl)
					inc hl
					ld a,(hl)
					or a
					jr z,HookChainer_Continue
					ld h,d
					ld l,e
					inc de
					ld (Op1),de
					ld a,b
					ld (Op1+2),a
					B_CALL(_GetBytePaged)
					ld a,b
					cp 83h
					jr nz,HookChainer_Continue
					pop bc
				pop af
			pop de
		pop hl
	ex (sp),hl
	bcall(OP1 | (1 << 14))
	ret

HookChainer_Continue:
					pop bc
				pop af
			pop de
		pop hl
	ex (sp),hl
	ret

;-----------------------------------
Menu4_GraphModify_ShowAxes:
	call Graph_Erase
	ld a,(axismode)
	inc a
	cp MAX_AXIS_MODES
	jr nz,Menu4_GraphModify_ShowAxes_Save
	xor a
Menu4_GraphModify_ShowAxes_Save:
	ld (axismode),a
	ret
	;jp Menu_4_Redraw

Menu4_GraphModify_GraphColor:
	call Graph_Erase
	ld a,(colormode)
	inc a
	cp MAX_COLOR_MODES
	jr nz,Menu4_GraphModify_GraphColor_Save
	xor a
Menu4_GraphModify_GraphColor_Save:
	ld (colormode),a
	ret
	;jp Graph_Recolor

Menu4_GraphModify_ToggleAll:
	;TODO
	ret
	;jp Menu_4_Redraw

Menu4_GraphModify_BGColor:
	ld hl,(bgcolor)
	ld a,h \ cpl \ ld h,a
	ld a,l \ cpl \ ld l,a
	ld (bgcolor),hl
	ret
	;jp Menu_4_Redraw
;============================================================
Graph_Erase:
	; Determine the number of segment draws needed to erase
	ld a,(dim_x)
	ld e,a
	ld d,0
	ld a,(dim_y)
	call multade
	ex de,hl
	ld a,(counteqs)
	call multade
	add hl,hl
	ld de,MIN_FILL_ERASE_SEGMENTS
	or a
	sbc hl,de
	jp nc,Graph_Clear_Screen					; Clear by filling
	
	call TrashRAM_SwapIn						; NB: CAN'T CALL OS ROUTINES UNTIL SWAPOUT!
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
	call Graph_Render
	call TrashRAM_SwapOut
	ret
;============================================================
Graph_Render_FromOffsets:
	di
	push iy
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
				call ColorLine
				pop hl
			inc hl
			pop bc
		djnz Graph_Render_FromOffsets_Inner
		pop iy
	ei
	ret

;============================================================
Rotate_B_Points:
	push bc
		; Get the Z coordinate
		ld hl,(pgrid_z)
		ld e,(hl)
		inc hl
		ld d,(hl)
		ld hl,FP_MAGICCULL
		call cphlde
		jp z,Rotate_B_Points_MagicCull
		
		ld hl,(cz)
		ex de,hl
		call subhlde_fp			;val_z = (val_z - cz)
		ld (val_z),hl
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
		
		; Do the rotation. Ogod.
		ld bc,(val_x)				;dx =  (val_x-cx)*costy*costz + (val_y-cy)*(costy*sintz) + (val_z-cz)*(sinty);
		ld de,(costy)
		call signed_multbcde_fp		;de.hl as d.e
#ifndef GAMMA_ZERO
		ld bc,(costz)				; Or if GAMMA_ZERO, gamma=0, costz = 1, null multiplication
		call signed_multbcde_fp
#endif
		push de
#ifdef GAMMA_ZERO
			ld de,0					; gamma = 0, sintz = 0, whole middle term = 0
#else
			ld bc,(val_y)
			ld de,(costy)
			call signed_multbcde_fp
			ld bc,(sintz)
			call signed_multbcde_fp
#endif
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
#ifndef GAMMA_ZERO
		push bc
			ld de,(costx)
			call signed_multbcde_fp
			ld bc,(sintz)
			call signed_multbcde_fp
			pop bc
		push de
#endif
#ifdef GAMMA_ZERO
			ld de,(sintx)			; Factor out the multiplication by costz = 1
#else
			ld de,(costz)
			call signed_multbcde_fp
			ld bc,(sintx)
#endif
			call signed_multbcde_fp
			ld bc,(sinty)
			call signed_multbcde_fp
			push de
				ld bc,(val_y)
				push bc
					ld de,(costx)
					call signed_multbcde_fp
#ifndef GAMMA_ZERO
					ld bc,(costz)
					call signed_multbcde_fp
#endif
					pop bc
				push de
#ifndef GAMMA_ZERO
					ld de,(sintx)
					call signed_multbcde_fp
					ld bc,(sinty)
					call signed_multbcde_fp
					ld bc,(sintz)
					call signed_multbcde_fp
					push de
#endif
						ld bc,(val_z)
						ld de,(costy)
						call signed_multbcde_fp
						ld bc,(sintx)
						call signed_multbcde_fp
#ifndef GAMMA_ZERO
						pop hl
					ex de,hl
					call subhlde_fp					;(val_y-cy)*(-sintx*sinty*sintz) + (val_z-cz)*costy*sintx;
					pop de
#else
					pop hl
#endif
				call addhlde_fp
				pop de
			call subhlde_fp
#ifndef GAMMA_ZERO
			pop de
		call subhlde_fp				; Handles the -(val_x-cx)*(costx*sintz) term
#endif
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
#ifndef GAMMA_ZERO
		push bc
			ld de,(sintx)
			call signed_multbcde_fp
			ld bc,(sintz)
			call signed_multbcde_fp
			pop bc
		push de
#endif
			ld de,(costx)
			call signed_multbcde_fp
#ifndef GAMMA_ZERO
			ld bc,(costz)
			call signed_multbcde_fp
#endif
			ld bc,(sinty)
			call signed_multbcde_fp
			push de
				ld bc,(val_y)
				push bc
#ifdef GAMMA_ZERO
					ld de,(sintx)
#else
					ld de,(costz)
					call signed_multbcde_fp
					ld bc,(sintx)
#endif
					call signed_multbcde_fp
					pop bc
				push de
#ifndef GAMMA_ZERO
					ld de,(costx)
					call signed_multbcde_fp
					ld bc,(sinty)
					call signed_multbcde_fp
					ld bc,(sintz)
					call signed_multbcde_fp
					push de
#endif
						ld bc,(val_z)
						ld de,(costx)
						call signed_multbcde_fp
						ld bc,(costy)
						call signed_multbcde_fp
#ifndef GAMMA_ZERO
						pop hl
					ex de,hl
					call subhlde_fp
					pop de
#else
					pop hl
				ex de,hl
#endif
				call subhlde_fp
				pop de
			call subhlde_fp
#ifndef GAMMA_ZERO
			pop de
		call addhlde_fp
#endif
		ld de,(cz)
		call addhlde_fp
		; Store the new y val
		ex de,hl
Rotate_B_Points_NextPoint:
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

Rotate_B_Points_MagicCull:
		;de still contains FP_MAGICCULL
		ld hl,(pgrid_x)
		inc hl
		inc hl
		ld (pgrid_x),hl
		ld hl,(pgrid_y)
		inc hl
		inc hl
		ld (pgrid_y),hl
		jr Rotate_B_Points_NextPoint
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
		ld hl,FP_MAGICCULL
		call cphlde
		jr z,Graph_Map_EQ_Inner_Cull
		ld hl,(cz)
		ex de,hl
		call subhlde_fp			;val_z = (val_z - cz)
		ld (val_z),hl
		
		;hl = (val_z)
		ld de,FP_EZ
		call cphlde_fp
		jr c,Graph_Map_EQ_Inner_Cull
		ex de,hl		;de = (val_z)
		ld a,(FP_EZ*(5/4))>>4			; Funky math here: FP_EZ*5/4 = 0x0140, val_z >= FP_EZ*(4/5), so get 4 more bits of precision
		ld b,((FP_EZ*(5/4))<<4)&$ff
		ld c,0
		call signed_divabcde			; returns result in abc (a.bc/d.e = ab.c)
		push bc
			ld de,(val_x)
			call signed_multbcde			; result is dHdLeHeLhH.hLlHlL because of <<4 in FP_EZ
			sla h							; move dLeHeLhH into de
			rl e
			rl d
			sla h
			rl e
			rl d
			sla h
			rl e
			rl d
			sla h
			rl e
			rl d
			ld hl,(MapFactorX)					;256*[0.625+(dx-ex)/((5/4)*ez/dz)] = 256*(5/4)*[0.5+(dx-ex)/(ez/dz)] 
			call addhlde_fp						; = 320*[0.5+(dx-ex)/(ez/dz)]
			;de*256 is the int, so just take d.e and put it directly in (sx)
			; Not quite! Now we need to shift 4x, because of the precision trick above.
			ld (sx),hl
			pop bc
		ld de,(val_y)
		call signed_multbcde			; result is dHdLeHeLhH.hLlHlL because of <<4 in FP_EZ
		sla h							; move dLeHeLhH into de
		rl e
		rl d
		sla h
		rl e
		rl d
		sla h
		rl e
		rl d
		sla h
		rl e
		rl d
		ld hl,(0.5*(5/4))*INT_TO_8P8
		call addhlde_fp
		ex de,hl
		sla e
		rl d									;double de
		ld bc,(MapFactorY)						;120 because 240.
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
#include "menuhook.asm"
#include "graphhook.asm"
#include "graphfuncs.asm"
#include "bigicon.inc"
#include "data.inc"

;------------------------------------------------------------------------------- SPRITES

Page0End:
.echo "Page 0 has ", (Page0End-Page0Start), "/16384 bytes allocated\n"

.end