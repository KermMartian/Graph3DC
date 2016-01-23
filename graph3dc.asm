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
; [X] Add ZoomFact to Window menu
; [-] Remove step_x / step_y? -> Still need for fixed-point X and Y computation
; [X] Try to optimize computation as much as possible: Pre-compute X and Y and X/Yinc, eg? -> Saved ~14% time for 289 points
; [X] Don't use ScaleFactor to scale ZoomFact in Window menu (x2)
; [X] Fix looping menu <> graph after using the Zoom menu
; [X] Consider averaging point colors for line colors -> Thanks to Runer112 for working through this with me and writing an optimized version
; [-] Experiment with the regraph hook for v--- this
; [X] Fix rendering when you leave via a menu and then return
; [X] Add some kind of graphDirty flag for switching between trace and graph.
; [X] +/- zooming from graph mode
; [-] Fix 2D graphing freezing after entering a 3D equation (Cannot replicate...?)
; [X] Implement Format menu
; [X] Implement Tracing
; [X] Apply graphhook key fixes to formathook and vice versa so all keys work right
; [X] Fix redrawing Format menu after other menu -> change cxCurApp? Yep, that did it.
; [X] Display proper number of equations based on mode
; [X] Add high-resolution, 2-equation mode -> set starting res properly based on mode
; [X] Set default res to 17/27 when switching modes
; [X] Erase progress bars using a fill
; [X] Reset colors before possible error message in graph computation
; [X] Fix resetting window when entering the Format menu
; [X] Fix erasing behind Format menu
; [X] Fix flashing trace cursor when no equations are enabled
; [X] Fix memory leak on context change out of Graph or Format mode
; [-] Fix bug in Y= menu when entering a menu or using Rcl. -> Unable to replicate
; [X] Explain what's happening while computation is underway
; [X] Add ability to label X, Y, and Z axes; add LabelOn/Off flag
; [X] Add tip for equation entry in Y= menu
; [X] Fix top line of Y=/Z= menu in splitscreen mode
; [X] Implement cleaner way to avoid going into style icons
; [X] Fix freeze when scrolling up after multiline Z= equation entry (changeset 38)
; [X] Resolve inconsistencies with how enabled Z= equations are displayed (changeset 39: hl was destroyed in YEquHook_Full)
; [X] Fix bug when Z= equation entry expands to second line -> related to blocking style editing? -> ended up being very complex.
;     Final solution is to perform equation-swapping between Y= and Z= equations and to just use the Y= menu in its normal function mode.
; [X] Try to fix Window menu title in scrolling splitscreen mode
; [X] Fix ellipses with long equations in graph status area -> simple logic error
; [-] Fix EOL erasing not occuring properly in Y= menu when screen scrolls -> Cannot replicate
; [-] Fix status area app title in graph menu [not replicable?]
; [X] Fix explanation display when Z= scrolls -> Moved to top of window
; [X] Test interactions with programs -> Allows programs to use Y= mode even when Z= mode is enabled for the user
; ------v----New items----v------
; [X] Deal with split-screen flag.
;     [X] Fix invalidating graph on zoom menu items (move the _resets) and Y= editing
;     [X] Adjust MapFactorY and/or MapFactorX for splitscreen modes?
;     [X] Test in splitscreen mode, including -F-o-r-m-a-t-, -W-i-n-d-o-w-, -Z-o-o-m-, -Y-=-, -G-r-a-p-h-
;     [X] Adjust culling code to take max and min Y into account
;     [X] Fix redisplay of homescreen in 3D splitscreen mode
;     [X] set grfSChanged when switching 2D<>3D modes in splitscreen mode
;     [X] Replicate and fix freeze after switching split mode without enabling 3D mode
;     [X] Test and correct glitchiness of graph after Catalog -> repaired, due to using AppVar when RAM swapped out
;     [X] Fix re-rendering 3D graph in splitscreen mode when leaving menus
; [X] Fix bounds when no equations enabled -> create fake min/max Z
; [X] Fix display of graphs not centered on (0,0,0) (rotation already works properly) -> current work manipulates cx/y/z
; [X] Don't force rotation around z=0 -> store computed Z bounds in appvar for use, including for cz?
; [X] Re-fix label erase
; [X] Test graph-table mode and adjust accordingly. Implement table mode? -> No. But catch it?
; [X] Implement self-adjusting scaling!
; [X] Move code to convert OS min/max X/Y to FP a function called separately, call from Window menu
;     [X] Actually make this the self-adjusting scaling routine 
; [X] Catch/fix divby0 when trying to set small window bounds
; [X] Make zoom in/out manipulate the OS values instead of FP values, then call function from previous bullet
;     [X] Fix zoom in/out to work with non-zero centers, then call the self-adjusting scaling function
; [X] Adjust tracing to display OS-computed X and Y values for precision
; [X] Fix trace cursor in status area
; [X] Fix bad erasing when quitting Trace mode. Also manifests itself as axes-over-graph in splitscreen mode.
; [X] Fix context-switching out of Format menu and Trace context (context-change hook getting wrong value) -> stack level...?
;     - It seems that for the TI-84+CSE's NewContext0, both a and b must be set to the new context. Bleh.
; [X] Test what happens when you select Draw, Calc, and Table menu items when 3D mode is enabled.
;     - Not ideal (Calc moves you to the homescreen with a ? preceding an entry), but nothing crashes
;     - [X] Disable calc menu? -> Calc menu is now replaced by a custom menu in 3D mode
; [X] Fix Y= hook causing bad style selection cursor
; [X] Fix memory error after Y= menu? / related to Syntax error in Graph? -> OP1 getting destroyed chaining hooks
; [X] Fix Y bounds set by ZStandard -> traced to bad OPX to OPX copy
; [X] Fix run indicator appearing within graph area while drawing "full"-screen 3D graph
; [X] Fix LCD panic when drawing splitscreen graph - run indicator?
; [ ] Test interaction between Transform and G3DC in all menus
; [ ] Make 2:Goto in syntax error go to proper equation somehow
; [ ] Lots of beta-testing!

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

temp1	.equ cmdShadow	; 260 bytes
temp2	.equ $8585+3	; part of textShadow; leave space for ISR -> at least 132 bytes
temp3	.equ plotSScreen

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
.var fp8.8, MapFactorY1			; See [POST HERE] for explanation
.var word,  MapFactorY2
.var word,  PxlMinY
.var word,  PxlMaxY

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
.var fp8.8, sup_max_x
.var fp8.8, sub_min_y
.var fp8.8, negsub_min_y
.var fp8.8, sub_max_y
.var fp8.8, sup_max_y
.var fp8.8, sub_max_z
.var fp8.8, sup_max_z
.var fp8.8, val_zero
.var byte[9], deltaX_OS
.var byte[9], deltaY_OS
.var byte[9], maxX_OS
.var byte[9], maxY_OS

.var word, sx
.var word, sy

.var byte, labelmode
.var byte, colormode
.var word, bgcolor
.var word, fgcolor				;Used for bounds and axis rendering
.var byte, axismode
.var byte, dim_x
.var byte, dim_y
.var byte, trace_x
.var byte, trace_y
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
.var byte, maxeqs
.var byte, erase_mode
.var word, lts_av
.var byte[MAX_EQS], eq_en_cache	; Cache of which Z functions are enabled
.var byte, totalXiters
.var byte, thisXiters
.var byte, completeXiters

; For multiMenu
.var byte, menuCurCol
.var byte, menuCurRow
.var byte, menuItemSelected

; The following are pointers to *not the beginning* that are moved as rendering progresses
.var word, pgrid_x				;Pointer to variable data	3*MAX_EQS*MAX_XY_RES*MAX_XY_RES*2
.var word, pgrid_y				;Pointer to variable data
.var word, pgrid_z				;Pointer to variable data
.var word, pgrid_sx				;Pointer to variable data	MAX_XY_RES*MAX_XY_RES*2
.var word, pgrid_sy				;Pointer to variable data	MAX_XY_RES*MAX_XY_RES*2
.var word, pgrid_colors			;Pointer to variable data	MAX_EQS*MAX_XY_RES*MAX_XY_RES*2
.var word, paxes_x				;Pointer to variable data	AXES_BOUND_COORDS*2
.var word, paxes_y				;Pointer to variable data	AXES_BOUND_COORDS*2
.var word, paxes_z				;Pointer to variable data	AXES_BOUND_COORDS*2

.varloc temp3, 768
.var byte[2+(8*8*2)], traceCursorBack
.var byte[4], traceCursorPalette

; Constants
#define OUT_LEFT 1
#define OUT_TOP 2
#define OUT_RIGHT 4
#define OUT_BOTTOM 8
#define INT_TO_8P8	256
#define MAX_COLOR_MODES 3
#define MAX_AXIS_MODES 4
#define COLOR_GRAY $8410
#define COLOR_WHITE $ffff
#define COLOR_BLACK $0000
#define COLOR_STATUS $52AA
#define WINDOW_ID_OFFSET $80
#define MIN_FILL_ERASE_SEGMENTS 300
#define PROGRESS_WIDTH 17

#define DEFAULT_XY_RES MAX_XY_RES
#define DEFAULT_XY_RES_HI MAX_XY_RES_HI

#define DEFAULT_XY_SCALEF $0100
#define DEFAULT_XY_ZOOMF  $00C0
#define DEFAULT_XY_MIN    $F800
#define DEFAULT_XY_MAX    $0800

#define AXES_BOUND_COORDS 23
#define AXES_BOUND_PAIRS_AXES 9
#define AXES_BOUND_PAIRS_BOUNDS 12
#define AXES_BOUND_PAIRS_LABELS 3

#define PXLMINY_WITHSTATUS 30
#define PXLMAXY_FULL 240
#define PXLMAXY_SPLIT 154
#define YEQU_EXPLAIN_START_X 100
#define TRACE_COORDS_START_Y 34
#define TRACE_COORDS_START_X 1
#define TRACE_EQ_END_X 290
#define TRACE_COORDS_WIDTH 85
#define TRACE_COORDS_HEIGHT 38

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
#define SETTINGS_AVOFF_BOUNDSMODE 2				;1 byte
#define SETTINGS_AVOFF_BGCOLOR	3				;2 bytes
#define SETTINGS_AVOFF_COLOR	5				;1 byte
#define SETTINGS_AVOFF_EQSWAPPED	6			;1 byte  - Used to swap Y<>Z equations
#define SETTINGS_AVOFF_XDIM		7				;1 byte
#define SETTINGS_AVOFF_YDIM		8				;1 byte
#define SETTINGS_AVOFF_SCALEF	9				;2 bytes
#define SETTINGS_AVOFF_ZOOMF	11				;2 bytes
#define SETTINGS_AVOFF_MINX		13				;2 bytes
#define SETTINGS_AVOFF_MINY		15				;2 bytes
#define SETTINGS_AVOFF_MINZ		17				;2 bytes
#define SETTINGS_AVOFF_MAXX		19				;2 bytes
#define SETTINGS_AVOFF_MAXY		21				;2 bytes
#define SETTINGS_AVOFF_MAXZ		23				;2 bytes
#define SETTINGS_HOOKBACK_WIN	25				;4 bytes  - WindowHook backup
#define SETTINGS_HOOKBACK_YEQU	29				;4 bytes  - YEquHook backup
#define SETTINGS_HOOKBACK_CUR	33				;4 bytes  - CursorHook backup
#define SETTINGS_MONVECBACK		37				;13 bytes - Monitor vector backup
#define SETTINGS_HOOKBACK_APP	50				;4 bytes  - AppChangeHook backup
#define SETTINGS_HOOKBACK_MENU	54				;4 bytes  - MenuHook backup
#define SETTINGS_HOOKBACK_REGR	58				;4 bytes  - GraphHook backup
#define SETTINGS_HOOKBACK_KEY	62				;4 bytes  - KeyHook backup
#define SETTINGS_HOOKBACK_REDISP 66				;4 bytes  - cxRedispHook backup
#define SETTINGS_AVOFF_MAXEQS	70				;1 byte
#define SETTINGS_AVOFF_TRACE	71				;1 byte   - 1 if tracing, 0 otherwise
#define SETTINGS_AVOFF_LABEL	72				;1 byte 
#define SETTINGS_AVOFF_CXCUR	73				;1 byte   - current context, used for when we need to rename the current mode
#define SETTINGS_AVOFF_CHKSM	74				;2 bytes  - checksum of data on trash RAM page
#define SETTINGS_AVOFF_MINXOS	76				;9 bytes
#define SETTINGS_AVOFF_MAXXOS	85				;9 bytes
#define SETTINGS_AVOFF_MINYOS	94				;9 bytes
#define SETTINGS_AVOFF_MAXYOS	103				;9 bytes
#define SETTINGS_AVOFF_MENUTRIG 112				;1 byte   - Whether a menu was triggered
#define SETTINGS_AVOFF_ERRGOTOPEND 113			;1 byte   - Whether an ERR:GOTO goto is pending.
#define SETTINGS_AVOFF_PENDEQ	114				;1 byte   - Which equation is pending
#define SETTINGS_AVOFF_ANGMODE	115				;1 byte   - Angle mode (0 = rad, 1 = deg)

; Used for the menu table
#define MT_TEXT		0
#define MT_OPTION	1
#define MT_OPTIONW	2

capacity_3d_el = sMAX(MAX_EQS * MAX_XY_RES * MAX_XY_RES, MAX_EQS_HI * MAX_XY_RES_HI * MAX_XY_RES_HI)
capacity_2d_el = sMAX(MAX_XY_RES * MAX_XY_RES, MAX_XY_RES_HI * MAX_XY_RES_HI)

; "Dynamic" allocation for graph-drawing data
#define TRASHABLE_RAM_PAGE 6							;0-7
trash_ram_loc	.equ	$C000
grid_x			.equ	trash_ram_loc + 0
grid_y			.equ	grid_x + (capacity_3d_el * 2)
grid_z			.equ	grid_y + (capacity_3d_el * 2)
axes_x			.equ 	grid_z + (capacity_3d_el * 2)
axes_y			.equ	axes_x + (2*AXES_BOUND_COORDS)
axes_z			.equ	axes_y + (2*AXES_BOUND_COORDS)
grid_colors		.equ	axes_z + (2*AXES_BOUND_COORDS)
grid_sx			.equ	grid_colors + (capacity_3d_el * 2)
grid_sy			.equ	grid_sx + (capacity_2d_el * 2)
axes_sx			.equ	grid_sy + (capacity_2d_el * 2)
axes_sy			.equ	axes_sx + (AXES_BOUND_COORDS*2)
trash_ram_end	.equ	axes_sy + (AXES_BOUND_COORDS*2)
trash_ram_fill	.equ	(trash_ram_end - trash_ram_loc)
trash_ram_fill_max .equ	$4000-$200
.echo "Trash RAM page has ", trash_ram_fill, "/", trash_ram_fill_max , " bytes allocated\n"
.if trash_ram_fill > trash_ram_fill_max
.fail "Trash RAM page has overflowed!"
.endif

; Reuse
#define TRACE_COORD_RAM_PAGE 7							;0-7
trace_coord_back	.equ	$C000
trace_coord_back_fill_max .equ	$4000-$200
.echo "trace_coord_back is ", ((trace_coord_back + trace_coord_back_fill_max - trace_coord_back) / 2), " pixels\n"

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
cxRedispHookPtr	.equ	$9E9D
appChangeHookActive .equ 2
yEquHookActive	.equ 	4		;1 = Y= hook active
MenuHookActive	.equ	6
GraphHookActive	.equ	3
traceHookActive	.equ	0					; In hookflags4
cxRedispHookActive .equ 5					; In hookflags4
cmdFlags	.equ	0Ch
cmdGoto			equ 7		;1=currently in the middle of a 2:Goto for an error
getCSCHookActive	equ 0		;1 = GetCSC hook active (in hookflags2)

; OS Equates - Other
APIFlg			equ 28h
appAllowContext		equ 0           ;App wants context changes to happen
monQueue		.equ	$8669
PlotEnabled1	.equ	$9812
PlotEnabled2	.equ	$9824
PlotEnabled3	.equ	$9836
maybe_menuGraphicalID .equ	$9d82	;Not currently used
menuButtonNum	.equ	$9d83
_ClearAppTitle	.equ	$56B3		;5056h
_RstrShadow		.equ	$4570
_maybe_MonRestart .equ	$4fba
_DrawSplitDivider .equ	$4855
mCalculate		.equ 	11h
mZoom			.equ	04h
mZoom3D			.equ	94h
fastSpeed		.equ	5
speedFlags		.equ	24h
_GetBytePaged	.equ	_LoadBIndPaged
tZ1				.equ	tY0+1
extraIndicFlags	.equ	$3E
saIndicForce	.equ	2
indicFlags		.equ 	12h	;Indicator flags
cxError			.equ	$fb
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
	jr z,ProgramStart_HookBackupNoMenu

	; Back up the current menuHook
	push bc
		ld a,SETTINGS_HOOKBACK_MENU
		call LTS_GetPtr						;to hl
		ld de,MenuHookPtr
		ex de,hl
		ld bc,3
		ldir
		ld a,(flags + hookflags4)			; contains MenuHookActive
		and 1 << MenuHookActive
		ld (de),a
		pop bc

ProgramStart_HookBackupNoMenu:
	ld a,(regraphHookPtr+2)
	cp b
	jr z,ProgramStart_HookBackupNoRegraph

	; Back up the current regraphHook
	ld a,SETTINGS_HOOKBACK_MENU
	call LTS_GetPtr						;to hl
	ld de,regraphHookPtr
	ex de,hl
	ld bc,3
	ldir
	ld a,(flags + hookflags3)			; contains MenuHookActive
	and 1 << regraphHookActive
	ld (de),a
	
ProgramStart_HookBackupNoRegraph:
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
		call DisplayNormal
		ld de,14
		ld hl,40
		ld ix,bigicon
		call DrawSprite_4Bit_Enlarge
		call DisplayOrg
		ld hl,1+(8*256)
		ld (currow),hl
		ld hl,sG3DCName
		call PutsApp						; Display App name
		ld hl,98
		ld (pencol),hl
		ld a,76
		ld (penrow),a
		ld hl,sG3DCDesc
		call VPutsApp						; Display small description text below
		ld hl,98
		ld (pencol),hl
		ld a,88
		ld (penrow),a
		ld hl,sAuthor
		call VPutsApp						; Display small description text below
		ld b,3
		ld a,201
		ld (penrow),a
		ld hl,sCopyright
ProgramStart_BottomTextLoop:
		ld de,1
		ld (pencol),de
		push bc
			call VPutsApp
			pop bc
		ld a,(penrow)
		add a,12
		ld (penrow),a
		djnz ProgramStart_BottomTextLoop
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
	ld a,SETTINGS_AVOFF_MODE
	call LTS_GetPtr
	ld (hl),0

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

	; Restore the reGraphHook area
	ld de,reGraphHookPtr+2
	ld bc,($ff^(1 << reGraphHookActive))*256 + SETTINGS_HOOKBACK_REGR
	ld hl,flags + hookflags3
	call DisableHook

	jr ProgramStart_Quit

ProgramStart_Install:
	call InitZEquations

	; Set up the appChangeHook
	res 1,(iy+$3A)							;????? Reverse-engineered from Transform & the TI-OS for something
	call GetCurrentPage
	ld hl,appChangeHook						;the ACTUAL appChange hook.
	bcall(_SetAppChangeHook)

	; Set up new menuHook
	call GetCurrentPage
	ld hl,MenuHook
	bcall(_SetMenuHook)

	; Set up new regraphHook. We use a custom App
	; context for the actual graph context, but the
	; OS uses the regraphhook in splitscreen mode to draw
	; a graph, and we want to hook it. It does NOT seem
	; to use the graphhook, which is a pain in the neck.
	call GetCurrentPage
	ld hl,SplitscreenGraphHook
	bcall(_SetRegraphHook)

ProgramStart_Quit:
	; Cleanup/quit
	call DisplayOrg
	bjump(_JForceCmdNoChar)

; b is current app, a is new app
appChangeHook:
	.db $83

	push af
		push bc
			push hl
				; The following is necessary to avoid erasing our Yequ hooks when going from
				; the Graph or Trace contexts, through an Error context, back to Y=.
				cp cxError
				jr z,AppChangeHook_Done
			
				; Get the current mode so we know if we can start triggering our stuff
				ld c,a										; b is current app, c is new app
				push bc
					call LTS_CacheAV
					call CleanTempHooks					; Clean up Yequ, Zoom, Window hooks, if they're ours
					call SwapZYFuncs_Out				; Make sure all the equations are properly sorted
					call setWindow_OS					; In some places we modify the window
					xor a
					call SetCXCur
					ld a,SETTINGS_AVOFF_MENUTRIG
					call LTS_GetPtr
					ld (hl),0

					ld a,SETTINGS_AVOFF_MODE
					call LTS_GetByte
					ld d,a
					or a
					jr z,appChangeHook_NoSplitOverride
					set grfSChanged,(iy+sgrFlags)
appChangeHook_NoSplitOverride:
					pop bc
				; b is current app, c is new app, a and d are mode

				ld a,c
				cp kYequ
				jr nz,appChangeHook_CheckMode
				call SetCXCur

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
				cp kTable
				jr z,appChangeHook_GoGraph			; No Table context in 3D mode
				cp kGraph
				jr nz,appChangeHook_CheckFormat
appChangeHook_GoGraph:
				push bc
					ld a,SETTINGS_AVOFF_TRACE
					call LTS_GetPtr					;to hl
					pop af
				ld (hl),a							;Whether tracing is enabled
				ld a,kGraph
				call SetCXCur
				
				; Set up the context
				bcall(_PutAway)
				ld hl,GraphCxVectors
				bcall(_AppInit)
				ld a,kExtApps						;If we leave cxGraph, the OS tries to draw stuff on [TRACE]
				ld (cxCurApp),a
				;res curAble,(iy+curFlags)
				
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
				
				; Compute graph and display initial version
				call cxInit_3DGraph
				bcall(_Redisp)

				; Let the OS take over now and call our cxMain when necessary
				bjump(_Mon)

appChangeHook_CheckFormat:
				cp kFormat
				jr nz,appChangeHook_Invalid
				call SetCXCur

				; Set up the context
				bcall(_PutAway)
				ld hl,FormatCxVectors
				bcall(_AppInit)
				ld a,kExtApps						;If we leave cxFormat, the OS tries to draw stuff on menu -> [clear]
				ld (cxCurApp),a

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
				call GetCurrentPage
				ld hl,formatCursorHook
				bcall(_SetCursorHook)

				; Compute graph and display initial version
				call cxInit_Format
				bcall(_Redisp)

				; Let the OS take over now and call our cxMain when necessary
				bjump(_Mon)

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
	;ld de,GraphHookPtr+2
	;ld bc,($ff^(1 << GraphHookActive))*256 + SETTINGS_HOOKBACK_GRPH
	;ld hl,flags + hookflags3
	;call DisableHook
	
	;GetKeyHook
	;ld de,GetKeyHookPtr+2
	;ld bc,($ff^(1 << GetCSCHookActive))*256 + SETTINGS_HOOKBACK_KEY
	;ld hl,flags + hookflags2
	;call DisableHook

	;CursorHook
	ld de,CursorHookPtr+2
	ld bc,($ff^(1 << cursorHookActive))*256 + SETTINGS_HOOKBACK_CUR
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

SetCXCur:
	push af
		ld a,SETTINGS_AVOFF_CXCUR
		call LTS_GetPtr
		pop af
	ld (hl),a
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
						call OP1SwapOP6_Safe
						ld hl,AVName
						rst 20h
						bcall(_chkfindsym)
						call OP1SwapOP6_Safe
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
					ld (OP6),de
					ld a,b
					ld (OP6+2),a
					B_CALL(_GetBytePaged)
					ld a,b
					cp 83h
					jr nz,HookChainer_Continue
					pop bc
				pop af
			pop de
		pop hl
	ex (sp),hl
	bcall(OP6 | (1 << 14))
	ret

HookChainer_Continue:
					pop bc
				pop af
			pop de
		pop hl
	ex (sp),hl
	ret

#include "g3dcmath.asm"
#include "utils.asm"
#include "ltstore.asm"
#include "winhook.asm"
#include "yequhook.asm"
#include "menuhook.asm"
#include "graphhook.asm"
#include "graphfuncs.asm"
#include "tracefuncs.asm"
#include "formathook.asm"
#include "multimenu.asm"
#include "funcfuncs.asm"
#include "bigicon.inc"
#include "data.inc"

;------------------------------------------------------------------------------- SPRITES

Page0End:
.echo "Page 0 has ", (Page0End-Page0Start), "/16384 bytes allocated\n"

.end