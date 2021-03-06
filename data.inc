AxesBoundsX:
	.dw max_x, min_x, sub_max_x, sub_max_x
	.dw val_zero, val_zero, sub_min_x, negsub_min_x
	.dw val_zero, val_zero, val_zero, val_zero
	.dw max_x, min_x, max_x, max_x
	.dw min_x, max_x, min_x, min_x
	.dw sup_max_x, val_zero, val_zero
AxesBoundsY:
	.dw val_zero, val_zero, sub_min_y, negsub_min_y
	.dw max_y, min_y, sub_max_y, sub_max_y
	.dw val_zero, val_zero, sub_min_y, negsub_min_y
	.dw max_y, max_y, min_y, max_y
	.dw min_y, min_y, max_y, min_y
	.dw val_zero, sup_max_y, val_zero
AxesBoundZ:
	.dw val_zero, val_zero, val_zero, val_zero
	.dw val_zero, val_zero, val_zero, val_zero
	.dw val_max_z, val_min_z, sub_max_z, sub_max_z
	.dw val_max_z, val_max_z, val_max_z, val_min_z
	.dw val_min_z, val_min_z, val_min_z, val_max_z
	.dw val_zero, val_zero, sup_max_z
Offsets_Axes:
	.db 0,1,0,2,0,3
	.db 4,5,4,6,4,7
	.db 8,9,8,10,8,11
Offsets_Axes_End:
Offsets_Bounds:
	.db 12,13,12,14,12,15
	.db 16,17,16,18,16,19
	.db 14,17,15,17,13,18
	.db 15,18,13,19,14,19
Offsets_Bounds_End:
Offsets_AxesMax:
	.db 0,20,4,21,8,22				; Offset of max x-axis tip, max y-axis tip, max z-axis tip
Offsets_AxesMax_End:
sString_Axes:
	.db " x",0,0," y",0,0," z",0,0	; *must* correspond to (Offsets_AxesMax >> 1)
sString_Axes_End:

; Sanity-checking on the values above
.if (Offsets_Axes_End - Offsets_Axes) != (2*AXES_BOUND_PAIRS_AXES)
.fail "Offsets_Axes contains wrong data, or AXES_BOUND_PAIRS_AXES is outdated"
.endif
.if (Offsets_Bounds_End - Offsets_Bounds) != (2*AXES_BOUND_PAIRS_BOUNDS)
.fail "Offsets_Bounds contains wrong data, or AXES_BOUND_PAIRS_BOUNDS is outdated"
.endif
.if (Offsets_AxesMax_End - Offsets_AxesMax) != 2*AXES_BOUND_PAIRS_LABELS
.fail "Offsets_AxesMax contains wrong data, or AXES_BOUND_PAIRS_LABELS is outdated"
.endif


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
	.db SETTINGS_AVOFF_MINX
	.db SETTINGS_AVOFF_MAXX
	.db SETTINGS_AVOFF_XDIM
	.db SETTINGS_AVOFF_MINY
	.db SETTINGS_AVOFF_MAXY
	.db SETTINGS_AVOFF_YDIM
	.db SETTINGS_AVOFF_ZOOMF
	
Window_Field_Table_OS:
	.db SETTINGS_AVOFF_MINXOS
	.db SETTINGS_AVOFF_MAXXOS
	.db SETTINGS_AVOFF_XDIM
	.db SETTINGS_AVOFF_MINYOS
	.db SETTINGS_AVOFF_MAXYOS
	.db SETTINGS_AVOFF_YDIM
	.db SETTINGS_AVOFF_ZOOMF

WindowAutoScale_BoundLUT:
	.db SETTINGS_AVOFF_MINXOS,SETTINGS_AVOFF_MINX
	.db SETTINGS_AVOFF_MINYOS,SETTINGS_AVOFF_MINY
	.db SETTINGS_AVOFF_MAXXOS,SETTINGS_AVOFF_MAXX
	.db SETTINGS_AVOFF_MAXYOS,SETTINGS_AVOFF_MAXY

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

sG3DCName:
	.db "Graph3DC 1.0",0
sG3DCDesc:
	.db "3D Graphing App",0

sMMenu1a:
	.db "1. Install App         ",0
sMMenu2a:
	.db "2. Cancel              ",0

sMMenu1b:
	.db "1. Reinstall/Repair App",0
sMMenu2b:
	.db "2. Uninstall App       ",0
sMMenu3b:
	.db "3. Cancel              ",0

MMenu_JTable:
	.dw sMMenu1a, sMMenu2a
	.dw sMMenu1b, sMMenu2b, sMMenu3b

GridIcon_Table:
	.dw GridIcon_Spectrum
	.dw GridIcon_Blues
	.dw GridIcon_Autumn
	
GridIcon_Spectrum_Palette:
	.db $ff,$ff,$f8,$0a,$f8,$12,$f8,$c3,$f8,$18,$e0,$1f
	.db $58,$3f,$02,$bf,$07,$fa,$07,$f2,$05,$3f,$07,$9f
	.db $2f,$e4,$af,$e0,$ef,$a0,$fd,$20
GridIcon_Spectrum:
	.dw GridIcon_Spectrum_Palette
	.db 20,12											;Image dimensions, for the sprite routine
	.db $00,$00,$01,$20,$00,$00,$00,$00,$00,$00
	.db $00,$03,$30,$45,$66,$67,$00,$89,$00,$00
	.db $00,$03,$02,$50,$06,$00,$ab,$09,$00,$00
	.db $00,$31,$20,$56,$66,$7a,$08,$9c,$c0,$00
	.db $00,$30,$05,$00,$06,$00,$b9,$00,$c0,$00
	.db $03,$12,$45,$66,$67,$ab,$89,$cc,$cd,$00
	.db $03,$00,$05,$00,$0a,$00,$9c,$cc,$0d,$00
	.db $31,$20,$50,$66,$7a,$b8,$0c,$cc,$de,$f0
	.db $10,$05,$66,$00,$0b,$09,$c0,$00,$ee,$f0
	.db $10,$00,$60,$00,$ab,$80,$00,$00,$00,$30
	.db $04,$50,$60,$7a,$00,$00,$00,$00,$00,$00
	.db $00,$06,$66,$00,$00,$00,$00,$00,$00,$00

GridIcon_Blues_Palette:
	.db $ff,$ff,$03,$bf,$04,$5f,$03,$3f,$05,$1f,$05,$9f
	.db $0e,$5f,$07,$1f,$07,$fb,$07,$f9,$07,$fe,$07,$df
	.db $07,$f5,$07,$f3,$07,$ef,$07,$ea
GridIcon_Blues:
	.dw GridIcon_Blues_Palette
	.db 20,12											;Image dimensions, for the sprite routine
	.db $00,$00,$01,$20,$00,$00,$00,$00,$00,$00
	.db $00,$03,$10,$44,$56,$77,$00,$89,$00,$00
	.db $00,$01,$02,$40,$06,$00,$a8,$09,$00,$00
	.db $00,$31,$20,$55,$67,$ba,$08,$9c,$d0,$00
	.db $00,$10,$04,$00,$07,$00,$89,$00,$d0,$00
	.db $03,$12,$45,$66,$7b,$a8,$99,$cd,$de,$00
	.db $01,$00,$05,$00,$0a,$00,$9c,$dd,$0e,$00
	.db $12,$40,$50,$77,$ba,$89,$0c,$de,$ef,$f0
	.db $10,$05,$66,$00,$08,$09,$c0,$00,$ef,$f0
	.db $20,$00,$60,$00,$a8,$90,$00,$00,$00,$f0
	.db $04,$50,$70,$ba,$00,$00,$00,$00,$00,$00
	.db $00,$06,$7b,$00,$00,$00,$00,$00,$00,$00
	
GridIcon_Autumn_Palette:
	.db $ff,$ff,$ff,$e0,$ff,$60,$ff,$20,$fe,$c0,$fe,$40
	.db $fd,$e0,$fd,$80,$fd,$00,$fb,$40,$fc,$60,$fb,$c0
	.db $fa,$c0,$fa,$40,$f9,$80,$f8,$60,$
GridIcon_Autumn:
	.dw GridIcon_Autumn_Palette
	.db 20,12											;Image dimensions, for the sprite routine
	.db $00,$00,$01,$20,$00,$00,$00,$00,$00,$00
	.db $00,$01,$10,$34,$56,$78,$00,$99,$00,$00
	.db $00,$01,$02,$30,$07,$00,$ab,$0c,$00,$00
	.db $00,$11,$20,$45,$68,$8a,$09,$9c,$d0,$00
	.db $00,$10,$04,$00,$08,$00,$b9,$00,$e0,$00
	.db $01,$12,$34,$56,$88,$ab,$99,$cd,$ef,$00
	.db $01,$00,$05,$00,$0a,$00,$9c,$de,$0f,$00
	.db $11,$20,$40,$78,$8a,$b9,$0d,$de,$ff,$f0
	.db $10,$04,$56,$00,$0b,$09,$c0,$00,$ff,$f0
	.db $10,$00,$50,$00,$ab,$90,$00,$00,$00,$f0
	.db $03,$40,$60,$8a,$00,$00,$00,$00,$00,$00
	.db $00,$06,$78,$00,$00,$00,$00,$00,$00,$00

BaseZVarName:
	.db EquObj, tVarEqu, tZ1, 0

sTotalProgress:
;   .db "12345678901234567890123456"
	.db "Total: ",$C1,"                 ]",0
sPartialProgress:
	.db "Z",$81,":    ",$C1,"                 ]",0
sXEqu:
	.db "X=",0
sYEqu:
	.db "Y=",0
sZEqu:
	.db "Z=",0
TraceCursor:
	.db 8,8
	.db %00111000
	.db %01111100
	.db %11000110
	.db %11000110
	.db %11000110
	.db %01111100
	.db %00111000
	.db %00000000

FormatTable:
	.db 3
	.db MT_TEXT,"Axes:",0
	.db MT_OPTION,SETTINGS_AVOFF_AXISMODE,1,"On",0
	.db MT_OPTION,SETTINGS_AVOFF_AXISMODE,0,"Off",0
	.db 3
	.db MT_TEXT,"Bounds:",0
	.db MT_OPTION,SETTINGS_AVOFF_BOUNDSMODE,1,"On",0
	.db MT_OPTION,SETTINGS_AVOFF_BOUNDSMODE,0,"Off",0
	.db 4
	.db MT_TEXT,"Color:",0
	.db MT_OPTION,SETTINGS_AVOFF_COLOR,0,"Full",0
	.db MT_OPTION,SETTINGS_AVOFF_COLOR,1,"Blues",0
	.db MT_OPTION,SETTINGS_AVOFF_COLOR,2,"Flame",0
	.db 3
	.db MT_TEXT,"Background:",0
	.db MT_OPTIONW,SETTINGS_AVOFF_BGCOLOR,$ff,$ff,"White",0
	.db MT_OPTIONW,SETTINGS_AVOFF_BGCOLOR,$00,$00,"Black",0
	.db 3
	.db MT_TEXT,"Resolution:",0
	.db MT_OPTION,SETTINGS_AVOFF_MAXEQS,5,"Normal",0
	.db MT_OPTION,SETTINGS_AVOFF_MAXEQS,2,"High",0
	.db 3
	.db MT_TEXT,"Label:",0
	.db MT_OPTION,SETTINGS_AVOFF_LABEL,0,"Off",0
	.db MT_OPTION,SETTINGS_AVOFF_LABEL,1,"On",0
	.db 0

sAuthor:
	.db "by Christopher Mitchell",0

sCopyright:
	.db "(c) 2012-2015 Cemetech",0
sWebsite:
	.db "http://www.cemetech.net",0
sBuildNum:
	.db "Version 1.0.0 ",$7C," "
	.db "Build "
.incbin "buildnum.txt"
	.db 0

sYequExplain:
	.db "Use X and Y as indep vars",0
sGraphExplain:
	.db "Precomputing X,Y,Z values",0
	
MinXYDefault:
	.db $80,$80,(DEFAULT_XY_MAX/INT_TO_8P8) * 16 + 0, 0, 0, 0, 0, 0, 0
MaxXYDefault:
	.db $00,$80,(DEFAULT_XY_MAX/INT_TO_8P8) * 16 + 0, 0, 0, 0, 0, 0, 0

ZOOM_MENU_CHOICES	.equ 3
ZoomMenuTable:
	.db 1	;how many headers?
	.db ZOOM_MENU_CHOICES 	;how many choices under 1st header?
	.db 5Eh	;string table entry for "ZOOM"
ZoomMenuTable_Entries:
	.db 0,kZIn
	.db 0,kZOut	;these are 2-byte keypresses of each entry
	.db 0,kStd
ZoomMenuTableEnd:

CALC_MENU_CHOICES	.equ 1
CalcMenuTable:
	.db 1	;how many headers?
	.db CALC_MENU_CHOICES 	;how many choices under 1st header?
	.db 5Eh	;string table entry for "ZOOM"
CalcMenuTable_Entries:
	.db 0,kQuit
CalcMenuTableEnd:

sCalcDisabled:
	.db "Calc Menu Disabled",0