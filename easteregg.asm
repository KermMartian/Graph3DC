EasterEgg:
	call DisplayNormal
	ld de,14
	ld hl,40
	ld ix,egg_Sprite
	call DrawSprite_8Bit
	call DisplayOrg
	ld hl,68
	ld (pencol),hl
	ld a,39
	ld (penrow),a
	ld hl,egg_text1
	call VPutsApp						; Display small description text below
	ld hl,68
	ld (pencol),hl
	ld a,54
	ld (penrow),a
	ld hl,egg_text2
	call VPutsApp						; Display small description text below
	ret

egg_text1:
	.db "Thanks for using Graph3DC!",0
egg_text2:
	.db "-- Christopher \"Kerm\" Mitchell",0

egg_palette:
	.db $ac,$6c,$ac,$8b,$b4,$ad,$b4,$ac,$bc,$cd,$7a,$e7,$59,$e5,$5a,$05,$49,$84,$51,$c5,$72,$66,$8b,$28,$59,$c4,$6a,$25,$72,$65,$a4,$0a,$a3,$e9,$93,$cb,$ac,$ce,$52,$05,$39,$43,$6a,$46,$7a,$a8,$39,$02,$8b,$48,$ac,$4b,$7b,$6c,$9c,$6f,$ac,$d1,$b5,$72,$41,$23,$8a,$c7,$82,$c7,$51,$83,$9b,$ca,$d6,$9a,$c6,$18,$bd,$75,$a5,$13,$8b,$ed,$83,$8c,$83,$6b,$30,$e3,$51,$84,$7a,$66,$82,$86,$61,$c4,$72,$26,$7a,$45,$9b,$28,$8a,$c6,$a3,$aa,$f7,$ff,$f7,$bf,$e7,$3d,$8c,$50,$8b,$cc,$62,$67,$9b,$07,$92,$a7,$ab,$6a,$b4,$0b,$c4,$af,$dd,$50,$cc,$ad,$c4,$4c,$93,$27,$ff,$ff,$52,$27,$41,$85,$82,$67,$9b,$08,$b3,$a9,$f6,$97,$f6,$75,$f6,$54,$ed,$b1,$b5,$76,$41,$86,$59,$a5,$ab,$69,$c4,$0b,$dc,$6c,$ed,$91,$f6,$13,$ab,$a9,$9c,$f6,$41,$c8,$41,$64,$82,$87,$dc,$ae,$e4,$ee,$f6,$34,$dd,$4f,$21,$05,$59,$e6,$cc,$6d,$ed,$2f,$f6,$14,$bc,$4b,$84,$32,$d5,$50,$63,$4f,$41,$66,$93,$47,$4a,$8c,$bc,$0c,$ed,$92,$ac,$6a,$41,$ea,$39,$25,$bb,$69,$cc,$0b,$a3,$ec,$7a,$26,$69,$c5,$71,$a4,$82,$25,$aa,$c6,$b3,$68,$6a,$48,$7a,$a9,$92,$86,$a2,$e7,$ff,$de,$a4,$11,$59,$e8,$31,$44,$9a,$e6,$8a,$66,$82,$6c,$93,$29,$51,$23,$9a,$65,$41,$84,$8a,$89,$c3,$88,$a2,$c8,$b3,$69,$c4,$ad,$ac,$2b,$b4,$d2,$93,$09,$d3,$ea,$b4,$ab,$cd,$76,$dc,$6d,$dc,$4c,$cb,$ca,$c4,$0a,$d5,$f8,$b3,$49,$ed,$71,$be,$1a,$ad,$b8,$d6,$bc,$9b,$2c,$dd,$0e,$3a,$0c,$52,$ee,$4a,$6d,$73,$d3,$69,$a4,$92,$26,$cc,$ac,$63,$50,$21,$48,$29,$89,$21,$27,$29,$ab,$93,$8d,$92,$87,$7a,$ea,$b5,$98,$74,$15,$6c,$16,$4b,$14,$a4,$50,$a5,$dc,$be,$9e,$ae,$3e,$79,$a5,$89,$e6,$ef,$df,$62,$89,$5a,$29,$94,$b6,$92,$06,$3a,$2a,$6a,$cb,$84,$d4,$bc,$6c,$6a,$ed,$39,$a7,$31,$86,$6b,$0f,$29,$66,$00,$84,$18,$c4,$29,$25,$18,$a3,$6b,$2b,$52,$69,$52,$68,$18,$e3,$41,$e8,$5a,$a9,$39,$a6,$62,$c9,$41,$e6,$29,$45,$18,$c3,$29,$04,$5a,$88,$21,$24,$53,$2c,$20,$e3,$10,$82,$29,$23,$73,$09,$4b,$67,$31,$85,$18,$a2,$39,$84,$41,$c5,$b5,$11,$ad,$f6,$3a,$46,$18,$61,$08,$41,$10,$61,$ec,$ef,$20,$c2,$18,$c2,$29,$03,$c3,$28,$89,$e4,$08,$20,$8a,$85,$30,$61,$48,$c2,$50,$a2,$72,$03,$61,$a2,$7a,$65
egg_sprite:
	.dw egg_palette
	.db 48,59											;Image dimensions, for the sprite routine
	.db 0,0,0,0,0,0,1,2,2,0,2,2,2,3,3,3,3,4,4,4,4,4,2,5,6,7,8,8,9,6,6,6,10,11,6,9,12,12,12,12,13,14,15,16,16,16,16,16
	.db 17,0,18,18,18,18,18,2,2,2,2,2,2,3,3,3,3,4,4,4,4,4,17,19,9,8,20,8,21,21,8,9,11,22,13,10,12,8,23,23,12,13,24,25,15,16,16,16
	.db 26,26,27,28,27,29,29,17,0,0,0,2,2,2,2,4,4,4,4,4,4,2,22,20,20,30,9,13,13,6,9,6,11,31,32,10,13,12,33,8,33,13,32,34,15,15,16,16
	.db 35,36,37,37,38,38,38,39,40,40,41,41,17,17,17,0,0,3,3,4,2,17,19,20,42,43,44,45,46,47,12,45,48,49,49,31,50,31,14,12,8,13,10,51,17,15,15,15
	.db 52,53,53,54,54,54,35,36,36,37,38,38,55,39,39,39,56,17,17,17,17,57,19,20,8,46,58,58,58,59,60,61,62,63,64,64,64,65,65,66,46,12,12,11,34,25,15,15
	.db 67,67,67,67,67,52,52,52,53,53,53,54,54,35,35,36,37,37,28,27,68,69,9,6,45,70,71,58,72,72,64,73,73,73,73,74,74,75,75,76,61,12,33,32,34,25,15,15
	.db 67,67,67,67,67,67,67,67,67,67,67,67,52,52,52,52,53,53,53,77,78,20,79,49,80,71,72,81,82,82,83,74,73,73,73,73,74,74,74,75,84,85,12,5,34,25,15,15
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,52,86,87,88,89,65,51,72,90,91,82,91,83,76,92,92,92,92,75,75,75,75,75,93,50,32,34,0,25,16
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,52,86,94,95,49,64,96,82,90,91,97,97,83,98,92,92,98,98,98,84,84,75,75,84,99,50,34,0,15,16
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,52,100,94,95,49,65,96,90,82,63,97,83,98,92,74,92,74,92,75,84,84,84,84,84,101,66,32,34,34,15
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,54,102,103,95,49,65,81,90,97,63,83,83,83,98,73,73,73,73,74,75,98,84,84,84,93,104,13,24,25,25
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,54,105,103,95,59,106,96,63,63,83,83,97,82,91,83,107,83,83,83,84,75,75,84,84,93,50,10,24,108,25
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,54,109,110,95,49,106,106,60,60,60,60,60,80,111,112,82,82,82,82,97,84,75,75,84,93,31,12,5,108,108
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,54,109,78,95,113,51,89,44,89,70,114,115,116,117,118,111,119,119,58,80,61,63,76,76,93,32,7,24,1,108
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,54,109,110,120,121,95,89,44,47,6,6,6,46,116,117,122,123,123,122,117,59,80,72,64,64,14,7,24,1,25
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,124,125,126,103,121,51,95,47,79,88,127,127,88,43,43,117,58,128,123,70,114,129,58,80,99,65,13,7,34,1,25
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,124,130,120,95,131,81,89,70,47,115,115,79,132,43,114,23,133,114,46,79,134,19,10,49,11,22,12,7,15,1,25
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,124,130,135,120,51,96,131,61,71,70,114,115,115,114,59,115,136,114,44,115,79,89,89,60,51,57,19,5,3,1,25
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,124,125,135,135,51,96,90,106,61,106,137,137,123,138,47,96,73,106,59,59,59,80,61,64,139,140,21,61,4,108,108
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,124,141,135,142,60,81,90,91,97,90,90,96,81,59,138,91,73,107,71,119,138,80,96,93,101,34,59,143,144,108,108
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,124,145,135,142,49,106,96,90,91,146,82,147,148,111,82,91,92,83,82,81,96,91,83,76,101,51,72,149,1,1,1
	.db 67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,150,135,142,131,71,138,138,138,148,151,122,123,146,82,91,152,83,97,91,82,90,93,93,93,93,149,139,1,1,1
	.db 67,67,67,53,153,154,154,154,153,153,153,153,155,155,155,155,67,67,54,156,142,131,89,70,59,137,137,114,116,114,123,138,90,91,152,90,111,111,82,97,83,76,157,112,3,108,108,1
	.db 52,67,52,86,158,158,158,158,102,100,102,102,159,160,160,160,161,52,124,145,156,131,89,114,89,70,114,114,114,115,162,116,137,138,81,138,123,163,111,82,157,93,164,112,144,108,108,108
	.db 86,52,54,165,166,166,166,166,167,167,166,166,166,166,168,166,169,153,67,124,35,170,89,44,70,114,115,114,114,114,115,115,70,59,137,137,138,171,122,149,82,164,157,64,3,1,1,1
	.db 172,173,54,159,169,174,175,175,175,175,174,175,174,176,176,169,166,173,67,67,54,177,89,89,70,114,115,114,70,70,114,114,171,171,137,60,80,71,171,72,99,164,164,2,3,1,1,1
	.db 121,125,155,105,158,178,179,179,180,180,180,180,180,178,179,160,166,173,67,67,53,177,89,89,70,70,115,30,162,162,114,181,182,171,137,151,60,71,122,72,99,164,139,3,3,1,1,1
	.db 120,172,173,105,160,155,52,183,183,183,183,52,52,52,52,159,166,153,67,67,67,141,22,44,70,70,114,132,43,48,114,70,59,137,137,114,162,129,128,99,164,4,3,3,3,1,1,1
	.db 95,184,86,105,160,155,52,183,183,183,183,183,183,183,52,159,166,153,67,67,67,37,121,44,45,129,117,114,181,114,45,49,140,66,80,116,117,122,72,99,164,3,3,3,3,1,1,1
	.db 103,185,186,105,160,155,52,183,183,183,183,183,183,183,52,159,168,153,67,67,67,37,121,44,45,129,129,114,182,181,181,181,182,181,187,123,119,119,99,99,139,3,3,3,144,108,108,108
	.db 103,185,86,188,160,155,52,53,183,183,183,183,183,183,52,105,168,153,67,67,53,177,89,89,89,70,129,129,114,114,182,182,187,137,151,81,119,149,99,99,3,3,3,3,1,108,108,1
	.db 103,189,153,188,105,54,52,53,52,183,52,183,183,183,52,105,168,155,155,190,102,172,89,44,44,70,122,58,117,114,114,114,171,138,72,72,72,99,99,191,3,3,3,3,1,1,1,25
	.db 110,192,155,193,105,155,52,52,52,52,52,67,67,67,67,105,168,102,167,168,194,22,31,47,13,44,58,123,58,171,117,171,123,119,72,72,99,99,191,3,3,3,3,1,1,1,1,25
	.db 184,195,77,94,196,188,188,188,188,188,188,105,105,105,105,196,94,94,197,94,78,22,31,70,46,46,45,71,60,171,59,171,138,119,72,99,61,191,3,3,3,3,3,1,1,1,25,25
	.db 28,173,37,198,196,196,196,196,199,94,199,199,94,94,199,199,198,198,200,199,199,21,31,70,47,79,12,114,70,70,59,59,58,80,81,61,61,3,3,3,3,1,1,1,1,108,108,25
	.db 201,54,36,198,189,39,202,55,68,26,26,203,39,202,26,203,198,204,204,204,204,69,45,45,45,47,46,79,46,47,44,70,71,60,51,51,64,3,3,3,3,1,1,1,1,25,25,25
	.db 94,86,36,198,205,202,194,206,194,203,203,207,208,209,208,209,200,204,204,200,204,204,6,45,45,44,47,47,46,46,47,45,31,80,85,64,140,34,3,3,3,1,1,1,1,25,25,0
	.db 94,193,102,198,188,105,199,207,204,199,199,200,207,208,210,211,200,204,204,200,200,200,212,13,45,45,45,44,47,47,47,70,71,80,65,64,24,213,0,0,3,1,1,1,1,25,0,29
	.db 199,194,214,200,215,215,199,207,216,210,214,194,26,208,40,212,200,216,200,200,200,200,217,42,13,45,45,45,44,44,47,70,71,81,65,65,5,218,213,219,17,34,0,1,1,0,29,36
	.db 214,194,210,211,220,220,218,194,200,210,214,221,39,26,39,212,217,211,217,217,217,200,222,217,212,13,31,31,45,89,10,44,71,61,65,61,19,218,223,224,68,213,219,41,17,225,226,226
	.db 221,127,218,222,227,227,214,210,200,214,214,214,203,26,209,211,228,95,40,212,217,217,217,217,217,212,6,49,32,32,32,31,49,61,65,11,218,127,127,223,209,68,68,68,203,201,55,190
	.db 214,205,56,69,194,221,214,68,217,209,221,127,209,207,68,211,42,82,107,57,229,217,217,217,230,217,211,69,22,31,31,49,51,61,51,127,218,127,127,127,224,209,68,68,68,68,203,206
	.db 92,90,231,142,216,216,216,200,222,216,211,232,218,212,211,233,8,143,143,9,217,217,230,230,230,222,222,211,234,21,31,49,51,51,19,211,127,127,218,127,223,224,68,68,209,209,209,68
	.db 91,235,118,115,216,216,211,209,229,221,218,214,221,222,221,222,43,118,118,6,229,230,217,230,230,230,222,222,222,216,9,22,11,13,233,218,127,127,211,218,223,223,209,209,209,209,209,209
	.db 82,236,162,232,222,222,216,217,217,217,222,222,222,216,216,216,23,133,119,113,217,230,230,230,230,230,222,222,222,222,211,127,30,234,211,218,127,127,211,216,223,224,209,209,209,224,207,224
	.db 107,163,69,230,218,216,230,216,229,216,222,211,233,68,28,209,232,117,80,64,209,229,230,230,230,230,222,222,222,222,222,216,211,211,218,218,127,127,222,216,218,223,209,209,209,223,207,207
	.db 146,146,71,232,216,211,127,209,217,207,221,127,221,209,26,223,229,116,58,49,127,230,230,230,230,230,217,222,222,211,222,216,216,127,209,218,218,127,222,222,218,223,223,203,27,68,223,223
	.db 118,112,60,232,218,222,201,225,212,225,40,201,39,208,39,223,230,46,59,6,237,230,230,230,230,230,217,222,222,222,222,216,216,127,209,218,217,218,222,230,218,127,223,41,29,40,127,221
	.db 236,133,115,230,216,230,206,39,210,39,208,184,208,203,39,127,230,31,32,216,230,230,230,230,230,230,230,222,222,222,230,230,233,218,224,218,237,222,230,230,218,221,221,203,56,213,221,221
	.db 58,117,12,230,211,237,39,225,207,225,39,40,40,208,39,127,228,71,19,230,222,230,230,230,230,230,230,222,222,230,229,229,222,218,223,207,237,228,228,229,218,221,221,127,224,221,221,221
	.db 96,238,33,222,127,222,207,68,216,184,209,207,224,203,39,218,232,51,216,222,211,230,230,230,230,222,230,222,217,217,229,229,230,216,209,209,211,239,239,229,216,221,223,221,127,127,221,221
	.db 146,80,33,230,211,229,39,225,127,225,40,40,39,208,27,234,232,19,216,222,211,217,230,230,230,230,217,230,230,230,229,229,217,216,208,127,216,240,239,229,233,221,221,223,223,127,221,221
	.db 133,122,33,230,216,211,206,201,216,201,68,203,206,206,39,223,217,218,216,222,222,222,230,230,230,230,217,237,230,222,230,229,230,221,184,216,216,241,239,229,222,127,221,221,221,221,221,127
	.db 119,72,33,230,211,229,40,27,214,225,208,201,208,68,39,216,230,211,218,211,217,222,217,230,230,230,222,230,230,222,230,229,222,217,205,218,222,240,240,217,222,127,221,223,221,221,221,127
	.db 76,76,10,237,237,237,201,39,210,28,208,26,201,237,237,237,230,237,216,216,216,222,217,222,230,230,217,230,237,230,222,237,230,230,216,210,211,241,240,217,230,127,221,223,223,223,221,127
	.db 83,75,62,234,237,237,218,216,218,233,218,216,230,229,229,233,218,222,230,216,216,216,211,222,217,230,230,222,217,230,230,230,230,230,216,212,216,241,240,237,237,218,127,223,221,221,221,127
	.db 149,98,74,62,10,33,9,8,23,23,23,23,23,8,9,21,127,127,222,222,216,218,218,216,211,222,230,222,230,230,237,232,222,222,233,232,214,240,240,230,237,218,127,221,221,223,221,127
	.db 122,82,76,75,76,72,238,50,242,243,242,48,48,31,85,22,127,127,212,211,216,218,218,214,216,211,230,217,217,230,230,127,218,216,233,232,216,241,132,222,237,218,127,221,223,221,221,127
	.db 48,128,149,76,84,76,99,128,238,242,242,244,50,31,11,69,127,127,218,218,211,216,216,216,216,216,222,230,222,222,237,212,209,218,233,232,222,241,132,211,237,216,127,221,221,221,221,127