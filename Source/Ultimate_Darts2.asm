
SONG_DARTS_DATA = ym_darts_len

ym_darts_len:
 dw 380
darts_ymregs:
 db $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $ff 
darts_ymdata:
 db $00, $00, $00, $00, $00, $00, $00, $3F, $00, $00, $00; 0
 db $00, $00, $00, $00, $00, $00, $00, $3F, $00, $00, $00; 1
 db $4E, $06, $86, $00, $94, $01, $12, $18, $0F, $0F, $0B; 2
 db $52, $06, $86, $00, $93, $01, $12, $18, $0F, $0F, $0A; 3
 db $56, $06, $43, $00, $94, $01, $12, $18, $0F, $0F, $0A; 4
 db $52, $06, $86, $00, $93, $01, $12, $18, $0F, $0F, $09; 5
 db $4E, $06, $43, $00, $68, $01, $00, $38, $0F, $0F, $0B; 6
 db $4A, $06, $86, $00, $67, $01, $00, $38, $0F, $0F, $0A; 7
 db $46, $06, $43, $00, $68, $01, $00, $38, $0F, $0F, $0A; 8
 db $42, $06, $86, $00, $67, $01, $00, $38, $0F, $0F, $09; 9
 db $CB, $00, $97, $00, $41, $01, $00, $38, $0F, $0F, $0B; 10
 db $CC, $00, $97, $00, $40, $01, $00, $38, $0F, $0F, $0A; 11
 db $A1, $00, $4B, $00, $41, $01, $04, $38, $0E, $0F, $0A; 12
 db $86, $00, $97, $00, $40, $01, $00, $38, $0E, $0F, $09; 13
 db $C9, $00, $4B, $00, $0E, $01, $00, $38, $0D, $0F, $0B; 14
 db $A0, $00, $97, $00, $0D, $01, $00, $38, $0D, $0F, $0A; 15
 db $87, $00, $4B, $00, $0E, $01, $00, $38, $0C, $0F, $0A; 16
 db $CC, $00, $97, $00, $0D, $01, $00, $38, $0C, $0F, $09; 17
 db $6A, $01, $A0, $00, $F1, $00, $1E, $10, $0C, $0F, $0B; 18
 db $A6, $01, $A0, $00, $F0, $00, $1E, $10, $0C, $0F, $0A; 19
 db $E2, $01, $50, $00, $F1, $00, $1E, $10, $0C, $0F, $0A; 20
 db $1E, $02, $A0, $00, $F0, $00, $1E, $10, $0B, $0F, $09; 21
 db $5A, $02, $50, $00, $CB, $00, $0B, $30, $0B, $0F, $0B; 22
 db $96, $02, $A0, $00, $CA, $00, $12, $30, $0B, $0F, $0A; 23
 db $D2, $02, $50, $00, $CB, $00, $1A, $30, $0B, $0F, $0A; 24
 db $0E, $03, $A0, $00, $CA, $00, $01, $30, $0A, $0F, $09; 25
 db $CB, $00, $B3, $00, $B4, $00, $00, $38, $0F, $0F, $0B; 26
 db $CC, $00, $B3, $00, $B3, $00, $00, $38, $0F, $0F, $0A; 27
 db $A1, $00, $5A, $00, $B4, $00, $04, $38, $0E, $0F, $0A; 28
 db $86, $00, $B3, $00, $B3, $00, $00, $38, $0E, $0F, $09; 29
 db $C9, $00, $5A, $00, $A1, $00, $00, $38, $0D, $0F, $0B; 30
 db $A0, $00, $B3, $00, $A0, $00, $00, $38, $0D, $0F, $0A; 31
 db $87, $00, $5A, $00, $A1, $00, $00, $38, $0C, $0F, $0A; 32
 db $CC, $00, $B3, $00, $A0, $00, $00, $38, $0C, $0F, $09; 33
 db $A1, $00, $5A, $00, $87, $00, $10, $18, $0B, $0E, $0B; 34
 db $86, $00, $B3, $00, $86, $00, $10, $18, $0B, $0E, $0A; 35
 db $C9, $00, $5A, $00, $87, $00, $10, $18, $0A, $0E, $0A; 36
 db $A0, $00, $B3, $00, $86, $00, $10, $18, $0A, $0E, $09; 37
 db $87, $00, $5A, $00, $79, $00, $00, $38, $09, $0E, $0B; 38
 db $CC, $00, $B3, $00, $78, $00, $00, $38, $09, $0E, $0A; 39
 db $A1, $00, $5A, $00, $79, $00, $00, $38, $08, $0E, $0A; 40
 db $86, $00, $B3, $00, $78, $00, $00, $38, $08, $0E, $09; 41
 db $4E, $06, $CA, $00, $66, $00, $00, $38, $0F, $0F, $0B; 42
 db $52, $06, $CA, $00, $65, $00, $00, $38, $0F, $0F, $0A; 43
 db $56, $06, $65, $00, $66, $00, $04, $38, $0F, $0F, $0A; 44
 db $52, $06, $CA, $00, $65, $00, $00, $38, $0F, $0F, $09; 45
 db $4E, $06, $65, $00, $79, $00, $00, $38, $0F, $0F, $0B; 46
 db $4A, $06, $CA, $00, $78, $00, $00, $38, $0F, $0F, $0A; 47
 db $46, $06, $65, $00, $79, $00, $00, $38, $0F, $0F, $0A; 48
 db $42, $06, $CA, $00, $78, $00, $00, $38, $0F, $0F, $09; 49
 db $6A, $01, $65, $00, $87, $00, $10, $10, $0C, $0E, $0B; 50
 db $A6, $01, $CA, $00, $86, $00, $10, $10, $0C, $0E, $0A; 51
 db $E2, $01, $65, $00, $87, $00, $10, $10, $0C, $0E, $0A; 52
 db $1E, $02, $CA, $00, $86, $00, $10, $10, $0B, $0E, $09; 53
 db $5A, $02, $65, $00, $A1, $00, $0B, $30, $0B, $0E, $0B; 54
 db $96, $02, $CA, $00, $A0, $00, $12, $30, $0B, $0E, $0A; 55
 db $D2, $02, $65, $00, $A1, $00, $1A, $30, $0B, $0E, $0A; 56
 db $0E, $03, $CA, $00, $A0, $00, $01, $30, $0A, $0E, $09; 57
 db $BB, $04, $CA, $00, $CB, $00, $00, $38, $0F, $0F, $0B; 58
 db $BF, $04, $CA, $00, $CA, $00, $00, $38, $0F, $0F, $0A; 59
 db $C3, $04, $65, $00, $CB, $00, $04, $38, $0F, $0F, $0A; 60
 db $BF, $04, $CA, $00, $CA, $00, $00, $38, $0F, $0F, $09; 61
 db $BB, $04, $65, $00, $0E, $01, $00, $38, $0F, $0F, $0B; 62
 db $B7, $04, $CA, $00, $0D, $01, $00, $38, $0F, $0F, $0A; 63
 db $B3, $04, $65, $00, $0E, $01, $00, $38, $0F, $0F, $0A; 64
 db $AF, $04, $CA, $00, $0D, $01, $00, $38, $0F, $0F, $09; 65
 db $B3, $04, $65, $00, $2F, $01, $05, $18, $0E, $0E, $0B; 66
 db $B7, $04, $CA, $00, $2E, $01, $05, $18, $0E, $0E, $0A; 67
 db $BB, $04, $65, $00, $2F, $01, $05, $18, $0E, $0E, $0A; 68
 db $BF, $04, $CA, $00, $2E, $01, $05, $18, $0E, $0E, $09; 69
 db $C3, $04, $65, $00, $0E, $01, $00, $38, $0E, $0E, $0B; 70
 db $BF, $04, $CA, $00, $0D, $01, $00, $38, $0E, $0E, $0A; 71
 db $BB, $04, $65, $00, $0E, $01, $00, $38, $0E, $0E, $0A; 72
 db $B7, $04, $CA, $00, $0D, $01, $00, $38, $0E, $0E, $09; 73
 db $CB, $00, $CA, $00, $F1, $00, $00, $38, $0F, $0F, $0B; 74
 db $CC, $00, $CA, $00, $F0, $00, $00, $38, $0F, $0F, $0A; 75
 db $98, $00, $65, $00, $F1, $00, $04, $38, $0E, $0F, $0A; 76
 db $78, $00, $CA, $00, $F0, $00, $00, $38, $0E, $0F, $09; 77
 db $C9, $00, $65, $00, $CB, $00, $00, $38, $0D, $0F, $0B; 78
 db $97, $00, $CA, $00, $CA, $00, $00, $38, $0D, $0F, $0A; 79
 db $79, $00, $65, $00, $CB, $00, $00, $38, $0C, $0F, $0A; 80
 db $CC, $00, $CA, $00, $CA, $00, $00, $38, $0C, $0F, $09; 81
 db $6A, $01, $65, $00, $B4, $00, $16, $10, $0C, $0E, $0B; 82
 db $A6, $01, $CA, $00, $B3, $00, $16, $10, $0C, $0E, $0A; 83
 db $E2, $01, $65, $00, $B4, $00, $16, $10, $0C, $0E, $0A; 84
 db $1E, $02, $CA, $00, $B3, $00, $16, $10, $0B, $0E, $09; 85
 db $5A, $02, $65, $00, $98, $00, $0B, $30, $0B, $0E, $0B; 86
 db $96, $02, $CA, $00, $97, $00, $12, $30, $0B, $0E, $0A; 87
 db $D2, $02, $65, $00, $98, $00, $1A, $30, $0B, $0E, $0A; 88
 db $0E, $03, $CA, $00, $97, $00, $01, $30, $0A, $0E, $09; 89
 db $BB, $04, $CA, $00, $B4, $00, $00, $38, $0F, $0F, $0B; 90
 db $BF, $04, $CA, $00, $B3, $00, $00, $38, $0F, $0F, $0A; 91
 db $C3, $04, $65, $00, $B4, $00, $04, $38, $0F, $0F, $0A; 92
 db $BF, $04, $CA, $00, $B3, $00, $00, $38, $0F, $0F, $09; 93
 db $BB, $04, $65, $00, $CB, $00, $00, $38, $0F, $0F, $0B; 94
 db $B7, $04, $CA, $00, $CA, $00, $00, $38, $0F, $0F, $0A; 95
 db $B3, $04, $65, $00, $CB, $00, $00, $38, $0F, $0F, $0A; 96
 db $AF, $04, $CA, $00, $CA, $00, $00, $38, $0F, $0F, $09; 97
 db $4E, $05, $E2, $00, $E3, $00, $1C, $18, $0F, $0F, $0B; 98
 db $52, $05, $E2, $00, $E2, $00, $1C, $18, $0F, $0F, $0A; 99
 db $56, $05, $71, $00, $E3, $00, $1C, $18, $0F, $0F, $0A; 100
 db $52, $05, $E2, $00, $E2, $00, $1C, $18, $0F, $0F, $09; 101
 db $4E, $05, $71, $00, $AA, $00, $00, $38, $0F, $0F, $0B; 102
 db $4A, $05, $E2, $00, $A9, $00, $00, $38, $0F, $0F, $0A; 103
 db $46, $05, $71, $00, $AA, $00, $00, $38, $0F, $0F, $0A; 104
 db $42, $05, $E2, $00, $A9, $00, $00, $38, $0F, $0F, $09; 105
 db $E3, $00, $71, $00, $98, $00, $00, $38, $0F, $0E, $0B; 106
 db $E4, $00, $E2, $00, $97, $00, $00, $38, $0F, $0E, $0A; 107
 db $AA, $00, $71, $00, $98, $00, $04, $38, $0E, $0E, $0A; 108
 db $86, $00, $E2, $00, $97, $00, $00, $38, $0E, $0E, $09; 109
 db $E1, $00, $71, $00, $87, $00, $00, $38, $0D, $0E, $0B; 110
 db $A9, $00, $E2, $00, $86, $00, $00, $38, $0D, $0E, $0A; 111
 db $87, $00, $71, $00, $87, $00, $00, $38, $0C, $0E, $0A; 112
 db $E4, $00, $E2, $00, $86, $00, $00, $38, $0C, $0E, $09; 113
 db $6A, $01, $0D, $01, $72, $00, $0E, $10, $0C, $0F, $0B; 114
 db $A6, $01, $0D, $01, $71, $00, $0E, $10, $0C, $0F, $0A; 115
 db $E2, $01, $86, $00, $72, $00, $0E, $10, $0C, $0F, $0A; 116
 db $1E, $02, $0D, $01, $71, $00, $0E, $10, $0B, $0F, $09; 117
 db $5A, $02, $86, $00, $87, $00, $0B, $30, $0B, $0F, $0B; 118
 db $96, $02, $0D, $01, $86, $00, $12, $30, $0B, $0F, $0A; 119
 db $D2, $02, $86, $00, $87, $00, $1A, $30, $0B, $0F, $0A; 120
 db $0E, $03, $0D, $01, $86, $00, $01, $30, $0A, $0F, $09; 121
 db $AA, $02, $E2, $00, $98, $00, $00, $38, $0F, $0F, $0B; 122
 db $AE, $02, $E2, $00, $97, $00, $00, $38, $0F, $0F, $0A; 123
 db $B2, $02, $71, $00, $98, $00, $04, $38, $0F, $0F, $0A; 124
 db $AE, $02, $E2, $00, $97, $00, $00, $38, $0F, $0F, $09; 125
 db $AA, $02, $71, $00, $AA, $00, $00, $38, $0F, $0F, $0B; 126
 db $A6, $02, $E2, $00, $A9, $00, $00, $38, $0F, $0F, $0A; 127
 db $A2, $02, $71, $00, $AA, $00, $00, $38, $0F, $0F, $0A; 128
 db $9E, $02, $E2, $00, $A9, $00, $00, $38, $0F, $0F, $09; 129
 db $93, $01, $CA, $00, $C5, $03, $00, $38, $0F, $0F, $0F; 130
 db $93, $01, $CA, $00, $01, $04, $00, $38, $0F, $0F, $0F; 131
 db $40, $01, $65, $00, $3D, $04, $04, $38, $0F, $0F, $0F; 132
 db $0D, $01, $CA, $00, $79, $04, $00, $38, $0F, $0F, $0F; 133
 db $93, $01, $65, $00, $B5, $04, $00, $38, $0F, $0F, $0E; 134
 db $40, $01, $CA, $00, $F1, $04, $00, $38, $0F, $0F, $0E; 135
 db $0D, $01, $65, $00, $2D, $05, $00, $38, $0F, $0F, $0E; 136
 db $93, $01, $CA, $00, $69, $05, $00, $38, $0F, $0F, $0E; 137
 db $93, $01, $A0, $00, $C5, $03, $00, $38, $0F, $0F, $0F; 138
 db $93, $01, $A0, $00, $01, $04, $00, $38, $0F, $0F, $0F; 139
 db $40, $01, $50, $00, $3D, $04, $04, $38, $0F, $0F, $0F; 140
 db $0D, $01, $A0, $00, $79, $04, $00, $38, $0F, $0F, $0F; 141
 db $93, $01, $50, $00, $B5, $04, $00, $38, $0F, $0F, $0E; 142
 db $40, $01, $A0, $00, $F1, $04, $00, $38, $0F, $0F, $0E; 143
 db $0D, $01, $50, $00, $2D, $05, $00, $38, $0F, $0F, $0E; 144
 db $93, $01, $A0, $00, $69, $05, $00, $38, $0F, $0F, $0E; 145
 db $40, $01, $50, $00, $C5, $03, $00, $38, $0E, $0E, $0F; 146
 db $0D, $01, $A0, $00, $01, $04, $00, $38, $0E, $0E, $0F; 147
 db $93, $01, $50, $00, $3D, $04, $04, $38, $0E, $0E, $0F; 148
 db $40, $01, $A0, $00, $79, $04, $00, $38, $0E, $0E, $0F; 149
 db $0D, $01, $50, $00, $B5, $04, $00, $38, $0E, $0E, $0E; 150
 db $93, $01, $A0, $00, $F1, $04, $00, $38, $0E, $0E, $0E; 151
 db $40, $01, $50, $00, $2D, $05, $00, $38, $0E, $0E, $0E; 152
 db $0D, $01, $A0, $00, $69, $05, $00, $38, $0E, $0E, $0E; 153
 db $C4, $01, $97, $00, $6F, $04, $00, $38, $0F, $0F, $0F; 154
 db $C4, $01, $97, $00, $AB, $04, $00, $38, $0F, $0F, $0F; 155
 db $67, $01, $4B, $00, $E7, $04, $04, $38, $0F, $0F, $0F; 156
 db $2E, $01, $97, $00, $23, $05, $00, $38, $0F, $0F, $0F; 157
 db $C4, $01, $4B, $00, $5F, $05, $00, $38, $0F, $0F, $0E; 158
 db $67, $01, $97, $00, $9B, $05, $00, $38, $0F, $0F, $0E; 159
 db $2E, $01, $4B, $00, $D7, $05, $00, $38, $0F, $0F, $0E; 160
 db $C4, $01, $97, $00, $13, $06, $00, $38, $0F, $0F, $0E; 161
 db $67, $01, $4B, $00, $6F, $04, $00, $38, $0E, $0E, $0F; 162
 db $2E, $01, $97, $00, $AB, $04, $00, $38, $0E, $0E, $0F; 163
 db $C4, $01, $4B, $00, $E7, $04, $04, $38, $0E, $0E, $0F; 164
 db $67, $01, $97, $00, $23, $05, $00, $38, $0E, $0E, $0F; 165
 db $2E, $01, $4B, $00, $5F, $05, $00, $38, $0E, $0E, $0E; 166
 db $C4, $01, $97, $00, $9B, $05, $00, $38, $0E, $0E, $0E; 167
 db $67, $01, $4B, $00, $D7, $05, $00, $38, $0E, $0E, $0E; 168
 db $2E, $01, $97, $00, $13, $06, $00, $38, $0E, $0E, $0E; 169
 db $C4, $01, $97, $00, $6F, $04, $00, $38, $0F, $0F, $0F; 170
 db $C4, $01, $97, $00, $AB, $04, $00, $38, $0F, $0F, $0F; 171
 db $67, $01, $4B, $00, $E7, $04, $04, $38, $0F, $0F, $0F; 172
 db $2E, $01, $97, $00, $23, $05, $00, $38, $0F, $0F, $0F; 173
 db $C4, $01, $4B, $00, $5F, $05, $00, $38, $0F, $0F, $0E; 174
 db $67, $01, $97, $00, $9B, $05, $00, $38, $0F, $0F, $0E; 175
 db $2E, $01, $4B, $00, $D7, $05, $00, $38, $0F, $0F, $0E; 176
 db $C4, $01, $97, $00, $13, $06, $00, $38, $0F, $0F, $0E; 177
 db $67, $01, $4B, $00, $86, $05, $00, $38, $0E, $0E, $0F; 178
 db $2E, $01, $97, $00, $C2, $05, $00, $38, $0E, $0E, $0F; 179
 db $C4, $01, $4B, $00, $FE, $05, $04, $38, $0E, $0E, $0F; 180
 db $67, $01, $97, $00, $3A, $06, $00, $38, $0E, $0E, $0F; 181
 db $2E, $01, $4B, $00, $76, $06, $00, $38, $0E, $0E, $0E; 182
 db $C4, $01, $97, $00, $B2, $06, $00, $38, $0E, $0E, $0E; 183
 db $67, $01, $4B, $00, $EE, $06, $00, $38, $0E, $0E, $0E; 184
 db $2E, $01, $97, $00, $2A, $07, $00, $38, $0E, $0E, $0E; 185
 db $C4, $01, $97, $00, $86, $05, $00, $38, $0F, $0F, $0F; 186
 db $C4, $01, $97, $00, $C2, $05, $00, $38, $0F, $0F, $0F; 187
 db $67, $01, $4B, $00, $FE, $05, $04, $38, $0F, $0F, $0F; 188
 db $2E, $01, $97, $00, $3A, $06, $00, $38, $0F, $0F, $0F; 189
 db $C4, $01, $4B, $00, $76, $06, $00, $38, $0F, $0F, $0E; 190
 db $67, $01, $97, $00, $B2, $06, $00, $38, $0F, $0F, $0E; 191
 db $2E, $01, $4B, $00, $EE, $06, $00, $38, $0F, $0F, $0E; 192
 db $C4, $01, $97, $00, $2A, $07, $00, $38, $0F, $0F, $0E; 193
 db $93, $01, $A0, $00, $86, $06, $00, $38, $0F, $0F, $0F; 194
 db $93, $01, $A0, $00, $C2, $06, $00, $38, $0F, $0F, $0F; 195
 db $40, $01, $50, $00, $FE, $06, $04, $38, $0F, $0F, $0F; 196
 db $0D, $01, $A0, $00, $3A, $07, $00, $38, $0F, $0F, $0F; 197
 db $93, $01, $50, $00, $76, $07, $00, $38, $0F, $0F, $0E; 198
 db $40, $01, $A0, $00, $B2, $07, $00, $38, $0F, $0F, $0E; 199
 db $0D, $01, $50, $00, $EE, $07, $00, $38, $0F, $0F, $0E; 200
 db $93, $01, $A0, $00, $2A, $08, $00, $38, $0F, $0F, $0E; 201
 db $40, $01, $50, $00, $66, $08, $00, $38, $0E, $0E, $0D; 202
 db $0D, $01, $A0, $00, $A2, $08, $00, $38, $0E, $0E, $0D; 203
 db $93, $01, $50, $00, $DE, $08, $04, $38, $0E, $0E, $0D; 204
 db $40, $01, $A0, $00, $1A, $09, $00, $38, $0E, $0E, $0D; 205
 db $0D, $01, $50, $00, $56, $09, $00, $38, $0E, $0E, $0C; 206
 db $93, $01, $A0, $00, $92, $09, $00, $38, $0E, $0E, $0C; 207
 db $40, $01, $50, $00, $CE, $09, $00, $38, $0E, $0E, $0C; 208
 db $0D, $01, $A0, $00, $0A, $0A, $00, $38, $0E, $0E, $0C; 209
 db $93, $01, $50, $00, $46, $0A, $00, $38, $0D, $0D, $0B; 210
 db $40, $01, $A0, $00, $82, $0A, $00, $38, $0D, $0D, $0B; 211
 db $0D, $01, $50, $00, $BE, $0A, $04, $38, $0D, $0D, $0B; 212
 db $95, $01, $A2, $00, $FA, $0A, $00, $38, $0D, $0D, $0B; 213
 db $44, $01, $54, $00, $36, $0B, $00, $38, $0D, $0D, $0A; 214
 db $0F, $01, $A2, $00, $72, $0B, $00, $38, $0D, $0D, $0A; 215
 db $93, $01, $50, $00, $AE, $0B, $00, $38, $0D, $0D, $0A; 216
 db $3E, $01, $9E, $00, $EA, $0B, $00, $38, $0D, $0D, $0A; 217
 db $09, $01, $4C, $00, $26, $0C, $00, $38, $0C, $0C, $09; 218
 db $91, $01, $9E, $00, $62, $0C, $00, $38, $0C, $0C, $09; 219
 db $40, $01, $50, $00, $9E, $0C, $04, $38, $0C, $0C, $09; 220
 db $0F, $01, $A2, $00, $DA, $0C, $00, $38, $0C, $0C, $09; 221
 db $97, $01, $54, $00, $16, $0D, $00, $38, $0C, $0C, $08; 222
 db $42, $01, $A2, $00, $52, $0D, $00, $38, $0C, $0C, $08; 223
 db $0D, $01, $50, $00, $8E, $0D, $00, $38, $0C, $0C, $08; 224
 db $91, $01, $9E, $00, $CA, $0D, $00, $38, $0C, $0C, $08; 225
 db $9D, $01, $AA, $00, $86, $06, $00, $38, $0F, $0F, $0F; 226
 db $A7, $01, $B4, $00, $C2, $06, $00, $38, $0F, $0F, $0F; 227
 db $5E, $01, $6E, $00, $FE, $06, $04, $38, $0F, $0F, $0F; 228
 db $68, $01, $78, $00, $3A, $07, $00, $38, $0F, $0F, $0F; 229
 db $72, $01, $82, $00, $76, $07, $00, $38, $0F, $0F, $0E; 230
 db $7C, $01, $8C, $00, $B2, $07, $00, $38, $0F, $0F, $0E; 231
 db $86, $01, $96, $00, $EE, $07, $00, $38, $0F, $0F, $0E; 232
 db $90, $01, $A0, $00, $2A, $08, $00, $38, $0F, $0F, $0E; 233
 db $9A, $01, $AA, $00, $66, $08, $00, $38, $0E, $0E, $0D; 234
 db $A4, $01, $B4, $00, $A2, $08, $00, $38, $0E, $0E, $0D; 235
 db $AE, $01, $BE, $00, $DE, $08, $04, $38, $0E, $0E, $0D; 236
 db $B8, $01, $C8, $00, $1A, $09, $00, $38, $0E, $0E, $0D; 237
 db $C2, $01, $D2, $00, $56, $09, $00, $38, $0E, $0E, $0C; 238
 db $CC, $01, $DC, $00, $92, $09, $00, $38, $0E, $0E, $0C; 239
 db $D6, $01, $E6, $00, $CE, $09, $00, $38, $0E, $0E, $0C; 240
 db $E0, $01, $F0, $00, $0A, $0A, $00, $38, $0E, $0E, $0C; 241
 db $EA, $01, $FA, $00, $46, $0A, $00, $38, $0D, $0D, $0B; 242
 db $F4, $01, $04, $01, $82, $0A, $00, $38, $0D, $0D, $0B; 243
 db $FE, $01, $0E, $01, $BE, $0A, $04, $38, $0D, $0D, $0B; 244
 db $08, $02, $18, $01, $FA, $0A, $00, $38, $0D, $0D, $0B; 245
 db $12, $02, $22, $01, $36, $0B, $00, $38, $0D, $0D, $0A; 246
 db $1C, $02, $2C, $01, $72, $0B, $00, $38, $0D, $0D, $0A; 247
 db $26, $02, $36, $01, $AE, $0B, $00, $38, $0D, $0D, $0A; 248
 db $30, $02, $40, $01, $EA, $0B, $00, $38, $0D, $0D, $0A; 249
 db $3A, $02, $4A, $01, $26, $0C, $00, $38, $0C, $0C, $09; 250
 db $44, $02, $54, $01, $62, $0C, $00, $38, $0C, $0C, $09; 251
 db $4E, $02, $5E, $01, $9E, $0C, $04, $38, $0C, $0C, $09; 252
 db $58, $02, $68, $01, $DA, $0C, $00, $38, $0C, $0C, $09; 253
 db $62, $02, $72, $01, $16, $0D, $00, $38, $0C, $0C, $08; 254
 db $6C, $02, $7C, $01, $52, $0D, $00, $38, $0C, $0C, $08; 255
 db $76, $02, $86, $01, $8E, $0D, $00, $38, $0C, $0C, $08; 256
 db $80, $02, $90, $01, $CA, $0D, $00, $38, $0C, $0C, $08; 257
 db $8A, $02, $9A, $01, $06, $0E, $00, $38, $0B, $0B, $07; 258
 db $94, $02, $A4, $01, $42, $0E, $00, $38, $0B, $0B, $07; 259
 db $9E, $02, $AE, $01, $7E, $0E, $04, $38, $0B, $0B, $07; 260
 db $A8, $02, $B8, $01, $BA, $0E, $00, $38, $0B, $0B, $07; 261
 db $B2, $02, $C2, $01, $F6, $0E, $00, $38, $0B, $0B, $06; 262
 db $BC, $02, $CC, $01, $32, $0F, $00, $38, $0B, $0B, $06; 263
 db $C6, $02, $D6, $01, $6E, $0F, $00, $38, $0B, $0B, $06; 264
 db $D0, $02, $E0, $01, $AA, $0F, $00, $38, $0B, $0B, $06; 265
 db $DA, $02, $EA, $01, $E6, $0F, $00, $38, $0B, $0B, $05; 266
 db $E4, $02, $F4, $01, $22, $00, $00, $38, $0A, $0A, $05; 267
 db $EE, $02, $FE, $01, $5E, $00, $04, $38, $0A, $0A, $05; 268
 db $F8, $02, $08, $02, $9A, $00, $00, $38, $0A, $0A, $05; 269
 db $02, $03, $12, $02, $D6, $00, $00, $38, $0A, $0A, $04; 270
 db $0C, $03, $1C, $02, $12, $01, $00, $38, $0A, $0A, $04; 271
 db $16, $03, $26, $02, $4E, $01, $00, $38, $0A, $0A, $04; 272
 db $20, $03, $30, $02, $8A, $01, $00, $38, $0A, $0A, $04; 273
 db $2A, $03, $3A, $02, $C6, $01, $00, $38, $0A, $0A, $03; 274
 db $34, $03, $44, $02, $02, $02, $00, $38, $09, $09, $03; 275
 db $3E, $03, $4E, $02, $3E, $02, $04, $38, $09, $09, $03; 276
 db $48, $03, $58, $02, $7A, $02, $00, $38, $09, $09, $03; 277
 db $52, $03, $62, $02, $B6, $02, $00, $38, $09, $09, $02; 278
 db $5C, $03, $6C, $02, $F2, $02, $00, $38, $09, $09, $02; 279
 db $66, $03, $76, $02, $2E, $03, $00, $38, $09, $09, $02; 280
 db $70, $03, $80, $02, $6A, $03, $00, $38, $09, $09, $02; 281
 db $7A, $03, $8A, $02, $A6, $03, $00, $38, $09, $09, $01; 282
 db $84, $03, $94, $02, $E2, $03, $00, $38, $08, $08, $01; 283
 db $8E, $03, $9E, $02, $1E, $04, $04, $38, $08, $08, $01; 284
 db $98, $03, $A8, $02, $5A, $04, $00, $38, $08, $08, $01; 285
 db $A2, $03, $B2, $02, $96, $04, $00, $38, $08, $08, $00; 286
 db $AC, $03, $BC, $02, $D2, $04, $00, $38, $08, $08, $00; 287
 db $B6, $03, $C6, $02, $0E, $05, $00, $38, $08, $08, $00; 288
 db $C0, $03, $D0, $02, $4A, $05, $00, $38, $08, $08, $00; 289
 db $CA, $03, $DA, $02, $86, $05, $00, $38, $08, $08, $00; 290
 db $D4, $03, $E4, $02, $C2, $05, $00, $38, $07, $07, $00; 291
 db $DE, $03, $EE, $02, $FE, $05, $04, $3C, $07, $07, $00; 292
 db $E8, $03, $F8, $02, $FE, $05, $00, $3C, $07, $07, $00; 293
 db $F2, $03, $02, $03, $FE, $05, $00, $3C, $07, $07, $00; 294
 db $FC, $03, $0C, $03, $FE, $05, $00, $3C, $07, $07, $00; 295
 db $06, $04, $16, $03, $FE, $05, $00, $3C, $07, $07, $00; 296
 db $10, $04, $20, $03, $FE, $05, $00, $3C, $07, $07, $00; 297
 db $1A, $04, $2A, $03, $FE, $05, $00, $3C, $07, $07, $00; 298
 db $24, $04, $34, $03, $FE, $05, $00, $3C, $06, $06, $00; 299
 db $2E, $04, $3E, $03, $FE, $05, $04, $3C, $06, $06, $00; 300
 db $38, $04, $48, $03, $FE, $05, $00, $3C, $06, $06, $00; 301
 db $42, $04, $52, $03, $FE, $05, $00, $3C, $06, $06, $00; 302
 db $4C, $04, $5C, $03, $FE, $05, $00, $3C, $06, $06, $00; 303
 db $56, $04, $66, $03, $FE, $05, $00, $3C, $06, $06, $00; 304
 db $60, $04, $70, $03, $FE, $05, $00, $3C, $06, $06, $00; 305
 db $6A, $04, $7A, $03, $FE, $05, $00, $3C, $06, $06, $00; 306
 db $74, $04, $84, $03, $FE, $05, $00, $3C, $05, $05, $00; 307
 db $7E, $04, $8E, $03, $FE, $05, $04, $3C, $05, $05, $00; 308
 db $88, $04, $98, $03, $FE, $05, $00, $3C, $05, $05, $00; 309
 db $92, $04, $A2, $03, $FE, $05, $00, $3C, $05, $05, $00; 310
 db $9C, $04, $AC, $03, $FE, $05, $00, $3C, $05, $05, $00; 311
 db $A6, $04, $B6, $03, $FE, $05, $00, $3C, $05, $05, $00; 312
 db $B0, $04, $C0, $03, $FE, $05, $00, $3C, $05, $05, $00; 313
 db $BA, $04, $CA, $03, $FE, $05, $00, $3C, $05, $05, $00; 314
 db $C4, $04, $D4, $03, $FE, $05, $00, $3C, $04, $04, $00; 315
 db $CE, $04, $DE, $03, $FE, $05, $04, $3C, $04, $04, $00; 316
 db $D8, $04, $E8, $03, $FE, $05, $00, $3C, $04, $04, $00; 317
 db $E2, $04, $F2, $03, $FE, $05, $00, $3C, $04, $04, $00; 318
 db $EC, $04, $FC, $03, $FE, $05, $00, $3C, $04, $04, $00; 319
 db $F6, $04, $06, $04, $FE, $05, $00, $3C, $04, $04, $00; 320
 db $00, $05, $10, $04, $FE, $05, $00, $3C, $04, $04, $00; 321
 db $0A, $05, $1A, $04, $FE, $05, $00, $3C, $04, $04, $00; 322
 db $14, $05, $24, $04, $FE, $05, $00, $3C, $03, $03, $00; 323
 db $1E, $05, $2E, $04, $FE, $05, $04, $3C, $03, $03, $00; 324
 db $28, $05, $38, $04, $FE, $05, $00, $3C, $03, $03, $00; 325
 db $32, $05, $42, $04, $FE, $05, $00, $3C, $03, $03, $00; 326
 db $3C, $05, $4C, $04, $FE, $05, $00, $3C, $03, $03, $00; 327
 db $46, $05, $56, $04, $FE, $05, $00, $3C, $03, $03, $00; 328
 db $50, $05, $60, $04, $FE, $05, $00, $3C, $03, $03, $00; 329
 db $5A, $05, $6A, $04, $FE, $05, $00, $3C, $03, $03, $00; 330
 db $64, $05, $74, $04, $FE, $05, $00, $3C, $02, $02, $00; 331
 db $6E, $05, $7E, $04, $FE, $05, $04, $3C, $02, $02, $00; 332
 db $78, $05, $88, $04, $FE, $05, $00, $3C, $02, $02, $00; 333
 db $82, $05, $92, $04, $FE, $05, $00, $3C, $02, $02, $00; 334
 db $8C, $05, $9C, $04, $FE, $05, $00, $3C, $02, $02, $00; 335
 db $96, $05, $A6, $04, $FE, $05, $00, $3C, $02, $02, $00; 336
 db $A0, $05, $B0, $04, $FE, $05, $00, $3C, $02, $02, $00; 337
 db $AA, $05, $BA, $04, $FE, $05, $00, $3C, $02, $02, $00; 338
 db $B4, $05, $C4, $04, $FE, $05, $00, $3C, $01, $01, $00; 339
 db $BE, $05, $CE, $04, $FE, $05, $04, $3C, $01, $01, $00; 340
 db $C8, $05, $D8, $04, $FE, $05, $00, $3C, $01, $01, $00; 341
 db $D2, $05, $E2, $04, $FE, $05, $00, $3C, $01, $01, $00; 342
 db $DC, $05, $EC, $04, $FE, $05, $00, $3C, $01, $01, $00; 343
 db $E6, $05, $F6, $04, $FE, $05, $00, $3C, $01, $01, $00; 344
 db $F0, $05, $00, $05, $FE, $05, $00, $3C, $01, $01, $00; 345
 db $FA, $05, $0A, $05, $FE, $05, $00, $3C, $01, $01, $00; 346
 db $04, $06, $14, $05, $FE, $05, $00, $3C, $00, $00, $00; 347
 db $0E, $06, $1E, $05, $FE, $05, $04, $3C, $00, $00, $00; 348
 db $18, $06, $28, $05, $FE, $05, $00, $3C, $00, $00, $00; 349
 db $22, $06, $32, $05, $FE, $05, $00, $3C, $00, $00, $00; 350
 db $2C, $06, $3C, $05, $FE, $05, $00, $3C, $00, $00, $00; 351
 db $36, $06, $46, $05, $FE, $05, $00, $3C, $00, $00, $00; 352
 db $40, $06, $50, $05, $FE, $05, $00, $3C, $00, $00, $00; 353
 db $4A, $06, $5A, $05, $FE, $05, $00, $3C, $00, $00, $00; 354
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 355
 db $54, $06, $64, $05, $FE, $05, $04, $3F, $00, $00, $00; 356
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 357
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 358
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 359
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 360
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 361
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 362
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 363
 db $54, $06, $64, $05, $FE, $05, $04, $3F, $00, $00, $00; 364
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 365
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 366
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 367
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 368
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 369
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 370
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 371
 db $54, $06, $64, $05, $FE, $05, $04, $3F, $00, $00, $00; 372
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 373
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 374
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 375
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 376
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 377
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 378
 db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 379
 db $54, $06, $64, $05, $FE, $05, $04, $3F, $00, $00, $00; 380
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 381
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 382
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 383
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 384
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 385
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 386
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 387
; db $54, $06, $64, $05, $FE, $05, $04, $3F, $00, $00, $00; 388
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 389
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 390
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 391
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 392
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 393
; db $54, $06, $64, $05, $FE, $05, $00, $3F, $00, $00, $00; 394