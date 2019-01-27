
; Generated by CharPad 2. Assemble with 64TASS or similar.


; General constants:-

TRUE = 1
FALSE = 0
COLRMETH_GLOBAL = 0
COLRMETH_PERTILE = 1
COLRMETH_PERCHAR = 2


; Project constants:-

COLOURING_METHOD = COLRMETH_PERTILE
CHAR_MULTICOLOUR_MODE = FALSE
COLR_SCREEN = 1
COLR_CHAR_DEF = 0
CHAR_COUNT = 16
TILE_COUNT = 16
TILE_WID = 4
TILE_HEI = 4
MAP_WID = 40
MAP_HEI = 25
MAP_WID_CHRS = 160
MAP_HEI_CHRS = 100
MAP_WID_PXLS = 1280
MAP_HEI_PXLS = 800


; Data block size constants:-

SZ_CHARSET_DATA        = 128
SZ_CHARSET_ATTRIB_DATA = 16
SZ_TILESET_DATA        = 256
SZ_TILESET_ATTRIB_DATA = 16
SZ_MAP_DATA            = 1000


; Data block address constants (dummy values):-

ADDR_CHARSET_DATA        = $1000   ; nb. label = 'charset_data'        (block size = $80).
ADDR_CHARSET_ATTRIB_DATA = $2000   ; nb. label = 'charset_attrib_data' (block size = $10).
ADDR_TILESET_DATA        = $3000   ; nb. label = 'tileset_data'        (block size = $100).
ADDR_TILESET_ATTRIB_DATA = $4000   ; nb. label = 'tileset_attrib_data' (block size = $10).
ADDR_MAP_DATA            = $5000   ; nb. label = 'map_data'            (block size = $3e8).



; * INSERT EXAMPLE PROGRAM HERE! * (Or just include this file in your project).



; CHAR SET DATA : 16 (8 byte) chars : total size is 128 ($80) bytes.

* = ADDR_CHARSET_DATA
charset_data

.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff



; CHAR SET ATTRIBUTE DATA : 16 attributes : total size is 16 ($10) bytes.
; nb. Upper nybbles = Material, Lower nybbles = Colour.

* = ADDR_CHARSET_ATTRIB_DATA
charset_attrib_data

.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00



; TILE SET DATA : 16 (4x4) tiles : total size is 256 ($100) bytes.

* = ADDR_TILESET_DATA
tileset_data

.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00



; TILE SET ATTRIBUTE DATA : 16 attributes : total size is 16 ($10) bytes.
; nb. Upper nybbles = Unused, Lower nybbles = Colour.

* = ADDR_TILESET_ATTRIB_DATA
tileset_attrib_data

.byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f



; MAP DATA : 1 (40x25) map : total size is 1000 ($3e8) bytes.

* = ADDR_MAP_DATA
map_data

.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$03,$08,$08,$08,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01
.byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$03,$08,$00,$00,$01
.byte $01,$01,$01,$01,$01,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
.byte $01,$01,$01,$01,$0f,$0f,$0f,$01,$01,$02,$02,$01,$0a,$01,$03,$01
.byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$01,$01,$0f,$0b,$01,$01
.byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0f,$01
.byte $01,$02,$02,$06,$0a,$01,$03,$01,$01,$01,$01,$03,$08,$00,$00,$01
.byte $03,$03,$03,$03,$0f,$0b,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $03,$03,$03,$03,$03,$03,$0f,$03,$03,$02,$02,$06,$0a,$03,$03,$01
.byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$01,$01,$0f,$0b,$09,$09
.byte $09,$0f,$0b,$0b,$0f,$0b,$0b,$0f,$09,$0f,$0c,$0b,$0b,$0c,$0f,$0f
.byte $09,$02,$02,$06,$0a,$09,$03,$01,$01,$01,$01,$03,$08,$00,$00,$01
.byte $01,$01,$01,$09,$0b,$0b,$09,$09,$09,$0f,$0f,$0f,$0f,$0f,$0f,$0f
.byte $0f,$09,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$09,$09,$09,$09,$09,$09,$03
.byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$09,$09,$09,$09,$09,$09
.byte $09,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$09,$09,$09,$09,$09,$09,$09
.byte $09,$09,$09,$09,$09,$09,$09,$09,$03,$01,$01,$03,$08,$00,$00,$01
.byte $01,$09,$09,$09,$09,$09,$09,$09,$08,$0b,$01,$01,$01,$01,$01,$01
.byte $01,$09,$08,$08,$07,$08,$08,$09,$08,$08,$09,$08,$09,$09,$09,$09
.byte $09,$03,$01,$03,$08,$00,$00,$01,$09,$09,$09,$09,$09,$09,$09,$09
.byte $08,$0b,$01,$00,$00,$00,$00,$00,$01,$09,$08,$08,$08,$08,$08,$09
.byte $08,$08,$09,$08,$08,$09,$09,$09,$09,$09,$03,$03,$08,$00,$00,$01
.byte $09,$03,$03,$03,$03,$0e,$09,$09,$08,$0b,$01,$00,$00,$00,$00,$00
.byte $01,$09,$09,$09,$09,$09,$09,$09,$08,$08,$09,$08,$08,$09,$09,$09
.byte $09,$09,$09,$03,$08,$00,$00,$01,$03,$03,$03,$03,$0e,$09,$09,$08
.byte $08,$0b,$01,$00,$00,$00,$00,$00,$01,$09,$08,$08,$08,$08,$08,$09
.byte $08,$08,$09,$08,$08,$09,$08,$09,$09,$09,$09,$03,$08,$00,$00,$01
.byte $0e,$0e,$0e,$0e,$0e,$09,$09,$08,$08,$0b,$01,$01,$01,$01,$01,$01
.byte $01,$09,$08,$08,$08,$08,$08,$09,$08,$08,$09,$08,$08,$09,$08,$09
.byte $09,$09,$09,$03,$08,$00,$00,$01,$09,$09,$09,$09,$09,$09,$09,$05
.byte $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
.byte $05,$05,$05,$08,$08,$09,$08,$08,$09,$09,$09,$03,$08,$00,$00,$01
.byte $09,$09,$09,$09,$09,$09,$09,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$08,$09,$08,$08
.byte $09,$08,$08,$03,$08,$00,$00,$01,$08,$05,$05,$05,$05,$08,$08,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$08,$09,$08,$08,$09,$08,$08,$03,$08,$00,$00,$01
.byte $08,$05,$05,$05,$05,$08,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$09,$08,$08
.byte $09,$08,$08,$03,$08,$00,$00,$01,$08,$05,$05,$05,$05,$08,$08,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$09,$08,$08,$09,$08,$08,$03,$08,$00,$00,$01
.byte $08,$05,$05,$05,$05,$08,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$08,$08
.byte $09,$08,$08,$03,$08,$00,$00,$01,$08,$05,$05,$05,$0d,$08,$08,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$08,$08,$09,$08,$08,$03,$08,$00,$00,$01
.byte $08,$0d,$0d,$0d,$0d,$08,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$08
.byte $09,$08,$08,$03,$08,$00,$05,$01,$08,$0d,$0d,$0d,$0d,$08,$08,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$05,$05,$05,$05,$05,$05,$05,$01
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
.byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$01


