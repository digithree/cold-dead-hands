;==========================================================
; Cold Dead Hands
;
; MAIN source file, build entry point.
;
;==========================================================

;==========================================================
; SYSTEM AND KERNAL ROUTINES 
;==========================================================

STD_INT         = $EA31     ; KERNAL standard interrupt service routine to handle keyboard scan, cursor display etc.
FN_SCR_WRITE_F  = $BDCD     ; Write integer value in A/X onto screen, in floating-point format.
FN_CHROUT       = $FFD2     ; Write byte to default output, input A = byte to write

; DATA STARTS HERE

;!to "main.prg"

!macro start_at .address {
  * = $0801
  !byte $0c,$08,$00,$00,$9e
  !if .address >= 10000 { !byte 48 + ((.address / 10000) % 10) }
  !if .address >=  1000 { !byte 48 + ((.address /  1000) % 10) }
  !if .address >=   100 { !byte 48 + ((.address /   100) % 10) }
  !if .address >=    10 { !byte 48 + ((.address /    10) % 10) }
  !byte $30 + (.address % 10), $00, $00, $00
  * = .address
}

+start_at $1000

* = $1000

;==========================================================
; CODE
;==========================================================

entry_and_first_time_setup
  lda #$24        ; set sprite data of player character (at block 36/0x24) = $0900 for sprite 0
  sta $07F8
  lda #$01        ; turn on bit 0 of multi-color mode indicator, sprite 0 to multicolor mode only
  sta $D01C
  lda #$03
  sta $D01D       ; bit 0 and 1 on for horiztonal expand for sprite 0 and 1
  sta $D017       ; bit 0 and 1 on for horiztonal expand for sprite 0 and 1
  ;jsr clear_screen
  jsr black_screen
;draw_character
new_game_start
  jsr new_game_reset_vars
  lda #$00        ; set param map number for set_map
  sta var_map_cur
new_map_screen
  lda var_map_cur
  jsr set_map
map_screen_loop
  lda var_map_cur
  rol            ; left shift << 4, 01 goes to 10, etc., to add to base img high byte
  rol
  rol
  rol
  clc           ; clear carry flag before adding (would influence result if set)
  adc const_map_img_base_low  ; add map number index to base img high byte
  tax             ; A -> X, X is param for draw_screen base page
  ;jsr draw_screen
setup_sprite
  ; TODO
position_loop
  ; get current position info for map
  lda #<map_meta_data_table     ; move map meta data table base adddres to zero page
  sta $14
  lda #>map_meta_data_table
  sta $15
  lda var_map_cur               ; use meta data table to get address position list for map number var_map_cur
  lsr                           ; need to first multiply by 2 (2 bytes per address)
  tay                           ; then store in Y for indirect look up offset
  lda ($14), Y
  sta $16                       ; store low / high byte in $16 / $17, for further look up
  iny
  lda ($14), Y
  sta $17
  ldy #$00                      ; reset Y counter for position 0, to which we'll add var_map_pos
  ;lda ($16), Y    ; number of positions
  ;tax
  ldx var_map_pos               ; set X = loop counter, +1 as we'll skip to infos for that position by looping via X
  inx
  lda #$00                      ; start at position zero
position_loop_add_loop
  adc #$07                      ; add +7 byte size to offset position in A
  dex                           ; loop counter--
  bne position_loop_add_loop    ; if not zero keep looping
  tay                           ; result of looped addition A -> Y, for pointer offset usage
  sty $18                       ; temp store Y -> Zero page $18
set_sprite_position
;  ldy $18                       ; load this position pointer offset
;  lda ($16), Y                  ; load scr-x value of position, multiply by 8, store in sprite 0 X mem loc
;  ; TODO : if Y*8 > 256 then need to set MSB sprite register at $D010
;  lsr
;  lsr
;  lsr
;  sta $D000
;  iny                           ; next value
;  lda ($16), Y                  ; load scr-y value of position, multiply by 8, store in sprite 0 Y mem loc
;  lsr
;  lsr
;  lsr
;  sta $D001
  lda #$00
  sta $D000
  sta $D001
  lda $D015                     ; finally, turn sprite on, OR sprite enable reg with 0x01, bit 0 for sprite 0
  ora #$01
  sta $D015
;have_some_heart
;  lda #$53      ; heart character
;  sta $05F4
;  lda #$02      ; red foreground
;  sta $D9F4

  jmp *

;==========================================================
; ROUTINES
;==========================================================

; === new_game_reset_vars
new_game_reset_vars
  ; TODO : set items carrying flag to off for all items
  rts

; === set_map
; A <- map number
set_map
  pha
  sta var_map_cur
  lda #$00
  sta var_map_pos
  pla
  rts


; === clear_screen
clear_screen
  clc           ; clear carry flag
  lda #$00      ; put address $0400 (end of screen chars) at zero page $14+1
  tay           ; - save #$00 to Y for later
  sta $14
  lda #$04
  sta $15
  lda #$E8      ; put high byte of address $07E8 (end of screen chars + 1) at zero page $16
  sta $16
  ldx #$08      ; load low byte of address $07E8 to X
  lda #$E0      ; put screen char reverse SPACE in A
clear_screen_loop_1
  sta ($14), Y  ; [$15$14]+Y = #$00
  iny
  bne clear_screen_skip_1
  inc $15
  jmp clear_screen_loop_1
clear_screen_skip_1
  cpx $15       ; check low byte of address $07E8 (end of screen chars + 1) against current addr low byte (zero page)
  bne clear_screen_loop_1
  cpy $16       ; check high byte of address $07E8 (end of screen chars + 1) against current addr high byte (in Y)
  bne clear_screen_loop_1
  rts
; === black_screen
black_screen
  lda #$01
  sta $D020
  lda #$00
  sta $D021
  rts
; === draw_screen
; X <- base page for char (+1k for col)
draw_screen
  clc           ; clear carry flag
  lda #$00      ; put address $0400 (end of screen chars) at zero page $14+1
  sta $20
  lda #$04
  sta $21
  lda #$00      ; put address $D800 (start of color chars) at zero page $14+1
  tay           ; - save #$00 to Y for later
  sta $14
  lda #$D8
  sta $15
  lda #$E8      ; put address $DBE8 (end of color chars) at zero page $16+1
  sta $16
  lda #$DB
  sta $17
  lda #$00      ; put address $8000 (character data) at zero page $18+1
  sta $18
  txa           ; load base high byte
  sta $19
  ldx #$E0      ; reverse SPACE (block) character in X
draw_screen_loop_1
  lda ($18), Y  ; A = [$19$18]+Y (next char data)
  sta ($14), Y  ; [$12$13]+Y = A
  txa           ; X -> A for block char
  sta ($20), Y  ; [$15$14]+Y = A
  iny
  bne draw_screen_skip_1
  inc $15
  inc $19
  inc $21
  jmp draw_screen_loop_1
draw_screen_skip_1
  lda $17       ; load low byte of target address
  cmp $15       ; check A against low byte of address $07E8 (end of screen chars + 1)
  bne draw_screen_loop_1
  cpy $16       ; check cur high byte (Y) against high byte of address $07E8 (end of screen chars + 1)
  bne draw_screen_loop_1
  rts

;==========================================================
; CONSTANT DATA (near)
;==========================================================

; values
const_bag_size
  !byte $1E     ; size of bag (30)
; addresses
const_map_meta_base_low
  !byte $00 ; low byte of addr $6000
const_map_meta_base_high
  !byte $60 ; high byte of addr $6000
const_map_img_base_low
  !byte $70

;==========================================================
; "VARIABLES" GLOBAL DATA, memory addresses for holding (near)
;==========================================================

; default init value of $00, same for all vars
var_map_img_base_low
  !byte $00   ; stores the value of img base low (#$70) for arithmetic
var_map_cur
  !byte $00   ; map number active
var_map_pos
  !byte $00   ; position on map

;==========================================================
; MAP META DATA
;
; First byte in set of positions is number of positions.
; Positions are always 7 bytes long, with maximum 6 positions.
;==========================================================
* = $6000

map_meta_data_table
  !byte $08,$60
  !byte $08,$60 ; TODO : change to position data
  !byte $08,$60 ; TODO : change to position data
  !byte $08,$60 ; TODO : change to position data

map_meta_data_1
;0-bedroom, 1 + (6 * 7) = 43 bytes of info
  !byte $05,$11,$00,$FF,$01,$FF,$FF   ;pos(05/0x05,17/0x11); pos0 - type item list, #0; U:-1, D: 1, L:-1, R:-1
  !byte $05,$17,$01,$00,$FF,$FF,$02   ;pos(05/0x05,23/0x17); pos1 - type item list, #1; U: 0, D:-1, L:-1, R: 2
  !byte $0E,$15,$02,$FF,$FF,$01,$03   ;pos(14/0x0E,21/0x15); pos2 - type item list, #2; U:-1, D:-1, L: 1, R: 3
  !byte $1B,$17,$81,$05,$FF,$02,$04   ;pos(27/0x1B,23/0x17); pos3 - type link to,   #1; U: 5, D:-1, L: 2, R: 4
  !byte $22,$16,$83,$05,$03,$03,$FF   ;pos(34/0x22,22/0x16); pos4 - type link to  , #3; U: 5, D: 3, L: 3, R:-1
  !byte $1A,$0D,$03,$FF,$03,$FF,$04   ;pos(26/0x1A,15/0x0D); pos5 - type item list, #3; U:-1, D: 3, L:-1, R: 4
;1-kitchen1

;==========================================================
; CHARACTER SPRITE DATA
;==========================================================

* = $0900

player_character_right
  !byte $00,$54,$00,$03,$58,$00,$07,$68,$00,$07,$68,$00,$04,$68,$00,$04
  !byte $a8,$00,$00,$a0,$00,$01,$50,$00,$01,$fc,$00,$01,$fc,$00,$01,$fc
  !byte $00,$01,$fc,$00,$00,$e8,$00,$00,$e8,$00,$00,$fc,$00,$00,$a0,$00
  !byte $00,$a0,$00,$00,$a0,$00,$00,$a0,$00,$00,$fc,$00,$00,$fc,$00,$81

player_character_left
  !byte $00,$15,$00,$00,$25,$c0,$00,$29,$d0,$00,$29,$d0,$00,$29,$10,$00
  !byte $2a,$10,$00,$0a,$00,$00,$05,$40,$00,$3f,$40,$00,$3f,$40,$00,$3f
  !byte $40,$00,$3f,$40,$00,$2b,$00,$00,$2b,$00,$00,$3f,$00,$00,$0a,$00
  !byte $00,$0a,$00,$00,$0a,$00,$00,$0a,$00,$00,$3f,$00,$00,$3f,$00,$81

;==========================================================
; MAP IMAGE DATA
;==========================================================

* = $7000

map_data
!byte $01,$01,$01,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
!byte $0e,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
!byte $0e,$01,$01,$01,$01,$0e,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0e,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$05,$05,$05,$05,$01
!byte $01,$01,$01,$01,$08,$00,$01,$01,$00,$00,$00,$00,$00,$00,$01,$01
!byte $01,$01,$01,$01,$0e,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$05,$05,$05,$05,$01,$01,$01,$08,$00,$00,$01,$01
!byte $03,$00,$03,$01,$00,$01,$04,$04,$01,$01,$01,$01,$0e,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$08,$08,$08,$08
!byte $09,$01,$01,$08,$00,$00,$01,$01,$01,$00,$01,$03,$00,$03,$04,$04
!byte $01,$01,$01,$01,$0e,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$08,$00,$00,$08,$09,$01,$01,$08,$00,$00,$01,$01
!byte $03,$00,$03,$01,$00,$00,$04,$04,$01,$01,$01,$01,$0e,$01,$08,$08
!byte $01,$01,$01,$01,$01,$01,$08,$08,$01,$01,$01,$01,$08,$00,$00,$08
!byte $09,$01,$01,$08,$00,$00,$01,$01,$01,$00,$01,$00,$00,$01,$04,$04
!byte $01,$01,$01,$01,$0e,$01,$08,$09,$01,$03,$03,$03,$03,$01,$08,$09
!byte $01,$01,$01,$01,$08,$08,$08,$08,$09,$01,$01,$08,$00,$00,$01,$01
!byte $03,$00,$00,$01,$00,$03,$04,$04,$01,$01,$01,$01,$0e,$01,$08,$09
!byte $03,$01,$01,$01,$01,$03,$08,$09,$01,$01,$01,$01,$08,$07,$07,$08
!byte $09,$01,$01,$08,$00,$00,$01,$01,$00,$00,$01,$03,$00,$03,$04,$04
!byte $01,$01,$01,$01,$0e,$01,$03,$03,$01,$01,$01,$01,$01,$03,$03,$0e
!byte $01,$01,$01,$01,$08,$07,$00,$08,$09,$01,$01,$08,$00,$00,$01,$01
!byte $01,$00,$03,$01,$00,$00,$04,$04,$01,$01,$01,$01,$0e,$03,$03,$03
!byte $01,$01,$01,$01,$01,$03,$03,$0e,$01,$01,$01,$01,$08,$08,$08,$08
!byte $09,$01,$01,$08,$00,$00,$01,$01,$03,$00,$01,$00,$00,$00,$01,$04
!byte $01,$01,$01,$05,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$0e
!byte $05,$05,$05,$05,$08,$00,$00,$08,$09,$01,$01,$08,$00,$00,$01,$01
!byte $03,$00,$00,$00,$00,$01,$01,$01,$01,$05,$05,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$0e,$05,$0d,$0d,$05,$08,$00,$07,$08
!byte $09,$01,$01,$08,$00,$00,$01,$01,$00,$00,$00,$00,$01,$01,$01,$01
!byte $05,$0d,$03,$01,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$0e,$0e
!byte $05,$0d,$0d,$05,$08,$08,$08,$08,$09,$01,$01,$08,$00,$00,$01,$01
!byte $00,$00,$01,$01,$01,$01,$01,$05,$0d,$03,$01,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$0e,$0e,$05,$0d,$0d,$0d,$08,$07,$00,$08
!byte $09,$01,$01,$08,$00,$00,$01,$01,$00,$01,$01,$01,$01,$05,$05,$0d
!byte $03,$01,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$0e,$0e
!byte $05,$0d,$0d,$0d,$08,$00,$00,$08,$09,$01,$01,$08,$00,$00,$01,$01
!byte $01,$01,$01,$01,$05,$05,$0d,$03,$01,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$0e,$0e,$05,$0d,$0d,$0d,$09,$09,$09,$09
!byte $09,$01,$01,$08,$00,$00,$01,$01,$01,$01,$01,$05,$05,$0d,$0d,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$0e,$0e,$0e,$0e
!byte $05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$01,$01,$08,$00,$00,$01,$01
!byte $01,$01,$05,$05,$0d,$0d,$0d,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
!byte $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$01,$08,$00,$00,$01,$01,$08,$08,$08,$09,$0d,$0d,$0d,$08
!byte $08,$09,$05,$05,$06,$06,$06,$06,$06,$05,$05,$05,$05,$08,$08,$09
!byte $05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$08,$00,$00,$01,$01
!byte $08,$08,$09,$09,$0d,$0d,$0d,$08,$08,$09,$05,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$08,$08,$09,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$08,$00,$05,$01,$01,$09,$09,$09,$09,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$05,$01,$01
!byte $08,$08,$09,$09,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$05,$01,$01,$08,$08,$09,$09,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $02,$0d,$0d,$0d,$0d,$0d,$02,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$01,$01
!byte $08,$08,$09,$09,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$02,$0d,$0d,$0d,$0d,$0d,$02
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$01