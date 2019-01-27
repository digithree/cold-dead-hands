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
FN_GETIN        = $FFE4

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
  lda #$0A        ; set main color to 1 - WHITE for sprite 0
  sta $D027
  lda #$09        ; set multi-color 1 to 9 - BROWN
  sta $D025
  lda #$02        ; set multi-color 2 to 2 - RED
  sta $D026
  lda #$01        ; turn on bit 0 of multi-color mode indicator, sprite 0 to multicolor mode only
  sta $D01C
  lda #$03
  sta $D01D       ; bit 0 and 1 on for horiztonal expand for sprite 0 and 1
  sta $D017       ; bit 0 and 1 on for vertical expand for sprite 0 and 1
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
  jsr draw_screen
setup_sprite
  ; TODO
position_loop
  ; get current position info for map
  lda #$60          ; store high byte of base positions for map infos ($60XX) in $16 / $17, for further look up
  sta $17
  ; move pointer to correct area for map var_map_cur
  lda #$00 ;offset counter to 00
  ldx var_map_cur
  inx
position_loop_add_map_loop
  dex
  beq position_loop_add_map_loop_exit
  clc
  adc #$30 ;size of positions in a single map
  jmp position_loop_add_map_loop
position_loop_add_map_loop_exit
  sta $16           ; store LOW byte of base positions for map infos ($60XX) in $16 / $17, for further look up
  ; z$16 now at position 0 for map var_map_cur
calc_map_position
  lda #$00          ; zero A for position offset, to be stored in Y and saved in $18
  ldx var_map_pos
position_loop_add_pos_loop
  cpx #$00
  beq position_loop_add_pos_loop_exit
  clc
  adc #$08 ;size of single position entry
  dex
  jmp position_loop_add_pos_loop
position_loop_add_pos_loop_exit
  tay                           ; result of looped addition A -> Y, for pointer offset usage
  sty $18                       ; temp store Y (position offset) -> Zero page $18
set_sprite_position
  ldy $18                       ; load this position pointer offset
  lda ($16), Y                  ; load scr-x value of position, multiply by 8, add 32 for offset, store in sprite 0 X mem loc
  asl
  asl
  ;if Y*8 > 256 then need to set MSB sprite register at $D010
  tax ;save A -> X
  and #$80  ;mas A with 10000000
  cmp #$80
  beq set_sprite_position_x_second_pos_set   ; set MSB if next shift will require more than 8 bits, if not, clear MSB
  lda #$00                      ; clear MSB register for sprite 0 only
  sta $D010
  txa ; restore X -> A
  asl
  jmp set_sprite_position_y
set_sprite_position_x_second_pos_set
  lda #$01                      ; set MSB register for sprite 0 only
  sta $D010
  txa ; restore X -> A
  asl
set_sprite_position_y
  clc
  adc #$08
  sta $D000
  iny                           ; next value
  lda ($16), Y                  ; load scr-y value of position, multiply by 8, add 48, add store in sprite 0 Y mem loc
  asl
  asl
  asl
  clc
  adc #$10
  sta $D001
  lda $D015                     ; finally, turn sprite on, OR sprite enable reg with 0x01, bit 0 for sprite 0
  ora #$01
  sta $D015
;have_some_heart
;  lda #$53      ; heart character
;  sta $05F4
;  lda #$02      ; red foreground
;  sta $D9F4
  ; *** MAIN LOOP
  iny
  ; note that Y will be base+2 at this point
  ; save Y value to z$19
  sty $19
map_screen_wait_for_input
  jsr FN_GETIN
  cmp #$00
  beq map_screen_wait_for_input
  cmp #$57        ; check if W, up
  beq map_screen_input_up
  cmp #$53        ; check if S, down
  beq map_screen_input_down
  cmp #$41        ; check if A, left
  beq map_screen_input_left
  cmp #$44        ; check if D, right
  beq map_screen_input_right
  cmp #$20        ; check if space, interact
  beq map_screen_wait_for_input
  jmp map_screen_wait_for_input
map_screen_input_right ; Y to up data using passtrough technique
  inc $19
map_screen_input_left
  inc $19
map_screen_input_down
  inc $19
map_screen_input_up
  inc $19         
map_screen_input_read
  ldy $19
  lda ($16), Y        ; load new position
  cmp #$FF            ; if FF then can't move
  beq calc_map_position   ; recalc, even no change
  sta var_map_pos
  jmp calc_map_position
map_screen_input_action
  ; TODO : interaction
  jmp map_screen_wait_for_input

infinite_loop_unused
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

map_meta_data
;0-bedroom, 1 + (6 * 8) = 48 bytes (0x30) of info
  !byte $05,$11,$00,$FF,$01,$FF,$FF,$00   ;pos(05/0x05,17/0x11); pos0 - type item list, #0; U:-1, D: 1, L:-1, R:-1; empty
  !byte $05,$17,$01,$00,$FF,$FF,$02,$00   ;pos(05/0x05,23/0x17); pos1 - type item list, #1; U: 0, D:-1, L:-1, R: 2; empty
  !byte $0E,$15,$02,$FF,$FF,$01,$03,$00   ;pos(14/0x0E,21/0x15); pos2 - type item list, #2; U:-1, D:-1, L: 1, R: 3; empty
  !byte $1B,$17,$81,$05,$FF,$02,$04,$00   ;pos(27/0x1B,23/0x17); pos3 - type link to,   #1; U: 5, D:-1, L: 2, R: 4; empty
  !byte $22,$16,$83,$05,$03,$03,$FF,$00   ;pos(34/0x22,22/0x16); pos4 - type link to  , #3; U: 5, D: 3, L: 3, R:-1; empty
  !byte $1A,$0D,$03,$FF,$03,$FF,$04,$00   ;pos(26/0x1A,15/0x0D); pos5 - type item list, #3; U:-1, D: 3, L:-1, R: 4; empty
;1-kitchen1
  !byte $02,$17,$00,$01,$FF,$FF,$01,$00   ;pos(02/0x02,23/0x17); pos0 - type item list, #0; U:xx, D:xx, L:xx, R:xx; empty
  !byte $09,$15,$01,$02,$00,$00,$02,$00   ;pos(09/0x09,21/0x15); pos1 - type item list, #1; U:xx, D:xx, L:xx, R:xx; empty
  !byte $0C,$11,$02,$FF,$01,$01,$04,$00   ;pos(12/0x0C,17/0x11); pos2 - type item list, #2; U:xx, D:xx, L:xx, R:xx; empty
  !byte $14,$10,$81,$FF,$FF,$02,$04,$00   ;pos(20/0x14,16/0x10); pos3 - type link to,   #1; U:xx, D:xx, L:xx, R:xx; empty
  !byte $1A,$13,$83,$FF,$05,$03,$05,$00   ;pos(26/0x1A,19/0x13); pos4 - type link to  , #3; U:xx, D:xx, L:xx, R:xx; empty
  !byte $22,$17,$03,$04,$FF,$04,$FF,$00   ;pos(34/0x22,23/0x17); pos5 - type item list, #3; U:xx, D:xx, L:xx, R:xx; empty
;1-kitchen2
  !byte $04,$17,$00,$01,$FF,$FF,$01,$00   ;pos(04/0x04,23/0x17); pos0 - type item list, #0; U:xx, D:xx, L:xx, R:xx; empty
  !byte $0D,$14,$01,$02,$00,$00,$02,$00   ;pos(13/0x0D,20/0x14); pos1 - type item list, #1; U:xx, D:xx, L:xx, R:xx; empty
  !byte $0F,$11,$02,$FF,$01,$01,$03,$00   ;pos(15/0x0F,17/0x11); pos2 - type item list, #2; U:xx, D:xx, L:xx, R:xx; empty
  !byte $1B,$11,$81,$FF,$04,$02,$04,$00   ;pos(27/0x1B,17/0x11); pos3 - type link to,   #1; U:xx, D:xx, L:xx, R:xx; empty
  !byte $1B,$14,$83,$03,$05,$03,$05,$00   ;pos(27/0x1B,20/0x14); pos4 - type link to  , #3; U:xx, D:xx, L:xx, R:xx; empty
  !byte $22,$17,$03,$04,$FF,$04,$FF,$00   ;pos(34/0x22,23/0x17); pos5 - type item list, #3; U:xx, D:xx, L:xx, R:xx; empty
;1-bathroom
  !byte $01,$17,$00,$FF,$FF,$FF,$01,$00   ;pos(01/0x01,23/0x17); pos0 - type item list, #0; U:xx, D:xx, L:xx, R:xx; empty
  !byte $07,$14,$01,$02,$FF,$00,$02,$00   ;pos(07/0x07,20/0x14); pos1 - type item list, #1; U:xx, D:xx, L:xx, R:xx; empty
  !byte $0A,$12,$02,$FF,$01,$01,$03,$00   ;pos(10/0x0A,18/0x12); pos2 - type item list, #2; U:xx, D:xx, L:xx, R:xx; empty
  !byte $13,$12,$81,$FF,$04,$02,$04,$00   ;pos(19/0x13,18/0x12); pos3 - type link to,   #1; U:xx, D:xx, L:xx, R:xx; empty
  !byte $1A,$16,$83,$03,$05,$03,$05,$00   ;pos(26/0x1A,22/0x16); pos4 - type link to  , #3; U:xx, D:xx, L:xx, R:xx; empty
  !byte $21,$17,$03,$04,$FF,$04,$FF,$00   ;pos(33/0x21,23/0x17); pos5 - type item list, #3; U:xx, D:xx, L:xx, R:xx; empty

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

map_data_bedroom
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

* = $8000

map_data_kitchen1
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$03,$08,$08,$08,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01
!byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$03,$08,$00,$00,$01
!byte $01,$01,$01,$01,$01,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$0f,$0f,$0f,$01,$01,$02,$02,$01,$0a,$01,$03,$01
!byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$01,$01,$0f,$0b,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0f,$01
!byte $01,$02,$02,$06,$0a,$01,$03,$01,$01,$01,$01,$03,$08,$00,$00,$01
!byte $03,$03,$03,$03,$0f,$0b,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$0f,$03,$03,$02,$02,$06,$0a,$03,$03,$01
!byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$01,$01,$0f,$0b,$09,$09
!byte $09,$0f,$0b,$0b,$0f,$0b,$0b,$0f,$09,$0f,$0c,$0b,$0b,$0c,$0f,$0f
!byte $09,$02,$02,$06,$0a,$09,$03,$01,$01,$01,$01,$03,$08,$00,$00,$01
!byte $01,$01,$01,$09,$0b,$0b,$09,$09,$09,$0f,$0f,$0f,$0f,$0f,$0f,$0f
!byte $0f,$09,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$09,$09,$09,$09,$09,$09,$03
!byte $01,$01,$01,$03,$08,$00,$00,$01,$01,$01,$09,$09,$09,$09,$09,$09
!byte $09,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$09,$09,$09,$09,$09,$09,$09
!byte $09,$09,$09,$09,$09,$09,$09,$09,$03,$01,$01,$03,$08,$00,$00,$01
!byte $01,$09,$09,$09,$09,$09,$09,$09,$08,$0b,$01,$01,$01,$01,$01,$01
!byte $01,$09,$08,$08,$07,$08,$08,$09,$08,$08,$09,$08,$09,$09,$09,$09
!byte $09,$03,$01,$03,$08,$00,$00,$01,$09,$09,$09,$09,$09,$09,$09,$09
!byte $08,$0b,$01,$00,$00,$00,$00,$00,$01,$09,$08,$08,$08,$08,$08,$09
!byte $08,$08,$09,$08,$08,$09,$09,$09,$09,$09,$03,$03,$08,$00,$00,$01
!byte $09,$03,$03,$03,$03,$0e,$09,$09,$08,$0b,$01,$00,$00,$00,$00,$00
!byte $01,$09,$09,$09,$09,$09,$09,$09,$08,$08,$09,$08,$08,$09,$09,$09
!byte $09,$09,$09,$03,$08,$00,$00,$01,$03,$03,$03,$03,$0e,$09,$09,$08
!byte $08,$0b,$01,$00,$00,$00,$00,$00,$01,$09,$08,$08,$08,$08,$08,$09
!byte $08,$08,$09,$08,$08,$09,$08,$09,$09,$09,$09,$03,$08,$00,$00,$01
!byte $0e,$0e,$0e,$0e,$0e,$09,$09,$08,$08,$0b,$01,$01,$01,$01,$01,$01
!byte $01,$09,$08,$08,$08,$08,$08,$09,$08,$08,$09,$08,$08,$09,$08,$09
!byte $09,$09,$09,$03,$08,$00,$00,$01,$09,$09,$09,$09,$09,$09,$09,$05
!byte $05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05,$05
!byte $05,$05,$05,$08,$08,$09,$08,$08,$09,$09,$09,$03,$08,$00,$00,$01
!byte $09,$09,$09,$09,$09,$09,$09,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$08,$09,$08,$08
!byte $09,$08,$08,$03,$08,$00,$00,$01,$08,$05,$05,$05,$05,$08,$08,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$08,$09,$08,$08,$09,$08,$08,$03,$08,$00,$00,$01
!byte $08,$05,$05,$05,$05,$08,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$09,$08,$08
!byte $09,$08,$08,$03,$08,$00,$00,$01,$08,$05,$05,$05,$05,$08,$08,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$09,$08,$08,$09,$08,$08,$03,$08,$00,$00,$01
!byte $08,$05,$05,$05,$05,$08,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$08,$08
!byte $09,$08,$08,$03,$08,$00,$00,$01,$08,$05,$05,$05,$0d,$08,$08,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$08,$08,$09,$08,$08,$03,$08,$00,$00,$01
!byte $08,$0d,$0d,$0d,$0d,$08,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$08
!byte $09,$08,$08,$03,$08,$00,$05,$01,$08,$0d,$0d,$0d,$0d,$08,$08,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$05,$05,$05,$05,$05,$05,$05,$01
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$01

* = $9000

map_data_kitchen2
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$03,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$03,$01,$01,$01,$02,$02,$01,$01,$0d,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0c,$0c,$0c,$0c
!byte $0c,$0c,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$08,$08,$01,$01,$01,$01
!byte $01,$01,$03,$01,$0c,$0c,$0c,$0c,$0c,$0c,$0b,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$08,$08,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0c,$0c,$0b,$0b
!byte $0c,$0c,$0b,$01,$01,$01,$01,$05,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$08,$08,$08,$01,$01,$01,$01
!byte $01,$01,$03,$01,$0c,$0c,$0c,$0c,$0c,$0c,$0b,$01,$01,$01,$01,$05
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$08,$08,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0b,$0b,$0b,$0b
!byte $0b,$0b,$0b,$01,$0d,$01,$01,$05,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$08,$08,$08,$01,$01,$01,$01
!byte $01,$01,$03,$01,$0c,$0c,$0c,$0c,$0c,$0c,$0b,$01,$05,$0d,$01,$05
!byte $01,$0d,$05,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0b,$01,$01,$01
!byte $01,$08,$08,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0c,$0c,$0c,$0c
!byte $0c,$0c,$0b,$01,$05,$05,$0d,$05,$01,$05,$01,$02,$02,$01,$01,$01
!byte $01,$01,$01,$01,$00,$0b,$01,$01,$01,$08,$08,$08,$01,$01,$01,$01
!byte $01,$01,$03,$01,$0c,$0c,$0b,$0b,$0c,$0c,$0b,$01,$01,$05,$05,$0d
!byte $05,$05,$01,$02,$02,$02,$01,$01,$01,$01,$01,$01,$00,$00,$0b,$01
!byte $01,$08,$08,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0c,$0c,$0c,$0c
!byte $0c,$0c,$0b,$01,$01,$01,$05,$05,$05,$01,$01,$02,$02,$02,$01,$08
!byte $08,$08,$08,$08,$00,$00,$00,$0b,$01,$08,$08,$08,$01,$01,$01,$01
!byte $01,$01,$03,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$0e,$05,$05
!byte $0e,$01,$01,$02,$02,$02,$01,$09,$08,$08,$0b,$0b,$0b,$00,$00,$0b
!byte $01,$08,$08,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0c,$0c,$0c,$0c
!byte $0c,$0c,$0b,$01,$01,$0e,$0e,$0e,$04,$01,$01,$01,$00,$01,$01,$09
!byte $01,$08,$08,$0b,$0b,$0b,$00,$0b,$01,$08,$08,$08,$01,$01,$01,$01
!byte $01,$01,$03,$01,$0c,$0c,$0c,$0c,$0c,$0c,$0b,$01,$01,$0e,$04,$04
!byte $04,$01,$01,$01,$00,$01,$01,$09,$01,$01,$08,$08,$0b,$0b,$0b,$0b
!byte $01,$08,$08,$08,$01,$01,$01,$01,$01,$01,$03,$01,$0c,$0c,$0b,$0b
!byte $0c,$0c,$0b,$01,$01,$0e,$04,$04,$04,$01,$01,$01,$00,$02,$02,$02
!byte $02,$01,$01,$08,$08,$08,$08,$08,$01,$08,$07,$08,$01,$01,$01,$01
!byte $08,$08,$08,$08,$09,$0c,$0c,$0c,$0c,$0c,$0b,$05,$05,$0e,$04,$04
!byte $04,$05,$05,$05,$00,$02,$02,$02,$02,$02,$05,$05,$08,$08,$08,$08
!byte $01,$08,$08,$08,$01,$01,$08,$08,$08,$08,$08,$09,$09,$05,$05,$05
!byte $05,$05,$05,$05,$05,$0d,$04,$04,$05,$05,$05,$05,$0d,$00,$02,$02
!byte $02,$02,$05,$05,$08,$08,$08,$08,$01,$08,$08,$08,$01,$08,$08,$08
!byte $08,$08,$09,$09,$00,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$05
!byte $05,$05,$05,$0d,$0d,$0d,$0d,$00,$0d,$0d,$05,$05,$09,$05,$08,$08
!byte $01,$08,$08,$08,$01,$08,$08,$08,$08,$08,$09,$00,$00,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00
!byte $0d,$0d,$0d,$0d,$09,$05,$05,$05,$01,$08,$08,$08,$01,$08,$00,$00
!byte $00,$08,$09,$00,$09,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$00,$00,$00,$00,$00,$0d,$0d,$09,$0d,$05,$05
!byte $01,$08,$08,$08,$05,$08,$00,$00,$00,$08,$09,$09,$05,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$05,$05
!byte $05,$00,$05,$0d,$09,$0d,$0d,$05,$01,$08,$08,$08,$05,$08,$08,$08
!byte $08,$08,$09,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$05,$05,$0d,$09,$0d,$0d,$0d
!byte $01,$08,$08,$08,$05,$05,$05,$05,$05,$05,$05,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$09,$05,$05,$05,$01,$08,$08,$05,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $01,$08,$05,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$01

* = $A000

map_data_bathroom
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$08,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$08,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0c,$0c
!byte $0c,$0c,$0c,$0c,$0c,$0c,$0c,$0d,$0d,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$0d,$0d,$0d,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0c,$0d,$0d,$01,$01,$0c,$0d,$0d,$0c,$0d
!byte $0d,$01,$03,$03,$03,$01,$03,$03,$03,$01,$0d,$0d,$0d,$08,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$08,$08,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$01,$0c,$0d,$0d,$0c,$0d,$0d,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$0d,$0d,$0a,$0a,$0a,$0a,$0e,$0e,$02,$02,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0d,$01,$01,$01,$0c,$01,$0d,$0c,$0d
!byte $0d,$01,$03,$03,$03,$01,$03,$03,$03,$01,$0d,$0d,$0a,$0a,$0a,$0a
!byte $06,$06,$02,$02,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$0d,$0c,$0d,$0d,$0c,$0d,$0d,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$0d,$0d,$0a,$0a,$0a,$0a,$06,$06,$02,$02,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0d,$0d,$08,$0d,$0c,$0d,$0d,$0c,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0a,$0a,$0a,$0a
!byte $0e,$0e,$02,$02,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$0d,$0c,$0d,$0d,$0c,$0d,$0d,$0d,$0d,$01,$01,$01,$01,$01
!byte $0d,$0d,$0d,$0d,$0a,$0a,$0a,$0a,$06,$0e,$02,$02,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0d,$0d,$08,$0d,$0c,$0d,$0d,$0c,$0d
!byte $0d,$0d,$0d,$01,$01,$01,$0c,$0c,$0d,$0d,$0d,$0d,$0a,$0a,$0a,$0a
!byte $06,$06,$02,$02,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$0d,$0c,$0d,$0d,$0c,$0d,$0d,$0d,$0d,$01,$01,$01,$01,$01
!byte $0d,$0d,$0d,$0d,$0d,$08,$0d,$0d,$0e,$06,$02,$02,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0d,$0d,$08,$0d,$0c,$0d,$0d,$0c,$0d
!byte $0d,$0d,$0d,$0d,$0d,$01,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$08,$0d,$0d
!byte $0d,$0e,$02,$02,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$0d,$0c,$0d,$0d,$0c,$0d,$0d,$0d,$0d,$01,$01,$01,$01,$01
!byte $0d,$0d,$0d,$0d,$0d,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0d,$0d,$08,$0d,$0c,$0d,$0d,$0c,$0d
!byte $0d,$0d,$0f,$0f,$0f,$0f,$0f,$01,$0d,$0d,$0d,$0d,$0d,$08,$0d,$0d
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$0d,$0c,$0d,$0d,$0c,$0d,$0d,$0d,$01,$01,$01,$01,$01,$01
!byte $0d,$0d,$0d,$0d,$0d,$08,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0d,$0d,$08,$0d,$0c,$0d,$0d,$0c,$0d
!byte $0d,$0d,$0d,$0d,$01,$01,$01,$0d,$0d,$0d,$0d,$01,$01,$01,$01,$01
!byte $01,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0d
!byte $0d,$08,$0b,$0c,$0b,$0b,$0c,$0e,$0e,$0e,$0e,$0e,$01,$01,$01,$0e
!byte $0e,$0e,$01,$01,$0f,$0f,$0f,$0f,$0f,$01,$01,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0d,$0c,$0b,$0b,$0b,$0b,$0c,$0b,$0c,$03,$03
!byte $03,$03,$03,$01,$01,$01,$01,$03,$03,$03,$03,$01,$01,$01,$01,$01
!byte $01,$01,$01,$0d,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0d,$0d,$0c,$0c
!byte $0c,$0c,$0c,$0c,$0c,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$01,$01,$01,$01,$01,$0f,$01,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0d,$0d,$0e,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$0f,$0f
!byte $0f,$01,$01,$0d,$0d,$0d,$0d,$0d,$00,$00,$08,$0d,$0e,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$01,$01,$01,$01,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$08,$0e,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$01
!byte $01,$01,$01,$0d,$0d,$0d,$0d,$0d,$00,$00,$08,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$01,$01,$01,$01,$0d,$0d,$0d,$0d,$0d
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$01,$01
!byte $01,$01,$01,$0e,$0d,$0d,$0d,$0d,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$0e,$0d,$0d,$0d
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
!byte $03,$03,$03,$03,$03,$0e,$0d,$0d