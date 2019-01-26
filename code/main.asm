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

+start_at $0900

* = $0900

;==========================================================
; CODE
;==========================================================

entry_and_first_time_setup
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
position_loop
  ; TODO - the rest
have_some_heart
  lda #$53      ; heart character
  sta $05F4
  lda #$02      ; red foreground
  sta $D9F4

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
const_map_meta_base
  !byte $00,$60 ; $6000
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
;==========================================================

* = $7000

map_data

;0-bedroom, 1 + (6 * 7) = 43 bytes of info
!byte $06   ; 6 positions in this map
!byte $00,$05,$11,$FF,$01,$FF,$FF   ;pos0 - type item list, #0; pos(05/0x05,17/0x11); U:-1, D: 1, L:-1, R:-1
!byte $01,$05,$17,$00,$FF,$FF,$02   ;pos1 - type item list, #1; pos(05/0x05,23/0x17); U: 0, D:-1, L:-1, R: 2
!byte $02,$0E,$15,$FF,$FF,$01,$03   ;pos2 - type item list, #2; pos(14/0x0E,21/0x15); U:-1, D:-1, L: 1, R: 3
!byte $81,$1B,$17,$05,$FF,$02,$04   ;pos3 - type link to,   #1; pos(27/0x1B,23/0x17); U: 5, D:-1, L: 2, R: 4
!byte $83,$22,$16,$05,$03,$03,$FF   ;pos4 - type link to  , #3; pos(34/0x22,22/0x16); U: 5, D: 3, L: 3, R:-1
!byte $03,$1A,$0D,$FF,$03,$FF,$04   ;pos5 - type item list, #3; pos(26/0x1A,15/0x0D); U:-1, D: 3, L:-1, R: 4
;1-kitchen1

;==========================================================
; MAP IMAGE DATA
;==========================================================

* = $7000

map_data
!byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$00,$00,$00,$00,$00,$00
!byte $00,$00,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$0a,$0b,$0c,$0d,$0e,$0f,$00,$00
!byte $00,$00,$00,$00,$00,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02,$02,$02,$02
!byte $0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$02,$02,$06,$06,$06,$06,$06,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$06,$02,$06,$02,$06,$06
!byte $02,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $02,$02,$06,$02,$06,$02,$06,$06,$02,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$06,$06,$06,$06,$06,$06
!byte $02,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$02,$02,$06,$06,$06,$06,$02,$02,$0d,$0d,$0d,$0d,$0d,$0d,$0d
!byte $0d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02,$02,$02,$02,$02
!byte $00,$0d,$0d,$0d,$0d,$0d,$0d,$0d,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0d,$0d,$0d,$0d,$0d,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00