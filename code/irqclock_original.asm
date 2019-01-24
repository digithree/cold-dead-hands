;==========================================================
; IRQ clock
;
; A routine from 40 Best Machine Code Routines for C64
;
; Usage: SYS 28672,hours,minutes
;    where hours are in 24 hour format
;==========================================================

* = $7000

;==========================================================
; CODE
;==========================================================

entry

                jsr $AEFD
                jsr $B79E
                txa
                cmp #$18
                bcs $701F
                sta $70B7
                jsr $AEFD
                jsr $B79E
                txa
                cmp #$3C
                bcs $701F
                sta $70B8
                jmp $7022
                jmp $B248
                sei
                lda #$3F
                sta $0314
                lda #$70
                sta $0315
                lda $70B7
                lda #70B8
                lda #$00
                sta $70B9
                lda #$00
                sta $70BA
                cli
                rts
                inc $70BA
                lda $70BA
                cmp #$3C
                bcs $704C
                jmp $EA31
                lda #$00
                sta $70BA
                inc $70B9
                lda $70B9
                cmp #$3C
                bcs $705E
                jmp $708D
                lda #$00
                sta $70B9
                inc $70B8
                lda $70B8
                cmp #$3C
                bcs $7070
                jmp $708D
                lda #$00
                sta $70B8
                inc $70B7
                lda $70B7
                cmp #$18
                bcc $708D
                lda #$00
                sta $70B9
                sta $70B8
                sta $70B8
                jmp $EA31
                lda #$13
                jsr $FFD2
                lda #$00
                ldx $70B7
                jsr $BDCD
                lda #$3A
                jsr $FFD2
                lda #$00
                ldx $70B8
                jsr $BDCD
                lda #$3A
                jsr $FFD2
                lda #$00
                ldx $70B9
                jsr $BDCD
                jmp $EA31
                brk
                brk
                brk
                brk
