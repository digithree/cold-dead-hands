;==========================================================
; IRQ clock
;
; A routine from 40 Best Machine Code Routines for C64
;
; Usage: SYS 28672,hours,minutes
;    where hours are in 24 hour format
;==========================================================

;==========================================================
; SYSTEM AND KERNAL ROUTINES
;==========================================================

STD_INT         = $EA31     ; KERNAL standard interrupt service routine to handle keyboard scan, cursor display etc.
FN_SCR_WRITE_F  = $BDCD     ; Write integer value in A/X onto screen, in floating-point format.
FN_CHROUT       = $FFD2     ; Write byte to default output, input A = byte to write
FN_CHKCOM       = $AEFD     ; BASIC param, checks that the next character in the command is a comma
FN_GETBYT       = $B79E     ; BASIC param, reads an unsigned byte parameter, into X

;==========================================================
; ADDRESSES
;==========================================================

IRQ_INT_LOW     = $0314     ; Vector Hardware IRQ Interrrupt address, low byte
IRQ_INT_HIGH    = $0315     ; Vector Hardware IRQ Interrrupt address, high byte

;==========================================================
; CODE
;==========================================================

* = $7000     ; set area of memory to write code

; setup part creates the timer from params from BASIC, then returns to BASIC when done
setup
                jsr FN_CHKCOM
                jsr FN_GETBYT
                txa
                cmp #$18
                bcs label_701F
                sta hour
                jsr FN_CHKCOM
                jsr FN_GETBYT
                txa
                cmp #$3C
                bcs label_701F
                sta minute
                jmp label_7022
label_701F
                jmp $B248
label_7022
                sei                 ; set interupt flag. preventing CPU from responding to IRQ interrupt events
                lda #$3F            ; low byte part of address $703F, to call on interrupt
                sta IRQ_INT_LOW
                lda #$70            ; high byte part of address $703F, to call on interrupt
                sta IRQ_INT_HIGH
                lda hour
                lda minute
                lda #$00
                sta second
                lda #$00
                sta counter
                cli                 ; clear interrupt flag, re-enable CPU response to IRQ
                rts

; rest of program, from 703F (28735) is called on interupt
call_on_int
                inc counter
                lda counter
                cmp #$3C
                bcs label_704C
                jmp STD_INT
label_704C
                lda #$00
                sta counter
                inc second
                lda second
                cmp #$3C
                bcs label_705E
                jmp label_708D
label_705E
                lda #$00
                sta second
                inc minute
                lda minute
                cmp #$3C
                bcs label_7070
                jmp label_708D
label_7070
                lda #$00
                sta minute
                inc hour
                lda hour
                cmp #$18
                bcc label_708D
                lda #$00
                sta second
                sta minute
                sta minute
                jmp STD_INT
label_708D
                lda #$13              ; clear screen (cursor to top left)
                jsr FN_CHROUT
                lda #$00
                ldx hour
                jsr FN_SCR_WRITE_F
                lda #$3A
                jsr FN_CHROUT
                lda #$00
                ldx minute
                jsr FN_SCR_WRITE_F
                lda #$3A
                jsr FN_CHROUT
                lda #$00
                ldx second
                jsr FN_SCR_WRITE_F
                jmp STD_INT

; place working memory (kind of variables) at end of program code
hour            !byte 0              ; hour storage
minute          !byte 0              ; minute storage
second          !byte 0              ; second storage
counter         !byte 0              ; counter storage
