.setcpu "65C02"
.debuginfo
.segment "BIOS"

ACIA_DATA	= $5000
ACIA_STATUS	= $5001
ACIA_CMD	= $5002
ACIA_CTRL	= $5003

PORTB = $6000                           ; VIA Port B
PORTA = $6001                           ; VIA Port A
DDRB = $6002                            ; Data Direction Register B
DDRA = $6003                            ; Data Direction Register A

LCD_SCREEN = $03E0                      ; LCD Screen Memory - 32 bytes (2 rows, 16 columns)

SAREG = $30C                            ; Storage Area for .A Register (Accumulator)
SXREG = $30D                            ; Storage Area for .X Index Register
SYREG = $30E                            ; Storage Area for .Y Index Register
SPREG = $30F                            ; Storage Area for .P (Status) Register

.ifdef CONFIG_LCD
.include "lcd.s"
.endif

RESET:

                ldx #$ff                ; Set X to 0xff for the stack pointer
                sei                     ; Disable interrupts
                txs                     ; Clear the stack
                cld                     ; Clear decimal mode

                lda #%11111111          ; Set PORTB pins to output
                ldx #%11100000          ; Set firt 3 pins for PORTA to ouput
                jsr DDRINIT

.ifdef CONFIG_LCD
                jsr LCDINIT
.endif
                jsr CLRRAM              ; Clear RAM

                jmp WOZMON              ; Boot into WOZMON

DDRINIT:
                sta DDRB                ; Configure DDRB from A param
                stx DDRA                ; Configure DDRA from X param

                rts
            
CLRRAM:

                lda #0                  ; Clear A
                tay                     ; Clear Index
CLRRAMLOOP:
                sta $0000,y             ; Clear Page 0
                sta $0200,y             ; Clear Page 2
                sta $0300,y             ; Clear Page 3
                iny
                bne CLRRAMLOOP
                rts

LOAD:
                rts

SAVE:
                rts

SYS:                
                jsr FRMNUM              ; Eval formula
                jsr GETADR              ; Convert to int. addr
                lda #>SYSRETURN         ; Push return address
                pha
                lda #<SYSRETURN
                pha
                lda SPREG               ; Status reg
                pha
                lda SAREG               ; Load 6502 regs
                ldx SXREG
                ldy SYREG
                plp                     ; Load 6502 status reg
                jmp (LINNUM)           ; Go do it
SYSRETURN=*-1                
                php                     ; Save status reg
                sta SAREG               ; Save 6502 regs
                stx SXREG
                sty SYREG
                pla                     ; Get status reg
                sta SPREG
                rts 

; Input a character from the serial interface.
; On return, carry flag indicates whether a key was pressed
; If a key was pressed, the key value will be in the A register
;
; Modifies: flags, A
MONRDKEY:
CHRIN:
                lda     ACIA_STATUS
                and     #$08
                beq     @no_keypressed
                lda     ACIA_DATA
                jsr     CHROUT			; echo
                sec
                rts
@no_keypressed:
                clc
                rts


; Output a character (from the A register) to the serial interface.
;
; Modifies: flags
MONCOUT:
CHROUT:
                pha
                sta     ACIA_DATA
                lda     #$FF
@txdelay:       dec
                bne     @txdelay
                pla
                rts

.include "vectors.s"

.include "wozmon.s"

.segment "RESETVEC"
                .word   $0F00           ; NMI vector
                .word   RESET           ; RESET vector
                .word   $0000           ; IRQ vector

