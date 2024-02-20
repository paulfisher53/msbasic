.setcpu "65C02"
.segment "BIOS"

E  = %10000000
RW = %01000000
RS = %00100000

TEXT_WOZ:
                .byte   "WOZMON <        "
                .byte   "BASIC           "

TEXT_BASIC:
                .byte   "WOZMON          "
                .byte   "BASIC <         "

LCDCLEAR:
                pha                     ; Save A
            
                tya                     ; Save Y
                pha

                ldy #$1f                ; Set counter to 31
                lda #$20

LCDCLEARLOOP:
                sta LCD_SCREEN,y        ; Store A (space char) at pos Y
                dey
                bne LCDCLEARLOOP

                sta LCD_SCREEN          ; Write last location manually

                pla                     ; Restore Y
                tay

                pla                     ; Restore A

                rts

LCDPRINT:
                ldx #0                  ; set offset to 0 as default
                jsr LCDPRINTOFF

                rts

LCDPRINTOFF:
                sta LCD_STR_BUFF
                sty LCD_STR_BUFF+1
                stx LCD_STR_OFFSET
                ldy #0

LCDPRINTLOOP:
                clc
                
                tya
                adc LCD_STR_OFFSET      ; Add offset to loop iterator
                tax                     ; and store in X

                lda (LCD_STR_BUFF),y    ; Load character at pos Y
                beq LCDPRINTEXIT        ; If we reach 0x00 exit loop

                sta LCD_SCREEN,x        ; Store character to video memory
                iny

                jmp LCDPRINTLOOP

LCDPRINTEXIT:
                jsr LCDRENDER               ; Render video memory to LCD
                rts

LCDINIT:
                lda #%00111000          ; Set 8 bit mode, 2 lines, 5x8 font
                jsr LCDSENDINST

                lda #%00001110          ; Set display on, cursor on, no blink
                jsr LCDSENDINST
                
                lda #%00000110          ; Increment and shift cursor
                jmp LCDSENDINST

LCDSETCUR:
                jmp LCDSENDINST

LCDSETCUR2:
                pha                     ; Save A

.ifndef CONFIG_EMULATOR
                lda #%11000000          ; Set cursor to line 2    
.else
                lda #%10100000          ; Set cursor to line 2  
.endif
        
                jsr LCDSENDINST

                pla                     ; Restore A
                rts

LCDRENDER:
                lda #%10000000          ; Set cursor to first line
                jsr LCDSETCUR                         
                ldx #0

LCDWRITECHAR:
                lda LCD_SCREEN,x        ; Load character from memory at pos X
                
                cpx #$10                ; If x = 16 write next line
                beq LCDNEXTLINE

                cpx #$20                ; If x = 32 return
                beq LCDRENDEREXIT

                jsr LCDSENDDATA         ; Send the character to LCD
                inx
                jmp LCDWRITECHAR

LCDNEXTLINE:
                jsr LCDSETCUR2          ; Set cursor to second line

                jsr LCDSENDDATA         ; Send the character to LCD  
                inx
                jmp LCDWRITECHAR

LCDRENDEREXIT:
                rts

LCDDELAY:
            lda #0                      ; Clear RS/RW/E bits
            sta PORTA

            lda #RW                     ; Set RW mode
            sta PORTA

            bit PORTB                   ; Read LCD data
            bpl LCDREADY

            lda #1                      ; Return busy flag
            rts

LCDREADY:
            lda #0                      ; Return ready flag
            rts

LCDSENDINST:
            pha                         ; Save A

LCDDELAYLOOP:
            jsr LCDDELAY
            bne LCDDELAYLOOP

            pla                         ; Restore A

            sta PORTB                   ; Write the A param (control byte)

            lda #E                      ; Set E mode
            sta PORTA

            lda #0                      ; Clear RS/RW/E bits
            sta PORTA

            rts

LCDSENDDATA:
            sta PORTB                   ; Write A param to PORTB

            lda #(RS | E)               ; Set RS and E mode
            sta PORTA

            lda #0                      ; Clear RS/RW/E bits
            sta PORTA

            rts
    