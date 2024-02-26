.setcpu "65C02"
.segment "BIOS"

E  = %10000000
RW = %01000000
RS = %00100000

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

LCDINIT:
                lda #%00111000          ; Set 8 bit mode, 2 lines, 5x8 font
                jsr LCDSENDINST

                lda #%00001110          ; Set display on, cursor on, no blink
                jsr LCDSENDINST
                
                lda #%00000110          ; Increment and shift cursor
                jsr LCDSENDINST

                lda #$00000001          ; Clear display     
                jsr LCDSENDINST
                
                rts           

LCDSETCUR:
                jmp LCDSENDINST

LCDSETCUR2:
                pha                     ; Save A
                lda #%11000000          ; Set cursor to line 2    
        
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
    