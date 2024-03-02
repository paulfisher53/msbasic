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

LCD_E  = %10000000
LCD_RW = %01000000
LCD_RS = %00100000

SD_CS   = %00010000
SD_SCK  = %00001000
SD_MOSI = %00000100
SD_MISO = %00000010

PORTA_OUTPUTPINS = LCD_E | LCD_RW | LCD_RS | SD_CS | SD_SCK | SD_MOSI

.ifdef CONFIG_LCD
.include "lcd.s"
.endif

.ifdef CONFIG_SD

SDADDR = $5E                            ; SD Address Pointer
SDCURRSEC = $E3                         ; SD Current Sector  
FAT32BUFFER = $3E00      

.include "sd.s"
.include "fat32.s"

.endif

RESET:

                ldx #$ff                ; Set X to 0xff for the stack pointer
                sei                     ; Disable interrupts
                txs                     ; Clear the stack
                cld                     ; Clear decimal mode

                lda #%11111111          ; Set PORTB pins to output
                ldx #PORTA_OUTPUTPINS    ; Set various pins on port A to output
                jsr DDRINIT

                lda #<QT_BANNER
                ldy #>QT_BANNER
                jsr STROUT 

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

PARSELOADSAVE:
                jsr SKIPJUNK            ; By-pass junk
	            jsr SAVEFILENAME        ; Get/set file name
                rts

SKIPJUNK:	
                jsr CHRGOT
                bne SKIPJUNKDONE
                pla
                pla
SKIPJUNKDONE:	
                rts

SAVEFILENAME:
                jsr FRMEVL
                jsr FRESTR      ;length in .a
                ldx INDEX
                ldy INDEX+1

                stx FAT32_FNPOINTER
                sty FAT32_FNPOINTER+1
                rts

LOAD:

.ifdef CONFIG_SD

                lda #<QT_SEARCHING
                ldy #>QT_SEARCHING
                jsr STROUT         

                ; Parse arguments
                jsr PARSELOADSAVE

                ; Print filename
                txa
                jsr STROUT
                lda #<QT_CRLF
                ldy #>QT_CRLF
                jsr STROUT  

                jsr SDINIT
                jsr FAT32INIT
                bcc INITSUCCESS                

                ; SD init failed
                lda #<QT_SDINITFAILED
                ldy #>QT_SDINITFAILED
                jsr STROUT      

                ; Error during FAT32 initialization
                lda #'Z'
                jsr CHROUT
                lda FAT32_ERRSTAGE
                jsr PRINTHEX
                rts

INITSUCCESS:
                lda #<QT_LOADING
                ldy #>QT_LOADING
                jsr STROUT     

                ; Open root directory
                jsr FAT32OPENROOT

                ; Find file by name
                jsr FAT32FINDDIRENT
                bcc FOUNDFILE

                lda #<QT_FILENOTFOUND
                ldy #>QT_FILENOTFOUND
                jsr STROUT    
                rts

FOUNDFILE:
                ; Open file
                jsr FAT32OPENDIRENT

                ; Read file contents into program area
                lda #<RAMSTART2
                sta FAT32_ADDR
                lda #>RAMSTART2
                sta FAT32_ADDR+1

                jsr FAT32FILEREAD

.endif
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

PRINTHEX:
                pha
                ror
                ror
                ror
                ror
                jsr PRINTNYBBLE
                pla
                pha
                jsr PRINTNYBBLE
                pla
                rts
PRINTNYBBLE:
                and #15
                cmp #10
                bmi SKIPLETTER
                adc #6
SKIPLETTER:
                adc #48
                jsr CHROUT
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

QT_BANNER:
                .byte "BE6502 Computer",CR,LF,CR,LF   
                .byte "8000R = Microsoft BASIC",CR,LF,CR,LF
                .byte "$0400 - $3FFF = User RAM",CR,LF,0
QT_SEARCHING:
                .byte "SEARCHING FOR ",0        
QT_LOADING:
                .byte "LOADING",CR,LF,0
QT_CRLF:                
                .byte CR,LF,0          
QT_SDINITFAILED:
                .byte "SD INIT FAILED",CR,LF,0                                 
QT_FILENOTFOUND:
                .byte "FILE NOT FOUND",CR,LF,0                                                 

.include "vectors.s"

.include "wozmon.s"

.segment "RESETVEC"
                .word   $0F00           ; NMI vector
                .word   RESET           ; RESET vector
                .word   $0000           ; IRQ vector

