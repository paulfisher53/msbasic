.setcpu "65C02"
.segment "BIOS"

SDINIT:
  
                lda     #<QT_SD_SEARCHING
                ldy     #>QT_SD_SEARCHING
                jsr     STROUT

                lda #SD_CS | SD_MOSI
                ldx #160                ; toggle the clock 160 times, so 80 low-high transitions
SDINITLOOP:
                eor #SD_SCK
                sta PORTA
                dex
                bne SDINITLOOP

SDCMD0:                                 ; GO_IDLE_STATE - resets card to idle state, and SPI mode
                lda #<SD_CMD0_BYTES
                sta SDADDR
                lda #>SD_CMD0_BYTES
                sta SDADDR+1

                jsr SDSENDCMD
                
                cmp #$01                ; Expect status response $01 (not initialized)
                bne SDINITFAIL

SDCMD8:                                 ; SEND_IF_COND - tell the card how we want it to operate (3.3V, etc)
                lda #<SD_CMD8_BYTES
                sta SDADDR
                lda #>SD_CMD8_BYTES
                sta SDADDR+1

                jsr SDSENDCMD

                cmp #$01                ; Expect status response $01 (not initialized)
                bne SDINITFAIL

                ; Read 32-bit return value, but ignore it
                jsr SDREADBYTE
                jsr SDREADBYTE
                jsr SDREADBYTE
                jsr SDREADBYTE

SDCMD55:                                ; APP_CMD - required prefix for ACMD commands
                lda #<SD_CMD55_BYTES
                sta SDADDR
                lda #>SD_CMD55_BYTES
                sta SDADDR+1

                jsr SDSENDCMD
                
                cmp #$01                ; Expect status response $01 (not initialized)
                bne SDINITFAIL

SDCMD41:                                ; APP_SEND_OP_COND - send operating conditions, initialize card
                lda #<SD_CMD41_BYTES
                sta SDADDR
                lda #>SD_CMD41_BYTES
                sta SDADDR+1

                jsr SDSENDCMD

                ; Status response $00 means initialised
                cmp #$00
                beq SDINITOK

                ; Otherwise expect status response $01 (not initialized)
                cmp #$01
                bne SDINITFAIL

                ; Not initialized yet, so wait a while then try again.
                ; This retry is important, to give the card time to initialize.
                ldx #0
                ldy #0
SDDELAYLOOP:
                dey
                bne SDDELAYLOOP
                dex
                bne SDDELAYLOOP

                jmp SDCMD55

SDINITOK:
                lda     #<QT_SD_FOUND
                ldy     #>QT_SD_FOUND
                jsr     STROUT
                rts

SDINITFAIL:
                lda #'C'
                jsr CHROUT
                ldx #0
                lda (SDADDR,x)
                jsr PRINTHEX
                lda #' '
                jsr CHROUT

                lda     #<QT_SD_FAILED
                ldy     #>QT_SD_FAILED
                jsr     STROUT                
                rts

SD_CMD0_BYTES:
                .byte $40, $00, $00, $00, $00, $95
SD_CMD8_BYTES:
                .byte $48, $00, $00, $01, $aa, $87
SD_CMD55_BYTES:
                .byte $77, $00, $00, $00, $00, $01
SD_CMD41_BYTES:
                .byte $69, $40, $00, $00, $00, $01

SDREADBYTE:
                ldx #$fe                ; Preloaded with seven ones and a zero, so we stop after eight bits
SDRREADLOOP:
                lda #SD_MOSI            ; enable card (CS low), set MOSI (resting state), SCK low
                sta PORTA

                lda #SD_MOSI | SD_SCK   ; toggle the clock high
                sta PORTA

                lda PORTA               ; read next bit
                and #SD_MISO

                clc                     ; default to clearing the bottom bit
                beq SDBITNOTSET         ; unless MISO was set
                sec                     ; in which case get ready to set the bottom bit
SDBITNOTSET:
                txa                     ; transfer partial result from X
                rol                     ; rotate carry bit into read result, and loop bit into carry
                tax                     ; save partial result back to X
                
                bcs SDRREADLOOP         ; loop if we need to read more bits
                rts

SDWRITEBYTE:
                ldx #8                  ; send 8 bits

SDWRITELOOP:
                asl                     ; shift next bit into carry
                tay                     ; save remaining bits for later

                lda #0
                bcc SDSENDBIT           ; if carry clear, don't set MOSI for this bit
                ora #SD_MOSI

SDSENDBIT:
                sta PORTA               ; set MOSI (or not) first with SCK low
                eor #SD_SCK
                sta PORTA               ; raise SCK keeping MOSI the same, to send the bit

                tya                     ; restore remaining bits to send

                dex
                bne SDWRITELOOP         ; loop if there are more bits to send

                rts

SDWAITRESULT:                           ; Wait for the SD card to return something other than $ff
                jsr SDREADBYTE
                cmp #$ff
                beq SDWAITRESULT
                rts

SDSENDCMD:                                                     
                lda #SD_MOSI            ; pull CS low to begin command
                sta PORTA

                ldy #0
                lda (SDADDR),y          ; command byte
                jsr SDWRITEBYTE
                ldy #1
                lda (SDADDR),y          ; data 1
                jsr SDWRITEBYTE
                ldy #2
                lda (SDADDR),y          ; data 2
                jsr SDWRITEBYTE
                ldy #3
                lda (SDADDR),y          ; data 3
                jsr SDWRITEBYTE
                ldy #4
                lda (SDADDR),y          ; data 4
                jsr SDWRITEBYTE
                ldy #5
                lda (SDADDR),y          ; crc
                jsr SDWRITEBYTE

                jsr SDWAITRESULT
                pha

                ; Debug print the result code
                ;jsr PRINTHEX

                ; End command
                lda #SD_CS | SD_MOSI   ; set CS high again
                sta PORTA

                pla   ; restore result code
                rts

SDREADSECTOR:
                lda #SD_MOSI
                sta PORTA

                ; Command 17, arg is sector number, crc not checked
                lda #$51                ; CMD17 - READ_SINGLE_BLOCK
                jsr SDWRITEBYTE
                lda SDCURRSEC+3         ; sector 24:31
                jsr SDWRITEBYTE
                lda SDCURRSEC+2         ; sector 16:23
                jsr SDWRITEBYTE
                lda SDCURRSEC+1         ; sector 8:15
                jsr SDWRITEBYTE
                lda SDCURRSEC           ; sector 0:7
                jsr SDWRITEBYTE
                lda #$01                ; crc (not checked)
                jsr SDWRITEBYTE

                jsr SDWAITRESULT
                cmp #$00
                bne SDREADFAIL

                ; wait for data
                jsr SDWAITRESULT
                cmp #$fe
                bne SDREADFAIL

                ; Need to read 512 bytes - two pages of 256 bytes each
                jsr SDREADPAGE
                inc SDADDR+1
                jsr SDREADPAGE
                dec SDADDR+1

                ; End command
                lda #SD_CS | SD_MOSI
                sta PORTA
                rts

SDREADFAIL:
                lda #'s'
                jsr CHROUT
                lda #':'
                jsr CHROUT
                lda #'f'
                jsr CHROUT
                rts

SDREADPAGE:                             ; Read 256 bytes to the address at SDADDR
                ldy #0
SDREADPAGELOOP:
                jsr SDREADBYTE
                sta (SDADDR),y
                iny
                bne SDREADPAGELOOP
                rts

QT_SD_SEARCHING:
                .byte "SEARCHING FOR SD..."
                .byte   CR,LF,0
QT_SD_FOUND:
                .byte "SD FOUND"
                .byte   CR,LF,0
QT_SD_FAILED:
                .byte "FAILED"
                .byte   CR,LF,0