		.segment "HEADER"
.ifdef KBD
        jmp     LE68C
        .byte   $00,$13,$56
.endif
.ifdef AIM65
        jmp     COLD_START
        jmp     RESTART
        .word   AYINT,GIVAYF
.endif
.ifdef SYM1
        jmp     PR_WRITTEN_BY
.endif
.ifdef EATER
  .ifdef CONFIG_LCD
        jsr LCDCLEAR
        lda #<TEXT_BASIC
        ldy #>TEXT_BASIC 
        jsr LCDPRINT
  .endif
        jmp     COLD_START
.endif
