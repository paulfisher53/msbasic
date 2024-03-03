
.feature org_per_seg
.zeropage

.org ZP_START1

GORESTART:
	.res 3
GOSTROUT:
	.res 3
GOAYINT:
	.res 2
GOGIVEAYF:
	.res 2

.org ZP_START2
Z15:
	.res 1
.ifndef POSX; allow override
POSX:
.endif
	.res 1
.ifndef Z17; allow override
Z17:
.endif
	.res 1
.ifndef Z18; allow override
Z18:
.endif
	.res 1
LINNUM:
.ifndef TXPSV; allow override
TXPSV:
.endif
	.res 2
.ifndef INPUTBUFFER; allow override
INPUTBUFFER:
.endif

.org ZP_START3

CHARAC:
	.res 1
ENDCHR:
	.res 1
EOLPNTR:
	.res 1
DIMFLG:
	.res 1
VALTYP:
.ifdef CONFIG_SMALL
	.res 1
.else
	.res 2
.endif
DATAFLG:
	.res 1
SUBFLG:
	.res 1
INPUTFLG:
	.res 1
CPRMASK:
	.res 1
Z14:
	.res 1

.org ZP_START4

TEMPPT:
	.res 1
LASTPT:
	.res 2
TEMPST:
	.res 9
INDEX:
	.res 2
DEST:
	.res 2
RESULT:
	.res BYTES_FP
RESULT_LAST = RESULT + BYTES_FP-1
TXTTAB:
	.res 2
VARTAB:
	.res 2
ARYTAB:
	.res 2
STREND:
	.res 2
FRETOP:
	.res 2
FRESPC:
	.res 2
MEMSIZ:
	.res 2
CURLIN:
	.res 2
OLDLIN:
	.res 2
OLDTEXT:
	.res 2
Z8C:
	.res 2
DATPTR:
	.res 2
INPTR:
	.res 2
VARNAM:
	.res 2
VARPNT:
	.res 2
FORPNT:
	.res 2
LASTOP:
	.res 2
CPRTYP:
	.res 1
FNCNAM:
TEMP3:
	.res 2
DSCPTR:
.ifdef CONFIG_SMALL
		.res 2
.else
		.res 3
.endif
DSCLEN:
	.res 2
.ifndef JMPADRS ; allow override
JMPADRS			:= DSCLEN + 1
.endif
Z52:
	.res 1
ARGEXTENSION:
.ifndef CONFIG_SMALL
	.res 1
.endif
TEMP1:
	.res 1
HIGHDS:
	.res 2
HIGHTR:
	.res 2
.ifndef CONFIG_SMALL
TEMP2:
	.res 1
.endif
INDX:
TMPEXP:
.ifdef CONFIG_SMALL
TEMP2:
.endif
	.res 1
EXPON:
	.res 1
LOWTR:
.ifndef LOWTRX ; allow override
LOWTRX:
.endif
	.res 1
EXPSGN:
	.res 1
FAC:
	.res BYTES_FP
FAC_LAST = FAC + BYTES_FP-1
FACSIGN:
	.res 1
SERLEN:
	.res 1
SHIFTSIGNEXT:
	.res 1
ARG:
	.res BYTES_FP
ARG_LAST = ARG + BYTES_FP-1
ARGSIGN:
	.res 1
STRNG1:
	.res 2
SGNCPR = STRNG1
FACEXTENSION = STRNG1+1
STRNG2:
	.res 2
.ifdef AIM65
ATN:
	.res 3
ZBE:
	.res 1
.endif
.ifdef SYM1
USR1:
	.res 3
USR2:
	.res 3
USR3:
	.res 3
.endif
CHRGET:
TXTPTR = <(GENERIC_TXTPTR-GENERIC_CHRGET + CHRGET)
CHRGOT = <(GENERIC_CHRGOT-GENERIC_CHRGET + CHRGET)
CHRGOT2 = <(GENERIC_CHRGOT2-GENERIC_CHRGET + CHRGET)
RNDSEED = <(GENERIC_RNDSEED-GENERIC_CHRGET + CHRGET)

.ifdef EATER

.org $200            ; store variables in tape error buffer

; -----------------------------------------------------------------------------
; variables
ACMD:    .res 1             ; addressing command
LENGTH:  .res 1             ; length of operand
MNEMW:   .res 3             ; 3 letter mnemonic buffer
SAVX:    .res 1             ; 1 byte temp storage, often to save X register
OPCODE:  .res 1             ; current opcode for assembler/disassembler
UPFLG:   .res 1             ; flag: count up (bit 7 clear) or down (bit 7 set)
DIGCNT:  .res 1             ; digit count
INDIG:   .res 1             ; numeric value of single digit
NUMBIT:  .res 1             ; numeric base of input
STASH:   .res 2             ; 2-byte temp storage
U0AA0:   .res 10            ; work buffer
U0AAE   =*                  ; end of work buffer
STAGE:   .res 30            ; staging buffer for filename, search, etc.
ESTAGE  =*                  ; end of staging buffer

        ;*= $0200            ; store more variables in basic line editor buffer

INBUFF:  .res 40            ; 40-character input buffer
ENDIN   =*                  ; end of input buffer

; the next 7 locations are used to store the registers when
; entering the monitor and restore them when exiting.

PCH:     .res 1             ; program counter high byte
PCL:     .res 1             ; program counter low byte
SR:      .res 1             ; status register
ACC:     .res 1             ; accumulator
XR:      .res 1             ; X register
YR:      .res 1             ; Y register
SP:      .res 1             ; stack pointer

STORE:   .res 2             ; 2-byte temp storage
CHRPNT:  .res 1             ; current position in input buffer
SAVY:    .res 1             ; temp storage, often to save Y register
U9F:     .res 1             ; index into assembler work buffer

.endif