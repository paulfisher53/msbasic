; configuration
CONFIG_2A := 1

CONFIG_PEEK_SAVE_LINNUM := 1
CONFIG_SCRTCH_ORDER := 2
CONFIG_LCD := 1

; zero page
ZP_START1 = $00
ZP_START2 = $0A
ZP_START3 = $60
ZP_START4 = $6B

; extra/override ZP variables
USR := GORESTART

; constants
SPACE_FOR_GOSUB := $3E
STACK_TOP := $FA
WIDTH := 40
WIDTH2 := 30
RAMSTART2 := $0400
LCD_STR_BUFF = $00FB            ; LCD String Buffer (2 bytes)
LCD_STR_OFFSET = $00FD          ; LCD String Offset
LCD_SCREEN = $04C0              ; LCD Screen Memory - 32 bytes (2 rows, 16 columns)
