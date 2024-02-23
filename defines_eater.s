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
USR := $310

; constants
SPACE_FOR_GOSUB := $3E
STACK_TOP := $FA
WIDTH := 40
WIDTH2 := 30
RAMSTART2 := $0400
