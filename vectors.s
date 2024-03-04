.setcpu "65C02"
.segment "JMPTBL"

                jmp RESET               ; $FE00 / 65024
                jmp WOZMON              ; $FE03 / 65027
.ifdef CONFIG_LCD
                jmp LCDCLEAR            ; $FE06 / 65030
                jmp LCDRENDER           ; $FE09 / 65033
                jmp LCDSENDDATA         ; $FE0C / 65036
.else
                .res 9
.endif          
                jmp MONITOR             ; #FE08 / 65039              