.setcpu "65C02"
.segment "JMPTBL"

jmp RESET                               ; $FD00 / 64768
jmp WOZMON                              ; $FD03 / 64771
jmp LCDCLEAR                            ; $FD06 / 64774
jmp LCDRENDER                           ; $FD09 / 64777
jmp LCDSENDDATA                         ; $FD0C / 64780
                         