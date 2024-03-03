; From https://github.com/gfoot/sdcard6502
; and https://github.com/liaminventions/sdcard6502

.setcpu "65C02"
.segment "BIOS"

FSTYPE_FAT32 = 12

FAT32INIT:
  lda #0
  sta FAT32_ERRSTAGE

  ; Sector 0
  lda #0
  sta SDCURRSEC
  sta SDCURRSEC+1
  sta SDCURRSEC+2
  sta SDCURRSEC+3

  ; Target buffer
  lda #<FAT32BUFFER
  sta SDADDR
  lda #>FAT32BUFFER
  sta SDADDR+1

  ; Do the read
  jsr SDREADSECTOR

  inc FAT32_ERRSTAGE ; stage 1 = boot sector signature check

  ; Check some things
  lda FAT32BUFFER+510 ; Boot sector signature 55
  cmp #$55
  bne @FAIL
  lda FAT32BUFFER+511 ; Boot sector signature aa
  cmp #$aa
  bne @FAIL

  inc FAT32_ERRSTAGE ; stage 2 = finding partition

  ; Find a FAT32 partition
  ldx #0
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq @FOUNDPART
  ldx #16
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq @FOUNDPART
  ldx #32
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq @FOUNDPART
  ldx #48
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq @FOUNDPART

@FAIL:
  jmp @ERROR

@FOUNDPART:
  ; Read the FAT32 BPB
  lda FAT32BUFFER+$1c6,x
  sta SDCURRSEC
  lda FAT32BUFFER+$1c7,x
  sta SDCURRSEC+1
  lda FAT32BUFFER+$1c8,x
  sta SDCURRSEC+2
  lda FAT32BUFFER+$1c9,x
  sta SDCURRSEC+3

  jsr SDREADSECTOR

  inc FAT32_ERRSTAGE ; stage 3 = BPB signature check

  ; Check some things
  lda FAT32BUFFER+510 ; BPB sector signature 55
  cmp #$55
  bne @FAIL
  lda FAT32BUFFER+511 ; BPB sector signature aa
  cmp #$aa
  bne @FAIL

  inc FAT32_ERRSTAGE ; stage 4 = RootEntCnt check

  lda FAT32BUFFER+17 ; RootEntCnt should be 0 for FAT32
  ora FAT32BUFFER+18
  bne @FAIL

  inc FAT32_ERRSTAGE ; stage 5 = TotSec16 check

  lda FAT32BUFFER+19 ; TotSec16 should be 0 for FAT32
  ora FAT32BUFFER+20
  bne @FAIL

  inc FAT32_ERRSTAGE ; stage 6 = SectorsPerCluster check

  ; Check bytes per filesystem sector, it should be 512 for any SD card that supports FAT32
  lda FAT32BUFFER+11 ; low byte should be zero
  bne @FAIL
  lda FAT32BUFFER+12 ; high byte is 2 (512), 4, 8, or 16
  cmp #2
  bne @FAIL

  ; Calculate the starting sector of the FAT
  clc
  lda SDCURRSEC
  adc FAT32BUFFER+14    ; reserved sectors lo
  sta FAT32_FATSTART
  sta FAT32_DATASTART
  lda SDCURRSEC+1
  adc FAT32BUFFER+15    ; reserved sectors hi
  sta FAT32_FATSTART+1
  sta FAT32_DATASTART+1
  lda SDCURRSEC+2
  adc #0
  sta FAT32_FATSTART+2
  sta FAT32_DATASTART+2
  lda SDCURRSEC+3
  adc #0
  sta FAT32_FATSTART+3
  sta FAT32_DATASTART+3

  ; Calculate the starting sector of the data area
  ldx FAT32BUFFER+16   ; number of FATs
  stx FAT32_NUMFATS         ; (stash for later as well)
@SKIPFATSLOOP:
  clc
  lda FAT32_DATASTART
  adc FAT32BUFFER+36 ; fatsize 0
  sta FAT32_DATASTART
  lda FAT32_DATASTART+1
  adc FAT32BUFFER+37 ; fatsize 1
  sta FAT32_DATASTART+1
  lda FAT32_DATASTART+2
  adc FAT32BUFFER+38 ; fatsize 2
  sta FAT32_DATASTART+2
  lda FAT32_DATASTART+3
  adc FAT32BUFFER+39 ; fatsize 3
  sta FAT32_DATASTART+3
  dex
  bne @SKIPFATSLOOP

  ; Sectors-per-cluster is a power of two from 1 to 128
  lda FAT32BUFFER+13
  sta FAT32_SECTORS

  ; Remember the root cluster
  lda FAT32BUFFER+44
  sta FAT32_ROOTCLUSTER
  lda FAT32BUFFER+45
  sta FAT32_ROOTCLUSTER+1
  lda FAT32BUFFER+46
  sta FAT32_ROOTCLUSTER+2
  lda FAT32BUFFER+47
  sta FAT32_ROOTCLUSTER+3

  ; Save Sectors Per FAT
  lda FAT32BUFFER+36
  sta FAT32_SECTORSPERFAT
  lda FAT32BUFFER+37
  sta FAT32_SECTORSPERFAT+1
  lda FAT32BUFFER+38
  sta FAT32_SECTORSPERFAT+2
  lda FAT32BUFFER+39
  sta FAT32_SECTORSPERFAT+3

  ; Set the last found free cluster to 0.
  lda #0
  sta FAT32_LASTFREECLUSTER
  sta FAT32_LASTFREECLUSTER+1
  sta FAT32_LASTFREECLUSTER+2
  sta FAT32_LASTFREECLUSTER+3

  ; As well as the last read clusters and sectors
  sta FAT32_LASTCLUSTER
  sta FAT32_LASTCLUSTER+1
  sta FAT32_LASTCLUSTER+2
  sta FAT32_LASTCLUSTER+3
  sta FAT32_LASTSECTOR
  sta FAT32_LASTSECTOR+1
  sta FAT32_LASTSECTOR+2
  sta FAT32_LASTSECTOR+3
  
  clc
  rts

@ERROR:
  sec
  rts

FAT32SEEKCLUSTER:
  ; Gets ready to read FAT32_NEXT, and advances it according to the FAT
  php

  ; Target buffer
  lda #<FAT32BUFFER
  sta SDADDR
  lda #>FAT32BUFFER
  sta SDADDR+1
  
  ; FAT sector = (cluster*4) / 512 = (cluster*2) / 256
  lda FAT32_NEXT
  asl
  lda FAT32_NEXT+1
  rol
  sta SDCURRSEC
  lda FAT32_NEXT+2
  rol
  sta SDCURRSEC+1
  lda FAT32_NEXT+3
  rol
  sta SDCURRSEC+2
  ; note: cluster numbers never have the top bit set, so no carry can occur

  ; Add FAT starting sector
  lda SDCURRSEC
  adc FAT32_FATSTART
  sta SDCURRSEC
  lda SDCURRSEC+1
  adc FAT32_FATSTART+1
  sta SDCURRSEC+1
  lda SDCURRSEC+2
  adc FAT32_FATSTART+2
  sta SDCURRSEC+2
  lda #0
  adc FAT32_FATSTART+3
  sta SDCURRSEC+3

  ; Branch if we don't need to check
  plp
  bcc @NEWSECTOR

  ; Check if this sector is the same as the last one
  lda FAT32_LASTSECTOR
  cmp SDCURRSEC
  bne @NEWSECTOR
  lda FAT32_LASTSECTOR+1
  cmp SDCURRSEC+1
  bne @NEWSECTOR
  lda FAT32_LASTSECTOR+2
  cmp SDCURRSEC+2
  bne @NEWSECTOR
  lda FAT32_LASTSECTOR+3
  cmp SDCURRSEC+3
  beq @NOTNEW

@NEWSECTOR:
  ; Read the sector from the FAT
  jsr SDREADSECTOR

  ; Update FAT32_LASTSECTOR

  lda SDCURRSEC
  sta FAT32_LASTSECTOR
  lda SDCURRSEC+1
  sta FAT32_LASTSECTOR+1
  lda SDCURRSEC+2
  sta FAT32_LASTSECTOR+2
  lda SDCURRSEC+3
  sta FAT32_LASTSECTOR+3

@NOTNEW:
  ; Before using this FAT data, set currentsector ready to read the cluster itself
  ; We need to multiply the cluster number minus two by the number of sectors per 
  ; cluster, then add the data region start sector

  ; Subtract two from cluster number
  sec
  lda FAT32_NEXT
  sbc #2
  sta SDCURRSEC
  lda FAT32_NEXT+1
  sbc #0
  sta SDCURRSEC+1
  lda FAT32_NEXT+2
  sbc #0
  sta SDCURRSEC+2
  lda FAT32_NEXT+3
  sbc #0
  sta SDCURRSEC+3
  
  ; Multiply by sectors-per-cluster which is a power of two between 1 and 128
  lda FAT32_SECTORS
@SPCSHIFTLOOP:
  lsr
  bcs @SPCSHIFTLOOPDONE
  asl SDCURRSEC
  rol SDCURRSEC+1
  rol SDCURRSEC+2
  rol SDCURRSEC+3
  jmp @SPCSHIFTLOOP
@SPCSHIFTLOOPDONE:
  ; Add the data region start sector
  clc
  lda SDCURRSEC
  adc FAT32_DATASTART
  sta SDCURRSEC
  lda SDCURRSEC+1
  adc FAT32_DATASTART+1
  sta SDCURRSEC+1
  lda SDCURRSEC+2
  adc FAT32_DATASTART+2
  sta SDCURRSEC+2
  lda SDCURRSEC+3
  adc FAT32_DATASTART+3
  sta SDCURRSEC+3

  ; That's now ready for later code to read this sector in - tell it how many consecutive
  ; sectors it can now read
  lda FAT32_SECTORS
  sta FAT32_PENDSECTORS

  ; Now go back to looking up the next cluster in the chain
  ; Find the offset to this cluster's entry in the FAT sector we loaded earlier

  ; Offset = (cluster*4) & 511 = (cluster & 127) * 4
  lda FAT32_NEXT
  and #$7f
  asl
  asl
  tay ; Y = low byte of offset

  ; Add the potentially carried bit to the high byte of the address
  lda SDADDR+1
  adc #0
  sta SDADDR+1

  ; Copy out the next cluster in the chain for later use
  lda (SDADDR),y
  sta FAT32_NEXT
  iny
  lda (SDADDR),y
  sta FAT32_NEXT+1
  iny
  lda (SDADDR),y
  sta FAT32_NEXT+2
  iny
  lda (SDADDR),y
  and #$0f
  sta FAT32_NEXT+3

  ; See if it's the end of the chain
  ora #$f0
  and FAT32_NEXT+2
  and FAT32_NEXT+1
  cmp #$ff
  bne @NOTENDOFCHAIN
  lda FAT32_NEXT
  cmp #$f8
  bcc @NOTENDOFCHAIN

  ; It's the end of the chain, set the top bits so that we can tell this later on
  sta FAT32_NEXT+3
@NOTENDOFCHAIN:
  rts

FAT32READNEXTSECTOR:
  ; Reads the next sector from a cluster chain into the buffer at FAT32_ADDR.
  ;
  ; Advances the current sector ready for the next read and looks up the next cluster
  ; in the chain when necessary.
  ;
  ; On return, carry is clear if data was read, or set if the cluster chain has ended.

  ; Maybe there are pending sectors in the current cluster
  lda FAT32_PENDSECTORS
  bne @READSECTOR

  ; No pending sectors, check for end of cluster chain
  lda FAT32_NEXT+3
  bmi @ENDOFCHAIN

  ; Prepare to read the next cluster
  sec
  jsr FAT32SEEKCLUSTER

@READSECTOR:
  dec FAT32_PENDSECTORS

  ; Set up target address  
  lda FAT32_ADDR
  sta SDADDR
  lda FAT32_ADDR+1
  sta SDADDR+1

  ; Read the sector
  jsr SDREADSECTOR

  ; Advance to next sector
  inc SDCURRSEC
  bne @SECTORINCREMENTDONE
  inc SDCURRSEC+1
  bne @SECTORINCREMENTDONE
  inc SDCURRSEC+2
  bne @SECTORINCREMENTDONE
  inc SDCURRSEC+3

@SECTORINCREMENTDONE:
  ; Success - clear carry and return
  clc
  rts

@ENDOFCHAIN:
  ; End of chain - set carry and return
  sec
  rts

FAT32WRITENEXTSECTOR:
  ; Writes the next sector in a cluster chain from the buffer at FAT32_ADDR.
  ;
  ; On return, carry is set if its the end of the chain. 

  ; Maybe there are pending sectors in the current cluster
  lda FAT32_PENDSECTORS
  bne @BEGINWRITE

  ; No pending sectors, check for end of cluster chain
  lda FAT32_NEXT+3
  bmi @ENDOFCHAIN

  ; Prepare to read the next cluster
  sec
  jsr FAT32SEEKCLUSTER

@BEGINWRITE:
  jsr @WRITESECTOR

  ; Success - clear carry and return
  clc
  rts

@ENDOFCHAIN:
  ; End of chain - set carry, write a sector, and return
  jsr @WRITESECTOR
  sec
  rts

@WRITESECTOR:
  dec FAT32_PENDSECTORS

  ; Set up target address
  lda FAT32_ADDR
  sta SDADDR
  lda FAT32_ADDR+1
  sta SDADDR+1

  ; Write the sector
  jsr SDWRITESECTOR

  ; Advance to next sector
  inc SDCURRSEC
  bne @NEXTSECTORINCREMENTDONE
  inc SDCURRSEC+1
  bne @NEXTSECTORINCREMENTDONE
  inc SDCURRSEC+2
  bne @NEXTSECTORINCREMENTDONE
  inc SDCURRSEC+3
@NEXTSECTORINCREMENTDONE:
  rts

FAT32UPDATEFAT:                         ; Preserve the current sector
  lda SDCURRSEC
  pha 
  lda SDCURRSEC+1
  pha 
  lda SDCURRSEC+2
  pha 
  lda SDCURRSEC+3
  pha

  ; Write FAT sector
  lda FAT32_LASTSECTOR
  sta SDCURRSEC
  lda FAT32_LASTSECTOR+1
  sta SDCURRSEC+1
  lda FAT32_LASTSECTOR+2
  sta SDCURRSEC+2
  lda FAT32_LASTSECTOR+3
  sta SDCURRSEC+3

  ; Target buffer
  lda #<FAT32BUFFER
  sta SDADDR
  lda #>FAT32BUFFER
  sta SDADDR+1

  ; Write the FAT sector
  jsr SDWRITESECTOR

  ; Check if FAT mirroring is enabled
  lda FAT32_NUMFATS
  cmp #2
  bne @ONEFAT

  ; Add the last sector to the amount of sectors per FAT
  ; (to get the second fat location)
  lda FAT32_LASTSECTOR
  adc FAT32_SECTORSPERFAT
  sta SDCURRSEC
  lda FAT32_LASTSECTOR+1
  adc FAT32_SECTORSPERFAT+1
  sta SDCURRSEC+1
  lda FAT32_LASTSECTOR+2
  adc FAT32_SECTORSPERFAT+2
  sta SDCURRSEC+2
  lda FAT32_LASTSECTOR+3
  adc FAT32_SECTORSPERFAT+3
  sta SDCURRSEC+3

  ; Write the FAT sector
  jsr SDWRITESECTOR

@ONEFAT:                                 ; Pull back the current sector
  pla
  sta SDCURRSEC+3
  pla
  sta SDCURRSEC+2
  pla
  sta SDCURRSEC+1
  pla
  sta SDCURRSEC
  rts
  
FAT32OPENROOT:                          ; Prepare to read the root directory
  lda FAT32_ROOTCLUSTER
  sta FAT32_NEXT
  lda FAT32_ROOTCLUSTER+1
  sta FAT32_NEXT+1
  lda FAT32_ROOTCLUSTER+2
  sta FAT32_NEXT+2
  lda FAT32_ROOTCLUSTER+3
  sta FAT32_NEXT+3
  clc
  jsr FAT32SEEKCLUSTER

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta SDADDR+1
  rts

FAT32ALLOCATECLUSTER:                   ; Allocate a cluster to start storing a file at.
  ; Find a free cluster
  jsr FAT32FINDFREECLUSTER

  ; Cache the value so we can add the address of the next one later, if any
  lda FAT32_LASTFREECLUSTER
  sta FAT32_LASTCLUSTER
  sta FAT32_FILECLUSTER
  lda FAT32_LASTFREECLUSTER+1
  sta FAT32_LASTCLUSTER+1
  sta FAT32_FILECLUSTER+1
  lda FAT32_LASTFREECLUSTER+2
  sta FAT32_LASTCLUSTER+2
  sta FAT32_FILECLUSTER+2
  lda FAT32_LASTFREECLUSTER+3
  sta FAT32_LASTCLUSTER+3
  sta FAT32_FILECLUSTER+3

  ; Add marker for the following routines, so we don't think this is free.
  ; (SDADDR),y is controlled by FAT32SEEKCLUSTER, called in FAT32FINDFREECLUSTER
  ; this points to the most significant byte in the last selected 32-bit FAT entry.
  lda #$0f
  sta (SDADDR),y
  rts

FAT32ALLOCATEFILE:
  ; Allocate an entire file in the FAT, with the
  ; file's size in FAT32_REMAIN

  ; We will read a new sector the first time around
  stz FAT32_LASTSECTOR
  stz FAT32_LASTSECTOR+1
  stz FAT32_LASTSECTOR+2
  stz FAT32_LASTSECTOR+3

  ; Allocate the first cluster.
  jsr FAT32ALLOCATECLUSTER

  ; We don't properly support 64k+ files, as it's unnecessary complication given
  ; the 6502's small address space. So we'll just empty out the top two bytes.
  lda #0
  sta FAT32_REMAIN+2
  sta FAT32_REMAIN+3

  ; Stash filesize, as we will be clobbering it here
  lda FAT32_REMAIN
  pha
  lda FAT32_REMAIN+1
  pha

  ; Round the size up to the next whole sector
  lda FAT32_REMAIN
  cmp #1                      ; set carry if bottom 8 bits not zero
  lda FAT32_REMAIN+1
  adc #0                      ; add carry, if any
  lsr                         ; divide by 2
  adc #0                      ; round up

  ; No data?
  bne @NOFAIL
  jmp @DONE

@NOFAIL:
  ; This will be clustersremaining now.
  sta FAT32_REMAIN

  ; Divide by sectors per cluster (power of 2)
  ; If it's 1, then skip
  lda FAT32_SECTORS
  cmp #1
  beq @ONE

  lsr
@CL:
  lsr FAT32_REMAIN
  lsr
  bcc @CL

@ONE:
  ; We will be making a new cluster every time
  stz FAT32_PENDSECTORS

  ; Find free clusters and allocate them for use for this file.
@ALLOCATELOOP:
  ; Check if it's the last cluster in the chain 
  lda FAT32_REMAIN
  beq @LASTCLUSTER
  cmp #1                 ; CHECK! is 1 the right amound for this?
  bcc @NOTLASTCLUSTER     ; clustersremaining <=1?

  ; It is the last one.

@LASTCLUSTER:
; go back the previous one
  lda FAT32_LASTCLUSTER
  sta FAT32_NEXT
  lda FAT32_LASTCLUSTER+1
  sta FAT32_NEXT+1
  lda FAT32_LASTCLUSTER+2
  sta FAT32_NEXT+2
  lda FAT32_LASTCLUSTER+3
  sta FAT32_NEXT+3

  sec
  jsr FAT32SEEKCLUSTER

  ; Write 0x0FFFFFFF (EOC)
  lda #$0f
  sta (SDADDR),y
  dey
  lda #$ff
  sta (SDADDR),y
  dey
  sta (SDADDR),y
  dey
  sta (SDADDR),y

  ; Update the FAT
  jsr FAT32UPDATEFAT

  ; End of chain - exit
  jmp @DONE

@NOTLASTCLUSTER:
  ; Wait! Is there exactly 1 cluster left?
  beq @LASTCLUSTER

  ; Find the next cluster
  jsr FAT32FINDFREECLUSTER

  ; Add marker so we don't think this is free.
  lda #$0f
  sta (SDADDR),y

  ; Seek to the previous cluster
  lda FAT32_LASTCLUSTER
  sta FAT32_NEXT
  lda FAT32_LASTCLUSTER+1
  sta FAT32_NEXT+1
  lda FAT32_LASTCLUSTER+2
  sta FAT32_NEXT+2
  lda FAT32_LASTCLUSTER+3
  sta FAT32_NEXT+3

  sec
  jsr FAT32SEEKCLUSTER

  ; Enter the address of the next one into the FAT
  lda FAT32_LASTFREECLUSTER+3
  sta FAT32_LASTCLUSTER+3
  sta (SDADDR),y
  dey
  lda FAT32_LASTFREECLUSTER+2
  sta FAT32_LASTCLUSTER+2
  sta (SDADDR),y
  dey
  lda FAT32_LASTFREECLUSTER+1
  sta FAT32_LASTCLUSTER+1
  sta (SDADDR),y
  dey
  lda FAT32_LASTFREECLUSTER
  sta FAT32_LASTCLUSTER
  sta (SDADDR),y

  ; Update the FAT
  jsr FAT32UPDATEFAT

  ldx FAT32_REMAIN    ; note - actually loads clusters remaining
  dex
  stx FAT32_REMAIN    ; note - actually stores clusters remaining
  bne @ALLOCATELOOP

  ; Done!
@DONE:
  ; Pull the filesize back from the stack
  pla
  sta FAT32_REMAIN+1
  pla
  sta FAT32_REMAIN
  rts

FAT32FINDFREECLUSTER:
; Find next free cluster
; 
; This program will search the FAT for an empty entry, and
; save the 32-bit cluster number at fat32_lastfoundfreecluter.
;
; Also sets the carry bit if the SD card is full.
;

  ; Find a free cluster and store it's location in FAT32_LASTFREECLUSTER

  lda #0
  sta FAT32_NEXT
  sta FAT32_LASTFREECLUSTER
  sta FAT32_NEXT+1
  sta FAT32_LASTFREECLUSTER+1
  sta FAT32_NEXT+2
  sta FAT32_LASTFREECLUSTER+2
  sta FAT32_NEXT+3
  sta FAT32_LASTFREECLUSTER+3

@SEARCHCLUSTERS:
  ; Seek cluster
  sec
  jsr FAT32SEEKCLUSTER

  ; Is the cluster free?
  lda FAT32_NEXT
  and #$0f
  ora FAT32_NEXT+1
  ora FAT32_NEXT+2
  ora FAT32_NEXT+3
  beq @FOUNDCLUSTER

  ; No, increment the cluster count
  inc FAT32_LASTFREECLUSTER
  bne @COPYCLUSTER
  inc FAT32_LASTFREECLUSTER+1
  bne @COPYCLUSTER
  inc FAT32_LASTFREECLUSTER+2
  bne @COPYCLUSTER
  inc FAT32_LASTFREECLUSTER+3

@COPYCLUSTER:
  ; Copy the cluster count to the next cluster
  lda FAT32_LASTFREECLUSTER
  sta FAT32_NEXT
  lda FAT32_LASTFREECLUSTER+1
  sta FAT32_NEXT+1
  lda FAT32_LASTFREECLUSTER+2
  sta FAT32_NEXT+2
  lda FAT32_LASTFREECLUSTER+3
  sta FAT32_NEXT+3
  
  ; Go again for another pass
  jmp @SEARCHCLUSTERS

@FOUNDCLUSTER:
  ; done.
  rts
  
FAT32OPENDIRENT:
  ; Prepare to read from a file or directory based on a dirent
  ;
  ; Point SDADDR at the dirent

  ; Remember file size in bytes remaining
  ldy #28
  lda (SDADDR),y
  sta FAT32_REMAIN
  iny
  lda (SDADDR),y
  sta FAT32_REMAIN+1
  iny
  lda (SDADDR),y
  sta FAT32_REMAIN+2
  iny
  lda (SDADDR),y
  sta FAT32_REMAIN+3

  ; Seek to first cluster
  ldy #26
  lda (SDADDR),y
  sta FAT32_NEXT
  iny
  lda (SDADDR),y
  sta FAT32_NEXT+1
  ldy #20
  lda (SDADDR),y
  sta FAT32_NEXT+2
  iny
  lda (SDADDR),y
  sta FAT32_NEXT+3

  clc
  jsr FAT32SEEKCLUSTER

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta SDADDR+1

  rts

FAT32WRITEDIRENT:
  ; Write a directory entry from the open directory
  ; requires:
  ;   fat32bytesremaining (2 bytes) = file size in bytes (little endian)

  ; Increment pointer by 32 to point to next entry
  clc
  lda SDADDR
  adc #32
  sta SDADDR
  lda SDADDR+1
  adc #0
  sta SDADDR+1

  ; If it's not at the end of the buffer, we have data already
  cmp #>(FAT32BUFFER+$200)
  bcc @GOTDIRENT

  ; Read another sector
  lda #<FAT32BUFFER
  sta FAT32_ADDR
  lda #>FAT32BUFFER
  sta FAT32_ADDR+1

  jsr FAT32READNEXTSECTOR
  bcc @GOTDIRENT

  sec
  rts

@GOTDIRENT:
  ; Check first character
  clc
  ldy #0
  lda (SDADDR),y
  bne FAT32WRITEDIRENT ; go again
  ; End of directory. Now make a new entry.
@DLOOP:
  lda (FAT32_FNPOINTER),y	; copy filename
  sta (SDADDR),y
  iny
  cpy #$0b
  bne @DLOOP
  ; The full Short filename is #11 bytes long so,
  ; this start at 0x0b - File type
  ; BUG assumes that we are making a file, not a folder...
  lda #$20		; File Type: ARCHIVE
  sta (SDADDR),y
  iny   ; 0x0c - Checksum/File accsess password
  lda #$10		            ; No checksum or password
  sta (SDADDR),y
  iny   ; 0x0d - first char of deleted file - 0x7d for nothing
  lda #$7D
  sta (SDADDR),y
  iny	; 0x0e-0x11 - File creation time/date
  lda #0
@EMPTY:
  sta (SDADDR),y	; No time/date because I don't have an RTC
  iny
  cpy #$14 ; also empty the user ID (0x12-0x13)
  bne @EMPTY
  ; 0x14-0x15 - File start cluster (high word)
  lda FAT32_LASTFREECLUSTER+2
  sta (SDADDR),y
  iny
  lda FAT32_LASTFREECLUSTER+3
  sta (SDADDR),y
  iny ; 0x16-0x19 - File modifiaction date
  lda #0
  sta (SDADDR),y
  iny
  sta (SDADDR),y   ; no rtc
  iny
  sta (SDADDR),y
  iny
  sta (SDADDR),y
  iny ; 0x1a-0x1b - File start cluster (low word)
  lda FAT32_LASTFREECLUSTER
  sta (SDADDR),y
  iny
  lda FAT32_LASTFREECLUSTER+1
  sta (SDADDR),y
  iny ; 0x1c-0x1f File size in bytes
  lda FAT32_REMAIN
  sta (SDADDR),y
  iny
  lda FAT32_REMAIN+1
  sta (SDADDR),y
  iny
  lda #0
  sta (SDADDR),y ; No bigger that 64k
  iny
  sta (SDADDR),y
  iny
  ; are we over the buffer?
  lda SDADDR+1
  cmp #>(FAT32BUFFER+$200)
  bcc @NOTOVERBUFFER
  jsr FAT32WRITECURRSECTOR ; if so, write the current sector
  jsr FAT32READNEXTSECTOR  ; then read the next one.
  bcs @DFAIL
  ldy #0
  lda #<FAT32BUFFER
  sta SDADDR
  lda #>FAT32BUFFER
  sta SDADDR+1
@NOTOVERBUFFER:
  ; next entry is 0 (end of dir)
  lda #0
  sta (SDADDR),y
  ; Write the dirent.
  jsr FAT32WRITECURRSECTOR

  ; Great, lets get this ready for other code to read in.

  ; Seek to first cluster
  lda FAT32_FILECLUSTER
  sta FAT32_NEXT
  lda FAT32_FILECLUSTER+1
  sta FAT32_NEXT+1
  lda FAT32_FILECLUSTER+2
  sta FAT32_NEXT+2
  lda FAT32_FILECLUSTER+3
  sta FAT32_NEXT+3

  clc
  jsr FAT32SEEKCLUSTER

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta SDADDR+1

  clc
  rts

@DFAIL:
  ; Card Full
  sec
  rts

FAT32WRITECURRSECTOR:
  ; decrement the sector so we write the current one (not the next one)
  lda SDCURRSEC
  bne @SKIP
  dec SDCURRSEC+1
  bne @SKIP
  dec SDCURRSEC+2
  bne @SKIP
  dec SDCURRSEC+3

@SKIP:
  dec SDCURRSEC

  lda FAT32_ADDR
  sta SDADDR
  lda FAT32_ADDR+1
  sta SDADDR+1

  ; Read the sector
  jsr SDWRITESECTOR

  ; Advance to next sector
  inc SDCURRSEC
  bne @SECTORINCREMENTDONE
  inc SDCURRSEC+1
  bne @SECTORINCREMENTDONE
  inc SDCURRSEC+2
  bne @SECTORINCREMENTDONE
  inc SDCURRSEC+3

@SECTORINCREMENTDONE:
  rts
  
FAT32READDIRENT:
  ; Read a directory entry from the open directory
  ;
  ; On exit the carry is set if there were no more directory entries.
  ;
  ; Otherwise, A is set to the file's attribute byte and
  ; SDADDR points at the returned directory entry.
  ; LFNs and empty entries are ignored automatically.

  ; Increment pointer by 32 to point to next entry
  clc
  lda SDADDR
  adc #32
  sta SDADDR
  lda SDADDR+1
  adc #0
  sta SDADDR+1

  ; If it's not at the end of the buffer, we have data already
  cmp #>(FAT32BUFFER+$200)
  bcc @GOTDATA

  ; Read another sector
  lda #<FAT32BUFFER
  sta FAT32_ADDR
  lda #>FAT32BUFFER
  sta FAT32_ADDR+1

  jsr FAT32READNEXTSECTOR
  bcc @GOTDATA

@ENDOFDIR:
  sec
  rts

@GOTDATA:
  ; Check first character
  ldy #0
  lda (SDADDR),y

  ; End of directory => abort
  beq @ENDOFDIR

  ; Empty entry => start again
  cmp #$e5
  beq FAT32READDIRENT

  ; Check attributes
  ldy #11
  lda (SDADDR),y
  and #$3f
  cmp #$0f ; LFN => start again
  beq FAT32READDIRENT

  ; Yield this result
  clc
  rts

FAT32FINDDIRENT:
  ; Finds a particular directory entry.  X,Y point to the 11-character filename to seek.
  ; The directory should already be open for iteration.

  ; Form ZP pointer to user's filename
  ;stx FAT32_FNAME
  ;sty FAT32_FNAME+1
  
  ; Iterate until name is found or end of directory
@DIRENTLOOP:
  jsr FAT32READDIRENT
  ldy #10
  bcc @COMPARENAMELOOP
  rts ; with carry set

@COMPARENAMELOOP:
  lda (SDADDR),y
  cmp (FAT32_FNPOINTER),y
  bne @DIRENTLOOP ; no match
  dey
  bpl @COMPARENAMELOOP

  ; Found it
  clc
  rts

FAT32MARKDEL:
  ; Mark the file as deleted
  ; We need to stash the first character at index 0x0D
  ldy #$00
  lda (SDADDR),y
  ldy #$0d
  sta (SDADDR),y

  ; Now put 0xE5 at the first byte
  ldy #$00
  lda #$e5 
  sta (SDADDR),y

  ; Get start cluster high word
  ldy #$14
  lda (SDADDR),y
  sta FAT32_NEXT+2
  iny
  lda (SDADDR),y
  sta FAT32_NEXT+3

  ; And low word
  ldy #$1a
  lda (SDADDR),y
  sta FAT32_NEXT
  iny
  lda (SDADDR),y
  sta FAT32_NEXT+1

  ; Write the dirent
  jsr FAT32WRITECURRSECTOR

  ; Done
  clc
  rts

FAT32DELFILE:
  ; Removes the open file from the SD card.
  ; The directory needs to be open and
  ; SDADDR pointed to the first byte of the file entry.

  ; Mark the file as "Removed"
  jsr FAT32MARKDEL

  ; We will read a new sector the first time around
  stz FAT32_LASTSECTOR
  stz FAT32_LASTSECTOR+1
  stz FAT32_LASTSECTOR+2
  stz FAT32_LASTSECTOR+3

  ; Now we need to iterate through this file's cluster chain, and remove it from the FAT.
  ldy #0
@CHAINLOOP:
  ; Seek to cluster
  sec
  jsr FAT32SEEKCLUSTER

  ; Is this the end of the chain?
  lda FAT32_NEXT+3
  bmi @ENDOFCHAIN

  ; Zero it out
  lda #0
  sta (SDADDR),y
  dey
  sta (SDADDR),y
  dey
  sta (SDADDR),y
  dey
  sta (SDADDR),y

  ; Write the FAT
  jsr FAT32UPDATEFAT

  ; And go again for another pass.
  jmp @CHAINLOOP

@ENDOFCHAIN:
  ; This is the last cluster in the chain.

  ; Just zero it out,
  lda #0
  sta (SDADDR),y
  dey
  sta (SDADDR),y
  dey
  sta (SDADDR),y
  dey
  sta (SDADDR),y

  ; Write the FAT
  jsr FAT32UPDATEFAT

  ; And we're done!
  clc
  rts
  
FAT32READBYTE:
  ; Read a byte from an open file
  ;
  ; The byte is returned in A with C clear; or if end-of-file was reached, C is set instead
  sec

  ; Is there any data to read at all?
  lda FAT32_REMAIN
  ora FAT32_REMAIN+1
  ora FAT32_REMAIN+2
  ora FAT32_REMAIN+3
  beq @GORTS

  ; Decrement the remaining byte count
  lda FAT32_REMAIN
  sbc #1
  sta FAT32_REMAIN
  lda FAT32_REMAIN+1
  sbc #0
  sta FAT32_REMAIN+1
  lda FAT32_REMAIN+2
  sbc #0
  sta FAT32_REMAIN+2
  lda FAT32_REMAIN+3
  sbc #0
  sta FAT32_REMAIN+3
  
  ; Need to read a new sector?
  lda SDADDR+1
  cmp #>(FAT32BUFFER+$200)
  bcc @GOTDATA

  ; Read another sector
  lda #<FAT32BUFFER
  sta FAT32_ADDR
  lda #>FAT32BUFFER
  sta FAT32_ADDR+1

  jsr FAT32READNEXTSECTOR
  bcs @GORTS                    ; this shouldn't happen

@GOTDATA:
  ldy #0
  lda (SDADDR),y

  inc SDADDR
  bne @GORTS
  inc SDADDR+1
  bne @GORTS
  inc SDADDR+2
  bne @GORTS
  inc SDADDR+3

@GORTS:
  rts

FAT32FILEREAD:
  ; Read a whole file into memory.  It's assumed the file has just been opened 
  ; and no data has been read yet.
  ;
  ; Also we read whole sectors, so data in the target region beyond the end of the 
  ; file may get overwritten, up to the next 512-byte boundary.
  ;
  ; And we don't properly support 64k+ files, as it's unnecessary complication given
  ; the 6502's small address space

  ; Round the size up to the next whole sector
  lda FAT32_REMAIN
  cmp #1                      ; set carry if bottom 8 bits not zero
  lda FAT32_REMAIN+1
  adc #0                      ; add carry, if any
  lsr                         ; divide by 2
  adc #0                      ; round up

  ; No data?
  beq @DONE

  ; Store sector count - not a byte count any more
  sta FAT32_REMAIN

  ; Read entire sectors to the user-supplied buffer
@SECTORREADLOOP:
  ; Read a sector to FAT32_ADDR
  jsr FAT32READNEXTSECTOR

  ; Advance FAT32_ADDR by 512 bytes
  lda FAT32_ADDR+1
  adc #2                      ; carry already clear
  sta FAT32_ADDR+1

  ldx FAT32_REMAIN    ; note - actually loads sectors remaining
  dex
  stx FAT32_REMAIN    ; note - actually stores sectors remaining

  bne @SECTORREADLOOP

@DONE:
  rts

FAT32FILEWRITE:
  ; Write a whole file from memory.  It's assumed the dirent has just been created 
  ; and no data has been written yet.

  ; Start at the first cluster for this file
  lda FAT32_FILECLUSTER
  sta FAT32_LASTCLUSTER
  lda FAT32_FILECLUSTER+1
  sta FAT32_LASTCLUSTER+1
  lda FAT32_FILECLUSTER+2
  sta FAT32_LASTCLUSTER+2
  lda FAT32_FILECLUSTER+3
  sta FAT32_LASTCLUSTER+3

  ; Round the size up to the next whole sector
  lda FAT32_REMAIN
  cmp #1                      ; set carry if bottom 8 bits not zero
  lda FAT32_REMAIN+1
  adc #0                      ; add carry, if any
  lsr                         ; divide by 2
  adc #0                      ; round up

  ; No data?
  beq @FAIL

  ; Store sector count - not a byte count anymore.
  sta FAT32_REMAIN

  ; We will be making a new cluster the first time around
  stz FAT32_PENDSECTORS

  ; Write entire sectors from the user-supplied buffer
@SECTORWRITELOOP:
  ; Write a sector from FAT32_ADDR
  jsr FAT32WRITENEXTSECTOR
  ;bcs .fail	; this shouldn't happen

  ; Advance FAT32_ADDR by 512 bytes
  clc
  lda FAT32_ADDR+1
  adc #2           
  sta FAT32_ADDR+1

  ldx FAT32_REMAIN    ; note - actually loads sectors remaining
  dex
  stx FAT32_REMAIN    ; note - actually stores sectors remaining

  bne @SECTORWRITELOOP

  ; Done!
@FAIL:
  rts
