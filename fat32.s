FAT32_FATSTART          = FAT32VARS + $00  ; 4 bytes
FAT32_DATASTART         = FAT32VARS + $04  ; 4 bytes
FAT32_ROOTCLUSTER       = FAT32VARS + $08  ; 4 bytes
FAT32_SECTORS           = FAT32VARS + $0c  ; 1 byte
FAT32_PENDSECTORS       = FAT32VARS + $0d  ; 1 byte
FAT32_ADDR              = FAT32VARS + $0e  ; 2 bytes
FAT32_NEXT              = FAT32VARS + $10  ; 4 bytes
FAT32_REMAIN            = FAT32VARS + $14  ; 4 bytes 

FAT32_ERRSTAGE          = FAT32_REMAIN  ; only used during initializatio
FAT32_FNAME             = FAT32_REMAIN  ; only used when searching for a file

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
  bne FAT32FAIL
  lda FAT32BUFFER+511 ; Boot sector signature aa
  cmp #$aa
  bne FAT32FAIL

  inc FAT32_ERRSTAGE ; stage 2 = finding partition

  ; Find a FAT32 partition
  ldx #0
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq FAT32FOUNDPART
  ldx #16
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq FAT32FOUNDPART
  ldx #32
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq FAT32FOUNDPART
  ldx #48
  lda FAT32BUFFER+$1c2,x
  cmp #FSTYPE_FAT32
  beq FAT32FOUNDPART

FAT32FAIL:
  jmp FAT32ERR

FAT32FOUNDPART:
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
  bne FAT32FAIL
  lda FAT32BUFFER+511 ; BPB sector signature aa
  cmp #$aa
  bne FAT32FAIL

  inc FAT32_ERRSTAGE ; stage 4 = RootEntCnt check

  lda FAT32BUFFER+17 ; RootEntCnt should be 0 for FAT32
  ora FAT32BUFFER+18
  bne FAT32FAIL

  inc FAT32_ERRSTAGE ; stage 5 = TotSec16 check

  lda FAT32BUFFER+19 ; TotSec16 should be 0 for FAT32
  ora FAT32BUFFER+20
  bne FAT32FAIL

  inc FAT32_ERRSTAGE ; stage 6 = SectorsPerCluster check

  ; Check bytes per filesystem sector, it should be 512 for any SD card that supports FAT32
  lda FAT32BUFFER+11 ; low byte should be zero
  bne FAT32FAIL
  lda FAT32BUFFER+12 ; high byte is 2 (512), 4, 8, or 16
  cmp #2
  bne FAT32FAIL

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
FAT32SLOOP:
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
  bne FAT32SLOOP

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

  clc
  rts

FAT32ERR:
  sec
  rts

FAT32SEEKCLUSTER:
  ; Gets ready to read FAT32_NEXT, and advances it according to the FAT
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

  ; Target buffer
  lda #<FAT32BUFFER
  sta SDADDR
  lda #>FAT32BUFFER
  sta SDADDR+1

  ; Read the sector from the FAT
  jsr SDREADSECTOR

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
SPCSHIFTLOOP:
  lsr
  bcs SPCSHIFTLOOPDONE
  asl SDCURRSEC
  rol SDCURRSEC+1
  rol SDCURRSEC+2
  rol SDCURRSEC+3
  jmp SPCSHIFTLOOP
SPCSHIFTLOOPDONE:
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
  bne NOTENDOFCHAIN
  lda FAT32_NEXT
  cmp #$f8
  bcc NOTENDOFCHAIN

  ; It's the end of the chain, set the top bits so that we can tell this later on
  sta FAT32_NEXT+3
NOTENDOFCHAIN:
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
  bne FAT32READSECTOR

  ; No pending sectors, check for end of cluster chain
  lda FAT32_NEXT+3
  bmi ENDOFCHAIN

  ; Prepare to read the next cluster
  jsr FAT32SEEKCLUSTER

FAT32READSECTOR:
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
  bne SECTORINCDONE
  inc SDCURRSEC+1
  bne SECTORINCDONE
  inc SDCURRSEC+2
  bne SECTORINCDONE
  inc SDCURRSEC+3

SECTORINCDONE:
  ; Success - clear carry and return
  clc
  rts

ENDOFCHAIN:
  ; End of chain - set carry and return
  sec
  rts

FAT32OPENROOT:
  ; Prepare to read the root directory
  lda FAT32_ROOTCLUSTER
  sta FAT32_NEXT
  lda FAT32_ROOTCLUSTER+1
  sta FAT32_NEXT+1
  lda FAT32_ROOTCLUSTER+2
  sta FAT32_NEXT+2
  lda FAT32_ROOTCLUSTER+3
  sta FAT32_NEXT+3

  jsr FAT32SEEKCLUSTER

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta SDADDR+1

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

  jsr FAT32SEEKCLUSTER

  ; Set the pointer to a large value so we always read a sector the first time through
  lda #$ff
  sta SDADDR+1

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
  bcc GOTDATA

  ; Read another sector
  lda #<FAT32BUFFER
  sta FAT32_ADDR
  lda #>FAT32BUFFER
  sta FAT32_ADDR+1

  jsr FAT32READNEXTSECTOR
  bcc GOTDATA

ENDOFDIR:
  sec
  rts

GOTDATA:
  ; Check first character
  ldy #0
  lda (SDADDR),y

  ; End of directory => abort
  beq ENDOFDIR

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
DIRENTLOOP:
  jsr FAT32READDIRENT
  ldy #10
  bcc COMPARENAMELOOP
  rts ; with carry set

COMPARENAMELOOP:
  lda (SDADDR),y
  cmp (FAT32_FNAME),y
  bne DIRENTLOOP ; no match
  dey
  bpl COMPARENAMELOOP

  ; Found it
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
  beq FAT32READDONE

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
  bcc GOTFILEDATA

  ; Read another sector
  lda #<FAT32BUFFER
  sta FAT32_ADDR
  lda #>FAT32BUFFER
  sta FAT32_ADDR+1

  jsr FAT32READNEXTSECTOR
  bcs FAT32READDONE                    ; this shouldn't happen

GOTFILEDATA:
  ldy #0
  lda (SDADDR),y

  inc SDADDR
  bne FAT32READDONE
  inc SDADDR+1
  bne FAT32READDONE
  inc SDADDR+2
  bne FAT32READDONE
  inc SDADDR+3

FAT32READDONE:
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
  beq FILEREADDONE

  ; Store sector count - not a byte count any more
  sta FAT32_REMAIN

  ; Read entire sectors to the user-supplied buffer
SECTORREADLOOP:
  ; Read a sector to FAT32_ADDR
  jsr FAT32READNEXTSECTOR

  ; Advance FAT32_ADDR by 512 bytes
  lda FAT32_ADDR+1
  adc #2                      ; carry already clear
  sta FAT32_ADDR+1

  ldx FAT32_REMAIN    ; note - actually loads sectors remaining
  dex
  stx FAT32_REMAIN    ; note - actually stores sectors remaining

  bne SECTORREADLOOP

FILEREADDONE:
  rts

