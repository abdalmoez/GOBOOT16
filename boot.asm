;AbdAlmoez GOBOOT16
; boot binary file in 16 bit real mode using FAT12 filesystem
; copyleft
[BITS 16]
ORG 0
DESTINATION_SEG_VALUE equ 0x0050
          jmp     START
          nop
     OEM_ID                db "GOBOOT16"
     BytesPerSector        dw 0x0200
     SectorsPerCluster     db 0x01
     ReservedSectors       dw 0x0001
     TotalFATs             db 0x02
     MaxRootEntries        dw 0x00E0
     TotalSectorsSmall     dw 0x0B40
     MediaDescriptor       db 0xF0
     SectorsPerFAT         dw 0x0009
     SectorsPerTrack       dw 0x0012
     NumHeads              dw 0x0002
     HiddenSectors         dd 0x00000000
     TotalSectorsLarge     dd 0x00000000
     DriveNumber           db 0x00
     Flags                 db 0x00
     Signature             db 0x29
     VolumeID              dd 0xFFFFFFFF
     VolumeLabel           db "GO-BOOTDISC"
     SystemID              db "FAT12   "
START:
	 
		mov ax,0x7C0
		mov ds,ax
		mov fs,ax
		mov es,ax
          mov ax, 0x0000
          mov ss, ax
          mov sp, 0xFFFF		
		
		;Read FAT
		mov cx,word[SectorsPerFAT]
		mov ax,word[ReservedSectors]
		mov bx,word[BytesPerSector]
		call ReadSectors;ex:bx -> 0x7E00
		;Read Root Entry
		call initEntryCluster
		mov ax,word[RootEntryLBA]
		mov cx,word[MaxRootEntries]
		shr cx,4
		mov word[EntryPtr],bx
		mov si,word[BytesPerSector]
		call ReadSectors;ex:bx -> 0x7E00	+ size of fat

		;Search for file
		mov cx,word[MaxRootEntries]
		mov si,word[EntryPtr]
 .lp02:	
		cmp byte[si],0
		je .FileNotFound
		call isKernelName
		je .FileExist
		add si,32;add size of entry		
		loop .lp02
 .FileNotFound:
		mov si,ERR_MSG
		call Print
		
		jmp $
		
 .FileExist:
		;Up done

		;Load kernel
		;FAT32 doit charger 4 octet
		mov ax,word[si+0x1A];First cluster		
		
		cmp ax,0x00;check if Empty File
		je .FileNotFound
		mov si,word[BytesPerSector];SI <=> FAT
		mov bx,DESTINATION_SEG_VALUE
		mov es,bx
		xor bx,bx
 .lp03:	
		
		call LoadCluster
		call FAT12SearchNextCluter; in si(fat),ax  ou ax
		
		cmp ax,0xFF8;FAT12 Check if last sector
		jae .EndLoadingFile
		
		;add size of cluster in bytes to bx ptr
		xor cx, cx
        mov cl, BYTE[SectorsPerCluster]
  .lp04:add bx,word[BytesPerSector]
		loop .lp04
		
		JMP .lp03
  .EndLoadingFile:
		
		mov ax,DESTINATION_SEG_VALUE
		mov ds,ax
		mov es,ax
		mov fs,ax
		
		push    word DESTINATION_SEG_VALUE
        push    word 0x0000
        retf
		
		
		
	
Print:
	pusha
		mov ah,0xE
	.lp:
		lodsb
		cmp al,0
		je .done
		int 0x10
		jmp .lp
	.done:
	popa
	ret

isKernelName:
		push si
		push cx
			mov di,KERNEL
			mov cx,7
			.lp:
			lodsb
			cmp al,byte[di]
			jne .end
			inc di
			loop .lp			
		.end:
		cmp cx,0
		pop cx
		pop si
		ret
FAT12SearchNextCluter:;IN: si(FAT) , ax(current cluster)
	push si
	push bx
	  cmp ax,0
	  je .error
	  cmp ax,0xFF8
	  jae .error
	  mov bx,ax
	  shr bx,1
	  add si,ax
	  add si,bx
	  mov bx,word[si]
	  test ax,0x0001
	  jnz .ODD_CLUSTER
  .EVEN_CLUSTER:
      and bx, 0000111111111111b               ; take low twelve bits
	  jmp .DONE
  .ODD_CLUSTER:
	  shr bx, 0x0004                          ; take high twelve bits	  
  .DONE:
	mov ax,bx
	jmp .exit
	
  .error:xor ax,ax
  .exit:
    pop bx
	pop si
	ret
initEntryCluster:;no args
	push ax
	push cx
		mov ax,word[ReservedSectors]
		xor cx,cx
		mov cl,byte[TotalFATs]
 .flp01:add ax,word[SectorsPerFAT]
		loop .flp01
		mov word[RootEntryLBA],ax
		mov cx,word[MaxRootEntries]
		shr cx,4
		add ax,cx
		mov word[FirstClusterLBA],ax
	pop cx
	pop ax
	ret
LoadCluster:;read the cluster number "AX" to "ES:BX"
	pusha
		
		sub ax,2;cluster zero and one is not used
		mov dx,ax
		mov ax,word[FirstClusterLBA]
		
		xor cx, cx
        mov cl, BYTE[SectorsPerCluster]
   .lp0:add ax,dx
		loop .lp0
		
		xor cx, cx
        mov cl, BYTE[SectorsPerCluster]
		call ReadSectors
	popa
	ret
	
	 
     
     ReadSectors: ; reads "cx" sectors from disk starting at "ax" into memory location "es:bx"
     .MAIN:
          mov     di, 0x0005                          ; five retries for error
		  
     .SECTORLOOP:	 
          push    ax
          push    bx
          push    cx
          call    .LBACHS
          mov     ah, 0x02                            ; BIOS read sector
          mov     al, 0x01                            ; read one sector
          mov     ch, BYTE [.absoluteTrack]            ; track
          mov     cl, BYTE [.absoluteSector]           ; sector
          mov     dh, BYTE [.absoluteHead]             ; head
          mov     dl, BYTE [DriveNumber]              ; drive
          int     0x13                                ; invoke BIOS
          jnc     .SUCCESS                            ; test for read error
          xor     ax, ax                              ; BIOS reset disk
          int     0x13                                ; invoke BIOS
          dec     di                                  ; decrement error counter
          pop     cx
          pop     bx
          pop     ax
          jnz     .SECTORLOOP                         ; attempt to read again		  
          int     0x18
     .SUCCESS:
          pop     cx
          pop     bx
          pop     ax
          add     bx, WORD [BytesPerSector]           ; queue next buffer
          inc     ax                                  ; queue next sector
          loop    .MAIN                               ; read next sector
          ret

     .LBACHS:
		 ; convert "ax" LBA addressing scheme to CHS addressing scheme
		 ; absolute sector = (logical sector / sectors per track) + 1
		 ; absolute head   = (logical sector / sectors per track) MOD number of heads
		 ; absolute track  = logical sector / (sectors per track * number of heads)
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [SectorsPerTrack]              ; calculate
          inc     dl                                  ; adjust for sector 0
          mov     BYTE [.absoluteSector], dl
          xor     dx, dx                              ; prepare dx:ax for operation
          div     WORD [NumHeads]                     ; calculate
          mov     BYTE [.absoluteHead], dl
          mov     BYTE [.absoluteTrack], al
          ret
     .absoluteSector db 0x00
     .absoluteHead   db 0x00
     .absoluteTrack  db 0x00
	 
     FirstClusterLBA dw 0x0000
	 RootEntryLBA	 dw 0x0000
	 EntryPtr 		 dw 0x0000
	 ERR_MSG    	 db "NO "
	 KERNEL			 db "KERNEL  BIN",0

     TIMES 510-($-$$) DB 0
     DW 0xAA55