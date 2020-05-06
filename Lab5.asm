.model small
.stack 100h

puts macro offst
push ax
push dx
mov dx, offset offst
mov ah, 09h
int 21h
pop dx
pop ax
call Sleep
endm

.data 
msgStart db "Start...",0Dh,0Ah,'$'
msgEnd db "...End",0Dh,0Ah,'$'
msgErrArg db "Error: incorrect arguments.",0Dh,0Ah,'$'
msgPropArg db "Proper arguments example:comand [fullpath]\sourceFileName.txt [fullpath]\destinationFileName.txt [Set of symbols to be excluded]",0Dh,0Ah,'$'
msgErrDest db "Error: cannot find file",0Dh,0Ah,'$'
msgErrCmdSize db "Error: comand line is bigger than 126",0Dh,0Ah,'$'
msgErrFile db "Error while processing file",0Dh,0Ah,'$'
msgErrOpen db "Error while opening",0Dh,0Ah,'$'

msgErrNf db "Error File not found",0Dh,0Ah,'$'
msgErrPnf db "Error Path not found",0Dh,0Ah,'$'
msgErrWrm db "Error Wrong mode",0Dh,0Ah,'$'
msgErrAcc db "Error access denied",0Dh,0Ah,'$'
msgErrTmn db "Error too many opened files",0Dh,0Ah,'$'
msgErrRead db "Error while reading",0Dh,0Ah,'$'

msgErrSet db "Error setting pointer",0Dh,0Ah,'$'
msgErrSet2 db "Error setting pointer while reading",0Dh,0Ah,'$'
msgErrNumWr db "Error while writing",0Dh,0Ah,'$'
newLine db 0Dh,0Ah,'$'

msgReaded db "Readed",0Dh,0Ah,'$'
msgSetPos db "SetPOs",0Dh,0Ah,'$'
msgEOF db "EOF encountered",0Dh,0Ah,'$'
msgWrited db "Writed!",0Dh,0Ah,'$'
msgBuffEnded db "BuffEnded!",0Dh,0Ah,'$'

srcFileName dw 0
destFileName dw 0

flagSkip db 1
flagArg db 0
flagDelim db 0
flagKeepDel db 0
flagSetPos db 0

maxBuff equ 1000
buffPtr dw ?
bufSize dw ?
buff db maxBuff+2 dup(0)

maxCmd  equ 126
cmdSize db ?
cmdBuff db maxCmd dup(?)

delSym db 50 dup(0)
delSymLen db ?
src db 50 dup(0)
dest db 50 dup(0)
spaceSym equ 20h
tabSym equ 09h 
CRSym equ 0Dh
NLSym equ 0Ah

.code

main:
    mov ax,@data
    mov es, ax
    xor cx,cx
    mov cl, ds:[80h]
    mov bx,cx
    mov si,81h
    mov di, offset cmdBuff
    rep movsb 
    
    mov ds,ax
    puts msgStart 
    mov cmdSize,bl
    call parse
    cmp dx,1
    je mainEnd


    call open
    cmp dx,1
    je mainEnd
    
    call processFile
mainEnd:
    puts msgEnd
    mov ah,4Ch
    int 21h

Sleep proc 					; proc that wait for RTC to reach time=RTC+generalWaitTime
	push ax 
	push bx 
	push cx
	push dx 
	mov ax, 00h 
	int 1Ah 
	add dx, 0001h 
	mov bx, dx 
    
checkTimeLoop: 
	mov ax, 00h 
	int 1Ah 
	cmp dx, bx
	jl checkTimeLoop 
	
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	ret 
endp

readFile proc
    mov ah, 3Fh
    mov bx, srcFileName
    mov cx, maxBuff
    mov dx, buffPtr
    int 21h
    jb readFileErr
    mov bufSize, ax
    jmp readFileEnd
readFileErr:
    puts msgErrRead
readFileEnd:    
    puts msgReaded
    ret
endp readFile

checkBuff proc
    xor bx,bx
    cmp flagKeepDel,1
    jne checkBuffBegin
    mov flagDelim,1
checkBuffBegin:
    mov si, buffPtr
    xor di,di
    mov cx, bufSize         ;num of symbols that were readed
    xor ax,ax
loopCheckDel:
    lodsb
    cmp flagDelim,0
    jne checkCR
    call checkDel
checkCR:    
    cmp al, CRSym
    je checkSetPosMark
    loop loopCheckDel

    cmp flagDelim,1
    je setKeep
    mov bx,bufSize

    jmp checkBuffEnd
setKeep:    
    mov flagKeepDel,1

    jmp checkBuffEnd
checkSetPosMark: 
    mov flagKeepDel,0
    mov bx, bufSize
    sub bx,cx
    call checkSetPos            ; sets proper bx and flagSetPos
checkBuffEnd:
    ret 
endp checkBuff

checkSetPos proc
    cmp flagDelim,1
    je checkSetPosPos
    jmp checkSetPosEnd
checkSetPosPos:
    cmp bx, 2
    jbe checkSetPosEnd
    mov flagSetPos,1
checkSetPosEnd:
    add bx,2
    ret
endp checkSetPos

checkDel proc
    push cx
    push di
    xor di,di
    xor cx,cx
    mov cl, delSymLen
loopDelSym:
    cmp al, delSym[di]
    je setflagDel
    inc di
    loop loopDelSym

    jmp checkDelEnd
setflagDel:
    mov flagDelim,1
checkDelEnd:
    pop di
    pop cx
    ret
endp checkDel

processFile proc
    mov bx, srcFileName
    mov ah, 42h
    xor al ,al 			
    xor cx, cx
    xor dx, dx			
    int 21h
    jb jmpprocessFileErrSet
    
    mov bx, destFileName
    mov ah, 42h
    xor al ,al 			
    xor cx, cx
    xor dx, dx			
    int 21h
    jb jmpprocessFileErrSet
    jmp readLoop
jmpprocessFileErrSet:
    jmp processFileErrSet
readLoop:
    mov bufSize,maxBuff
    mov buffPtr, offset buff
    call clearBuff
    call readFile
    
    cmp ax,0000h
    je endOfFile
   
checkBuffer:
    mov flagDelim,0
    mov flagSetPos,0
    call checkBuff          ; flags setPos, flagKeepDelim, flagDelim, bx -new pos
    cmp flagKeepDel,1
    je readLoop
    cmp flagDelim,0
    je writeMark
    cmp flagSetPos,1
    je setNewPos
    jmp readLoop
writeMark:
    call writeToDest
    cmp dx,1
    je processFileEnd
    cmp bx,maxBuff
    je readLoop
setNewPos:
    puts msgSetPos
    add buffPtr, bx 
    sub bufSize, bx
    cmp bufSize, 0000h
    je buffEnded
    jmp checkBuffer 
buffEnded:  
    puts msgBuffEnded    
    jmp readLoop
endOfFile:
    puts msgEOF
    jmp processFileEnd
processFileErrSet:
    mov dx,1
    puts msgErrSet
processFileEnd:
    ret 
endp processFile

clearBuff proc
    push cx
    push di
    xor di,di
    mov cx, maxBuff
clearLoop:        
    mov buff[di],'$'
    inc di
    loop clearLoop
    pop di 
    pop cx
    ret
endp clearBuff


writeToDest proc        ; input: bx - num to write
    push cx             ; output: dx -1 if error, ax -err code 
    push bx             
    mov ah,40h          
    mov cx,bx
    mov bx,destFileName
    mov dx, buffPtr
    int 21h
    pop bx
    cmp ax, bx
    je writeToDestEnd
    puts msgErrNumWr  
    mov dx,1  
writeToDestEnd: 
    pop cx
    puts msgWrited
    ret
endp writeToDest

open proc
    push ax
    push cx
    mov ah, 3Dh			
    mov al, 20h			
    mov dx, offset src
    mov cl, 01h			
    int 21h
    jb openErr	

    mov srcFileName, ax	
    mov ah, 3Ch
    mov cx, 00h			    
    mov dx, offset dest
    int 21h
    jb openErr	

    mov destFileName, ax				
    jmp openEnd
openErr:
    mov dx,1
    cmp ax,02h
    je nf
    cmp ax, 03h
    je pnf
    cmp ax,04h
    je tomch
    cmp ax,05h
    je access
    cmp ax,0Ch
    je wrr
nf:
    puts msgErrNf
    jmp openEnd
pnf:
    puts msgErrPnf
    jmp openEnd
tomch:
    puts msgErrTmn
    jmp openEnd
access:
    puts msgErrAcc
    jmp openEnd
wrr:
    puts msgErrWrm
    jmp openEnd
openEnd:
    pop cx
    pop ax
    ret
endp open

parseArg proc
    push ax
    mov bx,0
    call skipSpaces
    dec cx
loopArg:    
    mov es:[di],al
    inc bx
    inc di
    inc si
    dec cx

    mov al,ds:[si]
    cmp al,0
    je parseArgEnd

    cmp al,spaceSym     
    je parseArgEnd

    cmp al,tabSym
    je parseArgEnd

    cmp al,CRSym
    je parseArgEnd

    cmp al, NLSym
    je parseArgEnd

    jmp loopArg
parseArgEnd:
    pop ax
    ret
endp parseArg

skipSpaces proc
loopSkip:
    mov al, ds:[si]
    cmp al,spaceSym     
    je skip

    cmp al,tabSym
    je skip

    cmp al,CRSym
    je skip

    cmp al, NLSym
    je skip
    jmp skipSpacesEnd
skip:
    inc si
    jmp loopSkip
skipSpacesEnd:    
    ret 
endp skipSpaces

parse proc
    push cx
    push si
    push di
    push bx
    mov dx,0
    xor cx,cx
    mov cl, cmdSize
    mov si, offset cmdBuff
    mov di, offset src
    call parseArg

    mov di, offset dest
    call parseArg

    mov di,offset delSym
    call parseArg
    mov delSymLen,bl
    cmp cx, 0
    je parseEnd
    puts msgErrArg
    mov dx,1
parseEnd:
    pop bx
    pop di
    pop si
    pop cx
    ret
endp parse

end main
