.model small
.stack 100h

.data 
msgStart db "Start...",0Dh,0Ah,'$'
msgEnd db "...End",0Dh,0Ah,'$'
msgErrArg db "Error: incorrect arguments.",0Dh,0Ah,'$'
msgPropArg db "Proper arguments example:comand [fullpath]\sourceFileName.txt [fullpath]\destinationFileName.txt [Set of symbols to be excluded]",0Dh,0Ah,'$'

msgTempFile db "Temp file:",0Dh,0Ah,'$'
msgTempFileO db "Temp file Opened",0Dh,0Ah,'$'

msgErrNf db "Error File not found",0Dh,0Ah,'$'
msgErrPnf db "Error Path not found",0Dh,0Ah,'$'
msgErrWrm db "Error Wrong mode",0Dh,0Ah,'$'
msgErrAcc db "Error access denied",0Dh,0Ah,'$'
msgErrTmn db "Error too many opened files",0Dh,0Ah,'$'
msgErrRead db "Error while reading",0Dh,0Ah,'$'
msgErrOpen db "Error while opening",0Dh,0Ah,'$'
msgErrDel db "Error while deleting",0Dh,0Ah,'$'
msgErrClose db "Error while closing",0Dh,0Ah,'$'
msgErrSet db "Error setting pointer",0Dh,0Ah,'$'
msgErrNumWr db "Error while writing",0Dh,0Ah,'$'
msgErrTTD db "Error while adding temp to dest",0Dh,0Ah,'$'
msgErrBid db "Error Bad id",0Dh,0Ah,'$'
msgErrPutLog db "Error while putting in log",0Dh,0Ah,'$'
msgErrOpenLog db "Error while opening log",0Dh,0Ah,'$'

msgFlagDeWasKilled db "Flag deleted was killed",0Dh,0Ah,'$'
msgClosed db "Closed",0Dh,0Ah,'$'
msgDeleted db "Deleted",0Dh,0Ah,'$'
msgAdded db "Addition was ended",0Dh,0Ah,'$'
msgReaded db "Readed",0Dh,0Ah,'$'
msgSetPos db "SetPOs",0Dh,0Ah,'$'
msgEOF db "EOF encountered",0Dh,0Ah,'$'
msgWrited db "Writed!",0Dh,0Ah,'$'
msgWritedNL db "NL Writed!",0Dh,0Ah,'$'
msgWritedNLL db "NL Writed LAST!",0Dh,0Ah,'$'
msgTempFileW db "Temp Writed!",0Dh,0Ah,'$'
msgWTL db "Temp Writed LAST!",0Dh,0Ah,'$'
msgBuffEnded db "BuffEnded!",0Dh,0Ah,'$'
NL  db 0Ah
srcFile dw 0
srcFileName db 50 dup(0)

destFile dw 0
destFileName db 50 dup(0)

tempFile dw 0
tempFileName db "u:\temp0.txt",0
logFileName db "u:\log.txt",0
logFile dw 0 

flagWriteNL db 0 
flagDelim db 0
flagKeepDel db 0
flagSetPos db 0
flagTempFile db 0
flagDeleted db 0
falgSkipNL db 0

maxBuff equ 1000
buffPtr dw ?
buffSize dw ?
buff db maxBuff+1 dup('$')
buffTemp db maxBuff dup ('$')
buffTempSize dw ?

maxCmd  equ 126
cmdSize db ?
cmdBuff db maxCmd dup(?)

delSym db 50 dup(0)
delSymLen db ?
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
    call openLog
    cmp dx,1
    je mainEnd

    push dx 
    mov dx, offset msgStart
    call puts  
    pop dx
    mov cmdSize,bl
    call parse
    cmp dx,1
    je mainEnd

    call openSrcAndDest
    cmp dx,1
    je mainEnd
    
    call processFile
    mov bx, srcFile
    call closeFile
    cmp dx,1
    je mainEnd

    mov bx, destFile
    call closeFile
    cmp dx,1
    je mainEnd
    cmp flagTempFile,1
    jne mainEnd
    mov bx,tempFile
    call closeFile              ; check del
    cmp dx,1
    je mainEnd
        
mainEnd:
    push dx 
    mov dx, offset msgEnd
    call puts 
    pop dx
    mov bx, logFile
    call closeFile
    
    mov ah,4Ch
    int 21h

openLog proc
    mov ah, 3Ch
    mov cx, 00h			    
    mov dx, offset logFileName
    int 21h
    jb errOpenLog	
    mov logFile, ax
    jmp openLogEnd
errOpenLog:
    push dx
    mov dx, offset msgErrOpenLog
    push ax
    mov ah, 09h
    int 21h
    pop ax
    pop dx
    mov dx,1
openLogEnd:

    ret
endp openLog

puts proc
    push bx
    push cx
    push ax
    push di 
    mov si,dx
    
loopLog:
    mov cx,0001h  
    mov bx, logFile
    mov ah,40h          
    int 21h
    jb errPuts

    lodsb
    cmp al,'$'
    je loopLogEnd 
    mov dx,si

    jmp loopLog
loopLogEnd:
    jmp putsEnd
errPuts:
    push dx
    mov dx,offset msgErrPutLog
    push ax
    mov ah, 09h
    int 21h
    pop ax
    pop dx
    
putsEnd:
    pop di 
    pop ax
    pop cx
    pop bx
    ret
endp puts

readFile proc
    mov ah, 3Fh
    int 21h
    jb readFileErr
    jmp readedMsg
readFileErr:
    mov dx,1
    push dx 
    mov dx, offset msgErrRead
    call puts 
    pop dx
    jmp readFileEnd
readedMsg:    
    push dx 
    mov dx, offset msgReaded
    call puts 
    pop dx
readFileEnd:    
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
    mov cx, buffSize         ;num of symbols that were readed
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
    mov bx,buffSize
    cmp flagTempFile,1
    je jmpcheckBuffEnd
    mov flagTempFile,1
    push bx
    call openTemp
    pop bx
jmpcheckBuffEnd:    
    jmp checkBuffEnd
jmpsetKeepFalg0:
    jmp setKeepFalg0    
setKeep:    
    mov flagKeepDel,1

    jmp checkBuffEnd
checkSetPosMark:
    cmp cx,1h
    jne jmpToCheckSetPos
    mov flagWriteNL,1
jmpToCheckSetPos:    
    mov bx, buffSize
    sub bx, cx
    call checkSetPos            ; sets proper bx and flagSetPos
    
    cmp flagTempFile,0
    je jmpsetKeepFalg0
    mov flagTempFile,0

    cmp flagKeepDel,1
    je delTemp

    cmp flagDelim,1
    je delTemp
    push dx 
    mov dx, offset msgWTL
    call puts
    pop dx
    cmp flagWriteNL,1
    jne wttl
    dec bx 
wttl:
    mov cx,bx               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; + check close that flushes
    mov bx,tempFile
    mov dx, buffPtr
    call writeToFile
    cmp dx,1
    je jmpcheckBuffEnd

    cmp flagWriteNL,1
    jne closeTemp
    push bx
        mov cx,1h
        mov bx,tempFile
        mov dx,offset NL
        mov ah,40h          
        int 21h
        jb checkBuffEnd
        mov falgSkipNL,1
        mov flagWriteNL,0
        push dx 
        mov dx, offset msgWritedNLL 
        call puts 
        pop dx
    pop bx
closeTemp:
    push bx
    mov bx, tempFile
    call closeFile
    pop bx
    cmp dx,1
    je checkBuffEnd
    
    call addTempToDest
    cmp dx,1
    je checkBuffEnd
delTemp:
    push bx
    mov bx, tempFile
    call closeFile
    pop bx
    cmp dx,1
    je checkBuffEnd
    
    push bx
    mov dx, offset tempFileName
    call delFile
    pop bx
    mov flagDelim,0
    cmp dx,1
    je checkBuffEnd
setKeepFalg0:
    mov flagKeepDel,0
checkBuffEnd:
    ret 
endp checkBuff

addTempToDest proc
    push bx
    push dx 
    mov dx, offset msgTempFile
    call puts
    pop dx

    push dx 
    mov dx, offset msgTempFileO
    call puts
    pop dx
    mov ah, 3Dh			
    mov al, 20h			
    mov dx, offset tempFileName
    mov cl, 01h			
    int 21h
    jb checkBuffEnd	
    mov tempFile, ax	

    mov bx, tempFile
    xor al ,al 			
    xor cx, cx
    xor dx, dx			
    call setFilePtr
    cmp dx,1
    je addTempToDestEnd
    mov buffTempSize,maxBuff
loopWriteTempToDest:

    mov bx, tempFile
    mov cx, buffTempSize
    mov dx, offset buffTemp
    call readFile
    cmp dx,1
    je errAddTempToDest
    cmp ax,0000h
    jbe addTempToDestEnd
    mov buffTempSize,ax

    mov cx, buffTempSize
    mov bx, destFile
    mov dx, offset buffTemp
    call writeToFile
    cmp dx,1
    je errAddTempToDest
    jmp loopWriteTempToDest

    jmp addTempToDestEnd
errAddTempToDest:
    push dx 
    mov dx, offset msgErrTTD
    call puts
    pop dx
addTempToDestEnd:
    push dx 
    mov dx, offset msgAdded
    call puts
    pop dx
    pop bx
    ret
endp addTempToDest

closeFile proc
    push dx 
    mov dx, offset msgClosed
    call puts   
    pop dx
    xor ax,ax
    mov ah,3eh
    int 21h
    jb errcloseFile
    jmp closeFileEnd
errcloseFile:
             ;;;;
    push dx 
    mov dx, offset msgErrClose
    call puts
    pop dx
    mov dx,1
closeFileEnd:
  
    ret
endp closeFile

delFile proc 
    mov ah,41h
    int 21h
    jb errDelTemp
    mov flagDeleted,1
    jmp delFileEnd
errDelTemp:
    push dx 
    mov dx, offset msgErrDel      ;;;;;;
    call puts  
    pop dx
    mov dx,1
delFileEnd:
    push dx 
    mov dx, offset msgDeleted 
    call puts 
    pop dx
    ret
endp delFile

openTemp proc
    push dx 
    mov dx, offset msgTempFileO 
    call puts 
    pop dx
    mov ah, 3Ch
    mov cx, 00h			    
    mov dx, offset tempFileName
    int 21h
    jb errorTempOpen
    mov tempFile,ax

    mov bx, tempFile
    xor al ,al 			
    xor cx, cx
    xor dx, dx			
    call setFilePtr
    cmp dx,1
    je errorTempOpen
    jmp openTempEnd
errorTempOpen:
    mov dx,1
    push dx 
    mov dx, offset msgTempFile 
    call puts 
    pop dx
    cmp ax,03h 
    je tpath
    cmp ax,04h
    je ttmn
    cmp ax,05h
    je tacc
    cmp ax,06h
    je tid
    push dx 
    mov dx, offset msgErrOpen 
    call puts
    pop dx
    jmp openTempEnd
tid:    
    push dx 
    mov dx, offset msgErrBid 
    call puts
    pop dx
    jmp openTempEnd
tpath:
    push dx 
    mov dx, offset msgErrPnf 
    call puts
    pop dx
    jmp openTempEnd
ttmn:
    push dx 
    mov dx, offset msgErrTmn 
    call puts
    pop dx
    jmp openTempEnd
tacc:
    push dx 
    mov dx, offset msgErrAcc 
    call puts
    pop dx
openTempEnd:    
    ret
endp openTemp

checkSetPos proc
    add bx,2
    cmp flagDelim,1
    je checkSetPosPos
    jmp checkSetPosEnd
checkSetPosPos:
    cmp bx, 2
    jbe checkSetPosEnd
    mov flagSetPos,1
checkSetPosEnd:
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

setFilePtr proc

    mov ah,42h
    int 21h
    jb errSetFilePtr
    
    jmp setFilePtrEnd
errSetFilePtr:
    push dx 
    mov dx, offset msgErrSet 
    call puts
    pop dx
    mov dx,1
setFilePtrEnd:
    ret
endp setFilePtr

processFile proc

    mov bx, srcFile
    xor al ,al 			
    xor cx, cx
    xor dx, dx			
    call setFilePtr
    cmp dx,1
    je jmpProcessFileEnd
    
    mov bx, destFile
    xor al ,al 			
    xor cx, cx
    xor dx, dx			
    call setFilePtr
    cmp dx,1
    je jmpProcessFileEnd

    jmp readLoop
jmpProcessFileEnd:
    jmp processFileEnd
jmpEndOfFile:
    jmp endOfFile
readLoop:
    mov buffSize,maxBuff
    mov buffPtr, offset buff

    mov bx, srcFile
    mov cx, maxBuff
    mov dx, buffPtr
    call readFile
    cmp dx,1
    je jmpProcessFileEnd
    mov buffSize,ax
    cmp falgSkipNL,1
    jne checkAx
    mov falgSkipNL,0
    dec buffSize
    inc buffPtr
checkAx:
    cmp ax,0000h
    ja checkBuffer
    cmp flagTempFile,1
    jne jmpEndOfFile
    cmp flagKeepDel,1
    je closeLast

    cmp flagDelim,1
    je closeLast

    mov bx, tempFile
    call closeFile
    cmp dx,1
    je jmpProcessFileEnd
    call addTempToDest
closeLast: 
    mov flagTempFile,0
    mov bx, tempFile
    call closeFile
    cmp dx,1
    je jmpjmpjmpProcessFileEnd
    mov dx, offset tempFileName
    call delFile
    cmp dx,1
    je jmpjmpjmpProcessFileEnd  

    jmp jmpEndOfFile
checkBuffer:
    mov flagDelim,0
    mov flagSetPos,0
    call checkBuff          ; flags setPos, flagKeepDelim, flagDelim, bx -new pos
    cmp dx,1
    je jmpjmpjmpProcessFileEnd
    cmp flagTempFile,1
    je writeToTemp
    cmp flagKeepDel,1
    je jmpjmpReadLoop
    cmp flagDelim,0
    je writeToDest
    cmp flagSetPos,1
    je jmpjmpsetNewPos
    jmp readLoop
jmpjmpjmpProcessFileEnd:
    jmp ProcessFileEnd    
jmpjmpReadLoop:
    jmp readLoop
jmpjmpsetNewPos:
    jmp setNewPos
writeToTemp:
    push dx 
    mov dx, offset msgTempFileW 
    call puts
    pop dx

    mov cx,bx  
    mov dx,buffPtr  
    mov bx, tempFile
    call writeToFile
    cmp dx,1
    je jmpjmpprocessFileEnd           ;;;;;;;;;;;;;
    jmp readLoop
writeToDest:
    cmp flagDeleted,1          ; sign that last line was processed already
    je jmpSetNewPos
    cmp flagWriteNL,1
    jne wtd
    dec bx 
    wtd:
    mov cx,bx
    mov bx,destFile
    mov dx, buffPtr
    call writeToFile
    cmp dx,1
    je jmpjmpprocessFileEnd

    cmp flagWriteNL,1
    jne checkNext
    push bx
        mov cx,1h
        mov bx,destFile
        mov dx,offset NL
        mov ah,40h          
        int 21h
        jb jmpjmpprocessFileEnd
        mov falgSkipNL,1
        mov flagWriteNL,0
        push dx 
        mov dx, offset msgWritedNL 
        call puts 
        pop dx
    pop bx
checkNext:
    cmp bx,maxBuff
    je jmpreadLoop
    jmp setNewPos
jmpjmpprocessFileEnd:    
    jmp processFileEnd
jmpreadLoop:
    jmp readLoop
jmpSetNewPos:
    push dx 
    mov dx, offset msgFlagDeWasKilled 
    call puts
    pop dx
    mov flagDeleted,0
setNewPos:
    push dx 
    mov dx, offset msgSetPos 
    call puts
    pop dx
    add buffPtr, bx 
    sub buffSize, bx
    cmp buffSize, 0000h
    je buffEnded                ;;;;;;check jbe
    jmp checkBuffer 
buffEnded:  
    push dx 
    mov dx, offset msgBuffEnded 
    call puts   
    pop dx
    jmp readLoop
endOfFile:
    push dx 
    mov dx, offset msgEOF 
    call puts 
    pop dx
processFileEnd:
    ret 
endp processFile

writeToFile proc        ; input: bx - num to write
    push cx             ; output: dx -1 if error, ax -err code              
    mov ah,40h          
    int 21h
    pop bx
    cmp ax, bx
    je writeToFileEnd
    push dx 
    mov dx, offset msgErrNumWr 
    call puts 
    pop dx
    mov dx,1  
writeToFileEnd: 
    push dx 
    mov dx, offset msgWrited 
    call puts 
    pop dx
    ret
endp writeToFile

openSrcAndDest proc
    mov ah, 3Dh			
    mov al, 20h			
    mov dx, offset srcFileName
    mov cl, 01h			
    int 21h
    jb openErr	
    mov srcFile, ax	

    mov ah, 3Ch
    mov cx, 00h			    
    mov dx, offset destFileName
    int 21h
    jb openErr	
    mov destFile, ax

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
    push dx 
    mov dx, offset msgErrNf 
    call puts
    pop dx
    jmp openEnd
pnf:
    push dx 
    mov dx, offset msgErrPnf 
    call puts
    pop dx
    jmp openEnd
tomch:
    push dx 
    mov dx, offset msgErrTmn 
    call puts
    pop dx
    jmp openEnd
access:
    push dx 
    mov dx, offset msgErrAcc 
    call puts
    pop dx
    jmp openEnd
wrr:
    push dx 
    mov dx, offset msgErrWrm 
    call puts
    pop dx
    jmp openEnd
openEnd:  
    ret
endp openSrcAndDest

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
    mov di, offset srcFileName
    call parseArg

    mov di, offset destFileName
    call parseArg

    mov di,offset delSym
    call parseArg
    mov delSymLen,bl
    cmp cx, 0
    je parseEnd
    push dx 
    mov dx, offset msgErrArg 
    call puts
    pop dx
    push dx 
    mov dx, offset msgPropArg 
    call puts
    pop dx
    mov dx,1
parseEnd:
    pop bx
    pop di
    pop si
    pop cx
    ret
endp parse

end main
    

