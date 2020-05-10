.model small
.stack 100h

.data 
msgStart db "Start...",0Dh,0Ah,'$'
msgEnd db "...End",0Dh,0Ah,'$'
msgPropArg db "Proper arguments example:comand [fullpath]\sourceFileName.txt [fullpath]\destinationFileName.txt [Set of symbols to be excluded]",0Dh,0Ah,'$'

msgErrArg db "Error: incorrect arguments.",0Dh,0Ah,'$'
msgErrBadId db "Error Bad file Id",0Dh,0Ah,'$'
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
msgErrWNL  db "Error while writing NL",0Dh,0Ah,'$'


msgTempFile db "Temp file:",0Dh,0Ah,'$'
msgTempFileO db "Temp file Opened",0Dh,0Ah,'$'
msgFlagDeWasKilled db "Flag deleted was killed",0Dh,0Ah,'$'
msgClosed db "Closed",0Dh,0Ah,'$'
msgDeleted db "Deleted",0Dh,0Ah,'$'
msgAdded db "Addition was ended",0Dh,0Ah,'$'
msgReaded db "Readed",0Dh,0Ah,'$'
msgSetPos db "SetPOs",0Dh,0Ah,'$'
msgEOF db "EOF encountered",0Dh,0Ah,'$'
msgWrited db "Writed!",0Dh,0Ah,'$'
msgWritedNL db "NL Writed!",0Dh,0Ah,'$'
msgTempFileW db "Temp Writed!",0Dh,0Ah,'$'
msgWTL db "Temp Writed LAST!",0Dh,0Ah,'$'
msgBuffEnded db "BuffEnded!",0Dh,0Ah,'$'
logFileName db "u:\log.txt",0
logFile dw 0 

NL  db 0Ah
srcFile dw 0
srcFileName db 50 dup(0)

destFile dw 0
destFileName db 50 dup(0)

tempFile dw 0
tempFileName db "u:\temp0.txt",0

flagKeepCheck db 0
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

delBuff db 50 dup('$')
delBuffLen db ?
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

Sleep proc 					; proc that wait for RTC to reach time=RTC+generalWaitTime
	push ax 
	push bx 
	push cx
	push dx 
	mov ax, 00h 
	int 1Ah 
	add dx, 0009h 
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
endp   Sleep

readFile proc   ; input: bx- file id, dx - recieving buffer, cx - num of bytes to be readed
    mov ah, 3Fh ; output : ax - num of readed bytes
    int 21h
    jb readFileErr
    jmp readFileEnd
readFileErr:
    mov dx,1
    push dx 
    mov dx, offset msgErrRead
    call puts 
    pop dx
    jmp readFileEnd
readFileEnd:
    push dx 
    mov dx, offset msgReaded
    call puts  
    pop dx    
    ret
endp readFile

checkBuff proc               ; procedure that recieve buff pointer, buff size, checks buffer for delimited symbols and
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
    je setKeepDel
    mov bx,buffSize

    cmp flagTempFile,1
    je checkBuffEnd

    mov flagTempFile,1
    push bx
    call createTemp
    pop bx
    jmp checkBuffEnd  
setKeepDel:    
    mov flagKeepDel,1

    jmp checkBuffEnd
checkSetPosMark:
    mov flagKeepCheck, 0
    call checkSetPos            ; sets proper bx and flagSetPos
    
    cmp flagTempFile,0
    je setKeepFlagDel
    call manageTempFile

setKeepFlagDel:
    mov flagKeepDel,0
checkBuffEnd:
    ret 
endp checkBuff

manageTempFile proc

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
    jne writeTempLast
    dec bx 
writeTempLast:
    mov cx,bx               ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; + check close that flushes
    mov bx,tempFile
    mov dx, buffPtr
    call writeToFile
    cmp dx,1
    je manageTempFileEnd
    push dx
    mov dx, offset msgTempFileW
    call puts
    pop dx

    cmp flagWriteNL,1
    jne closeTemp
    push bx
    mov bx,tempFile
    call writeNL   
    pop bx

closeTemp:
    push bx
    mov bx, tempFile
    call closeFile
    pop bx
    cmp dx,1
    je manageTempFileEnd

    call addTempToDest
    cmp dx,1
    je manageTempFileEnd
delTemp:
    push bx 
    mov bx, tempFile
    call closeFile
    pop bx
    cmp dx,1
    je manageTempFileEnd

    push bx
    mov dx, offset tempFileName
    call delFile
    pop bx
    mov flagDelim,0
    cmp dx,1
    je manageTempFileEnd
manageTempFileEnd:
    ret
endp manageTempFile

addTempToDest proc
    push dx
    mov dx, offset msgTempFile
    call puts
    pop dx

    push bx
    mov ah, 3Dh			
    mov al, 20h			
    mov dx, offset tempFileName
    mov cl, 01h			
    int 21h
    jb addTempToDestEnd	
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
    mov dx, offset msgErrDel      
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

openFile proc               ;input: dx - fileName(ASCIZ); al - access mode ; cl - attributes
    mov ah, 3Dh             ; ;; opens existing file
    int 21h                 ; output: ax - file identificator; puts Error message if operation was failed
    jb errOpenFile
    jmp openFileEnd
errOpenFile:
    call manageError
openFileEnd:
    ret
endp openFile

createFile proc                   ;input: dx - fileName(ASCIZ); cl - attributes
    mov ah, 3Ch                   ; ;; Creates file, if it's already exists - sets it's length to 0 
    int 21h                       ; output: ax - file identificator; puts Error message if operation was failed
    jb errcreateFile
    jmp createFileEnd
errcreateFile:
    call manageError
createFileEnd:
    ret
endp createFile

manageError proc                   
    mov dx,1
    cmp ax,02h
    je nf
    cmp ax, 03h
    je pnf
    cmp ax,04h
    je tomch
    cmp ax,05h
    je access
    cmp ax,06h
    je badid
    cmp ax,0Ch
    je wrr
    push dx 
    mov dx, offset msgErrOpen 
    call puts
    pop dx
    jmp manageErrorEnd
nf:
    push dx 
    mov dx, offset msgErrNf 
    call puts
    pop dx
    jmp manageErrorEnd
pnf:
    push dx 
    mov dx, offset msgErrPnf 
    call puts
    pop dx
    jmp manageErrorEnd
tomch:
    push dx 
    mov dx, offset msgErrTmn 
    call puts
    pop dx
    jmp manageErrorEnd
access:
    push dx 
    mov dx, offset msgErrAcc 
    call puts
    pop dx
    jmp manageErrorEnd
wrr:
    push dx 
    mov dx, offset msgErrWrm 
    call puts
    pop dx
    jmp manageErrorEnd
badid:
    push dx 
    mov dx, offset msgErrBadId 
    call puts
    pop dx
manageErrorEnd:
    ret
endp manageError


createTemp proc                 
    mov cx, 00h			    
    mov dx, offset tempFileName
    call createFile
    cmp dx,1
    je createTempEnd
    mov tempFile,ax
createTempEnd:
    push dx 
    mov dx, offset msgTempFileO
    call puts  
    pop dx    
    ret
endp createTemp

checkSetPos proc            ; procedure sets flagNL if last symbol was New Line symbol, and flagSetPos, if buffer is not ended yet
    cmp cx,1h               ; also proceudre adds 2 to buffSize(bx!) for CR and NL
    jne jmpToCheckSetPos
    mov flagWriteNL,1
jmpToCheckSetPos:    
    mov bx, buffSize
    sub bx, cx
    
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

copyDelToDelBuff proc     
    push cx
    push si
    push di
    push ax
    xor di,di
    xor cx,cx
    mov si, offset delSym
    mov cl, delSymLen
    xor ax,ax
loopCopyDel:    
    lodsb 
    mov delBuff[di], al
    inc di
    cmp al,0000h
    je copyDelToBuffEnd
    loop loopCopyDel 
copyDelToBuffEnd:
    xor cx, cx
    mov cl, delSymLen
    mov delBuffLen, cl
    pop ax    
    pop di
    pop si
    pop cx
    ret
endp copyDelToDelBuff

shiftBuff proc          ; input: cx - current pos to be shifted by symbols from right
    push di             ; ;; procedure sets new delBuffLen
    push si
    push dx
    push cx

    mov di, offset delBuff
    mov si,di
    xor dx,dx
    mov dl, delBuffLen
    sub dx, cx 
    add di, dx
    inc dx
    add si,dx
    rep movsb
    dec delBuffLen

    pop cx
    pop dx
    pop si
    pop di
    ret 
endp shiftBuff

checkDel proc           
    push cx                 ; input: al -symbol to be checked
    push di                 ; ;; 
    push ax                 ; output: flagKeepCheck, flagDelim
    cmp flagKeepCheck,1     
    je keepCheck            
    call copyDelToDelBuff
keepCheck:
    xor di, di
    xor cx, cx
    mov cl, delBuffLen
loopCheckDelSym:
    cmp al, delBuff[di]
    jne notDelSym
    call shiftBuff
notDelSym:
    inc di
    loop loopCheckDelSym

    cmp delBuffLen,0000h
    jne setFlagKeepCheck
    mov flagDelim,1
    mov flagKeepCheck, 0

    jmp checkDelEnd
setFlagKeepCheck:
    mov flagKeepCheck, 1
    mov flagDelim,0
checkDelEnd:
    pop ax
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

processLastTemp proc
    cmp flagKeepDel,1
    je closeLast

    cmp flagDelim,1
    je closeLast

    mov bx, tempFile
    call closeFile
    cmp dx,1
    je processLastTempEnd
    call addTempToDest
    cmp dx,1
    je processLastTempEnd
closeLast: 
    mov flagTempFile,0
    mov bx, tempFile
    call closeFile
    cmp dx,1
    je processLastTempEnd
    mov dx, offset tempFileName
    call delFile
processLastTempEnd:
    ret
endp processLastTemp 

processFile proc                ; ;; procedure that sets Source and destination file pointers, reads form source to buff, checks buffer for delimited symbols
    mov bx, srcFile             ; manages writing and setting position according to flags, that checkBuff procedure sets or kills
    xor al ,al 			        ; 
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
    jne checkReadedSize

    mov falgSkipNL,0
    dec buffSize
    inc buffPtr
checkReadedSize:
    cmp ax,0000h
    ja checkBufferMark
    cmp flagTempFile,1
    jne jmpEOF
    call processLastTemp

jmpEOF:
    jmp endOfFile

checkBufferMark:
    mov flagDelim,0
    mov flagSetPos,0
    call checkBuff         
    cmp dx,1
    je jmpProcessFileEnd

    cmp flagTempFile,1
    je writeToTemp
    cmp flagKeepDel,1
    je readLoop
    cmp flagDelim,0
    je writeToDest
    cmp flagSetPos,1
    je setNewPos
    jmp readLoop

jmpsetNewPos:
    jmp setNewPos

writeToTemp:
    mov cx,bx  
    mov dx,buffPtr  
    mov bx, tempFile
    call writeToFile
    cmp dx,1
    jne jmpReadLoop    ; if no error

    jmp processFileEnd
writeToDest:
    cmp flagDeleted,1          ; sign that last line was processed already
    je killDelFlag
    cmp flagWriteNL,1
    jne writeDest
    dec bx 
writeDest:
    mov cx,bx
    mov bx,destFile
    mov dx, buffPtr
    call writeToFile
    cmp dx,1
    je processFileEnd

    cmp flagWriteNL,1
    jne checkMaxBuffCase
    push bx
    mov bx, destFile
    call writeNL
    pop bx
checkMaxBuffCase:
    cmp bx,maxBuff
    jne setNewPos

jmpReadLoop:    
    jmp readLoop
jmpjmpprocessFileEnd:    
    jmp processFileEnd

killDelFlag:
    mov flagDeleted,0
    push dx 
    mov dx, offset msgFlagDeWasKilled
    call puts  
    pop dx
setNewPos:
    push dx 
    mov dx, offset msgSetPos
    call puts  
    pop dx
    add buffPtr, bx 
    sub buffSize, bx
    cmp buffSize, 0000h
    jbe buffEnded                
    jmp checkBufferMark 
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

writeNL proc
    mov cx,1h
    mov dx,offset NL
    mov ah,40h          
    int 21h
    jb errWriteNL
    mov falgSkipNL,1
    mov flagWriteNL,0
    push dx
    mov dx, offset msgWritedNL
    call puts 
    pop dx
    jmp writeNLEnd
errWriteNL:
    mov dx,1
    push dx
    mov dx, offset msgErrWNL
    call puts
    pop dx
writeNLEnd:    
    ret
endp writeNL

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
    mov al, 20h			
    mov dx, offset srcFileName
    mov cl, 01h			
    call openFile
    cmp dx,1 
    je openSrcAndDestEnd
    mov srcFile, ax	
    
    mov cx, 00h			    
    mov dx, offset destFileName
    call createFile
    cmp dx,1 
    je openSrcAndDestEnd
    mov destFile, ax
openSrcAndDestEnd:  
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
