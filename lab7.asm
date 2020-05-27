.model small			
.stack 100h			
			
.data 				
msgStart db "Start...",0Dh,0Ah,'$'
msgEnd db "...End",0Dh,0Ah,'$'
msgPropArg db "Two arguments required. Proper arguments example:command [fullpath]sourceFileName.txt [fullpath]destinationFileName.txt",0Dh,0Ah,'$'
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
msgErrPutLog db "Error while putting in log",0Dh,0Ah,'$'
msgErrOpenLog db "Error while opening log",0Dh,0Ah,'$'
msgErrWS  db "Error while writing symbol",0Dh,0Ah,'$'


msgCreate db "Created",0Dh,0Ah,'$'
msgOpened db "Opened",0Dh,0Ah,'$'
msgClosed db "Closed",0Dh,0Ah,'$'
msgDeleted db "Deleted",0Dh,0Ah,'$'
msgReaded db "Readed",0Dh,0Ah,'$'
msgWrited db "Writed!",0Dh,0Ah,'$'
msgWritedNL db "NL Writed!",0Dh,0Ah,'$'
msgTempFileW db "Temp Writed!",0Dh,0Ah,'$'
logFileName db "u:\log.txt",0
logFile dw 0 

CR db 0Dh
NL  db 0Ah
srcFile dw 0
srcFileName db 50 dup(0)

destFile dw 0
destFileName db 50 dup(0)

tempFile dw 0
tempFileName db "u:\temp0.txt",0
tf db "u:\TEMP0.txt",0

flagKeepCheck db 0
flagWriteNL db 0 
flagDelim db 0
flagKeepDel db 0
flagSetPos db 0
flagDeleted db 0

maxBuff equ 10
buffPtr dw ?
stopSym db '$'
buffSize dw ?
buffTemp db maxBuff dup ('$')
buffTempSize dw ?

maxCmd  equ 126
cmdSize db ?
cmdBuff db maxCmd dup(?)

delBuff db 50 dup('$')
delBuffLen db ?
delSym db 50 dup(0)
delSymLen db ?
syms db 5Fh
spaceSym equ 20h
tabSym equ 09h 
CRSym equ 0Dh
NLSym equ 0Ah
;//////////////////////////////////////////////
;//////////////////////////////////////////////
;//////////////////////////////////////////////
;//////////////////////////////////////////////
stops db '|'
stope db '@'
stopb db '>'
rOperand dw 0000h
lOperand dw 0000h
prevOperation db 0009h
operation db 000Fh
operationPos dw ?
lastNumBuff db 7 dup('$')
buff db maxBuff+2 dup('$')
lastNumBuffLen dw 0000h
operandCounter db 00
resBuff db 7 dup ('$')
result dw 0000h
minusSym db '-'
leftSize dw ?

flagLoadTemp db 0
flagZeroOne db 0
flagOneZero db 0
flagSecond db 0
flagLRwasWrited db 0
flagDupl db 0
flagThereWhereMD db 0
flagNoMDInBuff db 0
flagRem db 0
flagNeg db 0
flagMinus db 0
flagNotYet db 0
flagLastRes db 0
flagFirstO db 0
flagSkipASym db 0
flagSTS db 0
flagTTT db 0
flagSkipMin db 2
flagOnce db 1
flagEndB db 0 

msgCheckEnded db "Source file check ended",0Dh,0Ah,'$'
msgGetLeft db "Getleft",0Dh,0Ah,'$'
msgGetRight db "Get right",0Dh,0Ah,'$'
msgWritedoddR db "writed old rem",0Dh,0Ah,'$'
msgOperation db "operation",0Dh,0Ah,'$'
msgOperationM db "Minus oper",0Dh,0Ah,'$'
msgWres db "Writed res",0Dh,0Ah,'$'
msgNulled db "Nulled",0Dh,0Ah,'$'
msgFirstPass db "First pass",0Dh,0Ah,'$'
msgSecondPass db "Second Pass",0Dh,0Ah,'$'
msgRemem db "Remebered",0Dh,0Ah,'$'
msgReached db "Reached eofbuff",0Dh,0Ah,'$'
msgMDFound db "md found",0Dh,0Ah,'$'
msgMDNot db "md not found",0Dh,0Ah,'$'
msgWOLN db "wo last num",0Dh,0Ah,'$'
msgNlFound db "New Line found",0Dh,0Ah,'$'
msgSecPrep db "Ready for second pass",0Dh,0Ah,'$'

msgErrSecondPass db "Error: while second pass",0Dh,0Ah,'$'
msgErrNumSize db "Error: number size is bigger than 5 digits", 0DH, 0Ah,'$'
msgErrLOverFlow db "Error: overflow, while getting left operand", 0DH, 0Ah,'$'
msgErrROverFlow db "Error: overflow, while getting right operand", 0DH, 0Ah,'$'
msgErrWriteNumBuff db "Error: while writing number buff",0Dh,0Ah,'$'
msgErrRemOverFlow db "Error: overflow, while remembering last nuber", 0DH, 0Ah,'$'
msgErrFileFormat db "Error: file should have form A0x0A1x1A2x2...x(N-1)AN. Ai - number, xi - operation. Strings like A1x1A2...\n are allowed",0Dh,0Ah,'$'
msgErrCalcOverFlow db "Error: overflow, while calculating", 0DH, 0Ah,'$'
msgErrCalcDivis db "Error: reminder!", 0DH, 0Ah,'$'
msgErrZeroDiv db "Error: zero division while calculating", 0DH, 0Ah,'$'
msgErrGetLeft db "Error: whie getting left operand",0Dh,0Ah,'$'
msgErrProcessFile db "Error: whie processing file",0Dh,0Ah,'$'
msgErrFirstPass db "Error: while first pass",0Dh,0Ah,'$'
msgErrDupl db "Error: operation duplication",0Dh,0Ah,'$'
msgErrLoad db "Error: while Loading overlay",0Dh,0Ah,'$'
msgErrAlloc db "Error: while allocating memory",0Dh,0Ah,'$'
msgErrFree db "Error: while freeing memory",0Dh,0Ah,'$'

overlayOffset dw ?
codeSegment dw ?
overlaySegment dw ?
BLOCK DD 0
pathAdd db "add.exe",0
pathDiv db "div.exe",0
pathMul db "mul.exe",0
pathSub db "sub.exe",0
pathPtr dw 0000h


.code

    prepLoadOverlay macro   
        mov codeSegment, cs
        mov ax, es ; PSP
        mov bx, ZSEG
        sub bx, ax 
        mov ah, 4Ah   
        int 21h 
        jnc allocate 
        mov dx, offset msgErrFree
        jmp errPrepLoadOverlay
        allocate:
        mov bx, 1000h
        mov ah, 48h
        int 21h
        jnc setBlock  
        mov dx,offset msgErrAlloc
        jmp errPrepLoadOverlay
        setBlock:
        mov overlaySegment, ax

        
        mov ax, codeSegment
        mov bx, overlaySegment
        sub bx, ax
        mov cl, 4
        shl bx, cl
        mov overlayOffset, bx 
        jmp prepLoadOverlayEnd
    errPrepLoadOverlay:
        call putStr
        mov dx, 1
    prepLoadOverlayEnd:
    endm prepLoadOverlay

main:
    push ds
    mov ax,@data
    mov ds,ax
    prepLoadOverlay
    cmp dx,1
    je mainEnd
    pop ds
    mov ax, @data
    mov es, ax
    xor cx, cx
    mov cl, ds:[80h]
    mov bx,cx
    mov si,81h
    mov di, offset cmdBuff
    rep movsb 

    mov ax,@data
    mov ds,ax
 
    call openLog
    cmp dx,1
    je mainEnd
    
    parseArgs:
    push dx 
    mov dx, offset msgStart
    call putStr  
    pop dx
    mov cmdSize,bl
    call parse
    cmp dx,1
    je mainEnd
    

    call prepFiles
    cmp dx,1
    je mainEnd
    
    call checkFormat
    cmp dx,1
    je mainEnd

    call processFile
    cmp dx,1
    je mainEnd

    mov bx, tempFile
    call closeFile
    cmp dx,1
    je mainEnd

    mov bx, srcFile
    call closeFile
    cmp dx,1
    je mainEnd

    mov bx, destFile
    call closeFile
    cmp dx,1
    je mainEnd            
    mov dx, offset tempFileName
    call delFile    
mainEnd:
    push dx 
    mov dx, offset msgEnd
    call putStr 
    pop dx
    mov bx, logFile
    call closeFile
    
    mov ah,4Ch
    int 21h

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
        call putStr
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
        call putStr  
        pop dx
        mov dx,1
    delFileEnd:
        push dx
        mov dx, offset msgDeleted
        call puts
        pop dx
        ret
    endp delFile

    writeSym proc
        push cx
        push bx
        mov cx,1h
        mov dx,ax
        mov ah,40h          
        int 21h
        jb errWriteSym
        push dx
        mov dx, offset msgWritedNL
        call puts 
        pop dx
        jmp writeSymEnd
    errWriteSym:
        mov dx,1
        push dx
        mov dx, offset msgErrWS
        call putStr
        pop dx
    writeSymEnd: 
        pop bx   
        pop cx
        ret
    endp writeSym

    writeDividers proc
        push cx
        push bx
        push ax
        push dx
        mov bx, ax
        xor ax,ax
        mov dx, offset stops
        mov ah,40h
        mov cx,1h
        int 21h
        cmp flagTTT,0
        je nextflag
        mov flagTTT,0
        push bx
        push ax
        mov bx, ax
        mov cx,1
        mov dx, offset stopb
        mov ah,40h
        int 21h
        pop ax
        pop bx
        jmp writeDividersEnd
    nextflag:            
        cmp flagSTS,0
        je writeDividersEnd
        mov flagSTS,0
        push bx
        push ax
        mov bx, ax
        mov cx,1
        mov dx, offset stope
        mov ah,40h
        int 21h
        pop ax
        pop bx
    writeDividersEnd:
        pop dx
        pop ax
        pop bx
        pop cx
        ret
    endp writeDividers

    writeToFile proc            ; input: bx - num to write
        push cx             ; output: dx -1 if error, ax -err code 
        cmp flagSecond,1
        jne writetext
        mov bx, destFile
    writetext:                     
        mov ah,40h          
        int 21h
        pop bx
        cmp ax, bx
        jne writeErr  

        cmp flagSecond,1
        jmp writeToFileEnd
        push ax
        mov ax, destFile
        call writeDividers 
        pop ax

        jmp writeToFileEnd
    writeErr:            
        push dx 
        mov dx, offset msgErrNumWr 
        call putStr
        pop dx
        mov dx,1  
    writeToFileEnd: 
        push dx 
        mov dx, offset msgWrited
        call puts  
        pop dx
        ret
    endp writeToFile

    prepFiles proc
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
        push dx
        mov dx, offset msgCreate
        call puts
        pop dx

        mov cx, 00h			    
        mov dx, offset tempFileName
        call createFile
        cmp dx,1
        je openSrcAndDestEnd
        mov tempFile,ax
        push dx
        mov dx, offset msgCreate
        call puts
        pop dx

        mov bx, srcFile            
        xor al ,al 			        
        xor cx, cx
        xor dx, dx			
        call setFilePtr
        cmp dx,1
        je openSrcAndDestEnd 

        mov bx, destFile
        xor al ,al 			
        xor cx, cx
        xor dx, dx			
        call setFilePtr
        cmp dx,1
        je openSrcAndDestEnd
        
        mov bx, tempFile
        xor al ,al 			
        xor cx, cx
        xor dx, dx			
        call setFilePtr
        cmp dx,1
        je openSrcAndDestEnd
    openSrcAndDestEnd:  
        ret
    endp prepFiles

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

        cmp cx, 0
        je parseEnd
    errParse:
    push dx 
    mov dx, offset msgErrArg 
    call putStr
    pop dx
    push dx 
    mov dx, offset msgPropArg 
    call putStr
    pop dx
    mov dx,1
    parseEnd:
    pop bx
    pop di
    pop si
    pop cx
    ret
    endp parse

    openLog proc
            mov cx, 00h			    
            mov dx, offset logFileName
            call createFile
            jb errOpenLog	
            mov logFile, ax
            push dx
            mov dx, offset msgCreate
            call puts
            pop dx
            jmp openLogEnd
    errOpenLog:
        push dx
        mov dx, offset msgErrOpenLog
        call putStr
        pop dx
        mov dx,1
    openLogEnd:
        ret
    endp openLog

    putStr proc
        push ax
        mov ah, 09h
        int 21h
        pop ax

        ret
    endp putStr

    puts proc
            push bx
            push cx
            push ax
            push di 
            push si
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
        call putStr
        pop dx
    putsEnd:
        pop si
        pop di 
        pop ax
        pop cx
        pop bx
        ret
    endp puts

    readFile proc               ; input: bx- file id, dx - recieving buffer, cx - num of bytes to be readed
            mov ah, 3Fh         ; output : ax - num of readed bytes
            int 21h
            jb readFileErr
            jmp readFileEnd
    readFileErr:
        mov dx,1
        push dx 
        mov dx, offset msgErrRead
        call putStr 
        pop dx
        jmp readFileEnd
    readFileEnd:
        push dx 
        mov dx, offset msgReaded
        call puts  
        pop dx    
        ret
    endp readFile

    openFile proc                   ;input: dx - fileName(ASCIZ); al - access mode ; cl - attributes
            mov ah, 3Dh             ; ;; opens existing file
            int 21h                 ; output: ax - file identificator; puts Error message if operation was failed
            jb errOpenFile
            jmp openFileEnd
    errOpenFile:
        call manageError
    openFileEnd:
        push dx
        mov dx, offset msgOpened
        call puts
        pop dx
        ret
    endp openFile

    createFile proc                        ;input: dx - fileName(ASCIZ); cl - attributes
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
            call putStr
            pop dx
            jmp manageErrorEnd
    nf:
        push dx 
        mov dx, offset msgErrNf 
        call putStr
        pop dx
        jmp manageErrorEnd
    pnf:
        push dx 
        mov dx, offset msgErrPnf 
        call putStr
        pop dx
        jmp manageErrorEnd
    tomch:
        push dx 
        mov dx, offset msgErrTmn 
        call putStr
        pop dx
        jmp manageErrorEnd
    access:
        push dx 
        mov dx, offset msgErrAcc 
        call putStr
        pop dx
        jmp manageErrorEnd
    wrr:
        push dx 
        mov dx, offset msgErrWrm 
        call putStr
        pop dx
        jmp manageErrorEnd
    badid:
        push dx 
        mov dx, offset msgErrBadId 
        call putStr
        pop dx
    manageErrorEnd:
        ret
    endp manageError

    setFilePtr proc
            mov ah,42h
            int 21h
            jb errSetFilePtr
            
            jmp setFilePtrEnd
    errSetFilePtr:
        push dx 
        mov dx, offset msgErrSet 
        call putStr
        pop dx
        mov dx,1
    setFilePtrEnd:
        ret
    endp setFilePtr 

    checkFBuff proc
        push si
        push cx
        push ax
        mov si, buffPtr
        mov cx, buffSize
        cmp flagSkipASym,1
        jne checkFbeg
        inc si
        dec cx
        cmp cx,0000h
        je jmpcheckFBuffEnd
        mov flagSkipASym,0
    checkFbeg:        
        cmp flagDupl,1
        je loopCheckF
        cmp operandCounter,00h
        ja loopCheckF
        dec cx
        lodsb 
        
        cmp al,'-'
        jne checkOp
        mov flagDupl,1
        mov flagMinus,1
        mov operandCounter,00
        jmp loopCheckF
    checkOp:        
        cmp al,'+'
        je jmpErrCheckF
        cmp al,'*'
        je jmpErrCheckF
        cmp al,'/'
        je jmpErrCheckF

        cmp al,'0'
        jb jmpErrCheckF
        cmp al,'9'
        ja jmpErrCheckF
        
        inc operandCounter
        jmp loopCheckF
    jmpcheckFBuffEnd:
        jmp checkFBuffEnd        
    jmpErrCheckF:
        jmp errCheckFBuff
    loopCheckF:
        lodsb
        
        cmp al, 0Dh
        jne checkFnext
        
        cmp flagFirstO,1
        jne jmpErrCheckF
        cmp flagDupl,1
        je jmpErrCheckF
        mov flagFirstO, 0
        mov operandCounter,00h  
        dec cx
        cmp cx, 0000h
        jbe skipASym
        inc si
        dec cx
        cmp cx, 0000h
        jbe jmpcheckFBuffEnd 
        mov flagRem,1
        mov bx, si
        jmp jmpcheckFBuffEnd
    skipASym:
        mov flagSkipASym,1
        jmp checkFBuffEnd
    checkFnext:       
        cmp al,'$'
        jne checkOperations 
        jmp checkFBuffEnd
    checkOperations:        
        cmp al,'+'
        je oper
        cmp al,'-'
        je oper
        cmp al,'*'
        je setFlagThereWereMD
        cmp al,'/'
        je setFlagThereWereMD

        cmp al,'0'
        jb errCheckFBuff
        cmp al,'9'
        ja errCheckFBuff
        inc operandCounter
        cmp operandCounter, 0006h
        jae numSizeError

        mov flagDupl,0
        cmp flagMinus,1
        jne loopCheckFEnd
        mov flagMinus,0
        cmp al,'0'
        je errCheckFBuff
        jmp loopCheckFEnd
    setFlagThereWereMD:
        mov flagThereWhereMD,1        
    oper:
        mov flagFirstO,1
        mov operandCounter,0
        cmp flagDupl ,1
        jne duplMark
        cmp flagMinus,0
        jne duplicationError
        cmp al,'-'  
        jne duplicationError
        mov flagMinus,1 
    duplMark:
        mov flagDupl,1
    loopCheckFEnd:
        dec cx
        cmp cx, 0000h
        jbe checkFBuffEnd
        jmp loopCheckF
        
    numSizeError:
        push dx
        mov dx, offset msgErrNumSize
        call putStr
        pop dx
        jmp errCheckFBuff
    duplicationError:
        push dx
        mov dx, offset msgErrDupl
        call putStr
        pop dx
    errCheckFBuff:
        mov dx,1
    checkFBuffEnd:
        cmp flagRem,1
        je noNull
        push dx
        push cx
        mov dx, buffPtr
        mov cx, buffSize
            mov si, dx
        loopNullBuff:
            mov [si],byte ptr '$'
            inc si
            loop loopNullBuff
        pop cx
        pop dx
    noNull:
        pop ax
        pop cx
        pop si
        ret
    endp checkFBuff 

    checkFormat proc
        push bx
        push cx
        push ax
    checkReadLoop:
        mov buffSize,maxBuff
        mov buffPtr, offset buff   

        mov bx, srcFile
        mov cx, maxBuff
        mov dx, buffPtr
        call readFile
        cmp dx,1 
        je checkFormatEnd
        mov buffSize,ax
        cmp ax, 0000h
        jne checkFmark
        
    
        cmp flagDupl,1
        jne checkFormatMsg
        mov dx,1
       
        jmp errCheckFormat
    checkFmark:   
        call checkFBuff
        cmp dx,1
        je errCheckFormat
        cmp flagRem,0
        je  checkReadLoop
        push ax
        mov ax, bx
        call changeBuff
        pop ax
        mov flagRem,0
        jmp checkFmark


    errCheckFormat:
        push dx
        mov dx, offset msgErrFileFormat
        call putStr
        pop dx
        jmp checkFormatEnd
    checkFormatMsg:
        push dx
        mov dx, offset msgCheckEnded
        call puts
        pop dx
        mov bx, srcFile            
        xor al ,al 			        
        xor cx, cx
        xor dx, dx			
        call setFilePtr
    checkFormatEnd:        
        pop ax
        pop cx
        pop bx
        ret
    endp checkFormat

    findMDPos proc              ; input: no
        push si                 ; ;;
        push cx                 ; output: bx - pos of * or /, or flagNoMDinBuff is set
        push ax
        mov si, buffPtr
        mov cx, buffSize
        xor ax,ax
    loopFindMd:
        lodsb
        cmp al, 0Dh
        je setFlagNL
        cmp al, '*'
        je setFlagLoad
        cmp al,'/'
        je setFlagLoad
    cmp flagSecond,1
    jne loopFindMdEnd
        cmp al,'+'       
        je setFlagLoad
        cmp al,'-'
        jne loopFindMdEnd 
        cmp flagSkipMin,1
        jb setFlagLoad
        mov flagSkipMin,0
    loopFindMdEnd:
        mov flagSkipMin,0    
        loop loopFindMd
        
        mov flagNoMDInBuff,1        ;;;;;;;;;;;;;;;009
        push dx
        mov dx, offset msgMDNot
        call puts 
        pop dx
        jmp findMDPosEnd
    setFlagNL:
        mov flagNoMDInBuff,1
        mov flagWriteNL, 1
        mov bx, buffSize
        sub bx, cx
        push dx
        mov dx, offset msgNlFound
        call puts 
        pop dx
        cmp cx,0001h
        ja findMDPosEnd

        mov flagSkipASym,1
        jmp findMDPosEnd 
    setFlagLoad:
        sub al,'*'
        mov operation,al
        call loadOver
        cmp dx,2
        jne noFload
        mov dx, 1
        jmp findMDPosEnd
    noFload:
        mov prevOperation,al        
        mov flagNoMDInBuff,0
        mov bx, buffSize
        sub bx, cx
        push dx
        mov dx, offset msgMDFound
        call puts 
        pop dx
    findMDPosEnd:
        pop ax
        pop cx
        pop si
        ret
    endp findMDPos

    killLastNum proc
        push cx
        push di
        mov di, offset lastNumBuff
        mov cx, 0007h
    loopCleanRbuff:
        mov [di],byte ptr 24h
        inc di
        loop loopCleanRbuff
        pop di
        pop cx
        ret 
    endp killLastNum

    rememberLeft proc                   ; output: lastNumBuff- stirng, that contains last num in buffer
        push si                         ; flagRem is set; lastNumBufflen - lenth of lastNumBuff
        push ax
        push di
        call killLastNum 

        mov di, offset lastNumBuff
        add di,0006h
        mov si, buffPtr
        add si, buffSize
        dec si
        
        std 
    loopRemember:
        lodsb 
        cmp al, '+'
        je rememberLeftEnd
        cmp al,0Ah
        je rememberLeftEnd
        cmp al,'-'
        je checkRNeg

        mov [di], al              
        dec di
        jmp loopRemember
    checkRNeg:
        cmp flagSecond,1
        je remNeg       
        lodsb 
        cmp al, '+'
        je remNeg
        cmp al, '-'
        je remNeg
        cmp al, '$'
        je remNeg
        cmp al, 0Ah
        je remNeg
        jmp rememberLeftEnd
    remNeg:
        mov [di], byte ptr'-'
        dec di              
        jmp rememberLeftEnd
    errOverFlowRem:
        push dx
        mov dx, offset msgErrRemOverFlow
        call putStr
        pop dx
        mov dx,1
    rememberLeftEnd:
        cld
        
        sub di, offset lastNumBuff

        push bx
        mov bx, 0006h
        sub bx, di
        mov lastNumBuffLen,bx
        pop bx
        mov flagRem,1

        cmp lastNumBuffLen, 0000h       ;;; may be unsafe
        jne msgRem
        mov flagRem,0

    msgRem:
        push dx
        mov dx, offset msgRemem
        call puts
        pop dx

        pop di
        pop ax
        pop si
        ret
    endp rememberLeft

    writeNumBuff proc
        push bx
        push cx
        mov bx, tempFile
        add dx, 0007h
        sub dx, cx 
        call writeToFile
        cmp dx, 1
        je errWriteRemNum
        jmp writeRemNumEnd
    errWriteRemNum:
        push dx
        mov dx, offset msgErrWriteNumBuff
        call putStr
        pop dx
    writeRemNumEnd:
        push dx
        mov dx, offset msgTempFileW
        call puts 
        pop dx
        pop cx
        pop bx
        ret
    endp writeNumBuff

    firstPass proc 
        jmp fpReadLoop
    jmpErrFirstPass:
        jmp errFirstPass
    fpReadLoop:
        mov buffSize, maxBuff
        mov buffPtr, offset buff
        cmp flagSecond,1
        je fromTemp       
        mov bx, srcFile
        jmp readIt
    fromTemp:
        mov bx, tempFile                
    readIt:        
        mov cx, maxBuff
        mov dx, buffPtr
        call readFile
        cmp dx,1
        je jmpErrFirstPass
        cmp flagSecond,1
        jne setSize
        cmp flagSkipMin,2
        jne setSize
        push si
        mov si, buffPtr
        cmp [si],byte ptr '-'               ;;;;;;;;;
        pop si
        jne setSize
        mov flagSkipMin,1
        
    setSize:
        push dx
        mov dx, buffPtr
        call puts
        pop dx
    
        mov buffSize, ax
        cmp ax, 0000h
        jne notEnd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                      
        cmp flagRem,0
        jne writeLastRem
        jmp lastRes
    writeLastRem:        
        push dx
        push cx
        mov cx, lastNumBuffLen
        mov dx, offset lastNumBuff
        call writeNumBuff                       
        pop dx
        mov dx, offset msgErrNf
        call putStr
        pop dx
    lastRes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                  
        cmp flagLRwasWrited,1
        jne lastOperProcessing 
        jmp firstPassEnd
    lastOperProcessing:    
        call calculate        
        cmp dx,1
        jne continueLast
        jmp errFirstPass 
    continueLast:
        
        call writeResult         
        cmp dx,1
        je jmpErrFirstP
       
        jmp firstPassEnd
    jmpErrFirstP:
        jmp errFirstPass        
    notEnd:
        mov flagLRwasWrited  ,0
        cmp flagSkipASym,1
        jne noSkipA
        inc buffPtr
        dec buffSize
    noSkipA:  
        push ax
        mov ax, maxBuff
        cmp flagSkipASym,1
        jne okok
        dec ax
        mov flagSkipASym,0
    okok:        
        cmp ax, buffSize
        pop ax
        je checkNY
        cmp buffSize,0000h
        jne cleanIt
        jmp firstPassEnd
    cleanIt:        
        push cx
        push si
        mov cx, maxBuff
        sub cx, ax
        add ax, buffPtr
        mov si, ax
    loopCleanBuff:               
        mov [si],byte ptr '$'
        inc si
        loop loopCleanBuff
        pop si
        pop cx
        mov flagLastRes,1
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
    checkNY:
        cmp flagNotYet,1
        jne findMDMark
        jmp markNotYet
    findMDMark: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;           
        call findMDPos
        cmp flagNoMDInBuff,1
        je checkFlagRem 

        mov flagNoMDInBuff,0
        xor cx,cx
        mov cl, flagRem

        jmp markGetLeft
    checkFlagRem:
        cmp flagRem,1
        jne checkNL
        push dx
        push cx
        mov flagTTT,1
        mov cx, lastNumBuffLen          
        mov dx, offset lastNumBuff
        call writeNumBuff                    
        pop cx
        pop dx
        push dx
        mov dx, offset msgWritedoddR
        call putS
        pop dx
        mov flagRem,0
    checkNL:
        cmp flagWriteNL,1
        je nlishere 
        cmp flagLastRes,1
        je writeLastRemainingBuff
        jmp remMark
    writeLastRemainingBuff:
        mov cx, buffSize   
        mov bx, tempFile                
        mov dx, buffPtr
        call writeToFile
        cmp dx,1
        jne oklast
        jmp errFirstPass   
    oklast:                
        jmp firstPassEnd
    nlishere:
    
        mov flagWriteNL, 0
        mov cx, bx    
    
        push cx        
        mov bx, tempFile    
        mov dx, buffPtr
        call writeToFile    
        pop cx
        cmp dx,1
        jne mark1  
        jmp jmpErrMark
    mark1:        
        push cx
        push bx
        push ax
        cmp flagSecond,1
        je markSecCRNL
        mov bx, tempFile
        jmp writeCRNLhere
    markSecCRNL:
        mov bx, destFile        
    writeCRNLhere:        
        mov ax, offset CR
        call writeSym
        mov ax, offset NL
        call writeSym
        pop ax
        pop bx
        mov flagOnce,1
        mov result,0000h
        cmp flagSecond,1
        jne killlastmark
        mov flagSkipMin,1
    killlastmark: 
        mov flagRem,0
        call killLastNum
    changeptr:
        pop cx
        push ax
        mov ax, buffSize
        sub ax, cx
        cmp ax, 0002h
        pop ax        
        jbe jmpFPRL
        add cx,0002h
        mov ax,cx
        add ax, buffPtr
        call changeBuff
        mov flagSkipASym,0
        jmp findMDMark

    remMark:                                    ;;; check \n
        call rememberLeft
        cmp dx, 1
        je jmpErrMark

        mov cx, buffSize      
        sub cx, lastNumBuffLen
        mov bx, tempFile                
        mov dx, buffPtr
        call writeToFile    ; writing without last num
        cmp dx,1
        je jmpErrMark
        push dx
        mov dx, offset msgWOLN
        call puts
        pop dx
    jmpFPRL:
        cmp flagLastRes,1
        jne lastjmp


        mov flagLRwasWrited,1
    lastjmp:       
        jmp fpReadLoop
    jmpErrMark:
        jmp errFirstPass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
    markGetLeft:        
        mov lOperand,0000h
        call getLeft
        cmp dx,1 
        je jmpErrMark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        cmp flagRem,1
        je killRem
        cmp cl, flagRem
        je noRem
        push dx                             
        push cx
        mov cx, lastNumBuffLen        
        mov dx, offset lastNumBuff
        call writeNumBuff                   
        cmp dx,1
        je jmpErrFp
        pop cx
        pop dx

    noRem:

        mov cx,bx
        sub cx, leftSize
        cmp cx, 0000h
        je killRem
        
        push bx
        mov bx, tempFile                
        mov dx, buffPtr
        call writeToFile    ; writing without last num
        pop bx
        cmp dx,1
        je jmpErrMark
    killRem:     
        mov flagRem,0
        inc bx 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        mov rOperand,0000h  
        mov flagZeroOne,0                                                      
        jmp markGetRight
    markNotYet: 
        mov flagNotYet,0
        mov bx, 0000h
    markGetRight:
        call getRight
        cmp dx,1
        je jmpErrFp
        cmp flagNotYet,1
        jne loadPrevMark
        jmp fpReadLoop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmpErrFp:
        jmp errFirstPass          
    loadPrevMark:
    
        push dx
        mov dx, offset msgOperation
        call puts
        pop dx
        cmp flagNeg,0
        je operatt
        push dx
        mov dx, offset msgOperationM
        call putStr
        pop dx
        push dx
        mov dx, offset msgOperationM
        call puts
        pop dx
    operatt:
        call calculate        
        cmp dx,1
        jne loadMark
        jmp errFirstPass
    loadMark:        
        call loadOver
        cmp dx,2
        jne continuepProg
        mov dx,1
        jmp errFirstPass         
    continuepProg:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        cmp flagEndB,1
        je endOfBuffser
        cmp flagEndB,2
        je markEndOfBigNum
        jmp nextOper
    endOfBuffser:
        mov flagEndB,0
        call writeResult
        cmp flagWriteNL,1
        je writeCRNL 
        cmp operationPos, 0FFFFh
        je jmpfpEnd
    markEndOfBigNum:
        mov flagEndB,0
        mov flagNotYet,1
        mov rOperand,0000h 
        mov ax, result
        mov lOperand,ax
        jmp fpReadLoop
    jmpfpEnd:        
        jmp firstPassEnd
    writeCRNL:        
        push bx
        push ax
        cmp flagSecond,1
        je secCR
        mov bx, tempFile                
        jmp wcr
    secCR:        
        mov bx, destFile
    wcr: 
        mov ax, offset CR
        call writeSym
        mov ax, offset NL
        call writeSym
        pop ax
        pop bx
        mov flagOnce,1
        mov result,0000h
        mov flagLRwasWrited,1
        mov flagWriteNL, 0
        cmp flagSecond,1
        jne killlastnummark
        mov flagSkipMin,1
    killlastnummark:        
        call killLastNum
        cmp flagSkipASym, 1
        jne setOperPos
        jmp fpReadLoop
    setOperPos:
        push ax       
        mov ax, operationPos
        call changeBuff
        pop ax
        cmp buffSize, 0000h
        jbe buffended
        jmp findMDMark
    buffended:
           
        cmp flagLastRes,1
        jne jmprl
        mov flagLRwasWrited,1
    jmprl:  
        jmp fpReadLoop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    nextOper:    
        cmp flagSecond,1
        je toNext
        cmp operation,01h   
        je writeResMark
        cmp operation,03h
        je writeResMark
    toNext:
        push ax
        mov ax, operationPos
        call changeBuff
        pop ax
        inc buffPtr
        dec buffSize
        mov rOperand,0000h
        mov ax, result
        mov lOperand,ax
        jmp markNotYet
    writeResMark:            
        mov flagSTS,1
        call writeResult         
        cmp dx,1
        mov flagLRwasWrited,1
        je errFirstPass
        cmp flagLastRes,1
        jne nullDatum 
        mov bx, operationPos
        mov ax, operationPos
        call changeBuff
        jmp findMDMark  
    nullDatum:              
        call nullData   
        cmp dx,1
        je firstPassEnd
    jmpToFMD:        
        jmp findMDMark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OK;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
    errFirstPass:
        mov dx,1
        push dx
        cmp flagSecond,1
        je errsec
        mov dx, offset msgErrFirstPass
        jmp puterr 
    errsec:               
        mov dx, offset msgErrSecondPass
    puterr:        
        call putStr
        pop dx
    firstPassEnd:
        cmp flagSecond,1
        je secMsg
        push dx
        mov dx, offset msgFirstPass
        call putS
        pop dx
        jmp endend
    secMsg:
        push dx
        mov dx, offset msgSecondPass
        call puts
        pop dx
    endend:        
        ret 
    endp firstPass

    changeBuff proc
        push bx
        
        mov bx, buffPtr
        mov buffPtr,ax
        sub ax, bx 
        sub buffSize, ax
       
        pop bx
        ret
    endp changeBuff

    nullData proc
        mov ax, operationPos
        call changeBuff
     
        mov flagNeg,0
        mov rOperand, 0000h
        mov lOperand,0000h
        mov operationPos,000h

        mov bx, tempFile
        call closeFile
        cmp dx,1
        je nullDataEnd
        mov al, 02h			
        mov dx, offset tempFileName
        mov cl, 01h			
        call openFile
        cmp dx,1
        je nullDataEnd
        mov tempFile, ax

        mov bx, tempFile
        mov al ,02h
        xor cx, cx
        xor dx, dx			
        call setFilePtr
    nullDataEnd:
        ret
    endp nullData

    chooseSign proc 
        push dx
        push bx
        mov flagLoadTemp,0  

        cmp prevOperation,01h
        je addition
        cmp flagNeg,1
        jne sPos
        push cx
        xor cx,cx
        mov cl, operation
        mov operation,01h
        call loadOver
        cmp dx,2
        jne conChoose1
        mov dx,1
        jmp chooseSignEnd
    conChoose1:        
        mov operation,cl
        pop cx
        mov flagLoadTemp,1
        cmp flagZeroOne,1
        je sZOn
        jmp setNeg
    sZOn:
        jmp setPositive
    sPos: 
        cmp flagOneZero,1
        je sOnZ
        cmp dx, 1
        je setNeg
        jmp setPositive
    sOnZ:
        cmp dx, 1
        je setPositive
        cmp ax,cx
        je setPositive
        jmp setNeg
        
    addition:
        cmp flagNeg,0
        je aOnZ
        push cx
        xor cx,cx
        mov cl, operation
        mov operation,03h
        call loadOver
        cmp dx,2
        jne conChoose2
        mov dx,1
        jmp chooseSignEnd
    conChoose2:  
        mov operation,cl
        pop cx
        mov flagLoadTemp,1
        cmp flagZeroOne,1
        je aZOn
        cmp dx, 1
        je setPositive
        cmp ax,cx
        je setPositive
        jmp setNeg
    aZOn:
        cmp dx, 0
        je setPositive
        jmp setNeg
    aOnZ:
        cmp flagOneZero,0
        je setPositive
    setNeg:
        mov flagNeg,1
        jmp chooseSignEnd
    setPositive:
        mov flagNeg,0
    chooseSignEnd:
        pop bx
        pop dx
        ret
    endp chooseSign

    calculate proc
        push ax
        push cx
        push bx
        mov ax, lOperand
        mov cx, rOperand
        cmp ax,0000h
        jne leftOk
    leftOk:
        cmp flagSecond,1
        jne nextMark
        cmp rOperand,0000h
        jne nextMark
    nextMark:

        cmp prevOperation,01h
        jne checkSubtraction
        jmp compareAXCX
    checkSubtraction:        
        cmp prevOperation,03h
        jne checkZero
        jmp compareAXCX
    checkZero: 
        cmp prevOperation,00h
        je calculateMark
        xor dx,dx
        cmp rOperand,0
        jne loperZero
        mov dx, offset msgErrZeroDiv
        jmp errCalculate
    loperZero:        
        cmp lOperand,0
        jne calculateMark
        mov dx, offset msgErrZeroDiv
        jmp  errCalculate      
    compareAXCX:        
        cmp ax,cx
        jae axB
        mov dx, 1
        jmp signMark
    axB:
        mov dx, 0
    signMark:
        xor bx,bx
        mov bl, prevOperation
        call chooseSign
    calculateMark:
        call dword ptr overlayOffset
        jnc okres
        mov dx, offset msgErrCalcOverFlow
        jmp errCalculate
    okres:
        cmp prevOperation,05h       
        jne OkResult
        cmp dx, 0000
        je OkResult
        mov dx,offset  msgErrCalcDivis
        jmp errCalculate
    OkResult:
        mov result,ax
        
        cmp flagLoadTemp,1
        jne noAdditionalLoad
        push cx
        xor cx,cx
        mov cl, operation
        mov operation,bl
        call loadOver
        mov operation,cl
        pop cx
    noAdditionalLoad:
        mov flagLoadTemp,0
        jmp calculateEnd
    errCalculate:
        call putStr
        mov dx,1
        jmp calcEndEnd
    calculateEnd:
        xor dx,dx
    calcEndEnd:        
        mov flagOneZero,0
        mov flagZeroOne,0
        pop bx
        pop cx
        pop ax
        ret
    endp calculate

    writeResult proc
        mov si, offset resBuff
        add si, 0006h
        xor dx,dx
        mov ax, result
        cmp ax, 0000h
        jne noZero
        mov flagNeg,0
    noZero:
        mov cx, 000Ah
        cmp ax,cx
        jae getInBuff
        jmp lessThanTen
    getInBuff:
        div cx
        mov [si],dl
        add [si],byte ptr 30h
        dec si
        xor dx,dx
        cmp ax, 000Ah
        jae getInBuff
    lessThanTen:        
        mov [si],al
        add [si],byte ptr 30h
        dec si
        mov result,0000h
        cmp flagNeg,1
        jne startWriteInBuff
    resNeg:                                             ;;;;;;;;;;here falgsOnZ
        mov [si],byte ptr 2Dh              
        dec si
        mov flagNeg,0
    startWriteInBuff:
        push cx
        sub si, offset resBuff
        push bx
        mov bx, 0006h
        sub bx, si
        mov cx,bx
        pop bx
        mov dx, offset resBuff
        call writeNumBuff
        pop cx
    writeResultEnd:
        push dx
        mov dx, offset msgWres
        call puts
        pop dx
        mov flagZeroOne,0
        mov flagOneZero,0
        ret 
    endp writeResult

    
    chooseOL proc
        cmp operation,00
        jne checkAdd
        push ax
        mov ax, offset pathMul
        mov pathPtr,ax
        pop ax
        jmp chooseOLEnd
    checkAdd:
        cmp operation,01
        jne checkSub
        push ax
        mov ax,offset pathAdd
        mov pathPtr,ax
        pop ax
        jmp chooseOLEnd
    checkSub:
        cmp operation,03
        jne checkDiv
        push ax
        mov ax,offset pathSub
        mov pathPtr,ax
        pop ax
        jmp chooseOLEnd
    checkDiv:
        cmp operation,05
        jne errChooseOL
        push ax
        mov ax,offset pathDiv
        mov pathPtr,ax
        pop ax
        jmp chooseOLEnd
    errChooseOL:
        mov dx, 2
    chooseOLEnd:
        ret
    endp chooseOL

    loadOver proc       ; dx -path
        push ax
        push bx
        push dx
        xor ax,ax
        cmp flagSecond,1
        je checkDiff
        cmp operation,01h
        je loadOverEndEnd
        cmp operation,03h
        je loadOverEndEnd
    checkDiff:        
        mov al, prevOperation
        cmp operation,al
        je loadOverEndEnd 
        call chooseOL
        cmp dx,2
        jne loadIt 
        mov dx,1
        jmp errLoadOver
    loadIt:        
        mov ax, SEG BLOCK
        mov es, ax
        mov bx, offset BLOCK    
        mov ax, overlaySegment
        mov [bx], ax
        mov [bx+2], ax 
        mov dx, pathPtr
        mov bx, offset block 
        mov ah, 4Bh
        mov al, 03h
        int 21h
        jnc loadOverEnd
    errLoadOver:
        mov dx, offset msgErrLoad
        call putStr
        pop dx
        pop bx
        pop ax
        mov dx, 2
        ret
    loadOverEnd:
        cmp prevOperation,09h
        je loadOverEndEnd
    choosePath:        
        mov al, operation
        mov prevOperation,al
    loadOverEndEnd:
        pop dx
        pop bx
        pop ax
        ret  
    endp loadOver

    prepSecondPass proc
        mov flagLastRes,0
        mov flagLRwasWrited,0
        mov flagNeg,0
        mov rOperand, 0000h
        mov lOperand,0000h
        mov operationPos,000h
        mov flagNoMDInBuff, 0
        mov flagRem, 0
        mov flagNotYet, 0
        mov flagSkipASym, 0
        mov prevOperation,0009h
        mov flagSkipMin,2
        mov bx, tempFile
        call closeFile
        cmp dx,1
        je prepSecondPassEnd
        cmp flagThereWhereMD,0
        je openSrc
        mov dx, offset tempFileName
        jmp openIt
    openSrc:
        mov dx, offset srcFileName        
    openIt:        
        mov al, 20h			
        mov cl, 01h			
        call openFile
        cmp dx,1 
        je prepSecondPassEnd
        mov tempFile, ax	

        mov bx, tempFile            
        xor al ,al 			        
        xor cx, cx
        xor dx, dx			
        call setFilePtr
        cmp dx,1
        je prepSecondPassEnd
        push dx
        mov dx, offset msgSecPrep
        call puts 
        pop dx  
    prepSecondPassEnd:
        ret
    endp prepSecondPass

    processFile proc  
        mov bx, tempFile
        push bx
        cmp flagThereWhereMD,1
        je firstMark 
        push ax
        mov ax, srcFile
        mov tempFile,ax     
        pop ax
        jmp secondMark
    firstMark:        
        mov flagSecond,0
        call firstPass      ;;;;;;;; call null data
        cmp dx,1
        je errProcessFile
    secondMark:
        call killLastNum
        call prepSecondPass
        mov flagSecond,1
       
        call firstPass
        cmp dx,1
        je errProcessFile
        jmp processFileEnd
    errProcessFile:
        mov dx,1
        push dx
        mov dx, offset msgErrProcessFile
        call putStr
        pop dx
    processFileEnd:
        pop bx
        mov tempFile,bx
        ret 
    endp processFile

    trigNegFlag proc
        cmp flagNeg,1
        je setNeg0
        mov flagNeg,1
        mov flagZeroOne,1
        mov flagOneZero,0
        jmp trigNegFlagEnd
    setNeg0:
        mov flagOneZero,1
        mov flagZeroOne,0
        mov flagNeg,0
    trigNegFlagEnd:    
        ret
    endp trigNegFlag

    getRight proc               
        push si                 
        push cx                 
        push ax                 
        xor ax,ax
        mov si, buffPtr
        add si, bx
        cmp [si],byte ptr '$'
        jne getRightLoop 
        jmp setFlagNotYet
    getRightLoop:
        lodsb 
        cmp al, '-'
        je trigRneg
        cmp al, '*'
        je setOperation
        cmp al,'+'
        je setOperation
        cmp al,'/'
        je setOperation
        cmp al,'$'
        je setFlagNotYet
        cmp al,0Dh
        jne rightProccessing
    DisHere:
        mov flagWriteNL,1
        mov flagEndB,1
        lodsb
        cmp al, '$'
        jne setOp
        mov flagSkipASym,1
    setOp:   
        mov operationPos, si       
        jmp getRightEnd
    rightProccessing:        
        push ax
        mov ax,000Ah
        mul rOperand
        jc errGetRightOverFlow
        mov rOperand,ax
        pop ax
        sub al,'0'
        add rOperand, ax
        jc errGetRightOverFlow
        cmp rOperand, 2710h
        jb getRightLoop
        lodsb 
        cmp al,0Dh
        je DisHere     
        cmp al, '$'
        jne setOperation
        mov flagEndB,2
        mov operationPos,si
        jmp getRightEnd
    trigRneg:        
        cmp rOperand, 0000h
        jne setOperation
        call trigNegFlag
        jmp getRightLoop
    setOperation:        
        sub al,'*'
        mov operation,al        ; 0  * ; 1  + ; 3 - ; 5 / ;
        mov operationPos, si
        dec operationPos
        jmp getRightEnd
    setFlagNotYet:
        cmp flagLastRes,1
        jne setny 
        mov flagEndB,1
        mov operationPos,0FFFFh
        jmp getRightEnd
    setny:    
        mov flagNotYet,1
        push dx
        mov dx, offset msgReached
        call puts
        pop dx
        jmp getRightEnd        
    errGetRightOverFlow:
        push dx
        mov dx, offset msgErrROverFlow
        call putStr
        pop dx
        mov dx,1
    getRightEnd:
        push dx
        mov dx, offset msgGetRight
        call puts
        pop dx
        pop ax
        pop cx
        pop si
        ret
    endp getRight

    getLeft proc
    jmp getLeftbeg
    jmpgetLeftEnd:
    jmp getLeftEnd
    getLeftbeg:
        push si
        push cx
        push ax
        push di
        mov cx, 0001h
        std
        mov si, buffPtr
        add si, bx
        dec si
        xor di,di
    loopGetLeft:
        xor ax,ax
        lodsb 
        cmp flagRem,1
        je checkRem
        cmp al, 0Ah
        je jmpgetLeftEnd
        cmp al, '+'
        je jmpgetLeftEnd 
        cmp al, '$'   
        je jmpgetLeftEnd
        cmp al, '-'
        jne getLeftMark
        cmp flagSecond,1
        je trigNegMark2
        lodsb 
        cmp al, '-'
        je trigNegMark2
        cmp al,'+'
        je trigNegMark2
        cmp al, 0Ah
        je trigNegMark2
        cmp al ,'$'
        jne jmpGLend 
        cmp flagOnce,1
        jne jmpGLend
        mov flagOnce,0
        jmp trigNegMark2
    jmpGLend:        
        jmp jmpgetLeftEnd
    trigNegMark2:
        inc di      ;;;;;;;;;
        call trigNegFlag
        jmp jmpgetLeftEnd
    checkRem:        
        cmp al, '$'
        je jmpgetLeftEnd
        cmp al, '+'
        je manageRem    ;;;
        cmp al, '-'
        je manageRemNeg
    getLeftMark:  
        inc di      
        sub al, '0'
        mul cx
        add lOperand, ax
        jc errGetLeft
        push ax
        mov ax,000Ah
        mul cx
        mov cx,ax
        pop ax
        xor dx,dx
        jmp loopGetLeft

    manageRemNeg:
        lodsb 
        cmp al,'+'
        je trigNegLeft
        cmp al, '-'
        je trigNegLeft
        cmp al, '$' 
        jne manageRem
        call trigNegFlag
        jmp getLeftEnd
    errGetLeft:   
        push dx
        mov dx, offset msgErrLOverFlow
        call putStr
        pop dx
        mov dx,1
        jmp getLeftEnd
    trigNegLeft:
        inc di        
        call trigNegFlag
    manageRem:
        mov flagRem,0
    getLeftEnd:
        mov flagOnce,0
        mov leftSize,di
        cld
        push dx
        mov dx, offset msgGetLeft
        call puts
        pop dx
        pop di
        pop ax
        pop cx
        pop si
        ret
    endp getLeft
ZSEG SEGMENT     
ZSEG ENDS      
end main
