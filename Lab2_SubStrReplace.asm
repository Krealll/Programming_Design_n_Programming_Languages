.model small
.stack 100h                                                     ; Programm aimed to find substring in Original stirng and change 
.data                                                           ; word with desired substring to new word

msg1 db "Input string:", 0dh, 0ah, '$'                          ;Messages for output
msg2 db 0dh, 0ah, "Enter the symbol set:", 0dh, 0ah, '$'
msg3 db 0dh, 0ah, "Result:",0dh,0ah,'$'
msg4 db 0dh, 0ah, "Enter replacing word:",0dh,0ah,'$'
emsg5 db 0dh, 0ah, "Error",0dh,0ah,'$'
emsg6 db 0dh, 0ah, "Error:There is space in set of symbols",0dh,0ah,'$'

string db 202 dup("$")                                          ; original string
symset db 202 dup("$")                                          ; desired substing
repword db 202 dup("$")                                         ; new word

capacity EQU 200

flagSpstr db 0
flagEnd db 0
flagIn db 0


.code
start proc
    mov ax, @data                   ;move data segment address in DS and ES
    mov ds, ax
    mov es, ax
    
    
    mov ah, capacity                ;set max string size in first byte
    mov string[0], ah
    mov symset[0], ah
    mov repword[0], ah
    
    lea dx, msg1                    ; str I/O
    call puts
    lea dx, string
    call gets
    
    lea dx, msg2
    call puts
    lea dx, symset
    call gets
    
    lea dx, msg4
    call puts
    lea dx, repword
    call gets
    

    call checkSet                   ; calling procedure that will check 
                                    ; substring for emptiness and spaces
    cld 
    lea si, string[2]
    lea di, symset[2]
    xor cx,cx   
    
    call ReplaceWord                ; calling main procedure
    
    ret
endp start

checkSet proc                       ;procedure that will check 
                                    ; substring for emptiness and spaces
    push di
    push cx
    
    lea cx,symset[2] 
    add cl,symset[1]
    mov di,cx                       ; set di at the begining of the substring
    dec di                          ;moving backwards
    
    xor cx,cx                       
    mov cl,symset[1]
    cmp cx,0                        ; check for emptiness
    jne lloop
    pop cx
    pop di
    jmp error1
lloop:    
    cmp byte ptr [di],' '           ; check for spaces
    jne decrease
    pop cx
    pop di
    jmp error2
decrease:
    dec di
loop lloop
    
    pop cx
    pop di             
    ret            
endp checkSet
 
 
endProg proc                        ; procedure that will finish the
                                    ; programm properly
    jmp fin
error1:
    
    lea dx, emsg5
    call puts
    jmp fin
error2:    
    lea dx,emsg6
    call puts                           
fin:    
    mov ah, 4ch
    int 21h       
       
    ret
endp endProg    

showResult proc                     ; proc that shows string buffer
    push ax
    push dx
    lea dx, msg3
    call puts
    lea dx,string[2]
    call puts
    pop dx
    pop ax    
    ret
endp showResult   

gets proc                           ; intput proc
    mov ah, 0Ah
    int 21h
    ret
endp gets

puts proc                           ; output proc
    mov ah, 9
    int 21h
    ret
endp puts

ReplaceWord proc                    ; main procedure
cycle_:    
    push si
    push di
    push ax
    xor ax,ax
    
    lodsb                           ; load symbol until we find space
    cmp al,' '                      ; if the first symbol isn't space - find end of the word
    jne fndEnd
    call toBegin                    ; proc that skips spaces
fndEnd:     
    dec si                          ;
    mov bx,si                       ; memorize beg of str
    call findEnd                    ;proc that will find the end of the word and set dx at symbol 
                                    ; after it
    xor ax,ax
    mov ax,dx                       ; memorize numb of symbols to delete
    sub ax,bx
    
    call isDelim                    ; searching for substring
    
    cmp flagIn,1                    ; if substring is there
    je proccess                     ; start processing
    push di     
    mov di,dx
    cmp byte ptr[di],24h            ; check for end of string
    pop di
    je exit
                            
    jmp cycle_                      ; if there weren't substring - start over
    
proccess:
   
    mov flagIn,0                    
   ; call showResult    
    call delWord                    
   ; call showResult
    call InsWord
   ; call showResult
    call changeLen                  ; changing len for proper output
    
    add bl,repWord[1]               
    mov si, bx
    mov di,dx
    
    cmp byte ptr[di],24h            ; check for end of string
    je exit
        
    jmp cycle_ 
        
    pop ax
    pop di
    pop si
    jmp exit
preExit:
    mov flagEnd,1
exit:

    call showResult
    
    call endProg
            
    ret
endp ReplaceWord

changeLen proc                       ; changing len for proper output
    push ax
    
    mov cx,ax
    cmp al,repWord[1]               
    ja decLen
    jb incLen  
    sub cl,repWord[1]
    jmp m
decLen:
    sub al,repWord[1]
    mov cx,ax
decloop:
    dec byte ptr string[1]
loop decloop   
    jmp m
    
incLen:    
    mov cl,repWord[1]
    sub cl,al
incloop:
    inc byte ptr string[1]    
loop incloop 
         
m:           
    pop ax       
    ret
endp changeLen    

isDelim proc                        ; searching for substring
    push cx
    push si        
    push di
    push ax
    
    mov si,bx                       ; setting si at the beg of the word
     
    
    push bx
    lea cx,symset[2] ;              
    mov di,cx                       ;set  di at the beg of the substring
    mov bx,di                       
    
    xor cx,cx
    mov cx,ax                       ; number of symbols to compare
    
    lodsb 
loop_:
    cmp al, byte ptr [di]           ; check for coincidence
    je incrr                    
    
    lodsb                           ; else load new symbol
    mov di,bx                       ; refresh subset "pointer"
loop loop_    
    jmp mark

incrr:                              ; if symbols are equal
    push ax
    lea ax,symset[2]
    add al, symset[1]
    dec ax
    cmp ax,di                       ; check for the end of the substring
    pop ax
    je setFlag                      ; end - so we set flag  
    
    inc di                                                    
    lodsb
   
loop loop_            
    jmp mark
setFlag:
    mov flagIn,1
mark:     

    pop bx    
    pop ax
    pop di
    pop si
    pop cx            
    ret
               
endp isDelim
             
             
             
delWord proc                    ; deleting symbols
    push si
    push di
    push cx       
    xor cx,cx
    
    mov cl,string[1]        
    mov di,bx
        
    repe movsb   
       
    pop cx
    pop di
    pop si
       
    ret                
endp delWord

InsWord proc                    ; inserting symbols
    push si
    push di
    push cx
    
    lea cx, string[2]
    add cl,string[1]
    sub cl, al
    mov si,cx
    dec si                      ; set SI on last symbol of original string
    
    push bx 
    
    mov bx,si
    add bl,repword[1]
    mov di,bx                   ; set DI on the last symbol of new string
    pop bx
    call setNum                 ; numb of symbols to copy
    std
    repe movsb                  ; "clearing space" to repWord
    

    
    lea si, repword[2]
    mov di,bx               
    xor cx,cx
    mov cl,repword[1]
    cld    
    repe movsb                   ; replacement 
        
    pop cx
    pop di
    pop si
            
    ret            
endp InsWord     

setNum proc                     ; proc that set proper number to copy
    push ax
               
    mov ax, si
    sub ax,bx
    cmp ax,0    
    js __end
    je __end
    mov cx,ax
    add cl,repWord[1]
    jmp ___end:
__end:
    mov cx,0
___end:                 
    pop ax
    ret       
endp setNum    

toBegin proc                    ; proc skipping spaces                
_loop:
    cmp byte ptr [si],24h       ; check for end of string
    je exit 
                                                           
    lodsb
    cmp al,' '
    je _loop  
    ret
endp    

findEnd proc                    ; proc for finding end of the word
_loop_:                         ; and setting dx at the symbol after it
    lodsb
    
    cmp byte ptr [si],24h       ; check for end of string    
    je end__
    
    cmp al,' '
    jne _loop_       
            
end_:    
    dec si
end__:        
    mov dx,si
    ret
endp
