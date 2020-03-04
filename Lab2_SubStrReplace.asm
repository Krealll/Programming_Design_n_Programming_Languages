.model small
.stack 100h
.data

msg1 db "Input string:", 0dh, 0ah, '$'
msg2 db 0dh, 0ah, "Enter the symbol set:", 0dh, 0ah, '$'
msg3 db 0dh, 0ah, "Result:",0dh,0ah,'$'
msg4 db 0dh, 0ah, "Enter replacing word:",0dh,0ah,'$'
emsg5 db 0dh, 0ah, "Error",0dh,0ah,'$'
emsg6 db 0dh, 0ah, "Error:There is space in set of symbols",0dh,0ah,'$'

string db 202 dup("$")   ; original string
symset db 202 dup("$")
repword db 202 dup("$")

capacity EQU 200

flagSpstr db 0
flagEnd db 0
flagIn db 0


.code
start proc
    mov ax, @data        ;move data segment address in DS and ES
    mov ds, ax
    mov es, ax
    
    
    mov ah, capacity     ;set max string size in first byte
    mov string[0], ah
    mov symset[0], ah
    mov repword[0], ah
    
    lea dx, msg1         ; str I/O
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
    

    call checkSet
   
    cld 
    lea si, string[2]
    lea di, symset[2]
    xor cx,cx   
    
    call ReplaceWord 
    
    ret
endp start

;====================================================
;====================================================
;==================================================== 

checkSet proc
    push di
    push cx
    
    lea cx,symset[2] ;
    add cl,symset[1]
    mov di,cx     
    dec di              ;moving backwards
    
    xor cx,cx
    mov cl,symset[1]
    cmp cx,0
    jne lloop
    pop cx
    pop di
    jmp error1
lloop:    
    cmp byte ptr [di],' '
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
 
 
endProg proc
   
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

showResult proc
    push ax
    push dx
    lea dx, msg3
    call puts
    lea dx,string[2]
    call puts
    pop dx
    pop ax    
    ret
endp    

gets proc
    mov ah, 0Ah
    int 21h
    ret
endp gets

puts proc
    mov ah, 9
    int 21h
    ret
endp puts
;====================================================
;====================================================
;====================================================

ReplaceWord proc
cycle_:    
    push si
    push di
    push ax
    xor ax,ax
    
    lodsb    
    cmp al,' '
    jne fndEnd
    call toBegin
fndEnd:    
    dec si
    mov bx,si       ; mem beg of str
    call findEnd     
    
    xor ax,ax
    mov ax,dx   ; mem numb of sym to del
    sub ax,bx
    
    call isDelim   ; searching for delimeted symbols
    
    cmp flagIn,1 
    je proccess
    push di     
    mov di,dx
    cmp byte ptr[di],24h
    pop di
    je exit
                            
    jmp cycle_
proccess:
   
    mov flagIn,0
    call showResult    
    call delWord 
    call showResult
    call InsWord
    call showResult
    call changeLen
    
    add bl,repWord[1]
    mov si, bx
    mov di,dx
    
    cmp byte ptr[di],24h
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

changeLen proc
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

isDelim proc
    push cx
    push si        
    push di
    push ax
    
    mov si,bx 
     
    
    push bx
    lea cx,symset[2] ;
    mov di,cx                   
    mov bx,di
    
    xor cx,cx
    mov cx,ax
    
    lodsb 
loop_:
    cmp al, byte ptr [di]
    je incrr
    
    lodsb
    mov di,bx
loop loop_    
    jmp mark

incrr:
    push ax
    lea ax,symset[2]
    add al, symset[1]
    dec ax
    cmp ax,di
    pop ax
    je setFlag  
    
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
             
             
             
delWord proc
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

InsWord proc
    push si
    push di
    push cx
    
    lea cx, string[2]
    add cl,string[1]
    sub cl, al
    mov si,cx
    dec si                ; set SI on last sym of orig str
    
    push bx 
    
    mov bx,si
    add bl,repword[1]
    mov di,bx           ; set DI on the last symbol of new string
    pop bx
    call setNum       ; numb of sym to copy
    std
    repe movsb      ; "clearing space" to repWord
    

    
    lea si, repword[2]
    mov di,bx
    xor cx,cx
    mov cl,repword[1]
    cld    
    repe movsb
        
    pop cx
    pop di
    pop si
            
    ret            
endp InsWord     

setNum proc
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

toBegin proc
  
_loop:
    cmp byte ptr [si],24h
    je exit 
                                                           
    lodsb
    cmp al,' '
    je _loop
    
     
    
    ret
endp    

findEnd proc

_loop_:                                                        
    lodsb
    
    cmp byte ptr [si],24h
    je end__
    
    cmp al,' '
    jne _loop_       
            
end_:    
    dec si
end__:        
    mov dx,si
    ret
endp
