
strTrim proc           ; string trim procedure
    push ax
    xor ax,ax
    
    lea si,string[2]  ;get string adress     
    mov di,si
lp: 
    lodsb             ; load symbol to al
    cmp al,' '
    jnz loadSym 
    
    cmp flag,0        ; if symbol is the fisrt space
    je  setFl         ; 
    jmp lp            ;
                      ;
setFl:                ;
    mov flag,1        ;then set flag and put that simbol to DI
    stosb             ;
    cmp al,'$' 
    jnz lp    
    
loadSym:              ; if symbol isn't a space
    mov flag,0        ; put it to DI
    stosb             ; 
    cmp al,'$'        ;if the end is reached
    jnz lp            ;
    
    pop ax
    ret                   
endp strTrim  
