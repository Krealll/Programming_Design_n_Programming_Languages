.model small
.stack 256
.data

Rows equ 5
Columns equ 1

MAX_VALUE equ 32767
matrixSize equ Rows*Columns

two dw 2
step dw 0
m dw Rows
n dw Columns

Matrix dw matrixSize dup(?)
mulD dd ?

newLine db 0dh, 0ah, '$'
startMsg db 'Input matrix:', '$'
currMsg db 'Current matrix:', '$'
resMsg db 'Result value:', '$'
elemBeg db 'matrix[', '$'
comma db ',', '$'
elemEnd db ']= ', '$'
errMsg db 'Error',0dh,0ah,'$'

buffLen equ 7
inputBuff db buffLen, 0, buffLen dup(0)

flagNeg dw 0

.386
.code

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

aToi proc
push ax
push bx
push cx
push dx
push si

mov cl, [si] ; mem size
xor ch, ch

inc si

mov bx, 10
xor ax, ax

cmp byte ptr [si], '-'
jne goop
inc si
dec cx
goop:
mul bx ; OF and CF are 0 when older half had only 0( Ah,dx,edx)
mov [di], ax
cmp dx, 0 ; check CF or OF ?
jnz Error

mov al, [si]
cmp al, '0'
jb Error

cmp al, '9'
ja Error

sub al, '0'
xor ah, ah
add ax, [di]
jc Error ; check if > 65535 ; optional

cmp ax, 8000h ; check if > 32678
ja Error

inc si

loop goop

pop si
push si
inc si
cmp byte ptr [si], '-'
jne CheckOverflow
neg ax
jmp SaveResult
CheckOverflow:
or ax, ax
js Error
SaveResult:
mov [di], ax
clc
pop si
pop dx
pop cx
pop bx
pop ax
ret
Error:
xor ax, ax
mov [di], ax
stc ; setting Carry Flag (CF) to 1
Errorr:
mov dx, OFFSET newLine
call puts
mov dx, OFFSET errMsg
call puts

pop si
pop dx
pop cx
pop bx
pop ax
ret
aToi endp

printAx proc
pusha

mov cx, 10
xor di, di

or ax, ax
jns Convert

push ax
mov dx, '-'
mov ah, 02h
int 21h
pop ax

neg ax
Convert:
xor dx, dx
div cx
add dl, '0'
inc di
push dx
or ax, ax
jnz Convert

Print:
pop dx
mov ah, 02h
int 21h
dec di
jnz Print

popa
ret
printAx endp

PrintMatrix proc
pusha

mov si, 0
mov di, 0
mov bx, dx

PrintRow:
mov ax, [bx]
call printAx

mov ah, 02h
mov dl, ' '
int 21h

add bx, 02h

inc di

cmp di, n
jb PrintRow

mov dx, OFFSET newLine
call puts

mov di, 0

inc si

cmp si, cx
jb PrintRow

popa
ret
PrintMatrix endp

InputMatrix proc
pusha

mov bx, dx

mov dx, OFFSET startMsg
call puts

mov dx, OFFSET newLine
call puts

xor si, si
xor di, di
InputInterrupt:
lea dx, elemBeg
call puts

mov ax, si
call printAx

lea dx, comma
call puts

mov ax, di
call printAx

lea dx, elemEnd
call puts

mov dx, OFFSET inputBuff
call gets

push di
push si

mov si, OFFSET inputBuff+1
mov di, bx
call aToi

pop si
pop di
jc InputInterrupt

UpperBorder:
cmp word ptr [bx], MAX_VALUE ;; cannot compare >32767
jle LowerBorder

mov dx, OFFSET newLine
call puts
mov dx, OFFSET errMsg
call puts
jmp InputInterrupt
LowerBorder:
cmp word ptr [bx], -MAX_VALUE
jge NumberSuits

mov dx, OFFSET newLine
call puts
mov dx, OFFSET errMsg
call puts
jmp InputInterrupt
NumberSuits:
mov dx, OFFSET newLine
mov ah, 09h
int 21h

add bx, 02h
inc di
cmp di, n
jnge InputInterrupt

mov di, 0
inc si
cmp si, m
jnge InputInterrupt

popa
ret
InputMatrix endp

showResult proc
pusha

mov ecx,0ah
xor di,di
; here is sign processing
metochka:
xor dx,dx
div ecx
add dl,'0'
inc di
push dx
or eax,eax
jnz metochka

Cmp flagNeg,1
Jne pri
Mov dx,2dh
Push dx
inc di
pri:
pop dx
mov ah, 02h
int 21h
dec di
jnz Pri

popa
ret
endp showResult

;description
proc checkFL
        push cx

Cmp flagNeg,1
Je setfn
Mov cx,1h
Jmp nexttt
Setfn:
Mov cx,0h
Nexttt:
Cmp ax,8000h
Jb endd
Neg ax
Mov flagNeg,cx
endd:


 pop cx
ret
endp checkFL

proccc proc
pusha

mov cx, Columns
Cycle_:
push cx

mov cx, Rows
Cycle:
xor eax,eax
mov ax, word ptr [bx]
call checkFL

push ecx
mov ecx, eax
mov eax, dword ptr [si]
mul ecx
pop ecx

cmp eax,9017E368h
jae errOver

neeext: ;call saveResult
mov word ptr [si], ax
shr eax,16
mov word ptr [si]+2,ax

add bx, step
loop Cycle

mov ax, word ptr mulD+2
shl eax,16
mov ax, word ptr muld

call showResult

mov dx, offset newLine
call puts
mov ax,1
mov word ptr [si], ax ;
shr eax,16
mov ax,0
mov word ptr [si]+2,ax ; here

mov flagNeg,0
mov bx, OFFSET Matrix
add bx, two
inc two
inc two
pop cx
loop Cycle_

popa
ret

errOver:
pop cx
popa
mov dx,OFFSET errMsg
call puts
mov ax,2ah
ret
endp proccc

Main proc
mov dx, @data
mov ds, dx

mov ax,1
mov word ptr [mulD],ax

mov si,OFFSET mulD
mov eax, dword ptr [si]
mov ax, Columns
mov cx, 2
mul cx
mov step, ax

mov dx, OFFSET Matrix
call InputMatrix

mov dx,OFFSET currMsg
call puts

mov dx, OFFSET newLine
call puts

mov cx, Rows
mov dx, OFFSET Matrix
call PrintMatrix

mov bx, OFFSET Matrix
mov si,OFFSET mulD

call proccc
cmp ax,2ah
je endMark

endMark:
mov ax, 4c00h
int 21h
Main endp
END Main
