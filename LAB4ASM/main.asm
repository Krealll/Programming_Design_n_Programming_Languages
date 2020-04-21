.model small
.stack 100h
.data
videoStart 			dw 0B800h 
dataStart 			dw 0000h 

KPlus				equ 0Dh		; + key
KMinus				equ 0Ch		; - key
KZero				equ 0Bh		; 0 key
KNine				equ 0Ah		; 9 key
KRes				equ 13h		; R key
KMoveUp 			equ 11h 	; W key
KMoveDown 			equ 1Fh 	; S key
KMoveLeft 			equ 1Eh 	; A key
KMoveRight 			equ 20h 	; D key
KExit	 			equ 01h 	; ESC key

xSize 				equ 80 		; window x size		
elementsNum			equ 2000	; total amount of symbols

generalWaitTime 	dw 0001h	; wait time
enemyDelayCounter	db 00h		
heroDelayCounter	db 00h

flagPushOff 		db 00h
flagEmptyCell 		db 00h
flagMoveEnemy 		db 00h
flagVirus 			db 00h
flagOnField 		db 00h
flagDoNothing 		db 00h
flagDrawLastLand 	db 00h
flagStartFillMap 	db 00h
Percent				db 00h
WINCounter			dw 0000h

vBorderSym 			equ 0F7Ch
hBorderSym 			equ 0F16h
spaceSym 			equ 0020h 
fillSym 			equ 0820h 
landSym 			equ 0F53h 
xonSym 				equ 5020h ; xonix game name block

virusPos 			dw 2614h ; 38x20y in base 10
virusPosOld 		dw 2614h ;
virusDir 			dw 0101h 	; Direction explanation for enemies: 0101 - left/up movement, 0001,right/up movement, 0000-right/down movement
virusSym	 		equ 8C02h ; Enemy virus block

heroLife 			db 03h
heroDir 			dw 0000h	; Direction explanation for hero: 0000 - no movement, 0001 - left, 0010 - right,0100- up,1000 -  down
heroPos 			dw 2601h ; 38 0
heroPosOld 			dw 2601h ; 38 0
heroDelay			db 00h
heroSym 			equ 0902h ; main character block
trackSym 			equ 092Ah

enemyOne 			dw 2010h
enemyOnePos 		dw 1209h ;18 9
enemyOnePosOld 		dw 1209h ;18 9
enemyOneDir 		dw 0101h ; 01 - left, 01- up

enemyTwo 			dw 2907h
enemyTwoPos 		dw 2710h ;27 16
enemyTwoPosOld 		dw 2710h ;27 16
enemyTwoDir 		dw 0000h ; 00 - right; 00 - down

enemyDelay			db 00h
enemySym 			equ 0A01h ; sea enemy block

fillArray 			dw 5472 dup (?)	; array to contain enemy positions(priority queue)
ptrcur				dw 0000h		; priority queue current pos
ptrend				dw 0001h		; priority queue end pos

screen 				dw xSize dup(hBorderSym)
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, landSym,landSym, xSize-6 dup(spaceSym),landSym,landSym, vBorderSym
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw vBorderSym, xSize-2 dup(landSym), vBorderSym
					dw xSize dup(hBorderSym)
					dw fillSym, xonSym,fillSym, xonSym,fillSym,fillSym, xonSym,fillSym,fillSym, xonSym,fillSym,fillSym, xonSym,fillSym, xonSym,fillSym, xonSym,fillSym, xonSym,0F7Ch, 0F41h,0F72h,0F65h,0F61h, 0F20h,0F25h, 0F3Ah,0F20h
area 				dw 0430h,0430h,0F25h,fillSym,0F43h,0F6Fh,0F6Eh,0F74h,0F72h,0F6Fh,0F6Ch,0F73h,0F3Ah,fillSym,0F20h,0F4Dh,0F6Fh,0F76h,0F65h,0F2Dh,0F77h,0F2Ch,0F73h,0F2Ch,0F61h,0F2Ch,0F64h,0F3Bh, xSize-56 dup(fillSym)
					dw fillSym,fillSym, xonSym, fillSym,fillSym, xonSym,fillSym, xonSym,fillSym, xonSym,xonSym,fillSym, xonSym,fillSym, xonSym,fillSym,fillSym, xonSym,fillSym,0F7Ch,23 dup (fillSym),0F2Dh,0F2Fh,0F2Bh,0F20h,0F64h,0F65h,0F63h,0F2Fh,0F69h,0F6Eh,0F63h,0F20h,0F67h,0F65h,0F6Eh,0F65h,0F72h,0F61h,0F6Ch,0F20h,0F73h,0F70h,0F65h,0F65h,0F64h, xSize-68 dup(fillSym)
					dw fillSym, xonSym,fillSym, xonSym,fillSym,fillSym, xonSym,fillSym,fillSym,xonSym,fillSym, xonSym,xonSym,fillSym, xonSym,fillSym, xonSym,fillSym, xonSym, 0F7Ch,0F4Ch,0F69h,0F66h,0F65h,0F3Ah,0F20h
life				dw 0933h, 16 dup (fillSym),0F39h,0F2Fh,0F30h,0F20h,0F64h,0F65h,0F63h,0F2Fh,0F69h,0F6Eh,0F63h,0F20h,0F65h,0F6Eh,0F65h,0F6Dh,0F79h,0F20h,0F73h,0F70h,0F65h,0F65h,0F64h, xSize-66 dup(fillSym)

GObannerWidth 		equ 40
GObannerLines		equ 10	 	 
red 				equ 4020h 			  
blackVSym 			equ 00FBAh
blackHSym 			equ 00FCDh
GObannerPos			equ 0488h

GameOverBanner 		dw 00FC9h, GObannerWidth-2 dup(blackHSym), 0FBBh
					dw blackVSym, 38 dup(spaceSym), blackVSym
					dw blackVSym, 2 dup(spaceSym),red,red,4 dup(spaceSym),red,2 dup(spaceSym),red, 3 dup(spaceSym), red, spaceSym, 3 dup(red), spaceSym,spaceSym, red,red,spaceSym,spaceSym, red,spaceSym,red,spaceSym,3 dup(red),spaceSym, 3 dup(red),spaceSym, blackVSym
					dw blackVSym, spaceSym,red,5 dup(spaceSym), red,spaceSym, red, spaceSym, red,red,spaceSym,red,red, spaceSym,red,3 dup(spaceSym), red,spaceSym,spaceSym,red,spaceSym,red,spaceSym,red,spaceSym,red,3 dup(spaceSym),red, spaceSym,red, spaceSym , blackVSym
					dw blackVSym, spaceSym,red,spaceSym,spaceSym,red,red,spaceSym,red,spaceSym,red,spaceSym,red,spaceSym,red,spaceSym,red,spaceSym,3 dup (red),spaceSym,red,spaceSym,spaceSym,red,spaceSym,red,spaceSym,red,spaceSym,3 dup(red),spaceSym,3 dup(red),spaceSym, blackVSym
					dw blackVSym, spaceSym,red,3 dup(spaceSym),red,spaceSym, 3 dup(red),spaceSym,red,3 dup(spaceSym),red,spaceSym,red,3 dup(spaceSym),red,spaceSym,spaceSym,red,spaceSym,red,spaceSym,red,spaceSym,red,3 dup(spaceSym),red,red,spaceSym,spaceSym,blackVSym
					dw blackVSym, spaceSym,spaceSym,3 dup (red), spaceSym,spaceSym,red,spaceSym,red,spaceSym,red,3 dup(spaceSym),red,spaceSym,3 dup(red),spaceSym,spaceSym,red,red,3 dup(spaceSym),red,spaceSym,spaceSym,3 dup(red),spaceSym,red,spaceSym,red,spaceSym, blackVSym
					dw blackVSym, 38 dup(spaceSym), blackVSym
					dw blackVSym, spaceSym ,08F50h, 08F72h, 08F65h, 08F73h, 08F73h, 08F00h, 08F61h, 08F6Eh, 08F79h, 08F00h, 08F6Bh, 08F65h, 08F79h, 08F00h, 08F74h, 08F6Fh, 08F00h, 08F65h, 08F78h, 08F69h, 08F74h, 08F2Eh, 08F52h, 08F20h, 08F74h, 08F6Fh, 08F20h, 08F72h, 08F65h, 08F73h, 08F74h, 08F61h, 08F72h, 08F74h, 08F20h,2 dup(spaceSym), blackVSym
					dw 0FC8h, GObannerWidth-2 dup(blackHSym), 0FBCh

WINbannerWidth 		equ 40
WINbannerLines		equ 10	 	 
Orange 				equ 6020h 			  
WINbannerPos		equ 0488h

WINBanner 			dw 00FC9h, WINbannerWidth-2 dup(blackHSym), 0FBBh
					dw blackVSym, 38 dup(spaceSym), blackVSym
					dw blackVSym, spaceSym,Orange,3 dup(spaceSym),Orange,3 dup (spaceSym),Orange,3 dup(spaceSym),Orange,3 dup(spaceSym),Orange,spaceSym,spaceSym,Orange,5 dup(spaceSym),Orange,spaceSym,Orange,Orange,Orange,spaceSym,Orange,3 dup(spaceSym),Orange,spaceSym, blackVSym
					dw blackVSym, spaceSym, spaceSym,Orange,spaceSym,Orange,3 dup(spaceSym),Orange,spaceSym,Orange,spaceSym,spaceSym,Orange, 3 dup(spaceSym),Orange,spaceSym,spaceSym,Orange,5 dup(spaceSym),Orange,spaceSym, spaceSym,Orange,spaceSym,spaceSym,Orange,Orange,spaceSym,spaceSym,Orange,spaceSym, blackVSym
					dw blackVSym, 3 dup(spaceSym),Orange,3 dup(spaceSym),Orange,3 dup (spaceSym),Orange,spaceSym,Orange,3 dup (spaceSym),Orange,spaceSym,spaceSym,Orange,spaceSym,spaceSym,Orange,spaceSym,spaceSym,Orange,spaceSym,spaceSym,Orange,spaceSym,spaceSym,Orange,spaceSym,Orange,spaceSym,Orange,spaceSym, blackVSym
					dw blackVSym, 3 dup(spaceSym),Orange,4 dup(spaceSym),Orange,spaceSym,Orange,2 dup(spaceSym),Orange,3 dup(spaceSym),Orange,3 dup(spaceSym),Orange,spaceSym,Orange,spaceSym,Orange,3 dup(spaceSym),Orange,spaceSym,spaceSym,Orange,spaceSym,spaceSym,Orange,Orange,spaceSym, blackVSym
					dw blackVSym, 3 dup(spaceSym),Orange,5 dup(spaceSym),Orange,4 dup(spaceSym),Orange,Orange,Orange,5 dup(spaceSym),Orange,spaceSym,Orange,3 dup(spaceSym), Orange,Orange,Orange,spaceSym,Orange,3 dup(spaceSym),Orange,spaceSym, blackVSym
					dw blackVSym, 38 dup(spaceSym), blackVSym
					dw blackVSym, spaceSym ,08F50h, 08F72h, 08F65h, 08F73h, 08F73h, 08F00h, 08F61h, 08F6Eh, 08F79h, 08F00h, 08F6Bh, 08F65h, 08F79h, 08F00h, 08F74h, 08F6Fh, 08F00h, 08F65h, 08F78h, 08F69h, 08F74h, 08F2Eh, 08F52h, 08F20h, 08F74h, 08F6Fh, 08F20h, 08F72h, 08F65h, 08F73h, 08F74h, 08F61h, 08F72h, 08F74h, 08F20h,2 dup(spaceSym), blackVSym
					dw 0FC8h, WINbannerWidth-2 dup(blackHSym), 0FBCh					
.code
clearScreen MACRO
	push ax
	mov ax, 0003h 
	int 10h
	pop ax
ENDM

CheckBuffer MACRO ; 
	mov ah, 01h
	int 16h
ENDM

ReadFromBuffer MACRO ; 
	mov ah, 00h
	int 16h
ENDM

main:
	mov ax, @data
	mov ds, ax
	mov dataStart, ax
	mov ax, videoStart
	mov es, ax
	xor ax, ax
	jmp startGame
restart:					; restart part, that refreshes virus and hero Positions, clears game field,
	push es					; kill necessary flags, initializes positions with it's start value
	push si
	push cx
	push bx

	mov bx,virusPosOld
	call getPosOffset
	mov si,bx
	mov screen[si],landSym
	mov bx,virusPos
	call getPosOffset
	mov si,bx
	mov screen[si],landSym

	mov bx,heroPos
	call getPosOffset
	mov si,bx
	cmp flagOnField,1
	je setSpace
	mov screen[si],landSym
	jmp nextmmark
setSpace:	
	mov screen[si],spaceSym
nextmmark:	
	
	xor bx,bx
	mov es,dataStart
	mov si,01E6h
	mov cx,04A0h
restartLoop:	
	mov screen[si],spaceSym
	add si,2
	dec cx
	inc bx
	cmp bx,004Ah
	jb restartLoop
	xor bx,bx
	add si,0Ch
	cmp cx,0000h
	ja restartLoop

	mov di,offset Area - offset screen
	mov screen[di],0430h
	inc di
	inc di
	mov screen[di],0430h
	mov WINCounter,0
	mov Percent,00h
	mov di, offset life - offset screen
	mov screen[di],0933h
	mov heroLife, 03h
	

	call killFlags
	mov enemyOnePos,1209h
	mov enemyOnePosOld,1209h
	mov enemyTwoPos, 2710h
	mov enemyTwoPosOld,2710h
	mov virusPos,2614h
	mov virusPosOld,2614h
	mov heroPos,2601h
	mov heroPosOld,2601h
	mov heroDir,0000h

	pop bx
	pop cx
	pop si
	pop es

startGame:
	
gameLoop:					; main game loop	
	call initScreen 		; proc that draw moving symbols on screen and initializes it
	call Sleep				; sleep procedure
	call updateGame			; game Logic update: keys,delay,moving hero n' enemies processing
	cmp WINCounter,0428h	; 0428h - equals 90-92 percents of game field.
	jge	exitWin				
	jmp gameLoop
exitGO:
	clearScreen
	mov ax,GObannerWidth
	mov bx, offset GameOverBanner
	mov cx, GObannerLines
	mov dx, GObannerPos
	call printBanner	
	mov ah,7h
	int 21h
	cmp al,72h
	je jmpRestart
	jmp exit
exitWin:
	clearScreen
	mov ax,WINbannerWidth
	mov bx, offset WINbanner
	mov cx, WINbannerLines
	mov dx, WINbannerPos
	call printBanner
	
	mov ah,7h
	int 21h
	cmp al,72h
	je jmpRestart
exit:
	clearScreen
	mov ah, 4ch			; endpoint of game
	int 21h
jmpRestart:
	jmp restart


checkPercent proc		; procedure that checks previous percent, than gets new percent, compare it,
	push ax				; and if they're not equal that increase Area percent (new-old)percent times
	push bx
	push dx
	push cx
	mov ax,WINCounter
	mov bx,000Ch
	div bl
	cmp al,Percent
	je checkPercentEnd
	xor cx,cx
	mov cl,Percent
	mov Percent,al
	sub al,cl
	mov cl,al
	
loopIncArea:	
	call incArea
	loop loopIncArea
checkPercentEnd:
	pop cx
	pop dx
	pop bx
	pop ax
	ret
endp checkPercent
	
printBanner proc		; proc that prints banner
	push es				; input: ax- banner width, bx - banner offser , cx- banner lines num , dx - pos to print
	push videoStart		; output: none
	pop es
	mov di, dx
	mov si, bx
	cld
loopPrintBanner:
	push cx
	mov cx, ax
	rep movsw
	add di, 2*xSize 
	sub di,ax
	sub di,ax
	pop cx
	loop loopPrintBanner
	pop es
ret
endp printBanner

drawSymbol proc 		; input: ax - old Position
	push si 			; bx - old Position Symbol
	push bx 			; dx - new Position
	mov bx,ax			; cx - new Position Symbol
	call getPosOffset	; Output: none - old position symbol was redrawed with bx symbol
	mov si,bx			; and new position symbol was redrawed with cx symbol
	pop bx
	mov screen[si],bx
	push bx
	mov bx, dx
	call getPosOffset
	mov si,bx
	mov screen[si],cx
	pop bx
	pop si
	ret
endp drawSymbol

incArea proc  					; proc that increase Area percent on the screen            
	push ax              
	push es               
	push si               
	push di               
	push cx
	mov es, videoStart    
	mov cx, 2 	  		  
	mov di, offset area  - offset screen +2	
							; moving from the  last symbol to the first
loopArea:	              
	mov ax, screen[di]    
	cmp al, 39h			  	;'9' symbol
	jne nineNotNow                      
	sub al, 9			 
	mov screen[di], ax               
	sub di, 2               
	loop loopArea         
	jmp incAreaEnd	      
                          
nineNotNow:               
	inc ax                
	mov screen[di], ax       
incAreaEnd:		          
	pop cx
	pop di                
	pop si                
	pop es                
	pop ax                
	ret                   
endp incArea  
							; it draw enemies and hero on screen
initScreen proc				; if (flagStartFillMap is 1) then it starts to fill all screen with enemies 
	push ax					; and then changes remaining space and track symbols with land symbol
	push cx					; else just initializes video
	push dx
	push bx
drawit:
	cmp flagDrawLastLand,1
	je setLand

	cmp flagStartFillMap,1
	je setTrack

	cmp flagOnField,1
	je setTrack
	jmp setLand

setLand:
	mov bx, landSym
	jmp drawHero
setTrack:
	mov bx, trackSym
drawHero:
	mov ax, heroPosOld
	mov dx, heroPos
	mov cx, heroSym
	call drawSymbol

	mov ax,enemyOnePosOld
	mov bx, spaceSym
	mov dx, enemyOnePos
	mov cx, enemySym
	call drawSymbol

	mov ax,enemyTwoPosOld
	mov bx, spaceSym
	mov dx, enemyTwoPos
	mov cx, enemySym
	call drawSymbol

	mov ax,virusPosOld
	mov bx, landSym
	mov dx, virusPos
	mov cx, virusSym
	call drawSymbol

	cmp flagStartFillMap,1
	jne showScreen
	mov flagStartFillMap,0
	
	mov ptrend,0000h
	mov ptrcur,0000h

	push bx
	mov bx, enemyOnePos
	push si
	mov si,ptrcur
	mov fillArray[si], bx
	pop si
	pop bx
	call fillMap
	call clearArray

	mov ptrend,0000h
	mov ptrcur,0000h

	push bx
	mov bx, enemyTwoPos
	push si
	mov si,ptrcur
	mov fillArray[si], bx
	pop si
	pop bx

	call fillMap
	call clearArray
	mov ptrend,0000h
	mov ptrcur,0000h
	call changeMap

	call checkPercent
	jmp drawit
showScreen:

	mov si, offset screen
	xor di,di
	mov cx,elementsNum
	rep movsw
initScreenEnd:
	pop bx
	pop dx
	pop cx
	pop ax
	ret
endp initScreen

Sleep proc 					; proc that wait for RTC to reach time=RTC+generalWaitTime
	push ax 
	push bx 
	push cx
	push dx 
	mov ax, 00h 
	int 1Ah 
	add dx, generalWaitTime 
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

getPosOffset proc 			; input: bh - Xpos, bl - Ypos
	push ax 				; output: bx - current "block" offset( bh + bl*LineSize)*oneBlockSize
	push dx
	push cx

	xor ah, ah
	mov al, bl 
	mov dl, xSize 
	mul dl 
	mov dl, bh 
	xor dh, dh 
	add ax, dx 
	mov dx, 2 
	mul dx 
	mov bx, ax

	pop cx
	pop dx
	pop ax
	ret
endp getPosOffset

toNextSym proc 				; input: none
	push ax 				; output: bh - new xPos, bl - new yPos, according to direction
	mov ax,dx
	mov bx,cx

	cmp al,01h
	jne incYpos
	dec bl
	jmp changeXpos
incYpos:
	inc bl

changeXpos:
	cmp ah,01h
	jne incXpos
	dec bh
	jmp toNextSymEnd
incXpos :
	inc bh
toNextSymEnd:
	pop ax
	ret
endp toNextSym

checkCellPushOff proc 		; procedure has two separate push off logic: sea enemy and virus(land enemy)
	push bx 				; input: bx - xPos yPos
	push si 				; output: none. Sets flagPushOff
	call getPosOffset
	mov si, bx

	cmp flagVirus,1
	je virusMark

	cmp screen[si],heroSym
	jne nextCheck
	cmp flagOnField,1
	jne pushOffMark
	call setGameOver
nextCheck:
	cmp screen[si],landSym
	je pushOffMark

	cmp screen[si],virusSym
	je pushOffMark

	cmp screen[si],enemySym
	je pushOffMark

	cmp screen[si],trackSym
	je goMark
	jmp nonOfPushOffSymbols
goMark:
	call setGameOver
nonOfPushOffSymbols:
	mov flagPushOff, 0
	jmp checkCellPushOffEnd

virusMark:					; virus logic
	cmp screen[si],heroSym
	je setVirusGo

	cmp screen[si],landSym
	jne pushOffMark

	jmp nonOfPushOffSymbols2
setVirusGo:
	call setGameOver
nonOfPushOffSymbols2:
	mov flagPushOff,0
	jmp checkCellPushOffEnd
pushOffMark:
	mov flagPushOff, 1
checkCellPushOffEnd:
	pop si
	pop bx
	ret
endp checkCellPushOff

setRightCell proc 		; input: bx- cell current Pos; ax - cell direction( ah - left/right, al - up/down.
						; Note: left-up directon means, that cell's "move vector" aimed at left upper corner, left-down - etc.)
	cmp ah,00 			; output: new bx Pos right to previous, according to its direction.
	je _plus

	cmp al,01
	je to_yRC

	sub bh,1
	jmp setRightCellEnd
to_yRC:
	sub bl,1
	jmp setRightCellEnd
_plus:
	cmp al,01
	je to_xRC

	add bl,1
	jmp setRightCellEnd
to_xRC:
	add bh,1

setRightCellEnd:
	ret
endp setRightCell

setLeftCell proc 		; input: bx - cell current Pos; ax - cell direction( ah - left/right, al - up/down.
						; Note: left-up directon means, that cell's "move vector" aimed at left upper corner, left-down - etc.)
						; output: bx - new Pos left to previous, according to its direction.
	cmp al,01
	je minus_

	cmp ah,01
	je to_yLC

	add bh,1
	jmp setLeftEnd
to_yLC:
	add bl,1
	jmp setLeftEnd

minus_:
	cmp ah,01
	je to_xLC

	sub bl,1
	jmp setLeftEnd
to_xLC:
	sub bh,1

setLeftEnd:
	ret
endp setLeftCell

checkCell proc 			; input: bx - cell to be checked( bh - Xpos, bl- Ypos)
						; output: none. Sets flagEmptyCell to 1 if cell is empty
	call checkCellPushOff

	cmp flagPushOff,1
	je setEmptyFlag
	mov flagEmptyCell,1

	jmp checkCellEnd
setEmptyFlag:
	mov flagEmptyCell,0
checkCellEnd:
	ret
endp checkCell

setLeftLow proc 		; input: bx - cell current Pos; ax - cell direction( ah - left/right, al - up/down.
						; Note: left-up directon means, that cell's "move vector" aimed at left upper corner, left-down - etc.)
						; output: bx - new left-low Pos , according to its direction.
						;Example(direction - left-up):
						;  			right cell
						;  	 		  |
						; 	 		  v
	cmp ah,01 				;  			_ _ _
	jne minus__ 				; 		       |_|_|_|<---right low
						;            left----->|_|0|_<---— current pos
	cmp al,01  				;            left-low->|_| |_|<------back cell
	je to_yLL  			

	add bh,1
	jmp setLeftLowEnd
to_yLL:
	add bl,1
	jmp setLeftLowEnd
minus__:
	cmp al,01
	je to_xLL

	sub bl,1
	jmp setLeftLowEnd
to_xLL:
	sub bh,1

setLeftLowEnd:
	ret
endp setLeftLow

setRightLow proc 		; input: bx - cell current Pos; ax - cell direction( ah - left/right, al - up/down.
						; Note: left-up directon means, that cell's "move vector" aimed at left upper corner, left-down - etc.)
						; output: bx - new right-low Pos , according to its direction.
	cmp al,01
	je __plus

	cmp ah,01
	je to_yRL

	sub bh,1
	jmp setRightLowEnd
to_yRL:
	sub bl,1
	jmp setRightLowEnd
__plus:
	cmp ah,01
	je to_xRL

	add bl,1
	jmp setRightLowEnd
to_xRL:
	add bh,1

setRightLowEnd:
	ret
endp setRightLow

setBackCell proc 			; input: bx - cell current Pos; ax - cell direction( ah - left/right, al - up/down.
							; Note: left-up directon means, that cell's "move vector" aimed at left upper corner, left-down - etc.)
							; output: bx - new pos of cell behind the current cell, according to ax( dircetion)
	cmp ah,01
	je incX
	dec bh
	jmp metka_
incX:
	inc bh

metka_:
	cmp al,01
	je incY
	dec bl
	jmp setBackCellEnd
incY:
	inc bl

setBackCellEnd:
	ret
endp setBackCell

pushOff1 proc			; push off to right low cell
						; input: none
	mov bx, cx 			; output: bx - new pos - left to current cell, according to ax(direction);
	mov ax,dx 			; Sets flagMoveEnemy, if movement is possible
	call setLeftCell
	call setLeftLow
	call checkCell
	cmp flagEmptyCell,1
	je PF1setFlagAndChangeVector
	call pushOff2

	jmp pushOff1End
PF1setFlagAndChangeVector:
	mov flagMoveEnemy,1
	push cx
	mov cx, ax

	cmp ch,01
	je _setVec1

	mov al,01
	jmp __setVec1
_setVec1:
	mov al,00

__setVec1:
	cmp cl, 01
	je __setVec1_

	mov ah,00
	jmp setVec1End
__setVec1_:
	mov ah,01

setVec1End:
	pop cx
pushOff1End:
	ret
endp pushOff1
					; push off to left low cell
pushOff2 proc 		; input: none
					; output: bx - new pos - right to current cell, according to ax(direction)
	mov bx, cx 		; Sets flagMoveEnemy, if movement is possible
	mov ax,dx
	call setRightCeLL
	call setRightLow
	call checkCell
	cmp flagEmptyCell,1
	je PF2setFlagAndChangeVector
	call pushOff3

	jmp pushOff2End
PF2setFlagAndChangeVector:
	mov flagMoveEnemy,1
	push cx
	mov cx, ax

	cmp ch,01
	je _setVec2

	mov al,00
	jmp __setVec2
_setVec2:
	mov al,01

__setVec2:
	cmp cl, 01
	je __setVec2_

	mov ah,01
	jmp setVec2End
__setVec2_:
	mov ah,00

setVec2End:
	pop cx
pushOff2End:
	ret
endp pushOff2
				; push off to back cell
pushOff3 proc 	; input: none
				; output: bx - new pos - back to current cell, according to ax( direction)
	mov bx, cx 	; Sets flagMoveEnemy, if movement is possible
	mov ax, dx
	call setBackCell
	call checkCell
	cmp flagEmptyCell,1
	je PF3setFlagAndChangeVector

	mov flagMoveEnemy,0
	jmp pushOff3End
PF3setFlagAndChangeVector:
	mov flagMoveEnemy,1
	push cx
	mov cx, ax

	cmp ch,01
	je _setVec3

	mov ah,01
	jmp __setVec3
_setVec3:
	mov ah,00

__setVec3:
	cmp cl, 01
	je __setVec3_

	mov al,01
	jmp setVec3End
__setVec3_:
	mov al,00

setVec3End:
	pop cx
pushOff3End:
	ret
endp pushOff3
				
checkAndSetPushOff proc 	; procedure that checks next cell and it's neibours and decides to continue movement or to push off(in 3 ways possible)
							; input: bx - xPos yPos
							; output: ax -new Direction vector.
							; bx - pos to move
	call checkCellPushOff 	; sets MoveEnemy flag
	cmp flagPushOff,1
	je checkSurroundings

	push bx
	mov bx, cx
	mov ax, dx
	call setLeftCell
	call checkCell
	pop bx

	cmp flagEmptyCell,1
	je moveToNextCell

	push bx
	mov bx, cx
	mov ax,dx
	call setRightCell
	call checkCell
	pop bx

	cmp flagEmptyCell,1
	je moveToNextCell

	jmp pushOff3_
checkSurroundings: 			; if it's impossible to continue movement forward, it's necessary to 
	mov bx, cx				; check right and left cells to decide how to push off
	mov ax,dx
	call setLeftCell
	call checkCell

	cmp flagEmptyCell,1
	je checkRight_

	mov bx, cx
	mov ax,dx
	call setRightCell
	call checkCell

	cmp flagEmptyCell,1
	je pushOff2_
	jmp pushOff3_

checkRight_:
	mov bx, cx
	mov ax,dx
	call setRightCell
	call checkCell

	cmp flagEmptyCell,1
	je pushOff3_
	jmp pushOff1_

pushOff1_:
	call pushOff1
	jmp checkAndPushOffEnd
pushOff2_:
	call pushOff2
	jmp checkAndPushOffEnd
pushOff3_:
	call pushOff3
	jmp checkAndPushOffEnd
moveToNextCell:
	mov flagMoveEnemy,1
checkAndPushOffEnd:
	ret
endp checkAndSetPushOff

moveEnemy proc				; procedure gets dx - enemy direction and cx - enemy position and processes it
	call toNextSym
	call checkAndSetPushOff ; checks symbol[si] for push off possibility
							; and returns in bx pushOff position if its allowed, ax -new direction vector, if direction was changed
	ret
endp moveEnemy

moveEnemies proc			; procedure that manages sea enemy movement
	push dx
	push cx
	push ax
	push bx

	mov dx, enemyOneDir
	mov cx, enemyOnePos
	call moveEnemy

	cmp flagMoveEnemy,1
	je startMove1

	jmp nextEnemy2
startMove1:
	mov enemyOneDir,ax
	mov enemyOnePosOld, cx ; Pos(cx) is old Pos now
	mov enemyOnePos, bx

nextEnemy2:
	mov flagMoveEnemy,0

	mov dx, enemyTwoDir
	mov cx, enemyTwoPos
	call moveEnemy

	cmp flagMoveEnemy,1
	je startMove2

	jmp moveEnemiesEnd
startMove2:
	mov enemyTwoDir,ax
	mov enemyTwoPosOld, cx ; Pos(cx) is old Pos now
	mov enemyTwoPos, bx

moveEnemiesEnd:
	mov flagMoveEnemy,0

	pop bx
	pop ax
	pop cx
	pop dx
	ret
endp moveEnemies

moveVirus proc			; move Virus procedure is almost an equivalent of sea enemy move proc
	push dx
	push cx
	push ax
	push bx

	mov flagVirus,1

	mov dx, virusDir
	mov cx, virusPos
	call moveEnemy

	cmp flagMoveEnemy,1
	je startMoveV

	jmp moveViEnd
startMoveV:
	mov virusDir,ax
	mov virusPosOld, cx ; Pos(cx) is old Pos now
	mov virusPos, bx

moveViEnd:
	mov flagMoveEnemy,0
	mov flagVirus,0

	pop bx
	pop ax
	pop cx
	pop dx
	ret
endp moveVirus

setPos proc			; procedure that sets new position according to direction
	push ax
	push bx

	mov bx, heroPos
	mov ax,heroDir
	cmp ax, 0001h
	je toLeft

	cmp ax, 0010h
	je toRight

	cmp ax, 0100h
	je toUp

	cmp ax,1000h
	je toDown

	jmp setPosEnd
toLeft:
	sub bh,1
	jmp setPosEnd
toRight:
	add bh,1
	jmp setPosEnd
toUp:
	sub bl,1
	jmp setPosEnd
toDown:
	add bl,1
setPosEnd:

	mov heroPos,bx

	pop bx
	pop ax
	ret
endp setPos

setDir proc				; procedure that sets new Direction:
	push bx				; if any direction button was pushed, direction will change: 0000->xxxx(if xxxx=0000 flagDoNothing is set), xxxx->new xxxx(but not 0000)

	mov bx, heroDir

	cmp bx,0000h
	je nextComp

	cmp ax,0000h
	je notSetD
	jmp setD
nextComp:
	cmp ax,0000h
	je doN
	jmp setD
setD:
	mov heroDir,ax
	jmp setDirEnd
doN:
	mov flagDoNothing,1
notSetD:
setDirEnd:
	pop bx
	ret
endp setDir

respawn proc				; respawn procedure 
	push cx					; clears track,virus and hero symbols
	push si					; set start hero and virus positions
	push es
	mov es,dataStart
	xor si,si
	mov cx,elementsNum

respawnLoop:

	cmp screen[si],heroSym
	je changeHero

	cmp screen[si],virusSym
	je changeVirus

	cmp screen[si],trackSym
	je changeTrack
	jmp respawnLoopEnd
changeVirus:
	mov screen[si],landSym
	jmp respawnLoopEnd
changeHero:
	cmp flagOnField,1
	je changeHeroSpace
	mov screen[si],landSym
	jmp respawnLoopEnd
changeHeroSpace:
	mov screen[si],spaceSym
	jmp respawnLoopEnd
changeTrack:
	mov screen[si],spaceSym

respawnLoopEnd:
	inc si
	inc si
	dec cx
	cmp cx, 0000h
	jne respawnLoop

	mov heroPos,2601h
	mov heroPosOLd, 2601h
	mov heroDir, 0000h

	mov virusPos,2614h
	mov virusPosOld,2614h
	mov virusDir,0101h

	pop es
	pop si
	pop cx
	ret 
endp respawn

killFlags proc				; sets some flags to start value

	mov flagOnField,0
	mov flagDoNothing,1
	mov flagDrawLastLand,0
	mov flagStartFillMap,0

	ret
endp killFlags

setGameOver proc			; procedure that decreases life and in case of 00 life jumps to game over mark
	push es
	push ax
	push di

	mov es, videoStart
	mov di,offset life - offset screen
	mov ax, screen[di]
	dec ax
	mov screen[di],ax
	
	pop di
	pop ax
	pop es

	dec heroLife

	cmp heroLife,00h
	je jmpexitGo
	call respawn
	call killFlags
	jmp gameLoop
jmpexitGo:
	jmp exitGO 
	ret
endp setGameOver

moveHero proc				; procedure that checks the next cell and 
	push ax					; if hero meets spaceSym it sets flagOnField=1, changes Direction(if button was pushhed) and Position
	push bx					; if hero meets enemy virus or it's own track it losses life
	push cx					; if hero meets borders it stops 
	push dx					; if hero meets land and hero is on field - it stops, saves position and sets flagStartFillMap=1
							; 								if hero is't on field - changes Direction(if button was pushhed) and Position
	mov flagDoNothing,0

	mov bx, heroPos
	push bx
	push cx
	mov cx,heroDir
	call setDir
	call setPos
	mov bx, heroPos
	mov dx, bx
	call getPosOffset
	mov heroDir,cx
	pop cx
	mov si,bx
	pop bx
	mov heroPos,bx

	cmp flagDoNothing,1
	je jmpMoveHeroEnd
	jmp moveHeromark
jmpMoveHeroEnd:					; that "code construction" is necessary for "far jump" 
	jmp moveHeroEnd				; as jmp is automatically transforms into "far jump" when it is necessary, but jxx dont
moveHeromark:
	cmp screen[si], spaceSym
	jne _continue

	cmp dx,enemyOnePos			; the following 9 lines necessary for situation, when enemy came into cell, but not drawed on screen yet
	jne check2enemy
	call setGameOver
	jmp moveHeroEnd
check2enemy:
	cmp dx,enemyTwoPos
	jne checkonff
	call setGameOver
	jmp moveHeroEnd
checkonff:
	cmp flagOnField,1
	je	mark1
	mov flagDrawLastLand,1
	mov flagOnField,1
	jmp mark2
mark1:	
	mov flagDrawLastLand,0
	mov flagOnField,1 
mark2:
	call setDir
	push cx
		mov cx, heroPos
		mov heroPosOld,cx
		call setPos
	pop cx
	jmp moveHeroEnd
	_continue:
	cmp screen[si],enemySym
	je evt
	cmp screen[si],virusSym
	je evt
	cmp screen[si],trackSym
	je evt
	jmp __continue
	evt:
	call setGameOver
	jmp moveHeroEnd
__continue:
	cmp screen[si],hBorderSym
	je borders
	cmp screen[si],vBorderSym
	je borders
	jmp __continue_
borders:
	mov heroDir,0000h
	jmp moveHeroEnd
	
__continue_:
	cmp screen[si],landSym
	je chenckOnf

	jmp moveHeroEnd
chenckOnf:	
; 	cmp dx,enemyOnePos			
; 	jne checkTwoenemy
; 	call setGameOver
; 	jmp moveHeroEnd
; checkTwoenemy:
; 	cmp dx,enemyTwoPos
; 	jne cmpOnField
; 	call setGameOver
; 	jmp moveHeroEnd
; cmpOnField:
	cmp flagOnField,1
	jne notOnField
	
	mov flagOnField,0
	mov flagStartFillMap,1

	call setDir
	push cx
		mov cx, heroPos
		mov heroPosOld,cx
		call setPos
	pop cx
	mov heroDir,0000h
	jmp moveHeroEnd
notOnField:
	mov flagStartFillMap,0
	call setDir
	push cx
		mov cx, heroPos
		mov heroPosOld,cx
		call setPos
	pop cx
moveHeroEnd:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp moveHero

updateGame proc					; prodedure manages delay for enemies and hero(hero delay is fixed and cannot be changed during the game)
	push ax						; if heroDelayCounter reached necessary delay it start process keys from buffer
	xor ax,ax
	mov al, enemyDelay
	cmp enemyDelayCounter,al
	pop ax
	jne incEnemyDelay
	call moveVirus
	call moveEnemies
	mov enemyDelayCounter,00h
	jmp checkHeroDelay
incEnemyDelay:
	inc enemyDelayCounter

checkHeroDelay:
	push ax
	xor ax,ax
	mov al,heroDelay
	cmp heroDelayCounter,al
	pop ax
	jne jmpIncHeroDelay
	jmp continueHeroMovement
jmpIncHeroDelay:
	jmp incHeroDelay
continueHeroMovement:
	mov heroDelayCounter,00h
	CheckBuffer
	jnz mainloop
	jz setNoKey
setNoKey:
	mov ax,0000h
	jmp noKeyPressed 

mainloop:
	ReadFromBuffer

	cmp ah,KRes
	je jmpToRestart

	cmp ah,KZero
	je incEnemySpeed

	cmp ah,KNine
	je decEnemySpeed

	cmp ah,KPlus
	je decGeneralWaitTime

	cmp ah,KMinus
	je incGeneralWaitTime

	cmp ah,KExit
	je jmpToExit

	jmp continueMainLoop
incEnemySpeed:
	cmp enemyDelay,01h
	jb noKeyPressed
	dec enemyDelay
	mov enemyDelayCounter,00h
	jmp noKeyPressed
decEnemySpeed:
	cmp enemyDelay,09h
	ja noKeyPressed
	inc enemyDelay
	mov enemyDelayCounter,00h
	jmp noKeyPressed
incGeneralWaitTime:
	cmp generalWaitTime,0009h
	jge noKeyPressed
	inc generalWaitTime
	jmp noKeyPressed
decGeneralWaitTime:
	cmp generalWaitTime,0001h
	jbe noKeyPressed
	dec generalWaitTime
	jmp noKeyPressed
jmpToExit:
	jmp exit
jmpToRestart:
	jmp restart	

continueMainLoop:
	cmp ah,KMoveUp
	je setMoveUp

	cmp ah,KMoveDown
	je setMoveDown

	cmp ah,KMoveLeft
	je setMoveLeft

	cmp ah,KMoveRight
	je setMoveRight

	mov ax,0000h
	jmp noKeyPressed
setMoveLeft: ;
	mov ax,0001h
	jmp noKeyPressed ;
setMoveRight: ;
	mov ax,0010h
	jmp noKeyPressed ;
setMoveUp: ;
	mov ax,0100h
	jmp noKeyPressed ;
setMoveDown: ;
	mov ax,1000h
	jmp noKeyPressed
noKeyPressed:						; moving hero according to direction - ax
	call moveHero
	jmp updateGameEnd
incHeroDelay:
	inc heroDelayCounter
updateGameEnd:
	ret
endp

changeMap proc						; procedure that change all enemy symbols on screen with spaces, track and space symbols with land sym
	push cx
	push si
	push es
	push bx
	xor bx,bx
	mov es,dataStart
	mov si,01E6h
	mov cx,04A0h

changeMapLoop:

	cmp screen[si],enemySym
	je chEnemy

	cmp screen[si],spaceSym
	je chLand

	cmp screen[si],trackSym
	je chLand
	jmp changeMapLoopEnd

chEnemy:
	mov screen[si],spaceSym
	jmp changeMapLoopEnd
chLand:
	inc WINCounter
	mov screen[si],landSym
changeMapLoopEnd:
	inc si
	inc si
	dec cx
	inc bx
	cmp bx,004Ah
	jb changeMapLoop
	xor bx,bx
	add si,0Ch
	cmp cx, 0000h
	jne changeMapLoop

	pop bx
	pop es
	pop si
	pop cx
	ret
endp changeMap

clearArray proc					
	push si
clearArrLoop:
	mov si,ptrend
	mov fillArray[si],0000h

	dec ptrend
	dec ptrend
	cmp ptrend,0FFFh
	jb continueClear
	inc ptrend
	inc ptrend
continueClear:
	cmp ptrend,0000h
	jne clearArrLoop

	mov si,ptrend
	mov fillArray[si],0000h

	pop si
	ret
endp clearArray

fillMap proc				; procedure that fills screen with enemies in order to fill empty spaces later
	push si					; it uses priority queue to keep all enemies positions, that are added if current enemy has empty (left/right/up/down) cell 
	push bx					
fillMapLoop:
	
	push si
	mov si, ptrcur
	mov bx, fillArray[si]
	pop si
right:
	push bx
	inc bh
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne left

	call fillEnemy
left:
	pop bx

	push bx
	dec bh
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne up

	call fillEnemy
up:
	pop bx
	push bx
	dec bl
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne down

	call fillEnemy

down:
	pop bx

	push bx
	inc bl
	push bx
	call getPosOffset
	mov si,bx
	pop bx
	cmp screen[si], spaceSym
	jne fillMapLoopEnd

	call fillEnemy

fillMapLoopEnd:
	pop bx

	inc ptrcur
	inc ptrcur
	push ax
	mov ax,ptrcur
	cmp ax,ptrend
	pop ax
	jle jmpfillMapLoop

	jmp fillMapEnd
jmpfillMapLoop:
	jmp fillMapLoop

fillMapEnd:
	pop bx
	pop si
	ret
endp fillMap

fillEnemy proc
	mov screen[si], enemySym
	push di
	inc ptrend
	inc ptrend
	mov di,ptrend
	mov fillArray[di],bx
	pop di
	ret
endp fillEnemy

end main

