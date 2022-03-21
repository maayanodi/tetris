IDEAL
MODEL small
STACK 100h
DATASEG

board db 18,18,18,18,18,18,18,18,18,18,18,18, 20 dup(18,0,0,0,0,0,0,0,0,0,0,18), 18,18,18,18,18,18,18,18,18,18,18,18

;; Shape 1 ;;

;Shape11 db		0, 0, 0, 0,
;				4, 4, 4, 0,
;				0, 4, 0, 0,
;				0, 0, 0, 0
Shape11 db    	0, 0, 0, 0, 4, 4, 4, 0, 0, 4, 0, 0, 0, 0, 0, 0

;Shape12 db		0, 4, 0, 0,
;				4, 4, 0, 0,
;				0, 4, 0, 0,
;				0, 0, 0, 0
Shape12 db		0, 4, 0, 0, 4, 4, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0

;Shape13 db		0, 4, 0, 0,
;				4, 4, 4, 0,
;				0, 0, 0, 0,
;				0, 0, 0, 0
Shape13 db		0, 4, 0, 0, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0

;Shape14 db		0, 4, 0, 0,
;				0, 4, 4, 0,
;				0, 4, 0, 0,
;				0, 0, 0, 0
Shape14 db		0, 4, 0, 0, 0, 4, 4, 0, 0, 4, 0, 0, 0, 0, 0, 0

;; Shape 2 ;;

;Shape21 db		0, 0, 0, 0,
;				0, 1, 1, 0, 
;				0, 1, 1, 0, 
;				0, 0, 0, 0
Shape21 db 	  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0

;Shape21 db		0, 0, 0, 0,
;				0, 1, 1, 0, 
;				0, 1, 1, 0, 
;				0, 0, 0, 0
Shape22 db 	  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0

;Shape21 db		0, 0, 0, 0,
;				0, 1, 1, 0, 
;				0, 1, 1, 0, 
;				0, 0, 0, 0
Shape23 db 	  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0

;Shape21 db		0, 0, 0, 0,
;				0, 1, 1, 0, 
;				0, 1, 1, 0, 
;				0, 0, 0, 0
Shape24 db 	  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0

;; Shape 3 ;;

;Shape31 db		0, 0, 0, 0,
;				0, 0, 0, 0,
;				2, 2, 2, 2,
;				0, 0, 0, 0
Shape31 db 	  0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 0, 0, 0, 0

;Shape32 db		0, 2, 0, 0,
;				0, 2, 0, 0,
;				0, 2, 0, 0,
;				0, 2, 0, 0
Shape32 db		0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0

;Shape33 db		0, 0, 0, 0,
;				0, 0, 0, 0,
;				2, 2, 2, 2,
;				0, 0, 0, 0
Shape33 db 	  0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 0, 0, 0, 0

;Shape34 db		0, 2, 0, 0,
;				0, 2, 0, 0,
;				0, 2, 0, 0,
;				0, 2, 0, 0
Shape34 db		0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0

;; Shape 4 ;;

;Shape41 db		0, 0, 0, 0,
;				3, 3, 3, 0,
;				0, 0, 3, 0,
;				0, 0, 0, 0
Shape41 db 	  0, 0, 0, 0, 3, 3, 3, 0, 0, 0, 3, 0, 0, 0, 0, 0

;Shape42 db		0, 0, 3, 0,
;				0, 0, 3, 0,
;				0, 3, 3, 0,
;				0, 0, 0, 0
Shape42 db		0, 0, 3, 0, 0, 0, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0

;Shape43 db		0, 0, 0, 0,
;				0, 3, 0, 0,
;				0, 3, 3, 3,
;				0, 0, 0, 0
Shape43 db		0, 0, 0, 0, 0, 3, 0, 0, 0, 3, 3, 3, 0, 0, 0, 0

;Shape44 db		0, 0, 0, 0,
;				0, 3, 3, 0,
;				0, 3, 0, 0,
;				0, 3, 0, 0
Shape44 db		0, 0, 0, 0, 0, 3, 3, 0, 0, 3, 0, 0, 0, 3, 0, 0

;; Shape 5 ;;

;Shape51 db		0, 0, 0, 0,
;				0, 5, 5, 5,
;				0, 5, 0, 0,
;				0, 0, 0, 0
Shape51 db 	  0, 0, 0, 0, 0, 5, 5, 5, 0, 5, 0, 0, 0, 0, 0, 0

;Shape52 db		0, 0, 0, 0,
;				0, 5, 5, 0,
;				0, 0, 5, 0,
;				0, 0, 5, 0
Shape52 db		0, 0, 0, 0, 0, 5, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0

;Shape53 db		0, 0, 0, 0,
;				0, 0, 5, 0,
;				5, 5, 5, 0,
;				0, 0, 0, 0
Shape53 db		0, 0, 0, 0, 0, 0, 5, 0, 5, 5, 5, 0, 0, 0, 0, 0

;Shape54 db		0, 5, 0, 0,
;				0, 5, 0, 0,
;				0, 5, 5, 0,
;				0, 0, 0, 0
Shape54 db		0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0



;; Shape 6 ;;

;Shape61 db		0, 0, 0, 0,
;				6, 6, 0, 0,
;				0, 6, 6, 0,
;				0, 0, 0, 0
Shape61 db 	  0, 0, 0, 0, 6, 6, 0, 0, 0, 6, 6, 0, 0, 0, 0, 0

;Shape62 db		0, 0, 6, 0,
;				0, 6, 6, 0,
;				0, 6, 0, 0,
;				0, 0, 0, 0
Shape62 db		0, 0, 6, 0, 0, 6, 6, 0, 0, 6, 0, 0, 0, 0, 0, 0

;Shape63 db		0, 0, 0, 0,
;				6, 6, 0, 0,
;				0, 6, 6, 0,
;				0, 0, 0, 0
Shape63 db		0, 0, 0, 0, 6, 6, 0, 0, 0, 6, 6, 0, 0, 0, 0, 0

;Shape64 db		0, 0, 6, 0,
;				0, 6, 6, 0,
;				0, 6, 0, 0,
;				0, 0, 0, 0
Shape64 db		0, 0, 6, 0, 0, 6, 6, 0, 0, 6, 0, 0, 0, 0, 0, 0

;; Shape 7 ;;

;Shape71 db		0, 0, 0, 0,
;				0, 0, 9, 9,
;				0, 9, 9, 0,
;				0, 0, 0, 0
Shape71 db 	  0, 0, 0, 0, 0, 0, 9, 9, 0, 9, 9, 0, 0, 0, 0, 0

;Shape72 db		0, 9, 0, 0,
;				0, 9, 9, 0,
;				0, 0, 9, 0,
;				0, 0, 0, 0
Shape72 db		0, 9, 0, 0, 0, 9, 9, 0, 0, 0, 9, 0, 0, 0, 0, 0

;Shape73 db		0, 0, 0, 0,
;				0, 0, 9, 9,
;				0, 9, 9, 0,
;				0, 0, 0, 0
Shape73 db 	  0, 0, 0, 0, 0, 0, 9, 9, 0, 9, 9, 0, 0, 0, 0, 0

;Shape74 db		0, 9, 0, 0,
;				0, 9, 9, 0,
;				0, 0, 9, 0,
;				0, 0, 0, 0
Shape74 db		0, 9, 0, 0, 0, 9, 9, 0, 0, 0, 9, 0, 0, 0, 0, 0

columns db 12
x dw 160
y dw 100
DelayDiv dw 0000h
CurrRandom dw 1234h
FallRate dw 6
rfallrate dw 6
Shapex dw 3
Shapey dw 0
ShapeRotation dw 0
ShapeType dw 0
score dw 0
highscore dw 0
scorestring db 48,49,50,51,52
highscorestring db 48,49,50,51,52
ScoreAsAString db "score:"
HighScoreAsAString db "high score:"
Instructions db "Instructions:"
moveright db "right=D"
moveleft db "left=A"
turn db "rotate=W"
fast db "fast=S"
fall db "drop=SPACE"
newgame db "new game=R"
NextRandom dw 467
color db 15
loststring db "lose!"


CODESEG
proc GetSquare
	push bp
	mov bp, sp

	push bx
	
	mov ax, [bp+4]
	mul [columns]
	add ax, [bp+6]
	mov bx, ax
	mov al, [BYTE PTR board + bx]
	mov ah, al
	and al, 7Fh
	and ah, 80h
	
	pop bx

	pop bp
	ret 4
endp GetSquare


proc SetSquare
	push bp
	mov bp, sp
	
	push bx
	push cx
	push dx
	
	mov bx, [bp+8] ;x
	mov cx, [bp+6] ;y
	mov dx, [bp+4]
	
	and dh, 80h
	and dl, 7Fh
	or dl, dh
	
	mov ax, cx
	mul [columns]
	add ax, bx
	mov bx, ax
	mov [BYTE PTR board + bx], dl
	
	
	pop dx
	pop cx
	pop bx
	pop bp
	
	ret 6
endp SetSquare

	
	
proc DrawSquare
	push bp
	mov bp, sp

	push bx
	push cx
	push dx
	
	mov ax, [bp+6]
	inc ax
	mov bx, 320
	mul bx
	add ax, [bp+8]
	mov bx, 8
	mul bx
	mov bx, ax
	add bx, 112

	mov ax, [bp+4]


	mov dx, 7
	Pcol:
		mov cx, 7
		Prow:
			mov [es:bx], al
			inc bx
			loop Prow
		add bx, 313
		dec dx
		cmp dx, 0
		jne Pcol

	pop dx
	pop cx
	pop bx

	pop bp
	ret 6
endp DrawSquare



proc DrawBoard
	push cx
	push dx

	mov dx, 21
	Bcol:
		mov cx, 12
		Brow:
		    dec cx
			
			push cx
			push dx
			call GetSquare
			push cx
			push dx
			push ax
			call DrawSquare

			inc cx
			loop Brow
		dec dx
		cmp dx, 0
		jne Bcol

	pop dx
	pop cx

	ret
endp DrawBoard


proc CanSquareFall
	push bp
	mov bp, sp

	push bx

	mov bx, [bp+4]
	mov ax, [bp+6]
	
	add bx, [bp+8]
	add ax, [bp+10]
	push ax
	push bx
	call GetSquare
	
	cmp ah, 80h
	je FallingOrEmpty
	
	cmp al, 0
	je FallingOrEmpty
	
	xor ax, ax
	jmp CantFall
	
	FallingOrEmpty:
	mov ax, 1
	
	CantFall:

	pop bx
	pop bp
	ret 8
endp CanSquareFall


proc CanObjMov

	push bp
	mov bp, sp

	push bx
	push cx
	push dx

	
	mov dx, 20
	col:
		mov cx, 10
		row:
			push cx
			push dx
			call GetSquare
			cmp ah, 80h
			jne SoFarSoGood
			
			mov bx, [bp+6]
			push bx

			mov bx, [bp+4]
			push bx

			push cx
			push dx
			call CanSquareFall
			
			Left:

			cmp ax, 1
			je SoFarSoGood
			xor ax, ax
			jmp return
			
			SoFarSoGood:
			loop row
		dec dx
		cmp dx, 0
		jne col
	
	mov ax, 1
	
	return:
	pop dx
	pop cx
	pop bx
	pop bp
	ret 4
endp CanObjMov
	
	
proc MovSquare
	push bp
	mov bp, sp

	push cx
	push dx

	mov cx, [bp+6] 
	mov dx, [bp+4]

	push cx
	push dx
	call GetSquare
	
	add cx, [bp+10] ;x
	add dx, [bp+8] ;y
	push cx
	push dx
	push ax
	call SetSquare
	
	sub cx, [bp+10]
	sub dx, [bp+8]
	xor ax, ax

	push cx
	push dx
	push ax
	call SetSquare

	pop dx
	pop cx
	pop bp
	ret 8
endp MovSquare
	

proc MovObj
	push bp
	mov bp, sp

	push cx
	push dx
	push bx

	
	mov dx, 20
	Dcol:
		mov cx, 10
		Drow:

		    mov bx, [bp+8]
			cmp bx, 0
			je ppp1

			mov bx, 11
			sub bx, cx
			jmp ppp2
			ppp1:
			mov bx, cx
		
			ppp2:
			push bx
			push dx
			call GetSquare
			cmp ah, 80h
			je Drop
			jmp DontDrop
			
			Drop:
			mov ax, [bp+6]
			push ax

			mov ax, [bp+4]
			push ax

			push bx
			push dx
			call MovSquare

			
			DontDrop:
			loop Drow
		dec dx
		cmp dx, 0
		jne Dcol

	call DrawBoard
	
	mov bx, [Shapex]
	add bx, [bp+6]
	mov [Shapex], bx

	mov bx, [Shapey]
	add bx, [bp+4]
	mov [Shapey], bx

	pop bx
	pop dx
	pop cx
	pop bp
	ret 6
endp MovObj


proc Delay
push ax
push bx
push cx
push dx

system_time:   
  call HandleKeys

  mov  ah, 2ch
  int  21h
  
  mov  ax, dx
  mov al, ah
  mov ah, 0
  mov bl, 100
  mul bl
  mov dh, 0
  add ax, dx

  mov dx, 0
  mov bx, 10
  div bx

  mov dx, 0
  mov bx, [FallRate]
  div  bx
  cmp  dx, 0
  jnz  system_time
  cmp ax, [DelayDiv]
  jz  system_time

  mov [DelayDiv], ax

  pop dx
  pop cx
  pop bx
  pop ax

  ret
 endp Delay



proc Droppp
	push cx
	mov cx, 21
	droploop:
	push 0
	push 1
	call CanObjMov
	cmp ax, 0
	je RRRRR

	push 0
	push 0
	push 1
	call movobj
	loop droploop

	RRRRR:
	pop cx
	ret
endp Droppp


proc HandleKeys
	push bx

	mov ah, 01h
	int 16h
	jne KeyPressed
	jmp NoNeedToMov

	KeyPressed:
	mov ah, 00h
	int 16h

	cmp al, 32
	jne nopee
	call droppp

	nopee:
	cmp al, 97
	jge ToUpper
	jmp AlreadyUpper
	ToUpper:
	sub ax, 32
	AlreadyUpper:

	cmp al, 65
	jne NotA

	; Is A
	push -1
	push 0
	call CanObjMov

	cmp ax, 1
	jne NotA

	push 1
	push -1
	jmp Move

	NotA:

	cmp al, 68
	jne NotD

	; Is D
	DDDD:
	push 1
	push 0
	call CanObjMov
	cmp ax, 1
	jne NoNeedToMov

	push 0
	push 1
	jmp Move

	NotD:

	cmp al, 82
	jne NotR
	; Is R

	jmp startt

	NotR:

	cmp al, 83
	jne NotW

	; Is S

	
	push ax
	mov ax, [FallRate]
	mov [rfallrate], ax
	pop ax

	mov [FallRate], 1
	jmp NoNeedToMov

	NotW:

	cmp al, 87
	jne NotS

	; Is w
	call GetNextRotation
	mov dx, [ShapeRotation]
	mov [ShapeRotation], ax

	call CanDrawShapeWxy
	cmp ax, 0
	je RestoreRotation

	call DeleteFallingShape
	call DrawShapeWxy
	jmp NotS
	
	RestoreRotation:
	mov [ShapeRotation], dx
	jmp NoNeedToMov

	NotS:
	jmp NoNeedToMov

	Move:
	push 0
	call MovObj

	NoNeedToMov:

	pop bx
	ret 
	
endp HandleKeys

proc GetNextRotation

	push dx
	push bx

	xor dx, dx
	mov ax, [ShapeRotation]
	inc ax
	mov bx, 4
	div bx
	mov ax, dx
	
	pop bx
	pop dx

	ret
endp GetNextRotation


proc IsLineFull
	push bp
	mov bp, sp

	push bx
	push cx

	mov bx, [bp+4]

		mov cx, 10
		lineloop:
			push cx
			push bx
			call GetSquare

			and ax, 7Fh
			cmp ax, 0
			je NotFull

			loop lineloop
	
	mov ax, 1
	jmp returnv


	NotFull:
	xor ax, ax


	returnv:
	pop cx
	pop bx
	pop bp

	ret 2
endp IsLineFull


proc DropLine
	push bp
	mov bp, sp

	push bx
	push cx
	push dx
	
	mov bx, [bp+4] ;y

	mov cx, 10 ;x
	ClearLoop:
		push cx
		push bx
		xor ax, ax
		push ax
		call SetSquare

		loop ClearLoop
	
	mov cx, 10
	SetRow:
		mov bx, [bp+4] ;y
		dec bx
		SetCol:
			push cx
			push bx
			call GetSquare

			push cx
			push bx
			mov ah, 80h
			push ax
			call SetSquare

		dec bx
		cmp bx, 0
		jne SetCol

	loop SetRow

	push 0
	push 0
	push 1
	call MovObj



	pop dx
	pop cx
	pop bx
	pop bp


	inc [score]
	call scoretostring
	call PRINT

	call makefallfaster

	call Highscoreupdate
	call highScoreToString
	call PRINThighScore


	ret 2
endp DropLine


proc DropAllLines
	push cx
	push bx

	mov bx, 1
	BordLoop:
	
		push bx
		call IsLineFull
		cmp ax, 1
		jne Nope

		push bx
		call DropLine
		inc cx

		Nope:
		inc bx
		cmp bx, 20
		jle BordLoop
	call MakeBooardNotFall

	pop bx
	pop cx
	ret
endp DropAllLines

proc MakeSquareNotFall
	push bp
	mov bp, sp

	push bx
	push cx

	mov bx, [bp+6]
	mov cx, [bp+4]

	push bx
	push cx
	call GetSquare

	mov ah, 0
	push bx
	push cx
	push ax
	call SetSquare

	pop cx
	pop bx
	pop bp

	ret 4
endp MakeSquareNotFall


proc MakeBooardNotFall
	push cx
	push dx

	mov dx, 20 ;y
	Ncol:
		mov cx, 10 ;x
		Nrow:

		  push cx
		  push dx
		  call MakeSquareNotFall

		loop Nrow
		dec dx
		cmp dx, 0
		jne Ncol
	
	pop dx
	pop cx
	ret
endp MakeBooardNotFall


proc MakeBooardB
	push cx
	push dx

	mov dx, 20 ;y
	BBcol:
		mov cx, 10 ;x
		BBrow:

		  push cx
		  push dx

		  xor ax, ax
		  push ax
		  call SetSquare

		loop BBrow
		dec dx
		cmp dx, 0
		jne BBcol
	
	pop dx
	pop cx
	ret
endp MakeBooardB

; GetShapeSquare(x, y, shape address)
proc GetShapeSquare
	push bp
	mov bp, sp

	push bx
	
	mov ax, [bp+6]
	mov bh, 4
	dec ax
	mul bh
	add ax, [bp+8]
	dec ax

	mov bx, ax
	add bx, [bp+4]
	mov al, [BYTE PTR bx]
	mov ah, al
	and al, 7Fh
	mov ah, 80h
	
	pop bx

	pop bp
	ret 6
endp GetShapeSquare




proc prg
    push dx
    xor dx, dx

    mov ax, [NextRandom]
    mov dx, 25173
    imul dx

    add  ax, 13849
    xor  ax, 62832
    mov  [NextRandom], ax

    pop dx
    ret
endp prg




proc DrawRandomShape
	push bx
	push cx
	push dx
	

	mov ah, 2ch
	int 21

	call prg
	mov ax, [NextRandom]


    mov dl, dh

	add ax, dx
	inc ax
	xor dx, dx
	mov cx, 7
	div cx

	mov [ShapeType], dx

	call CanDrawShapeWxy
	cmp ax, 0
	je Cant


	call DrawShapeWxy
	mov ax, 1
	jmp RR

	Cant:
	xor ax, ax
	RR:
	pop dx
	pop cx
	pop bx
	ret
endp DrawRandomShape


proc CanDrawShapeWxy
	push bp
	mov bp, sp

	push bx
	push cx
	push dx

	mov bx, [ShapeType]
	mov ax, 64
	mul bx
	mov bx, ax
	add bx, offset Shape11
	
	mov ax, [ShapeRotation]
	push cx
	mov cx, 16
	mul cx
	pop cx
	add bx, ax

	mov dx, 4
	ShapeColy:
		mov cx, 4
		ShapeRowx:

			push cx
			push dx
			push bx
			call GetShapeSquare

			cmp al, 0
			je IsEmptyOrFalling
			
			add cx, [Shapex] ;x
			add dx, [Shapey] ;y
			push cx
			push dx
			sub cx, [Shapex]
			sub dx, [Shapey]
			call GetSquare

			cmp ax, 0
			je IsEmptyOrFalling

			cmp ah, 80h
			je IsEmptyOrFalling

			jmp ObjCantMov
			
			IsEmptyOrFalling:
		loop ShapeRowx
		dec dx
		cmp dx, 0
	jne ShapeColy

	mov ax, 1
	jmp R

	ObjCantMov:
	xor ax, ax


	R:

	pop dx
	pop cx
	pop bx
	pop bp

	ret
endp CanDrawShapeWxy


proc DrawShapeWxy
	push bp
	mov bp, sp

	push bx
	push cx
	push dx

	mov bx, [ShapeType]
	mov ax, 64
	mul bx
	mov bx, ax
	add bx, offset Shape11

	mov ax, [ShapeRotation]
	push cx
	mov cx, 16
	mul cx
	pop cx
	add bx, ax


	mov dx, 4
	ShapeColyy:
		mov cx, 4
		ShapeRowxx:
			push cx
			push dx
			push bx
			call GetShapeSquare
			cmp al, 0
			je DontSett
			
			add cx, [Shapex] ;x
			add dx, [Shapey] ;y
			push cx
			push dx
			push ax
			call SetSquare
			
			sub cx, [Shapex]
			sub dx, [Shapey]

			DontSett:
		loop ShapeRowxx
		dec dx
		cmp dx, 0
	jne ShapeColyy

	call DrawBoard

	pop dx
	pop cx
	pop bx
	pop bp

	ret
endp DrawShapeWxy

proc DeleteFallingShapeSquare
	push bp
	mov bp, sp

	push bx
	push cx

	mov bx, [bp+6]
	mov cx, [bp+4]

	push bx
	push cx
	call GetSquare

    cmp ah, 80h
	jne DoNotDelete
	mov ax, 0
	push bx
	push cx
	push ax
	call SetSquare

    DoNotDelete:
	pop cx
	pop bx
	pop bp

	ret 4
endp DeleteFallingShapeSquare

proc DeleteFallingShape
	push cx
	push dx

	mov dx, 20 ;y
	DFNcol:
		mov cx, 10 ;x
		DFNrow:

		  push cx
		  push dx
		  call DeleteFallingShapeSquare

		loop DFNrow
		dec dx
		cmp dx, 0
		jne DFNcol
	
	pop dx
	pop cx
	ret
endp DeleteFallingShape




proc PRINT
	push bp
	mov bp, sp

	push bx
	push cx
	push dx



	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 6
	mov dl, 0
	mov dh, 2
	mov bp, offset ScoreAsAString
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es



	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 5
	mov dl, 7
	mov dh, 2
	mov bp, offset scorestring
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es



	pop dx
	pop cx
	pop bx
	pop bp
	ret
endp PRINT



proc ScoreToString
	push bx
	push cx
	push dx

	mov ax, [score]
    
    ;initialize count
    mov cx,4
    mov dx,0
    stringlabel:
        ; if ax is zero
        ;cmp ax,0
        ;jne isnziro
		;mov dx, 0
		;jmp next
		;isnziro:

        mov bx,10

        ; extract the last digit
		mov dx, 0
        div bx

		next:
		add dx, 48

        mov bx, offset scorestring
		add bx, cx



        mov [ds:bx], dl
		

        ;increment the count
		loop stringlabel
 
		DONE:	
	pop dx
	pop cx
	pop bx
	ret
endp ScoreToString


proc PrintInstructions
	push bp
	mov bp, sp

	push bx
	push cx
	push dx



	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 7
	mov dl, 0
	mov dh, 19
	mov bp, offset moveright
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 6
	mov dl, 0
	mov dh, 20
	mov bp, offset moveleft
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 8
	mov dl, 0
	mov dh, 21
	mov bp, offset turn
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 6
	mov dl, 0
	mov dh, 22
	mov bp, offset fast
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es
	
	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 10
	mov dl, 0
	mov dh, 23
	mov bp, offset fall
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es



	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 10
	mov dl, 0
	mov dh, 24
	mov bp, offset newgame
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es

	pop dx
	pop cx
	pop bx
	pop bp

ret
endp PrintInstructions




proc makefallfaster
	push bx
	push cx
	push dx

	mov ax, [score]
	mov cx, 10
	mov dx, 0
	div cx
	mov cx, 6
	sub cx, ax
	cmp cx, 2
	js Donotchange

	mov [rfallrate], cx


	Donotchange:
	pop dx
	pop cx
	pop bx
	ret
endp makefallfaster



proc resetnextrandom
	mov ax, [NextRandom]
	push ax
	call prg
	pop ax
	xor ax, [NextRandom]
	ret
endp resetnextrandom



proc Highscoreupdate
	mov ax, [score]
	cmp [highscore], ax
	jge doesnotwin

	mov ax, [score]
	mov [highscore], ax

	doesnotwin:
	ret
endp Highscoreupdate



proc highScoreToString
	push bx
	push cx
	push dx

	mov ax, [highscore]
    
    ;initialize count
    mov cx,4
    mov dx,0
    highscorestringlabel:
        ; if ax is zero
        ;cmp ax,0
        ;jne isnziro
		;mov dx, 0
		;jmp next
		;isnziro:

        mov bx,10

        ; extract the last digit
		mov dx, 0
        div bx

		nnext:
		add dx, 48

        mov bx, offset highscorestring
		add bx, cx



        mov [ds:bx], dl
		

        ;increment the count
		loop highscorestringlabel
 
		highscoreDONE:	
	pop dx
	pop cx
	pop bx
	ret
endp highScoreToString



proc PRINThighScore
	push bp
	mov bp, sp

	push bx
	push cx
	push dx



	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 11
	mov dl, 0
	mov dh, 1
	mov bp, offset highScoreAsAString
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es



	mov al, 1
	mov bh, 0
	mov bl, [color]
	mov cx, 5
	mov dl, 12
	mov dh, 1
	mov bp, offset highscorestring
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es



	pop dx
	pop cx
	pop bx
	pop bp
	ret
endp PRINThighScore



proc LoseInRed
	push bp
	mov bp, sp

	push bx
	push cx
	push dx



	mov al, 1
	mov bh, 0
	mov bl, 4
	mov cx, 5
	mov dl, 5
	mov dh, 8
	mov bp, offset loststring
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	mov al, 1
	mov bh, 0
	mov bl, 4
	mov cx, 5
	mov dl, 30
	mov dh, 8
	mov bp, offset loststring
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	pop dx
	pop cx
	pop bx

	pop bp
	ret
endp LoseInRed



proc LoseInBlack
	push bp
	mov bp, sp

	push bx
	push cx
	push dx



	mov al, 1
	mov bh, 0
	mov bl, 0
	mov cx, 5
	mov dl, 5
	mov dh, 8
	mov bp, offset loststring
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	mov al, 1
	mov bh, 0
	mov bl, 0
	mov cx, 5
	mov dl, 30
	mov dh, 8
	mov bp, offset loststring
	push es
	push ds
	pop es

	mov ah, 13h
	int 10h
	pop es


	pop dx
	pop cx
	pop bx
	
	pop bp

	ret
endp LoseInBlack


start:	
    mov ax, @data
    mov ds, ax
	mov ax, 0A000h
	mov es, ax
	
	mov ax, 13h
	int 10h


	startt:
	call MakeBooardB
	mov [score], 0
	mov [FallRate], 6
	mov [rFallRate], 6

	call Highscoreupdate
	call highScoreToString
	call PRINThighScore

	call resetnextrandom


	call PrintInstructions

	call ScoreToString
	call PRINT
	
	call LoseInBlack

	
    AnotherShape:

	push ax
	mov ax, [rfallrate]
	mov [FallRate], ax
	pop ax

	mov [Shapex], 3
	mov [Shapey], 1
	mov [ShapeRotation], 0

	call DrawRandomShape
	cmp ax, 0
	je lose

	call DrawBoard

	Tick:
		call Delay
		push 0
		push 1
		call CanObjMov
		cmp ax, 1
		jne NearAnotherShape
		push 0
		push 0
		push 1
		call MovObj
		call DrawBoard
	jmp Tick
	
	NearAnotherShape:
	call MakeBooardNotFall
	call DropAllLines
	jmp AnotherShape


	lose:
	call Highscoreupdate
	call highScoreToString
	call PRINThighScore
	
	call LoseInRed

	loseloop:
	call HandleKeys
	jmp loseloop




Exit:
    mov ax, 4C00h
    int 21h
END start