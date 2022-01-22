IDEAL
MODEL small
STACK 100h
DATASEG

board db 18,18,18,18,18,18,18,18,18,18,18,18, 20 dup(18,0,0,0,0,0,0,0,0,0,0,18), 18,18,18,18,18,18,18,18,18,18,18,18
Shape1 db     0, 0, 0, 0, 4, 4, 4, 0, 0, 4, 0, 0, 0, 0, 0, 0
Shape2 db 	  0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0
Shape3 db 	  0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 0, 0, 0, 0
Shape4 db 	  0, 0, 0, 0, 3, 3, 3, 0, 0, 0, 3, 0, 0, 0, 0, 0
Shape5 db 	  0, 0, 0, 0, 0, 5, 5, 5, 0, 5, 0, 0, 0, 0, 0, 0
Shape6 db 	  0, 0, 0, 0, 6, 6, 0, 0, 0, 6, 6, 0, 0, 0, 0, 0
Shape7 db 	  0, 0, 0, 0, 0, 0, 9, 9, 0, 9, 9, 0, 0, 0, 0, 0

columns db 12
x dw 160
y dw 100
DelayDiv dw 0000h
CurrRandom dw 1234h
FallRate dw 5


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
	
	mov ax, [bp+6] ;4?
	inc ax
	mov bx, 320
	mul bx
	add ax, [bp+8] ;6?
	mov bx, 8
	mul bx
	mov bx, ax

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

	mov dx, 20
	Bcol:
		mov cx, 10
		Brow:
			push cx
			push dx
			call GetSquare
			push cx
			push dx
			push ax
			call DrawSquare
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
  call LeftOrRight

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


proc LeftOrRight
	mov ah, 01h
	int 16h
	jne YesMove
	jmp NoNeedToMov

	YesMove:
	mov ah, 00h
	int 16h

	cmp al, 65
	je AAAA
	cmp al, 97
	je AAAA

	cmp al, 68
	je DDDD
	cmp al, 100
	je DDDD

	cmp al, 86
	je SSSS
	cmp al, 115
	je SSSS

	jmp NoNeedToMov

	AAAA:
	push -1
	push 0
	call CanObjMov

	cmp ax, 1
	jne NoNeedToMov

	push 1
	push -1
	jmp Move

	DDDD:
	push 1
	push 0
	call CanObjMov
	cmp ax, 1
	jne NoNeedToMov

	push 0
	push 1

	Move:
	push 0
	call MovObj

	jmp NoNeedToMov

	SSSS:
	mov [FallRate], 1

	NoNeedToMov:
	ret 
endp LeftOrRight



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

	ret 2
endp DropLine


proc DropAllLines
	push cx
	push bx

	mov cx, 20
	BordLoop:
		mov bx, 21
		sub bx, cx

		push bx
		call IsLineFull
		cmp ax, 1
		jne Nope

		push bx
		call DropLine

		Nope:
		loop BordLoop
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



proc DrawShape
	push bp
	mov bp, sp

	push bx
	push cx
	push dx

	mov bx, [bp+4]
	mov ax, 16
	mul bx
	mov bx, ax
	add bx, offset Shape1

	mov dx, 4
	ShapeCol:
		mov cx, 4
		ShapeRow:
			push cx
			push dx
			push bx
			call GetShapeSquare
			cmp al, 0
			je DontSet
			
			add cx, 3
			push cx
			push dx
			push ax
			call SetSquare
			
			sub cx, 3

			DontSet:
		loop ShapeRow
		dec dx
		cmp dx, 0
	jne ShapeCol

	pop dx
	pop cx
	pop bx
	pop bp
	ret 2
endp DrawShape



proc DrawRandomShape
	push bx
	push cx
	push dx
	

	mov ah, 2ch
	int 21

	mov ax, [CurrRandom]
	rcl [CurrRandom], 1

	xor ax, [CurrRandom]
	xor ax, dx
	mov [CurrRandom], ax

	xor dx, dx
	mov cx, 7
	div cx

	push dx
	call DrawShape

	pop dx
	pop cx
	pop bx
	ret
endp DrawRandomShape



start:	
    mov ax, @data
    mov ds, ax
	mov ax, 0A000h
	mov es, ax
	
	mov ax, 13h
	int 10h


    AnotherShape:
	mov [FallRate], 5
	call DrawRandomShape	

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

Exit:
    mov ax, 4C00h
    int 21h
END start