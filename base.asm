IDEAL
MODEL small
STACK 100h
DATASEG

board db 18,18,18,18,18,18,18,18,18,18,18,18, 20 dup(18,0,0,0,0,0,0,0,0,0,0,18), 18,18,18,18,18,18,18,18,18,18,18,18
columns db 12
x dw 160
y dw 100
DelayDiv dw 0000h


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
  mov bx, 2
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




start:	
    mov ax, @data
    mov ds, ax
	mov ax, 0A000h
	mov es, ax
	
	mov ax, 13h
	int 10h
	
	push 5
	push 1
	mov ah, 80h
	mov al, 4
	push ax
	call SetSquare
	
	push 6
	push 1
	mov ah, 80h
	mov al, 4
	push ax
	call SetSquare
	
	push 5
	push 2
	mov ah, 80h
	mov al, 4
	push ax
	call SetSquare
	
	push 6
	push 2
	mov ah, 80h
	mov al, 4
	push ax
	call SetSquare


	;call CanObjMovR
	;cmp ax, 1
	;jne Exit
	;call MovObjRight
	;call MovObjRight
	;call MovObjRight


	;call DrawBoard

	forever:
	call Delay
	push 0
	push 1
	call CanObjMov
	cmp ax, 1
	jne Exit
	push 1
	push 0
	push 0
	call MovObj
	call DrawBoard
	jmp forever



Exit:
    mov ax, 4C00h
    int 21h
END start