IDEAL
MODEL small
STACK 100h
DATASEG

board db 18,18,18,18,18,18,18,18,18,18,18,18, 20 dup(18,3,0,0,1,0,0,0,0,0,4,18), 18,18,18,18,18,18,18,18,18,18,18,18
columns db 12
x dw 160
y dw 100


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



proc DrawSquare
	push bp
	mov bp, sp

	push bx
	push cx
	push dx
	
	mov ax, [bp+6]
	mov bx, 320
	mul bx
	add ax, [bp+8]
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



start:	
    mov ax, @data
    mov ds, ax
	mov ax, 0A000h
	mov es, ax
	
	mov ax, 13h
	int 10h
		
	call DrawBoard
	
Exit:
    mov ax, 4C00h
    int 21h
END start
