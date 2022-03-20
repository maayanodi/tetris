proc scoretostring
    push bp
	mov bp, sp

	mov ax, [score]
	push bx
	push cx
	push dx
    
    ;initialize count
    mov cx,5
    mov dx,0
    label1:
        ; if ax is zero
        cmp ax,0
        je print1   
         
        ;initialize bx to 10
        mov bx,10       
         
        ; extract the last digit
        div bx

		add dx, 48

		push bx
        mov bx, 5
		sub bx, cx
		add bx, offset string


        mov [es:bx], dx
		
		pop bx
        ;increment the count
        dec cx
         
        ;set dx to 0
        xor dx,dx
        jmp label1
    print1:
        ;check if count
        ;is greater than zero
        cmp cx, 0
        je DONE

		push bx
        mov bx, 5
		sub bx, cx
		add bx, offset string
        
		mov [es:bx], 48

		pop bx
        
         
        ;decrease the count
        dec cx
        jmp print1

		DONE:












Instructions db "Instructions:"
moveright db "right --> D"
moveleft db "left --> A"
turn db "turn --> W"
speed db "increasespeed --> S"
fall db "fall instantly--> SPACEBAR"
newgame db "To start a new game, press R"


