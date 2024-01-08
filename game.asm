; Program: 16-bit x86 Pong
; Developer: [My Name]
; Language: 16 bit x86 assembly NASM syntax
;
; Description: This is a bare-metal pong program targeting x86 and amd64 processors. It has no supporting software other than itself.
;
; TODO:
; 1. Optimize, refine, and document everything.

;
; BOOT
; This section contains the bootloader, which loads the program's binary from the disk on startup.
;
BITS 16             ; Tell NASM that this is a 16 bit program
org 0x7c00          ; Set the starting point for the program, which will be RAM address 0x7c00

Boot:
    ; Clear all the segment registers
    mov ax, 0
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov fs, ax
    mov gs, ax

    ; Set the starting point of the stack
    mov sp, 0x7c00

    ; Load a whole bunch of sectors, up to 9.2kb
    mov ah, 0x02    ; int 0x13 read disk sectors function
    mov al, 18      ; Number of sectors to read
    mov ch, 0       ; Cylinder number
    mov cl, 2       ; Sector number (the sector we start reading at)
    mov dh, 0       ; Head number
    or dl, dl       ; Drive number. This instruction doesn't need to exist because it just assures that the BIOS put the disk ID into dl.
    mov bx, 0x7e00  ; Where the read data begins in memory
    int 0x13        ; BIOS Disk Service
    
    cld             ; Clear the direction flag

    ; Print the boot message
    mov si, msg     ; Move the pointer to the boot message into si
    mov ah, 0x0e    ; BIOS int 0x10 write letter function
    mov bh, 0       ; I forgot what this does lol
.loop:
    ; Load the message
    lodsb           ; Load the current character si points to into the next position
    cmp al, 0       ; Have we reached a null terminator?
    je .done        ; If so, we're done
    int 0x10        ; BIOS video service
    jmp .loop       ; Repeat the loop
.done:
    jmp Start       ; Start the game


msg: db "Booting Pong...", 0        ; Boot message

; Make the disk bootable
times 510 - ($-$$) db 0
dw 0xAA55
;
; BOOT
;

;
; MAIN
; This section contains the main function ("Start") and the game loop. This is the hub of the program.
;
Start:
    ; Set the screen to VGA text mode
    call Set_Video_Mode

    ; Set es to point to VRAM for direct manipulation of the screen
    mov ax, 0xB800
    mov es, ax

    ; Move the cursor out of view because the player doesn't need to see it
    mov bh, 255
    mov bl, 255
    call Mov_Cursor

    ; Initially draw everything to the screen
    call Render_Screen

    ; Initialize the keyboard driver
    call Enable_Keyboard

    ; Start the game
    jmp Game_Loop


Wait_Timer:
    ; This function is a timer that does nothing for a few milliseconds so the game doesn't travel at warp speed (Using the PIT is really freaking hard)
    call Check_Keyboard                 ; Check for keyboard input (So that it isn't missed)
    inc word [Wait_Accumulator]         ; Increment the accumulator

    cmp word [Wait_Accumulator], 40000  ; Has the accumulator reached 40,000?
    je .done                            ; If so, we are done waiting

    jmp Wait_Timer                      ; Otherwise, repeat the loop
.done:
    mov word [Wait_Accumulator], 0      ; Reset the timer
    call Check_Keyboard                 ; Check for keyboard input again just in case it was missed
    ret                                 ; Return



;
; Game_Loop: the primary function of the game, where input and output are processed.
;
Game_Loop:
    call Wait_Timer                 ; Delay the execution of the function

    call Check_Keyboard             ; Look for keyboard input


    cmp byte [buffer], 0x20         ; Was the D key (0x20) pressed?
    je Set_Flag_Right               ; If so, move the player right

    cmp byte [buffer], 0x1E         ; Was the A key (0x1E) pressed?
    je Set_Flag_Left                ; Make the player move left if so

.continue:    ; After keyboard events are processed, we continue from here
    call Render_Screen              ; Re-render the screen

    cmp byte [PlayerPoints], 6      ; Has the player reached 6 points?
    je Player_Wins                  ; If so, the player wins

    cmp byte [ComputerPoints], 6    ; Has the computer reached 6 points
    je Computer_Wins                ; If so, the computer wins

    mov byte [LeftRightFlag], 0     ; Reset the movement flag to 0
    mov ax, [BallXSpeed]            ; Move the speed of the ball into ax
    add word [BallX], ax            ; Increase the X value of the ball by the ball's X speed (if it's negative, it decreases)

    mov ax, [BallYSpeed]            ; Put the ball's Y speed into ax
    add word [BallY], ax            ; Increase the Y value based on the ball's Y speed (Once again, if negative, it decreases)

    call Detect_Collision           ; Detect collisions
    
    jmp Game_Loop                   ; Continue the loop

Player_Wins:
    ; This function is called when the player wins the game
    call Clear_Screen       ; Clear the screen

    ; Move the cursor to the center of the screen
    mov bh, 34
    mov bl, 12
    call Mov_Cursor

    mov si, PlayerWinMsg    ; Point to the win message with si
    call Print              ; Print the message

    ; Move the cursor off the screen
    mov bh, 255
    mov bl, 255
    call Mov_Cursor

    jmp End_Game            ; End the game

Computer_Wins:
    ; This function is called when the computer wins the game
    call Clear_Screen       ; Clear the screen

    ; Move the cursor to the center of the screen
    mov bh, 34
    mov bl, 12
    call Mov_Cursor

    mov si, CompWinMsg      ; Point to the win message with si
    call Print              ; Print the win message

    ; Move the cursor off the screen
    mov bh, 255
    mov bl, 255
    call Mov_Cursor

    jmp End_Game            ; End the game

End_Game:
    ; This function is just an infinite loop that halts the processor after the game is complete
    jmp $
    jmp End_Game

;
; MAIN
;

;
; RNG
;

next: dw 0

; This function created a psuedo-random number. It is very rudimentary.
; OUT: random number in ax
Get_Random_Numer:
    mov ax, [next]
    mov bx, 1234
    mul bx
    mov word [next], ax
    add ax, 12345
    mov bx, 65535
    div bx
    mov ax, dx

    ret
;
; RNG
;
    
;
; KEYBOARD
;

;
; Enable_Keyboard: This function enables the CPU port that accesses keyboard input
;
Enable_Keyboard:
    mov al, 0xAE                ; Put a keyboard access code into al
    out KBD_STATUS_PORT, al     ; Send the value in AL out of the CPU's keyboard status port
    ret                         ; Return

;
; Check_Keyboard: This function checks if there was any input from the keyboard (which will be in al)
;
Check_Keyboard:
    ; Check if there is data in the keyboard buffer
    in al, KBD_STATUS_PORT              ; Put the status of the port that checks if a key was pressed into al
    test al, KBD_STATUS_OUTPUT_FULL     ; Did the port say anything was pressed?
    jz .no_data                         ; If not, exit

    ; Read the key from the keyboard buffer
    in al, KBD_DATA_PORT                ; Put the data stored in the keyboard into al by activating the data port
    mov [buffer], al                    ; Put that keyboard data into the buffer
    mov al, 1                           ; This is a flag indicating that there was data entered
    ret                                 ; Return
.no_data:
    xor al, al                          ; Flag is 0, no data was entered
    ret                                 ; Return
;
; KEYBOARD
;

; 
; COLLISIONS
; 

Detect_Collision:
    ; This function is the Ball's collision logic
    cmp word [BallY], 1                 ; Is the Y value of the ball 1?
    je .Test_Player_Paddle              ; If so, check to see if it intersects with the player's paddle

    cmp word [BallY], 24                ; If not, check if it's equal to 24
    je .Test_Computer_Paddle            ; If it is, check to see if it intersects with the computer's paddle

    cmp word [BallX], 79                ; Is the X value of the ball at the right side of the screen?
    jnl .Hit_Right_Side                 ; If so, process a collision with the right wall

    cmp word [BallX], 0                 ; Is the X value of the ball 0?
    jng .Hit_Left_Side                  ; If so, process a collision with the left wall

    cmp word [BallY], 25                ; Is the Y value of the ball 25? (Assumung it doesn't collide with a paddle)
    jnl .Hit_Bottom                     ; If so, process a collision with the bottom

    cmp word [BallY], 0                 ; Is the Y value of the ball 0?
    jng .Hit_Top                        ; If so, process a collision with the top

    jmp .done                           ; All collisions have been checked


; Edit the ball's movement based on what was hit
.Hit_Right_Side:
    ; The logic for colliding with the right wall

    ; Keep the ball from going out of bounds
    mov word [BallX], 78
    call Reset_Ball_Pos

    ; Invert the sign of the ball's X speed (BallXSpeeed = -BallXSpeed)
    not word [BallXSpeed]       ; Bitwise NOT operation, inverts all bits
    inc word [BallXSpeed]       ; Increment the speed by 1, inverting its two's compliment sign
    jmp .done                   ; Collision processed

.Hit_Left_Side:
    ; The logic for collising with the left wall

    ; Move the ball to the correct position
    mov word [BallX], 0
    call Reset_Ball_Pos

    ; Invert the sign of the ball's X speed
    not word [BallXSpeed]
    inc word [BallXSpeed]
    jmp .done                   ; Collision processed

.Hit_Bottom:
    ; The logic for colliding with the bottom of the screen

    ; Move the ball to the center of the screen
    mov word [BallY], 12
    mov word [BallX], 39
    call Reset_Ball_Pos

    inc byte [PlayerPoints]     ; Increase the player's points (since the ball it the computer's goal).

    ; Invert the sign of the ball's Y speed
    not word [BallYSpeed]
    inc word [BallYSpeed]
    jmp .done                   ; Collision processed

.Hit_Top:
    ; The logic for colliding with the top of the screen

    ; Move the ball back to the center
    mov word [BallY], 12
    mov word [BallX], 39
    call Reset_Ball_Pos

    inc byte [ComputerPoints]   ; Increase the computer's points (since the ball hit the player's goal)

    ; Invert the sign of the ball's Y speed
    not word [BallYSpeed]
    inc word [BallYSpeed]
    jmp .done                   ; Collision processed

.Test_Player_Paddle:
    ; Check if the ball is collising with the player's paddle
    mov ax, [Playerxval]        ; Put the current X value (left side) of the player's paddle into ax
    cmp word [BallX], ax        ; Is the ball's X value the same as the paddle's X value?
    jl .done                    ; If the ball's X value is farther to the left than the player's, there was no collision
    add ax, 10                  ; Add 10 to ax, which makes it 10 points to the right of the player's X value (the right side of the paddle)
    cmp word [BallX], ax        ; Is the ball's X value the same as the right side of the player's paddle?
    jg .done                    ; If the ball's position is farther to the right than that, there was no collision

    jmp .Hit_Player_Paddle      ; If we make it here, then there was a collision. Process it.
    
.Hit_Player_Paddle:
    ; The logic for colliding with the player's paddle

    ; Put the ball in the correct spot
    mov word [BallY], 1
    call Reset_Ball_Pos

    ; Invert the sign of the ball's Y speed
    not word [BallYSpeed]
    inc word [BallYSpeed]

    jmp .done                   ; Collision processed

.Test_Computer_Paddle:
    ; Check if the ball is colliding with the computer's paddle
    mov ax, [Computerxval]      ; Put the computer's X value into ax
    cmp word [BallX], ax        ; Is the ball's X value the same as the paddle's X value?
    jl .done                    ; If the ball's X value is farther to the left than the computer's, there was no collision
    add ax, 10                  ; Add 10 to ax, which makes it 10 points to the right of the computer's X value (the right side of the paddle)
    cmp word [BallX], ax        ; Is the ball's X value the same as the right side of the computer's paddle?
    jg .done

    jmp .Hit_Computer_Paddle    ; If we make it here, then there was a collision. Process it.

.Hit_Computer_Paddle:
    ; The logic for colliding with the computer's paddle

    ; Put the ball in the correct spot
    mov word [BallY], 24
    call Reset_Ball_Pos

    ; Invert the sign of the ball's Y speed
    not word [BallYSpeed]
    inc word [BallYSpeed]

    jmp .done                   ; Collision processed

.done:
    ret                         ; All collisions processed, return

;
; COLLISIONS
;


;
; RENDERING
;
Render_Screen:
    ; This function calls all the other rendering functions so that they don't have to be retyped one by one
    call Clear_Screen
    call Move_Player_Paddle
    call Reset_Ball_Pos
    call Draw_Scores
    call Draw_Net
    call Move_Computer_Paddle

    cmp word [WrongMoveCounter], 5
    je .get_mistake
    
    ret

.get_mistake:
    call Get_Random_Numer
    cmp ax, 6554
    jnl .normal
    jl .mistake

.mistake:
    mov word [MistakeMade], 1
    cmp word [WrongMoveCounter], 11
    jl Make_Mistake
    mov word [MistakeMade], 0
    jmp No_Mistake
    ret

.normal:
    cmp word [MistakeMade], 1
    je Make_Mistake
    jmp No_Mistake
    ret


Draw_Net:
    ; This function draws the net in the center of the screen.

    ; The location to start printing from
    mov word [DrawXVal], 0      ; Starting X value
    mov word [DrawYVal], 12     ; Y value
    
    mov al, '-'                 ; Put a dash into al, which is what the net will be
    mov dx, 80                  ; Repeat 80 times
    call Draw_To_Screen         ; Draw the given character at the location with the amount of times in dx
    ret                         ; Return

Draw_Scores:
    ; This function draws the scores of the player and the computer, above and below the net, respectively

    ; Draw the player's score, just above the right side of the net
    mov word [DrawXVal], 79
    mov word [DrawYVal], 11

    ; Make the player's points value become a string by adding ASCII 0 to it and then put it on the screen
    mov al, [PlayerPoints]      ; Set the character to draw
    add al, '0'                 ; Turn into ASCII
    mov dx, 1                   ; Draw only once
    call Draw_To_Screen         ; Draw to the screen

    ; Draw the computer's score, just below the right side of the net
    mov word [DrawXVal], 79
    mov word [DrawYVal], 13

    mov al, [ComputerPoints]    ; Set the character to draw
    add al, '0'                 ; Turn it into ASCII
    mov dx, 1                   ; Draw once
    call Draw_To_Screen         ; Draw to the screen


    ret                         ; Return

Reset_Ball_Pos:
    ; This function renders the ball to the screen
    mov ax, [BallX]             ; Put the ball's current X value into ax
    mov word [DrawXVal], ax     ; So it can be moved into the drawing X value variable
    
    mov ax, [BallY]             ; Do the same thing
    mov word [DrawYVal], ax     ; With the ball's Y position

    mov dx, 2                   ; Draw twice

    mov al, 219                 ; Solid block in ASCII

    call Draw_To_Screen         ; Draw to the screen

    ret                         ; Return

;
; IN: None, activated when a or d are pressed
; OUT: Movement of the player's paddle on the screen
;
Move_Player_Paddle:
    cmp byte [LeftRightFlag], 1     ; Are we moving left?
    je .mov_left                    ; If so, do the logic for it
    
    cmp byte [LeftRightFlag], 2     ; Are we moving right?
    je .mov_right                   ; If so, process it

.continue:
    mov ax, [Playerxval]            ; Move the player's X value into register ax
    mov word [DrawXVal], ax         ; So that it can be sent to the drawing value
    
    mov word [DrawYVal], 0          ; The Y value will always be 0

    mov dx, 10                      ; Draw 10 times
    mov al, 219                     ; ASCII solid block

    call Draw_To_Screen             ; Draw it to the screen

    ret                             ; Return

.mov_left:
    ; Decrease the player's X value if moving left
    dec word [Playerxval]
    jmp .continue
.mov_right:
    ; Increase the player's X value if moving right
    inc word [Playerxval]
    jmp .continue

;
; Set the player direction flag
;
Set_Flag_Left:
    ; If the player is moving left, set the flag for it
    xor al, al                      ; Clear al
    mov al, [Playerxval]            ; Move the player's X value into al
    cmp al, 0                       ; Is al 0?
    je Game_Loop.continue           ; If so, do not set the flag, because the player has hit the left side of the screen
    mov byte [LeftRightFlag], 1     ; Otherwise, set the flag to move the player left
    jmp Game_Loop.continue          ; Continue the game
Set_Flag_Right:
    ; If the player is moving right, set the flag for it
    xor al, al                      ; Clear al
    mov al, [Playerxval]            ; Move the player's X value into al
    cmp al, 70                      ; Is al 70?
    je Game_Loop.continue           ; If so, the player is colliding with the right side, so do not set the flag.
    mov byte [LeftRightFlag], 2     ; Otherwise, set the flag to indicate that the player is moving right
    jmp Game_Loop.continue          ; Continue the game
Set_Flag_None:
    ; If the player is not moving, set the flag as such
    xor al, al                      ; Clear al
    mov byte [LeftRightFlag], 0     ; Set the flag to 0
    jmp Game_Loop.continue          ; Continue the game



Make_Mistake:
    inc word [WrongMoveCounter]
    ; Random number generator said the computer should make a mistake
    mov ax, [BallX]                 ; Move the ball's X value into ax
    sub ax, 5                       ; Subtract 5 from that value to make it be detected as over the center of the computer's paddle

    ; Invert the sign of the ball's X position to make it inaccurate
    not ax
    inc ax
    
    cmp word [Computerxval], ax     ; Is the new ball X value equal to the computer's X value?
    jg Mov_Computer_Left                    ; If the computer's X value is greater, move left

    ; Otherwise, move right
    cmp word [Computerxval], ax
    jl Mov_Computer_Right

No_Mistake:
    mov ax, [BallX]                 ; Move the ball's X value into ax
    sub ax, 5                       ; Subtract 5 from that value to make it be detected as over the center of the computer's paddle
    cmp word [Computerxval], ax     ; Is the new ball X value equal to the computer's X value?
    jg Mov_Computer_Left                   ; If the computer's X value is greater, move left

    ; Otherwise, move right
    cmp word [Computerxval], ax
    jl Mov_Computer_Right

Move_Computer_Paddle:
    mov ax, [Computerxval]          ; Put the computer's X value into ax
    mov word [DrawXVal], ax         ; So that it can be sent to the draw function's X value
    
    mov word [DrawYVal], 24         ; The computer's paddle will always be at the bottom of the screen

    mov dx, 10                      ; Repeat 10 times
    mov al, 219                     ; ASCII solid block
    call Draw_To_Screen             ; Draw to the screen

    ret                             ; Return

Mov_Computer_Left:
    ; Move the computer paddle left
    cmp word [Computerxval], 0      ; Has the paddle reached the left side?
    je Move_Computer_Paddle         ; If so, do not continue moving
    dec word [Computerxval]         ; Otherwise, move the paddle left by one
    jmp Move_Computer_Paddle        ; Continue executing the function
Mov_Computer_Right:
    ; Move the computer paddle right
    cmp word [Computerxval], 70     ; Has the computer's paddle reached the right side?
    je Move_Computer_Paddle         ; If so, do not continue moving
    inc word [Computerxval]         ; Otherwise, move the paddle right by one
    jmp Move_Computer_Paddle        ; Continue executing the function


; DrawXVal = x value
; DrawYVal = y value
; al = character to draw
; dx = number of repeats
Draw_To_Screen:
    ; Draw anything (as long as it's an ASCII character) to the screen
    push ax                         ; Preserve the value of ax on the stack
    mov word [DrawRepeats], dx      ; Put the number of repetitions into its memory location

    ; Calculate the Y position by multiplying it by 80 then adding it to the X position
    mov ax, [DrawYVal]              ; Move the Y value into ax
    mov bx, 80                      ; Put 80 into bx
    mul bx                          ; Multiply ax by bx, result in ax
    mov bx, [DrawXVal]              ; Move the X value into bx
    add ax, bx                      ; Add it onto the current value in ax and you have your position in X and Y

    mov di, ax                      ; Set the offset register to the new value in ax
    shl di, 1                       ; Multiply it by 2 because we're putting a word and not a byte into memory

    pop ax                          ; Restore the value of ax from the stack

    xor cx, cx                      ; Initialize counter
.draw:
    cmp cx, [DrawRepeats]           ; Has the counter reached the total number of repeats?
    je .done                        ; If so, the loop is complete

    mov ah, 0x0F                    ; Attribute (white text on black background)

    mov [es:di], ax                 ; Write the word (character and attribute) to memory at the pointer location of ex with offfset di
    add di, 2                       ; Move to the next memory location
    inc cx                          ; Increment the counter
    jmp .draw                       ; repeat the loop
.done:
    ret                             ; When done, return

;
; Clears the screen by resetting VRAM
; IN: None
; OUT: Cleared Screen
;
Clear_Screen:
    xor di, di                      ; Clear the offset register
    mov cx, 2000                    ; Move the total size of the screen into cx (that is how many times it will repeat)

    mov ax, 0x0720                  ; Clear value
    rep stosw                       ; Repeatedly store nothing until 2,000 RAM locations have been cleared

    ret                             ; return

;
; RENDERING
;


;
; BIOS
;

;
; Function: MovCursor
; Takes the values in bh and bl and makes the cursor go to that point
; bh = x, bl = y
;
Mov_Cursor:
	mov ah, 02h			; 02h = 10h move cursor function
	mov dh, bl			; Put the y value into the dh register, which is used as y
	mov dl, bh			; Put the x value into the dl register, which is used as x
	mov bh, 0			; Clear bh, which is used as input but is also used in 10h
	int 10h				; BIOS video interrupt
	ret                 ; Return when done


;
; Function: Print
; Takes the value in si and prints it as a string
; In: si, which should be a pointer to a memory location
; Out: Screen text output equivalent to what has been stored at the memory location si points to.
;
Print:
	mov ah, 0x0e		; Use 0x0e for printing, which supports lodsb.
	mov bh, 0
.loop:
	lodsb				; Shift to the next character to the right both in memory and on video
	cmp al, 0			; Are we at the null terminator?
	je .done			; If so, return
	int 10h				; Call BIOS video services to print a new character to the screen
	jmp .loop			; Repeat the loop
.done:
	ret					; Return when done

;
; Function: SetVideoMode
; Set video mode to 80x25 text mode
;
Set_Video_Mode:
	mov ah, 00h			; 00h is set video mode
	mov al, 03h			; 03h is set to VGA text
	int 10h				; Call BIOS video service
    ret					; Return

;
; BIOS
;


;
; DATA
;
WrongMoveCounter: dw 0                  ; How many wrong moves that should be made
MistakeMade: dw 0

Playerxval: dw 35                       ; the X position (left side) of the player's paddle
Computerxval: dw 35                     ; The X position (left side) of the computer's paddle

PlayerPoints: db 0                      ; The total points the player has
ComputerPoints: db 0                    ; The total points the computer has

LeftRightFlag: db 0                     ; 0 is no movement, 1 is left, 2 is right

BallXSpeed: dw 1                        ; The X speed of the ball
BallYSpeed: dw 1                        ; The Y speed of the ball

BallX: dw 39                            ; The X position of the ball
BallY: dw 12                            ; The Y position of the ball

DrawXVal: dw 0                          ; The X value to start drawing at in the draw function
DrawYVal: dw 0                          ; The Y value to start drawing at in the draw function
DrawRepeats: dw 0                       ; The amount of repititions that the drawing function's loop will iterate through

Wait_Accumulator: dw 0                  ; Set the accumulator for the timer

PlayerWinMsg: db "Player Wins!", 0      ; This message is shown on the screen if the player wins
CompWinMsg: db "Computer Wins!", 0      ; This message is shown on the screen if the computer wins

buffer: db 0                            ; Reserve a byte to store any keyboard input


; Constants
KBD_STATUS_PORT: equ 0x64               ; The port to the keyboard from the CPU that reads the status of a key pressed
KBD_DATA_PORT: equ 0x60                 ; The port to the keyboard from the CPU that transfers the keyboard input into the CPU to be used later

KBD_STATUS_OUTPUT_FULL: equ 1           ; If the keyboard has produced an output, its status will equal this
KBD_STATUS_INPUT_FULL:  equ 2           ; If the keyboard has recieved an input, its status will equal this

;
; DATA
;
