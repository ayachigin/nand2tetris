// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

(RESET)        
        @cnt
        M=0
        @SCREEN
        D=A
        @screen
        M=D
(LOOP)
        @color
        M=0
        @KBD
        D=M // any key does not pressed
        @FILL
        D;JEQ

        // any key pressed
        @color
        M=-1
        
(FILL)
        @cnt
        D=M
        @8192 // SCREEN data length
        D=D-A
        @RESET
        D;JEQ

        @color
        D=M
        @screen
        A=M
        M=D

        @cnt
        M=M+1
        @screen
        M=M+1
        
        @LOOP
        0;JMP
        
        
