#include "zx81defs.asm" ;; https://www.sinclairzxworld.com/viewtopic.php?t=2186&start=40
;EQUs for ROM routines
#include "zx81rom.asm"
;ZX81 char codes/how to survive without ASCII
#include "charcodes.asm"
;system variables
#include "zx81sys.asm"

;the standard REM statement that will contain our 'hex' code
#include "line1.asm"


	jp start
	
m1_mem
	DEFB 0,0
m2_mem
	DEFB 0,0
answer_mem
	DEFB 0,0
to_print_mem
	DEFB 0,0
mainLoopCount_mem
	DEFB 0
mainCLSCountDown_mem
	DEFB 0

mainLoopCount .equ mainLoopCount_mem
m1 		.equ m1_mem ;use in sum128
m2 		.equ m2_mem ;use in sum128
answer 	.equ answer_mem ;use in sum128
to_print .equ to_print_mem ;use hprint128

hprint16  ; print one 2byte number stored in location $to_print modified from hprint http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	;ld hl,$to_print
	ld hl,$to_print+$01	
	ld b,2	
hprint16_loop	
	ld a, (hl)
	push af ;store the original value of a for later
	and $f0 ; isolate the first digit
	rra
	rra
	rra
	rra
	add a,$1c ; add 28 to the character code
	call PRINT ;
	pop af ; retrieve original value of a
	and $0f ; isolate the second digit
	add a,$1c ; add 28 to the character code
	call PRINT
	ld a, 00;_NL ;print new line ; 00 is space
	;call PRINT ; print a space character
	
	dec hl
	djnz hprint16_loop
	; restore registers
	ld a, _NL
	call PRINT
	ret	
	
;; multiply DE and BC
;; DE is equivalent to the number in the top row in our algorithm
;; and BC is equivalent to the number in the bottom row in our algorithm
multiply16
    ld a,16     ; this is the number of bits of the number to process
    ld hl,0     ; HL is updated with the partial result, and at the end it will hold 
                ; the final result.
mul_loop
    srl b
    rr c        ;; divide BC by 2 and shifting the state of bit 0 into the carry
                ;; if carry = 0, then state of bit 0 was 0, (the rightmost digit was 0) 
                ;; if carry = 1, then state of bit 1 was 1. (the rightmost digit was 1)
                ;; if rightmost digit was 0, then the result would be 0, and we do the add.
                ;; if rightmost digit was 1, then the result is DE and we do the add.
    jr nc,no_add	

    ;; will get to here if carry = 1        
    add hl,de   

no_add
    ;; at this point BC has already been divided by 2

    ex de,hl    ;; swap DE and HL
    add hl,hl   ;; multiply DE by 2
    ex de,hl    ;; swap DE and HL

    ;; at this point DE has been multiplied by 2
    
    dec a
    jr nz,mul_loop  ;; process more bits

	ret
	
zero16				; zero 16bits starting from hl,
					; b and hl clobbered by this
	ld b,2			; set loop control variable for next loop
zero16_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero16_loop		; jump if loop control variable not zero
	ret


	
start

	call CLS	

	ld hl, m1		
	ld (hl),1
	
	ld hl, m2		
	ld (hl),3		

	ld hl,mainLoopCount  ; number of lines per screen to display
	ld a,18
	ld (hl),a

	ld hl,mainCLSCountDown_mem  ;; limit the number of screen clears 
	ld a,1
	ld (hl),a
	
mainloop		
	ld de, (m1)
	ld bc, (m2)		
	call multiply16  	; de and bc contain numbers to mulitply, hl contains result

	ld (answer), hl
	ld hl, answer
	ld de, to_print		
	ld bc, 2
	ldir	
	
	call hprint16 		; print a 16 bit number as hex

	ld hl, (m1)
	inc hl				; add one, we're printing times tables here
	ld (m1), hl
	
	ld hl,mainLoopCount
	ld a,(hl)	
	dec a
	jp nz, skip
		
	ld hl,mainLoopCount
	ld a,18
	ld (hl),a	

	ld hl,mainCLSCountDown_mem
	ld a, (hl)
	dec a
	jp z, endPROG
	
	call CLS		
	jp mainloop
	
skip
	;ld hl,mainLoopCount
	ld (hl),a			; save a back to (hl) (mainLoopCount)
	
	jp mainloop	
	
endPROG
	ret





;include our variables
#include "vars.asm"

; ===========================================================
; code ends
; ===========================================================
;end the REM line and put in the RAND USR line to call our 'hex code'
#include "line2.asm"

;display file defintion
#include "screen.asm"               

;close out the basic program
#include "endbasic.asm"

