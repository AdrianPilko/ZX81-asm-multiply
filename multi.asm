;; zx81 -fibonacci 

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
	
s1_mem
	DEFB 0,0
s2_mem
	DEFB 0,0
sumof_mem
	DEFB 0,0
to_print_mem
	DEFB 0,0
mainLoopCount_mem
	DEFB 0
mainCLSCountDown_mem
	DEFB 0

mainLoopCount .equ mainLoopCount_mem
s1 		.equ s1_mem ;use in sum128
s2 		.equ s2_mem ;use in sum128
sumof 	.equ sumof_mem ;use in sum128
to_print .equ to_print_mem ;use hprint128


hprint 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	PUSH AF ;store the original value of A for later
	AND $F0 ; isolate the first digit
	RRA
	RRA
	RRA
	RRA
	ADD A,$1C ; add 28 to the character code
	CALL PRINT ;
	POP AF ; retrieve original value of A
	AND $0F ; isolate the second digit
	ADD A,$1C ; add 28 to the character code
	CALL PRINT
	LD A,_NL
	CALL PRINT ; print a space character
	RET

hprint16  ; print one 2byte number stored in location $to_print
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
	
hprint128  ; print one 16byte number stored in location $to_print
	;ld hl,$to_print
	ld hl,$to_print+$0f	
	ld b,16	
hprint128_loop	
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
	djnz hprint128_loop
	; restore registers
	ld a, _NL
	call PRINT
	ret
	
hprint256  ; print one 32byte number stored in location $to_print
	;ld hl,$to_print
	ld hl,$to_print+31	
	ld b,32	
hprint256_loop	
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
	djnz hprint256_loop
	; restore registers
	ld a, _NL
	call PRINT
	
	ret	

hprint512  ; print one 64byte number stored in location $to_print
		   ; this takes 8 lines on zx81 screen
	ld hl,$to_print+63
	ld b,64	
hprint512_loop	
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
	djnz hprint512_loop
	; restore registers
	ld a, _NL
	call PRINT
	ld a, _NL
	call PRINT	
	ret		
	
sum128 ;; add two 128bit values from memory locations s1,s2 , then store in sumof
		;; all three locations MUST be consecutive in memory 16bytes (128bits!) appart
    ld hl,(s1) \ ld de,(s2) \ add hl,de \ ld (sumof),hl
    ld hl,(s1+2) \ ld de,(s2+2) \ adc hl,de \ ld (sumof+2),hl
    ld hl,(s1+4) \ ld de,(s2+4) \ adc hl,de \ ld (sumof+4),hl
    ld hl,(s1+6) \ ld de,(s2+6) \ adc hl,de \ ld (sumof+6),hl
    ld hl,(s1+8) \ ld de,(s2+8) \ adc hl,de \ ld (sumof+8),hl
	ld hl,(s1+10) \ ld de,(s2+10) \ adc hl,de \ ld (sumof+10),hl
	ld hl,(s1+12) \ ld de,(s2+12) \ adc hl,de \ ld (sumof+12),hl
	ld hl,(s1+14) \ ld de,(s2+14) \ adc hl,de \ ld (sumof+14),hl
	ret

sum256 ;; add two 256bit values from memory locations s1, s2, then store in sumof
		;; all three locations MUST be consecutive in memory 32bytes (256bits!) appart
    ld hl,(s1) \ ld de,(s2) \ add hl,de \ ld (sumof),hl
    ld hl,(s1+2) \ ld de,(s2+2) \ adc hl,de \ ld (sumof+2),hl
    ld hl,(s1+4) \ ld de,(s2+4) \ adc hl,de \ ld (sumof+4),hl
    ld hl,(s1+6) \ ld de,(s2+6) \ adc hl,de \ ld (sumof+6),hl
    ld hl,(s1+8) \ ld de,(s2+8) \ adc hl,de \ ld (sumof+8),hl
	ld hl,(s1+10) \ ld de,(s2+10) \ adc hl,de \ ld (sumof+10),hl
	ld hl,(s1+12) \ ld de,(s2+12) \ adc hl,de \ ld (sumof+12),hl
	ld hl,(s1+14) \ ld de,(s2+14) \ adc hl,de \ ld (sumof+14),hl
	
	ld hl,(s1+16) \ ld de,(s2+16) \ adc hl,de \ ld (sumof+16),hl
	ld hl,(s1+18) \ ld de,(s2+18) \ adc hl,de \ ld (sumof+18),hl
	ld hl,(s1+20) \ ld de,(s2+20) \ adc hl,de \ ld (sumof+20),hl
	ld hl,(s1+22) \ ld de,(s2+22) \ adc hl,de \ ld (sumof+22),hl
	ld hl,(s1+24) \ ld de,(s2+24) \ adc hl,de \ ld (sumof+24),hl
	ld hl,(s1+26) \ ld de,(s2+26) \ adc hl,de \ ld (sumof+26),hl
	ld hl,(s1+28) \ ld de,(s2+28) \ adc hl,de \ ld (sumof+28),hl
	ld hl,(s1+30) \ ld de,(s2+30) \ adc hl,de \ ld (sumof+30),hl	
	ret	
	
sum512 ;; add two 512bit values from memory locations s1, s2, then store in sumof
		;; all three locations MUST be consecutive in memory 64bytes (512bits!) appart
    ld hl,(s1) \ ld de,(s2) \ add hl,de \ ld (sumof),hl
    ld hl,(s1+2) \ ld de,(s2+2) \ adc hl,de \ ld (sumof+2),hl
    ld hl,(s1+4) \ ld de,(s2+4) \ adc hl,de \ ld (sumof+4),hl
    ld hl,(s1+6) \ ld de,(s2+6) \ adc hl,de \ ld (sumof+6),hl
    ld hl,(s1+8) \ ld de,(s2+8) \ adc hl,de \ ld (sumof+8),hl
	ld hl,(s1+10) \ ld de,(s2+10) \ adc hl,de \ ld (sumof+10),hl
	ld hl,(s1+12) \ ld de,(s2+12) \ adc hl,de \ ld (sumof+12),hl
	ld hl,(s1+14) \ ld de,(s2+14) \ adc hl,de \ ld (sumof+14),hl
	
	ld hl,(s1+16) \ ld de,(s2+16) \ adc hl,de \ ld (sumof+16),hl
	ld hl,(s1+18) \ ld de,(s2+18) \ adc hl,de \ ld (sumof+18),hl
	ld hl,(s1+20) \ ld de,(s2+20) \ adc hl,de \ ld (sumof+20),hl
	ld hl,(s1+22) \ ld de,(s2+22) \ adc hl,de \ ld (sumof+22),hl
	ld hl,(s1+24) \ ld de,(s2+24) \ adc hl,de \ ld (sumof+24),hl
	ld hl,(s1+26) \ ld de,(s2+26) \ adc hl,de \ ld (sumof+26),hl
	ld hl,(s1+28) \ ld de,(s2+28) \ adc hl,de \ ld (sumof+28),hl
	ld hl,(s1+30) \ ld de,(s2+30) \ adc hl,de \ ld (sumof+30),hl	

	ld hl,(s1+32) \ ld de,(s2+32) \ adc hl,de \ ld (sumof+32),hl
	ld hl,(s1+34) \ ld de,(s2+34) \ adc hl,de \ ld (sumof+34),hl
	ld hl,(s1+36) \ ld de,(s2+36) \ adc hl,de \ ld (sumof+36),hl
	ld hl,(s1+38) \ ld de,(s2+38) \ adc hl,de \ ld (sumof+38),hl
	ld hl,(s1+40) \ ld de,(s2+40) \ adc hl,de \ ld (sumof+40),hl
	ld hl,(s1+42) \ ld de,(s2+42) \ adc hl,de \ ld (sumof+42),hl
	ld hl,(s1+44) \ ld de,(s2+44) \ adc hl,de \ ld (sumof+44),hl
	ld hl,(s1+46) \ ld de,(s2+46) \ adc hl,de \ ld (sumof+46),hl	

	ld hl,(s1+48) \ ld de,(s2+48) \ adc hl,de \ ld (sumof+48),hl
	ld hl,(s1+50) \ ld de,(s2+50) \ adc hl,de \ ld (sumof+50),hl
	ld hl,(s1+52) \ ld de,(s2+52) \ adc hl,de \ ld (sumof+52),hl
	ld hl,(s1+54) \ ld de,(s2+54) \ adc hl,de \ ld (sumof+54),hl
	ld hl,(s1+56) \ ld de,(s2+56) \ adc hl,de \ ld (sumof+56),hl
	ld hl,(s1+58) \ ld de,(s2+58) \ adc hl,de \ ld (sumof+58),hl
	ld hl,(s1+60) \ ld de,(s2+60) \ adc hl,de \ ld (sumof+60),hl
	ld hl,(s1+62) \ ld de,(s2+62) \ adc hl,de \ ld (sumof+62),hl		
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
	dec hl   ;; ok so for some reason the code above always adds two to the result, eg 0 * 0 = 2
	dec hl
ret
	
zero16				; zero 16bits starting from hl,
					; b and hl clobbered by this
	ld b,2			; set loop control variable for next loop
zero16_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero16_loop		; jump if loop control variable not zero
	ret

	
	
zero128				; zero 128bits starting from hl,
					; b and hl clobbered by this
	ld b,16			; set loop control variable for next loop
zero128_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero128_loop		; jump if loop control variable not zero
	ret

zero256				; zero 256bits starting from hl,
					; b and hl clobbered by this
	ld b,32			; set loop control variable for next loop 
zero256_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero256_loop		; jump if loop control variable not zero
	ret	
	
zero512				; zero 512bits starting from hl,
					; b and hl clobbered by this
	ld b,64			; set loop control variable for next loop
zero512_loop
	ld (hl),0			; set the thing hl points to to a constant
	inc hl	
	djnz zero512_loop		; jump if loop control variable not zero
	ret		
	
start

	call CLS	

	ld hl, s1		;;; initialise s1 s2 and sumof all to zero
	call zero16	
	ld hl, s2	
	call zero16
	ld hl, sumof		
	call zero16
					;;; set s2 to the second in fibonacci sequence ie "1", s1 is already zero	
	ld hl, s1		
	ld (hl),1
	;ld hl, s1+1
	;ld (hl),2
	
	ld hl, s2		
	ld (hl),3		

	ld hl, s1		; copy s1 to print buffer 
	ld de, to_print		;
	ld bc, 2
	ldir				; copy 64 bytes from hl to de	
	call hprint16 		; print a 512 bit (64byte numnber as hex to screen)

	ld hl, s2		; copy s2 to print buffer 
	ld de, to_print		;
	ld bc, 2
	ldir				; copy 64 bytes from hl to de	
	;call hprint16 		; print a 512 bit (64byte numnber as hex to screen)
	
mainloop		
	ld de, (s1)
	ld bc, (s2)		
	call multiply16  ; de and bc contain numbers to mulitply, hl contains result

	ld (sumof), hl
	ld hl, sumof
	ld de, to_print		
	ld bc, 2
	ldir	
	
	call hprint16 		; print a 512 bit 
	
	ld hl, (s1)
	inc hl
	ld (s1), hl
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

