;	SCELBAL BASIC i8008 program loader for generic MS-DOS
;
;Gazelle states the copyright about;
;Converted old 8008 mnemonics to new 8008 mnemonics.
;Added IO routines to match the specifications of the DOS loader.
;Added startup and IO routines to run on CP/M running an 8080.
;
;	Copyright (C) 2025 by Gazelle
;
;Permission is hereby granted, free of charge, to any person
;obtaining a copy of this software and associated documentation
;files (the "Software"), to deal in the Software without
;restriction, including without limitation the rights to use,
;copy, modify, merge, publish, distribute, sublicense, and/or sell
;copies of the Software, and to permit persons to whom the
;Software is furnished to do so, subject to the following
;conditions:
;
;The above copyright notice and this permission notice shall be
;included in all copies or substantial portions of the Software.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;OTHER DEALINGS IN THE SOFTWARE.
;
;Repository https://github.com/Gazelle8087/SCELVAL-BASIC
;original source for SVELVAL BASIC is available here under
;https://www.willegal.net/scelbi/the8008andScelbi.html
;
;2025/6/5 Rev. 1.00 Initial release
;
;;; This is the Scelbi Basic Program from 1974 known as
;;; SCELBAL by Mark G. Arnold (MGA) and Nat Wadsworth  
;;;
;;;  Copyright 1975 Scelbi Computer Consulting, Inc.
;;;  All rights reserved
;;;
;;; MGA gives permission to use SCELBAL for 
;;; educational, historical, non-commercial purposes.
;;; Versions of this have been circulating on the web since
;;; about 2000; this version is authorized by MGA (Mar 2012)
;;; with the understanding no warranty is expressed or implied.
;;; As stated in the original, "no responsibility is assumed for
;;; for inaccuracies or for the success or failure of
;;; various applications to which the information herein
;;; may be applied."
;;; 
;;; SCELBAL is the only open-source, floating-point 
;;; high-level language ever implemented on Intel's first
;;; general-purpose microprocessor, the 8008.  It was
;;; published in book form:
;;;
;;;  SCELBAL: A Higher-Level Language for 8008/8080 Systems
;;;
;;; (Tiny BASIC only used 16-bit integers; the MCM\70
;;; was a closed system; calculators implemented with 8008
;;; were floating-point, but not high-level.)
;;;
;;; This version is modified to assemble with the
;;; as8 assembler (using the -octal option) 
;;; for the Intel 8008 by Thomas E. Jones.
;;; This current form is made up non-relocatable so that
;;; locations of all code and data is identical to the
;;; original SCELBAL documents and patches.  It should be
;;; reasonable after debugging code to convert this to a
;;; relocatable and ROMable code with variables in RAM.
;;; This code originates from a version made by 
;;;
;;;    Steve Loboyko in 2001.
;;;
;;; This version has all 3 patches for SCELBAL (the two
;;; pasted in the original manual, and a third which was
;;; written in SCELBAL UPDATE publication, as well as
;;; a couple changes to constants which didn't actually
;;; require a patch, just changes to bytes of data or
;;; arguments to an instruction--one of these (Tucker) was 
;;; incorrect and restored to original by MGA March 2012).
;;; 
;;; This comment must be incorporated with any version of SCELBAL
;;; downloaded, distributed, posted or disemenated.

FOR8080	EQU	0		;switch to 1 for 8080CPU and CP/M

 IF FOR8080
		CPU	8080
FOR8008		EQU	0
 ELSE
		CPU	8008new
FOR8008		EQU	1
 ENDIF
		page	0,100

ENDPGRAM	EQU	055o	;MGA 4/10/12 as in orig; for his ROMable Loboyko said 077       [077]
BGNPGRAM	EQU	033o	;MGA 4/10/12 as in orig; for his ROMable Loboyko said 044       [044]

;;; Here are labels originally attempting to make the code
;;; relocatable.  These 4 pages contain variable data
;;; which needs to be relocated from ROM to RAM.
;;; I can't vouch for ALL references to these pages in
;;; the code being switched to these labels, but they
;;; seem to be.

PG01		EQU	1	;OLDPG1:	EQU	001#000
PG26		EQU	026o	;OLDPG26:	EQU	026#000
PG27		EQU	027o	;OLDPG27:	EQU	027#000
PG57		EQU	057o	;OLDPG57:	EQU	057#000

;;; Page zero will contain the I/O Routines.  These are actually
;;; just as suggested by Scelbal Manual for Serial I/O.

 IF FOR8008
		ORG	0
		JMP	START3000
 ENDIF

;;; THE ABOVE MUST CONCLUDE BEFORE BY PAGE 1 STARTS

;;; Page one has many constants and variables.

		ORG	0100h		;001#000
		JMP	START3000
		db	0
;		db	0,0,0,0		;DATA *4
		db	0,0,100o,1	;DATA 000,000,100,001	; STORES FLOATING POINT CONSTANT +1.0
		db	0,0,0		;DATA *3
		db	0		;DATA 000		; EXPONENT COUNTER
		db	0,0,0,0		;DATA 000,000,000,000	; STORES FLOATING POINT NUMBER TEMPORARILLY
		db	0,0,0,0		;DATA *4
		db	0,0,300o,1	;DATA 000,000,300,001	; STORES FLOATING POINT CONSTANT -1.0
		db	0,0,0,0		;DATA 000,000,000,000	; SCRATCH PAD AREA (16 BYTES)
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0		;DATA 000,000,000,000
		db	1,120o,162o,2o	;DATA 001,120,162,002	; STORES RANDOM NUMBER GENERATOR CONSTANT VALUE
		db	0,0,0,0		;DATA *4
		db	3,150o,157o,14o	;DATA 003,150,157,014	; STORES RANDOM NUMBER GENERATOR CONSTANT VALUE
		db	0,0,0,0		;DATA 000,000,000,000	; SCRATCH PAD AREA (12 BYTES) (01 064-077)
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0		;DATA 000,000		; SIGN INDICATOR
		db	0		;DATA 000		; BITS COUNTER
		db	0,0		;DATA 000,000		; SIGN INDICATOR
IN_DIGIT_CC_L0	db	0		;DATA 000		; INPUT DIGIT COUNTER
IN_DIGIT_CC_L	equ	105o
		db	0		;DATA 000		; TEMP STORATE
		db	0		;DATA 000		; OUTPUT DIGIT COUNTER
		db	0		;DATA 000 		; FP MODE INDICATOR
		db	0,0,0,0,0,0,0	;DATA *7		; NOT ASSIGNED (SHOULD BE 01 111-117)
		db	0,0,0,0		;DATA 000,000,000,000	; FPACC EXTENSION
		db	0,0,0,0		;DATA 000,000,000,000	; FPACC LSW, NSW, MSW, EXPONENT
		db	0,0,0,0		;DATA 000,000,000,000	; FPOP  Extension
		db	0,0,0,0		;DATA 000,000,000,000	; FPOP  LSW, NSW, MSW, EXPONENT
		db	0,0,0,0		;DATA 000,000,000,000	; FLOATING POINT WORKING AREA
		db	0,0,0,0		;DATA 000,000,000,000	; (SHOULD BE AT 01 140-01-167)
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0		;DATA 000,000,000,000
		db	0,0,0,0,0,0,0,0	;DATA *8		; NOT ASSIGNED (SHOULD BE 01 170-01 177)
		db	0,0,0,0		;DATA 000,000,000,000	; TEMPORARY REGISTER STORAGE AREA (D,E,H&L)
		db	0,0,0,0		;DATA *4		; NOT ASSIGNED (01 204-01 207)
		db	0,0,120o,4	;DATA 000,000,120,004	; STORES FLOATING POINT CONSTANT +10.0
		db	147o,146o,146o,375o	;DATA 147,146,146,375	; STORES FLOATING POINT CONSTANT +0.1
		db	0		;DATA 000		; GETINP COUNTER
		db	0,0,0,0,0,0	;DATA *6		; NOT ASSIGNED (01 221-01 226)
		db	0		;DATA 000		; ARITHMETIC STACK POINTER (01 227)
		db	0		;DATA 000		; ARITHMETIC STACK (NOT CLEAR HOW LONG)

;		db	(1bah-$) dup 0	

		ORG 	01bah		;001#272
		db	4		;DATA 004		; CC FOR SAVE
;		db	"SAVE"		;DATA "SAVE"
		db	"S"+80h
		db	"A"+80h
		db	"V"+80h
		db	"E"+80h
		db	4		;DATA 004		; CC FOR LOAD
;		db	"LOAD"		;DATA "LOAD"
		db	"L"+80h
		db	"O"+80h
		db	"A"+80h
		db	"D"+80h
		db	0,0,0,0		;DATA 000,000,000,000	; UNCLEAR WHAT THIS IS (01 304-01 317) ZEROS
		db	0,0,0,0		;DATA 000,000,000,000	; (PROBABLY STEP, FOR/NEXT, AND ARRAY PTR TEMP)
		db	0,0,0,0		;DATA 000,000,000,000
					;; AT THIS POINT WE SHOULD BE AT LOCATION 01 320 01d0h
		db	4		;DATA 4
;		db	"THEN"		;DATA "THEN"
		db	"T"+80h
		db	"H"+80h
		db	"E"+80h
		db	"N"+80h
		db	2		;DATA 2
;		db	"TO"		;DATA "TO"
		db	"T"+80h
		db	"O"+80h
		db	4		;DATA 4
;		db	"STEP"		;DATA "STEP"
		db	"S"+80h
		db	"T"+80h
		db	"E"+80h
		db	"P"+80h
		db	4		;DATA 4
;		db	"LIST"		;DATA "LIST"
		db	"L"+80h
		db	"I"+80h
		db	"S"+80h
		db	"T"+80h
		db	3		;DATA 3
;		db	"RUN"		;DATA "RUN"
		db	"R"+80h
		db	"U"+80h
		db	"N"+80h
		db	3		;DATA 3
;		db	"SCR"		;DATA "SCR
		db	"S"+80h
		db	"C"+80h
		db	"R"+80h
		db	013o		;DATA 013		; CC FOR "READY" MESSAGE
		db	224o,215o,212o	;DATA 224,215,212	; CTRL-T, CARRIAGE RETURN, LINE FEED
;		db	"READY"		;DATA "READY"
		db	"R"+80h
		db	"E"+80h
		db	"A"+80h
		db	"D"+80h
		db	"Y"+80h
		db	215o,212o,212o	;DATA 215,212,212	; CARRIAGE RETURN, LINE FEED, LINE FEED;
		db	011o		;DATA 011
;		db	" AT LINE "	;DATA " AT LINE "
		db	" "+80h
		db	"A"+80h
		db	"T"+80h
		db	" "+80h
		db	"L"+80h
		db	"I"+80h
		db	"N"+80h
		db	"E"+80h
		db	" "+80h

	;; THIS SHOULD BE THE END OF PAGE 01

;	db	(200h-$) dup 0

           ORG	0200h		;002#000		; START PAGE 02, THE CODE
SYNTAX:    CALL	CLESYM             ;Clear the SYMBOL BUFFER area
           MVI	L, 340o               ;Set L to start of LINE NUMBER BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of LINE NUMBER BUFFER
           MVI	M, 000o               ;Initialize line number buff by placing zero as (cc)
           MVI	L, 201o               ;Change pointer to syntax counter/pointer storage loc.
           MVI	M, 001o               ;Set pointer to first character (after cc) in line buffer
SYNTX1:    MVI	L, 201o               ;Set pointer to syntax cntr/pntr storage location
           CALL	GETCHR             ;Fetch the character pointed to by contents of syntax
           JZ	SYNTX2             ;Cntr/pntr from the line input buffer. If character was
           CPI	260o               ;A space, ignore. Else, test to see if character was ASCII
           JM	SYNTX3             ;Code for a decimal digit. If not a decimal digit, consider
           CPI	272o               ;Line number to have been processed by jumping
           JP	SYNTX3             ;Over the remainder of this SYNTX1 section.
           MVI	L, 340o               ;If have decimal digit, set pointer to start of LINE
           CALL	CONCT1             ;NUMBER BUFFER and append incoming digit there.
SYNTX2:    MVI	L, 201o               ;Reset L to syntax cntr/pntr storage location. Call sub-
           CALL	LOOP               ;Routine to advance pntr and test for end of inr)ut buffer
           JNZ	SYNTX1             ;If not end of input buffer, go back for next digit
           MVI	L, 203o               ;If end of buffer, only had a line number in the line.
           MVI	M, 000o               ;Set pntr to TOKEN storage location. Set TOKEN = 000.
           RET                    ;Return to caller.
SYNTX3:    MVI	L, 201o               ;Reset pointer to syntax cntr/pntr and fetch
           MOV	B,M                    ;Position of next character after the line number
           MVI	L, 202o               ;Change pntr to SCAN pntr storage location
           MOV	M,B                    ;Store address when SCAN takes up after line number
SYNTX4:    MVI	L, 202o               ;Set pntr to SCAN pntr stomge location
           CALL	GETCHR             ;Fetch the character pointed to by contents of the SCAN
           JZ	SYNTX6             ;Pointer storage location. If character was ASCII code
           CPI	275o               ;For space, ignore. Else, compare character with "=" sign
           JZ	SYNTX7             ;If is an equal sign, go set TOKEN for IMPLIED LET.
           CPI	250o               ;Else, compare character with left parenthesis " ( "
           JZ	SYNTX8             ;If left parenthesis, go set TOKEN for implied array LET
           CALL	CONCTS             ;Otherwise, concatenate the character onto the string
; MGA 4/2012 begin "fast SYNTX5" patch: 
; the following patch doubles the overall speed of execution.  
; It is similar to the approach taken on 8080 SCELBAL II in 1978 
; it adhears to the rules for patches in issue 1 of SCELBAL update 
;SYNTX6:   these four lines moved up w/o label
           MVI	L, 202o               ;Set L to SCAN pointer storage location
;           LHI \HB\OLDPG26       ;** Set H to page of SCAN pointer stomge location
;MGA 4/2012 except LHI needed at original place, not here 
           CALL	LOOP               ;Call routine to advance pntr & test for end of In buffer
           JNZ	SYNTX4             ;Go back and add another character to SYMBOL BUFF
SYNTX6:   ; MGA 4/2012 label here 

           MVI	L, 203o               ;Being constructed in the SYMBOL BUFFER. Now set
           MVI	M, 001o               ;Up TOKEN storage location to an initial value of 001.
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to point to start of KEYWORD TABLE.
           MVI	L, 000o               ;Set L to point to start of KEYWORD TABLE.
SYNTX5:    MVI	D,PG26 ;\HB\OLDPG26   ;** Set D to page of SYMBOL BUFFER
           MVI	E, 120o               ;Set E to start of SYMBOL BUFFER
           CALL	STRCP              ;Compare char string presently in SYMBOL BUFFER
           RZ                    ;With entry in KEYWORD TABLE. Exit if match.
           CALL	SWITCH             ;TOKEN will be set to keyword found. Else, switch
SYNTXL:    INR	L                    ;Pointers to get table address back and advance pntr to
           MOV	A,M                    ;KEYWORD TABLE. Now look for start of next entry
           ANI	300o               ;In KEYWORD TABLE by looking for (cc) byte which
           JNZ	SYNTXL             ;Will NOT have a one in the two most sig. bits. Advance
           CALL	SWITCH             ;Pntr til next entry found. Then switch pointers apin so
           MVI	L, 203o               ;Table pointer is in D&E. Put addr of TOKEN in L.
           MVI	H,PG26 ;\HB\OLDPG26   ;** And page of TOKEN in H. Fetch the value currently
           MOV	B,M                    ;In TOKEN and advance it to account for going on to
           INR	B                    ;The next entry in the KEYWORD TABLE.
           MOV	M,B                    ;Restore the updated TOKEN value back to storage.
           CALL	SWITCH             ;Restore the keyword table pointer back to H&L.
           MOV	A,B                    ;Put TOKEN count in ACC.
           CPI	015o               ;See if have tested all entries in the keyword table.
           JNZ	SYNTX5             ;If not, continue checking the keyword table.
;MGA 4/2012 3 of 4 lines removed below (keep LHI)
           MVI	H, 26o	;\HB\OLDPG26        ;** Set H to page of SCAN pointer stomge location
; MGA 4/2012 end of "fast SYNTX5" patch: 
           MVI	L, 203o               ;And search table for KEYWORD again. Unless reach
           MVI	M, 377o               ;End of line input buffer. In which case set TOKEN=377
           RET                    ;As an error indicator and exit to caMVI L,ng routine.
SYNTX7:    MVI	L, 203o               ;Set pointer to TOKEN storage register. Set TOKEN
           MVI	M, 015o               ;Equal to 015 when "=" sign found for IMPLIED LET.
           RET                    ;Exit to caMVI L,ng routine.
SYNTX8:    MVI	L, 203o               ;Set pointer to TOKEN storage register. Set TOKEN
           MVI	M, 016o               ;Equal to 016 when "(" found for IMPLIED array LET.
           RET                    ;Exit to calling routine.

                                  ;The following are subroutines used by SYNTAX and
                                  ;other routines in SCELBAL.

BIGERR:    MVI	A, 302o               ;Load ASCII code for letters B and G to indicate BIG
           MVI	C, 307o               ;ERROR (for when buffer, stack,etc., overflows).
ERROR:     CALL	ECHO               ;Call user provided display routine to print ASCII code
           MOV	A,C                    ;In accumulator. Transfer ASCII code from C to ACC
           CALL	ECHO               ;And repeat to display error codes.
           JMP	FINERR             ;Go cpmplete error message (AT LINE) as required.
GETCHR:    MOV	A,M                    ;Get pointer from memory location pointed to by H&L
           CPI	120o               ;See if within range of line input buffer.
           JP	BIGERR             ;If not then have an overflow condition = error.
           MOV	L,A                    ;Else can use it as addr of character to fetch from the
           MVI	H,PG26 ;\HB\OLDPG26   ;** LINE INPUT BUFFER by setting up H too.
           MOV	A,M                    ;Fetch the character from the line input buffer.
           CPI	240o               ;See if it is ASCII code for space.
           RET                    ;Return to caller with flags set according to comparison.
CLESYM:    MVI	L, 120o               ;Set L to start of SYMBOL BUFFER.
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of SYMBOL BUFFER.
           MVI	M, 000o               ;Place a zero byte at start of SYMBOL BUFFER.
           RET                    ;To effectively clear the buffer. Then exit to caller.


                                  ;Subroutine to concatenate (append) a character to the
                                  ;SYMBOL BUFFER. Character must be alphanumeric.

CONCTA:    CPI	301o               ;See if character code less than that for letter A.
           JM	CONCTN             ;If so, go see if it is numeric.
           CPI	333o               ;See if character code greater than that for letter Z.
           JM	CONCTS             ;If not, have valid alphabetical character.
CONCTN:    CPI	260o               ;Else, see if character in valid numeric range.
           JM	CONCTE             ;If not, have an error condition.
           CPI	272o               ;Continue to check for valid number.
           JP	CONCTE             ;If not, have an error condition.
CONCTS:    MVI	L, 120o               ;If character alphanumeric, can concatenate. Set pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** To starting address of SYMBOL BUFFER.
CONCT1:    MOV	C,M                    ;Fetch old character count in SYMBOL BUFFER.
           INR	C                    ;Increment the value to account for adding new
           MOV	M,C                    ;Character to the buffer. Restore updated (cc).
           MOV	B,A                    ;Save character to be appended in register B.
           CALL	INDEXC             ;Add (cc) to address in H & L to get new end of buffer
           MOV	M,B                    ;Address and append the new character to buffer
           MVI	A, 000o               ;Clear the accumulator
           RET                    ;Exit to caller
CONCTE:    JMP	SYNERR             ;If character to be appended not alphanumeric, ERROR!

                                  ;Subroutine to compare
                                  ;character strings pointed to by
                                  ;register pairs D & E and H & L.

STRCP:     MOV	A,M                    ;Fetch (cc) of first string.
           CALL	SWITCH             ;Switch pointers and fetch length of second string (cc)
           MOV	B,M                    ;Into register B. Compare the lengths of the two strings.
           CMP	B                    ;If they are not the same
           RNZ                    ;Return to caller with flags set to non-zero condition
           CALL	SWITCH             ;Else, exchange the pointers back to first string.
STRCPL:    CALL	ADV                ;Advance the pointer to string number 1 and fetch a
           MOV	A,M                    ;Character from that string into the accumulator.
           CALL	SWITCH             ;Now switch the pointers to string number 2.
           CALL	ADV                ;Advance the pointer in line number 2.
STRCPE:    CMP	M                    ;Compare char in stxing 1 (ACC) to string 2 (memory)
           RNZ                    ;If not equal, return to cauer with flags set to non-zero
           CALL	SWITCH             ;Else, exchange pointers to restore pntr to string 1
           DCR	B                    ;Decrement the string length counter in register B
           JNZ	STRCPL             ;If not finiahed, continue testing entire string
           RET                    ;If complete match, return with flag in zero condition
STRCPC:    MOV	A,M                    ;Fetch character pointed to by pointer to string 1
           CALL	SWITCH             ;Exchange pointer to examine string 2
           JMP	STRCPE             ;Continue the string comparison loop

                                  ;Subroutine to advance the two byte
                                  ;value in CPU registers H and L.

ADV:       INR	L                    ;Advance value in register L.
           RNZ                    ;If new value not zero, return to caller.
           INR	H                    ;Else must increment value in H
           RET                    ;Before retuming to caller

                                  ;Subroutine to advance a buffer pointer
                                  ;and test to see if the end of the buffer
                                  ;has been reached.

LOOP:      MOV	B,M                    ;Fetch memory location pointed to by H & L into B.
           INR	B                    ;Increment the value.
           MOV	M,B                    ;Restore it back to memory.
           MVI	L, 000                ;Change pointer to start of INPUT LINE BUFFER
           MOV	A,M                    ;Fetch buffer length (cc) value into the accumulator
           DCR	B                    ;Make value in B original value
           CMP	B                    ;See if buffer length same as that in B
           RET                    ;Return with flags yielding results of the comparison

                                  ;The following subroutine is used to
                                  ;input characters from the system's
                                  ;input device (such as a keyboard)
                                  ;into the LINE INPUT BUFFER. Routine has limited
                                  ;editing capability included.
                                  ;(Rubout = delete previous character(s) entered.)
;;; This label, STRIN:	should be location 03 014 030ch
STRIN:     MVI	C, 000                ;Initialize register C to zero.
STRIN1:    CALL	CINPUT             ;Call user provided device input subroutine to fetch one
           CPI	0FFH	;modifiedby gazelle 2025/6/1 ;377o  ;Character from the input device. Is it ASCII code for
           JNZ	NOTDEL             ;Rubout? Skip to next section if not rubout.
;          MVI	A, 0DCH	;modified by Gazelle 2025/6/1 ;334o ;Else, load ASCII code for backslash into ACC.
;          CALL	ECHO	;modified by gazelle 2025/6/1 ;Call user display driver to present backslash as a delete
           DCR	C                    ;Indicator. Now decrement the input character counter.
           JM	STRIN              ;If at beginning of line do NOT decrement H and L.
           CALL	DEC                ;Else, decrement H & L line pointer to erase previous
           MVI	A, 0DCH	;modified by gazelle 2025/6/1
           CALL	ECHO	;modified by gazelle 2025/6/1

           JMP	STRIN1             ;Entry, then go back for a new input.
NOTDEL:    CPI	203o               ;See if character inputted was'CONTROL C'
           JZ	CTRLC              ;If so, stop inputting and go back to the EXECutive
           CPI	215o               ;If not, see if character was carriage-return
           JZ	STRINF             ;If so, have end of line of input
           CPI	212o               ;If not, see if character was line-feed
           JZ	STRIN1             ;If so, ignore the input, get another character
           CALL	ADV                ;If none of the above, advance contents of H & L
           INR	C                    ;Increment the character counter
           MOV	M,A                    ;Store the new character in the line input buffer
           MOV	A,C                    ;Put new character count in the accumulator
           CPI	120o               ;Make sure maximum buffer size not exceeded
           JP	BIGERR             ;If buffer size exceeded, go display BG error message
           JMP	STRIN1             ;Else can go back to look for next input
STRINF:    MOV	B,C                    ;Transfer character count from C to B
           CALL	SUBHL              ;Subtract B from H & L to get starting address of
           MOV	M,C                    ;The string and place the character count (cc) there
           CALL	CRLF               ;Provide a line ending CR & LF combination on the
           RET                    ;Display device. Then exit to caller.

                                  ;Subroutine to subtract contents of CPU register B from
                                  ;the two byte value in CPU registers H & L.

SUBHL:     MOV	A,L                    ;Load contents of register L into the accumulator
           SUB	B                    ;Subtract the contents of register B
           MOV	L,A                    ;Restore the new value back to L
           RNC                    ;If no carry, then no underflow. Exit to caller.
           DCR	H                    ;Else must also decrement contents of H.
           RET                    ;Before retuming to caller.

                                  ;Subroutine to display a character string on the system's
                                  ;display device.

TEXTC:     MOV	C,M                    ;Fetch (cc) from the first location in the buffer (H & L
           MOV	A,M                    ;Pointing there upon entry) into register B and ACC.
           ANA	A                    ;Test the character count value.
           RZ                    ;No display if (cc) is zero.
TEXTCL:    CALL	ADV                ;Advance pointer to next location in buffer
           MOV	A,M                    ;Fetch a character from the buffer into ACC
           CALL	ECHO               ;Call the user's display driver subroutine
           DCR	C                    ;Decrement the (cc)
           JNZ	TEXTCL             ;If character counter not zero, continue display
           RET                    ;Exit to caller when (cc) is zero.

                                  ;Subroutine to provide carriage-return and line-feed
                                  ;combination to system's display device. Routine also
                                  ;initializes a column counter to zero. Column counter
                                  ;is used by selected output routines to count the num-
                                  ;ber of characters that have been displayed on a line.

CRLF:      MVI	A, 215o               ;Load ASCII code for carriage-return into ACC
           CALL	ECHO               ;Call user provided display driver subroutine
           MVI	A, 212o               ;Load ASCII code for line-feed into ACC
           CALL	ECHO               ;Call user provided display driver subroutine
           MVI	L, 043o               ;Set L to point to COLUMN COUNTER storage location
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of COLUMN COUNTER
           MVI	M, 001o               ;Initialize COLUMN COUNTER to a value of one
           MOV	H,D                    ;Restore H from D (saved by ECHO subroutine)
           MOV	L,E                    ;Restore L from E (saved by ECHO subroutine)
           RET                    ;Then exit to calling routine

                                  ;Subroutine to decrement double-byte value in CPU
                                  ;registers H and L.

DEC:       DCR	L                    ;Decrement contents of L
           INR	L                    ;Now increment to exercise CPU flags
           JNZ	DECNO              ;If L not presently zero, skip decrementing H
           DCR	H                    ;Else decrement H
DECNO:     DCR	L                    ;Do the actual decrement of L
           RET                    ;Return to caller


                                  ;Subroutine to index the value in CPU registers H and L
                                  ;by the contents of CPU register B.

INDEXB:    MOV	A,L                    ;Load L into the accumulator
           ADD	B                    ;Add B to that value
           MOV	L,A                    ;Restore the new value to L
           RNC                    ;If no carry,  return to caller
           INR	H                    ;Else, increment value in H
           RET                    ;Before returning to caller

                                  ;The following subroutine is used to
                                  ;display the ASCII encoded character in the ACC on the
                                  ;system's display device. This routine calls a routine
                                  ;labeled CINPUT which must be provided by the user to
                                  ;actually drive the system's output device. The subroutine
                                  ;below also increments an output column counter each time
                                  ;it is used.

ECHO:      MOV	D,H                    ;Save entry value of H in register D
           MOV	E,L                    ;And save entry value of L in register E
           MVI	L, 043o               ;Set L to point to COLUMN COUNTER storage location
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of COLUMN COUNTER
           MOV	B,M                    ;Fetch the value in the COLUMN COUNTER
           INR	B                    ;And increment it for each character displayed
           MOV	M,B                    ;Restore the updated count in memory
           CALL	CPRINT             ;tt Call the user's device driver subroutine
           MOV	H,D                    ;Restore entry value of H from D
           MOV	L,E                    ;Restore entry value of L from E
           RET                    ;Return to calling routine
CINPUT:	   JMP	CINP               ;Reference to user defined input subroutine

;;; The label EVAL: SHOULD BE AT 03 224 0394h
EVAL:      MVI	L, 227o               ;Load L with address of ARITHMETIC STACK pointer
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of ARITHMETIC STACK pointer
           MVI	M, 224o               ;Initialize ARITH STACK pointer value to addr minus 4
           INR	L                    ;Advance memory pointer to FUN/ARRAY STACK pntr
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of FUN/ARRAY STACK pointer
           MVI	M, 000o               ;Initialize FUNIARRAY STACK pointer to start of stack
           CALL	CLESYM             ;Initialize the SYMBOL BUFFER to empty condition
           MVI	L, 210o               ;Load L with address of OPERATOR STACK pointer
           MVI	M, 000                ;Initialize OPERATOR STACK pointer value
           MVI	L, 276o               ;Set L to address of EVAL pointer (start of expression)
           MOV	B,M                    ;Fetch the EVAL pointer value into register B
           MVI	L, 200o               ;Set up a working pointer register in this location
           MOV	M,B                    ;And initialize EVAL CURRENT pointer
SCAN1:     MVI	L, 200o               ;Load L with address of EVAL CURRENT pointer
           CALL	GETCHR             ;Fetch a character in the expression being evaluated
           JZ	SCAN10             ;If character is a space, jump out of this section
           CPI	253o               ;See if character is a "+" sign
           JNZ	SCAN2              ;If not, continue checking for an operator
           MVI	L, 176o               ;If yes, set pointer to PARSER TOKEN storage location
           MVI	M, 001                ;Place TOKEN value for "+" sign in PARSER TOKEN
           JMP	SCANFN             ;Go to PARSER subroutine entry point
SCAN2:     CPI	255o               ;See if character is a minus ("-") sign
           JNZ	SCAN4              ;If not, continue checking for an operator
           MVI	L, 120o               ;If yes, check the length of the symbol stored in the
           MOV	A,M                    ;SYMBOL BUFFER by fetching the (cc) byte
           ANA	A                    ;And testing to see if (cc) is zero
           JNZ	SCAN3              ;If length not zero, then not a unary minus indicator
           MVI	L, 176o               ;Else, check to see if last operator was a right parenthesi
           MOV	A,M                    ;By fetching the value in the PARSER TOKEN storage
           CPI	007                ;Location and seeing if it is token value for ")"
           JZ	SCAN3              ;If last operator was I')" then do not have a unary minus
           CPI	003                ;Check to see if last operator was C4*~2
           JZ	SYNERR             ;If yes, then have a syntax error
           CPI	005                ;Check to see if last operator was exponentiation
           JZ	SYNERR             ;If yes, then have a syntax error
           MVI	L, 120o               ;If none of the above, then minus sign is unary, put
           MVI	M, 001                ;Character string representing the
           INR	L                    ;Value zero in the SYMBOL BUFFER in string format
           MVI	M, 260o               ;(Character count (cc) followed by ASCII code for zero)
SCAN3:     MVI	L, 176o               ;Set L to address of PARSER TOKEN storage location
           MVI	M, 002                ;Set PARSER TOKEN value for minus operator
SCANFN:    CALL	PARSER             ;Call the PARSER subroutine to process current symbol
           JMP	SCAN10             ;And operator. Then jump to continue processing.
SCAN4:     CPI	252o               ;See if character fetched from expression is
           JNZ	SCAN5              ;If not, continue checking for an operator
           MVI	L, 176o               ;If yes, set pointer to PARSER TOKEN storage location
           MVI	M, 003                ;Place TOKEN value for "*" (multiplication) operator in
           JMP	SCANFN             ;PARSER TOKEN and go to PARSER subroutine entry
SCAN5:     CPI	257o               ;See if character fetched from expression is
           JNZ	SCAN6              ;If not, continue checking for an operator
           MVI	L, 176o               ;If yes, set pointer to PARSER TOKEN storage location
           MVI	M, 004                ;Place TOKEN value for "/" (division) operator in
           JMP	SCANFN             ;PARSER TOKEN and go to PARSER subroutine entry
SCAN6:     CPI	250o               ;See if character fetched from expression is
           JNZ	SCAN7              ;If not, continue checking for an operator
           MVI	L, 230o               ;If yes, load L with address of FUN/ARRAY STACK
           MOV	B,M                    ;Pointer. Fetch the value in the stack pointer. Increment
           INR	B                    ;It to indicate number of "(" operators encountered.
           MOV	M,B                    ;Restore the updated stack pointer back to memory
           CALL	FUNARR             ;Call subroutine to process possible FUNCTION or
           MVI	L, 176o               ;ARRAY variable subscript. Ihen set pointer to
           MVI	M, 006                ;PARSER TOKEN storage and set value for operator
           JMP	SCANFN             ;Go to PARSER subroutine entry point.
SCAN7:     CPI	251o               ;See if character fetched from expression is
           JNZ	SCAN8              ;If not, continue checking for an operator
           MVI	L, 176o               ;If yes, load L with address of PARSER TOKEN
           MVI	M, 007                ;Set PARSER TOKEN value to reflect ")"
           CALL	PARSER             ;Call the  PARSER subroutine to process current symbol

           CALL	PRIGHT             ;Call subroutine to handle FUNCTION or ARRAY
           MVI	L, 230o               ;Load L with address of FUN/ARRAY STACK pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of FUN/ARRAY STACK pointer
           MOV	B,M                    ;Fetch the value in the stack pointer. Decrement it
           DCR	B                    ;To account for left parenthesis just processed.
           MOV	M,B                    ;Restore the updated value back to memory.
           JMP	SCAN10             ;Jump to continue processing expression.
SCAN8:     CPI	336o               ;See if character fetched from expression is " t
           JNZ	SCAN9              ;If not, continue checking for an operator
           MVI	L, 176o               ;If yes, load L with address of PARSER TOKEN
           MVI	M, 005                ;Put in value for exponentiation
           JMP	SCANFN             ;Go to PARSER subroutine entry point.
SCAN9:     CPI	274o               ;See if character fetched is the "less than" sign
           JNZ	SCAN11             ;If not, continue checking for an operator
           MVI	L, 200o               ;If yes, set L to the EVAL CURRENT pointer
           MOV	B,M                    ;Fetch the pointer
           INR	B                    ;Increment it to point to the next character
           MOV	M,B                    ;Restore the updated pointer value
           CALL	GETCHR             ;Fetch the next character in the expression
           CPI	275o               ;Is the character the "= 9 $ sign?
           JZ	SCAN13             ;If so, have 'less than or equal" combination
           CPI	276o               ;Is the character the "greater than" sign?
           JZ	SCAN15             ;If so, have "less than or greater than" combination
           MVI	L, 200o               ;Else character is not part of the operator. Set L back
           MOV	B,M                    ;To the EVAL CURRENT pointer. Fetch the pointer
           DCR	B                    ;Value and decriment it back one character in the
           MOV	M,B                    ;Expression. Restore the original pointer value.
           MVI	L, 176o               ;Have just the 'less than" operator. Set L to the
           MVI	M, 011o               ;PARSER TOKEN storage location and set the value for
           JMP	SCANFN             ;The 'less than" sign then go to PARSER entry point.
SCAN11:    CPI	275o               ;See if character fetched is the "= " sign
           JNZ	SCAN12             ;If not, continue checking for an operator
           MVI	L, 200o               ;If yes, set L to the EVAL CURRENT pointer
           MOV	B,M                    ;Fetch the pointer
           INR	B                    ;Increment it to point to the next character
           MOV	M,B                    ;Restore the updated pointer value
           CALL	GETCHR             ;Fetch the next character in the expression
           CPI	274o               ;Is the character the "less than" sign?
           JZ	SCAN13             ;If so, have "less than or equal" combination
           CPI	276o               ;Is the character the "greater than" sign?
           JZ	SCAN14             ;If so, have "equal or greater than" combination
           MVI	L, 200o               ;Else character is not part of the operator. Set L back
           MOV	B,M                    ;To the EVAL CURRENT pointer. Fetch the pointer
           DCR	B                    ;Value and decrement it back one character in the
           MOV	M,B                    ;Expression. Restore the original pointer value.
           MVI	L, 176o               ;Just have '~-- " operator. Set L to the PARSER TOKEN
           MVI	M, 012o               ;Storage location and set the value for the sign.
           JMP	SCANFN             ;Go to the PARSER entry point.
SCAN12:    CPI	276o               ;See if character fetched is the "greater than" sign
           JNZ	SCAN16             ;If not, go append the character to the SYMBOL BUFF
           MVI	L, 200o               ;If so, set L to the EVAL CURRENT pointer
           MOV	B,M                    ;Fetch the pointer
           INR	B                    ;Increment it to point to the next character
           MOV	M,B                    ;Restore the updated pointer value
           CALL	GETCHR             ;Fetch the next character in the expression
           CPI	274o               ;Is the character the "less than" sign?
           JZ	SCAN15             ;If so, have "less than or greater than" combination
           CPI	275o               ;Is the character the "= " sign?
           JZ	SCAN14             ;If so, have the "equal to or greater than " combination
           MVI	L, 200o               ;Else character is not part of the operator. Set L back
           MOV	B,M                    ;To the EVAL CURRENT pointer. Fetch the pointer
           DCR	B                    ;Value and decrement it back one character in the
           MOV	M,B                    ;Expression. Restore the original pointer value.
           MVI	L, 176o               ;Have just the "greater than" operator. Set L to the
           MVI	M, 013o               ;PARSER TOKEN storage location and set the value for
           JMP	SCANFN             ;The "greater than" sign then go to PARSER entry
SCAN13:    MVI	L, 176o               ;When have 'less than or equal" combination set L to
           MVI	M, 014o               ;PARSER TOKEN storage location and set the value.
           JMP	SCANFN             ;Then go to the PARSER entry point.
SCAN14:    MVI	L, 176o               ;When have "equal to or greater than" combination set L
           MVI	M, 015o               ;To PARSER TOKEN storage location and set the value.
           JMP	SCANFN             ;Then go to the PARSER entry point.
SCAN15:    MVI	L, 176o               ;When have 'less than or greater than" combination set
           MVI	M, 016o               ;L to PARSER TOKEN storage location and set value.
           JMP	SCANFN             ;Then go to the PARSER entry point.
SCAN16:    CALL	CONCTS             ;Concatenate the character to the SYMBOL BUFFER
SCAN10:    MVI	L, 200o               ;Set L to the EVAL CURRENT pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of EVAL CURRENT pointer
           MOV	B,M                    ;Fetch the EVAL CURRENT pointer value into B
           INR	B                    ;Increment the pointer value to point to next character
           MOV	M,B                    ;In the expression and restore the updated value.
           MVI	L, 277o               ;Set L to EVAL FINISH storage location.
           MOV	A,M                    ;Fetch the EVAL FINISH value into the accumulator.
           DCR	B                    ;Set B to last character processed in the expression.
           CMP	B                    ;See if last character was at EVAL FINISH location.
           JNZ	SCAN1              ;If not, continue processing the expression. Else, jump
           JMP	PARSEP             ;To final evaluation procedure and test.  (Directs routine
           HLT             ;To a dislocated section.) Safety Halt in unused byte.
PARSER:    MVI	L, 120o               ;Load L with starting address of SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SYMBOL BUFFER
           MOV	A,M                    ;Fetch the (cc) for  contents of SYMBOL BUFFER
           ANA	A                    ;Into the ACC and see if buffer is  empty
           JZ	PARSE              ;If empty then no need to convert contents
           INR	L                    ;If not empty, advance buffer pointer
           MOV	A,M                    ;Fetch the first character in the buffer
           CPI	256o               ;See if it is ASCII code for decimal sign
           JZ	PARNUM             ;If yes, consider contents of buffer to be a number
           CPI	260o               ;If not decimal sign, see if first character represents
           JM	LOOKUP             ;A deciinal digit, if not, should have a variable
           CPI	272o               ;Continue to test for a decimal digit
           JP	LOOKUP             ;If not, go look up the variable nwne
PARNUM:    DCR	L                    ;If SYMBOL BUFFER contains number, decrement
           MOV	A,M                    ;Buffer pointer back to (cc) and fetch it to ACC
           CPI	001o               ;See if length of string in buffer is just one
           JZ	NOEXPO             ;If so, cannot have number with scientific notation
           ADD	L                    ;If not, add length to buffer pointer to
           MOV	L,A                    ;Point to last character in the buffer
           MOV	A,M                    ;Fetch the last character in buffer and see if it
           CPI	305o               ;Represents letter E for Exponent
           JNZ	NOEXPO             ;If not, cannot have number with scientific notation
           MVI	L, 200o               ;If yes, have part of a scientific number, set pointer to
           CALL	GETCHR             ;Get the operator that follows the E and append it to
           JMP	CONCTS             ;The SYMBOL BUFFER and return to EVAL routine
NOEXPO:    MVI	L, 227o               ;Load L with address of ARITHMETIC STACK pointer
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of ARITHMETIC STACK pointer
           MOV	A,M                    ;Fetch AS pointer value to ACC and add four to account
           ADI	004o               ;For the number of bytes required to store a number in
           MOV	M,A                    ;Floating point format. Restore pointer to mernory.
           MOV	L,A                    ;Then, change L to point to entry position in the AS
           CALL	FSTORE             ;Place contents of the FPACC onto top of the AS
           MVI	L, 120o               ;Change L to point to start of the SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the SYMBOL BUFFER
           CALL	DINPUT             ;Convert number in the buffer to floating point format
           JMP	PARSE              ;In the FPACC then jump to check operator sign.
LOOKUP:    MVI	L, 370o               ;Load L with address of LOOK-UP COUNTER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of the counter
           MVI	M, 000                ;Initialize the counter to zero
           MVI	L, 120o               ;Load L with starting address of the SYMBOL BUFFER
           MVI	D,PG27 ;\HB\OLDPG27   ;** Load D with page of the VARIABLES TABLE
           MVI	E, 210o               ;Load E with start of the VARL433LES TABLE
           MOV	A,M                    ;Fetch the (cc) for the string in the SYMBOL BUFFER
           CPI	001                ;See if the name length is just one character. If not,
           JNZ	LOOKU1             ;Should be two so proceed to look-up routine. Else,
           MVI	L, 122o               ;Change L to second character byte in the buffer and set
           MVI	M, 000                ;It to zero to provide compatibility with entries in table
LOOKU1:    MVI	L, 121o               ;Load L with addr of first character in the SYMBOL
           MVI	H,PG26 ;\HB\OLDPG26   ;** BUFFER. Set H to page of the SYMBOL BUFFER.
           CALL	SWITCH             ;Exchange contents of D&E with H&L so that can
           MOV	A,M                    ;Fetch the first character of a name in the VARIABLES
           INR	L                    ;TABLE. Advance the table pointer and save the
           MOV	B,M                    ;Second byte of name in B. Then advance the pointer
           INR	L                    ;Again to reach first bvte of floating point forrnatted
           CALL	SWITCH             ;Number in table. Now exchange D&E with H&L and
           CMP	M                    ;Compare first byte in table against first char in buffer
           JNZ	LOOKU2             ;If not the same, go try next entry in table. If same,
           INR	L                    ;Advance pointer to next char in buffer. Transfer the
           MOV	A,B                    ;Character in B (second byte in table entry) to the ACC
           CMP	M                    ;Compare it against second character in the buffer.
           JZ	LOOKU4             ;If match, have found the name in the VARIABLES tbl.
LOOKU2:    CALL	AD4DE              ;Call subroutine to add four to the pointer in D&E to
           MVI	L, 370o               ;Advance the table pointer over value bytes. Then set
           MVI	H,PG26 ;\HB\OLDPG26   ;** Up H and L to point to LOOK-UP COUNTER.
           MOV	B,M                    ;Fetch counter value (counts number of entries tested
           INR	B                    ;In the VARIABLES TABLE), increment it
           MOV	M,B                    ;And restore it back to meynory
           MVI	L, 077o               ;Load L with address of SYMBOL VARIABLES counter
           MVI	H,PG27 ;\HB\OLDPG27   ;** Do same for H. (Counts number of names in table.)
           MOV	A,B                    ;Place LOOK-UP COUNTER value in the accumulator.
           CMP	M                    ;Compare it with number of entries in the table.
           JNZ	LOOKU1             ;If have not reached end of table, keep looking for name.
           MVI	L, 077o               ;If reach end of table without match, need to add name
           MVI	H,PG27 ;\HB\OLDPG27   ;** To table. First set H & L to the SYMBOL
           MOV	B,M                    ;VARIABLES counter. Fetch the counter value and
           INR	B                    ;Increment to account for new name being added to the
           MOV	M,B                    ;Table. Restore the updated count to meinory. Also,
           MOV	A,B                    ;Move the new counter value to the accumulator and
           CPI	025o               ;Check to see that table size is not exceeded. If try to
           JP	BIGERR             ;Go over 20 (decirnal) entries then have BIG error.
           MVI	L, 121o               ;Else, set L to point to first character in the SYMBOL
           MVI	H,PG26 ;\HB\OLDPG26   ;** BUFFER and set H to proper page. Set the number
           MVI	B, 002                ;Of bytes to be transferred into register B as a counter.
           CALL	MOVEIT             ;Move the symbol name from the buffer to the
           MOV	L,E                    ;VARIABLES TABLE. Now set up H & L with value
           MOV	H,D                    ;Contained in D & E after moving ops (points to first
           XRA	A                    ;Byte of the value to be associated with the symbol
           MOV	M,A                    ;Name.) Clear the accumulator and place zero in all four
           INR	L                    ;Bytes associated with the variable name entered
           MOV	M,A                    ;In the VARIABLES TABLE
           INR	L                    ;In order to
           MOV	M,A                    ;Assign an
           INR	L                    ;Initial value
           MOV	M,A                    ;To the variable narne
           MOV	A,L                    ;Then transfer the address in L to the acc'umulator
           SUI	004                ;Subtract four to reset the pointer to start of zeroing ops
           MOV	E,A                    ;Restore the address in D & E to be in same state as if
           MOV	D,H                    ;Name was found in the table in the LOOKUP routine
LOOKU4:    CALL	SAVEHL             ;Save current address to VARIABLES TABLE
           MVI	L, 227o               ;Load L with address of ARITHMETIC STACK pointer
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of the pointer
           MOV	A,M                    ;Fetch the AS pointer value to the accumulator
           ADI	004                ;Add four to account for next floating point forrnatted
           MOV	M,A                    ;Number to be stored in the stack. Restore the stack
           MOV	L,A                    ;Pointer to memory and set it up in register L too.
           CALL	FSTORE             ;Place the value in the FPACC on the top of the
           CALL	RESTHL             ;ARITHMETIC STACK. Restore the VARIABLES
           CALL	SWITCH             ;TABLE pointer to H&L and move it to D&E. Now load
           CALL	FLOAD              ;The VARIABLE value from the table to the FPACC.
PARSE:     CALL	CLESYM             ;Clear the SYMBOL BUFFER
           MVI	L, 176o               ;Load L with address of PARSER TOKEN VALUE
           MOV	A,M                    ;And fetch the token value into the accumulator
           CPI	007                ;Is it token value for right parenthesis ")" ? If so, have
           JZ	PARSE2             ;Special case where must perforin ops til find a "(" !
           ADI	240o               ;Else, fon-n address to HEIRARCHY IN table and
           MOV	L,A                    ;Set L to point to HEIRARCHY IN VALUE in the table
           MOV	B,M                    ;Fetch the heirarchy value from the table to register B
           MVI	L, 210o               ;Set L to OPERATOR STACK pointer storage location
           MOV	C,M                    ;Fetch the OS pointer into CPU register C
           CALL	INDEXC             ;Add OS pointer to address of OS pointer storage loc
           MOV	A,M                    ;Fetch the token value for the operator at top of the OS
           ADI	257o               ;And form address to HEIRARCHY OUT table
           MOV	L,A                    ;Set L to point to HEIRARCHY OUT VALUE in the
           MOV	A,B                    ;Table. Move the HEIRARCHY IN value to the ACC.
           CMP	M                    ;Compare the HEIRARCHY IN with the HEIRARCHY
           JZ	PARSE1             ;OUT value. If heirarchy of current operator equal to or
           JM	PARSE1             ;Less than operator on top of OS stack, perfo
           MVI	L, 176o               ;Operation indicated in top of OS stack. Else, fetch the
           MOV	B,M                    ;Current operator token value into register B.
           MVI	L, 210o               ;Load L with address of the OPERATOR STACK pntr
           MOV	C,M                    ;Fetch the stack pointer value
           INR	C                    ;Increment it to account for new entry on the stack
           MOV	M,C                    ;Restore the stack pointer value to memory
           CALL	INDEXC             ;For in pointer to next entry in OPERATOR STACK
           MOV	M,B                    ;Place the current operator token value on top of the OS
           RET                    ;Exit back to the EVAL routine.
PARSE1:    MVI	L, 210o               ;Load L with address of the OPERATOR STACK pntr
           MOV	A,M                    ;Fetch the stack pointer value to the accumulator
           ADD	L                    ;Add in the value of the stack pointer address to form
           MOV	L,A                    ;Address that points to top entry in the OS
           MOV	A,M                    ;Fetch the token value at the top of the OS to the ACC
           ANA	A                    ;Check to see if the token value is zero for end of stack
           RZ                    ;Exit back to the EVAL routine if stack empty
           MVI	L, 210o               ;Else, reset L to the OS pointer storage location
           MOV	C,M                    ;Fetch the pointer value
           DCR	C                    ;Decrement it to account for operator rernoved from
           MOV	M,C                    ;The OPERATOR STACK and restore the pointer value
           CALL	FPOPER             ;Perform the operation obtained from the top of the OS
           JMP	PARSE              ;Continue to compare current operator against top of OS
PARSE2:    MVI	L, 210o               ;Load L with address of the OPERATOR STACK pntr
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of the pointer
           MOV	A,M                    ;Fetch the stack pointer value to the accumulator
           ADD	L                    ;Add in the value of the stack pointer address to form
           MOV	L,A                    ;Address that points to top entry in the OS
           MOV	A,M                    ;Fetch the token value at the top of the 0 S to the ACC
           ANA	A                    ;Check to see if the token value is zero for end of stack
           JZ	PARNER             ;If end of stack, then have a parenthesis error condx
           MVI	L, 210o               ;Else, reset L to the OS pointer storage location
           MOV	C,M                    ;Fetch the pointer value
           DCR	C                    ;Decrement it to account for operator removed from
           MOV	M,C                    ;The OPERATOR STACK and restore the pointer value
           CPI	006                ;Check to see if token value is "(" to close parenthesis
           RZ                    ;If so, exit back to EVAL routine.
           CALL	FPOPER             ;Else, perforin the op obtained from the top of the OS
           JMP	PARSE2             ;Continue to process data in parenthesis
FPOPER:    MVI	L, 371o               ;Load L with address of TEMP OP storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of TEMP OP storage location
           MOV	M,A                    ;Store OP (from top of OPERATOR STACK)
           MVI	L, 227o               ;Change L to address of ARff HMETIC STACK pointer
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of AS pointer
           MOV	A,M                    ;Fetch AS pointer value into ACC
           MOV	L,A                    ;Set L to top of ARITHMETIC STACK
           CALL	OPLOAD             ;Transfer number from ARffHMETIC STACK to FPOP
           MVI	L, 227o               ;Restore pointer to AS pointer
           MOV	A,M                    ;Fetch the pointer value to the ACC and subtract four
           SUI	004                ;To remove top value from the ARITHMETIC STACK
           MOV	M,A                    ;Restore the updated AS pointer to memory
           MVI	L, 371o               ;Set L to address of TEMP OP storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of TEMP OP storage location
           MOV	A,M                    ;Fetch the operator token value to the ACC
           CPI	001                ;Find out which kind of operation indicated
           JZ	FPADD              ;Perforn addition if have plus operator
           CPI	002                ;If not plus, see if minus
           JZ	FPSUB              ;Perform subtraction if have minus operator
           CPI	003                ;If not minus, see if multiplication
           JZ	FPMULT             ;Perform multiplication if have multiplication operator
           CPI	004                ;If not multiplication, see if division
           JZ	FPDIV              ;Perform division if have division operator
           CPI	005                ;If not division, see if exponentiation
           JZ	INTEXP             ;Perform exponentiation if have exponentiation operator
           CPI	011o               ;If not exponentiation, see if "less than" operator
           JZ	LT                 ;Perform compaison for "less than" op if indicated
           CPI	012o               ;If not 'less than" see if have "equal" operator
           JZ	EQ                 ;Perforin comparison for "equal" op if indicated
           CPI	013o               ;If not "equal" see if have "greater than" operator
           JZ	GT                 ;Perform comparison for "greater than" op if indicated
           CPI	014o               ;If not "'greater than" see if have 'less than or equal" op
           JZ	LE                 ;Perform comparison for the combination op if indicated
           CPI	015o               ;See if have "equal to or greater than" operator
           JZ	GE                 ;Perform comparison for the combination op if indicated
           CPI	016o               ;See if have "less than or greater than" operator
           JZ	NE                 ;Perform comparison for the combination op if indicated
PARNER:    MVI	L, 230o               ;If cannot find operator, expression is not balanced
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H and L to address of F/A STACK pointer
           MVI	M, 000                ;Clear the F/A STACK pointer to re-initialize
           MVI	A, 311o               ;Load ASCII code for letter I into the accumulator
           MVI	C, 250o               ;And code for "(" character into register C
           JMP	ERROR              ;Go display 1( for "Imbalanced Parenthesis") error msg
LT:        CALL	FPSUB              ;Subtract contents of FPACC from FPOP to compare
           MVI	L, 126o               ;Set L to point to the MSW of the FPACC (Contains
           MOV	A,M                    ;Result of the subtraction.) Fetch the MSW of the
           ANA	A                    ;FPACC to the accumulator and test to see if result is
           JM	CTRUE              ;Positive or negative. Set up the FPACC as a function
           JMP	CFALSE             ;Of the result obtained.
EQ:        CALL	FPSUB              ;Subtract contents of FPACC from FPOP to compare
           MVI	L, 126o               ;Set L to point to the MSW of the FPACC (Contains
           MOV	A,M                    ;Result of the subtraction.) Fetch the MSW of the
           ANA	A                    ;FPACC to the accumulator and test to see if result is
           JZ	CTRUE              ;Equal. Set up the FPACC as a function
           JMP	CFALSE             ;Of the result obtained.
GT:        CALL	FPSUB              ;Subtract contents of FPACC from FPOP to compare
           MVI	L, 126o               ;Set L to point to the MSW of the FPACC (Contains
           MOV	A,M                    ;Result of the subtraction.) Fetch the MSW of the
           ANA	A                    ;FPACC to the accumulator and test to see if result is
           JZ	CFALSE             ;Positive, Negative, or Equal. Set up the FPACC
           JP	CTRUE              ;As a function
           JMP	CFALSE             ;Of the result obtained.
LE:        CALL	FPSUB              ;Subtract contents of FPACC from FPOP to compare
           MVI	L, 126o               ;Set L to point to the MSW of the FPACC (Contains
           MOV	A,M                    ;Result of the subtraction.) Fetch the MSW of the
           ANA	A                    ;FPACC to the accumulator and test to see if result is
           JZ	CTRUE              ;Positive, Negative, or Equal. Set up the FPACC
           JM	CTRUE              ;As a function
           JMP	CFALSE             ;Of the result obtained
GE:        CALL	FPSUB              ;Submit contents of FPACC from FPOP to compare
           MVI	L, 126o               ;Set L to point to the MSW of the FPACC (Contains
           MOV	A,M                    ;Result of the subtraction.) Fetch the MSW of the
           ANA	A                    ;FPACC to the accumulator and test to see if result is
           JP	CTRUE              ;Positive or Negative. Set up the FPACC
           JMP	CFALSE             ;As a function of the result obtained
NE:        CALL	FPSUB              ;Subtract contents of FPACC from FPOP to compare
           MVI	L, 126o               ;Set L to point to the MSW of the FPACC (Contains
           MOV	A,M                    ;Result of the subtraction.) Fetch the MSW of the
           ANA	A                    ;FPACC to the accumulator and test to see if result is
           JZ	CFALSE             ;Equal. Set up the FPACC as a function of the result.
CTRUE:
FPONE:     MVI	L, 004                ;Load L with address of floating point value +1.0
           JMP	FLOAD              ;Load FPACC with value +1.0 and exit to caller
CFALSE:    MVI	L, 127o               ;Load L with address of FPACC Exponent register
           MVI	M, 000                ;Set the FPACC Exponent to zero and then set the
           JMP	FPZERO             ;Mantissa portion of the FPACC to zero. Exit to caller.
AD4DE:     MOV	A,E                    ;Subroutine to add four to the value in register E.
           ADI	004                ;Move contents of E to the ACC and add four.
           MOV	E,A                    ;Restore the updated value back to register E.
           RET                    ;Return to the caMVI L,ng routine.
INTEXP:    MVI	L, 126o               ;Load L with address of WSW of FPACC (Floating Point
           MVI	H,PG01 ;\HB\OLDPG1    ;** ACCumulator). Load H with page of FPACC.
           MOV	A,M                    ;Fetch MSW of the FPACC into the accumulator.
           MVI	L, 003                ;Load L with address of EXP TEMP storage location
           MOV	M,A                    ;Store the FPACC MSW value in EXP TEMP location
           ANA	A                    ;Test contents of the MSW of the FPACC. ff zero, then
           JZ	FPONE              ;Set FPACC equal to +1.0 (any nr to zero power = 1.0!)
           CM	 FPCOMP             ;If MSW indicates negative number, complement
           CALL	FPFIX              ;The FPACC. Then convert floating point number to
           MVI	L, 124o               ;Fixed point. Load L with address of LSW of fixed nr
           MOV	B,M                    ;Fetch the LSW into CPU register B.
           MVI	L, 013o               ;Set L to address of EXPONENT COUNTER
           MOV	M,B                    ;Place the fixed value in the EXP CNTR to indicate
           MVI	L, 134o               ;Number of multiplications needed (power). Now set L
           MVI	E, 014o               ;To LSW of FPOP and E to address of FP TEMP (LSW)
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to floating point working area page.
           MOV	D,H                    ;Set D to same page address.
           MVI	B, 004o               ;Set transfer (precision) counter. Call subroutine to move
           CALL	MOVEIT             ;Contents of FPOP into FP TEMP registers to save
           CALL	FPONE              ;Original value of FPOP. Now set FPACC to +1.0.
           MVI	L, 003                ;Load L with pointer to original value of FPACC
           MOV	A,M                    ;(Stored in FP TEMP) MSW and fetch contents to ACC.
           ANA	A                    ;Test to see if raising to a negative power. If so, divide
           JM	DVLOOP             ;Instead of multiply!
MULOOP:    MVI	L, 014o               ;Load L with address of LSW of FP TEMP (original
           CALL	FACXOP             ;Value in FPOP). Move FP TEMP into FPOP.
           CALL	FPMULT             ;Multiply FPACC by FPOP. Result left in FPACC.
           MVI	L, 013o               ;Load L with address of EXPONENT COUNTER.
           MOV	B,M                    ;Fetch the counter value
           DCR	B                    ;Decrement it
           MOV	M,B                    ;Restore it to memory
           JNZ	MULOOP             ;If counter not zero, continue exponentiation process
           RET                    ;When have raised to proper power, return to caller.
DVLOOP:    MVI	L, 014o               ;Load L with address of LSW of FP TEMP (original
           CALL	FACXOP             ;Value in FPOP). Move FP TEMP into FPOP.
           CALL	FPDIV              ;Divide FPACC by FPOP. Result left in FPACC.
           MVI	L, 013o               ;Load L with address of EXPONENT COUNTER
           MOV	B,M                    ;Fetch the counter value
           DCR	B                    ;Decrement it
           MOV	M,B                    ;Restore to memory
           JNZ	DVLOOP             ;If counter not zero, continue exponentiation process
           RET                    ;When have raised to proper power, return to caller.

;;; The label PRIGHT: SHOULD BE UP TO 07 003 0703h
PRIGHT:    MVI	L, 230o               ;Load L with address of F/A STACK pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of F/A STACK pointer
           MOV	A,M                    ;Fetch the pointer value into the ACC
           ADD	L                    ;Form pointer to top of the F/A STACK
           MOV	L,A                    ;Set L to point to top of the F/A STACK
           MOV	A,M                    ;Fetch the contents of the top of the F/A STACK into
           MVI	M, 000                ;The ACC then clear the top of the F/A STACK
           MVI	L, 203o               ;Load L with address of F /A STACK TEMP storage
           MVI	H,PG27 ;\HB\OLDPG27   ;** Location. Set H to page of F/A STACK TEMP
           MOV	M,A                    ;Store value from top of F/A STACK into temp loc.
           ANA	A                    ;Test to see if token value in top of stack was zero
           RZ                    ;If so, just had simple grouping parenthesis!
           JM	PRIGH1             ;@@ If token value minus, indicates array subscript
           CPI	001                ;For positive token value, look for appropriate function
           JZ	INTX               ;If token value for INTeger function, go do it.
           CPI	002                ;Else, see if token value for SIGN function.
           JZ	SGNX               ;If so, go do it.
           CPI	003                ;Else, see if token value for ABSolute function
           JZ	ABSX               ;If so, go do it.
           CPI	004                ;If not, see if token value for SQuare Root function
           JZ	SQRX               ;If so, go do it.
           CPI	005                ;If not, see if token value for TAB function
           JZ	TABX               ;If so, go do it.
           CPI	006                ;If not, see if token value for RaNDom function
           JZ	RNDX               ;If so, go find a random number.
           CPI	007                ;If not, see if token value for CHaRacter function
           JZ	CHRX               ;If so, go perform the function.
           CPI	010o               ;Else, see if token for user defined machine language
           JZ	UDEFX              ;# Function. If so, perform the User DEfined Function
           HLT             ;Safety halt. Program should not reach this location!

;;; The label FUNARR SHOULD BE AT 07 100 0740h
FUNARR:    MVI	L, 120o               ;Load L with starting address of SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SYMBOL BUFFER
           MOV	A,M                    ;Fetch the (cc) for contents of buffer to the ACC
           ANA	A                    ;See if (cc) is zero, if so buffer is empty, return to
           RZ                    ;Caller as have simple grouping parenthesis sign
           MVI	L, 202o               ;Else set L to TEMP COUNTER location
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to TEMP COUNTER page
           MVI	M, 000                ;Initialize TEMP COUNTER to zero
FUNAR1:    MVI	L, 202o               ;Load L with address of TEMP COUNTER
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of TEMP COUNTER
           MOV	B,M                    ;Fetch the counter value to register B
           INR	B                    ;Increment the counter
           MOV	M,B                    ;Restore the updated value to memory
           MVI	C, 002                ;Initialize C to a value of two for future ops
           MVI	L, 274o               ;Load L with starting address (less four) of FUNCTION
           MVI	H,PG26 ;\HB\OLDPG26   ;** LOOK-UP TABLE. Set H to table page.
           CALL	TABADR             ;Find address of next entry in the table
           MVI	D,PG26 ;\HB\OLDPG26   ;** Load D with page of SYMBOL BUFFER
           MVI	E, 120o               ;Load E with starting address of SYMBOL BUFFER
           CALL	STRCP              ;Compare entry in FUNCTION LOOK-UP TABLE with
           JZ	FUNAR4             ;Contents of SYMBOL BUFFER. If find match, go set
           MVI	L, 202o               ;Up the function token value. Else, set L to the TEMP
           MVI	H,PG27 ;\HB\OLDPG27   ;** COUNTER and set H to the proper page. Fetch the
           MOV	A,M                    ;Current counter value and see if have tried all eight
           CPI	010o               ;Possible functions in the table.
           JNZ	FUNAR1             ;If not, go back and check the next entry.
           MVI	L, 202o               ;If have tried all of the entries in the table, set L
           MVI	H,PG27 ;\HB\OLDPG27   ;** As well as H to the address of the TEMP COUI,.7ER
           MVI	M, 000                ;And reset it to zero. Now go see if have subscripted
           JMP	FUNAR2             ;@@ Array (unless array capability not in program).
FAERR:     MVI	L, 230o               ;Load L with address of F/A STACK pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of F/A STACK pointer
           MVI	M, 000                ;Clear the F/A STACK pointer to reset on an error
           MVI	A, 306o               ;Load the ASCII code for letter F into the ACC
           MVI	C, 301o               ;Load the ASCII code for letter A into register C
           JMP	ERROR              ;Go display the FA error message
FUNAR4:    MVI	L, 202o               ;Load L with address of TEMP COUNTER
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of TEMP COUNTER
           MOV	B,M                    ;Load value in counter to register B. This is FUNCTION
           MVI	L, 230o               ;TOKEN VALUE. Cbange- L to F/A STACK pointer.
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of F/A STACK pointer.
           MOV	C,M                    ;Fetch the F/A STACK pointer value into register C.
           CALL	INDEXC             ;Form the address to the top of the F/A STACK.
           MOV	M,B                    ;Store the FUNCTION TOKEN VALUE in the F/A
           JMP	CLESYM             ;STACK. Then exit by clearing the SYMBOL BUFFER.
TABADR:    MOV	A,B                    ;Move the TEMP COUNTER value from B to ACC
TABAD1:    RLC                    ;Multiply by four using this loop to form value equal
           DCR	C                    ;To number of bytes per entry (4) times current entry
           JNZ	TABAD1             ;In the FUNCTION LOOK-UP TABLE.
           ADD	L                    ;Add this value to the starting address of the table.
           MOV	L,A                    ;Form pointer to next entry in table
           RNC                    ;If no carry return to caller
           INR	H                    ;Else, increment H before
           RET                    ;Returning to caller

;;; The label INTX SHOULD BE AT 07 243 07a3h
INTX:      MVI	L, 126o               ;Load L with address of MSW of the FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with the page of the PPACC
           MOV	A,M                    ;Fetch the MSW of the FPACC into the accumulator
           ANA	A                    ;Test the sign of the number in the FPACC. If
           JP	INT1               ;Positive jump ahead to integerize
           MVI	L, 014o               ;If negative, load L with address of FP TEMP registers
           CALL	FSTORE             ;Store the value in the FPACC in FP TEMP
           CALL	FPFIX              ;Convert the value in FPACC from floating point to
           MVI	L, 123o               ;Fixed point. Load L with address of FPACC
           MVI	M, 000                ;Extension register and clear it.
           CALL	FPFLT              ;Convert fixed binary back to FP to integerize
           MVI	L, 014o               ;Load L with address of FP TEMP registers
           CALL	OPLOAD             ;Load the value in FP TEMP into FPOP
           CALL	FPSUB              ;Subtract integerized value from original
           MVI	L, 126o               ;Set L to address of MSW of FPACC
           MOV	A,M                    ;Fetch the MSW of the FPACC into the accumulator
           ANA	A                    ;See if original value and integerized value the same
           JZ	INT2               ;If so, have integer value in FP TEMP
           MVI	L, 014o               ;Else, load L with address of FP TEMP registers
           CALL	FLOAD              ;Restore FPACC to original (non-integerized) value
           MVI	L, 024o               ;Set L to register containing small value
           CALL	FACXOP             ;Set up to add small value to original value in FPACC
           CALL	FPADD              ;Perform the addition
INT1:      CALL	FPFIX              ;Convert the number in FPACC from floating point
           MVI	L, 123o               ;To fixed point. Load L with address of FPACC
           MVI	M, 000                ;Extension register and clear it. Now convert the number
           JMP	FPFLT              ;Back to floating point to integerize it and exit to caller
INT2:      MVI	L, 014o               ;Load L with address of FP TEMP registers. Transfer
           JMP	FLOAD              ;Number from FP TEMP (orig) to FPACC and return.
ABSX:      MVI	L, 126o               ;Load L with address of MSW of the FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of the FPACC
           MOV	A,M                    ;Fetch the MSW of the FPACC into the accumulator
           ANA	A                    ;Test the sign of the number to see if it is positive.
           JM	FPCOMP             ;If negative, complement the number before returning.
           RET                    ;Else, just return with absolute value in the FPACC.
SGNX:      MVI	L, 126o               ;Load L with address of MSW of the FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with the page of the FPACC
           MOV	A,M                    ;Fetch the MSW of the FPACC into the accumulator
           ANA	A                    ;Test to see if the FPACC is zero
           RZ                    ;Return to caller if FPACC is zero
           JP	FPONE              ;If FPACC is positive, load +1.0 into FPACC and exit
           MVI	L, 024o               ;If FPACC is negative, set up to load -1.0 into the
           JMP	FLOAD              ;FPACC and exit to caller
CHRX:      CALL	FPFIX              ;Convert contents of FPACC from floating point to
           MVI	L, 124o               ;Fixed point. Load L with address of LSW of fixed
           MOV	A,M                    ;Value. Fetch this byte into the accumulator.
           CALL	ECHO               ;Display the value.
           MVI	L, 177o               ;Set L to address of the TAB FLAG
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the TAB FLAG
           MVI	M, 377o               ;Set TAB FLAG (to inhibit display of FP value)
           RET                    ;Exit to caller.
TABX:      CALL	FPFIX              ;Convert contents of FPACC from floating point to
TAB1:      MVI	L, 124o               ;Fixed point. Load L with address of 1,SW of fixed
           MOV	A,M                    ;Value. Fetch this byte into the accumulator.
           MVI	L, 043o               ;Load L with address of COLUMN COUNTER
           SUB	M                    ;Subtract value in C-OLUMN COUNTER from desired
           MVI	L, 177o               ;TAB position. Load L with address of the TAB FLAG.
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the TAB FLAG.
           MVI	M, 377o               ;Set TAB FLAG (to inhibit display of FP value)
           JM	BACKSP             ;If beyond TAB point desired, simulate back spacing
           RZ                    ;Return to caller if at desired TAB location
TABC:      MOV	C,A                    ;Else, put difference count in register C
           MVI	A, 240o               ;Place ASCII code for space in ACC
TABLOP:    CALL	ECHO               ;Display space on output device
           DCR	C                    ;Decrement displacement counter
           JNZ	TABLOP             ;If have not reached TAB position, continue to space
           RET                    ;Else, return to calling routine.

;;; The label STOSYM should be AT 10 055 082dh
STOSYM:    MVI	L, 201o               ;Load L with address of ARRAY FLAG
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of ARRAY FLAG
           MOV	A,M                    ;Fetch the value of the ARRAY FLAG into the ACC
           ANA	A                    ;Check to see if the flag is set indicating processing an
           JZ	STOSY1             ;Array variable value. Jump ahead if flag not set.
           MVI	M, 000                ;If ARRAY FLAG was set, clear it for next time.
           MVI	L, 204o               ;Then load L with address of array address storage loc
           MOV	L,M                    ;Fetch the array storage address as new pointer
           MVI	H,PG57 ;\HB\OLDPG57   ;tt Set H to ARRAY VALUES page   ****************
           JMP	FSTORE             ;Store the array variable value and exit to caller.
STOSY1:    MVI	L, 370o               ;Load L with address of TEMP CNTR
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of TEMP CNTR
           MVI	M, 000                ;Initialize the TEMP CNTR by clearing it
           MVI	L, 120o               ;Load L with starting address of SYMBOL BUFFER
           MVI	D,PG27 ;\HB\OLDPG27   ;** Load D with page of VARIABLES LOOK-UP table
           MVI	E, 210o               ;Load E with starting addr of VARIABLES LOOK-UP
           MOV	A,M                    ;Table. Fetch the (cc) for the SYMBOL BUFFER into
           CPI	001                ;The ACC and see if length of variable name is just one
           JNZ	STOSY2             ;Character. If not, skip next couple of instructions.
           MVI	L, 122o               ;Else, set pointer to second character location in the
           MVI	M, 000                ;SYMBOL BUFFER and set it to zero
STOSY2:    MVI	L, 121o               ;load L with address of first character in the SYMBOL
           MVI	H,PG26 ;\HB\OLDPG26   ;** BUFFER. Load H with page of the buffer.
           CALL	SWITCH             ;Exchange pointer to buffer for pointer to VARIABLES
           MOV	A,M                    ;LOOK-UP table. Fetch first char in a name from the
           INR	L                    ;Table. Advance the pointer to second char in a name.
           MOV	B,M                    ;Fetch the second character into register B.
           INR	L                    ;Advance the pointer to first byte of a value in the table.
           CALL	SWITCH             ;Exchange table pointer for pointer to SYMBOL BUFF
           CMP	M                    ;Compare first character in buffer against first character
           JNZ	STOSY3             ;In table entry. If no match, try next entry in the table.
           INR	L                    ;If match, advance pointer to second character in buffer.
           MOV	A,B                    ;Move second character obtained from table into ACC.
           CMP	M                    ;Compare second characters in table and buffer.
           JZ	STOSY5             ;If same, have found the variable name in the table.
STOSY3:    CALL	AD4DE              ;Add four to pointer in registers D&E to skip over value
           MVI	L, 370o               ;Portion of entry in table. Load L with address of TEMP
           MVI	H,PG26 ;\HB\OLDPG26   ;** CNTR. Load H with page of TEMP CNTR.
           MOV	B,M                    ;Fetch the counter
           INR	B                    ;Increment the counter
           MOV	M,B                    ;Restore it to storage
           MVI	L, 077o               ;Set L to address of VARIABLES CNTR (indicates
           MVI	H,PG27 ;\HB\OLDPG27   ;** Number of variables currently in table.) Set H too
           MOV	A,B                    ;Move the TEMP CNTR value into the ACC. (Number of
           CMP	M                    ;Entries checked.) Compare with number of entries in
           JNZ	STOSY2             ;The table. If have not checked all entries, try next one.
           MVI	L, 077o               ;If have checked all entries, load L with address of the
           MVI	H,PG27 ;\HB\OLDPG27   ;** VARIABLES CNTR. Set H too. Fetch the counter
           MOV	B,M                    ;Value and incrernent it to account for
           INR	B                    ;New variable nwne that will now be
           MOV	M,B                    ;Added to the table. Save the new value.
           MOV	A,B                    ;Place the new counter value into the accumulator
           CPI	025o               ;And check to see that adding new variable name to the
           JP	BIGERR             ;Table will not cause table overflow. Big Error if it does!
           MVI	L, 121o               ;If room available in table, set L to address of first
           MVI	H,PG26 ;\HB\OLDPG26   ;** Caracter in the SYMBOL BUFFER. Set H too.
           MVI	B, 002                ;Set a counter for number of characters to transfer.
           CALL	MOVEIT             ;Move the variable name from buffer to table.
STOSY5:    CALL	SWITCH             ;Exchange buffer pointer for table pointer.
           CALL	FSTORE             ;Transfer new mathematical value into the table.
           JMP	CLESYM             ;Clear the SYMBOL BUFFER and exit to calling routine.

                                  ;The subroutines below are used by some of the routines
                                  ;in this chapter as well as other parts of the program.

SAVESY:    MVI	L, 120o               ;Load L with the address of the start of the SYMBOL
           MVI	H,PG26 ;\HB\OLDPG26   ;** BUFFER. Load H with the page of the buffer.
           MOV	D,H                    ;Load register D with the page of the AUX SYMBOL
           MVI	E, 144o               ;BUFFER and set register E to start of that buffer.
           JMP	MOVECP             ;Transfer SYMBOL BF contents to AUX SYMBOL BF

RESTSY:    MVI	L, 144o               ;Load L with address of start of AUX SYMBOL BUFF
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of AUX SYMBOL BUFFER
           MOV	D,H                    ;Set D to page of SYMBOL BUFFER (same as H)
           MVI	E, 120o               ;Load E with start of SYMBOL BUFFER
MOVECP:    MOV	B,M                    ;Load (cc) for source string (first byte in source buffer)
           INR	B                    ;Add one to (cc) to include (cc) byte itself
           JMP	MOVEIT             ;Move the source string to destination buffer

;;; The label Exec SHOULD BE AT 10 266 (This is the start of the code) 08b6h
EXEC:      MVI	L, 352o               ;Load L with address of READY message
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of READY message
           CALL	TEXTC              ;Call subroutine to display the READY message

EXEC1:     MVI	L, 000                ;Load L with starting address of INPUT LINE BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of INPUT LINE BUFFER
           CALL	STRIN              ;Call subroutine to input a line into the buffer
           MOV	A,M                    ;The STRIN subroutine will exit with pointer set to the
           ANA	A                    ;CHARACTER COUNT for the line inputted. Fetch the
           JZ	EXEC1              ;Value of the counter, if it is zero then line was blank.
           MVI	L, 335o               ;Load L with address of LIST in look up table
           MVI	H,PG01 ;\HB\OLDPG1    ;Load H with address of LIST in look up table
           MVI	D,PG26 ;\HB\OLDPG26   ;Load D with page of line input buffer
           MVI	E, 000                ;Load E with start of line input buffer
           CALL	STRCP              ;Call string compare subroutine to see if first word in
           JNZ	NOLIST             ;Input buffer is LIST. Jump 3 ahead if not LIST.
           MVI	L, 000                ;If LIST, set up pointers to start of USER PROGRAM
           MVI	H, BGNPGRAM           ;BUFFER. (Note user could alter this starting addr)   *****

                                  ;Next portion of program will LIST the contents of the
                                  ;USER PROGRAM BUFFER until an end of buffer
                                  ;(zero byte) indicator is detected.

LIST:      MOV	A,M                    ;Fetch the first byte of a line in the USER PROGRAM
           ANA	A                    ;BUFFER and see if it is zero. If so, have finished LIST
           JZ	EXEC               ;So go back to start of Executive and display READY.
           CALL	TEXTC              ;Else call subroutine to display a line of information
           CALL	ADV                ;Now call subroutine to advance buffer pointer to
           CALL	CRLF               ;Character count in next line. Also display a CR & LF.
           JMP	LIST               ;Continue LISTing process

                                  ;If line inputted by operator did not contain a LIST comman
                                  ;continue program to see if RUN or SCRatch command.

NOLIST:    MVI	L, 342o               ;Load L with address of RUN in look up table
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with address of RUN in look up table
           MVI	E, 000                ;Load E with start of line input buffer
           MVI	D,PG26 ;\HB\OLDPG26   ;** Load D with page of line input buffer
           MVI	E, 000                ;(Reserve 2 locs in case of patching by duplicating above)
           CALL	STRCP              ;Call string compare subroutine to see if first word in
           JZ	RUN                ;Input buffer is RUN. Go to RUN routine if match.
           MVI	D,PG26 ;\HB\OLDPG26   ;** If not RUN command, reset address pointers back
           MVI	E, 000                ;To the start of the line input buffer
           MVI	L, 346o               ;Load L with address of SCR in look up table
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of SCR in look up table
           CALL	STRCP              ;Call string compare subroutine to see if first word in
           JNZ	NOSCR              ;Input buffer is SCR. If not then jump ahead.
ENTRY_SCR: MVI	H,PG26 ;\HB\OLDPG26   ;** If found SCR command then load memory pointer
           MVI	L, 364o               ;With address of a pointer storage location. Set that
           MVI	M, BGNPGRAM           ;tt Storage location to page of start of USER PRO-  *******
           INR	L                    ;GRAM BUFFER. (Buffer start loc may be altered).
           MVI	M, 000                ;Then adv pntr and do same for low addr portion of pntr
           MVI	L, 077o               ;Now set pointer to address of VARIABLES counter
           MVI	H,PG27 ;\HB\OLDPG27   ;** Storage location. Initialize this counter by placing
           MVI	M, 001                ;The count of one into it. Now change the memory pntr
;MGA 3/31/12 put it back to 001; solves nested FOR/NEXT, but limits vars to 19
;   as the letter from James Tucker (1/77) mentioned
;   apparently, James didn't test FOR/NEXT; original Loboyko didn't have this
;;;           LMI 001                ;The count of one into it. Now change the memory pntr
;;; Apparently, in Page 3 of Issue 4 of Scelbal update (1/77) they say the above should change.
;;; This makes the SCR command clear the whole variable space, otherwise one space is lost.  
           MVI	L, 075o               ;To storage location for number of dimensioned arrays
           MVI	M, 000                ;@@ And initialize to zero. (@@ = Substitute NOPs if
           MVI	L, 120o               ;@@ DIMension capability not used in package.) Also
           MVI	M, 000                ;@@ Initialize l'st byte of array name table to zero.
           MVI	L, 210o               ;Set pointer to storage location for the first byte of the
           MVI	M, 000                ;VARIABLES symbol table. Initialize it to zero too.
           INR	L                    ;Advance the pointer and zero the second location
           MVI	M, 000                ;In the Variables table also.
           MVI	H, BGNPGRAM           ;tt Load H with page of start of USER PROGRAM    **********
           MVI	L, 000                ;BUFFER. (Buffer start location could be altered.)
           MVI	M, 000                ;Clear first location to indicate end of user program.
           MVI	H,PG57 ;\HB\OLDPG57   ;@@ Load H with page of ARRAYS storage
SCRLOP:    MVI	M, 000                ;@@ And form a loop to clear out all the locations
           INR	L                    ;@@ On the ARRAYS storage page. (@@ These become
           JNZ SCRLOP             ;@@ NOPs if DIMension capability deleted fm package.)
           JMP EXEC               ;SCRatch operations completed, go back to EXEC.

                                  ;If line inputted did not contain RUN or SCRatch com-
                                  ;mand, program continues by testing for SAVE or LOAD
                                  ;commands. If it does not find either of these com-
                                  ;mands, then operator did not input an executive com-
                                  ;mand. Program then sets up to see if the first entry in
                                  ;the line inputted is a LINE NUMBER.

NOSCR:     MVI	E, 272o               ;Load E with address of SAVE in look up table
           MVI	D,PG01 ;\HB\OLDPG1    ;Load D with page of look up table
           MVI	H,PG26 ;\HB\OLDPG26   ;Load H with page of input line buffer
           MVI	L, 000                ;Set L to start of input line buffer
           CALL	STRCP              ;Call string compare subroutine to see if first word in
           JZ	SAVE               ;tt Input buffer is SAVE. If so, go to user's SAVE rtn
           MVI	L, 277o               ;If not SAVE then load L with address of LOAD in look
           MVI	H,PG01 ;\HB\OLDPG1    ;Up table and load H with page of look up table
           MVI	D,PG26 ;\HB\OLDPG26   ;Load D with page of input line buffer
           MVI	E, 000                ;And L to start of input line buffer
           CALL	STRCP              ;Call string compare subroutine to see if first word in
           JZ	LOAD               ;tt Input buffer is LOAD. If so, go to user's LOAD rtn
           MVI	L, 360o               ;If not LOAD then set pointer to address of storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;** For USER PROGRAM BUFFER pointer. Initialize this
           MVI	M, BGNPGRAM           ;tt Pointer to the starting address of the program buffer.
           INR	L                    ;Advance memory pntr. Since pointer storage requires
           MVI	M, 000                ;Two locations, initialize the low addr portion also.
           CALL	SYNTAX             ;Call the SYNTAX subroutine to obtain a TOKEN indi-
           MVI	L, 203o               ;Cator which will be stored in this location. Upon return
           MVI	H,PG26 ;\HB\OLDPG26   ;** From SYNTAX subroutine set memory pointer to
           MOV	A,M                    ;The TOKEN indicator storage location and fetch the
           ANA	A                    ;Value of the TOKEN. If the value of the syntax TOKEN
           JP	SYNTOK             ;Is positive then have a valid entry.
SYNERR:    MVI	A, 323o               ;However, if SYNTAX returns a negative value TOKEN
           MVI	C, 331o               ;Then have an error condition. Set up the letters SY in
           JMP	ERROR              ;ASCII code and go to display error message to operator.
SYNTOK:    MVI	L, 340o               ;Set pointer to start of LINE NUMBER storage area
           MOV	A,M                    ;First byte there will contain the length of the line
           ANA	A                    ;Number character string. Fetch that value (cc).
           JZ	DIRECT             ;DIRECT If line number blank, have a DIRECT statement!
           MVI	L, 360o               ;If have a line number must get line in input buffer into
           MVI	M, BGNPGRAM           ;tt User program buffer. Initialize pointer to user buffer.
           INR	L                    ;This is a two byte pointer so after initializing page addr
           MVI	M, 000                ;Advance pointer and initialize location on page address

                                  ;If the line in the LINE INPUT BUFFER has a line num-
                                  ;ber then the line is to be placed in the USER PRO-
                                  ;GRAM BUFFER. It is now necessary to determine
                                  ;where the new line is to be placed in the USER PRO-
                                  ;GRAM BUFFER. This is dictated by the value of the
                                  ;new line number in relation to the line numbers cur-
                                  ;rently in the program buffer. The next portion of the
                                  ;program goes through the contents of the USER PRO-
                                  ;GRAM BUFFER comparing the values of the line num-
                                  ;bers already stored against the value of the line number
                                  ;currently being held in the LINE INPUT BUFFER.
                                  ;Appropriate action is then taken to Insert or Append,
                                  ;Change, or Delete a line in the program buffer.

GETAUX:    MVI	L, 201o               ;Set memory pointer to line character pointer storage
           MVI	H,PG26 ;\HB\OLDPG26   ;** Location and then initialize that storage location
           MVI	M, 001                ;To point to the 1'st character in a line
           MVI	L, 350o               ;Set memory pointer to addr of start of auxiliary line
           MVI	M, 000                ;Number storage area and initialize first byte to zero
GETAU0:    MVI	L, 201o               ;Set memory pointer to line character pointer storage loc
           CALL	GETCHP             ;Fetch a char in line pointed to by line pointer
           JZ	GETAU1             ;If character is a space, skip it by going to advance pntrs
           CPI	260o               ;If not a space check to see if character represents a
           JM	GETAU2             ;Valid decimal digit in the range 0 to 9 by testing the
           CPI	272o               ;ASCII code value obtained. If not a deciznal digit then
           JP	GETAU2             ;Assume have obtained the line number. Go process.
           MVI	L, 350o               ;If valid decimal digit want to append the digit to the
           MVI	H,PG26 ;\HB\OLDPG26   ;** Current string being built up in the auxiliary line
           CALL	CONCT1             ;Number storage area so call sub to concat a character.
GETAU1:    MVI	L, 201o               ;Reset memory pointer to line character pntr storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;On the appropriate page.
           MOV	B,M
           INR	B                    ;Fetch the pointer, increment it, and restore new value
           MOV	M,B
           MVI	L, 360o               ;Set memory pointer to pgm buff line pntr storage loc
           MVI	H,PG26 ;\HB\OLDPG26   
           MOV	C,M                    ;Bring the high order byte of this double byte pointer
           INR	L                    ;Into CPU register C. Then advance the memory pntr
           MOV	L,M                    ;And bring the low order byte into register L. Now trans-
           MOV	H,C                    ;Fer the higher order portion into memory pointer H.
           MOV	A,M                    ;Obtain the char cntr (cc) which indicates the length of
           DCR	B                    ;The line being pointed to by the user program line pntr
           CMP	B                    ;Compare this with the value of the chars processed so
           JNZ	GETAU0             ;Far in current line. If not equal, continue getting line n
GETAU2:    MVI	L, 360o               ;Reset mem pntr to pgm buffer line pntr storage
           MVI	H,PG26 ;\HB\OLDPG26   ;** On this page and place the high order byte
           MOV	D,M                    ;Of this pointer into CPU register D
           INR	L                    ;Advance the memory pointer, fetch the second
           MOV	L,M                    ;Byte of the pgm buffer line pointer into register L
           MOV	H,D                    ;Now make the memory pointer equal to this value
           MOV	A,M                    ;Fetch the first byte of a line in the program buffer
           ANA	A                    ;Test to see if end of contents of pgm buff (zero byte)
           JNZ	NOTEND             ;If not zero continue processing. If zero have reached
           JMP	NOSAME             ;End of buffer contents so go APPEND line to buffer.
;;; there are some open addresses here.  Above JUMP starts at 11-304;
;;; The below label patch3 should start at 11 307 09c7h
PATCH3:	   MVI	L, 201o               ; ptr to A/V storage
	   MVI	H,PG27 ;\HB\OLDPG27   ; MGA 3/31/12 make relocatable; prev: LHI 027
	   MVI	M, 000o               ; clear A/V flag
	   JMP	EXEC

;	db	(09deh-$) dup 0

           ORG	09deh              ;011#336
NOTEND:    MVI	L, 350o               ;Load L with addr of auxiliary line number storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;Load H with addr of aux line number storage loc
           MVI	D,PG26 ;\HB\OLDPG26   ;Load D with addr of line number buffer location
           MVI	E, 340o               ;Load E with address of line number buffer location
           CALL	STRCP              ;Compare line nr in input buffer with line number in
           JM	CONTIN             ;User program buffer. If lesser in value keep looking.
           JNZ	NOSAME             ;If greater in value then go to Insert line in pgm buffer
           MVI	L, 360o               ;If same values then must remove the line with the same
           MVI	H,PG26 ;\HB\OLDPG26   ;** Line number from the user program buffer. Set up
           MOV	C,M                    ;The CPU memory pointer to point to the current
           INR	L                    ;Position in the user program buffer by retrieving that
           MOV	L,M                    ;Pointer from its storage location. Then obtain the first
           MOV	H,C                    ;Byte of data pointed to which will be the character
           MOV	B,M                    ;Count for that line (cc). Add one to the cc value to take
           INR	B                    ;Account of the (cc) byte itself and then remove that
           CALL	REMOVE             ;Many bytes to effectively delete the line fm the user
           MVI	L, 203o               ;Program buffer. Now see if line in input buffer consists
           MVI	H,PG26 ;\HB\OLDPG26   ;** Only of a line number by checking SYNTAX
           MOV	A,M                    ;TOKEN value. Fetch the TOKEN value from its
           ANA	A                    ;Storage location. If it is zero then input buffer only
           JZ	EXEC               ;Contains a line number. Action is a pure Delete.
NOSAME:    MVI	L, 360o               ;Reset memory pointer to program buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;Line pointer storage location
           MOV	D,M                    ;Load high order byte into CPU register D
           INR	L                    ;Advance memory pointer
           MOV	E,M                    ;Load low order byte into CPU register E
           MVI	L, 000                ;Load L with address of start of line input buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Do same for CPU register H
           MOV	B,M                    ;Get length of line input buffer
           INR	B                    ;Advance length by one to include (cc) byte
           CALL	INSERT             ;Go make room to insert line into user program buffer
           MVI	L, 360o               ;Reset memory pointer to program buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Line pointer storage location
           MOV	D,M                    ;Load higher byte into CPU register D
           INR	L                    ;Advance memory pointer
           MOV	E,M                    ;Load low order byte into CPU register E
           MVI	L, 000                ;Load L with address of start of line input buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Do same for CPU register H
           CALL	MOVEC              ;Call subroutine to Insert line in input buffer into the
           JMP	EXEC1              ;User program buffer then go back to start of EXEC.
MOVEC:     MOV	B,M                    ;Fetch length of string in line input buffer
           INR	B                    ;Increment that value to provide for (cc)
MOVEPG:    MOV	A,M                    ;Fetch character from line input buffer
           CALL	ADV                ;Advance pointer for line input buffer
           CALL	SWITCH             ;Switch memory pointer to point to user pgm buffer
           MOV	M,A                    ;Deposit character fm input buff into user pgm buff
           CALL	ADV                ;Advance pointer for user program buffer
           CALL	SWITCH             ;Switch memory pntr back to point to input buffer
           DCR	B                    ;Decrement character counter stored in CPU register B
           JNZ	MOVEPG             ;If counter does not go to zero continue transfer ops
           RET                    ;When counter equals zero return to caMVI L,ng routine
CONTIN:    MVI	L, 360o               ;Reset memory pointer to program buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Line pointer storage location
           MOV	D,M                    ;Load high order byte into CPU register D
           INR	L                    ;Advance memory pointer
           MOV	E,M                    ;Load low order byte into CPU register E
           MOV	H,D                    ;Now set CPU register H to high part of address
           MOV	L,E                    ;And set CPU register L to low part of address
           MOV	B,M                    ;Fetch the character counter (cc) byte fm line in
           INR	B                    ;Program buffer and add one to compensate for (cc)
           CALL	ADBDE              ;Add length of line value to old value to get new pointer
           MVI	L, 360o               ;Reset memory pointer to program buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Line pointer storage location
           MOV	M,D                    ;Restore new high portion
           INR	L                    ;Advance memory pointer
           MOV	M,E                    ;And restore new low portion
           JMP	GETAUX             ;Continue til find point at which to enter new line
GETCHP:    MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with pointer page (low portion set upon
           MOV	B,M                    ;Entry). Now fetch pointer into CPU register B.
           MVI	L, 360o               ;Reset pntr to pgm buffer line pointer storage location
           MOV	D,M                    ;Load high order byte into CPU register D
           INR	L                    ;Advance memory pointer
           MOV	E,M                    ;Load low order byte into CPU register E
           CALL	ADBDE              ;Add pointer to pgm buffer pointer to obtain address of
           MOV	H,D                    ;Desired character. Place high part of new addr in H.
           MOV	L,E                    ;And low part of new address in E.
           MOV	A,M                    ;Fetch character from position in line in user pgm buffer
           CPI	240o               ;See if it is the ASCII code for space
           RET                    ;Return to caller with flags set to indicate result
REMOVE:    CALL	INDEXB             ;Add (cc) plus one to addr of start of line
           MOV	C,M                    ;Obtain byte from indexed location and
           CALL	SUBHL              ;Subtract character count to obtain old location
           MOV	M,C                    ;Put new byte in old location
           MOV	A,C                    ;As well as in the Accumulator
           ANA	A                    ;Test to see if zero byte to indicate end of user pgm buff
           JZ	REMOV1             ;If it is end of user pgm buffer, go complete process
           CALL	ADV                ;Otherwise add one to the present pointer value
           JMP	REMOVE             ;And continue removing chamcters from the user pgm bf
REMOV1:    MVI	L, 364o               ;Load L with end of user pgm buffer pointer storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of that pointer storage location
           MOV	D,M                    ;Get page portion of end of pgm buffer address
           INR	L                    ;Advance memory pointer
           MOV	A,M                    ;And get low portion of end of pgm buffer address into
           SUB	B                    ;Accumulator then subtract displacement value in B
           MOV	M,A                    ;Restore new low portion of end of pgm buffer address
           RNC                    ;If subtract did not cause carry can return now
           DCR	L                    ;Otherwise decrement memory pointer back to page
           DCR	D                    ;Storage location, decrement page value to give new page
           MOV	M,D                    ;And store new page value back in buffer pntr storage loc
           RET                    ;Then return to calling routine
INSERT:    MVI	L, 364o               ;Load L with end of user pgm buffer pointer storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of that pointer storage location
           MOV	A,M                    ; Get page portion of end of program buffer address
           INR	L                    ;Advance memory pointer
           MOV	L,M                    ;Load low portion of end of program buffer address
           MOV	H,A                    ;Into L and finish setting up memory pointer
           CALL	INDEXB             ;Add (cc) of line in input buffer to form new end of
           MOV	A,H                    ;Program buffer address. Fetch new end of buffer page
           CPI	ENDPGRAM           ;tt Address and see if this value would exceed user's
           JP	BIGERR             ;System capabilit'y. Go display error message if so!
           CALL	SUBHL              ;Else restore original value of end of buffer address
INSER1:    MOV	C,M                    ;Bring byte pointed to by H & L into CPU register C
           CALL	INDEXB             ;Add displacement value to current memory pointer
           MOV	M,C                    ;Store the byte in the new location
           CALL	SUBHL              ;Now subtract displacement value from H & L
           CALL	CPHLDE             ;Compare this with the address stored in D & E
           JZ	INSER3             ;If same then go finish up Insert operation
           CALL	DEC                ;Else set pointer to the byte before the byte just
           JMP	INSER1             ;Processed and continue the Insert operation
INSER3:
INCLIN:    MVI	L, 000                ;Load L with start of line input buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of start of line input buffer
           MOV	B,M                    ;Fetch length of the line in line input buffer
           INR	B                    ;Increment value by one to include (cc) byte
           MVI	L, 364o               ;Set memory pointer to end of user pgrn buffer pointer
           MOV	D,M                    ;Storage location on same page and fetch page address
           INR	L                    ;Of this pointer into D. Then advance memory pointer
           MOV	E,M                    ;And get low part of this pointer into CPU register E.
           CALL	ADBDE              ;Now add displacement (cc) of line in input buffer to
           MOV	M,E                    ;The end of program buffer pointer. Replace the updated
           DCR	L                    ;Low portion of the new pointer value back in stomge
           MOV	M,D                    ;And restore the new page value back into storage
           RET                    ;Then return to calling routine
CPHLDE:    MOV	A,H                    ;Subroutine to compare if the contents of CPU registers
           CMP	D                    ;H & L are equal to registers D & E. First compare
           RNZ                    ;Register H to D. Return with flags set if not equal. If
           MOV	A,L                    ;Equal continue by comparing register L to E.
           CMP	E                    ;IF L equals E then H & L equal to D & E so return to
           RET                    ;Calling routines with flags set to equality status
ADBDE:     MOV	A,E                    ;Subroutine to add the contents of CPU register B (single
           ADD	B                    ;Byte value) to the double byte value in registers D & E.
           MOV	E,A                    ;First add B to E to form new least significant byte
           RNC                    ;Restore new value to E and exit if no carry resulted
           INR	D                    ;If had a carry then must increment most significant byte
           RET                    ;In register D before returning to calling routine
CTRLC:     MVI	A, 336o               ;Set up ASCII code for t (up arrow) in Accumulator.
           MVI	C, 303o               ;Set up ASCII code for letter 'C' in CPU register C.
           JMP	ERROR              ;Go display the 'Control C' condition message.
FINERR:    MVI	L, 340o               ;Load L with starting address of line number storage area
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of line number storage area
           MOV	A,M                    ;Get (cc) for line number string. If length is zero meaning
           ANA	A                    ;There is no line number stored in the buffer then jump
           JZ	FINER1             ;Ahead to avoid displaying "AT LINE" message
           MVI	L, 366o               ;Else load L with address of start of "AT LINE" message
           MVI	H,PG01 ;\HB\OLDPG1    ;** Stored on this page
           CALL	TEXTC              ;Call subroutine to display the "AT LINE" message
           MVI	L, 340o               ;Now reset L to starting address of line number storage
           MVI	H,PG26 ;\HB\OLDPG26   ;** Area and do same for CPU register H
           CALL	TEXTC              ;Call subroutine to display the line number
FINER1:    CALL	CRLF               ;Call subroutine to provide a carriage-return and line-feed
	   JMP	PATCH3 
;;; The following is the old code, before patch 3
;;;        JMP	EXEC               ;To the display device then return to EXECUTIVE.
DVERR:     MVI	A, 304o               ;Set up ASCII code for letter 'D' in Accumulator
           MVI	C, 332o               ;Set up ASCII code for letter 'Z' in CPU register C
           JMP	ERROR              ;Go display the 'DZ' (divide by zero) error message
FIXERR:    MVI	A, 306o               ;Set up ASCII code for letter 'F' in Accumulator
           MVI	C, 330o               ;Set up ASCII code for letter 'X' in CPU register C
           JMP	ERROR              ;Go display the 'FX' (FiX) error message
NUMERR:    MVI	A, 311o               ;Set up ASCII code for letter 'I' in Accumulator
           MVI	C, 316o               ;Set up ASCII code for letter 'N' in CPU register C
           MVI	L, 220o               ;Load L with address of pointer used by DINPUT
           MVI	H,PG01 ;\HB\OLDPG1    ;** Routine. Do same for register H.
           MVI	M, 000                ;Clear the location
           JMP	ERROR              ;Go display the'IN'(Illegal Number) error message

                                  ;The following subroutine, used by various sections of
                                  ;SCELBAL, will search the LINE INPUT BUGGER for
                                  ;a character string which is contained in a buffer starting
                                  ;at the address pointed to by CPU registers H & L when
                                  ;the subroutine is entered.

INSTR:     MVI	D,PG26 ;\HB\OLDPG26   ;**Set D to starting page of LINE INPUT BUFFER
           MVI	E, 000                ;Load E with starting location of LINE INPUT BUFFER
INSTR1:    CALL	ADVDE              ;Advancer D & E pointer to the next location (input
           CALL	SAVEHL             ;Buffer). Now save contents of d, E, H & L vefore the
           MOV	B,M                    ;Compare operations. Get length of TEST buffer in B.
           CALL	ADV                ;Advance H & L buffer to first char in TEST buffer.
           CALL	STRCPC             ;Compare contents of TEST buffer against input buffer
           JZ	RESTHL             ;For length B. If match, restore pntrs and exit to caller.
           CALL	RESTHL             ;If no match, restore pointers for loop test.
           MVI	L, 000                ;Load L with start of input buffer (to get the char cntr).
           MVI	H,PG26 ;\HB\OLDPG26   ;**Load H with page of input buffer.
           MOV	A,M                    ;Get length of buffer (cc) into the accumulator.
           CMP	E                    ;Compare with current input buffer pointer value.
           JZ	INSTR2             ;If at end of buffer, jump ahead.
           CALL	RESTHL             ;Else restore test string address (H&L) and input buffer
           JMP	INSTR1             ;Address (D&E). Look gor occurrence of test string in ln.
           HLT             ;Safety halt. If program reaches here have system failure.
INSTR2:    MVI	E, 000                ;If reach end of input buffer without finding a match
           RET                    ;Load E with 000 as an indicator and return to caller.
ADVDE:     INR	E                    ;Subroutine to advance the pointer in the register
           RNZ                    ;Pair D & E. Advance contents of E. Return if not zero.
           INR	D                    ;If register E goes to 0 when advanced, then advance
           RET                    ;Register D too. Exit to calling routine.

;;; The label RUN should start at 13-170 0b78h
RUN:       MVI	L, 073o               ;Load L with addr of GOSUB/RETURN stack pointer
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of same pointer
           MVI	M, 000                ;Initialize the GOSUB/RETURN stack pointer to zero
           MVI	L, 205o               ;Load L with addr of FOR/NEXT stack pointer
           MVI	M, 000                ;Initialize the FOR/NEXT stack pointer to zero
           MVI	L, 360o               ;Load L with addr of user pgm buffer line pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of user pgm buffer line pointer
           MVI	M, BGNPGRAM           ;tt Initialize pointer (may be altered by user)   *******
           INR	L                    ;Advance memory pointer to low portion of user pgm
           MVI	M, 000                ;Buffer pointer and initialize to start of buffer
           JMP	SAMLIN             ;Start executing user program with first line in buffer
NXTLIN:    MVI	L, 360o               ;Load L with addr of user program buffer line pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of user pgm buffer line pointer
           MOV	D,M                    ;Place page addr of pgm buffer line pointer in D
           INR	L                    ;Advance the memory pointer
           MOV	E,M                    ;Place low addr of pgm buffer line pointer in E
           MOV	H,D                    ;Also put page addr of pgm buffer line pointer in H
           MOV	L,E                    ;And low addr of pgm buffer line pointer in L
           MOV	B,M                    ;Now fetch the (cc) of current line into register B
           INR	B                    ;Add one to account for (cc) byte itself
           CALL	ADBDE              ;Add value in B to D&E to point to next line in
           MVI	L, 360o               ;User program buffer. Reset L to addr of user logrn
           MVI	H,PG26 ;\HB\OLDPG26   ;** Buffer pointer storage location. Store the new
           MOV	M,D                    ;Updated user pgm line pointer in pointer storage
           INR	L                    ;Location. Store both the high portion
           MOV	M,E                    ;And low portion. (Now points to next line to be
           MVI	L, 340o               ;Processed from user program buffer.) Change pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** To address of line number buffer. Fetch the last
           MOV	A,M                    ;Line number (length) processed. Test to see if it was
           ANA	A                    ;Blank. If it was blank
           JZ	EXEC               ;Then stop processing and return to the Executive
           MOV	A,A                    ;Insert two effective NOPs here
           MOV	A,A                    ;In case of patching
SAMLIN:    MVI	L, 360o               ;Load L with addr of user program buffer line pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of same pointer
           MOV	C,M                    ;Fetch the high portion of the pointer into register C
           INR	L                    ;Advance the memory pointer
           MOV	L,M                    ;Fetch the low portion of the pointer into register L
           MOV	H,C                    ;Now move the high portion into register H
           MVI	D,PG26 ;\HB\OLDPG26   ;** Set D to page of line input buffer
           MVI	E, 000                ;Set E to address of start of line input buffer
           CALL	MOVEC              ;Move the line ftom the user program buffer into the
           MVI	L, 000                ;Line input buffer. Now reset the pointer to the start
           MVI	H,PG26 ;\HB\OLDPG26   ;** Of the line input buffer.
           MOV	A,M                    ;Fetch the first byte of the line input buffer (cc)
           ANA	A                    ;Test (cc) value to see if fetched a blank line
           JZ	EXEC               ;If fetched a blank line, return to the Executive
           CALL	SYNTAX             ;Else call subrtn to strip off line nr & set statement toke

DIRECT:    MVI	L, 203o               ;Load L with address of syntax TOKEN storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of syntax TOKEN location
           MOV	A,M                    ;Fetch the TOKEN value into the accumulator
           CPI	001                ;Is it token value for REM statement? If so, ignore the
           JZ	NXTLIN             ;Current line and go on to the next line in pgm buffer.
           CPI	002                ;Is it token value for IF statement?
           JZ	IF                 ;If yes, then go to the IF statement routine.
           CPI	003                ;Is it token value for LET statement? (Using keyword)
           JZ	LET                ;If yes, then go to the LET statement routine.
           CPI	004                ;Is it token value for GOTO statement?
           JZ	GOTO               ;If yes, then go to the GOTO statement routine.
           CPI	005                ;Is it token value for PRINT statement?
           JZ	PRINT              ;If yes, then go to the PRINT statement routine.
           CPI	006                ;Is it token value for INPUT statement?
           JZ	INPUT              ;If yes, then go to the INPUT statement routine.
           CPI	007                ;Is it token value for FOR statement?
           JZ	FOR                ;If yes, then go to the FOR statement routine.
           CPI	010o               ;Is it token value for NEXT statement?
           JZ	NEXT               ;If yes, then go to the NEXT statement routine.
           CPI	011o               ;Is it token value for GOSUB statement?
           JZ	GOSUB              ;If yes, then go to the GOSUB statement routine.
           CPI	012o               ;Is it token value for RETURN statement?
           JZ	RETURN             ;If yes, then go to the RETURN statement routine.
           CPI	013o               ;Is it token value for DIM statement?
           JZ	DIM                ;If yes, then go to the DIM statement routine.
           CPI	014o               ;Is it token value for END statement?
           JZ	EXEC               ;If yes, then go back to the Executive, user pgm finished!
           CPI	015o               ;Is it token value for IMPLIED LET statement?
           JZ	LET0               ;If yes, then go to special LET entry point.
           CPI	016o               ;@@ Is it token value for ARRAY IMPLIED LET?
           JNZ	SYNERR             ;If not, then assume a syntax error condition.
           CALL	ARRAY1             ;@@ Else, perform array storage set up subroutine.
           MVI	L, 206o               ;@@ Set L to array pointer storage location.
           MVI	H,PG26 ;\HB\OLDPG26   ;@@ * * Set H to array pointer storage location.
           MOV	B,M                    ;@@ Fetch array pointer to register B.
           MVI	L, 202o               ;@@ Change memory pointer to syntax pntr storage loc.
           MOV	M,B                    ;@@ Save array pointer value there.
           CALL	SAVESY             ;@@ Save array name in auxiliary symbol buffer
           JMP	LET1
PRINT:     MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SCAN pointer
           MOV	A,M                    ;Fetch the pointer value (last character scanned by the
           MVI	L, 000                ;SYNTAX routine). Change pointer to line buffer (cc).
           CMP	M                    ;Compare pointer value to buffer length. If not equal
           JM	PRINT1             ;Then line contains more than stand alone PRINT state-
           CALL	CRLF               ;Ment. However, if just have PRINT statement then issue
           JMP	NXTLIN             ;A carriage-return & line-feed combination, then exit.
PRINT1:    CALL	CLESYM             ;Initialize the SYMBOL buffer for new entry.
           MVI	L, 202o               ;Load L with address of SCAN buffer pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SCAN pointer
           MOV	B,M                    ;Pointer points to last char scanned by SYNTAX. Need
           INR	B                    ;To increment it to point to next char in statement line.
           MVI	L, 203o               ;Load L with address of former TOKEN value. Use it as
           MOV	M,B                    ;Storage location for a PRINT statement pointer.
PRINT2:    MVI	L, 203o               ;Set memory pointer to PRINT pointer storage location
           CALL	GETCHR             ;Fetch character in input buffer pointed to by PRINT
           CPI	247o               ;Pointer. See if it is ASCII code for single quote mark.
           JZ	QUOTE              ;If so, go to QUOTE section to process text string.
           CPI	242o               ;If not, see if it is ASCII code for double quote mark.
           JZ	QUOTE              ;If so, go to QUOTE section to process text string.
           CPI	254o               ;If not, see if it is ASCII code for comma sign.
           JZ	PRINT3             ;If so, go evaluate expression.
           CPI	273o               ;If not, see if it is ASCII code for semi-colon sign.
           JZ	PRINT3             ;If so, go evaluate expression.
           MVI	L, 203o               ;Load L with address of PRINT pointer storage location.
           CALL	LOOP               ;Increment pointer and test for end of line.
           JNZ	PRINT2             ;If not end of line, fetch the next character.
PRINT3:    MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MOV	B,M                    ;Fetch value of the pointer (last letter of KEYWORD)
           INR	B                    ;Add one to point to first character of expression
           MVI	L, 276o               ;Load L with addr of EVAL pointer storage location
           MOV	M,B                    ;Store addr at which EVAL should start scanning
           MVI	L, 203o               ;Load L with address of PRINT pointer
           MOV	B,M                    ;Which points to field terminator
           DCR	B                    ;Decrement pointer value to last character of expression
           MVI	L, 277o               ;Load L with address of EVAL FINISH pntr storage loc.
           MOV	M,B                    ;Place address value of last char in PRINT field there
           MVI	L, 367o               ;Load L with address of QUOTE flag
           MOV	A,M                    ;Fetch the value of the QUOTE flag into the ACC
           ANA	A                    ;Test the QUOTE flag status
           JZ	PRINT4             ;If field not quoted, proceed to evaluate expression
           MVI	M, 000                ;If field quoted, then clear the QUOTE flag for next field
           JMP	PRINT6             ;And skip the evaluation procedure
PRINT4:    CALL	EVAL               ;Evaluate the current PRINT field
           MVI	L, 177o               ;Then load L,with address of the TAB flag
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with the page of the TAB flag
           MOV	A,M                    ;Fetch the value of the TAB flag into the accumulator
           ANA	A                    ;Test the TAB flag
           MVI	L, 110o               ;Change L to the FIXED/FLOAT flag location
           MVI	H,PG01 ;\HB\OLDPG1    ;** Change H to the FIXED/FLOAT flag page
           MVI	M, 377o               ;Set FIXED/FLOAT flag to fixed point
PRINT5:    CZ	PFPOUT             ;If TAB flag not set, display value of expression
           MVI	L, 177o               ;Load L with address of TAB flag
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of TAB flag
           MVI	M, 000                ;Reset TAB flag for next PRINT field
PRINT6:    MVI	L, 203o               ;Load L with address of PRINT pointer stomge location
           CALL	GETCHR             ;Fetch the character pointed to by the PRINT pointer
           CPI	254o               ;See if the last character scanned was a comma sign
           CZ	PCOMMA             ;If so, then display spaces to next TA.B location
           MVI	L, 203o               ;Reset L to address of PRINT pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Reset H to page of PRINT pointer stomge location
           MOV	B,M                    ;Fetch the value of the pointer into register B
           MVI	L, 202o               ;Change L to SCAN pointer storage location
           MOV	M,B                    ;Place end of last field processed into SCAN pointer
           MVI	L, 000                ;Change pointer to start of line input buffer
           MOV	A,B                    ;Place pntr to last char scanned into the accumulator
           CMP	M                    ;Compare this value to the (cc) for the line buffer
           JM	PRINT1             ;If not end of line, continue to process next field
           MVI	L, 000                ;If end of line, fetch the last character in the line
           CALL	GETCHR             ;And check to see if it
           CPI	254o               ;Was a comma. If it was, go on to the next line in the
           JZ	NXTLIN             ;User program buffer without displaying a CR & LF.
           CPI	273o               ;If not a comma, check to see if it was a semi-colon.
           JZ	NXTLIN             ;If so, do not provide a CR & LF combination.
           CALL	CRLF               ;If not comma or semi-colon, provide CR & LF at end
           JMP	NXTLIN             ;Of a PRINT statement. Go process next line of pgrm.
QUOTE:     MVI	L, 367o               ;Load L with address of QUOTE flag
           MOV	M,A                    ;Store type of quote in flag storage location
           CALL	CLESYM             ;Initialize the SYMBOL buffer for new entry
           MVI	L, 203o               ;Load L with address of PRINT pointer
           MOV	B,M                    ;Fetch the PRINT pointer into register B
           INR	B                    ;Add one to advance over quote character
           MVI	L, 204o               ;Load L with address of QUOTE pointer
           MOV	M,B                    ;Store the beginning of the QUOTE field pointer
QUOTE1:    MVI	L, 204o               ;Load L with address of QUOTE pointer
           CALL	GETCHR             ;Fetch the next character in the TEXT field
           MVI	L, 367o               ;Load L with the QUOTE flag (type of quote)
           CMP	M                    ;Compare to see if latest character this quote mark
           JZ	QUOTE2             ;If so, finish up this quote field
           CALL	ECHO               ;If not, display the character as part of TEXT
           MVI	L, 204o               ;Reset L to QUOTE pointer storage location
           CALL	LOOP               ;Increment QUOTE pointer and test for end of line
           JNZ	QUOTE1             ;If not end of line, continue processing TEXT field
QUOTER:    MVI	A, 311o               ;If end of line before closing quote mark have an error
           MVI	C, 321o               ;So load ACC with I and register C with Q
           MVI	L, 367o               ;Load L with the address of the QUOTE flag
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with the page of the QUOTE flag
           MVI	M, 000                ;Clear the QUOTE flag for future use
           JMP	ERROR              ;Go display the IQ (Illegal Quote) error message
QUOTE2:    MVI	L, 204o               ;Load L with address of QUOTE pointer
           MOV	B,M                    ;Fetch the QUOTE pointer into register B
           MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MOV	M,B                    ;Store former QUOTE vointer as start of next field
           MOV	A,B                    ;Place QUOTE pointer into the accumulator
           MVI	L, 000                ;Change L to point to start of the input line buffer
           CMP	M                    ;Compare QUOTE pointer value with (cc) value
           JNZ	PRINT1             ;If not end of line, process next PRINT field
           CALL	CRLF               ;Else display a CR & LF combination at the end of line
           MVI	L, 367o               ;Load L with the address of the TAB flag
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with the page of the TAB flag
           MVI	M, 000                ;Clear the TAB flag for future use
           JMP	NXTLIN             ;Go process next line of the program.

                                  ;The following subroutines are utilized by the PRINT
                                  ;routine.
;;; The label PFPOUT SHOULD BE AT 14 314 0ccch
PFPOUT:    MVI	L, 126o               ;Load L with the address of the FPACC MSW (Floating
           MVI	H,PG01 ;\HB\OLDPG1    ;** Point ACC). Load H with page of the FPACC MSW.
           MOV	A,M                    ;Fetch the FPACC MSW into the accumulator. Test to
           ANA	A                    ;See if the FPACC MSW is zero. If so, then simply go and
           JZ	ZERO               ;Display the value "0"
           INR	L                    ;Else advance the pointer to the FPACC Exponent
           MOV	A,M                    ;Fetch the FPACC Exponent into the accumulator
           ANA	A                    ;See if any exponent value. If not, mantissa is in range
           JZ	FRAC               ;0.5 to 1.0. Treat number as a fraction.
           JMP	FPOUT              ;Else perform regular numerical output routine.
ZERO:      MVI	A, 240o               ;Load ASCII code for space into the ACC
           CALL	ECHO               ;Display the space
           MVI	A, 260o               ;Load ASCII code for 0 into the ACC
           JMP	ECHO               ;Display 0 and exit to calling routine
FRAC:      MVI	L, 110o               ;Load L with address of FIXED/FLOAT flag
           MVI	M, 000                ;Reset it to indicate floating point mode
           JMP	FPOUT              ;Display floating point number and return to caller
PCOMMA:    MVI	L, 000                ;Load L with address of (cc) in line input buffer
           MOV	A,M                    ;Fetch the (cc) for the line into the ACC
           MVI	L, 203o               ;Change pointer to PRINT pointer storage location
           SUB	M                    ;Subtract value of PRINT pointer from line (cc)
           RM                    ;If at end of buffer, do not TAB
           MVI	L, 043o               ;If not end, load L with address of COLUMN COUNTER
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of COLUMN COUNTER
           MOV	A,M                    ;Fetch COLUMN COUNTER into the accumulator
           ANI	360o               ;Find the last TAB position (multiple of 16 decimal)
           ADI	020o               ;Add 16 (decimal) to get new TAB position
           SUB	M                    ;Subtract current position from next TAB position
           MOV	C,A                    ;Store this value in register C as a counter
           MVI	A, 240o               ;Load the ACC with the ASCII code for space
PCOM1:     CALL	ECHO               ;Display the space
           DCR	C                    ;Decrement the loop counter
           JNZ	PCOM1              ;Continue displaying spaces until loop counter is zero
           RET                    ;Then return to calling routine
LET0:      CALL	SAVESY             ;Entry point for IMPLIED LET statement. Save the
           MVI	L, 202o               ;Variable (to left of the equal sign). Set L to the SCAN
           MVI	H,PG26 ;\HB\OLDPG26   ;** Pointer. Set H to the page of the SCAN pointer.
           MOV	B,M                    ;Fetch value of SCAN pointer. (Points to = sign in In bf)
           MVI	L, 203o               ;Change pointer to LET pointer (was TOKEN value)
           MOV	M,B                    ;Place the SCAN pointer value into the LET pointer
           JMP	LET5               ;Continue processing the LET statement line
LET:       CALL	CLESYM             ;Initialize the SYMBOL BUFFER for new entry
           MVI	L, 144o               ;Load L with address of start of AUX SYMBOL BUFF
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of AUX SYMBOL BUFFER
           MVI	M, 000                ;Initialize AUX SYMBOL BUFFER
LET1:      MVI	L, 202o               ;Entry point for ARRAY IMPLIED LET statement.
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set pointer to SCAN pointer storage location
           MOV	B,M                    ;Fetch the SCAN pointer value (last letter scanned by
           INR	B                    ;SYNTAX subroutine) and add one to next character
           MVI	L, 203o               ;Change L to LET pointer storage location
           MOV	M,B                    ;Store former SCAN value (updated) in LET pointer
LET2:      MVI	L, 203o               ;Set L to gtorage location of LET pointer
           CALL	GETCHR             ;Fetch the character pointed to by the LET pointer
           JZ	LET4               ;If character is a space, ignore it
           CPI	275o               ;See if character is the equal (=) sign
           JZ	LET5               ;If so, go process other side of the statement (after
           CPI	250o               ;@@ If not, see if character is a right parenthesis
           JNZ	LET3               ;If not, continue looking for equal sign
           CALL	ARRAY              ;@@ If so, have subscript. Call array set up subroutine.
           MVI	L, 206o               ;@@ Load L with address of ARRAY pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;@@ ** Load H with page of ARRAY pointer
           MOV	B,M                    ;@@ Fetch value (points to ")" character of subscript)
           MVI	L, 203o               ;@@ Load L with address of LET pointer
           MOV	M,B                    ;@@ Place ARRAY pointer value as new LET pointer
           JMP	LET4               ;@@ Continue to look for = sign in statement line
LET3:      MVI	L, 144o               ;Reset L to start of AUX SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ** Load H with page of AUX SYMBOL BUFFER
           CALL	CONCT1             ;Concatenate character to the AUX SYMBOL BUFFER
LET4:      MVI	L, 203o               ;Load L with address of LET pointer storage location
           CALL	LOOP               ;Add one to pointer and test for end of line input buffer
           JNZ	LET2               ;If not end of line, continue looking for the equal sign
LETERR:    MVI	A, 314o               ;If do not find an equal sign in the LET statement line
           MVI	C, 305o               ;Then have a LE (Let Error). Load the code for L and E
           JMP	ERROR              ;Into registers ACC and C and go display the error msg.
LET5:      MVI	L, 203o               ;When find the equal sign, reset L to point to the LET
           MVI	H,PG26 ;\HB\OLDPG26   ;** Pointer and H to the proper page. Fetch the pointer
           MOV	B,M                    ;Value into register B and add one to advance pointer
           INR	B                    ;Over the equal sign to first char in the expression.
           MVI	L, 276o               ;Set L to point to the address of the EVAL pointer
           MOV	M,B                    ;Set EVAL pointer to start evaluating right after the
           MVI	L, 000                ;Equal sign. Now change L to start of line input buffer.
           MOV	B,M                    ;Fetch the (cc) value into register B. (Length of line.)
           MVI	L, 277o               ;Load L with EVAL FINISH pointer storage location.
           MOV	M,B                    ;Set it to stop evaluating at end of the line.
           CALL	EVAL               ;Call the subroutine to evaluate the expression.
           CALL	RESTSY             ;Restore the name of the variable to receive new value.
           CALL	STOSYM             ;Store the new value for the variable in variables table.
           JMP	NXTLIN             ;Go process next line of the program.
GOTO:      MVI	L, 350o               ;Load L with start of AUX LINE NR BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of AUX LINE NR BUFFER
           MVI	M, 000                ;Initialize the AUX LINE NR BUFFER to zero
           MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MOV	B,M                    ;Fetch pointer value (last char scanned by SYNTAX)
           INR	B                    ;Add one to skip over the last 0 in GOTO keyword
           MVI	L, 203o               ;Change pointer to GOTO pointer (formerly TOKEN)
           MOV	M,B                    ;Store the updated SCAN pointer as the GOTO pointer
GOTO1:     MVI	L, 203o               ;Load L with address of GOTO pointer
           CALL	GETCHR             ;Fetch the character pointed to by the GOTO pointer
           JZ	GOTO2              ;If character was a space, ignore it
           CPI	260o               ;See if character is in the range of a decimal digit
           JM	GOTO3              ;If not, must have end of the line number digit string
           CPI	272o               ;Continue to test for decitnal digit
           JP	GOTO3              ;If not, mugt have end of the line number digit string
           MVI	L, 350o               ;If valid decimal digit, load L with addr of AUX LINE
           CALL	CONCT1             ;NR BUFFER and concatenate digit to the buffer.
GOTO2:     MVI	L, 203o               ;Reset pointer to GOTO pointer storage location
           CALL	LOOP               ;Advance the pointer value and test for end of line
           JNZ	GOTO1              ;If not end of line, fetch next digit in GOTO line number
GOTO3:	   MVI	L, 360o               ;Set L to user program buffer pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of program buffer pointer
           MVI	M, BGNPGRAM           ;Initialize high part of pointer to start of pgm buffer
           INR	L                    ;Advance the memory point
           MVI	M, 000                ;Initialize the low part of pointer to start of pgm buffer
GOTO4:     CALL	CLESYM             ;Clear the SYMBOL BUFFER
           MVI	L, 204o               ;Load L with address of GOTO SEARCH pointer
           MVI	M, 001                ;Initialize to one for first char of line
GOTO5:     MVI	L, 204o               ;Load L with address of GOTO SEARCH pointer
           CALL	GETCHP             ;Fetch character pointed to by GOTO SEARCH pointer
           JZ	GOTO6              ;From line pointed to in user program buffer. Ignore
           CPI	260o               ;Spaces. Check to see if character is a decirnal digit.
           JM	GOTO7              ;If not, then have processed line number at the start of
           CPI	272o               ;The current line. Continue the check for a valid decimal
           JP	GOTO7              ;Digit. If have a decirnal digit then concatenate the digit
           CALL	CONCTS             ;Onto the current string in the SYMBOL BUFFER,
GOTO6:     MVI	L, 204o               ;Change L to the address of the GOTO SEARCH pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** And H to the proper page of the pointer
           MOV	B,M                    ;Fetch the GOTO SEARCH pointer value
           INR	B                    ;Increment the GOTO SEARCH pointer
           MOV	M,B                    ;And restore it back to memory
           MVI	L, 360o               ;Change L to address of user program buffer pointer
           MOV	C,M                    ;Save the high part of this pointer value in register C
           INR	L                    ;Advance L to the low part of the pgrn buffer pointer
           MOV	L,M                    ;Now load it into L
           MOV	H,C                    ;And transfer C into H to point to start of the line
           MOV	A,M                    ;Fetch the (cc) of the current line being pointed to in the
           DCR	B                    ;User pgm buff. Decrernent B to previous value. Compare
           CMP	B                    ;GOTO SEARCH pointer value to length of current line.
           JNZ	GOTO5              ;If not end of line then continue getting current line nr.
GOTO7:     MVI	L, 120o               ;Load L with address of start of the SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;Set H to the page of the SYMBOL BUFFER
           MVI	D,PG26 ;\HB\OLDPG26   ;Set D to the page of the AUX LINE NR BUFFER
           MVI	E, 350o               ;Set E to the start of the AUX LINE NR BUFFER
           CALL	STRCP              ;Compare GOTO line number against current line nr.
           JZ	SAMLIN             ;If they match, found GOTO line. Pick up ops there!
           MVI	L, 360o               ;Else, set L to user program buffer pntr storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of user program buffer pointer
           MOV	D,M                    ;Fetch the high part of this pointer into register D
           INR	L                    ;Advance the memory pointer
           MOV	E,M                    ;Fetch the low part into register E
           MOV	H,D                    ;Transfer the pointer to H
           MOV	L,E                    ;And L. Fetch the (cc) of the current line into register
           MOV	B,M                    ;B and then add one to account for the (cc) byte to get
           INR	B                    ;Total length of the current line in the user pgm buffer
           CALL	ADBDE              ;Add the total length to the pointer value in D & E
           MVI	L, 360o               ;To get the starting address of the next line in the user
           MVI	H,PG26 ;\HB\OLDPG26   ;** User program buffer. Place the new value for the user
           MOV	M,D                    ;Program buffer pointer back into the user program
           INR	L                    ;Buffer pointer storage locations so that it points to the
           MOV	M,E                    ;Next line to be processed in the user program buffer.
           MVI	L, 364o               ;Load L with address of end of user pgm buffer storage
           MOV	A,D                    ;Location (page address) and fetch end of buffer page.
           CMP	M                    ;Compare this with next line pointer (updated).
           JNZ	GOTO4              ;If not end of buffer, keep looking for the specified line
           INR	L                    ;If have same page addresses, check the low address
           MOV	A,E                    ;Portions to see if
           CMP	M                    ;Have reached end of user program buffer
           JNZ	GOTO4              ;If not, continue looking. If end of buffer without
GOTOER:    MVI	A, 325o               ;Finding specified line, then have an error condition.
           MVI	C, 316o               ;Load ACC and register C with code for "UN" and go
           JMP	ERROR              ;Display "Undefined Line" error message.
IF:        MVI	L, 202o               ;Set L to SCAN pointer storage location.
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H to page of SCAN pointer storage location.
           MOV	B,M                    ;Fetch the SCAN pointer value to register B.
           INR	B                    ;Add one to advance pointer over last char scanned.
           MVI	L, 276o               ;Change L to address of EVAL pointer. Set up EVAL
           MOV	M,B                    ;Pointer to begin evaluation with next char in the line.
           CALL	CLESYM             ;Clear the SYMBOL BUFFER.
           MVI	L, 320o               ;Set L to starting address of THEN in look-up table.
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of the look-up table.
           CALL	INSTR              ;Search for occurrence of THEN in the line input buffer.
           MOV	A,E                    ;Transfer register E to ACC. If THEN not found
           ANA	A                    ;The value in E will be zero.
           JNZ	IF1                ;If THEN found, can evaluate the IF expression.
           MVI	L, 013o               ;If THEN not found, set L to Auting address of GOTO
           MVI	H,PG27 ;\HB\OLDPG27   ;** In the KEYWORD look-up table. Set H to table
           CALL	INSTR              ;Search for occurrence of GOTO in the line input buffer.
           MOV	A,E                    ;Transfer E to ACC. If GOTO not found
           ANA	A                    ;The value in E will be zero.
           JNZ	IF1                ;If GOTO found, can evaluate the IF expression.
IFERR:     MVI	A, 311o               ;Set ASCII code for letter I in ACC
           MVI	C, 306o               ;And code for letter F in register C
           JMP	ERROR              ;Go display the IF error message
IF1:       MVI	L, 277o               ;Load L with addr of EVAL FINISH pointer storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of storage location
           DCR	E                    ;Subtract one from pointer in E and set the EVAL
           MOV	M,E                    ;FINISH pointer so that it will evaluate up to the THEN
           CALL	EVAL               ;Or GOTO directive. Evaluate the expression.
           MVI	L, 126o               ;Load L with address of FPACC Most Significant Word
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of FPACC MSW
           MOV	A,M                    ;Fetch the FPACC MSW into the accumulator
           ANA	A                    ;Test the value of the FPACC MSW
           JZ	NXTLIN             ;If it is zero, IF condition failed, ignore rest of line.
           MVI	L, 277o               ;If not, load L with addr of EVAL FINISH pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to the appmpriate page
           MOV	A,M                    ;Fetch the value in the EVAL FINISH pointer
           ADI	005                ;Add five to skip over THEN or GOTO directive
           MVI	L, 202o               ;Change L to SCAN pointer stomge location
           MOV	M,A                    ;Set up the SCAN pointer to location after THEN or
           MOV	B,A                    ;GOTO directive. Also put this value in register B.
           INR	B                    ;Add one to the value in B to point to next character
           MVI	L, 204o               ;After THEN or GOTO. Change L to addr of THEN pntr
           MOV	M,B                    ;Storage location and store the pointer value.
IF2:       MVI	L, 204o               ;Load L with the address of the THEN pointer
           CALL	GETCHR             ;Fetch the character pointed to by the THEN pointer
           JNZ	IF3                ;If character is not a space, exit this loop
           MVI	L, 204o               ;If fetch a space, ignore. Reset L to the THEN pointer
           CALL	LOOP               ;Add one to the THEN pointer and test for end of line
           JNZ	IF2                ;If not end of line, keep looking for a character other
           JMP	IFERR              ;Than a space. If reach end of line first, then error
IF3:       CPI	260o               ;When find a character see if it is numeric.
           JM	IF4                ;If not numeric, then should have a new type of
           CPI	272o               ;Statement. If numeric, then should have a line number.
           JM	GOTO               ;So process as though have a GOTO statement!
IF4:       MVI	L, 000                ;Load L with addr of start of line input buffer.
           MOV	A,M                    ;Fetch the (cc) byte to get length of line value.
           MVI	L, 204o               ;Change L to current value of THEN pointer (where first
           SUB	M                    ;Non-space char. found after THEN or GOTO). Subtract
           MOV	B,A                    ;This value from length of line to get remainder. Now
           INR	B                    ;Have length of second statement portion. Add one for
           MOV	C,M                    ;(cc) count. Save THEN pointer value in register C.
           MVI	L, 000                ;Reset L to start of line input buffer. Now put length of
           MOV	M,B                    ;Second statement into (cc) position of input buffer.
           MOV	L,C                    ;Set L to where second statement starts.
           MVI	D,PG26 ;\HB\OLDPG26   ;** Set D to page of line input buffer.
           MVI	E, 001                ;Set E to first character position of line input buffer.
           CALL	MOVEIT             ;Move the second statement up in line to become first!
           MVI	L, 202o               ;Load L with address of new SCAN pointer. Load
           MVI	M, 001                ;It with starting position for SYNTAX scan.
           CALL	SYNTX4             ;Use special entry to SYNTAX to get new TOKEN value.
           JMP	DIRECT             ;Process the second statement in the original line.
GOSUB:     MVI	L, 340o               ;Load L with start of LINE NUMBER BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;Fetch (cc) of cuffent line number into register D
           MOV	D,M                    ;Fetch high value (page) of pgm line pointer to D
           INR	D                    ;Test contents of register by first incrementing
           DCR	D                    ;And then decrementing the value in the register
           JZ	GOSUB1             ;If no line number, then processing a DIRECT statement
           MVI	L, 360o               ;Else, load L with address of user pgm buff line pointer
           MOV	D,M                    ;Fetch high value (page) of pgm line pointer to D
           INR	L                    ;Advance the memory pointer
           MOV	E,M                    ;Fetch the low part of pgm line pointer to E
GOSUB1:    MVI	L, 073o               ;Set L to address of GOSUB STACK POINTER
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of GOSUB STACK POINTER
           MOV	A,M                    ;Fetch value in GOSUB stack pointer to ACC
           ADI	002                ;Add two to current stack pointer for new data to be
           CPI	021o               ;Placed on the stack and see if stack overflows
           JP	GOSERR             ;If stack filled, have an error condition
           MOV	M,A                    ;Else, store updated stack pointer
           MVI	L, 076o               ;Load L with address of start of stack less offset (2)
           ADD	L                    ;Add GOSUB stack pointer to base address
           MOV	L,A                    ;To get pointer to top of stack (page byte)
           MOV	M,D                    ;Store page part of pgm buffer line pointer in stack
           INR	L                    ;Advance pointer to next byte in stack
           MOV	M,E                    ;Store low part of pgm buffer line pointer in stack
           JMP	GOTO               ;Proceed from here as though processing a GOTO
RETURN:    MVI	L, 073o               ;Set L to address of GOSUB STACK POINTER
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of GOSUB STACK POINTER
           MOV	A,M                    ;Fetch the value of GOSUB stack pointer to ACC
           SUI	002                ;Subtract two for data to be removed from stack
           JM	RETERR             ;If stack underflow, then have an error condition
           MOV	M,A                    ;Restore new stack pointer to memory
           ADI	002                ;Add two to point to previous top of stack
           MVI	L, 076o               ;Load L with address of start of GOSUB stack less two
           ADD	L                    ;Add address of previous top of stack to base value
           MOV	L,A                    ;Set pointer to high address value in the stack
           MOV	D,M                    ;Fetch the high address value from stack to register D
           INR	D                    ;Exercise the register contents to see if high address
           DCR	D                    ;Obtained is zero. If so, original GOSUB statement was
           JZ	EXEC               ;A DIRECT statement. Must return to Executive!
           INR	L                    ;Else, advance pointer to get low address value from the
           MOV	E,M                    ;Stack into CPU register E.
           MVI	L, 360o               ;Load L with address of user pgm line pointer storage
           MVI	H,PG26 ;\HB\OLDPG26   ;** Location. Load H with page of user pgm line pntr.
           MOV	M,D                    ;Put high address from stack into pgm line pointer.
           INR	L                    ;Advance the memory pointer
           MOV	M,E                    ;Put low address from stack into pgrn line pointer.
           JMP	NXTLIN             ;Execute the next line after originating GOSUB line!
GOSERR:    MVI	A, 307o               ;Load ASCII code for letter G into accumulator
           MVI	C, 323o               ;Load ASCII code for letter S into register C
           JMP	ERROR              ;Go display GoSub (GS) error message.
RETERR:    MVI	A, 322o               ;Load ASCII code for letter R into accumulator
           MVI	C, 324o               ;Load ASCII code for letter T into register C
           JMP	ERROR              ;Go display ReTurn (RT) error message.
INPUT:     CALL	CLESYM             ;Clear the SYMBOL BUFFER
           MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MOV	B,M                    ;Fetch value of SCAN pointer to register B
           INR	B                    ;Increment value to point to next chamcter
           MVI	L, 203o               ;Change L to point to INPUT pointer (formerly TOKEN)
           MOV	M,B                    ;Updated SCAN pointer becomes INPUT pointer
INPUT1:    MVI	L, 203o               ;Load L with address of INPUT pointer
           CALL	GETCHR             ;Fetch a character from the line input buffer
           JZ	INPUT3             ;If character is a space, ignore it. Else,
           CPI	254o               ;See if character is a comma. If so, process the
           JZ	INPUT4             ;Variable that preceeds the comma.
           CPI	250o               ;If not, see if character is a left parenthesis.
           JNZ	INPUT2             ;If not, continue processing to build up symbolic variable
           CALL	ARRAY2             ;@@ If so, call array subscripting subroutine
           MVI	L, 206o               ;@@ Load L with address of array set up pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;@@ ** Load H with page of array set up pointer
           MOV	B,M                    ;@@ Fetch pointer value (point to ")" of subscript)
           MVI	L, 203o               ;@@ Change pointer to address of INPUT pointer
           MOV	M,B                    ;@@ Update INPUT pointer
           JMP	INPUT3             ;@@ Jump over concatenate instruction below
INPUT2:    CALL	CONCTS             ;Concatenate character to SYMBOL BUFFER
INPUT3:    MVI	L, 203o               ;Load L with address of INPUT pointer
           CALL	LOOP               ;Increment INPUT pointer and test for end of line
           JNZ	INPUT1             ;If not end of line, go get next character
           CALL	INPUTX             ;If end of buffer, get input for variable in the SYMBOL
           CALL	STOSYM             ;BUFFER and store the value in the VARIABLES table
           JMP	NXTLIN             ;Then continue to interpret next statement line
INPUT4:    CALL	INPUTX             ;Get input from user for variable in SYMBOL BUFFER
           CALL	STOSYM             ;Store the inputted value in the VARIABLES table
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of INPUT pointer
           MVI	L, 203o               ;Set L to location of INPUT pointer
           MOV	B,M                    ;Fetch pointer value for last character examined
           MVI	L, 202o               ;Change L to point to SCAN pointer storage location
           MOV	M,B                    ;Update the SCAN pointer
           JMP	INPUT              ;Continue processing statement line for next variable
INPUTX:    MVI	L, 120o               ;Load L with start of SYMBOL BUFFER (contains cc)
           MOV	A,M                    ;Fetch the (cc) (length of symbol in the buffer) to ACC
           ADD	L                    ;Add (cc) to base address to set up
           MOV	L,A                    ;Pointer to last character in the SYMBOL BUFFER
           MOV	A,M                    ;Fetch the last character in the SYMBOL BUFFER
           CPI	244o               ;See if the last chamcter was a $ sign
           JNZ	INPUTN             ;If not a $ sign, get variable value as a numerical entry
           MVI	L, 120o               ;If $ sign, reset L to start of the SYMBOL BUFFER
           MOV	B,M                    ;Fetch the (cc) for the variable in the SYMBOL BUFF
           DCR	B                    ;Subtract one from (cc) to chop off the $ sign
           MOV	M,B                    ;Restore the new (cc) for the SYMBOL BUFFER
           CALL	FP0                ;Call subroutine to zero the floating point accumulator
           CALL	CINPUT             ;Input one character from system input device
           MVI	L, 124o               ;Load L with address of the LSW of the FPACC
           MOV	M,A                    ;Place the ASCII code for the character inputted there
           JMP	FPFLT              ;Convert value to floating point format in FPACC
INPUTN:    MVI	L, 144o               ;Load L with address of start of AUX SYMBOL BUFF
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of AUX SYMBOL BUFFER
           MVI	A, 277o               ;Load accumulator with ASCII code for ? mark
           CALL	ECHO               ;Call output subroutine to display the ? mark
           CALL	STRIN              ;Input string of characters (number) fm input device
           JMP	DINPUT             ;Convert decimal string into binary floating point nr.
FP0:       MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with floating point working registers page
           JMP	CFALSE             ;Zero the floating point accumulator & exit to caller
FOR:       MVI	L, 144o               ;Load L with address of AUX SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of AUX SYMBOL BUFFER
           MVI	M, 000                ;Initialize buffer by clearing first byte
           MVI	L, 146o               ;Load L with location of second character in buffer
           MVI	M, 000                ;Clear that location in case of single character variable
           MVI	L, 205o               ;Load L with address of FOR/NEXT STACK pointer
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of FOR/NEXT STACK pointer
           MOV	B,M                    ;Fetch the FOR/NEXT STACK pointer
           INR	B                    ;Increment it in preparation for pushing operation
           MOV	M,B                    ;Restore it back to its storage location
           MVI	L, 360o               ;Load L with address of user pgrn buffer line pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of line pointer
           MOV	D,M                    ;Fetch page address of pgm buffer line pntr into D
           INR	L                    ;Advance the memory pointer to pick up low part
           MOV	E,M                    ;Fetch low address of pgm buffer line pntr into E
           MOV	A,B                    ;Restore updated FOR/NEXT STACK pointer to ACC
           RLC                    ;Rotate it left to multiply by two, then rotate it again to
           RLC                    ;Multiply by four. Add this value to the base address of
           ADI	134o               ;The FOR/NEXT STACK to point to the new top of
           MOV	L,A                    ;The FOR/NEXT STACK and set up to point to stack
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H for page of the FOR/NEXT STACK
           MOV	M,D                    ;Store the page portion of the user pgrn buffer line pntr
           INR	L                    ;In the FORINEXT STACK, advance register 4 then
           MOV	M,E                    ;Store the low portion of the pgrn line pntr on the stack
           MVI	L, 325o               ;Change L to point to start of TO string which is stored
           MVI	H,PG01 ;\HB\OLDPG1    ;** In a text strings storage area on this page
           CALL	INSTR              ;Search the statement line for the occurrence of TO
           MOV	A,E                    ;Register E wiU be zero if TO not found. Move E to ACC
           ANA	A                    ;To make a test
           JNZ	FOR1               ;If TO found then proceed with FOR statement
FORERR:    MVI	A, 306o               ;Else have a For Error. Load ACC with ASCII code for
           MVI	C, 305o               ;Letter F and register C with code for letter E.
           JMP	ERROR              ;Then go display the FE message.
FOR1:      MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the SCAN pointer
           MOV	B,M                    ;Fetch pointer value to ACC (points to letter R in the
           INR	B                    ;For directive). Increment it to point to next character
           MVI	L, 204o               ;In the line. Change register L and set this value up
           MOV	M,B                    ;As an updated FOR pointer.
           MVI	L, 203o               ;Set L to address of TO pointer (formerly TOKEN)
           MOV	M,E                    ;Save pointer to TO in the TO pointer!
FOR2:      MVI	L, 204o               ;Load L with address of the FOR pointer
           CALL	GETCHR             ;Fetch a character from the statement line
           JZ	FOR3               ;If it is a space, ignore it
           CPI	275o               ;Test to see if character is the "=" sign
           JZ	FOR4               ;If so, variable name is in the AUX SYMBOLBUFFER
           MVI	L, 144o               ;If not, then set L to point to start of the AUX SYMBOL
           CALL	CONCT1             ;BUFFER and concatenate the character onto the buffer
FOR3:      MVI	L, 204o               ;Reset L to address of the FOR pointer
           CALL	LOOP               ;Increment the pointer and see if end of line
           JNZ	FOR2               ;If not end of line, continue looking for the "=" sign
           JMP	FORERR             ;If reach end of line before "=" sign, then have error
FOR4:      MVI	L, 204o               ;Set L with address of the FOR pointer
           MOV	B,M                    ;Fetch pointer value to ACC (pointing to sign)
           INR	B                    ;Increment it to skip over the "=" sign
           MVI	L, 276o               ;Set L to address of the EVAL pointer
           MOV	M,B                    ;Restore the updated pointer to storage
           MVI	L, 203o               ;Set L to the address of the TO pointer
           MOV	B,M                    ;Fetch pointer value to ACC (pointing to letter T in TO)
           DCR	B                    ;Decrement it to point to character before the T in TO
           MVI	L, 277o               ;Set L to EVAL FINISH pointer storage location
           MOV	M,B                    ;Store the EVAL FINISH pointer value
           CALL	EVAL               ;Evaluate the expression between the "=" sign and TO
           CALL	RESTSY             ;Directive. Place the variable name in the variables table.
           MVI	L, 144o               ;Load L with starting address of the AUX SYMBOL BF
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with the page of the AUX SYMBOL BUFF
           MOV	A,M                    ;Fetch the (cc) for the name in the buffer
           CPI	001o               ;See if the symbol (name) length is just one character
           JNZ	FOR5               ;If not, go directly to place name in FOR/NEXT STACK
           MVI	L, 146o               ;If so, set L to point to second character location in the
           MVI	M, 000                ;AUX SYMBOL BUFFER and set it equal to zero.
           JMP	FOR5               ;This jump directs program over ontrs/cntrs/table area
;;; LAST LINE SHOULD START AT 17 365 0ff5h
;;; PATCH AREA FOLLOWS THIS

;	db	(1000h-$) dup 0

           ORG	1000h              ;020#000

FPFIX:     MVI	L, 126o               ;Set L to point to MSW of FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to point to page of FPACC
           MOV	A,M                    ;Fetch MSW of FPACC
           MVI	L, 100o               ;Change pointer to SIGN indicator on same page
           MOV	M,A                    ;Place MSW of FPACC into SIGN indicator
           ANA	A                    ;Now test sign bit of MSW of FPACC
           CM	FPCOMP             ;Two's complement value in FPACC if negative
           MVI	L, 127o               ;Change pointer to FPACC Exponent register
           MVI	A, 027o               ;Set accumulator to 23 (decimal) for number of bits
           MOV	B,M                    ;Load FPACC Exponent into CPU register B
           INR	B                    ;Exercise the value in register B
           DCR	B                    ;To set CPU flags
           JM	FPZERO             ;If FPACC Exponent is negative set FPACC to zero
           SUB	B                    ;Subtract value of FPACC Exponent from 23 decimal
           JM	FIXERR             ;If Exp larger than 23 decimal cannot convert
           MOV	C,A                    ;Else place result in register C as counter for number
FPFIXL:    MVI	L, 126o               ;Of rotate ops. Set pointer to MSW of FPACC
           MVI	B, 003                ;Set precision counter (number of bytes in mantissa)
           CALL	ROTATR             ;Rotate FPACC right the number of places indicated
           DCR	C                    ;By count in register C to effectively rotate all the
           JNZ	FPFIXL             ;Significant bits to the left of the floating point decimal
           JMP	RESIGN             ;Point. Go check original sign & negate answer if req'd.

                                  ;Following subroutine clears the FPACC to the zero
                                  ;condition.

FPZERO:    MVI	L, 126o               ;Set L to point to MSW of FPACC
           XRA	A                    ;Clear the accumulator
           MOV	M,A                    ;Set the MSW of FPACC to zero
           DCR	L                    ;Decrement the pointer
           MOV	M,A                    ;Set the next significant word of FPACC to zero
           DCR	L                    ;Decrement the pointer
           MOV	M,A                    ;Set the LSW of FPACC to zero
           DCR	L                    ;Decrement the pointer
           MOV	M,A                    ;Set the auxiliary FPACC byte to zero
           RET                    ;Exit to calling routine

                                  ;The next instruction is a special entry point to
                                  ;the FPNORM subroutine that is used when a number is
                                  ;converted from fixed to floating point. The FPNORM
                                  ;label is the entry point when a number already in float-
                                  ;ing point fonnat is to be normalized.

FPFLT:     MVI	B, 027o               ;For fixed to float set CPU register B to 23 decimal
FPNORM:    MOV	A,B                    ;Get CPU register B into ACC to check for special case
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of FPACC
           MVI	L, 127o               ;Set L to FPACC Exponent byte
           ANA	A                    ;Set CPU flags to test what was in CPU register B
           JZ	NOEXC0             ;If B was zero then do standard normalization
           MOV	M,B                    ;Else set Exponent of FPACC to 23 decimal
NOEXC0:    DCR	L                    ;Change pointer to MSW of FPACC
           MOV	A,M                    ;Fetch MSW of FPACC into accumulator
           MVI	L, 100o               ;Change pointer to SIGN indicator storage location
           MOV	M,A                    ;Place the MSW of FPACC there for future reference
           ANA	A                    ;Set CPU flags to test MSW of FPACC
           JP	ACZERT             ;If sign bit not set then jump ahead to do next test
           MVI	B, 004                ;If sign bit set, number in FPACC is negative. Set up
           MVI	L, 123o               ;For two's complement operation
           CALL	COMPLM             ;And negate the value in the FPACC to make it positive
ACZERT:    MVI	L, 126o               ;Reset pointer to MSW of FPACC
           MVI	B, 004                ;Set precision counter to number of bytes in FPACC
LOOK0:     MOV	A,M                    ;Plus one. Fetch a byte of the FPACC.
           ANA	A                    ;Set CPU flags
           JNZ	ACNONZ             ;If find anything then FPACC is not zero
           DCR	L                    ;Else decrement pointer to NSW of FPACC
           DCR	B                    ;Decrement precision counter
           JNZ	LOOK0              ;Continue checking to see if FPACC contains anything
           MVI	L, 127o               ;Until precision counter is zero. If reach here then
           XRA	A                    ;Reset pointer to FPACC Exponent. Clear the ACC and
           MOV	M,A                    ;Clear out the FPACC Exponent. Value of FPACC is zip!
           RET                    ;Exit to calling routine
ACNONZ:    MVI	L, 123o               ;If FPACC has any value set pointer to LSW minus one
           MVI	B, 004                ;Set precision counter to number of bytes in FPACC
           CALL	ROTATL             ;Plus one for special cases. Rotate the contents of the
           MOV	A,M                    ;FPACC to the LEFT. Pointer will be set to MSW after
           ANA	A                    ;Rotate ops. Fetch MSW and see if have anything in
           JM	ACCSET             ;Most significant bit position. If so, have rotated enough
           INR	L                    ;If not, advance pointer to FPACC Exponent. Fetch
           MOV	B,M                    ;The value of the Exponent and decrement it by one
           DCR	B                    ;To compensate for the rotate left of the mantissa
           MOV	M,B                    ;Restore the new value of the Exponent
           JMP	ACNONZ             ;Continue rotating ops to normalize the FPACC
ACCSET:    MVI	L, 126o               ;Set pntr to FPACC MSW. Now must provide room for
           MVI	B, 003                ;Sign bit in nonnalized FPACC. Set precision counter.
           CALL	ROTATR             ;Rotate the FPACC once to the right now.
RESIGN:    MVI	L, 100o               ;Set the pointer to SIGN indicator storage location
           MOV	A,M                    ;Fetch the original sign of the FPACC
           ANA	A                    ;Set CPU flags
           RP                    ;If original sign of FPACC was positive, can exit now.

FPCOMP:    MVI	L, 124o               ; However, if original sign was negative, must now restore
           MVI	B, 003                ;The FPACC to negative by performing two's comple-
           JMP	COMPLM             ;Ment on FPACC. Return to caring rtn via COMPLM.

                                  ;Floating point ADDITION. Adds contents of FPACC to
                                  ;FPOP and leaves result in FPACC. Routine first checks
                                  ;to see if either register contains zero. If so addition
                                  ;result is already present!

FPADD:     MVI	L, 126o               ;Set L to point to MSW of FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Do same for register H
           MOV	A,M                    ;Fetch MSW of FPACC to accumulator
           ANA	A                    ;Set CPU flags after loading op
           JNZ	NONZAC             ;If accumulator non-zero then FPACC has some value
MOVOP:     MVI	L, 124o               ;But, if accumulator was zero then normalized FPACC
           MOV	D,H                    ;Must also be zero. Thus answer to addition is simply the
           MOV	E,L                    ;Value in FPOP. Set up pointers to transfer contents of
           MVI	L, 134o               ;FPOP to FPACC by pointing to the LSW of both
           MVI	B, 004                ;Registers and perform the transfer. Then exit to calling
           JMP	MOVEIT             ;Routine with answer in FPACC via MOVEIT.
NONZAC:    MVI	L, 136o               ;If FPACC was non-zero then check to see if FPOP has
           MOV	A,M                    ;Some value by obtaining MSW of FPOP
           ANA	A                    ;Set CPU flags after loading op. If MSW zero then
           RZ                    ;Normalized FPOP must be zero. Answer is in FPACC!

                                  ;If neither FPACC or FPOP was zero then must perform
                                  ;addition operation. Must first check to see if two num-
                                  ;bers are within significant mnge. If not, largest number
                                  ;is answer. If numbers within range, then must align ex-
                                  ;ponents before perforrning the addition of the man-
                                  ;tissa.

CKEQEX:    MVI	L, 127o               ;Set pointer to FPACC Exponent storage location.
           MOV	A,M                    ;Fetch the Exponent value to the accumulator.
           MVI	L, 137o               ;Change the pointer to the FPOP Exponent
           CMP	M                    ;Compare the values of the exponents. If they are the
           JZ	SHACOP             ;Same then can immediately proceed to add operations.
           MOV	B,A                    ;If not the same, store FPACC Exponent size in regis B
           MOV	A,M                    ;Fetch the FPOP Exponent size into the ACC
           SBB	B                    ;Subtract the FPACC Exponent from the FPOP Exp.
           JP	SKPNEG             ;If result is positive jump over the next few instructions
           MOV	B,A                    ;If result was negative, store the result in B
           XRA	A                    ;Clear the accumulator
           SBB	B                    ;Subtract register B to negate the original value
SKPNEG:    CPI	030o               ;See if difference is less than 24 decimal.
           JM	LINEUP             ;If so, can align exponents. Go do it.
           MOV	A,M                    ;If not, find out which number is largest. Fetch FPOP
           MVI	L, 127o               ;Exponent into ACC. Change pointer to FPACC Exp.
           SUB	M                    ;Subtract FPACC from FPOP. If result is negative then
           RM                    ;was larger. Return with answer in FPACC.
           MVI	L, 124o               ;If result was positive, larger value in FPOP. Set pointers
           JMP	MOVOP              ;To transfer FPOP into FPACC and then exit to caller.
LINEUP:    MOV	A,M                    ;Fetch FPOP Exponent into accumulator.
           MVI	L, 127o               ;Change pointer to FPACC Exponent.
           SUB	M                    ;Subtract FPACC Exponent from FPOP Exponent. If
           JM	SHIFT0             ;Result is negative FPACC is larger. Go shift FPOP.
           MOV	C,A                    ;If result positive FPOP larger, must shift FPACC. Store
MORACC:    MVI	L, 127o               ;Difference count in C. Reset pointer to FPACC Exp
           CALL	SHLOOP             ;Call the SHift LOOP to rotate FPACC mantissa RIGHT
           DCR	C                    ;And INCREMENT Exponent. Decr difference counter
           JNZ	MORACC             ;Continue rotate operations until diff counter is zero
           JMP	SHACOP             ;Go do final alignment and perform addition process
SHIFT0:    MOV	C,A                    ;Routine to shift FPOP. Set difference count into reg. C
MOROP:     MVI	L, 137o               ;Set pointer to FPOP Exponent.
           CALL	SHLOOP             ;Call the SHift LOOP to rotate FPOP mantissa RIGHT
           INR	C                    ;And INCREMENT Exponent. Then incr difference cntr
           JNZ	MOROP              ;Continue rotate opemtions until diff counter is zero
;;; The below two instructions are changed by PATCH NR.1
;;;SHACOP:    LLI 123                ;Set pointer to FPACC LSW minus one to provide extra
;;;           LMI 000                ;Byte for addition ops. Clear that location to zero.
SHACOP:	   CALL	PATCH1		; patch 1 inserts a few lines at 30-000
	   MOV	A,A
	
;;;           MVI L, 133
;;;           LMI 000                ;THIS IS PATCH #1
           MVI	L, 127o               ;Change pointer to FPACC Exponent
           CALL	SHLOOP             ;Rotate FPACC mantissa RIGHT & Increment Exponent
           MVI	L, 137o               ;Change pointer to FPOP Exponent
           CALL	SHLOOP             ;Rotate FPOP mantissa RIGHT & Increment Exponent
           MOV	D,H                    ;Rotate ops provide room for overflow. Now set up
           MVI	E, 123o               ;Pointers to LSW minus one for both FPACC & FPOP
           MVI	B, 004                ;(FPOP already set after SHLOOP). Set precision counter
           CALL	ADDER              ;Call quad precision ADDITION subroutine.
           MVI	B, 000                ;Set CPU register B to indicate standard normalization
           JMP	FPNORM             ;Go normalize the result and exit to caller.
SHLOOP:    MOV	B,M                    ;Shifting loop. First fetch Exponent currently being
           INR	B                    ;Pointed to and Increment the value by one.
           MOV	M,B                    ;Return the updated Exponent value to memory.
           DCR	L                    ;Decrement the pointer to mantissa portion MSW
           MVI	B, 004                ;Set precision counter
FSHIFT:    MOV	A,M                    ;Fetch MSW of mantissa
           ANA	A                    ;Set CPU flags after load ops
           JP	ROTATR             ;If MSB not a one can do normal rotate ops
BRING1:    RAL                    ;If MSB is a one need to set up carrv bit for the negative
           JMP	ROTR               ;Number case. Then make special entry to ROTATR sub

                                  ;The following subroutine moves the contents of a string
                                  ;of memory locations from the address pointed to by
                                  ;CPU registers H & L to the address specified by the con-
                                  ;tents of registers D & E when the routine is entered. The
                                  ;process continues until the counter in register B is zero.

MOVEIT:    MOV	A,M                    ;Fetch a word from memory string A
           INR	L                    ;Advance A string pointer
           CALL	SWITCH             ;Switch pointer to string B
           MOV	M,A                    ;Put word from string A into string B
           INR	L                   ;Advance B string pointer
           CALL	SWITCH             ;Switch pointer back to string A
           DCR	B                    ;Decrement loop counter
           RZ                    ;Return to calling routine when counter reaches zero
           JMP	MOVEIT             ;Else continue transfer operations

                                  ;The following subroutine SUBTRACTS the
                                  ;contents of the FLOATING POINT ACCUMULATOR from the
                                  ;contents of the FLOATING POINT OPERAND and
                                  ;leaves the result in the FPACC. The routine merely
                                  ;negates the value in the FPACC and then goes to the
                                  ;FPADD subroutine just presented.

FPSUB:     MVI	L, 124o               ;Set L to address of LSW of FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of FPACC
           MVI	B, 003                ;Set precision counter
           CALL	COMPLM             ;Two's complement the value in the FPACC
           JMP	FPADD              ;Now go add the negated value to perform subtraction!

                                  ;The first part of the FLOATING POINT MULTIPLI-
                                  ;CATION subroutine calls a subroutine to check the
                                  ;original signs of the numbers that are to be multi-
                                  ;plied and perform working register clearing functions.
                                  ;Next the exponents of the numbers to be multiplied
                                  ;are added together.

FPMULT:    CALL	CKSIGN             ;Call routine to set up registers & ck signs of numbers
ADDEXP:    MVI	L, 137o               ;Set pointer to FPOP Exponent
           MOV	A,M                    ;Fetch FPOP Exponent into the accumulator
           MVI	L, 127o               ;Change pointer to FPACC Exponent
           ADD	M                    ;Add FPACC Exponent to FPOP Exponent
           ADI	001                ;Add one more to total for algorithm compensation
           MOV	M,A                    ;Store result in FPACC Exponent location
SETMCT:    MVI	L, 102o               ;Change pointer to bit counter storage location
           MVI	M, 027o               ;Initialize bit counter to 23 decimal

                                  ;Next portion of the FPMULT routine is the iinplernen-
                                  ;tation of the algorithm illustrated in the flow chart
                                  ;above. This portion multiplies the values of the two
                                  ;mantissas. The final value is rounded off to leave the
                                  ;23 most significant bits as the answer that is stored
                                  ;back in the FPACC.

MULTIP:    MVI	L, 126o                ;Set pointer to MSW of FPACC mantissa
           MVI	B, 003                ;Set precision counter
           CALL	ROTATR             ;Rotate FPACC (multiplier) RIGHT into carry bit
           CC	ADOPPP             ;If carry is a one, add multiplicand to partial-product
           MVI	L, 146o                ;Set pointer to partial-product most significant byte
           MVI	B, 006                ;Set precision counter (p-p register is double length)
           CALL	ROTATR             ;Shift partial-product RIGHT
           MVI	L, 102o                ;Set pointer to bit counter storage location
           MOV	C,M                    ;Fetch current value of bit counter
           DCR	C                    ;Decrement the value of the bit counter
           MOV	M,C                    ;Restore the updated bit counter to its storage location
           JNZ	MULTIP             ;If have not multiplied for 23 (deciinal) bits, keep going
           MVI	L, 146o                ;If have done 23 (decimal) bits, set pntr to p-p MSW
           MVI	B, 006                ;Set precision counter (for double length)
           CALL	ROTATR             ;Shift partial-product once more to the RIGHT
           MVI	L, 143o                ;Set pointer to access 24'th bit in partial-product
           MOV	A,M                    ;Fetch the byte containing the 24'th bit
           RAL                    ;Position the 24'th bit to be MSB in the accumulator
           ANA	A                    ;Set the CPU flags after to rotate operation and test to
           CM	MROUND             ;See if 24'th bit of p-p is a ONE. If so, must round-off
           MVI	L, 123o                ;Now set up pointers
           MOV	E,L                    ;To perform transfer
           MOV	D,H                    ;Of the multiplication results
           MVI	L, 143o                ;From the partial-product location
           MVI	B, 004                ;To the FPACC

	
EXMLDV:    CALL	MOVEIT             ;Perform the transfer from p-p to FPACC
           MVI	B, 000                ;Set up CPU register B to indicate regular normalization
           CALL	FPNORM             ;Normalize the result of multiplication
           MVI	L, 101o               ;Now set the pointer to the original SIGNS indicator
           MOV	A,M                    ;Fetch the indicator
           ANA	A                    ;Exercise the CPU flags
           RNZ                    ;If indicator is non-zero, answer is positive, can exit her
           JMP	FPCOMP             ;If not, answer must be negated, exit via 2's complement.

                                  ;The following portions of the FPMULT
                                  ;routine set up working locations in memory by clearing
                                  ;locations for an expanded FPOP area and the partial-produc
                                  ;area. Next, the signs of the two numbers to be multiplied
                                  ;are examined. Negative numbers are negated
                                  ;in preparation for the multiplication
                                  ;algorithm. A SIGNS indicator register is set up during
                                  ;this process to indicate whether the final result of the
                                  ;multiplication should be positive or negative. (Negative
                                  ;if original signs of the two numbers to be multiplied are
                                  ;different.)

CKSIGN:    MVI	L, 140o               ;Set pointer to start of partial-product working area
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to proper page
           MVI	B, 010o               ;Set up a loop counter in CPU register B
           XRA	A                    ;Clear the accumulator

CLRNEX:    MOV	M,A                    ;Now clear out locations for the partial-product
           INR	L                    ;Working registers
           DCR	B                    ;Until the loop counter
           JNZ	CLRNEX             ;Is zero
CLROPL:    MVI	B, 004                ;Set a loop counter
           MVI	L, 130o               ;Set up pointer
CLRNX1:    MOV	M,A                    ;Clear out some extra registers so that the
           INR	L                    ;FPOP may be extended in length
           DCR	B                    ;Perform clearing ops until loop counter
           JNZ	CLRNX1             ;Is zero
           MVI	L, 101o               ;Set pointer to M/D SIGNS indicator storage location
           MVI	M, 001                ;Set initial value of SIGNS indicator to plus one
           MVI	L, 126o               ;Change pointer to MSW of FPACC
           MOV	A,M                    ;Fetch MSW of mantissa into accumulator
           ANA	A                    ;Test flags
           JM	NEGFPA             ;If MSB in MSW of FPACC is a one, number is negative
OPSGNT:    MVI	L, 136o               ;Set pointer to MSW of FPOP
           MOV	A,M                    ;Fetch MSW of mantissa into accumulator
           ANA	A                    ;Test flags
           RP                    ;Return to caller if number in FPOP is positive
           MVI	L, 101o               ;Else change pointer to M/D SIGNS indicator
           MOV	C,M                    ;Fetch the value in the SIGNS indicator
           DCR	C                    ;Decrement the value by one
           MOV	M,C                    ;Restore the new value back to storage location
           MVI	L, 134o               ;Set pointer to LSW of FPOP
           MVI	B, 003                ;Set precision counter
           JMP	COMPLM             ;Two's complement value of FPOP & return to caller
NEGFPA:    MVI	L, 101o               ;Set pointer to M/D SIGNS indicator
           MOV	C,M                    ;Fetch the value in the SIGNS indicator
           DCR	C                    ;Decrement the value by one
           MOV	M,C                    ;Restore the new value back to storage location
           MVI	L, 124o               ;Set pointer to LSW of FPACC
           MVI	B, 003                ;Set precision counter
           CALL	COMPLM             ;Two's complement value of FPACC
           JMP	OPSGNT             ;Proceed to check sign of FPOP

                                  ;The following subroutine adds the double length (six regis
                                  ;multiplicand in FPOP to the partial-product register when
                                  ;called on by the multiplication algorithm.

ADOPPP:    MVI	E, 141o               ;Pointer to LSW of partial-product
           MOV	D,H                    ;On same page as FPOP
           MVI	L, 131o               ;LSIV of FPOP which contains extended multiplicand
           MVI	B, 006                ;Set precision counter (double length working registers)
           JMP	ADDER              ;Add multiplicand to partial-product & return to caller

MROUND:    MVI	B, 003                ;Set up precision counter
           MVI	A, 100o               ;Prepare to add one to 24'th bit of partial-product
           ADD	M                    ;Add one to the 24'th bit of the partial-product
CROUND:    MOV	M,A                    ;Restore the updated byte to memory
           INR	L                    ;Advance the memory pointer to next most significant
           MVI	A, 000                ;Byte of partial-product, then clear ACC without
           ADC	M                    ;Disturbing carry bit. Now perform add with carry to
           DCR	B                    ;Propagate any rounding in the partial-product registers.
           JNZ	CROUND             ;If cotinter is not zero continue propagating any carry
           MOV	M,A                    ;Restore final byte to memory
           RET                    ;Exit to CALLling routine

FPDIV:     CALL	CKSIGN             ;Call routine to set up registers & ck signs of numbers
           MVI	L, 126o               ;Set pointer to MSW of FPACC (divisor)
           MOV	A,M                    ;Fetch MSW of FPACC to accumulator
           ANA	A                    ;Exercise CPU flags
           JZ	DVERR              ;If MSW of FPACC is zero go display 'DZ' error message
SUBEXP:    MVI	L, 137o               ;Set pointer to FPOP (dividend) Exponent
           MOV	A,M                    ;Get FPOP Exponent into accumulator
           MVI	L, 127o               ;Change pointer to FPACC (divisor) Exponent
           SUB	M                    ;Subtract divisor exponent from dividend exponent
           ADI	001                ;Add one for algorithm compensation
           MOV	M,A                    ;Place result in FPACC Exponent
SETDCT:    MVI	L, 102o               ;Set pointer to bit counter storage location
           MVI	M, 027o               ;Initialize bit counter to 23 decimal

                                  ;Main division algorithm for mantissas

DIVIDE:    CALL	SETSUB             ;Go subtmct divisor from dividend
           JM	NOGO               ;If result is negative then place a zero bit in quotient
           MVI	E, 134o               ;If result zero or positive then move remainder after
           MVI	L, 131o               ;Subtraction from working area to become new dividend
           MVI	B, 003                ;Set up moving pointers and initialize precision counter
           CALL	MOVEIT             ;Perform the transfer
           MVI	A, 001                ;Place a one into least significant bit of accumulator
           RAR                    ;And rotate it out into the carry bit
           JMP	QUOROT             ;Proceed to rotate the carry bit into the current quotient
NOGO:      XRA	A                    ;When result is negative, put a zero in the carry bit, then
QUOROT:    MVI	L, 144o               ;Set up pointer to LSW of quotient register
           MVI	B, 003                ;Set precision counter
           CALL	ROTL               ;Rotate carry bit into quotient by using special entry to
           MVI	L, 134o               ;ROTATL subroutine. Now set up pointer to dividend
           MVI	B, 003                ;LSW and set precision counter
           CALL	ROTATL             ;Rotate the current dividend to the left
           MVI	L, 102o               ;Set pointer to bit counter storage location
           MOV	C,M                    ;Fetch the value of the bit counter
           DCR	C                    ;Decrement the value by one
           MOV	M,C                    ;Restore the new counter value to storage
           JNZ	DIVIDE             ;If bit counter is not zero, continue division process
           CALL	SETSUB             ;After 23 (decimal) bits, do subtraction once more for
           JM	DVEXIT             ;Possible rounding. Jump ahead if no rounding required.
           MVI	L, 144o               ;If rounding required set pointer to LSW of quotient
           MOV	A,M                    ;Fetch LSW of quotient to accumulator
           ADI	001                ;Add one to 23rd bit of quotient
           MOV	M,A                    ;Restore updated LSW of quotient
           MVI	A, 000                ;Clear accumulator without disturbing carry bit
           INR	L                    ;Advance pointer to next significant byte of quotient
           ADC	M                    ;Propagate any carry as part of rounding process
           MOV	M,A                    ;Restore the updated byte of quotient
           MVI	A, 000                ;Clear ACC again without disturbing carry bit
           INR	L                    ;Advance pointer to MSW of quotient
           ADC	M                    ;Propagate any carry to finish rounding process
           MOV	M,A                    ;Restore the updated byte of quotient
           JP	DVEXIT             ;If most significant bit of quotient is zero, go finish up
           MVI	B, 003                ;If not, set precision counter
           CALL	ROTATR             ;And rotate quotient to the right to clear the sign bit
           MVI	L, 127o               ;Set pointer to FPACC Exponent
           MOV	B,M                    ;Fetch FPACC exponent
           INR	B                    ;Increment the value to compensate for the rotate right
           MOV	M,B                    ;Restore the updated exponent value
DVEXIT:    MVI	L, 143o               ;Set up pointers
           MVI	E, 123o               ;To transfer the quotient into the FPACC
           MVI	B, 004                ;Set precision counter
                                  ;THIS IS A CORRECTION FOUND IN THE NOTES
           JMP	EXMLDV             ;And exit through FPMULT routine at EXMLDV

                                  ;Subroutine to subtract divisor from dividend. Used by
                                  ;main DIVIDE subroutine.

SETSUB:    MVI	E, 131o               ;Set pointer to LSW of working area
           MOV	D,H                    ;On same page as FPACC
           MVI	L, 124o               ;Set pointer to LSW of FPACC (divisor)
           MVI	B, 003                ;Set precision counter
           CALL	MOVEIT             ;Perform transfer
           MVI	E, 131o               ;Reset pointer to LSW of working area (now divisor)
           MVI	L, 134o               ;Reset pointer to LSW of FPOP (dividend)
           MVI	B, 003                ;Set precision counter
           CALL	SUBBER             ;Subtract divisor from dividend
           MOV	A,M                    ;Get MSW of the result of the subtraction operations
           ANA	A                    ;Exercise CPU flags
           RET                    ;Return to caller with status
ADDER:     ANA	A                    ;Initialize the carry bit to zero upon entry
ADDMOR:    MOV	A,M                    ;Fetch byte from register group A
           CALL	SWITCH             ;Switch memory pointer to register group B
           ADC	M                    ;Add byte from A to byte from B with carry
           MOV	M,A                    ;Leave result in register group B
           DCR	B                    ;Decrement number of bytes (precision) counter
           RZ                    ;Return to caller when all bytes in group processed
           INR	L                    ;Else advance pointer for register group B
           CALL	SWITCH             ;Switch memory pointer back to register group A
           INR	L                    ;Advance the pointer for register group A
           JMP	ADDMOR             ;Continue the multi-byte addition operation

                                  ;N'th precision two's complement (negate)
                                  ;subroutine. Performs a two's complement on the multi-byte
                                  ;registers tarting at the address pointed
                                  ; to by H & L (least significant byte) upon entry.

COMPLM:    MOV	A,M                    ;Fetch the least significant byte of the number to ACC
           XRI	377o               ;Exclusive OR to complement the byte
           ADI	001                ;Add one to form two's complement of byte
MORCOM:    MOV	M,A                    ;Restore the negated byte to memory
           RAR                   ;Save the carry bit
           MOV	D,A                    ;In CPU register D
           DCR	B                    ;Decrement number of bytes (precision) counter
           RZ                    ;Return to caller when all bytes in number processed
           INR	L                    ;Else advance the pointer
           MOV	A,M                    ;Fetch the next byte of the number to ACC
           XRI	377o               ;Exclusive OR to complement the byte
           MOV	E,A                    ;Save complemented value in register E temporarily
           MOV	A,D                    ;Restore previous carry status to ACC
           RAL                    ;And rotate it out to the carry bit
           MVI	A, 000                ;Clear ACC without disturbing carry status
           ADC	E                    ;Add in any carry to complemented value
           JMP	MORCOM             ;Continue the two's complement procedure as req'd

                                  ;N'th precision rotate left subroutine. Rotates a multi-
                                  ;byte number left starting at the address initially
                                  ;specified by the contents of CPU registers H & L upon
                                  ;subroutine entry (LSW). First entry point will clear
                                  ;the carry bit before beginning rotate operations. Second
                                  ;entry point does not clear the carry bit.

ROTATL:    ANA	A                    ;Clear the carry bit at this entry point
ROTL:      MOV	A,M                    ;Fetch a byte from memory
           RAL                    ;Rotate it left (bring carry into LSB, push MSB to carry)
           MOV	M,A                    ;Restore rotated word to memory
           DCR	B                    ;Decrement precision counter
           RZ                    ;Exit to caller when finished
           INR	L                    ;Else advance pointer to next byte
           JMP	ROTL               ;Continue rotate left operations


                                  ;N'th precision rotate
                                  ;right subroutine. Opposite of
                                  ;above subroutine.

ROTATR:    ANA	A                    ;Clear the carry bit at this entry point
ROTR:      MOV	A,M                    ;Fetch a byte from memory
           RAR                    ;Rotate it right (carry into MSB, LSB to carry)
           MOV	M,A                    ;Restore rotated word to memory
           DCR	B                    ;Decrement precision counter
           RZ                    ;Exit to caller when finished
           DCR	L                    ;Else decrement pointer to next byte
           JMP	ROTR               ;Continue rotate right operations

                                  ;N'th precision subtraction subroutine.
                                  ;Number starting at location pointed to by D & E (least
                                  ;significant byte) is subtracted from number starting at
                                  ;address specified by contents of H & L.

SUBBER:    ANA	A                    ;Initialize the carry bit to zero upon entry
SUBTRA:    MOV	A,M                    ;Fetch byte from register group A
           CALL	SWITCH             ;Switch memory pointer to register group B
           SBB	M                    ;Subtract byte from group B ftom that in group A
           MOV	M,A                    ;Leave result in register group B
           DCR	B                    ;Decrement number of bytes (precision) counter
           RZ                    ;Return to caller when all bytes in group processed
           INR	L                    ;Else advance pointer for register group B
           CALL	SWITCH             ;Switch memory pointer back to register group A
           INR	L                    ;Advance the pointer for register group A
           JMP	SUBTRA             ;Continue the multi-byte subtraction operation

                                  ;The next subroutine will transfer the four byte
                                  ;register string (generally a number in floating point
                                  ;format) from the starting address pointed to by CPU
                                  ;registers H & L when the subroutine is entered to
                                  ;the FPACC (floating point accumulator registers).

FLOAD:     MVI	D,PG01 ;\HB\OLDPG1    ;** Set page address of FPACC
           MVI	E, 124o               ;Set address of least signficant byte of FPACC
           MVI	B, 004                ;Set precision counter to four bytes (mantissa bytes
           JMP	MOVEIT             ;Plus Exponent) and exit via the transfer routine

                                  ;The next several subroutines are used to perform
                                  ;floating pojnt register loading and transfer operations.

FSTORE:    MOV	E,L                    ;Transfer contents of register L to E
           MOV	D,H                    ;Transfer contents of register H to D
           MVI	L, 124o               ;Set L to least significant byte of FPACC mantissa
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set page to FPACC storage area
           JMP	SETIT              ;Go transfer FPACC contents to area pointed to by D&E
OPLOAD:    MVI	D,PG01 ;\HB\OLDPG1    ;** Set page to FPOP storage area
           MVI	E, 134o               ;Set pointer to least significant byte of FPOP
SETIT:     MVI	B, 004                ;Set precision counter. Transfer from H & L area to
           JMP	MOVEIT             ;Locations pointed to by D & E

                                  ;The next subroutine perforins a double transfer opera-
                                  ;tion. It first transfers the contents of the FPACC into
                                  ;the FPOP. It then transfers new data (as pointed to by
                                  ;H & L upon entry to the subroutine) into the FPACC.

FACXOP:    CALL	SAVEHL             ;Save contents of H & L upon entry to subroutine
           MVI	L, 124o               ;Set pointer to FPACC LSW
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set pointer to page of FPACC
           CALL	OPLOAD             ;Transfer FPACC to FPOP
           CALL	RESTHL             ;Recover original subroutine entry values for H & L
           JMP	FLOAD              ;Transfer registers pointed to by H & L into the FPACC

                                  ;Subroutine to save the contents of CPU registers D, E, H
                                  ;and L in a temporary storage area in memory.

SAVEHL:    MOV	A,H                    ;Transfer value in H to ACC
           MOV	B,L                    ;And value in L to B
           MVI	L, 200o               ;Now set L to start of tempomry storage locations
           MVI	H,PG01 ;\HB\OLDPG1    ;** And set H to storage area page
           MOV	M,A                    ;Save A (entry value of H) in memory
           INR	L                    ;Advance pointer
           MOV	M,B                    ;Save B (entry value of L) in memory
           INR	L                    ;Advance pointer
           MOV	M,D                    ;Save D in memory
           INR	L                    ;Advance pointer
           MOV	M,E                    ;Save E in memory
           MOV	H,A                    ;Restore entry value of H
           MOV	L,B                    ;Restore entry value of L
           RET                    ;Exit to calling routine

                                  ;Subroutine to restore the contents of CPU registers D,
                                  ;E, H and L from temporary storage in memory.

RESTHL:    MVI	L, 200o               ;Set L to start of temporary storage locations
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to storage area page
           MOV	A,M                    ;Fetch stored value for li iii ACC
           INR	L                    ;Advance pointer
           MOV	B,M                    ;Fetch stored value for L into B
           INR	L                    ;Advance pointer
           MOV	D,M                    ;Fetch stored value for T.)
           INR	L                    ;Advance pointer
           MOV	E,M                    ;Fetch stored value for
           MOV	H,A                    ;Restore  saved value for H
           MOV	L,B                    ;Restore saved value for L
           MOV	A,M                    ;Leave stored value for E in ACC
           RET                    ;Exit to calling routine

                                  ;Subroutine to exchange the contents of H & L with
                                  ;D & E.

SWITCH:    MOV	C,H                    ;Transfer register H to C temporarily
           MOV	H,D                    ;Place value of D into H
           MOV	D,C                    ;Now put former H from C into D
           MOV	C,L                    ;Transfer register L to C temporarily
           MOV	L,E                    ;Place value of E into L
           MOV	E,C                    ;Now put former L from C into E
           RET                    ;Exit to calling routine
GETINP:    MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of GETINP character counter
           MVI	L, 220o               ;Set L to address of GETINP character counter
           MOV	C,M                    ;Load counter value into CPU register C
           INR	C                    ;Exercise the counter in order
           DCR	C                    ;To set CPU flags. If counter is non-zero, then indexing
           JNZ	NOT0               ;Register (GETINP counter) is all set so jump ahead.
           MOV	L,E                    ;But, if counter zero, then starting to process a new
           MOV	H,D                    ;Character string. Transfer char string buffer pointer into
           MOV	C,M                    ;H & L and fetch the string's character count value (cc)
           INR	C                    ;Increment the (cc) by one to take account of (cc) byte
           CALL	INDEXC             ;Add contents of regis C to H & L to point to end of the
           MVI	M, 000                ;Character string in buffer and place a zero byte marker
NOT0:      MVI	L, 220o               ;Set L back to address of GETINP counter which is used
           MVI	H,PG01 ;\HB\OLDPG1    ;** As an indexing value. Set H to correct page.
           MOV	C,M                    ;Fetch the value of GETINP counter into register C
           INR	C                    ;Increment the value in C
           MOV	M,C                    ;Restore the updated value for future use
           MOV	L,E                    ;Bring the base address of the character string buffer into
           MOV	H,D                    ;CPU registers H & L
           CALL	INDEXC             ;Add contents of register C to form indexed address of
           MOV	A,M                    ;Next character to be fetched as input. Fetch the next
           ANA	A                    ;Character. Exercise the CPU flags.
           MVI	H,PG01 ;\HB\OLDPG1    ;** Restore page pointer to floating point working area
           RNZ                    ;If character is non-zero, not end of string, exit to calle
           MVI	L, 220o               ;If zero character, must reset GETINP counter for next
           MVI	M, 000                ;String. Reset pointer and clear GETINP counter to zero
           RET                    ;Then exit to calling routine

                                  ;Following subroutine causes register C to be used as an
                                  ;indexing register. Value in C is added to address in H
                                  ;and L to form new address.

INDEXC:    MOV	A,L                    ;Place value from register L into accumulator
           ADD	C                    ;Add quantity in register C
           MOV	L,A                    ;Restore updated value back to L
           RNC                    ;Exit to caller if no carry from addition
           INR	H                    ;But, if have carry then must increment register H
           RET                    ;Before returning to calling routine

                                  ;Main Decimal INPUT subroutine to convert strings of
                                  ;ASCII characters representing decimal fixed or floating
                                  ;point numbers to binary floating point numbers.

DINPUT:    MOV	E,L                    ;Save entry value of register L in E. (Pointer to buffer
           MOV	D,H                    ;Containing ASCII character string.) Do same for H to D.
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of floating point working registers
           MVI	L, 150o               ;Set L to start of decirnal-to-binary working area
           XRA	A                    ;Clear the accumulator
           MVI	B, 010o               ;Set up a loop counter
CLRNX2:    MOV	M,A                    ;Deposit zero in working area to initialize
           INR	L                    ;Advance the memory pointer
           DCR	B                    ;Decrement the loop counter
           JNZ	CLRNX2             ;Clear working area until loop counter is zero
           MVI	L, 103o               ;Set pointer to floating point temporary registers and
           MVI	B, 004                ;Indicators working area. Set up a loop counter.
CLRNX3:    MOV	M,A                    ;Deposit zero in working area to initialize
           INR	L                    ;Advance the memory pointer
           DCR	B                    ;Decrement the loop counter
           JNZ	CLRNX3             ;Clear working area until loop counter is zero
           CALL	GETINP             ;Fetch a character from the ASCII chax string buffer
           CPI	253o               ;(Typically the SYMBOL/TOKEN buffer). See if it is
           JZ	NINPUT             ;Code for + sign. Jump ahead if code for + sign.
           CPI	255o               ;See if code for minus (-) sign.
           JNZ	NOTPLM             ;Jump ahead if not code for minus sign. If code for
           MVI	L, 103o               ;Minus sign, set pointer to MINUS flag storage location.
           MOV	M,A                    ;Set the MINUS flag to indicate a minus number
NINPUT:    CALL	GETINP             ;Fetch another character from the ASCII char string
NOTPLM:    CPI	256o               ;See if character represents a period (decimal point) in
           JZ	PERIOD             ;Input string. Jump ahead if yes.
           CPI	305o               ;If not period, see if code for E as in Exponent
           JZ	FNDEXP             ;Jump ahead if yes.
           CPI	240o               ;Else see if code for space.
           JZ	NINPUT             ;Ignore space character, go fetch another character.
           ANA	A                    ;If none of the above see if zero byte
           JZ	ENDINP             ;Indicating end of input char string. If yes, jumn ahead.
           CPI	260o               ;If not end of string, check to see
           JM	NUMERR             ;If character represents
           CPI	272o               ;A valid decimal number (0 to 9)
           JP	NUMERR             ;Display error message if not a valid digit at this point!
           MVI	L, 156o               ;For valid digit, set pointer to MSW of temporary
           MOV	C,A                    ;Decimal to binary holding registers. Save character in C.
           MVI	A, 370o               ;Form mask for sizing in accumulator. Now see if
           ANA	M                    ;Holding register has enough room for the conversion of
           JNZ	NINPUT             ;Another digit. Ignore the input if no more room.
           MVI	L, IN_DIGIT_CC_L      ;If have room in register then set pointer to input digit
           MOV	B,M                    ;Counter location. Fetch the present value.
           INR	B                    ;Increment it to account for incoming digit.
           MOV	M,B                    ;Restore updated count to storage location.
           CALL	DECBIN             ;Call the DECimal to BINary conversion routine to add
           JMP	NINPUT             ;In the new digit in holding registers. Continue inputting.
PERIOD:    MOV	B,A                    ;Save character code in register B
           MVI	L, 106o               ;Set pointer to PERIOD indicator storage location
           MOV	A,M                    ;Fetch value in PERIOD indicator
           ANA	A                    ;Exercise CPU flags
           JNZ	NUMERR             ;If already have a period then display error message
           MVI	L, IN_DIGIT_CC_L      ;If not, change pointer to digit counter storage location
           MOV	M,A                    ;Clear the digit counter back to zero
           INR	L                    ;Advance pointer to PERIOD indicator
           MOV	M,B                    ;Set the PERIOD indicator
           JMP	NINPUT             ;Continue processing the input character string
FNDEXP:    CALL	GETINP             ;Get next character in Exponent
           CPI	253o               ;See if it is code for + sign
           JZ	EXPINP             ;Jump ahead if yes.
           CPI	255o               ;If not + sign, see if minus sign
           JNZ	NOEXPS             ;If not minus sign then jump ahead
           MVI	L, 104o               ;For minus sign, set pointer to EXP SIGN indicator
           MOV	M,A                    ;Set the EXP SIGN indicator for a minus exponent
EXPINP:    CALL	GETINP             ;Fetch the next character in the decimal exponent
NOEXPS:    ANA	A                    ;Exercise the CPU flags
           JZ	ENDINP             ;If character inputted was zero, then end of input string
           CPI	260o               ;If not end of string, check to see
           JM	NUMERR             ;If character represents
           CPI	272o               ;A valid decimal number (0 to 9)
           JP	NUMERR             ;Display error message if not a valid digit at this point!
           ANI	017o               ;Else trim the ASCII code to BCD
           MOV	B,A                    ;And save in register B
           MVI	L, 157o               ;Set pointer to input exponent storage location
           MVI	A, 003                ;Set accumulator equal to three
           CMP	M                    ;See if any previous digit in exponent greater than three
           JM	NUMERR             ;Display error message if yes
           MOV	C,M                    ;Else save any previous value in register C
           MOV	A,M                    ;And also place any previous value in accumulator
           ANA	A                    ;Clear the carry bit with this instruction
           RAL                    ;Single precision multiply by ten algorithm
           RAL                    ;Two rotate lefts equals times four
           ADD	C                    ;Adding in the digit makes total times five
           RAL                    ;Rotating left again equals times ten
           ADD	B                    ;now add in digit just inputted
           MOV	M,A                    ;Restore the value to exponent storage location
           JMP	EXPINP             ;Go get any additional exponent int)ut
ENDINP:    MVI	L, 103o               ;Set pointer to mantissa SIGN indicator
           MOV	A,M                    ;Fetch the SIGN indicator to the acclimulator
           ANA	A                    ;Exercise the CPU flags
           JZ	FININP             ;If SIGN indicator is zero, go finish up as nr is positive
           MVI	L, 154o               ;But, if indicator is non-zero, number is negative
           MVI	B, 003                ;Set pntr to LSW of storage registers, set precision entr
           CALL	COMPLM             ;Negate the triple-precision number in holding registers
FININP:    MVI	L, 153o               ;Set pointer to input storage LS~V minus one
           XRA	A                    ;Clear the accumulator
           MOV	M,A                    ;Clear the LSW minus one location
           MOV	D,H                    ;Set register D to floating point working page
           MVI	E, 123o               ;Set E to address of FPACC LSW minus one
           MVI	B, 004                ;Set precision counter
           CALL	MOVEIT             ;Move number from input register to FPACC
           CALL	FPFLT              ;Now convert the binary fixed point to floating point
           MVI	L, 104o               ;Set pointer to Exponent SIGN indicator location
           MOV	A,M                    ;Fetch the value of the EXP SIGN indicator
           ANA	A                    ;Exercise the CPU flags
           MVI	L, 157o               ;Reset pointer to input exponent storage location
           JZ	POSEXP             ;If EXP SIGN indicator zero, exponent is positive
           MOV	A,M                    ;Else, exponent is negative so must negate
           XRI	377o               ;The value in the input exponent storage location
           ADI	001                ;By performing this two's complement
           MOV	M,A                    ;Restore the negated value to exponent storage location
POSEXP:    MVI	L, 106o               ;Set pointer to PERIOD indicator storage location
           MOV	A,M                    ;Fetch the contents of the PERIOD indicator
           ANA	A                    ;Exercise the CPU flags
           JZ	EXPOK              ;If PERIOD indicator clear, no decimal point involved
           MVI	L, IN_DIGIT_CC_L      ;If have a decimal point, set pointer to digit counter
           XRA	A                    ;Storage location. Clear the accumulator.
           SUB	M                    ;And get a negated value of the digit counter in ACC
EXPOK:     MVI	L, 157o               ;Change pointer to input exponent storage location
           ADD	M                    ;Add this value to negated digit counter value
           MOV	M,A                    ;Restore new value to storage location
           JM	MINEXP             ;If new value is minus, skip over next subroutine
           RZ                    ;If new value is zero, no further processing required

                                  ;Following subroutine will multiply the floating point
                                  ;binary number stored in FPACC by ten tirnes the
                                  ;value stored in the deciinal exponent storage location.

FPX10:     MVI	L, 210o               ;Set pointer to registers containing floating point
           MVI	H,PG01 ;\HB\OLDPG1    ;** Binary representation of 10 (decimal).
           CALL	FACXOP             ;Transfer FPACC to FPOP and 10 (dec) to FPACC
           CALL	FPMULT             ;Multiply FPOP (formerly FPACC) by 10 (decimal)
           MVI	L, 157o               ;Set pointer to decimal exponent storage location
           MOV	C,M                    ;Fetch the exponent value
           DCR	C                    ;Decrement
           MOV	M,C                    ;Restore to storage
           JNZ	FPX10              ;If exponent value is not zero, continue multiplication
           RET                    ;When exponent is zero can exit. Conversion completed.

                                  ;Following subroutine will multiply the floating point
                                  ;binary number stored in PPACC by 0.1 times the value
                                  ;(negative) stored in the decimal exponent storage location

MINEXP:
FPD10:     MVI	L, 214o               ;Set pointer to registers containing floating point
           MVI	H,PG01 ;\HB\OLDPG1    ;** Binary representation of 0.1 (decimal).
           CALL	FACXOP             ;Transfer FPACC to FPOP and 0.1 (dec) to FPACC
           CALL	FPMULT             ;Multitply FPOP (formerly FPACC) by 0.1 (decimal)
           MVI	L, 157o               ;Set pointer to decimal exponent storage location
           MOV	B,M                    ;Fetch the exponent value
           INR	B                    ;Increment
           MOV	M,B                    ;Restore to storage
           JNZ	FPD10              ;If exponent value is not zero, continue multiplication
           RET                    ;When exponent is zero can exit. Conversion completed.

                                  ;Following subroutine is used
                                  ;to convert decimal charac-
                                  ;ters to binary fixed point forinat
                                  ;in a triple-precision format.

DECBIN:    CALL	SAVEHL             ;Save entry value of D, E, H and L in memory
           MVI	L, 153o               ;Set pointer to temporary storage location
           MOV	A,C                    ;Restore character inputted to accumulator
           ANI	017o               ;Trim ASCII code to BCD
           MOV	M,A                    ;Store temporarily
           MVI	E, 150o               ;Set pointer to working area LSW of multi-byte register
           MVI	L, 154o               ;Set another pointer to LSW of conversion register
           MOV	D,H                    ;Make sure D set to page of working area
           MVI	B, 003                ;Set precision counter
           CALL	MOVEIT             ;Move original value of conversion register to working
           MVI	L, 154o               ;Register. Reset pointer to LSW of conversion register.
           MVI	B, 003                ;Set precision counter
           CALL	ROTATL             ;Rotate register left, (Multiplies value by two.)
           MVI	L, 154o               ;Reset pointer to LSW.
           MVI	B, 003                ;Set precision counter
           CALL	ROTATL             ;Multiply by two again (total now times four).
           MVI	E, 154o               ;Set pointer to LSW of conversion register.
           MVI	L, 150o               ;Set pointer to LSW of working register (original value).
           MVI	B, 003                ;Set precision counter.
           CALL	ADDER              ;Add original value to rotated value (now times five).
           MVI	L, 154o               ;Reset pointer to LSW
           MVI	B, 003                ;Set precision counter
           CALL	ROTATL             ;Multiply by two once more (total now times ten).
           MVI	L, 152o               ;Set pointer to clear working register locatiotis
           XRA	A                    ;Clear the accumulator
           MOV	M,A                    ;Clear MSW of working register
           DCR	L                    ;Decrement pointer
           MOV	M,A                    ;Clear next byte
           MVI	L, 153o               ;Set pointer to current digit storage location
           MOV	A,M                    ;Fetch the current digit
           MVI	L, 150o               ;Change pointer to LSW of working register
           MOV	M,A                    ;Deposit the current digit in LSW of working register
           MVI	E, 154o               ;Set pointer to conversion register LSW
           MVI	B, 003                ;Set precision counter
           CALL	ADDER              ;Add current digit to conversion register to complete
           JMP	RESTHL             ;Conversion. Exit to caller by restoring CPU registers.
FPOUT:     MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to working area for floating point routines
           MVI	L, 157o               ;Set pointer to decimal exponent storage location
           MVI	M, 000                ;Initialize storage location to zero
           MVI	L, 126o               ;Change pointer to FPACC (number to be outputted)
           MOV	A,M                    ;And fetch MSW of FPACC
           ANA	A                    ;Test the contents of MSW of FPACC
           JM	OUTNEG             ;If most significant bit of MSW is a one, have a minus nr.
           MVI	A, 240o               ;Else number is positive, set ASCII code for space for a
           JMP	AHEAD1             ;Positive number and go display a space
OUTNEG:    MVI	L, 124o               ;If number in FPACC is negative must negate in order
           MVI	B, 003                ;To display. Set pntr to LSW of FPACC & set prec. cntr.
           CALL	COMPLM             ;Negate the number in the FPACC to make it positive
           MVI	A, 255o               ;But load ACC with ASCII code for minus sign
AHEAD1:    CALL	ECHO               ;Call user display driver to output space or minus sign
           MVI	L, 110o               ;Set pointer to FIXED/FLOAT indicator
           MOV	A,M                    ;Fetch value of FIXED/FLOAT indicator
           ANA	A                    ;Test contents of indicator. If contents are zero, calling
           JZ	OUTFLT             ;Routine has directed floating point output format.
           MVI	L, 127o               ;If indicator non-zero, fixed point fonnat requested if
           MVI	A, 027o               ;Possible. Point to FPACC Exponent. Put 23 decimal in
           MOV	B,M                    ;Accumulator. Fetch FPACC Exponent into register B
           INR	B                    ;And exercise the register to test its
           DCR	B                    ;Original contents. If FPACC Exponent is negative in
           JM	OUTFLT             ;Value then go to floating point output forrnat. If value
           SUB	B                    ;Is positive, subtract value from 23 (decimal). If result
           JM	OUTFLT             ;Negative, number is too big to use fixed format.
           JMP	OUTFIX             ;Else, can use fixed format so skip next routine
OUTFLT:    MVI	L, 110o               ;Set pointer to FIXED/FLOAT indicator.
           MVI	M, 000                ;Clear indicator to indicate floating point output format
           MVI	A, 260o               ;Load ASCII code for '0' into accumulator
           CALL	ECHO               ;Call user display driver to output '0' as first character
           MVI	A, 256o               ;Number string. Now load ASCII code for decimal point.
           CALL	ECHO               ;Call user display driver to output '.'as second character.
OUTFIX:    MVI	L, 127o               ;Set pointer to FPACC Exponent
           MVI	A, 377o               ;Load accumulator with minus one
           ADD	M                    ;Add value in FPACC Exponent
           MOV	M,A                    ;Restore compensated exponent value

                                  ;Next portion of routine establishes the value for the
                                  ;decimal exponent that will be outputted by processing
                                  ;the binary exponent value in the FPACC.

DECEXT:    JP	DECEXD             ;If compensated exponent value is zero or positive
           MVI	A, 004                ;Then go multiply FPACC by 0.1 (decimal). Else,
           ADD	M                    ;Add four to the exponent value.
           JP	DECOUT             ;If exponent now zero or positive, ready to output
           MVI	L, 210o               ;If exponent negative, multiply FPACC by 10 (decimal)
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set pointer to registers holding 10 (dec) in binary
           CALL	FACXOP             ;Floating point format. Set up for multiplication.
           CALL	FPMULT             ;Perform the multiplication. Answer in FPACC.
           MVI	L, 157o               ;Set pointer to decimal exponent storage location.
           MOV	C,M                    ;Each time the FPACC is multiplied by ten, need to
           DCR	C                    ;Decrement the value in the decinial exponent storage
           MOV	M,C                    ;Location. (This establishes decimal exponent value!)
DECREP:    MVI	L, 127o               ;Reset pointer to FPACC Exponent
           MOV	A,M                    ;Fetch value in exponent
           ANA	A                    ;Test value
           JMP	DECEXT             ;Repeat process as required
DECEXD:    MVI	L, 214o               ;If exponent is positive, multiply FPACC by 0.1
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set pointer to registers holding 0.1 dec in binary
           CALL	FACXOP             ;Floating point format. Set up for multipli(-ation.
           CALL	FPMULT             ;Perform the multiplication. Answer in FPACC.
           MVI	L, 157o               ;Set pointer to decimal exponent storage location.
           MOV	B,M                    ;Each time the FPACC is multiplied by one tenth, need
           INR	B                    ;To increment the value in the decimal exponent storage
           MOV	M,B                    ;Location. (This establishes decimal exponent value!)
           JMP	DECREP             ;Repeat process as required

                                  ;The next section outputs the mantissa
                                  ;(or fixed point number) by converting the value remaining
                                  ;in the FPACC (after the decimal exponent equivalent has
                                  ;been extracted from the original value if required by the
                                  ;previous routines) to a string of decirnal digits.
DECOUT:    MVI	E, 164o               ;Set pointer to LSW of output working register
           MOV	D,H                    ;Set D to same page value as H
           MVI	L, 124o               ;Set pointer to LSW of FPACC
           MVI	B, 003                ;Set precision counter
           CALL	MOVEIT             ;Move value in FPACC to output working register
           MVI	L, 167o               ;Set pointer to MSW plus one of output working register
           MVI	M, 000                ;Clear that location to 0
           MVI	L, 164o               ;Set pointer to LSW of output working register
           MVI	B, 003                ;Set precision counter
           CALL	ROTATL             ;Rotate register left once to compensate for sign bit
           CALL	OUTX10             ;Multiply output register by 10, overflow into N4SW+ 1
COMPEN:    MVI	L, 127o               ;Set pointer back to FPACC Exponent
           MOV	B,M                    ;Compensate for any remainder in the binary exponent
           INR	B                    ;By performing a rotate right on the output working
           MOV	M,B                    ;Register until the binary exponent becomes zero
           JZ	OUTDIG             ;Go output decimal digits when this loop is finished
           MVI	L, 167o               ;Binary exponent compensating loop. Setpointe'r to
           MVI	B, 004                ;Working register MSW+L. Set precision counter.
           CALL	ROTATR             ;Rotate working register to the right.
           JMP	COMPEN             ;Repeat loop as required.
OUTDIG:    MVI	L, 107o               ;Set pointer to output digit counter storage location
           MVI	M, 007                ;Initialize to value of seven
           MVI	L, 167o               ;Change pointer to output working register MSW+L
           MOV	A,M                    ;Fetch MSW+L byte containing BCD of digit to be
           ANA	A                    ;Displayed. Test the contents of this byte.
           JZ	ZERODG             ;If zero jump to ZERODG routine.
OUTDGS:    MVI	L, 167o               ;Reset pointer to working register MSW+L
           MOV	A,M                    ;Fetch BCD of digit to be outputted
           ANA	A                    ;Exercise CPU flags
           JNZ	OUTDGX             ;If not zero, go display the digit
           MVI	L, 110o               ;If zero, change pointer to FIXED/FLOAT indicator
           MOV	A,M                    ;Fetch the indicator into the accumulator
           ANA	A                    ;Test value of indicator
           JZ	OUTZER             ;If in floating point mode, go display the digit
           MVI	L, 157o               ;Else change pointer to decimal exponent storage
           MOV	C,M                    ;Location, which, for fixed point, will have a positive
           DCR	C                    ;Value for all digits before the decimal point. Decrement
           INR	C                    ;And increment to exercise flags. See if count is positive.
           JP	OUTZER             ;If positive, must display any zero digit.
           MVI	L, 166o               ;If not, change pointer to MSW of working register
           MOV	A,M                    ;And test to see if any significant digits coming up
           ANI	340o               ;By forming a mask and testing for presence of bits
           JNZ	OUTZER             ;If more significant digits coming up soon, display the
           RET                    ;Zero digit. Else, exit to calling routine. Finished.
OUTZER:    XRA	A                    ;Clear the accumulator to restore zero digit value
OUTDGX:    ADI	260o               ;Add 260 (octal) to BCD code in ACC to form ASCII
           CALL	ECHO               ;Code and call the user's display driver subroutine
DECRDG:    MVI	L, 110o               ;Set pointer to FIXED/FLOAT indicator storage
           MOV	A,M                    ;Fetch the indicator to the accumulator
           ANA	A                    ;Exercise the CPU flags
           JNZ	CKDECP             ;If indicator non-zero, doing fixed point output
           MVI	L, 107o               ;Else, get output digit counter
           MOV	C,M
           DCR	C                    ;Decrement the digit counter & restore to storage
           MOV	M,C
           JZ	EXPOUT             ;When digit counter is zero, go take care of exponent
PUSHIT:    CALL	OUTX10             ;Else push next BCD digit out of working register
           JMP	OUTDGS             ;And continue the outputting process
CKDECP:    MVI	L, 157o               ;For fixed point output, decimal exponent serves as
           MOV	C,M                    ;Counter for number of digits before decimal point
           DCR	C                    ;Fetch the counter and decrement it to account for
           MOV	M,C                    ;Current digit being processed. Restore to storage.
           JNZ	NODECP             ;If count does not go to zero, jump ahead.
           MVI	A, 256o               ;When count reaches zero, load ASCII code for period
           CALL	ECHO               ;And call user's display driver to display decimal point
NODECP:    MVI	L, 107o               ;Set pointer to output digit counter storage location
           MOV	C,M                    ;Fetch the digit counter
           DCR	C                    ;Decrement the value
           MOV	M,C                    ;Restore to storage
           RZ                    ;If counter reaches zero, exit to caller. Finished.
           JMP	PUSHIT             ;Else continue to output the number.
ZERODG:    MVI	L, 157o               ;If first digit of floating point number is a zero, set
           MOV	C,M                    ;Pointer to decimal exponent storage location.
           DCR	C                    ;Decrement the value to compensate for skipping
           MOV	M,C                    ;Display of first digit. Restore to storage.
           MVI	L, 166o               ;Change pointer to MSW of output working register
           MOV	A,M                    ;Fetch MSW of output working register
           ANA	A                    ;Test the contents
           JNZ	DECRDG             ;If non-zero, continue outputting
           DCR	L                    ;Else decrement pointer to next byte in working register
           MOV	A,M                    ;Fetch its contents
           ANA	A                    ;Test
           JNZ	DECRDG             ;If non-zero, continue outputting
           DCR	L                    ;Else decrement pointer to LSW of working register
           MOV	A,M                    ;Fetch its contents
           ANA	A                    ;Test
           JNZ	DECRDG             ;If non-zero, continue outputting
           MVI	L, 157o               ;If decimal mantissa is zero, set pointer to decirnal
           MOV	M,A                    ;Exponent storage and clear it
           JMP	DECRDG             ;Finish outputting

                                  ;Following routine multiplies the binary number in the
                                  ;output working register by ten to push the most signifi-
                                  ;cant digit out to the MSW+L byte.

OUTX10:    MVI	L, 167o               ;Set pointer to work ing register M SW+ 1
           MVI	M, 000                ;Clear it in preparation for receiving next digit pushed
           MVI	L, 164o               ;Into it. Change pointer to working register LSW.
           MOV	D,H                    ;Set up register D to same page as H.
           MVI	E, 160o               ;Set second pointer to LSW of second working register
           MVI	B, 004                ;Set precision counter
           CALL	MOVEIT             ;Move first working register into second
           MVI	L, 164o               ;Reset pointer to LSW of first working register
           MVI	B, 004                ;Set precision counter
           CALL	ROTATL             ;Rotate contents of first working register left (X 2)
           MVI	L, 164o               ;Reset pointer to LSW
           MVI	B, 004                ;Reset precision counter
           CALL	ROTATL             ;Rotate contents left again (X 4)
           MVI	L, 160o               ;Set pointer to LSW of original value in 2'nd register
           MVI	E, 164o               ;Set pointer to LSW of rotated value
           MVI	B, 004                ;Set precision counter
           CALL	ADDER              ;Add rotated value to original value (X 5)
           MVI	L, 164o               ;Reset pointer to LSW of first working register
           MVI	B, 004                ;Set precision counter
           CALL	ROTATL             ;Rotate contents left again (X 10)
           RET                    ;Exit to calling routine

                                  ;The final group of routines in the floating point output
                                  ;section take care of outputting the decimal exponent
                                  ;portion of floating point numbers.

EXPOUT:    MVI	L, 157o               ;Set pointer to decimal exponent storage location
           MOV	A,M                    ;Fetch value to the accumulator
           ANA	A                    ;Test the value
           RZ                    ;If zero, then no exponent portion. Exit to CALLler.
           MVI	A, 305o               ;Else, load ACC with ASCII code for letter E.
           CALL	ECHO               ;Display E for Exponent via user's display driver rtn
           MOV	A,M                    ;Get decimal exponent value back into ACC
           ANA	A                    ;Test again
           JM	EXOUTN             ;If value is negative, skip ahead
           MVI	A, 253o               ;If positive, load ASCII code for + sign
           JMP	AHEAD2             ;Jump to display the + sign
EXOUTN:    XRI	377o               ;When decimal exponent is negative, must negate
           ADI	001                ;Value for display purposes. Perform two's complement
           MOV	M,A                    ;And restore the negated value to storage location
           MVI	A, 255o               ;Load ASCII code for minus sign
AHEAD2:    CALL	ECHO               ;Display the ASCII character in ACC
           MVI	B, 000                ;Clear register B
           MOV	A,M                    ;Fetch the decimal exponent value back into ACC
SUB12:     SUI	012o               ;Subtract 10 (decimal) from value in ACC
           JM	TOMUCH             ;Break out of loop when accumulator goes negative
           MOV	M,A                    ;Else restore value to storage location
           INR	B                    ;Increment register B as a counter
           JMP	SUB12              ;Repeat loop to form tens value of decimal exponent
TOMUCH:    MVI	A, 260o               ;Load base ASCII value for digit into the accumulator
           ADD	B                    ;Add to the count in B to forin tens digit of decimal
           CALL	ECHO               ;Exponent. Display via user's driver subroutine
           MOV	A,M                    ;Fetch remainder of decimal exponent value
           ADI	260o               ;Add in ASCII base value to form final digit
           CALL	ECHO               ;Display second digit of decirnal exponent
           RET                    ;Finished outputting. Return to caller.
;;; The above RETURN SHOULD BE AT 25 367 15f7h

;;; NOW OPEN AREA UP TO 26 000 CAN BE USED FOR PATCHING...

;	db	(1600h-$) dup 0

	ORG	1600h	;026#000

	db	0		;DATA 000		; CC FOR INPUT LINE BUFFER
	db	79 dup (0)	;DATA *79 		; THE INPUT LINE BUFFER
	db	0,0,0,0		;DATA 000,000,000,000	; THESE ARE SYMBOL BUFFER STORAGE
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000	; SHOULD BE 26-120 TO 26 143
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000	; THESE LOCATIONS ARE AUXILIARY SYMBOL BUFFER
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000	; SHOULD BE 26 144 TO 26 175
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0		;DATA 000,000
	db	0		;DATA 000		; TEMP SCAN STORAGE REGISTER
	db	0		;DATA 000		; TAB FLAG
	db	0		;DATA 000		; EVAL CURRENT TEMP REG.
	db	0		;DATA 000		; SYNTAX LINE NUMBER
	db	0		;DATA 000		; SCAN TEMPORARY REGISTER
	db	0		;DATA 000		; STATEMENT TOKEN
	db	0,0		;DATA 000,000		; TEMPORARY WORKING REGISTERS
	db	0,0		;DATA 000,000		; ARRAY POINTERS
;;; NOW WE SHOULD BE UP TO 26 210 1688h
	db	0		;DATA 000		; OPERATOR STACK POINTER
	db	15 dup (0)	;DATA *15		; OPERATOR STACK
	db	0		;DATA 000		; FUN/ARRAY STACK POINTER
	db	7 dup (0)	;DATA *7			; FUNCTION/ARRAY STACK
;;; THE LAST BYTE SHOULD HAVE BEEN 26 237 169fh


	;; HEIRARCHY TABLE (FOR OUT OF STACK OPS)
	;; USED BY PARSER ROUTINE.
;;; This SHOULD START AT 26 240
	db	0		;DATA 000		; EOS
	db	3		;DATA 003		; PLUS SIGN
	db	3		;DATA 003		; MINUS SIGN
	db	4		;DATA 004		; MULTIPLICATION SIGN
	db	4		;DATA 004		; DIVISION SIGN
	db	5		;DATA 005		; EXPONENT SIGN
	db	6		;DATA 006		; LEFT PARENTHESIS
	db	1		;DATA 001		; RIGHT PARENTHESIS
	db	2		;DATA 002		; NOT ASSIGNED
	db	2		;DATA 002		; LESS THAN SIGN
	db	2		;DATA 002		; Equal sign
	db	2		;DATA 002		; GREATER THAN SIGN
	db	2		;DATA 002		; LESS THAN OR EQUAL COMBO
	db	2		;DATA 002		; EQUAL OR GREATER THAN
	db	2		;DATA 002		; LESS THAN OR GREATER THAN

	;; HEIRARCHY TABLE (FOR INTO STACK OPS)
	;; USED BY PARSER ROUTINE.
;;; This SHOULD START AT 26 257 16afh
	db	0		;DATA 000		; EOS
	db	3		;DATA 003		; PLUS SIGN
	db	3		;DATA 003		; MINUS SIGN
	db	4		;DATA 004		; MULTIPLICATION SIGN
	db	4		;DATA 004		; DIVISION SIGN
	db	5		;DATA 005		; EXPONENTIATION SIGN
	db	1		;DATA 001		; LEFT PARENTHESIS
	db	1		;DATA 001		; RIGHT PARENTHESIS
	db	2		;DATA 002		; NOT ASSIGNED
	db	2		;DATA 002		; LESS THAN SIGN
	db	2		;DATA 002		; EQUAL SIGN
	db	2		;DATA 002		; GREATER THAN SIGN
	db	2		;DATA 002		; LESS THAN OR EQUAL SIGN
	db	2		;DATA 002		; EQUAL TO OR GREATER THAN
	db	2		;DATA 002		; LESS THAN OR GREATER THAN

	db	0		;DATA 000		; EVAL START POINTER
	db	0		;DATA 000		; EVAL FINISH POINTER

	;; FUNCTION NAMES TABLE
;;; This SHOULD START AT 26 300 16c0h

	db	3		;DATA 3
;	db	"INT"		;DATA "INT"
	db	"I"+80h
	db	"N"+80h
	db	"T"+80h
	db	3		;DATA 3
;	db	"SGN"		;DATA "SGN"
	db	"S"+80h
	db	"G"+80h
	db	"N"+80h
	db	3		;DATA 3
;	db	"ABS"		;DATA "ABS"
	db	"A"+80h
	db	"B"+80h
	db	"S"+80h
	db	3		;DATA 3
;	db	"SQR"		;DATA "SQR"
	db	"S"+80h
	db	"Q"+80h
	db	"R"+80h
	db	3		;DATA 3
;	db	"TAB"		;DATA "TAB"
	db	"T"+80h
	db	"A"+80h
	db	"B"+80h
	db	3		;DATA 3
;	db	"RND"		;DATA "RND"
	db	"R"+80h
	db	"N"+80h
	db	"D"+80h
	db	3		;DATA 3
;	db	"CHR"		;DATA "CHR"
	db	"C"+80h
	db	"H"+80h
	db	"R"+80h
	db	3		;DATA 3
;	db	"UDF"		;DATA "UDF"
	db	"U"+80h
	db	"D"+80h
	db	"F"+80h
	db	0,0,0,0		;DATA 000,000,000,000	; LINE NUMBER BUFFER STORAGE
	db	0,0,0,0		;DATA 000,000,000,000	; (SHOULD BE 340-347)
	db	0,0,0,0		;DATA 000,000,000,000	; AUX LINE NUMBER BUFFER
	db	0,0,0,0		;DATA 000,000,000,000	; (SHOULD BE 350-357)
;;; The following data is a change in page 3 of Scelbal update issue 4
;;; which apparently makes the "INSERT" command work correctly, the
;;; first time (later SCR commands load 33 into this spot) 
	db	033o		;DATA 033 		; USER PGM LINE PTR (PG)
	db	0		;DATA 000 		; USER PGM LINE PTR (LOW)
	db	0		;DATA 000 		; AUX PGM LINE PTR (PG)
	db	0		;DATA 000 		; AUX PGM LINE PTR (LOW)
	db	0		;DATA 000 		; END OF USER PGM BUFFER PTR (PG)
	db	0		;DATA 000 		; END OF USER PGM BUFFER PTR (LOW)
	db	0		;DATA 000		; PARENTHESIS COUNTER (366)
	db	0		;DATA 000		; QUOTE INDICATOR
	db	0		;DATA 000		; TABLE COUNTER (370)
;;; locations 371-377 NOT ASSIGNED

;	db	(1700h-$) dup 0

	org	1700h		;ORG 027#000
	db	3		;DATA 3
;	db	"REM"		;DATA "REM"
	db	"R"+80h
	db	"E"+80h
	db	"M"+80h
	db	2		;DATA 2
;	db	"IF"		;DATA "IF"
	db	"I"+80h
	db	"F"+80h
	db	3		;DATA 3
;	db	"LET"		;DATA "LET"
	db	"L"+80h
	db	"E"+80h
	db	"T"+80h
	db	4		;DATA 4
;	db	"GOTO"		;DATA "GOTO"
	db	"G"+80h
	db	"O"+80h
	db	"T"+80h
	db	"O"+80h
	db	5		;DATA 5
;	db	"PRINT"		;DATA "PRINT"
	db	"P"+80h
	db	"R"+80h
	db	"I"+80h
	db	"N"+80h
	db	"T"+80h
	db	5		;DATA 5
;	db	"INPUT"		;DATA "INPUT"
	db	"I"+80h
	db	"N"+80h
	db	"P"+80h
	db	"U"+80h
	db	"T"+80h
	db	3		;DATA 3
;	db	"FOR"		;DATA "FOR"
	db	"F"+80h
	db	"O"+80h
	db	"R"+80h
	db	4		;DATA 4
;	db	"NEXT"		;DATA "NEXT"
	db	"N"+80h
	db	"E"+80h
	db	"X"+80h
	db	"T"+80h
	db	5		;DATA 5
;	db	"GOSUB"		;DATA "GOSUB"
	db	"G"+80h
	db	"O"+80h
	db	"S"+80h
	db	"U"+80h
	db	"B"+80h
	db	6		;DATA 6
;	db	"RETURN"	;DATA "RETURN"
	db	"R"+80h
	db	"E"+80h
	db	"T"+80h
	db	"U"+80h
	db	"R"+80h
	db	"N"+80h
	db	3		;DATA 3
;	db	"DIM"		;DATA "DIM"
	db	"D"+80h
	db	"I"+80h
	db	"M"+80h
	db	3		;DATA 3
;	db	"END"		;DATA "END"
	db	"E"+80h
	db	"N"+80h
	db	"D"+80h
	db	0		;DATA 0

	; END OF TABLE, SHOULD BE 072 3ah

	db	0		;DATA 000		; GOSUB STACK POINTER
	db	0		;DATA *1			; NOT ASSIGNED;
	db	0		;DATA 000		; NUMBER OF ARRAYS COUNTER
	db	0		;DATA 000		; ARRAY POINTER
	db	0		;DATA 000		; VARIABLES COUNTER SHOULD BE 077
	db	0,0,0,0		;DATA 000,000,000,000	; USED AS THE GOSUB STACK 100-117
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000	; USED AS ARRAY VARIABLES TABLE
	db	0,0,0,0		;DATA 000,000,000,000	; SHOULD BE 120-137
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000

	db	0,0,0,0		;DATA 000,000,000,000	; USED FOR FOR/NEXT STACK STORAGE
	db	0,0,0,0		;DATA 000,000,000,000	; SHOULD BE 140 TO 177
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0,0,0,0		;DATA 000,000,000,000
	db	0		;DATA 000		; FOR/NEXT STACK POINTER
	db	0		;DATA 000		; ARRAY/VARIABLE FLAG
	db	0		;DATA 000  		; STOSYM COUNTER
	db	0		;DATA 000		; FUN/ARRAY STACK POINTER (203
	db	0		;DATA 000		; ARRAY VALUES POINTER
	db	0,0,0		;DATA *3			; NOT USED (SHOULD BE 205-207)
	db	0		;DATA 000		; USED AS VARIABLES SYMBOL TABLE
	db	119 dup (0)	;DATA *119		; (SHOULD BE 211-377 RESERVED)
	;; THERE ARE NOW ADDRESSES AT START OF PAGE 30, NOT ASSIGNED;

;	db	(1800h-$) dup 0

;;; The following is PATCH NR.1
           ORG	1800h	;030#000
PATCH1:    MVI	L, 123o
           MVI	M, 000
           MVI	L, 133o
           MVI	M, 000
           RET

;	db	(180bh-$) dup 0

           ORG 	180bh	;030#013
NEXT:      MVI	L, 144o               ;Load L with start of AUX SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of AUX SYMBOL BUFFER
           MVI	M, 000                ;Initialize AUX SYMBOL BUFFER by clearing first byte
           MVI	L, 202o               ;Change L to address of SCAN pointer
           MOV	B,M                    ;Fetch pointer value to CPU register B
           INR	B                    ;Add one to the current pointer value
           MVI	L, 201o               ;Load L with address of NEXT pointer storage location
           MOV	M,B                    ;Place the updated SCAN pointer as the NEXT pointer
NEXT1:     MVI	L, 201o               ;Reset L to address of NEXT pointer storage location
           CALL	GETCHR             ;Fetch the character pointed to by the NEXT pointer
           JZ	NEXT2              ;If the character is a space, ignore it
           MVI	L, 144o               ;Else, load L with start of AUX SYMBOL BUFFER
           CALL	CONCT1             ;Concatenate the character onto the AUX SYMBOL BF
NEXT2:     MVI	L, 201o               ;Reset L to address of NEXT pointer storage location
           CALL	LOOP               ;Advance the NEXT pointer and see if end of line
           JNZ	NEXT1              ;Fetch next character in line if not end of line
           MVI	L, 144o               ;When reach end of line, should have variable name
           MOV	A,M                    ;In the AUX SYMBOL BUFFER. Fetch the (cc) for
           CPI	001                ;The buffer and see if variable name is just one letter
           JNZ	NEXT3              ;If more than one proceed directly to look for name
           MVI	L, 146o               ;In FOR/NEXT STACK. If have just a one letter name
           MVI	M, 000                ;Then set second character in buffer to zero
NEXT3:     MVI	L, 205o               ;Load L with address of FOR/NEXT STACK pointer
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of FOR/NEXT STACK pointer
           MOV	A,M                    ;Fetch the FOR/NEXT STACK pointer value to ACC
           RLC                    ;Rotate value left to multiply by two. Then rotate it
           RLC                    ;Left again to multiply by four. Add base address plus
           ADI	136o               ;Two to form pointer to variable name in top of stack
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of FOR/NEXT STACK
           MOV	L,A                    ;Move pointer value from ACC to CPU register L
           MVI	D,PG26 ;\HB\OLDPG26   ;** Set register D to page of AUX SYMBOL BUFFER
           MVI	E, 145o               ;Set register E to first character in the buffer
           MVI	B, 002                ;Set B to serve as a character counter
           CALL	STRCPC             ;See if variable name in the NEXT statement same as
           JZ	NEXT4              ;That stored in the top of the FOR/NEXT STACK
FORNXT:    MVI	A, 306o               ;Load ACC with ASCII code for letter F
           MVI	C, 316o               ;Load register C with ASCII code for letter N
           JMP	ERROR              ;Display For/Next (FN) error message if required
NEXT4:     MVI	L, 360o               ;Load L with address of user program line pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of user pgm line pntr storage loc.
           MOV	D,M                    ;Fetch the page portion of the line pointer into D
           INR	L                    ;Advance the memory pointer
           MOV	E,M                    ;Fetch the low portion of the line pointer into E
           INR	L                    ;Advance pntr to AUXILIARY LINE POINTER storage
           MOV	M,D                    ;Location and store value of line pointer there too (page)
           INR	L                    ;Advance pointer to second byte of AUXILIARY line
           MOV	M,E                    ;Pointer and store value of line pointer (low portion)
           MVI	L, 205o               ;Load L with address of FOR/NEXT STACK pointer
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of FOR/NEXT STACK pointer
           MOV	A,M                    ;Fetch the FOR/NEXT STACK pointer value to ACC
           RLC                    ;Rotate value left to multiply by two. Then rotate it
           RLC                    ;Left again to multiply by four. Add base address to
           ADI	134o               ;Form pointer to top of FOR/NEXT STACK and place
           MOV	L,A                    ;The pointer value into CPU register L. Fetch the page
           MOV	D,M                    ;Address of the associated FOR statement line pointer
           INR	L                    ;Into register D. Advance the pointer and fetch the low
           MOV	E,M                    ;Address value into register E. Prepare to change user
           MVI	L, 360o               ;Program line pointer to the FOR statement line by
           MVI	H,PG26 ;\HB\OLDPG26   ;** Setting H & L to the user pgrn line pntr storage loc.
           MOV	M,D                    ;Place the page value in the pointer storage location
           INR	L                    ;Advance the memory pointer
           MOV	M,E                    ;Place the low value in the pointer storage location
           MOV	H,D                    ;Now set up H and L to point to the start of the
           MOV	L,E                    ;Associated FOR statement line in the user pgm buffer
           MVI	D,PG26 ;\HB\OLDPG26   ;** Change D to point to the line input buffer
           MVI	E, 000                ;And set L to the gtart of the line input buffer
           CALL	MOVEC              ;Move the associated FOR statement line into the input
           MVI	L, 325o               ;Line buffer. Set L to point to start of TO string which is
           MVI	H,PG01 ;\HB\OLDPG1    ;** Stored in a text strings storage area on this page
           CALL	INSTR              ;Search the statement line for the occurrence of TO
           MOV	A,E                    ;Register E will be zero if TO not found. Move E to ACC
           ANA	A                    ;To make a test. If TO found then proceed to set up for
           JZ	FORNXT             ;Evaluation. If TO not found, then have error condition.
           ADI	002                ;Advance the pointer over the characters in TO string
           MVI	L, 276o               ;Change L to point to EVAL pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of EVAL pointer. Set up the starting
           MOV	M,A                    ;Position for the EVAL subroutine (after TO string)
           MVI	L, 330o               ;Set L to point to start of STEP string which is stored
           MVI	H,PG01 ;\HB\OLDPG1    ;** In text stxings storage area on this page. Search the
           CALL	INSTR              ;Statement line for the occurrence of STEP
           MOV	A,E                    ;Register E will be zero if STEP not found. Move E to
           ANA	A                    ;The accumulator to make a test. If STEP found must
           JNZ	NEXT5              ;Evaluate expression after STEP to get STEP SIZE.
           MVI	L, 004                ;Else, have an IMPLIED STEP SIZE of 1.0. Set pointer
           MVI	H,PG01 ;\HB\OLDPG1    ;** To start of storage area for 1.0 in floating point
           CALL	FLOAD              ;Format and call subroutine to load FPACC with 1.0
           MVI	L, 304o               ;Set L to start of FOR/NEXT STEP SIZE storage loc.
           CALL	FSTORE             ;Store the value 1.0 in the F/N STEP SIZE registers
           MVI	L, 000                ;Change L to the start of the input line buffer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to the page of the input line buffer
           MOV	B,M                    ;Fetch the (cc) into CPU register B (length of FOR line)
           MVI	L, 277o               ;Change L to EVAL FINISH pointer stomge location
           MOV	M,B                    ;Set the EVAL FINISH pointer to the end of the line
           CALL	EVAL               ;Evaluate the LIMIT expression to obtain FOR LIMIT
           MVI	L, 310o               ;Load L with address of start of F/N LIMIT registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of FOR/NEXT LIMIT registers
           CALL	FSTORE  ;MGA 3/31/12 no lab here Store the FOR/NEXT LIMIT value
           JMP	NEXT6              ;Since have IMPLIED STEP jump ahead
NEXT5:     DCR	E  ;MGA 3/21/12 lab here When have STEP directive, subtract one from pointer
           MVI	L, 277o               ;To get to character before S in STEP. Save this value in
           MVI	H,PG26 ;\HB\OLDPG26   ;** The EVAL FINISH pointer stomge location to serve
           MOV	M,E                    ;As evaluation end location when obtaining TO Iiinit
           CALL	EVAL               ;Evaluate the LIMIT expression to obtain FOR LIMIT
           MVI	L, 310o               ;Load L with address of start of FIN LIMIT registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Load H with page of FORINEXT LIMIT registers
           CALL	FSTORE             ;Store the FOR/NEXT LIMIT value
           MVI	L, 277o               ;Reset L to EVAL FINISH pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of EVAL FINISH pointer storage loc.
           MOV	A,M                    ;Fetch the pointer value (character before S in STEP)
           ADI	005                ;Add five to change pointer to character after P in STEP
           DCR	L                    ;Decrement L to point to EVAL (start) pointer
           MOV	M,A                    ;Set up the starting position for the EVAL subroutine
           MVI	L, 000                ; Load L with starting address of the line input buffer
           MOV	B,M                    ;Fetch the (cc) for the line input buffer (line length)
           MVI	L, 277o               ;Change L to the EVAL FINISH storage location
           MOV	M,B                    ;Set the EVAL FINISH pointer
           CALL	EVAL               ;Evaluate the STEP SIZE expression
           MVI	L, 304o               ;Load L with address of start of F/N STEP registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of FIN STEP registers
           CALL	FSTORE             ;Store the FOR/NEXT STEP SIZE value
NEXT6:     MVI	L, 144o               ;Load L with address of AUX SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the AUX SYMBOL BUFFER
           MVI	M, 000                ;Initialize AUX SUMBOL BUFFER with a zero byte
           MVI	L, 034o               ;Set L to start of FOR string which is stored in the
           MVI	H,PG27 ;\HB\OLDPG27   ;** KEYWORD look-up table on this page
           CALL	INSTR              ;Search the statement line for the FOR directive
           MOV	A,E                    ;Register E will be zero if FOR not found. Move E to
           ANA	A                    ;ACC and -make test to see if FOR directive located
           MVI	L, 202o               ;Load L with address of SCAN pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SCAN pointer
           MOV	M,A                    ;Set up pointer to occurrence of FOR directive in line
           JZ	FORNXT             ;If FOR not found, have an error condition
           ADI	003                ;If have FOR, add three to advance pointer over FOR
           MVI	L, 203o               ;Set L to point to F/N pointer storage location
           MOV	M,A                    ;Set F/N pointer to character after FOR directive
NEXT7:     MVI	L, 203o               ;Set L to point to FIN pointer storage location
           CALL	GETCHR             ;Fetch a character from position pointed to by FIN pntr
           JZ	NEXT8              ;If character is a space, ignore it
           CPI	275o               ;Else, test to see if character is "=" sign
           JZ	NEXT9              ;If yes, have picked up variable name, jump ahead
           MVI	L, 144o               ;If not, set L to the start of the AUX SYMBOL BUFFER
           CALL	CONCT1             ;And store the character in the AUX SYMBOL BUFFER
NEXT8:     MVI	L, 203o               ;Load L with address of the F/N pointer
           CALL	LOOP               ;Increment the pointer and see if end of the line
           JNZ	NEXT7              ;If not, continue fetching characters
           JMP	FORNXT             ;If end of line before "=" sign then have error condx
NEXT9:     MVI	L, 202o               ;Load L with address of SCAN pointer
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SCAN pointer
           MOV	A,M                    ;Fetch pointer value to ACC (points to start of FOR
           ADI	003                ;Directive) and add three to move pointer over FOR
           MVI	L, 276o               ;Directive. Change L to EVAL pointer storage location
           MOV	M,A                    ;Set EVAL pointer to character after FOR in line
           MVI	L, 203o               ;Load L with address of FIN pointer storage location
           MOV	B,M                    ;Fetch pointer to register B (points to "=" sign) and
           DCR	B                    ;Decrement the pointer (to character before "=" sign)
           MVI	L, 277o               ;Load L with address of EVAL FINISH pointer
           MOV	M,B                    ;Set EVAL FINISH pointer
           CALL	EVAL               ;Call subroutine to obtain current value of the variable
           MVI	L, 304o               ;Load L with address of start of F/N STEP registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of F/N STEP registers
           CALL	FACXOP             ;Call subroutine to set up FP registers for addition
           CALL	FPADD              ;Add FIN STEP size to current VARIABLE value
           MVI	L, 314o               ;Load L with address of FIN TEMP storage registers
           MVI	H,PG01 ;\HB\OLDPG1    ;**Set H to page of FIN TEMP storage registers
           CALL	FSTORE             ;Save the result of the addition in F/N TEMP registers
           MVI	L, 310o               ;Load L with starting address of F/N LIMIT registers
           CALL	FACXOP             ;Call subroutine to set up FP registers for subtraction
           CALL	FPSUB              ;Subtract F/N LIMIT value from VARIABLE value
           MVI	L, 306o               ;Set pointer to MSW of F/N STEP registers
           MOV	A,M                    ;Fetch this value into the ACC
           ANA	A                    ;Test to see if STEP value might be zero
           MVI	L, 126o               ;Load L with address of MSW of FPACC
           MOV	A,M                    ;Fetch this value into the ACC
           JZ	FORNXT             ;If STEP size was zero, then endless loop, an error condx
           JM	NEXT11             ;If STEP size less than zero make alternate test on limit
           ANA	A                    ;Test the contents of the MSW of the FPACC
           JM	NEXT12             ;Continue FORINEXT loop if current variable value is
           JZ	NEXT12             ;Less than or equal to the F/N LIMIT value
NEXT10:    MVI	L, 363o               ;If out of LIMIT range, load L with address of the AUX
           MVI	H,PG26 ;\HB\OLDPG26   ;** PGM LINE pointer. (Contains pointer to the NEXT
           MOV	E,M                    ;Statement line that initiated this routine.) Fetch the
           DCR	L                    ;Low part of the address into E, decrement the memory
           MOV	D,M                    ;And get the page part of the address into CPU register
           DCR	L                    ;Decrement memory pointer to the low portion of the
           MOV	M,E                    ;User pgm buffer line pointer (regular pointer) and set it
           DCR	L                    ;With the value from the AUX line pntr, decrement the
           MOV	M,D                    ;Pointer and do the same for the page portion
           MVI	L, 205o               ;Set L to address of FOR/NEXT STACK pointer
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of FOR/NEXT STACK pointer
           MOV	B,M                    ;Fetch and decrement the
           DCR	B                    ;FOR/NEXT STACK pointer value
           MOV	M,B                    ;To perform effective popping operation
           JMP	NXTLIN             ;Statement line after NEXT statement is done next
NEXT11:    ANA	A                    ;When F/N STEP is negative, reverse test so that if the
           JP	NEXT12             ;Variable value is greater than or equal to the F/N LIMIT
           JMP	NEXT10             ;The FOR/NEXT loop continues. Else it is finished.
NEXT12:    MVI	L, 314o               ;Load L with address of FIN TEMP storage registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to FIN TEMP storage registers page
           CALL	FLOAD              ;Transfer the updated variable value to the FPACC
           CALL	RESTSY             ;Restore the variable name and value
           CALL	STOSYM             ;In the VARIABLES table. Exit routine so that
           JMP	NXTLIN             ;Statement line after FOR statement is done next

;;; The label BACKSP SHOULD BE AT 31 217 198fh

BACKSP:    MVI	A, 215o               ;Load ASCII code for carriage-return into the ACC
           CALL	ECHO               ;Display the carriage-return
           CALL	ECHO               ;Repeat to provide extra time if TTY
           MVI	L, 043o               ;Load L with address of COLUMN COUNTER
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of COLUMN COUNTER
           MVI	M, 001                ;Set COLUMN COUNTER to first column
           MVI	L, 124o               ;Set L to address containing desired TAB position
           MOV	A,M                    ;Fetch the desired TAB position value
           ANA	A                    ;Test to see if it is
           RM                    ;Negative or zero
           RZ                    ;In which case return to caller
           JMP	TAB1               ;Else, proceed to perform the TAB operation.

	
;;; The label FOR5 SHOULD START AT 31 246 19a6h
	
FOR5:      MVI	L, 205o               ;Load L with address of the FOR/NEXT STACK pointer
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of the FOR/NEXT STACK pntr
           MOV	A,M                    ;Fetch the stack pointer to the ACC.
           RLC                    ;Rotate it left to multiply by two, then rotate it again to
           RLC                    ;Multiply by four. Add this value to the base address
           ADI	136o               ;Plus two of the base address to point to the next part of
           MOV	E,A                    ;The FOR/NEXT STACK. Place this value in register E.
           MOV	D,H                    ;Set D to the FORINEXT STACK area page.
           MVI	L, 145o               ;Load L with the address of the first character in the
           MVI	H,PG26 ;\HB\OLDPG26   ;** AUX SYMBOL BUFFER and set up H to this page.
           MVI	B, 002                ;Set up register B as a number of bytes to move counter.
           CALL	MOVEIT             ;Move the variable name into the FOR/NEXT STACK.
           CALL	STOSYM             ;Store initial variable value in the VARIABLES TABLE.
           JMP	NXTLIN             ;Continue with next line in user program buffer.


;;; The label PARSEP SHOULD START AT 31 300 19c0h
PARSEP:    MVI	L, 176o               ;Load L with PARSER TOKEN storage location. Set
           MVI	M, 000                ;The value indicating end of expression. Call the
           CALL	PARSER             ;PARSER subroutine for final time for the expression.
           MVI	L, 227o               ;Change L to point to the ARITH STACK pointer.
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to the page of the ARITH STACK pointer.
           MOV	A,M                    ;Fetch the ARITH STACK pointer value.
           CPI	230o               ;Should indicate only one value (answer) in stack.
           RZ                    ;Exit with answer in FPACC if ARITH STACK is O.K.
           JMP	SYNERR             ;Else have a syntax error!


;;; THERE IS SOME BLANK ADDRESSES HERE 317-NEXT PAGE

;	db	(1a00h-$) dup 0

           ORG	1a00h	; 032#000
SQRX:      MVI	L, 014o               ;Load L with address of FP TEMP registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of FP TEMP. Move contents of FPACC
           CALL	FSTORE             ;[Argument of SQR(X)] into FP TEMP for storage.
           MVI	L, 126o               ;Load L with MSW of FPACC
           MOV	A,M                    ;Fetch the MSW into the accumulator
           ANA	A                    ;Check the sign of the number in the FPACC
           JM	SQRERR             ;If number negative, cannot take square root
           JZ	CFALSE             ;If number is zero, return with zero value in FPACC
           MVI	L, 017o               ;Load L with address of FP TEMP Exponent register
           MOV	A,M                    ;Fetch the Exponent value into the ACC
           ANA	A                    ;Check sign of the Fxponent
           JM	NEGEXP             ;If Exponent less than zero, process negative Exponent
           RAR                    ;If Exponent positive, rotate right to divide by two
           MOV	B,A                    ;And save the result in CPU register B
           MVI	A, 000                ;Clear the accumulator without disturbing Carry bit
           RAL                    ;Rotate Carry bit into the ACC to save remainder
           MOV	M,A                    ;Store the remainder back in FP TEMP Exponent reg.
           JMP	SQREXP             ;Jump to continue processing
NEGEXP:    MOV	B,A                    ;For negative Exponent, form two Is complement by
           XRA	A                    ;Placing the positive value in CPU register B, clearing
           SUB	B                    ;The accumulator, and then subtracting B from the ACC
           ANA	A                    ;Clear the Carry bit after the complementing operation
           RAR                    ;Rotate the value right to divide by two
           MOV	B,A                    ;Save the result in CPU register B
           MVI	A, 000                ;Clear the accumulator without disturbing Carry bit
           ADC	A                    ;Add Carry bit to the accumulator as remainder
           MOV	M,A                    ;Store the remainder back in FP TEMP Exponent reg
           JZ	NOREMD             ;If remainder was zero skip ahead. If not, increment the
           INR	B                    ;Result of the divide by two ops to compen for negative
NOREMD:    XRA	A                    ;Clear the accumulator
           SUB	B                    ;Subtract the quotient of the divide by two op to
           MOV	B,A                    ;Form two's complement and save the result in register B
SQREXP:    MVI	L, 013o               ;Load L with address of TEMP register
           MOV	M,B                    ;Store Fxponent quotient from above ops in TEMP
           MVI	L, 004                ;Load L with address of FP registers containing +1.0
           MVI	E, 034o               ;Load E with address of SQR APPROX working registers
           MOV	D,H                    ;Set D to same page as H
           MVI	B, 004                ;Set up register B as a number of bytes to move counter
           CALL	MOVEIT             ;Transfer value +1.0 into SQR APPROX registers
           CALL	CFALSE             ;Now clear the FPACC registers
           MVI	L, 044o               ;Load L with address of LAST SQR APPROX temp regs.
           CALL	FSTORE             ;Initialize the LAST SQR APPROX regs to value of zero
SQRLOP:    MVI	L, 034o               ;Load L with address of SQR APPROX working registers
           CALL	FLOAD              ;Transfer SQR APPROX into the FPACC
           MVI	L, 014o               ;Load L with address of SQR ARG storage registers
           CALL	OPLOAD             ;Transfer SQR ARG into the FPOP
           CALL	FPDIV              ;Divde SQR ARG by SQR APPROX (Fon-n X/A)
           MVI	L, 034o               ;Load L with address of SQR APPROX registers
           CALL	OPLOAD             ;Transfer SQR APPROX into the FPOP
           CALL	FPADD              ;Add to form value (X/A + A)
           MVI	L, 127o               ;Load L with address of FPACC Exponent register
           MOV	B,M                    ;Fetch Exponent value into CPU register B
           DCR	B                    ;Subtract one to effectively divide FPACC by two
           MOV	M,B                    ;Restore to memory. (Now have ((X/A + A) /2)
           MVI	L, 034o               ;Load L with address of SQR APPROX registers
           CALL	FSTORE             ;Store contents of FPACC as new SQR APPROX
           MVI	L, 044o               ;Load L with address of LAST SQR APPROX registers
           CALL	OPLOAD             ;Transfer LAST SQR APPROX into the FPOP
           CALL	FPSUB              ;Subtract (LAST SQR APPROX - SQR APPROX)
           MVI	L, 127o               ;Load L with address of FPACC Exponent
           MOV	A,M                    ;Fetch the Exponent into the accumulator
           CPI	367o                ;See if difference less than 2 to the minus ninth
;;; The below is changed for PATCH 2
;;; following is the original code
;;;           JTS SQRCNV             ;If so, approximation has converged
;;; Now is the new line
	   JMP	PATCH2
;;;;           DCR L
;;;;           MOV A,M
;;;;           ANA A
;;;;           JZ SQRCNV             ;THIS IS PATCH #2
SQR1:	   MVI	L, 034o               ;Else, load L with address of SQR APPROX
           MOV	D,H                    ;Set D to same page as H
           MVI	E, 044o               ;And E with address of LAST SQR APPROX
           MVI	B, 004o               ;Set up register B as a number of bytes to move counter
           CALL	MOVEIT             ;Transfer SQR APPROX into LAST SQR APPROX
           JMP	SQRLOP             ;Continue ops until approximation converges
SQRCNV:    MVI	L, 013o               ;Load L with address of TEMP register. Fetch the
           MOV	A,M                    ;Exponenent quotient store there into accumulator.
           MVI	L, 037o               ;Change L to point to SQR APPROX exponent.
           ADD	M                    ;Add SQR APPROX exponent to quotient value.
           MOV	M,A                    ;Store sum back in SQR APPROX Exponent register.
           MVI	L, 034o               ;Load L with address of SQR APPROX. Transfer the
           JMP	FLOAD              ;SQR APPROX into FPACC as answer and exit.
SQRERR:    MVI	A, 323o               ;Load ASCII code for letter S into the accumulator.
           MVI	C, 321o               ;Load ASCII code for letter Q into CPU register C.
           JMP	ERROR              ;Display the SQuare root (SQ) error message.
;;; above instruction starts at 223
;;; some blank addresses available here.

;	db	(1aa0h-$) dup 0

           ORG	1aa0h              ; 032#240
RNDX:      MVI	L, 064o               ;Load L with address of SEED storage registers
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page for floating point working registers
           CALL	FLOAD              ;Transfer SEED into the FPACC
           MVI	L, 050o               ;Load L with address of random constant A
           CALL	OPLOAD             ;Transfer random constant A into the FPOP
           CALL	FPMULT             ;Multiply to form (SEED * A)
           MVI	L, 060o               ;Load L with address of random constant C
           CALL	OPLOAD             ;Transfer random constant C into the FPOP
           CALL	FPADD              ;Add to fom (SEED * A) + C
           MVI	L, 064o               ;Load L with address of SEED storage registers
           CALL	FSTORE             ;Store I (SEED * A) + C] in former SEED registers
           MVI	L, 127o               ;Load L with address of FPACC Exponent register
           MOV	A,M                    ;Fetch Exponent value into the accumulator
           SUI	020o               ;Subtract 16 (decimal) to effectively divide by 65,536
           MOV	M,A                    ;Now FPACC = [((SEED * A) + C)/65,536]
           CALL	FPFIX              ;Convert floating to fixed point to obtain integer part
           MVI	L, 123o               ;Load L with address of FPACC Extension register
           MVI	M, 000                ;Clear the FPACC Extension register
           MVI	L, 127o               ;Load L with address of FPACC Exponent
           MVI	M, 000                ;Clear the FPACC Exponent register
           CALL	FPFLT              ;Fetch INT(((SEED * A) + C)/65,536) into the FPACC
           MVI	L, 127o               ;Load L with address of FPACC Exponent
           MOV	A,M                    ;Fetch FPACC Exponent into the accumulator
           ADI	020o               ;Add 16 (decimal) to effectively multiply by 65,536
           MOV	M,A                    ;(65,536 * INT[ ((SEED * A) + C)/65,5361) in FPACC
           MVI	L, 064o               ;Load L with address of [(SEED * A) + C]
           CALL	OPLOAD             ;Transfer it into FPOP. Subtract FPACC to form
           CALL	FPSUB              ;[(SEED * A) + C] MOD 65,536
           MVI	L, 064o               ;Load L with address of former SEED registers
           CALL	FSTORE             ;Store SEED MOD 65,536 in place of [(SEED * A) + Cl
           MVI	L, 127o               ;Load L with address of FPACC Exponent
           MOV	A,M                    ;Fetch FPACC Exponent into the ACC and subtract
           SUI	020o               ;16 (decimal) to form (SEED MOD 65,536)/65,536
           MOV	M,A                    ;So that random number in FPACC is between
           RET                    ;0.0 and +1.0 and exit to calling routine
;;; THE ABOVE RETURN SHOULD BE 32 351 1ae9h


;;; NOTE OPEN ADDRESSES TO END OF PAGE 32

;;; following is PATCH 2

;	db	(1af4h-$) dup 0

	ORG	1af4h	; 032#364
PATCH2	JM	SQRCNV
	DCR	L
	MOV	A,M
	ANA	A
	JZ	SQRCNV
	JMP	SQR1
;;; The above jump should start at 32 375 1afdh
	

	;; PAGES 33 TO REMAINDER OF MEMORY
	;; OR START OF OPTIONAL ARRAY HANDLING
	;; ROUTINES USED AS USER PROGRAM BUFFER



	;; OPTIONAL ARRAY ROUTINES ASSEMBLED FOR OPERATION
	;; IN THE UPPER 3 PAGES OF A 12K SYSTEM ARE LISTED HERE.

;	db	(2d00h-$) dup 0

           ORG 2d00h              ; 055#000

PRIGH1:    MVI	L, 126o               ;Load L with address of the MSW in the FPACC
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of FPACC
           MOV	A,M                    ;Fetch MSW of FPACC into the ACC.
           ANA	A                    ;Test to see if value in FPACC is positive.
           JM	OUTRNG             ;If not, go display error message.
           CALL	FPFIX              ;If O.K. then convert floating point to fixed point
           MVI	L, 124o               ;Load L with address of LSAL of converted value
           MOV	A,M                    ;Fetch the LSW of the value into the ACC
           SUI	001                ;Subtract one from the value to establish proper
           RLC                    ;Origin for future ops. Now rotate the value twice
           RLC                    ;To effectively multiply by four. Save the
           MOV	C,A                    ;Calculated result in CPU register C
           MVI	L, 203o               ;Load L with address of F/A STACK TEMP
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of F/A STACK TEMP
           MOV	A,M                    ;Fetch the value into the accumulator
           XRI	377o               ;Complement the value
           RLC                    ;Rotate the value twice to multiply by four (the number
           RLC                    ;Of bytes per entry in the ARRAY VARIABLES table).
           ADI	120o               ;Add the starting address of the ARRAY VARIABLES
           MVI	H,PG27 ;\HB\OLDPG27   ;** TABLE to forin pointer. Set page address in H.
           MOV	L,A                    ;Point to the name in the ARRAY VARIABLES
           INR	L                    ;Increment the pointer value twice to move over the
           INR	L                    ;Name in the table and point to starting address for the
           MOV	A,M                    ;Array values in the ARRAY VALUES table. Fetch this
           ADD	C                    ;Address to the ACC. Now add in the figure calculated
           MOV	L,A                    ;To reach desired subscripted data storage location. Set
           MVI	H,PG57 ;\HB\OLDPG57   ;tt The pointer to that location. Load the floating point
           JMP	FLOAD              ;Value stored there into the FPACC and exit to caller.


;;; The label FUNAR2 SHOULD START AT 55-054 2d2ch
FUNAR2:    MVI	L, 202o               ;Load L with address of TEMP COUNTER
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of counter
           MOV	B,M                    ;Fetch the counter value
           INR	B                    ;Increment the value
           MOV	M,B                    ;Restore the value to memory
           MVI	C, 002                ;Initialize register C to a value of two for future ops
           MVI	L, 114o               ;Load L with address of start of ARRAY VARIABLES
           MVI	H,PG27 ;\HB\OLDPG27   ;** TABLE (less four). Set H to page of the table.
           CALL	TABADR             ;Calculate address of start of next narne in table.
           MVI	D,PG26 ;\HB\OLDPG26   ;** Load D with page of the SYMBOL BUFFER
           MVI	E, 120o               ;Set E to starting address of the SYMBOL BUFFER
           CALL	STRCP              ;Compare name in ARRAY VARIABLES table to the
           JZ	FUNAR3             ;Contents of the SYMBOL BUFFER. If match, go set up
           MVI	L, 202o               ;Array token value. Else, reset L to address of TEMP
           MVI	H,PG27 ;\HB\OLDPG27   ;** COUNTER. Set H to page of TEMP COUNTER.
           MOV	A,M                    ;Fetch the counter value into the accumulator.
           MVI	L, 075o               ;Change L to number of arrays storage location.
           CMP	M                    ;Compare number of entries checked against number
           JNZ	FUNAR2             ;Possible. Keep searching table if not finished.
           JMP	FAERR              ;If finished and no match than have F/A error condx.
FUNAR3:    MVI	L, 202o               ;Load L with address of TEMP COUNTER
           MVI	H,PG27 ;\HB\OLDPG27   ;** Load H with page of counter.
           XRA	A                    ;Clear the accumulator. Subtract the value in the TEMP
           SBB	M                    ;COUNTER from zero to obtain two's complement.
           MOV	M,A                    ;Place this back in counter location as ARRAY TOKEN
           JMP	FUNAR4             ;VALUE (negative). Go place the value on F/A STACK.


;;; The label OUTRNG STARTS AT 55 136 2d5eh
OUTRNG:    MVI	A, 317o               ;Load the ASCII code for letter 0 into the accumulator
           MVI	C, 322o               ;Load the ASCII code for letter R into register C
           JMP	ERROR              ;Go display Out of Range (OR) error message.




ARRAY:     CALL	RESTSY             ;Transfer contents of AUX SYMBOL BUFFER into the
           JMP	ARRAY2             ;SYMBOL BUFFER. (Entry when have actual LET)
ARRAY1:    MVI	L, 202o               ;Load L with address of SCAN pointer
           JMP	ARRAY3             ;Proceed to process. (Entry point for IMPLIED LET)
ARRAY2:    MVI	L, 203o               ;Load L with address of LET pointer
ARRAY3:    MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to pointer page
           MOV	B,M                    ;Fetch pointer to location where "(" found in statement
           INR	B                    ;Line. Increment it to point to next character in the line.
           MVI	L, 276o               ;Load L with address of EVAL pointer and load it with
           MOV	M,B                    ;The starting address for the EVAL routine
           MVI	L, 206o               ;Change L to address of ARRAY SETUP pointer
           MOV	M,B                    ;And also store address in that location
ARRAY4:    MVI	L, 206o               ;Load L with address of ARRAY SETUP pointer
           CALL	GETCHR             ;Fetch character pointed to by ARRAY SETUP pntr
           CPI	251o               ;See if character is ")" ? If so, then have located
           JZ	ARRAY5             ;End of the subscript. If not, reset
           MVI	L, 206o               ;to the ARRAY SETUP pointer. Increment the
           CALL	LOOP               ;Pointer and test for the end of the statement line.
           JNZ	ARRAY4             ;If not end of line, continue looking for right paren.
           MVI	A, 301o               ;If reach end of line before right parenthesis than load
           MVI	C, 306o               ;ASCII code for letters A and F and display message
           JMP	ERROR              ;Indicating Array Forrnat (AF) error condition
ARRAY5:    MVI	L, 206o               ;Load L with address of ARRAY SETUP pointer
           MOV	B,M                    ;Fetch pointer (pointing to ")"sign) into register B
           DCR	B                    ;Decrement it to move back to end of subscript number
           MVI	L, 277o               ;Load L with address of EVAL FINISH pointer location
           MOV	M,B                    ;Place the pointer value in the EVAL FINISH pointer
           MVI	L, 207o               ;Load L with address of LOOP COUNTER
           MVI	M, 000                ;Initialize LOOP COUNTER to value of zero
ARRAY6:    MVI	L, 207o               ;Load L with address of LOOP COUNTER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of LOOP COUNTER
           MOV	B,M                    ;Fetch the counter value
           INR	B                    ;Increment it
           MOV	M,B                    ;Restore the counter value to memory
           MVI	C, 002                ;Set up counter in register C for future ops
           MVI	L, 114o               ;Load L with address of start of ARRAY VARIABLES
           MVI	H,PG27 ;\HB\OLDPG27   ;** Table less four). Set H to page of the table.
           CALL	TABADR             ;Calculate the address of next entry in the table
           MVI	E, 120o               ;Load register E with starting address of SYMBOL BUFF
           MVI	D,PG26 ;\HB\OLDPG26   ;** Set D to page of SYMBOL BUFFER
           CALL	STRCP              ;Compare entry in table against contents of SYMBOL BF
           JZ	ARRAY7             ;If match, have found array naine in the table.
           MVI	L, 207o               ;Else, set L to address of the LOOP COUNTER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the LOOP COUNTER
           MOV	A,M                    ;Fetch the counter value to the ACC
           MVI	L, 075o               ;Change L to the counter containing number of arrays
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to the proper page
           CMP	M                    ;Compare number of arrays to count in LOOP CNTR
           JNZ	ARRAY6             ;If more entries in the table, continue looking for match
           JMP	FAERR              ;If no matching name in table then have an error condx.
ARRAY7:    CALL	EVAL               ;Call subroutine to evaluate subscript expression
           CALL	FPFIX              ;Convert the subscript value obtained to fixed forrnat
           MVI	L, 207o               ;Load L with address of LOOP COUNTER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of the LOOP COUNTER
           MOV	B,M                    ;Fetch the value in the LOOP COUNTER into the ACC
           MVI	C, 002                ;Set up counter in register C future ops
           MVI	L, 114o               ;Load L with address of ARRAY VARIABLES
           MVI	H,PG27 ;\HB\OLDPG27   ;** Table less four). Set H to page of the table.
           CALL	TABADR             ;Calculate the address of entry in the table
           INR	L                    ;Advance the ARRAY VARIABLES table pointer twice
           INR	L                    ;To advance pointer over array name.
           MOV	C,M                    ;Fetch array base address in ARRAY VALUES table
           MVI	L, 124o               ;Load L with address of subscript value
           MVI	H,PG01 ;\HB\OLDPG1    ;** Set H to page of subscript value
           MOV	A,M                    ;Fetch the subscript value into the accumulator
           SUI	001                ;Subtract one from subscript value to allow for zero
           RLC                    ;Origin. Now multiply by four
           RLC                    ;Using rotates (number of bytes required for each entry
           ADD	C                    ;In the ARRAY VALUES table). Add in base address to
           MVI	L, 204o               ;The calculated value to form final address in the
           MVI	H,PG27 ;\HB\OLDPG27   ;** ARRAY VALUES table. Now set H & L to TEMP
           MOV	M,A                    ;ARRAY ELEMENT storage location & store the addr.
           MVI	L, 201o               ;Change L to point to ARRAY FLAG
           MVI	M, 377o               ;Set the ARRAY FLAG for future use
           RET                    ;Exit to calling routine


;;; The label DIM SHOULD START AT 55 365 2df5h
DIM:       CALL	CLESYM             ;Initialize the SYMBOL BUFFER to cleared condition
           MVI	L, 202o               ;Load L with address of SCAN pointer
           MOV	B,M                    ;Fetch SCAN pointer value into register B
           INR	B                    ;Add one to the SCAN pointer value
           MVI	L, 203o               ;Change L to DIM pointer (formerly TOKEN) storage
           MOV	M,B                    ;Store the updated SCAN pointer as the DIM pointer
DIM1:      MVI	L, 203o               ;Load L with the address of DIM pointer storage location
           CALL	GETCHR             ;Fetch a character from the line input buffer
           JZ	DIM2               ;If character fetched is a space, ignore it
           CPI	250o               ;Else see if character is "(" left parenthesis
           JZ	DIM3               ;If so, should have ARRAY VARIABLE naine in buffer
           CALL	CONCTS             ;If not, append the character to the SYMBOL BUFFER
DIM2:      MVI	L, 203o               ;Load L with the address of DIM pointer stomge location
           CALL	LOOP               ;Increment the pointer and see if end of line
           JNZ	DIM1               ;If not end of line, fetch next character
           JMP	DIMERR             ;Else have a DIMension error condition
DIM3:      MVI	L, 206o               ;Load L with address of ARRAY pointer storage loc
           MVI	M, 000                ;Initialize ARRAY pointer to starting value of zero
DIM4:      MVI	L, 206o               ;Load L with address of ARRAY pointer storage loc
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of ARRAY pointer storage location
           MOV	A,M                    ;Fetch value in ARRAY pointer to ACC (effectively
           RLC                    ;Represents number of arrays defined in pgm). Rotate
           RLC                    ;Left twice to multiply by four (niunber of bytes per
           ADI	114o               ;entry in ARRAY VARIABLES table). Add to base
           MVI	H,PG27 ;\HB\OLDPG27   ;** Address to form pointer to ARRAY VARIA.BLES
           MOV	L,A                    ;Table and set up H & L as the memory pointer.
           MVI	E, 120o               ;Load E with starting address of the SYMBOL BUFFER
           MVI	D,PG26 ;\HB\OLDPG26   ;** Load D with the page address of the SYMBOL BUFF
           CALL	STRCP              ;Compare contents of SYMBOL BF to entry in ARRAY
           JZ	DIM9               ;VARIABLES table. If same, have duplicate array name.
           MVI	L, 206o               ;Else, load L with address of ARRAY pointer storage
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of ARRAY pointer storage
           MOV	B,M                    ;Fetch the ARRAY pointer value to register B
           INR	B                    ;Increment the value
           MOV	M,B                    ;Restore it to ARRAY pointer storage location
           MVI	L, 075o               ;Change L to number of arrays storage location
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to page of the number of arrays stomge loc
           MOV	A,M                    ;Fetch the number of arrays value to the ACC
           DCR	B                    ;Restore B to previous count
           CMP	B                    ;Compare number of arrays tested against nr defined
           JNZ	DIM4               ;If not equal, continue searching ARRAY VARIABLES
           MVI	L, 075o               ;Table. When table searched with no match, then must
           MVI	H,PG27 ;\HB\OLDPG27   ;** Append naine to table. First set pointer to number
           MOV	B,M                    ;Of arrays storage location. Fetch that value and
           INR	B                    ;Add one to account for new name being added.
           MOV	M,B                    ;Restore the updated value back to memory.
           MVI	L, 076o               ;Change pointer to ARRAY TEMP pointer storage
           MOV	M,B                    ;Store pointer to current array in ARRAY TEMP too.
           MVI	L, 206o               ;Load L with address of ARRAY pointer stomge loc.
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of ARRAY pointer storage location
           MOV	M,B                    ;And update it also for new array being added.
           MOV	A,M                    ;Fetch the current ARRAY pointer value to the ACC
           RLC                    ;Multiply it times four by performing two rotate left
           RLC                    ;Operations and add it to base value to form address in
           ADI	114o               ;The ARRAY VARIABLES table. Place the low part
           MOV	E,A                    ;Of this calculated address value into register E.
           MVI	D,PG27 ;\HB\OLDPG27   ;** Set register D to the page of the table.
           MVI	L, 120o               ;Load L with the start of the SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with the page of the SYMBOL BUFFER
           CALL	MOVEC              ;Move the array name from the SYMBOL BUFFER to
           CALL	CLESYM             ;The ARRAY VARIABLES table. Then clear the
           MVI	L, 203o               ;SYMBOL BUFFER. Reset L to the DIM pointer storage
           MVI	H,PG26 ;\HB\OLDPG26   ;** Location. Set H to the DIM pointer page.
           MOV	B,M                    ;Fetch the pointer value (points to "(" part of DIM
           INR	B                    ;Statement). Increment the pointer to next character in
           MVI	L, 204o               ;The line input buffer. Cbange L to DIMEN pointer.
           MOV	M,B                    ;Store the updated DIM pointer in DIMEN storage loc.
DIM5:      MVI	L, 204o               ;Set L to DIMEN pointer storage location
           CALL	GETCHR             ;Fetch character in line input buffer
           JZ	DIM6               ;Ignore character for space
           CPI	251o               ;If not space, see if character is right parenthesis
           JZ	DIM7               ;If yes, process DIMension size (array length)
           CPI	260o               ;If not, see if character is a valid decimal number
           JM	DIMERR             ;If not valid number, have DIMension error condition
           CPI	272o               ;Continue testing for valid decitnal number
           JP	DIMERR             ;If not valid number, then DIMension error condition
           CALL	CONCTS             ;If valid decirnal number, append digit to SYMBOL BF
DIM6:      MVI	L, 204o               ;Set L to DIMEN pointer storage location
           CALL	LOOP               ;Advance the pointer value and check for end of the line
           JNZ	DIM5               ;If not end of line, continue fetching DIMension size
           JMP	DIMERR             ;If end of line before right parenthesis, have error condx.
DIM7:      MVI	L, 120o               ;Load L with address of start of SYMBOL BUFFER
           MVI	H,PG26 ;\HB\OLDPG26   ;** Load H with page of SYMBOL BUFFER. (Now
           CALL	DINPUT             ;Contains DIMension size.) Convert buffer to floating
           CALL	FPFIX              ;Point number and then reformat to fixed point.
           MVI	L, 124o               ;Load L with address of LSW of fixed point number
           MOV	A,M                    ; And fetch the low order byte of the nr into the ACC
           RLC                    ;Rotate it left two tirnes to multiply it by four (the
           RLC                    ;Number of bytes required to store a floating point nr).
           MOV	C,A                    ;Store this value in CPU register C temporarily
           MVI	L, 076o               ;Set L to ARRAY TEMP storage location.
           MVI	H,PG27 ;\HB\OLDPG27   ;** Set H to ARRAY TEMP pointer page.
           MOV	A,M                    ;Fetch the value in ARRAY TEMP (points to ARRAY
           SUI	001                ;VARIABLES table). Subtract one from the pointer
           RLC                    ;Value and multiply the result by four using rotate left
           RLC                    ;Instructions. Add this value to a base address
           ADI	122o               ;(Augmented by two) to point to ARRAY VALUES
           MOV	L,A                    ;Pointer storage location in the ARRAY VARIABLES
           MVI	H,PG27 ;\HB\OLDPG27   ;Table and set the pointer up in registers H & L.
           MOV	B,M                    ;Fetch the starting address in the ARRAY VALUES
           ADI	004                ;Table for the previous array into register B. Now add
           MOV	L,A                    ;Four to the ARRAY VARIABLES table pointer to
           MOV	A,B                    ;Point to curront ARRAY VALUES starting address.
           ADD	C                    ;Add the previous array starting address plus number of
           MOV	M,A                    ;Bytes required and store as starting loc for next array
DIM8:      MVI	L, 204o               ;Set L to address of DIMEN pointer storage location
           MVI	H,PG26 ;\HB\OLDPG26   ;** Set H to page of DIMEN pointer
           MOV	B,M                    ;Fetch pointer value (points to ") " in line)
           MVI	L, 203o               ;Change L to DIM pointer storage location
           MOV	M,B                    ;Store former DIMEN value back in DIM pointer
DIM9:      MVI	L, 203o               ;Load L with address of DIM pointer storage location
           CALL	GETCHR             ;Fetch a character from the line input buffer
           CPI	254o               ;See if character is a comma (,) sign
           JZ	DIM10              ;If yes, have another array being defined on the line
           MVI	L, 203o               ;If not, reset L to the DIM pointer
           CALL	LOOP               ;Increment the pointer and see if end of the line
           JNZ	DIM9               ;If not end of the line, keep looking for a comma
           JMP	NXTLIN             ;Else exit the DIM statement routine to continue pgm
DIM10:     MVI	L, 203o               ;Set L to DIM pointer storage location
           MOV	B,M                    ;Fetch pointer value (points to comma sign just found)
           MVI	L, 202o               ;Load L with address of SCAN pointer storage location
           MOV	M,B                    ;Place DIM pointer into the-SCAN pointer
           JMP	DIM                ;Continue processing DIM statement line for next array
DIMERR:    MVI	A, 304o               ;On error condition, load ASCII code for letter D in ACC
           MVI	C, 305o               ;And ASCII code for letter E in CPU register C
           JMP	ERROR              ;Go display the Dirnension Error (DE) message.

;##################################################################################################

IN_STATUS	EQU 	00
IN_DATA		EQU	01
OUT_DATA	EQU 	10H

		ORG	3000H

OPN:		DB	OPN_MSG1 - OPN_MSG
OPN_MSG:	DB	"Faster SCELBAL (2012)",0dh,0ah
		DB	"IO and start up routine by Gazelle 2025",0dh,0ah
OPN_MSG1:	DB	0

START3000:
 IF FOR8080
		LXI	SP,4000H
		LHLD	1
		LXI	B,3
		DAD	B
		SHLD	CIN_ST+1
		DAD	B
		SHLD	CIN_IN+1
		DAD	B
		SHLD	CIN_OUT+1
		SHLD	CP_1+1
		SHLD	CP_2+1
		SHLD	CP_3+1
 ENDIF
		MVI	H,30H
		MVI	L,00H
		CALL	TEXTC
		MVI	H,1
		XRA	A
		MOV	L,A
		MOV	M,A
		INR	L
		MOV	M,A
		INR	L
		MOV	M,A
		JMP	ENTRY_SCR
;---------------------------------------------------------------------
;;; no user defined functions yet, stop here if we see one.
UDEFX:		RET

;##################################################################################################
;
;		HARDWARE DEPENDENT ROUTINE
;
 IF		FOR8008
SAVE:
LOAD:		OUT	1FH	; Exit from emulator.
 ENDIF

 IF		FOR8080
SAVE:
LOAD:		JMP	0	; Exit from emulator.(reboot CP/M)
 ENDIF

;;; HERE IS THE USER DEFINED CHARACTER INPUT TO READ FROM SERIAL PORT

 IF		FOR8008

CINP:		IN	IN_DATA
		CPI	08H
		JZ	CINP_BS
		OUT	OUT_DATA
		ORI	80H
		RET

CINP_BS:	MVI	A,0FFH
		RET
 ENDIF

 IF		FOR8080
CINP:
		PUSH	B
		PUSH	D
		PUSH	H
CINP0:
CIN_ST:		CALL	0FA06H
		ANA	A
		JZ	CINP0
CIN_IN:		CALL	0FA09H
		CPI	08H
		JZ	CINP_BS
		PUSH	PSW
		MOV	C,A
CIN_OUT		CALL	0FA0CH
		POP	PSW
		ORI	80H
		POP	H
		POP	D
		POP	B
		RET

CINP_BS:	MVI	A,0FFH
		POP	H
		POP	D
		POP	B
		RET
 ENDIF
;---------------------------------------------------------------------
;;; HERE IS THE USER DEFINED PRINT ROUTINE FOR A SERIAL PORT

 IF		FOR8008
CPRINT:
		CPI	0DCH
		JZ	CP_BS
		ANI	7Fh
		CPI	0DH
		JZ	CP_OK
		CPI	0AH
		JZ	CP_OK
		CPI	20H
		JC	CP_NO
CP_OK:		OUT	OUT_DATA
CP_NO:		RET

CP_BS:		MVI	A,08H
		OUT	OUT_DATA
		MVI	A,20H
		OUT	OUT_DATA
		MVI	A,08H
		OUT	OUT_DATA
		RET
 ENDIF

 IF		FOR8080
CPRINT:
		PUSH	PSW
		PUSH	B
		PUSH	D
		PUSH	H
		CPI	0DCH
		JZ	CP_BS
		ANI	7Fh
		CPI	0DH
		JZ	CP_OK
		CPI	0AH
		JZ	CP_OK
		CPI	20H
		JC	CP_NO
CP_OK:		MOV	C,A
CP_1:		CALL	0FA0CH
CP_NO:		POP	H
		POP	D
		POP	B
		POP	PSW
		RET

CP_BS:		MVI	C,08H
CP_2:		CALL	0FA0CH
		MVI	C,20H
CP_3:		CALL	0FA0CH
		MVI	C,08H
		JMP	CP_1

 ENDIF
;------------------------------------------------------------------------
