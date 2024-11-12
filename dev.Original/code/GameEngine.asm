; -------------------
; METROID source code
; -------------------
; MAIN PROGRAMMERS
;     HAI YUKAMI
;   ZARU SOBAJIMA
;    GPZ SENGOKU
;    N.SHIOTANI
;     M.HOUDAI
; (C) 1986 NINTENDO
;
; Disassembled, reconstructed and commented
; by SnowBro [Kent Hansen] <kentmhan@online.no>
; Continued by Dirty McDingus (nmikstas@yahoo.com)
; Can be reassembled using Ophis.
; A work in progress.
;Last updated: 3/9/2010

;Game engine (memory page 7)

.org $C000

.require "Defines/Defines.asm"

;-------------------------------------[ Forward declarations ]--------------------------------------

.require "Defines/ForwardDeclarations.asm"

;----------------------------------------[ Start of code ]------------------------------------------

;This routine generates pseudo random numbers and updates those numbers
;every frame. The random numbers are used for several purposes including
;password scrambling and determinig what items, if any, an enemy leaves
;behind after it is killed.

RandomNumbers:
	txa						;put X in the stack	
	pha						;
	ldx #$05				;load 5 into x
*	lda RandomNumber1		;load random number 1
	clc						;
	adc #$05				;add 5 to the random number 1 (adds 25 in total per call)
	sta RandomNumber1		;
	lda RandomNumber2		;load random number 2 			
	clc						;
	adc #$13				;add 19 to random number 2 (adds 95 in total per call)
	sta RandomNumber2		;store in random number 2
	dex						;dec x, 
	bne -					;loop until x = 0
	pla						;restore X from the stack
	tax						;
	lda RandomNumber1		;leave with random number 1 in the accumulator
	rts						;

;------------------------------------------[ Startup ]----------------------------------------------

Startup:
	lda #$00				;
	sta MMC1Reg1 			;Clear bit 0. MMC1 is serial controlled
	sta MMC1Reg1			;Clear bit 1
	sta MMC1Reg1			;Clear bit 2
	sta MMC1Reg1			;Clear bit 3
	sta MMC1Reg1			;Clear bit 4 
	sta MMC1Reg2			;Clear bit 0
	sta MMC1Reg2			;Clear bit 1
	sta MMC1Reg2			;Clear bit 2
	sta MMC1Reg2			;Clear bit 3
	sta MMC1Reg2			;Clear bit 4 
	jsr MMCWriteReg3		;Swap to PRG bank #0 at $8000
	dex						;X = $FF
	txs						;S points to end of stack page

;Clear RAM at $000-$7FF.
ClearRamTo7FF:
	ldy #$07				;High byte of start address.
	sty $01					;
	ldy #$00      			;Low byte of start address.
	sty $00       			;$0000 = #$0700
	tya	    				;A = 0
*	sta ($00),y    			;clear address
	iny						;
	bne -	  				;Repeat for entire page.
	dec $01       			;Decrement high byte of address.
	bmi +	 				;If $01 < 0, all pages are cleared.
	ldx $01					;
	cpx #$01				;Keep looping until ram is cleared.
	bne -					;

;Clear cartridge RAM at $6000-$7FFF.
ClearSRAM:
*	ldy #$7F				;High byte of start address.
	sty $01					;
	ldy #$00				;Low byte of start address.
	sty $00					;$0000 points to $7F00
	tya	   					;A = 0
*	sta ($00),y				;
	iny						;Clears 256 bytes of memory before decrementing to next-->
	bne -					;256 bytes.
	dec $01					;
	ldx $01					;Is address < $6000?-->
	cpx #$60     			;If not, do another page.
	bcs -      				; 

	lda #%00001110			;Verticle mirroring.
							;H/V mirroring (As opposed to one-screen mirroring).
							;Switch low PRGROM area during a page switch.
							;16KB PRGROM switching enabled.
							;8KB CHRROM switching enabled.
	sta MMCReg0Cntrl		;
	
	lda #$00				;Clear bits 3 and 4 of MMC1 register 3.
	sta SwitchUpperBits		;

	ldy #$00				;
	sty ScrollX				;ScrollX = 0
	sty ScrollY     		;ScrollY = 0
	sty PPUScroll			;Clear hardware scroll x
	sty PPUScroll			;Clear hardware scroll y
	iny						;Y = #$01
	sty GameMode			;Title screen mode
	jsr ClearNameTables		;
	jsr EraseAllSprites		;

	lda #%10010000			;NMI = enabled
							;Sprite size = 8x8
							;BG pattern table address = $1000
							;SPR pattern table address = $0000
							;PPU address increment = 1
							;Name table address = $2000
	sta PPUControl0			;
	sta PPUCNT0ZP			;

	lda #%00000010			;Sprites visible = no
							;Background visible = no
							;Sprite clipping = yes
							;Background clipping = no
							;Display type = color
	sta PPUCNT1ZP			;

	lda #$47				;
	sta MirrorCntrl			;Prepare to set PPU to vertical mirroring.
	jsr PrepVertMirror		;

	lda #$00				;
	sta DMCCntrl1			;PCM volume = 0 - disables DMC channel
	lda #$0F				;
	sta APUCommonCntrl0		;Enable sound channel 0,1,2,3

	ldy #$00				;
	sty TitleRoutine		;Set title routine and and main routine function-->
	sty MainRoutine			;pointers equal to 0.
	lda #$11				;
	sta RandomNumber1		;Initialize RandomNumber1 to #$11
	lda #$FF				;
	sta RandomNumber2		;Initialize RandomNumber2 to #$FF

	iny						;Y = 1
	sty SwitchPending		;Prepare to switch page 0 into lower PRGROM.
	jsr CheckSwitch			;
	bne WaitNMIEnd			;Branch always

;-----------------------------------------[ Main loop ]----------------------------------------------

;The main loop runs all the routines that take place outside of the NMI.

MainLoop:
	jsr CheckSwitch			;Check to see if memory page needs to be switched.
	jsr UpdateTimer			;Update Timers 1, 2 and 3.
	jsr GoMainRoutine		;Go to main routine for updating game.
	inc FrameCount			;Increment frame counter.
	lda #$00				;
	sta NMIStatus			;Wait for next NMI to end.

WaitNMIEnd:
	tay						;
	lda NMIStatus			;
	bne +					;If nonzero, NMI has ended. Else keep waiting.
	jmp WaitNMIEnd			;

*	jsr RandomNumbers		;($C000)Update pseudo random numbers.
	jmp MainLoop			;($C0BC)Jump to top of subroutine.

;-------------------------------------[ Non-Maskable Interrupt ]-------------------------------------

;The NMI is called 60 times a second by the VBlank signal from the PPU. When the
;NMI routine is called, the game should already be waiting for it in the main 
;loop routine in the WaitNMIEnd loop.  It is possible that the main loop routine
;will not be waiting as it is bogged down with excess calculations. This causes
;the game to slow down.

NMI:
	php						;Save processor status, A, X and Y on stack.
	pha						;Save A.
	txa						;
	pha						;Save X.
	tya						;
	pha						;Save Y.
	lda #$00				;
	sta SPRAddress			;Sprite RAM address = 0.
	lda #$02				;
	sta SPRDMAReg			;Transfer page 2 ($200-$2FF) to Sprite RAM.
	lda NMIStatus			;
	bne ++					;Skip if the frame couldn't finish in time.
	lda GameMode			;
	beq +					;Branch if mode=Play.
	jsr NMIScreenWrite		;Write end message on screen(If appropriate).
*	jsr CheckPalWrite		;Check if palette data pending.
	jsr CheckPPUWrite		;check if data needs to be written to PPU.
	jsr WritePPUCtrl		;Update $2000 & $2001.
	jsr WriteScroll			;Update h/v scroll reg.
	jsr ReadJoyPads			;Read both joypads.
* 	jsr SoundEngine			;Update music and SFX.
	jsr UpdateAge			;Update Samus' age.
	ldy #$01				;NMI = finished.
	sty NMIStatus			;
	pla						;Restore Y.
	tay						;
	pla						;Restore X.
	tax						;
	pla						;restore A.
	plp						;Restore processor status flags.
	rti						;Return from NMI.

;----------------------------------------[ GoMainRoutine ]-------------------------------------------

;This is where the real code of each frame is executed.
;MainRoutine or TitleRoutine (Depending on the value of GameMode)
;is used as an index into a code pointer table, and this routine
;is executed.

GoMainRoutine:
	lda GameMode			;0 if game is running, 1 if at intro screen.
	beq +					;Branch if mode=Play.
	jmp AreaRefresh			;Jump to $8000, where a routine similar to the one-->
							;below is executed, only using TitleRoutine instead
							;of MainRoutine as index into a jump table.
*	lda Joy1Change			;
	and #$10				;Has START been pressed?-->
	beq +++					;if not, execute current routine as normal.

	lda MainRoutine			;
	cmp #$03				;Is game engine running?-->
	beq +					;If yes, check for routine #5 (pause game).
	cmp #$05				;Is game paused?-->
	bne +++					;If not routine #5 either, don't care about START being pressed.
	lda #$03				;Otherwise, switch to routine #3 (game engine).
	bne ++					;Branch always.
*	lda #$05				;Switch to pause routine.
*	sta MainRoutine			;(MainRoutine = 5 if game paused, 3 if game engine running).
	lda GamePaused			;
	eor #$01				;Toggle game paused.
	sta GamePaused			;
	jsr PauseMusic			;Silences music while game paused.

*	lda MainRoutine			;
	jsr ChooseRoutine		;Use MainRoutine as index into routine table below.

;Pointer table to code.

	.word AreaInit			; Area init.
	.word MoreInit			; More area init.
	.word SamusInit			; Samus init.
	.word GameEngine  		; Game engine.
	.word EngineGameOver	; Display GAME OVER.
	.word PauseMode   		; Pause game.
	.word GoPassword		; Display password.
	.word IncrementRoutine	; Just advances to next routine in table.
	.word SamusIntro  		; Intro.
	.word WaitTimer   		; Delay.

IncrementRoutine:
	inc MainRoutine			;Increment to next routine in above table.
	rts						;

;-------------------------------------[ Clear name tables ]------------------------------------------

ClearNameTables:
	jsr ClearNameTable0		;Always clear name table 0 first.
	lda GameMode			;
	beq +					;Branch if mode = Play.
	lda TitleRoutine		;
	cmp #$1D				;If running the end game routine, clear-->
	beq ++					;name table 2, else clear name table 1.
*	lda #$02				;Name table to clear + 1 (name table 1).
	bne +++					;Branch always.
*	lda #$03				;Name table to clear + 1 (name table 2).
	bne ++					;Branch always.

ClearNameTable0:
*	lda #$01				;Name table to clear + 1 (name table 0).
*	sta $01					;Stores name table to clear.
	lda #$FF				;
	sta $00					;Value to fill with.

ClearNameTable:
	ldx PPUStatus			;Reset PPU address latch.
	lda PPUCNT0ZP			;
	and #$FB				;PPU increment = 1.
	sta PPUCNT0ZP			;
	sta PPUControl0			;Store control bits in PPU.
	ldx $01					;
	dex						;Name table = X - 1.
	lda HiPPUTable,x		;get high PPU address.  pointer table at $C19F.
	sta PPUAddress			;
	lda #$00				;Set PPU start address (High byte first).
	sta PPUAddress			;
	ldx #$04				;Prepare to loop 4 times.
	ldy #$00				;Inner loop value.
	lda $00					;Fill-value.
*	sta PPUIOReg			;
	dey						;
	bne -					;Loops until the desired name table is cleared.-->
	dex						;It also clears the associated attribute table.
	bne -					;
	rts						;

;The following table is used by the above routine for finding
;the high byte of the proper name table to clear.

HiPPUTable:
 	.byte $20				;Name table 0.
	.byte $24				;Name table 1.
	.byte $28				;Name table 2.
	.byte $2C				;Name table 3.

;-------------------------------------[ Erase all sprites ]------------------------------------------

EraseAllSprites:
	ldy #$02				;
	sty $01					;Loads locations $00 and $01 with -->
	ldy #$00				;#$00 and #$02 respectively
	sty $00					;
	ldy #$00				;
	lda #$F0				;
*	sta ($00),y				;Stores #$F0 in memory addresses $0200 thru $02FF.
	iny						; 
	bne -					;Loop while more sprite RAM to clear.
	lda GameMode			;
	beq Exit101				;Exit subroutine if GameMode=Play(#$00)
	jmp DecSpriteYCoord		;($988A)Find proper y coord of sprites.

Exit101:
	rts						;Return used by subroutines above and below.

;---------------------------------------[ Remove intro sprites ]-------------------------------------

;The following routine is used in the Intro to remove the sparkle sprites and the crosshairs
;sprites every frame.  It does this by loading the sprite values with #$F4 which moves the 
;sprite to the bottom right of the screen and uses a blank graphic for the sprite.

RemoveIntroSprites:
	ldy #$02				;Start at address $200.
	sty $01					;
	ldy #$00				;
	sty $00					;($00) = $0200 (sprite page)
	ldy #$5F				;Prepare to clear RAM $0200-$025F
	lda #$F4				;
*	sta ($00),y				;
	dey						;Loop unitl $200 thru $25F is filled with #$F4.
	bpl -					;
	lda GameMode			;
	beq Exit101				; branch if mode = Play.
	jmp DecSpriteYCoord		;($988A)Find proper y coord of sprites.

;-------------------------------------[Clear RAM $33 thru $DF]---------------------------------------

;The routine below clears RAM associated with rooms and enemies.

ClearRAM_33_DF:
	ldx #$33				;
	lda #$00				;
*	sta $00,x				;Clear RAM addresses $33 through $DF.
	inx						;
	cpx #$E0				;
	bcc -					;Loop until all desired addresses are cleared.
	rts						;

;--------------------------------[ Check and prepare palette write ]---------------------------------

CheckPalWrite:
	lda GameMode			;
	beq +					;Is game being played? If so, branch to exit.
	lda TitleRoutine		;
	cmp #$1D				;Is Game at ending sequence? If not, branch
	bcc +					;
	jmp EndGamePalWrite		;($9F54)Write palette data for ending.
*	ldy PalDataPending		;
	bne ++					;Is palette data pending? If so, branch.
	lda GameMode			;
	beq +	   				;Is game being played? If so, branch to exit.
	lda TitleRoutine		;
	cmp #$15				;Is intro playing? If not, branch.
	bcs +					;
	jmp StarPalSwitch		;($8AC7)Cycles palettes for intro stars twinkle.
*	rts						;Exit when no palette data pending.

;Prepare to write palette data to PPU.

*	dey										;Palette # = PalDataPending - 1.
	tya										;
	asl										;* 2, each pal data ptr is 2 bytes (16-bit).
	tay										;
	ldx PaletteColorsPointerTable,y			;X = low byte of PPU data pointer.
	lda PaletteColorsPointerTable+1,y		;
	tay										;Y = high byte of PPU data pointer.
	lda #$00								;Clear A.
	sta PalDataPending						;Reset palette data pending byte.
	
PreparePPUProcess_:
	stx $00									;Lower byte of pointer to PPU string.
	sty $01									;Upper byte of pointer to PPU string.
	jmp ProcessPPUString					;Write data string to PPU.

;----------------------------------------[Read joy pad status ]--------------------------------------

;The following routine reads the status of both joypads

ReadJoyPads:
	ldx #$00				;Load x with #$00. Used to read status of joypad 1.
	stx $01					;
	jsr ReadOnePad			;
	inx						;Load x with #$01. Used to read status of joypad 2.
	inc $01					;

ReadOnePad:
	ldy #$01				;These lines strobe the -->       
	sty CPUJoyPad1   		;joystick to enable the -->
	dey						;program to read the -->
	sty CPUJoyPad1  		;buttons pressed.
	
	ldy #$08				;Do 8 buttons.
*	pha						;Store A.
	lda CPUJoyPad1,x		;Read button status. Joypad 1 or 2.
	sta $00					;Store button press at location $00.
	lsr						;Move button push to carry bit.
	ora $00					;If joystick not connected, -->
	lsr						;fills Joy1Status with all 1s.
	pla						;Restore A.
	rol						;Add button press status to A.
	dey     				;Loop 8 times to get -->
	bne -					;status of all 8 buttons.

	ldx $01					;Joypad #(0 or 1).
	ldy Joy1Status,x		;Get joypad status of previous refresh.
	sty $00					;Store at $00.
	sta Joy1Status,x		;Store current joypad status.
	eor $00					;
	beq +	   				;Branch if no buttons changed.
	lda $00					;			
	and #$BF				;Remove the previous status of the B button.
	sta $00					;
	eor Joy1Status,x		;
*	and Joy1Status,x		;Save any button changes from the current frame-->
	sta Joy1Change,x		;and the last frame to the joy change addresses.
	sta Joy1Retrig,x		;Store any changed buttons in JoyRetrig address.
	ldy #$20				;
	lda Joy1Status,x		;Checks to see if same buttons are being-->
	cmp $00					;pressed this frame as last frame.-->
	bne +					;If none, branch.
	dec RetrigDelay1,x		;Decrement RetrigDelay if same buttons pressed.
	bne ++					;		
	sta Joy1Retrig,x		;Once RetrigDelay=#$00, store buttons to retrigger.
	ldy #$08				;
*	sty RetrigDelay1,x		;Reset retrigger delay to #$20(32 frames)-->
*	rts						;or #$08(8 frames) if already retriggering.

;-------------------------------------------[ Update timer ]-----------------------------------------

;This routine is used for timing - or for waiting around, rather.
;TimerDelay is decremented every frame. When it hits zero, $2A, $2B and $2C are
;decremented if they aren't already zero. The program can then check
;these variables (it usually just checks $2C) to determine when it's time
;to "move on". This is used for the various sequences of the intro screen,
;when the game is started, when Samus takes a special item, and when GAME
;OVER is displayed, to mention a few examples.

UpdateTimer:
	ldx #$01				;First timer to decrement is Timer2.
	dec TimerDelay			;
	bpl DecTimer			;
	lda #$09				;TimerDelay hits #$00 every 10th frame.
	sta TimerDelay			;Reset TimerDelay after it hits #$00.
	ldx #$02				;Decrement Timer3 every 10 frames.

DecTimer:
	lda Timer1,x			;
	beq +					;Don't decrease if timer is already zero.
	dec Timer1,x			;
*	dex						;Timer1 and Timer2 decremented every frame.
	bpl DecTimer			;
	rts						;

;-----------------------------------------[ Choose routine ]-----------------------------------------

;This is an indirect jump routine. A is used as an index into a code
;pointer table, and the routine at that position is executed. The programmers
;always put the pointer table itself directly after the JSR to ChooseRoutine,
;meaning that its address can be popped from the stack.

ChooseRoutine:
	asl						;* 2, each ptr is 2 bytes (16-bit).
	sty TempY				;Temp storage.
	stx TempX				;Temp storage.
	tay						;
	iny						;
	pla						;Low byte of ptr table address.
	sta CodePtr				;
	pla						;High byte of ptr table address.
	sta CodePtr+1			;
	lda (CodePtr),y			;Low byte of code ptr.
	tax						;
	iny						;
	lda (CodePtr),y			;High byte of code ptr.
	sta CodePtr+1			;
	stx CodePtr				;
	ldx TempX				;Restore X.
	ldy TempY				;Restore Y.
	jmp (CodePtr)			;

;--------------------------------------[ Write to scroll registers ]---------------------------------

WriteScroll:
	lda PPUStatus			;Reset scroll register flip/flop
	lda ScrollX				;
	sta PPUScroll			;
	lda ScrollY				;X and Y scroll offsets are loaded serially.
	sta PPUScroll			;
	rts						;

;----------------------------------[ Add y index to stored addresses ]-------------------------------

;Add Y to pointer at $0000. 

AddYToPtr00:
	tya					;
	clc					;Add value stored in Y to lower address-->
	adc $00				;byte stored in $00.
	sta $00				;
	bcc +				;Increment $01(upper address byte) if carry-->
	inc $01				;has occurred.
*	rts					;

;Add Y to pointer at $0002

AddYToPtr02:
	tya					;
	clc					;Add value stored in Y to lower address-->
	adc $02				;byte stored in $02.
	sta $02				;
	bcc +				;Increment $01(upper address byte) if carry-->
	inc $03				;has occurred.
*	rts					;

;--------------------------------[ Simple divide and multiply routines ]-----------------------------

Adiv32: 
	lsr				;Divide by 32.

Adiv16: 
	lsr				;Divide by 16.

Adiv8:  
	lsr				;Divide by 8.
	lsr				;
	lsr				;Divide by shifting A right.
	rts				;

Amul32: 
	asl				;Multiply by 32.

Amul16: 
	asl				;Multiply by 16.

Amul8:
	asl				;Multiply by 8.
	asl				;
	asl				;Multiply by shifting A left.
	rts				;

;-------------------------------------[ PPU writing routines ]---------------------------------------

;Checks if any data is waiting to be written to the PPU.
;RLE data is one tile that repeats several times in a row.  RLE-Repeat Last Entry

CheckPPUWrite:
	lda PPUDataPending			;
	beq +	   					;If zero no PPU data to write, branch to exit.
	lda #$A1					;			
	sta $00						;Sets up PPU writer to start at address $07A1.
	lda #$07					;
	sta $01	 					;$0000 = ptr to PPU data string ($07A1).
	jsr ProcessPPUString    	;($C30C)write it to PPU.
	lda #$00					;
	sta PPUStrIndex				;PPU data string has been written so the data-->
	sta PPUDataString			;stored for the write is now erased.
	sta PPUDataPending			;
*	rts							;

PPUWrite:
	sta PPUAddress				;Set high PPU address.
	iny							;
	lda ($00),y					;
	sta PPUAddress				;Set low PPU address.
	iny							;
	lda ($00),y					;Get data byte containing rep length & RLE status.
	asl							;Carry Flag = PPU address increment (0 = 1, 1 = 32).
	jsr SetPPUInc				;($C318)Update PPUCtrl0 according to Carry Flag.
	asl							;Carry Flag = bit 6 of byte at ($00),y (1 = RLE).
	lda ($00),y					;Get data byte again.
	and #$3F					;Keep lower 6 bits as loop counter.
	tax							;
	bcc PPUWriteLoop			;If Carry Flag not set, the data is not RLE.
	iny							;Data is RLE, advance to data byte.

PPUWriteLoop:
	bcs +						;
	iny							;Only inc Y if data is not RLE.
*	lda ($00),y					;Get data byte.
	sta PPUIOReg				;Write to PPU.
	dex							;Decrease loop counter.
	bne PPUWriteLoop			;Keep going until X=0.
	iny							;
	jsr AddYToPtr00				;($C2A8)Point to next data chunk.

;Write data string at ($00) to PPU.

ProcessPPUString:
	ldx PPUStatus				;Reset PPU address flip/flop.
	ldy #$00					;
	lda ($00),y					;
	bne PPUWrite				;If A is non-zero, PPU data string follows,-->
	jmp WriteScroll				;($C29A)Otherwise we're done.

;In: CF = desired PPU address increment (0 = 1, 1 = 32).
;Out: PPU control #0 ($2000) updated accordingly.

SetPPUInc:
	pha							;Preserve A.
	lda PPUCNT0ZP				;
	ora #$04					;
	bcs +						;PPU increment = 32 only if Carry Flag set,-->
	and #$FB					;else PPU increment = 1.
*	sta PPUControl0				;
	sta PPUCNT0ZP				;
	pla							;Restore A.
	rts							;

;Erase blasted tile on nametable.  Each screen is 16 tiles across and 15 tiles down.
EraseTile:
	ldy #$01					;
	sty PPUDataPending			;data pending = YES.
	dey							;	
	lda ($02),y					;
	and #$0F					;
	sta $05						;# of tiles horizontally.
	lda ($02),y					;
	jsr Adiv16					;($C2BF)/ 16.
	sta $04						;# of tiles vertically.
	ldx PPUStrIndex				;
*	lda $01						;
	jsr WritePPUByte			;($C36B)write PPU high address to $07A1,PPUStrIndex.
	lda $00						;
	jsr WritePPUByte			;($C36B)write PPU low address to $07A1,PPUStrIndex.
	lda $05						;data length.
	sta $06						;
	jsr WritePPUByte			;($C36B)write PPU string length to $07A1,PPUStrIndex.
*	iny							;
	lda ($02),y					;Get new tile to replace old tile.
	jsr WritePPUByte			;($C36B)Write it to $07A1,PPUStrIndex, inc x.
	dec $06						;
	bne -						;Branch if more horizontal tiles to replace.
	stx PPUStrIndex				;
	sty $06						;
	ldy #$20					;
	jsr AddYToPtr00				;($C2A8)Move to next name table line.
	ldy $06						;Store index to find next tile info.
	dec $04						;
	bne --						;Branch if more lines need to be changed on name table.
	jsr EndPPUString			;($c376)Finish writing PPU string and exit.

WritePPUByte:
	sta PPUDataString,x			;Store data byte at end of PPUDataString.

NextPPUByte:
	inx							;PPUDataString has increased in size by 1 byte.
	cpx #$4F					;PPU byte writer can only write a maximum of #$4F bytes
	bcc +						;If PPU string not full, branch to get more data.
	ldx PPUStrIndex				;

EndPPUString:
	lda #$00					;If PPU string is already full, or all PPU bytes loaded,-->
	sta PPUDataString,x			;add #$00 as last byte to the PPU byte string.
	pla							;
	pla							;Remove last return address from stack and jump out of-->
*	rts							;PPU writing routines.

;The following routine is only used by the intro routine to load the sprite 
;palette data for the twinkling stars. The following memory addresses are used:
;$00-$01 Destination address for PPU write, $02-$03 Source address for PPU data,
;$04 Temp storage for PPU data byte, $05 PPU data string counter byte,
;$06 Temp storage for index byte.

PrepPPUPaletteString:
	ldy #$01					;
	sty PPUDataPending			;Indicate data waiting to be written to PPU.
	dey							;			
	beq ++++					;Branch always

*	sta $04						;$04 now contains next data byte to be put into the PPU string.
	lda $01						;High byte of staring address to write PPU data 
	jsr WritePPUByte			;($C36B)Put data byte into PPUDataString.
	lda $00						;Low byte of starting address to write PPU data.
	jsr WritePPUByte			;($C36B)Put data byte into PPUDataString.
	lda $04						;A now contains next data byte to be put into the PPU string.
	jsr SeparateControlBits		;($C3C6)Break control byte into two bytes.

	bit $04						;Check to see if RLE bit is set in control byte.-->
	bvc WritePaletteStringByte	;If not set, branch to load byte. Else increment index-->
	iny							;to find repeating data byte.

WritePaletteStringByte:
	bit $04						;Check if RLE bit is set (again). if set, load same-->
	bvs +						;byte over and over again until counter = #$00.
	iny							;Non-repeating data byte. Increment for next byte.
*	lda ($02),y					;
	jsr WritePPUByte			;($C36B)Put data byte into PPUDataString.
	sty $06						;Temporarily store data index.
	ldy #$01					;PPU address increment = 1.
	bit $04						;If MSB set in control bit, it looks like this routine might-->
	bpl +						;have been used for a software control verticle mirror, but
								;the starting address has already been written to the PPU-->
								;string so this section has no effect whether the MSB is set-->
								;or not. The PPU is always incremented by 1.
	ldy #$20					;PPU address increment = 32.
*	jsr AddYToPtr00				;($C2A8)Set next PPU write address.(Does nothing, already set).
	ldy $06						;Restore data index to Y.
	dec $05						;Decrement counter byte.
	bne WritePaletteStringByte	;If more bytes to write, branch to write another byte.
	stx PPUStrIndex				;Store total length, in bytes, of PPUDataString.
	iny							;Move to next data byte(should be #$00).

*	ldx PPUStrIndex				;X now contains current length of PPU data string.
	lda ($02),y					;
	bne ----					;Is PPU string done loading (#$00)? If so exit,-->
	jsr EndPPUString			;($C376)else branch to process PPU byte.

SeparateControlBits:
	sta $04						;Store current byte 
	and #$BF					;
	sta PPUDataString,x			;Remove RLE bit and save control bit in PPUDataString.
	and #$3F					;
	sta $05						;Extract counter bits and save them for use above.
	jmp NextPPUByte				;($C36E)

;----------------------------------------[ Math routines ]-------------------------------------------

TwosCompliment:
	eor #$FF					;
	clc							;Generate twos compliment of value stored in A.
	adc #$01					;
	rts							;

;The following two routines add a Binary coded decimal (BCD) number to another BCD number.
;A base number is stored in $03 and the number in A is added/subtracted from $03.  $01 and $02 
;contain the lower and upper digits of the value in A respectively.  If an overflow happens after
;the addition/subtraction, the carry bit is set before the routine returns.

Base10Add:
	jsr ExtractNibbles			;($C41D)Separate upper 4 bits and lower 4 bits.
	adc $01						;Add lower nibble to number.
	cmp #$0A					;
	bcc +						;If result is greater than 9, add 5 to create-->
	adc #$05					;valid result(skip #$0A thru #$0F).
*	clc							;
	adc $02						;Add upper nibble to number.
	sta $02						;
	lda $03						;
	and #$F0					;Keep upper 4 bits of HealthLo/HealthHi in A.
	adc $02						;
	bcc ++						;
*	adc #$5F					;If upper result caused a carry, add #$5F to create-->
	sec							;valid result. Set carry indicating carry to next digit.
	rts							;
*	cmp #$A0					;If result of upper nibble add is greater than #$90,-->
	bcs --						;Branch to add #$5F to create valid result.
	rts							;

Base10Subtract:
	jsr ExtractNibbles			;($C41D)Separate upper 4 bits and lower 4 bits.
	sbc $01						;Subtract lower nibble from number.
	sta $01						;
	bcs +						;If result is less than zero, add 10 to create-->
	adc #$0A					;valid result.
	sta $01						;
	lda $02						;
	adc #$0F					;Adjust $02 to account for borrowing.
	sta $02						;
*	lda $03						;Keep upper 4 bits of HealthLo/HealthHi in A.
	and #$F0					;
	sec							;
	sbc $02						;If result is greater than zero, branch to finish.
	bcs +						;
	adc #$A0					;Add 10 to create valid result.
	clc							;
*	ora $01						;Combine A and $01 to create final value.
	rts							;

ExtractNibbles:
	pha							;
	and #$0F					;Lower 4 bits of value to change HealthLo/HealthHi by.
	sta $01						;
	pla							;
	and #$F0					;Upper 4 bits of value to change HealthLo/HealthHi by.
	sta $02						;
	lda $03						;
	and #$0F					;Keep lower 4 bits of HealthLo/HealthHi in A.
	rts							;

;---------------------------[ NMI and PPU control routines ]--------------------------------

; Wait for the NMI to end.

WaitNMIPass:    
	jsr ClearNMIStat		;($C434)Indicate currently in NMI.
*	lda NMIStatus			;
	beq -					;Wait for NMI to end.
	rts						;

ClearNMIStat:
	lda #$00				;Clear NMI byte to indicate the game is-->
	sta NMIStatus			;currently running NMI routines.
	rts						;

ScreenOff:
	lda PPUCNT1ZP			;
	and #$E7				; BG & SPR visibility = off

WriteAndWait:
*	sta PPUCNT1ZP			;Update value to be loaded into PPU control register.

WaitNMIPass_:
	jsr ClearNMIStat		;($C434)Indicate currently in NMI.
*	lda NMIStatus			;
	beq -					;Wait for NMI to end before continuing.
	rts						;

ScreenOn:
	lda PPUCNT1ZP			;
	ora #$1E				;BG & SPR visibility = on
	bne --					;Branch always

;Update the actual PPU control registers.

WritePPUCtrl:
	lda PPUCNT0ZP			;
	sta PPUControl0			;
	lda PPUCNT1ZP			;Update PPU control registers.
	sta PPUControl1			;
	lda MirrorCntrl			;
	jsr PrepPPUMirror		;($C4D9)Setup vertical or horizontal mirroring.

ExitSub:
	rts						;Exit subroutines.

;Turn off both screen and NMI.

ScreenNmiOff:
	lda PPUCNT1ZP			;
	and #$E7				;BG & SPR visibility = off
	jsr WriteAndWait		;($C43D)Wait for end of NMI.
	lda PPUCNT0ZP			;Prepare to turn off NMI in PPU.
	and #$7F				;NMI = off
	sta PPUCNT0ZP			;
	sta PPUControl0			;Actually load PPU register with NMI off value.
	rts						;

;The following routine does not appear to be used.

	lda PPUCNT0ZP			;Enable VBlank.
	ora #$80				;
	sta PPUCNT0ZP			;Write PPU control register 0 and PPU status byte.
	sta PPUControl0			;
	lda PPUCNT1ZP			;Turn sprites and screen on.
	ora #$1E				;
	bne --					;Branch always.

VBOffAndHorzWrite: 
	lda PPUCNT0ZP			;
	and #$7B				;Horizontal write, disable VBlank. 
*	sta PPUControl0			;Save new values in the PPU control register-->
	sta PPUCNT0ZP			;and PPU status byte.
	rts						;

NmiOn:
*	lda PPUStatus			;
	and #$80				;Wait for end of VBlank.
	bne -					;
	lda PPUCNT0ZP			;
	ora #$80				;Enable VBlank interrupts.
	bne --					;Branch always.

;--------------------------------------[ Timer routines ]--------------------------------------------

;The following routines set the timer and decrement it. The timer is set after Samus dies and
;before the GAME OVER message is dispayed.  The timer is also set while the item pickup music
;is playing.

WaitTimer:
	lda Timer3				;Exit if timer hasn't hit zero yet
	bne +					;
	lda NextRoutine			;Set GameOver as next routine.
	cmp #$04				;
	beq SetMainRoutine		;Set GoPassword as main routine.
	cmp #$06				;
	beq SetMainRoutine		;
	jsr StartMusic			;($D92C)Assume power up was picked up and GameEngine-->
	lda NextRoutine			;is next routine. Start area music before exiting.

SetMainRoutine:
	sta MainRoutine			;Set next routine to run.
*	rts						;

SetTimer:
	sta Timer3				;Set Timer3. Frames to wait is value stored in A*10.
	stx NextRoutine			;Save routine to jump to after Timer3 expires.
	lda #$09				;Next routine to run is WaitTimer.
	bne SetMainRoutine		;Branch always.

;-----------------------------------[ PPU mirroring routines ]---------------------------------------

PrepVertMirror:
	nop						;
	nop						;Prepare to set PPU for vertical mirroring (again).
	lda #$47				;

SetPPUMirror:
	lsr						;
	lsr						;Move bit 3 to bit 0 position.
	lsr						;
	and #$01				;Remove all other bits.
	sta $00					;Store at address $00.
	lda MMCReg0Cntrl		;
	and #$FE				;Load MMCReg0Cntrl and remove bit 0.
	ora $00					;Replace bit 0 with stored bit at $00.
	sta MMCReg0Cntrl		;
	sta MMC1Reg0			;
	lsr						;
	sta MMC1Reg0			;
	lsr						;
	sta MMC1Reg0			;
	lsr						;Load new configuration data serially-->
	sta MMC1Reg0			;into MMC1Reg0.
	lsr						;
	sta MMC1Reg0			;
	rts						;

PrepPPUMirror:
	lda MirrorCntrl			;Load MirrorCntrl into A.
	jmp SetPPUMirror		;($C4B6)Set mirroring through MMC1 chip.

;-----------------------------[ Switch bank and init bank routines ]---------------------------------

;This is how the bank switching works... Every frame, the routine below
;is executed. First, it checks the value of SwitchPending. If it is zero,
;the routine will simply exit. If it is non-zero, it means that a bank
;switch has been issued, and must be performed. SwitchPending then contains
;the bank to switch to, plus one.

CheckSwitch:
	ldy SwitchPending		;
	beq +					;Exit if zero(no bank switch issued). else Y contains bank#+1.
	jsr SwitchOK			;($C4E8)Perform bank switch.
	jmp GoBankInit			;($C510)Initialize bank switch data.

SwitchOK:
	lda #$00				;Reset(so that the bank switch won't be performed-->
	sta SwitchPending		;every succeeding frame too).
	dey						;Y now contains the bank to switch to.
	sty CurrentBank			;

ROMSwitch:
	tya						;
	sta $00					;Bank to switch to is stored at location $00.
	lda SwitchUpperBits		;Load upper two bits for Reg 3 (they should always be 0).
	and #$18				;Extract bits 3 and 4 and add them to the current-->
	ora $00					;bank to switch to.
	sta SwitchUpperBits		;Store any new bits set in 3 or 4(there should be none).

;Loads the lower memory page with the bank specified in A.

MMCWriteReg3:
	sta MMC1Reg3			;Write bit 0 of ROM bank #.
	lsr						;
	sta MMC1Reg3			;Write bit 1 of ROM bank #.
	lsr						;
	sta MMC1Reg3			;Write bit 2 of ROM bank #.
	lsr						;
	sta MMC1Reg3			;Write bit 3 of ROM bank #.
	lsr						;
	sta MMC1Reg3			;Write bit 4 of ROM bank #.
	lda $00					;Restore A with current bank number before exiting.
*	rts						;

;Calls the proper routine according to the bank number in A.

GoBankInit:
	asl							;*2 For proper table offset below.
	tay							;
	lda BankInitTable,y			;
	sta $0A						;Load appropriate subroutine address into $0A and $0B.
	lda BankInitTable+1,y		;
	sta $0B						;
	jmp ($000A)					;Jump to appropriate initialization routine.

BankInitTable:
	.word InitBank0				;($C531)Initialize bank 0.
	.word InitBank1				;($C552)Initialize bank 1.
	.word InitBank2				;($C583)Initialize bank 2.
	.word InitBank3				;($C590)Initialize bank 3.
	.word InitBank4				;($C5B6)Initialize bank 4.
	.word InitBank5				;($C5C3)Initialize bank 5.
	.word ExitSub				;($C45C)Rts
	.word ExitSub				;($C45C)Rts
	.word ExitSub				;($C45C)Rts

;Title screen memory page.

InitBank0:
	ldy #$00					;
	sty GamePaused				; Ensure game is not paused.
	iny							; Y=1.
	sty GameMode				; Game is at title routines.
	jsr ScreenNmiOff			; Waits for NMI to end then turns it off.
	jsr CopyMap					; Copy game map from ROM to cartridge RAM $7000-$73FF
	jsr ClearNameTables			; Erase name table data.

	ldy #$A0					;
*	lda IntroStarsData-1,y		; start from end of Intro stars data table, go down till 0
	sta IntroStarSpriteMem,y	; Loads sprite info for stars into RAM $6E00 thru 6E9F.
	dey							;
	bne -						;

	jsr InitTitleGFX			;($C5D7)Load title GFX.
	jmp NmiOn					;($C487)Turn on VBlank interrupts.

;Brinstar memory page.

InitBank1:
	lda #$00				;
	sta GameMode			;GameMode = play.
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	lda MainRoutine			;
	cmp #$03				;Is game engine running? if so, branch.-->
	beq +					;Else do some housekeeping first.
	lda #$00				;
	sta MainRoutine			;Run InitArea routine next.
	sta InArea				;Start in Brinstar.
	sta GamePaused			;Make sure game is not paused.
	jsr ClearRAM_33_DF		;($C1D4)Clear game engine memory addresses.
	jsr ClearSamusStats		;($C578)Clear Samus' stats memory addresses.
*	ldy #$00				;
	jsr ROMSwitch			;($C4EF)Load Brinstar memory page into lower 16Kb memory.
	jsr InitBrinstarGFX		;($C604)Load Brinstar GFX.
	jmp NmiOn				;($C487)Turn on VBlank interrupts.

ClearSamusStats:
	ldy #$0F				;
	lda #$00				;Clears Samus stats(Health, full tanks, game timer, etc.).
*	sta $0100,y				;Load $100 thru $10F with #$00.
	dey						;
	bpl -					;Loop 16 times.
	rts						;

;Norfair memory page.

InitBank2:
	lda #$00				;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	jsr InitNorfairGFX		;($C622)Load Norfair GFX.
	jmp NmiOn				;($C487)Turn on VBlank interrupts.

;Tourian memory page.

InitBank3:
	lda #$00				;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	ldy #$0D				;
*	lda MetroidData,y		;Load info from table below into-->
	sta $77F0,y				;$77F0 thru $77FD.
	dey				;
	bpl -					;
	jsr InitTourianGFX		;($C645)Load Tourian GFX.
	jmp NmiOn				;($C487)Turn on VBlank interrupts.

;Table used by above subroutine and loads the initial data used to describe
;metroid's behavior in the Tourian section of the game.

MetroidData:
	.byte $F8, $08, $30, $D0, $60, $A0, $02, $04, $00, $00, $00, $00, $00, $00

;Kraid memory page.

InitBank4:
	lda #$00				;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	jsr InitKraidGFX		;($C677)Load Kraid GFX.
	jmp NmiOn				;($C487)Turn on VBlank interrupts.

;Ridley memory page.

InitBank5:
	lda #$00				;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	jsr InitRidleyGFX		;($C69F)Loag Ridley GFX.
	jmp NmiOn				;($C487)Turn on VBlank interrupts.

InitEndGFX:
	lda #$01				;
	sta GameMode			;Game is at title/end game.
	jmp InitGFX6			;($C6C2)Load end game GFX.

InitTitleGFX:
	ldy #$15				;Entry 21 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.

LoadSamusGFX:
	ldy #$00				;Entry 0 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	lda JustInBailey		;
	beq +					;Branch if wearing suit
	ldy #$1B				;Entry 27 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Switch to girl gfx
*	ldy #$14				;Entry 20 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$17				;Entry 23 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$18				;Entry 24 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitBrinstarGFX:
	ldy #$03				;Entry 3 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$04				;Entry 4 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$05				;Entry 5 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$06				;Entry 6 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitNorfairGFX:
	ldy #$04				;Entry 4 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$05				;Entry 5 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$07				;Entry 7 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$08				;Entry 8 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$09				;Entry 9 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitTourianGFX:
	ldy #$05				;Entry 5 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0A				;Entry 10 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0B				;Entry 11 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0C				;Entry 12 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0D				;Entry 13 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0E				;Entry 14 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$1A				;Entry 26 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$1C				;Entry 28 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitKraidGFX:
	ldy #$04				;Entry 4 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$05				;Entry 5 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0A				;Entry 10 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0F				;Entry 15 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$10				;Entry 16 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$11				;Entry 17 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitRidleyGFX:
	ldy #$04				;Entry 4 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$05				;Entry 5 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$0A				;Entry 10 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$12				;Entry 18 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$13				;Entry 19 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitGFX6:
	ldy #$01				;Entry 1 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$02				;Entry 2 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$19				;Entry 25 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

InitGFX7:
	ldy #$17				;Entry 23 in GFXInfo table.
	jsr LoadGFX				;($C7AB)Load pattern table GFX.
	ldy #$16				;Entry 22 in GFXInfo table.
	jmp LoadGFX				;($C7AB)Load pattern table GFX.

;The table below contains info for each tile data block in the ROM.
;Each entry is 7 bytes long. The format is as follows:
;byte 0: ROM bank where GFX data is located.
;byte 1-2: 16-bit ROM start address (src).
;byte 3-4: 16-bit PPU start address (dest).
;byte 5-6: data length (16-bit).

GFXInfo:
	.byte $06					;[SPR]Samus, items.				Entry 0.
	.word $8000, $0000, $09A0	
	.byte $04					;[SPR]Samus in ending.			Entry 1.
	.word $8D60, $0000, $0520
	.byte $01					;[BGR]Partial font, "The End".	Entry 2.
	.word $8D60, $1000, $0400
	.byte $06					;[BGR]Brinstar rooms.			Entry 3.
	.word $9DA0, $1000, $0150
	.byte $05					;[BGR]Misc. objects.			Entry 4.
	.word $8D60, $1200, $0450
	.byte $06					;[BGR]More Brinstar rooms.		Entry 5.
	.word $9EF0, $1800, $0800
	.byte $01					;[SPR]Brinstar enemies.			Entry 6.
	.word $9160, $0C00, $0400
	.byte $06					;[BGR]Norfair rooms.			Entry 7.
	.word $A6F0, $1000, $0260
	.byte $06					;[BGR]More Norfair rooms.		Entry 8.
	.word $A950, $1700, $0070
	.byte $02					;[SPR]Norfair enemies.			Entry 9.
	.word $8D60, $0C00, $0400
	.byte $06					;[BGR]Tourian rooms.			Entry 10.
	.word $A9C0, $1000, $02E0
	.byte $06					;[BGR]More Tourian rooms.		Entry 11.
	.word $ACA0, $1200, $0600
	.byte $06					;[BGR]Mother Brain room.		Entry 12.
	.word $B2A0, $1900, $0090
	.byte $05					;[BGR]Misc. object.				Entry 13.
	.word $91B0, $1D00, $0300
	.byte $02					;[SPR]Tourian enemies.			Entry 14.
	.word $9160, $0C00, $0400
	.byte $06					;[BGR]More Tourian rooms.		Entry 15.
	.word $B330, $1700, $00C0
	.byte $04					;[BGR]Misc. object and fonts.	Entry 16.
	.word $9360, $1E00, $0200
	.byte $03					;[SPR]Miniboss I enemies.		Entry 17.
	.word $8D60, $0C00, $0400
	.byte $06					;[BGR]More Tourian Rooms.		Entry 18.
	.word $B3F0, $1700, $00C0
	.byte $03					;[SPR]Miniboss II enemies.		Entry 19.
	.word $9160, $0C00, $0400
	.byte $06					;[SPR]Inrto/End sprites.		Entry 20.
	.word $89A0, $0C00, $0100
	.byte $06					;[BGR]Title.					Entry 21.
	.word $8BE0, $1400, $0500
	.byte $06					;[BGR]Solid tiles.				Entry 22.
	.word $9980, $1FC0, $0040
	.byte $06					;[BGR]Complete font.			Entry 23.
	.word $B4C0, $1000, $0400
	.byte $06					;[BGR]Complete font.			Entry 24.
	.word $B4C0, $0A00, $00A0
	.byte $06					;[BGR]Solid tiles.				Entry 25.
	.word $9980, $0FC0, $0040
	.byte $06					;[BGR]Complete font.			Entry 26.
	.word $B4C0, $1D00, $02A0
	.byte $06					;[SPR]Suitless Samus.			Entry 27.
	.word $90E0, $0000, $07B0
	.byte $06					;[BGR]Exclaimation point.		Entry 28.
	.word $9890, $1F40, $0010

;--------------------------------[ Pattern table loading routines ]---------------------------------

;Y contains the GFX header to fetch from the table above, GFXInfo.

LoadGFX:
	lda #$FF				;
*	clc						;Every time y decrements, the entry into the table-->
	adc #$07				;is increased by 7.  When y is less than 0, A points-->
	dey						;to the last byte of the entry in the table.
	bpl -					;
	tay						;Transfer offset into table to Y.

	ldx #$06				;
*	lda GFXInfo,y			;
	sta $00,x				;Copy entries from GFXInfo to $00-$06.
	dey						;
	dex						;
	bpl -					;

	ldy $00					;ROM bank containing the GFX data.
	jsr ROMSwitch			;($C4EF)Switch to that bank.
	lda PPUCNT0ZP			;
	and #$FB				;
	sta PPUCNT0ZP			;Set the PPU to increment by 1.
	sta PPUControl0			;
	jsr CopyGFXBlock		;($C7D5)Copy graphics into pattern tables.
	ldy CurrentBank			;
	jmp ROMSwitch			;($C4FE)Switch back to the "old" bank.

;Writes tile data from ROM to VRAM, according to the gfx header data
;contained in $00-$06.

CopyGFXBlock:
	lda $05					;
	bne GFXCopyLoop			;If $05 is #$00, decrement $06 before beginning.
	dec $06					;

GFXCopyLoop:
	lda $04					;
	sta PPUAddress			;Set PPU to proper address for GFX block write.
	lda $03					;
	sta PPUAddress			;
	ldy #$00				;Set offset for GFX data to 0.
*	lda ($01),y				;
	sta PPUIOReg			;Copy GFX data byte from ROM to Pattern table.
	dec $05					;Decrement low byte of data length.
	bne +					;Branch if high byte does not need decrementing.
	lda $06					;
	beq ++					;If copying complete, branch to exit.
	dec $06					;Decrement when low byte has reached 0.
*	iny						;Increment to next byte to copy.
	bne --					;
	inc $02					;After 256 bytes loaded, increment upper bits of-->
	inc $04					;Source and destination addresses.
	jmp GFXCopyLoop			;(&C7DB)Repeat copy routine.
*	rts						;

;-------------------------------------------[ AreaInit ]---------------------------------------------

AreaInit:
	lda #$00			;
	sta ScrollX     		;Clear ScrollX.
	sta ScrollY     		;Clear ScrollY.
	lda PPUCNT0ZP			;	
	and #$FC				;Sets nametable address = $2000.
	sta PPUCNT0ZP			;
	inc MainRoutine			;Increment MainRoutine to MoreInit.
	lda Joy1Status			;
	and #$C0				;Stores status of both the A and B buttons.
	sta ABStatus	 		;Appears to never be accessed.
	jsr EraseAllSprites		;($C1A3)Clear all sprite info.
	lda #$10				;Prepare to load Brinstar memory page.
	jsr SwitchBankIfEngineRunning		;($CA18)Check to see if ok to switch lower memory page.

;------------------------------------------[ MoreInit ]---------------------------------------------

MoreInit:
	ldy #$01				;
	sty PalDataPending		; Palette data pending = yes.
	ldx #$FF				;
	stx SpareMem75			; $75 Not referenced ever again in the game.
	inx						; X=0.
	stx AtEnding			; Not playing ending scenes.
	stx SamusDoorStatus			; Samus not in door.
	stx SamusDoorData		; Samus is not inside a door.
	stx UpdatingProjectile	; No projectiles need to be updated.
	txa						; A=0.

*	cpx #$65					; Check to see if more RAM to clear in $7A thru $DE.
	bcs +						;
	sta $7A,x					; Clear RAM $7A thru $DE.
*	cpx #$FF					; Check to see if more RAM to clear in $300 thru $3FE.
	bcs +						;
	sta ObjAction,x				; Clear RAM $300 thru $3FE.
*	inx							;
	bne ---						; Loop until all required RAM is cleared.

	jsr ScreenOff				; Turn off Background and visibility.
	jsr ClearNameTables			; Clear screen data.
	jsr EraseAllSprites			; Erase all sprites from sprite RAM.
	jsr DestroyEnemies			; Clear Enemies

	stx DoorOnNameTable3		; Clear data about doors on the name tables.
	stx DoorOnNameTable0		; 
	inx							; X=1.
	stx SpareMem30				; Not accessed by game.
	inx							; X=2.
	stx ScrollDir				; Set initial scroll direction as left.

	lda AreaStartRoomX			; Get Samus start x pos on map.
	sta MapIdxX					;
	lda AreaStartRoomY			; Get Samus start y pos on map.
	sta MapIdxY					;

	lda AreaPaletteToggleValue  ; this will have two values, 01 or 06 - toggled with an EOR of 0000 0111 to go back and forth
	sta PalToggle
	lda #$FF
	sta RoomNumber				; Room number = $FF(undefined room).
	jsr CopyPtrs    			; copy area pointers from ROM to RAM 
	jsr UpdateRoomNum				; Put room number at current map pos in $5A.
*   jsr SetupRoom				; 
	ldy RoomNumber  			; load room number to y
	iny							; inc room number
	bne -						; if room number isn't updating during this (as far as I know), how does this not loop forever?

	ldy CartRAMPtr+1			
	sty $01
	ldy CartRAMPtr
	sty $00
	lda PPUCNT0ZP
	and #$FB				; PPU increment = 1
	sta PPUCNT0ZP
	sta PPUControl0
	ldy PPUStatus   		; reset PPU addr flip/flop

; Copy room RAM #0 ($6000) to PPU Name Table #0 ($2000)

	ldy #$20
	sty PPUAddress
	ldy #$00
	sty PPUAddress
	ldx #$04				; prepare to write 4 pages
*   lda ($00),y
	sta PPUIOReg
	iny
	bne -
	inc $01
	dex
	bne -

	stx $91
	inx	     				; X = 1
	stx PalDataPending
	stx SpareMem30			;Not accessed by game.
	inc MainRoutine			;SamusInit is next routine to run.
	jmp ScreenOn

; CopyPtrs
; ========
; Copy 7 16-bit pointers from $959A thru $95A7 to $3B thru $48.

CopyPtrs:
	ldx #$0D
*   lda AreaPointers+2,x
	sta RoomPtrTable,x
	dex
	bpl -
	rts

; DestroyEnemies
; ==============

DestroyEnemies:
	lda #$00
	tax
*   cpx #$48
	bcs +
	sta $97,x
*   sta EnStatus,x
	pha
	pla
	inx
	bne --
	stx MetroidOnSamus				; Samus had no Metroid stuck to her.
	jmp TourianRoutine95AB			; tourian routine - auto return from other areas
	
; SamusInit
; =========
; Code that sets up Samus, when the game is first started.

SamusInit:
	lda #$08				;
	sta MainRoutine			;SamusIntro will be executed next frame.
	lda #$2C				;440 frames to fade in Samus(7.3 seconds).
	sta Timer3				;
	jsr IntroMusic			;($CBFD)Start the intro music.
	ldy #sa_FadeIn0			;
	sty ObjAction			;Set Samus status as fading onto screen.
	ldx #$00
	stx SamusBlink
	dex						;X = $FF
	stx EnSpawnerStatus				;init all spawner slots to inactive
	stx EnSpawnerStatus	+ 8			;...or, at least, it should? it looks like we 
	stx EnSpawnerStatus	+ 16		; only actually change the wrong 3 of them, then set one of the y pos to 0
	stx EnSpawnerStatus	+ 24		; this might be bugged! changing to match the actual 3 slot starts
	stx EndTimerLo			;Set end timer bytes to #$FF as-->
	stx EndTimerHi			;escape timer not currently active.
	stx $8B
	stx $8E
	ldy #$27
	lda InArea
	and #$0F
	beq +					;Branch if Samus starting in Brinstar.
	lsr ScrollDir			;If not in Brinstar, change scroll direction from left-->
	ldy #$2F				;to down. and set PPU for horizontal mirroring.
*       sty MirrorCntrl		;
	sty RoomMaxMissileSpawns
	sty MaxEnergyPickup
	lda AreaStartYPos		;Samus' initial vertical position
	sta ObjectY				;
	lda #$80				;Samus' initial horizontal position
	sta ObjectX				;
	lda PPUCNT0ZP			;
	and #$01				;Set Samus' name table position to current name table-->
	sta ObjectHi			;active in PPU.
	
	lda TankCount			;mod to give full health at game start
	jsr Amul16				;taking tank count into account
	ora #$09				;
	sta HealthHi			; give us 90 +
	lda #$99				;
	sta HealthLo			; 9.9 health
	
	;lda #$00				;
	;sta HealthLo			;Starting health is-->
	;lda #$03				;set to 30 units.
	;sta HealthHi			;
*   rts						;

;------------------------------------[ Main game engine ]--------------------------------------------

GameEngine:
	jsr ScrollDoor			;($E1F1)Scroll doors, if needed. 2 routine calls scrolls-->
	jsr ScrollDoor			;($E1F1)twice as fast as 1 routine call.

	lda NARPASSWORD			;
	beq +					;
	lda #$03				;The following code is only accessed if -->
	sta HealthHi			;NARPASSWORD has been entered at the -->
	lda #$FF				;password screen. Gives you new health,-->
	sta SamusGear			;missiles and every power-up every frame.
	lda #$05				;
	sta MissileCount		;

*	jsr UpdateWorld			;($CB29)Update Samus, enemies and room tiles.
	lda FanfareMusicDelay	; 
	ora PowerUpDelay		; Check if mini boss was just killed or powerup aquired.-->
	beq +					; If not, branch.

	lda #$00				;
	sta FanfareMusicDelay	;Reset delay indicators.
	sta PowerUpDelay		;
	lda #$18				;Set timer for 240 frames(4 seconds).
	ldx #$03				;GameEngine routine to run after delay expires
	jsr SetTimer			;($C4AA)Set delay timer and game engine routine.

*	lda ObjAction			;Check is Samus is dead.
	cmp #sa_Dead2   		;Is Samus dead?-->
	bne ---	   				;exit if not.
	lda AnimDelay			;Is Samus still exploding?-->
	bne ---					;Exit if still exploding.
	jsr SilenceMusic		;Turn off music.
	lda MotherBrainStatus	;
	cmp #$0A				;Is mother brain already dead? If so, branch.
	beq +					;
	lda #$04				;Set timer for 40 frames (.667 seconds).
	ldx #$04				;GameOver routine to run after delay expires.
	jmp SetTimer			;($C4AA)Set delay timer and run game over routine.

*	inc MainRoutine			;Next routine to run is GameOver.
	rts						;

;----------------------------------------[ Update age ]----------------------------------------------

;This is the routine which keeps track of Samus' age. It is called in the
;NMI. Basically, this routine just increments a 24-bit variable every
;256th frame. (Except it's not really 24-bit, because the lowest age byte
;overflows at $D0.)

UpdateAge:
	lda GameMode			;
	bne ++					;Exit if at title/password screen.
	lda MainRoutine			;
	cmp #$03				;Is game engine running?
	bne ++					;If not, don't update age.
	ldx FrameCount			;Only update age when FrameCount is zero-->
	bne ++					;(which is approx. every 4.266666666667 seconds).
	inc SamusAge,x			;Minor Age = Minor Age + 1.
	lda SamusAge			;
	cmp #$D0				;Has Minor Age reached $D0?-->
	bcc ++					;If not, we're done.-->
	lda #$00				;Else reset minor age.
	sta SamusAge			;
*	cpx #$03				;
	bcs +					;Loop to update middle age and possibly major age.
	inx						;
	inc SamusAge,x			;
	beq -	   				;Branch if middle age overflowed, need to increment--> 
*	rts						;major age too. Else exit.

;-------------------------------------------[ Game over ]--------------------------------------------

EngineGameOver:
	lda #$1C				;GameOver is the next routine to run.
	sta TitleRoutine		;
	lda #$01				;
	sta SwitchPending		;Prepare to switch to title memory page.
	jmp ScreenOff			;($C439)Turn screen off.

;------------------------------------------[ Pause mode ]--------------------------------------------

PauseMode:
	lda Joy2Status			;Load buttons currently being pressed on joypad 2.
	and #$88				;
	eor #$88				;both A & UP pressed?-->
	bne Exit14				;Exit if not.
	ldy EndTimerHi			;
	iny						;Is escape timer active?-->
	bne Exit14				;Sorry, can't quit if this is during escape scence.
	sta GamePaused			;Clear pause game indicator.
	inc MainRoutine			;Display password is the next routine to run.

Exit14:
	rts						;Exit for routines above and below.

;------------------------------------------[ GoPassword ]--------------------------------------------

GoPassword:
	lda #$19				;DisplayPassword is next routine to run.
	sta TitleRoutine		;
	lda #$01				;
	sta SwitchPending		;Prepare to switch to intro memory page.
	lda NoiseSFXFlag		;
	ora #$01				;Silence music.
	sta NoiseSFXFlag		;
	jmp ScreenOff			;($C439)Turn off screen.

;-----------------------------------------[ Samus intro ]--------------------------------------------

SamusIntro:
	jsr EraseAllSprites			;($C1A3)Clear all sprites off screen.
	ldy ObjAction				;Load Samus' fade in status.
	lda Timer3					;
	bne +						;Branch if Intro still playing.
	
;Fade in complete.
	sta ItemRoomMusicStatus		;Make sure item room music is not playing.
	lda #sa_Begin				;Samus facing forward and can't be hurt.
	sta ObjAction				;
	jsr StartMusic				;($D92C)Start main music.
	jsr SelectSamusPal			;($CB73)Select proper Samus palette.
	lda #$03					;
	sta MainRoutine				;Game engine will be called next frame.

;Still fading in.
*	cmp #$1F					;When 310 frames left of intro, display Samus.
	bcs Exit14					;Branch if not time to start drawing Samus.
	cmp SamusFadeInTimeTbl-20,y	;sa_FadeIn0 is beginning of table.
	bne +						;Every time Timer3 equals one of the entries in the table-->
	inc ObjAction				;below, change the palette used to color Samus.
	sty PalDataPending			;
*	lda FrameCount				;Is game currently on an odd frame?-->
	lsr							;If not, branch to exit.
	bcc Exit14					;Only display Samus on odd frames [the blink effect].
	lda #an_SamusFront			;Samus front animation is animation to display.-->
	jsr SetSamusAnim			;($CF6B)while fading in.
	lda #$00					;
	sta SpritePagePos			;Samus sprites start at Sprite00RAM.
	sta PageIndex				;Samus RAM is first set of RAM.
	jmp AnimDrawObject			;($DE47)Draw Samus on screen.

;The following table marks the time remaining in Timer3 when a palette change should occur during
;the Samus fade-in sequence. This creates the fade-in effect.

SamusFadeInTimeTbl:
	.byte $1E,$14,$0B,$04,$FF

;---------------------------------[ Check if game engine running ]-----------------------------------

SwitchBankIfEngineRunning:
	ldy MainRoutine			;If Samus is fading in or the wait timer is-->
	cpy #$07				;active, return from routine.
	beq +					;
	cpy #$03				;Is game engine running?
	beq ++					;If yes, branch to SwitchBank.
*	rts						;Exit if can't switch bank.

;-----------------------------------------[ Switch bank ]--------------------------------------------

;Switch to appropriate area bank

SwitchBank:
*	sta InArea				;Save current area Samus is in.
	and #$0F				;
	tay						;Use 4 LSB to load switch pending offset from BankTable table.
	lda BankTable,y			;Base is $CA30.
	sta SwitchPending		;Store switch data.
	jmp CheckSwitch			;($C4DE)Switch lower 16KB to appropriate memory page.

;Table used by above subroutine.
;Each value is the area bank number plus one.

BankTable:
	.byte $02				;Brinstar.
	.byte $03				;Norfair.
	.byte $05				;Kraid hideout.
	.byte $04				;Tourian.
	.byte $06				;Ridley hideout.

;----------------------------------[ Saved game routines (not used) ]--------------------------------

;----------------------------------------[ Choose ending ]-------------------------------------------

;Determine what type of ending is to be shown, based on Samus' age
ChooseEnding:
	ldy #$01				;
*	lda SamusAge+2			;If SamusAge+2 anything but #$00, load worst-->
	bne +					;ending(more than 37 hours of gameplay).
	lda SamusAge+1			;
	cmp AgeTable-1,y		;Loop four times to determine-->
	bcs +					;ending type from table below.
	iny						;
	cpy #$05				;
	bne -					;
*	sty EndingType			;Store the ending # (1..5), 5=best ending
	lda #$00				;
	cpy #$04				;Was the best or 2nd best ending achieved?
	bcc +					;Branch if not (suit stays on)
	lda #$01				;
*	sta JustInBailey		;Suit OFF, baby!
	rts						;

;Table used by above subroutine to determine ending type.
AgeTable:
	.byte $7A				;Max. 37 hours
	.byte $16				;Max. 6.7 hours
	.byte $0A				;Max. 3.0 hours
	.byte $04				;Best ending. Max. 1.2 hours

;--------------------------------[ Clear screen data (not used) ]------------------------------------

ClearScreenData:
	jsr ScreenOff			;($C439)Turn off screen.
	lda #$FF				;
	sta $00					;Prepare to fill nametable with #$FF.
	jsr ClearNameTable		;($C175)Clear selected nametable.
	jmp EraseAllSprites		;($C1A3)Clear sprite data.

;----------------------------------------------------------------------------------------------------

; ===== THE REAL GUTS OF THE GAME ENGINE! =====

UpdateWorld:
	ldx #$00				;Set start of sprite RAM to $0200.
	stx SpritePagePos		;

	jsr UpdateEnemies		;($F345)Display of enemies.
	jsr UpdateProjectiles		;($D4BF)Display of bullets/missiles/bombs.
	jsr UpdateSamus			;($CC0D)Display/movement of Samus.
	jsr AreaRoutine			;($95C3)Area specific routine.
	jsr UpdateElevator		;($D7B3)Display of elevators.
	jsr UpdateStatues		;($D9D4)Display of Ridley & Kraid statues.
	jsr EnemyDestruction    ; destruction of enemies
	jsr UpdateMellowEnemies ; update of Mellow/Memu enemies
	jsr UpdateEnProjectiles
	jsr DestroyGeenSpinner  ; destruction of green spinners
	jsr SamusEnterDoor		;($8B13)Check if Samus entered a door.
	jsr DisplayDoors      	; display of doors
	jsr UpdateTiles 		; tile de/regeneration
	jsr CrashDetection      ; Samus <--> enemies crash detection
	jsr DisplayBar			;($E0C1)Display of status bar.
	jsr UpdatePipeSpawns
	jsr CheckMissileToggle
	jsr UpdateItems 		; display of special items
	jsr UpdateEndTimer

;Clear remaining sprite RAM
	ldx SpritePagePos		;sprite page pos starts at 0
	lda #$F4				;load F4
*   sta Sprite00RAM,x		;store data into sprite ram (F4, F8, FC)
	jsr Xplus4       		;X = X + 4
	bne -
	rts

; SelectSamusPal
; ==============
; Select the proper palette for Samus based on:
; - Is Samus wearing Varia (protective suit)?
; - Is Samus firing missiles or regular bullets?
; - Is Samus with or without suit?

SelectSamusPal:
	tya
	pha
	lda SamusGear
	asl
	asl
	asl							;CF contains Varia status (1 = Samus has it)
	lda MissileToggle			;A = 1 if Samus is firing missiles, else 0
	rol							;Bit 0 of A = 1 if Samus is wearing Varia
	adc #$02
	ldy JustInBailey			;In suit?
	beq +						;Branch if yes
	clc
	adc #$17					;Add #$17 to the pal # to reach "no suit"-palettes
*   sta PalDataPending			;Palette will be written next NMI
	pla
	tay
	rts

;----------------------------------[ Initiate SFX and music routines ]-------------------------------

;Initiate sound effects.

SilenceMusic:				;The sound flags are stored in memory-->
	lda #$01				;starting at $0680. The following is a-->
	bne SFX_SetX0			;list of sound effects played when the-->
							;flags are set:
PauseMusic:					;
	lda #$02   	     		;$0680: These SFX use noise channel.
	bne SFX_SetX0			;Bit 7 - No sound.
							;Bit 6 - ScrewAttack.
SFX_SamusWalk:				;Bit 5 - MissileLaunch.
	lda #$08				;Bit 4 - BombExplode.
	bne SFX_SetX0			;Bit 3 - SamusWalk.
							;Bit 2 - SpitFlame.
SFX_BombExplode:			;Bit 1 - No sound.
	lda #$10				;Bit 0 - No sound.
	bne SFX_SetX0			;
							;$0681: These SFX use sq1 channel.
SFX_MissileLaunch:			;Bit 7 - MissilePickup.
	lda #$20				;Bit 6 - EnergyPickup.
							;Bit 5 - Metal.
SFX_SetX0:					;Bit 4 - BulletFire.
	ldx #$00				;Bit 3 - OutOfHole.
	beq SFX_SetSoundFlag	;Bit 2 - EnemyHit.
							;Bit 1 - SamusJump.
SFX_OutOfHole:				;Bit 0 - WaveFire.
	lda #$08				;
	bne SFX_SetX1			;$0682: Not used.
							;
SFX_BombLaunch:				;$0683: These SFX use tri channel.
	lda #$01				;Bit 7 - SamusDie.
	bne SFX_SetX3			;Bit 6 - DoorOpenClose.
							;Bit 5 - MetroidHit.
SFX_SamusJump:				;Bit 4 - StatueRaise.
	lda #$02				;Bit 3 - Beep.
	bne SFX_SetX1			;Bit 2 - BigEnemyHit.
							;Bit 1 - SamusBall.
SFX_EnemyHit:				;Bit 0 - BombLaunch.
	lda #$04				;
	bne SFX_SetX1			;$0684: These SFX use multi channels.
							;Bit 7 - FadeInMusic		(music).
SFX_BulletFire:				;Bit 6 - PowerUpMusic		(music).
	lda #$10				;Bit 5 - EndMusic  (Page 0 only)(music).
	bne SFX_SetX1			;Bit 4 - IntroMusic(Page 0 only)(music).
							;Bit 3 - not used		(SFX).
SFX_Metal:					;Bit 2 - SamusHit		(SFX).
	lda #$20				;Bit 1 - BossHit		(SFX).
	bne SFX_SetX1			;Bit 0 - IncorrectPassword	(SFX).
							;
SFX_EnergyPickup:			;$0685: Music flags. The music flags start different-->
	lda #$40				;music depending on what memory page is loaded. The-->
	bne SFX_SetX1			;following lists what bits start what music for each-->
							;memory page.
SFX_MissilePickup:			;
	lda #$80				;Page 0: Intro/ending.
							;Bit 7 - Not used.
SFX_SetX1:					;Bit 6 - TourianMusic.
	ldx #$01				;Bit 5 - ItemRoomMusic.
	bne SFX_SetSoundFlag	;Bit 4 - Not used.
							;Bit 3 - Not used.
SFX_WaveFire:				;Bit 2 - Not used.
	lda #$01				;Bit 1 - Not used.
	bne SFX_SetX1			;Bit 0 - Not used.
							;
SFX_ScrewAttack:			;Page 1: Brinstar.
	lda #$40				;Bit 7 - Not used.
	bne SFX_SetX0			;Bit 6 - TourianMusic.
							;Bit 5 - ItemRoomMusic.
SFX_BigEnemyHit:			;Bit 4 - Not used.
	lda #$04				;Bit 3 - Not used.
	bne SFX_SetX3			;Bit 2 - Not used.
							;Bit 1 - Not used.
SFX_MetroidHit:				;Bit 0 - BrinstarMusic.
	lda #$20				;
	bne SFX_SetX3			;Page 2: Norfair.
							;Bit 7 - Not used.
SFX_BossHit:				;Bit 6 - TourianMusic.
	lda #$02				;Bit 5 - ItemRoomMusic.
	bne SFX_SetX4			;Bit 4 - Not used.
							;Bit 3 - NorfairMusic.
SFX_Door:					;Bit 2 - Not used.
	lda #$40				;Bit 1 - Not used.
	bne SFX_SetX3			;Bit 0 - Not used.
							;
SFX_SamusHit:				;Page 3: Tourian.
	lda #$04				;Bit 7 - Not used.
	bne SFX_SetX4			;Bit 6 - TourianMusic
							;Bit 5 - ItemRoomMusic.
SFX_SamusDie:				;Bit 4 - Not used.
	lda #$80				;Bit 3 - Not used.
	bne SFX_SetX3			;Bit 2 - EscapeMusic.
							;Bit 1 - MotherBrainMusic
SFX_SetX2:					;Bit 0 - Not used.
	ldx #$02				;
							;Page 4: Kraid.
SFX_SetSoundFlag:			;Bit 7 - RidleyAreaMusic.
	ora $0680,x				;Bit 6 - TourianMusic.
	sta $0680,x				;Bit 5 - ItemRoomMusic.
	rts						;Bit 4 - KraidAreaMusic.
							;Bit 3 - Not used.
SFX_SamusBall:				;Bit 2 - Not used.
	lda #$02				;Bit 1 - Not used.
	bne SFX_SetX3			;Bit 0 - Not used.
							;
SFX_Beep:					;Page 5: Ridley.
	lda #$08				;Bit 7 - RidleyAreaMusic.
							;Bit 6 - TourianMusic.
SFX_SetX3:					;Bit 5 - ItemRoomMusic.
	ldx #$03				;Bit 4 - KraidAreaMusic.
	bne SFX_SetSoundFlag	;Bit 3 - Not used.
							;Bit 2 - Not used.
;Initiate music				;Bit 1 - Not used.
							;Bit 0 - Not used.
PowerUpMusic:				;
	lda #$40				;
	bne SFX_SetX4			;
							;
IntroMusic:					;
	lda #$80				;
							;
SFX_SetX4:					;
	ldx #$04				;
	bne SFX_SetSoundFlag	;
							;
MotherBrainMusic:			;
	lda #$02				;
	bne SFX_SetX5			;
							;
TourianMusic:				;
	lda #$40				;
							;
SFX_SetX5:					;
	ldx #$05				;
	bne SFX_SetSoundFlag	;

;--------------------------------------[ Update Samus ]----------------------------------------------

UpdateSamus:
	ldx #$00				;Samus data is located at index #$00.
	stx PageIndex			;
	inx						;x=1.
	stx IsSamus				;Indicate Samus is the object being updated.
	jsr GoSamusHandler		;($CC1A)Find proper Samus handler routine.
	dec IsSamus				;Update of Samus complete.
	rts						;

GoSamusHandler:
	lda ObjAction			;
	bmi SamusStand			;Branch if Samus is standing.
	jsr ChooseRoutine		;Goto proper Samus handler routine.

;Pointer table for Samus' action handlers.

	.word SamusStand		;Standing.
	.word SamusRun			;Running.
	.word SamusJump			;Jumping.
	.word SamusRoll			;Rolling.
	.word SamusPntUp		;Pointing up.
	.word SamusDoor			;Inside door while screen scrolling.
	.word SamusJump			;Jumping while pointing up.
	.word SamusDead			;Dead.
	.word SamusDead2		;More dead.
	.word SamusElevator		;Samus on elevator.

;---------------------------------------[ Samus standing ]-------------------------------------------

SamusStand:
	lda Joy1Status				;Status of joypad 1.
	and #$CF					;Remove SELECT & START status bits.
	beq +						;Branch if no buttons pressed.
	jsr ClearHorzMvmtAnimData	;($CF5D)Set no horiontal movement and single frame animation.
	lda Joy1Status				;
*	and #$07					;Keep status of DOWN/LEFT/RIGHT.
	bne +						;Branch if any are pressed.
	lda Joy1Change				;
	and #$08     				;Check if UP was pressed last frame.-->
	beq +++						;If not, branch.
*	jsr BitScan					;($E1E1)Find which directional button is pressed.
	cmp #$02					;Is down pressed?-->
	bcs +						;If so, branch.
	sta SamusDir				;1=left, 0=right.
*	tax							;
	lda ActionTable,x			;Load proper Samus status from table below.
	sta ObjAction				;Save Samus status.
*	lda Joy1Change				;
	ora Joy1Retrig				;Check if fire was just pressed or needs to retrigger.
	asl							;
	bpl +						;Branch if FIRE not pressed.
	jsr FireWeapon				;($D1EE)Shoot left/right.
*	bit Joy1Change				;Check if jump was just pressed.
	bpl +						;Branch if JUMP not pressed.
	lda #sa_Jump				;
	sta ObjAction				;Set Samus status as jumping.
*	lda #$04					;Prepare to set animation delay to 4 frames.
	jsr SetSamusData			;($CD6D)Set Samus control data and animation.
	lda ObjAction				;
	cmp #sa_Door				;Is Samus inside a door, dead or pointing up and jumping?-->
	bcs +						;If so, branch to exit.
	jsr ChooseRoutine			;Select routine below.

;Pointer table to code.

	.word ExitSub				;($C45C)Rts.
	.word SetSamusRun			;($CC98)Samus is running.
	.word SetSamusJump			;($CFC3)Samus is jumping.
	.word SetSamusRoll			;($D0B5)Samus is in a ball.
	.word SetSamusPntUp			;($CF77)Samus is pointing up.

;Table used by above subroutine.

ActionTable:
	.byte sa_Run				;Run right.
	.byte sa_Run				;Run left.
	.byte sa_Roll
	.byte sa_PntUp

;----------------------------------------------------------------------------------------------------

SetSamusExplode:
	lda #$50
	sta SamusJumpDsplcmnt
	lda #an_Explode
	jsr SetSamusAnim
	sta ObjectCounter
*   rts

SetSamusRun:
	lda #$09
	sta WalkSoundDelay
	ldx #$00
	lda AnimResetIndex
	cmp #an_SamusStand
	beq +
	inx
	cmp #$27
	beq +
	lda #$04
	jsr PlayAnimOnSamus
*   lda RunAnimationTbl,x
	sta AnimResetIndex
	ldx SamusDir
SetSamusRunAccel:
	lda RunAccelerationTbl,x
	sta SamusHorzAccel
	rts

RunAnimationTbl:
	.byte an_SamusRun
	.byte an_SamusRunPntUp

RunAccelerationTbl:
	.byte $30								;Accelerate right.
	.byte $D0								;Accelerate left.

; SamusRun
; ========

SamusRun:
	ldx SamusDir							; load samus direction into x
	lda SamusGravity						; load samus gravity into A - we need to check if we move to different states
	beq +++++++								; 	if the gravity is 0, jump way ahead to elevator stuff
	ldy SamusJumpDsplcmnt					; 	else, load jump displacement to y
	bit ObjVertSpeed						; 	bit vert speed
	bmi +									; 	grounded or going up? jump ahead	
	cpy #$18								; 		falling down - check our jump dispalement instead
	bcs ++++								;   	jump dispalcement is >= 0x18, jump ahead
	lda #an_SamusJump						; 			else, we're close to where we jumped from - switch to jump anim 
	sta AnimResetIndex						; 		set jump anim
	bcc ++++	  							; 		branch always, jump to in lava checks
*   cpy #$18								;	going up, check the jump displacement
	bcc +++									;		jump displacement is < 0x18, jump ahead to lava checks 
	lda AnimResetIndex						; 			else, load current anim starting frame index
	cmp #an_SamusFireJump					; 			if it's the fire+jump anim
	beq +									; 			jump ahead
	lda #an_SamusSalto						; 			else, load mid air spin anim
	sta AnimResetIndex						;			and queue it up
*   cpy #$20								; 		check the jump dispalcement again 
	bcc ++									; 		if displacement is < 0x20, jump ahead and load samus run anim 
	lda Joy1Status							; 		
	and #$08
	beq +
	lda #an_SamusJumpPntUp
	sta AnimResetIndex
*   bit Joy1Status
	bmi +
	jsr StopSamusVertMovement					;($D147)
*	lda #an_SamusRun
	cmp AnimResetIndex
	bne +
	lda #an_SamusJump
	sta AnimResetIndex
*   lda SamusInLava							; load samus in lava status
	beq +									; if not, jump ahead
	lda Joy1Change							; else, check jump- we can retrigger in lava
	bmi JumpPressed       					; branch if JUMP pressed
*   jsr UpdateSamusJumpDirection
	jsr UpdateSamusJumpFireAnim
	jsr ReverseHoriAccelOnType04Collision
	lda #$02
	bne SetSamusData       					; branch always
	

*	lda SamusOnElevator						; check if samus is on an elevator
	bne +									; no? jump ahead
	jsr SetSamusRunAccel					; 	else, 
*   jsr UpdateRunAimAnim
	dec WalkSoundDelay  					; time to play walk sound?
	bne +	       							; branch if not
	lda #$09
	sta WalkSoundDelay  					; # of frames till next walk sound trigger
	jsr SFX_SamusWalk
*   jsr ReverseHoriAccelOnType04Collision
	lda Joy1Change
	bpl +	   								; branch if JUMP not pressed
JumpPressed:
	jsr SetSamusJump
	lda #$12
	sta SamusHorzSpeedMax
	jmp Load3FrameAnimDelayThenSetSamusData

*   ora Joy1Retrig
	asl
	bpl +	   								; branch if FIRE not pressed
	jsr FireWeaponWhileRunning
*   lda Joy1Status
	and #$03
	bne +
	jsr StopHorzMovement
	jmp Load3FrameAnimDelayThenSetSamusData

*   jsr BitScan								;($E1E1)
	cmp SamusDir
	beq Load3FrameAnimDelayThenSetSamusData
	sta SamusDir
	jsr SetSamusRun
	
Load3FrameAnimDelayThenSetSamusData:
	lda #$03	;default anim delay time for samus

;---------------------------------------[ Set Samus data ]-------------------------------------------

;The following function sets various animation and control data bytes for Samus.

SetSamusData:
	jsr UpdateObjAnim			;($DC8F)Update animation if needed.
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active to change palette.
	bcs +						;If screw attack not active, branch to skip palette change.
	lda FrameCount				;
	lsr							;
	and #$03					;Every other frame, change Samus palette while screw-->
	ora #$A0					;Attack is active.
	sta ObjectCntrl				;
*	jsr CheckHealthStatus		;($CDFA)Check if Samus hit, blinking or Health low.
	jsr LavaAndMoveCheck		;($E269)Check if Samus is in lava or moving.
	lda MetroidOnSamus			;Is a Metroid stuck to Samus?-->
	beq +						;If not, branch.
	lda #$A1					;Metroid on Samus. Turn Samus blue.
	sta ObjectCntrl				;
*	jsr SetmirrorCntrlBit		;($CD92)Mirror Samus, if necessary.
	jmp DrawFrame				;($DE4A)Display Samus.

;---------------------------------[ Set mirror control bit ]-----------------------------------------

SetmirrorCntrlBit:
	lda SamusDir				;Facing left=#$01, facing right=#$00.
	jsr Amul16					;($C2C5)*16. Move bit 0 to bit 4 position.
	ora ObjectCntrl				;
	sta ObjectCntrl				;Use SamusDir bit to set mirror bit.
	rts							;

;------------------------------[ Check if screw attack is active ]-----------------------------------

IsScrewAttackActive:
	sec							;Assume screw attack is not active.
	ldy ObjAction				;
	dey							;Is Samus running?-->
	bne ++						;If not, branch to exit.
	lda SamusGear				;
	and #gr_SCREWATTACK			;Does Samus have screw attack?-->
	beq ++						;If not, branch to exit.
	lda AnimResetIndex			;
	cmp #an_SamusSalto			;Is Samus summersaulting?-->
	beq +						;If so, branch to clear carry(screw attack active).
	cmp #an_SamusJump			;
	sec							;Is Samus jumping?-->
	bne ++						;If not, branch to exit.
	bit ObjVertSpeed			;If Samus is jumping and still moving upwards, screw--> 
	bpl ++						;attack is active.
*	cmp AnimIndex				;Screw attack will still be active if not spinning, but-->
*	rts							;jumping while running and still moving upwards.

;----------------------------------------------------------------------------------------------------

UpdateRunAimAnim:
	lda Joy1Status						; load controller state
	and #UpButton						; isolate up, 0000 u000
	lsr									; 0u00
	lsr									; 00u0
	lsr									; 000u
	tax									; put u into X
	lda RunAnimationTbl,x				; get anim index for run animation based on if we're pressing up or not
	cmp AnimResetIndex					; 	anim already playing?
	beq -								; 	leave
	jsr SetSamusAnim					; new anim? set anim
	pla									;
	pla									; prep double return
	jmp Load3FrameAnimDelayThenSetSamusData	; set samus animation

; if we fire while running, we'll want to adjust the animation we get from firing the weapon
; we're we're facing left or right, simply overwrite the standing fire anim with the running fire anim
; if we're pointing up, then we bypass setting the standing anim when running and set our fire animation based on the 
; current frame of our running animation... for some reason. seems overly complicated. maybe the feet are just
; more noticable when pointing up?

FireWeaponWhileRunning:
	jsr FireWeapon						; Shoot left/right- will start playing the standing fire animation
	lda Joy1Status						; load controller state
	and #UpButton						; isolate up
	bne +								; 	up pressed? jump ahead
	lda #an_SamusFireRun				; load fire animation
	sta AnimIndex						; jump the anim index to the running fire anim instead
	rts									; return

*   lda AnimIndex						; up was pressed- load current anim index of running anim - since we're running, the animation was not set from FireWeapon
	sec									; set carry
	sbc AnimResetIndex					; subtract the current anim reset index, should be an_SamusRunPntUp (#$37)
	and #$03							; we were on one of the four point up frames, so get an index
	tax									; transfer A to X
	lda FireRunPntUpAnimTable,x			; pick one of the new anim indexes from below
	jmp PlayAnimOnSamus					; play the anim on samus

; Table used by above subroutine

FireRunPntUpAnimTable:	
	.byte an_SamusFireRunPntUp3
	.byte an_SamusFireRunPntUp1
	.byte an_SamusFireRunPntUp2
	.byte an_SamusFireRunPntUp3

CheckHealthStatus:
	lda SamusHit				;
	and #$20					;Has Samus been hit?-->
	beq +++						;If not, branch to check if still blinking from recent hit.
	lda #$32					;
	sta SamusBlink				;Samus has been hit. Set blink for 32 frames.
	lda #$FF
	sta DamagePushDirection
	lda $73
	sta $77
	beq ++
	bpl +
	jsr SFX_SamusHit
*   lda SamusHit
	and #$08
	lsr
	lsr
	lsr
	sta DamagePushDirection
*	lda #$FD
	sta ObjVertSpeed
	lda #$38					;Samus is hit. Store Samus hit gravity.
	sta SamusGravity			;
	jsr IsSamusDead
	bne +
	jmp CheckHealthBeep

*	lda SamusBlink
	beq CheckHealthBeep
	dec SamusBlink
	ldx DamagePushDirection
	inx
	beq +++
	jsr Adiv16       			; / 16
	cmp #$03		
	bcs +
	ldy SamusHorzAccel
	bne +++
	jsr SetHorzMvmntData
*       dex
	bne +
	jsr TwosCompliment			;($C3D4)
*       sta ObjHorzSpeed
*	lda $77
	bpl CheckHealthBeep
	lda FrameCount
	and #$01
	bne CheckHealthBeep
	tay
	sty AnimDelay
	ldy #$F7
	sty AnimFrame

CheckHealthBeep:
	ldy HealthHi
	dey
	bmi +
	bne ++
	lda HealthLo
	cmp #$70
	bcs ++
; health < 17
*       lda FrameCount
	and #$0F
	bne +						;Only beep every 16th frame.
	jsr SFX_Beep
*	lda #$00
	sta SamusHit
	rts

;----------------------------------------[ Is Samus dead ]-------------------------------------------

IsSamusDead:
	lda ObjAction			;
	cmp #sa_Dead			;
	beq Exit3				;Samus is dead. Zero flag is set.
	cmp #sa_Dead2			;
	beq Exit3				;
	cmp #$FF				;Samus not dead. Clear zero flag.

Exit3:  
	rts						;Exit for routines above and below.

;----------------------------------------[ Subtract health ]-----------------------------------------

SubtractHealth:
	lda HealthLoChange		;Check to see if health needs to be changed.-->
	ora HealthHiChange		;If not, branch to exit.
	beq Exit3				;
	jsr IsSamusDead			;($CE84)Check if Samus is already dead.
	beq ClearDamage			;Samus is dead. Branch to clear damage values.
	ldy EndTimerHi			;If end escape timer is running, Samus cannot be hurt.
	iny						;
	beq +					;Branch if end escape timer not active.

ClearDamage:
	jmp ClearHealthChange	;($F323)Clear health change values.

*	lda MotherBrainStatus	;If mother brain is in the process of dying, receive-->
	cmp #$03				;no damage.
	bcs ClearDamage			;

	lda SamusGear			;
	and #gr_VARIA			;Check is Samus has Varia.
	beq +					;
	lsr HealthLoChange		;If Samus has Varia, divide damage by 2.
	lsr HealthHiChange		;
	bcc +					;If HealthHi moved a bit into the carry flag while-->
	lda #$4F				;dividing, add #$4F to HealthLo for proper-->
	adc HealthLoChange		;division results.
	sta HealthLoChange		;

*	lda HealthLo			;Prepare to subtract from HealthLo.
	sta $03					;
	lda HealthLoChange		;Amount to subtract from HealthLo.
	sec						;
	jsr Base10Subtract		;($C3FB)Perform base 10 subtraction.
	sta HealthLo			;Save results.

   lda HealthHi				;Prepare to subtract from HealthHi.
	sta $03					;
	lda HealthHiChange		;Amount to subtract from HealthHi.
	jsr Base10Subtract		;($C3FB)Perform base 10 subtraction.
	sta HealthHi			;Save Results.

	lda HealthLo			;
	and #$F0				;Is Samus health at 0?  If so, branch to-->
	ora HealthHi			;begin death routine.
	beq +					;
	bcs ++					;Samus not dead. Branch to exit.

*	lda #$00				;Samus is dead.
	sta HealthLo			;
	sta HealthHi			;Set health to #$00.
	lda #sa_Dead			;
	sta ObjAction			;Death handler.
	jsr SFX_SamusDie		;($CBE2)Start Samus die SFX.
	jmp SetSamusExplode		;($CC8B)Set Samus exlpode routine.

;----------------------------------------[ Add health ]----------------------------------------------

AddHealth:
	lda HealthLo			;Prepare to add to HealthLo.
	sta $03					;
	lda HealthLoChange		;Amount to add to HealthLo.
	clc						;
	jsr Base10Add			;($C3DA)Perform base 10 addition.
	sta HealthLo			;Save results.

	lda HealthHi			;Prepare to add to HealthHi.
	sta $03					;
	lda HealthHiChange		;Amount to add to HealthHi.
	jsr Base10Add			;($C3DA)Perform base 10 addition.
	sta HealthHi			;Save results.

	lda TankCount			;
	jsr Amul16				;($C2C5)*16. Move tank count to upper 4 bits.
	ora #$0F				;Set lower 4 bits.
	cmp HealthHi			;
	bcs +					;Is life less than max? if so, branch.
	and #$F9				;Life is more than max amount. 
	sta HealthHi			;
	lda #$99				;Set life to max amount.
	sta HealthLo			;
*	jmp ClearHealthChange	;($F323)

;----------------------------------------------------------------------------------------------------

;I'm not quite sure this ever gets used, as I can't seem to find the conditions under which samus can even 
;get #04 or #05 as a possible hit value - only the bullets can even get #04 on door collisions
ReverseHoriAccelOnType04Collision:	; 	
	lda SamusHit				;  .... .x.y 	load samus hit into A
	lsr							;  0... ..x. y	move y into carry
	and #$02					;  0000 00x0 y  flip all but x
	beq +++						;  0000 0000 ?	RTS
	bcs +						;   y  =  1	 ?  negate left movement     
								;   y  =  0  ?  negate right movement
	lda SamusHorzAccel
	bmi +++						;going left, leave
	bpl ++						;going right, jump to negate
*   lda SamusHorzAccel		
	bmi +						;going left, to negate
	bne ++						;going right, leave
*	jsr TwosCompliment			
	sta SamusHorzAccel			;store change in horizontal accell

ClearHorzMvmntData:
  ldy #$00						;
SetHorzMvmntData:
  sty ObjHorzSpeed				;Set Samus Horizontal speed and horizontal-->
	sty HorzCountrLinear			;linear counter to #$00.
*	rts							;

StopHorzMovement:
  lda SamusHorzAccel			;Is Samus moving horizontally?-->
	bne ClearHorzMvmtAnimData	;If so, branch to stop movement.
	jsr SFX_SamusWalk			;($CB96)Play walk SFX.

ClearHorzMvmtAnimData:
  jsr NoHorzMoveNoDelay			;($CF81)Clear horizontal movement and animation delay data.
	sty ObjAction				;Samus is standing.
	lda Joy1Status				;
	and #$08					;Is The up button being pressed?-->
	bne +						;If so, branch.
	lda #an_SamusStand			;Set Samus animation for standing.

SetSamusAnim:
	sta AnimResetIndex			;Set new animation reset index.

PlayAnimOnSamus:
	sta AnimIndex				;Set new animation data index.
	lda #$00					;
	sta AnimDelay				;New animation to take effect immediately.
	rts							;

SetSamusPntUp:
*	lda #sa_PntUp				;
	sta ObjAction				;Samus is pointing up.
	lda #an_SamusPntUp			;
	jsr SetSamusAnim			;($CF6B)Set new animation values.

NoHorzMoveNoDelay:
  jsr ClearHorzData				;($CFB7)Clear all horizontal movement data.
	sty AnimDelay				;Clear animation delay data.
	rts							;

UpdateSamusJumpDirection: 
	lda Joy1Status						; load controller status
	and #$03							; check left and right bits 
	beq +								; 0? left and right not pressed - jump ahead
	jsr BitScan							; 	else, bit scan to find out if it was left or right - A holds the bit index
	tax									; 	move A into x ( should be either 0 = r or 1 = l)
	jsr SetSamusRunAccel				;	set samus run accel to match inoput direction
	lda SamusGravity					; 	load samus gravity
	bmi ++								; 	if it's negative, return
	lda AnimResetIndex					; 		else positive- load reset anim index
	cmp #an_SamusSalto					; 		check summersault anim 
	beq ++								; 			equal? return
	stx SamusDir						; 		else, move direction from x to samus' direction
	lda SamusJumpAnimTable+1,x			; 		pick one of the animations from the table
	jmp SetSamusAnim					; 		set anim, return

*  	lda SamusGravity					; load samus gravity
	bmi +								; if it's negative, return
	beq +								; or if it's zero, return
	lda AnimResetIndex					; load the anim reset index, gravity positive
	cmp #an_SamusJump					; compare to jump
	bne +								; not jumping? return
										; we're jumping, gravity is positive, no hori input- kill horizontal speed
ClearHorzData:						
	jsr ClearHorzMvmntData				;($CF4C)Clear horizontal speed and linear counter.
	sty SamusHorzAccel					;Clear horizontal acceleration data.
*	rts				;

SetSamusJumpPntUp:
	ldy #an_SamusJumpPntUp
	jmp +					

SetSamusJump:
	ldy #an_SamusJump
*   sty AnimResetIndex
	dey
	sty AnimIndex
	lda #$04
	sta AnimDelay
	lda #$00
	sta SamusJumpDsplcmnt
	lda #$FC
	sta ObjVertSpeed
	ldx ObjAction
	dex
	bne +	   ; branch if Samus is standing still
	lda SamusGear
	and #gr_SCREWATTACK
	beq +	   ; branch if Samus doesn't have Screw Attack
	lda #$00
	sta $0686
	jsr SFX_ScrewAttack
*       jsr SFX_SamusJump
  ldy #$18	; gravity (high value -> low jump)
	lda SamusGear
	and #gr_HIGHJUMP
	beq +	   ; branch if Samus doesn't have High Jump
	ldy #$10	; lower gravity value -> high jump!
*       sty SamusGravity
	rts

SamusJump:
	lda SamusJumpDsplcmnt
	bit ObjVertSpeed
	bpl +	   					; branch if falling down
	cmp #$20
	bcc +	   					; branch if jumped less than 32 pixels upwards
	bit Joy1Status
	bmi +	   					; branch if JUMP button still pressed
	jsr StopSamusVertMovement		;($D147)Stop jump (start falling).
*   jsr UpdateSamusJumpAnimAndDirection
	jsr ReverseHoriAccelOnType04Collision
	lda Joy1Status
	and #$08     				; UP pressed?
	beq +	   					; branch if not
	lda #an_SamusJumpPntUp
	sta AnimResetIndex
	lda #sa_PntJump      ; "jumping & pointing up" handler
	sta ObjAction
*       jsr UpdateSamusJumpFireAnim
	lda SamusInLava
	beq +
	lda Joy1Change
	bpl +	   ; branch if JUMP not pressed
	jsr SetSamusJump
	jmp Load3FrameAnimDelayThenSetSamusData

*       lda SamusGravity
	bne ++
	lda ObjAction
	cmp #sa_PntJump
	bne +
	jsr SetSamusPntUp
	bne ++
*       jsr StopHorzMovement
*	lda #$03
	jmp SetSamusData		;($CD6D)Set Samus control data and animation.

UpdateSamusJumpAnimAndDirection:
	ldx #$01						; load 1 into X - will be our horizontal speed
	ldy #$00						; and 0 into Y - will be our direction (0 = right, 1 = left)
	lda Joy1Status					; load controller state
	lsr								; push RIGHT into carry 
	bcs +	   						; 	branch if RIGHT pressed
	dex								; dec X to 0
	lsr								; push LEFT into carry
	bcc ++++       					; branch if LEFT not pressed - no direction pressed, update nothing and set horizontal speed to 0
	dex								; dec X to -1
	iny								; inc Y to 1
*   cpy SamusDir					; comapre Y to current samus Direction
	beq +++							; 	same direction? keep horizontal speed the same and leave
	lda ObjAction					; different direction - load object action
	cmp #sa_PntJump					; check if we're doing a point up jump
	bne +							; 	not doing a point up jump - jump ahead
	lda AnimResetIndex				; 
	cmp SamusJumpPntUpAnimTable,y	; 
	bne ++							; TODO: parse whatever this anim table nonsense is even doing here
	lda SamusJumpPntUpAnimTable+1,y	;
	jmp ++

*   lda AnimResetIndex				; 
	cmp SamusJumpAnimTable,y		; TODO: like, seriously, why?
	bne +							; 
	lda SamusJumpAnimTable+1,y		;
*	jsr SetSamusAnim				; set samus jump anim 
	lda #$08						; jump lasts 8 frames or something
	sta AnimDelay					; set in anim delay
	sty SamusDir					; update samus direction
*	stx ObjHorzSpeed				; 1 if right, 0 if no direction, -1 if left
*   rts

; Table used by above subroutine

SamusJumpAnimTable:
	.byte an_SamusJump
	.byte an_SamusJump
	.byte an_SamusJump
SamusJumpPntUpAnimTable:
	.byte an_SamusJumpPntUp
	.byte an_SamusJumpPntUp
	.byte an_SamusJumpPntUp

UpdateSamusJumpFireAnim:
	lda Joy1Change				; load changes from last frame
	ora Joy1Retrig				; or with retrigger
	asl							; 
	bpl -	   					; exit if FIRE not pressed
	lda AnimResetIndex			; load samus anim 
	cmp #an_SamusJumpPntUp		; compare to pnt up jump
	bne +						; not that? jump ahead
	jmp FireVertical			; 	else, fire vertical and return 
*   jsr FireHorizontal			; fire horizontal
	lda #an_SamusFireJump		; load fire jump anim 
	jmp SetSamusAnim			; set it as the anim for hiring horizontal

SetSamusRoll:
  lda SamusGear
	and #gr_MARUMARI
	beq +	   ; branch if Samus doesn't have Maru Mari
	lda SamusGravity
	bne +

;Turn Samus into ball
	ldx SamusDir
	lda #an_SamusRoll
	sta AnimResetIndex
	lda #an_SamusRunJump
	sta AnimIndex
	lda RunAccelerationTbl,x
	sta SamusHorzAccel
	lda #$01
	sta $0686
	jmp SFX_SamusBall

*   lda #sa_Stand
	sta ObjAction
	rts

; SamusRoll
; =========

	SamusRoll:
	lda Joy1Change
	and #$08     ; UP pressed?
	bne +	   ; branch if yes
	bit Joy1Change	; JUMP pressed?
	bpl ++	  ; branch if no
*       lda Joy1Status
	and #$04	   ; DOWN pressed?
	bne +	  ; branch if yes
;break out of "ball mode"
	lda ObjRadY
	clc
	adc #$08
	sta ObjRadY
	jsr CheckMoveUp
	bcc +	  ; branch if not possible to stand up
	ldx #$00
	jsr MakeObjectLocationStruct98B
	stx $05
	lda #$F5
	sta $04
	jsr CalcPotentialPosition
	jsr SetPositionDataFromStruct98B
	jsr StopHorzMovement
	dec AnimIndex
	jsr StopSamusVertMovement		;($D147)
	lda #$04
	jmp JumpToSetSamusDataFromRoll

*	lda Joy1Change
	jsr BitScan			;($E1E1)
	cmp #$02
	bcs +
	sta SamusDir
	lda #an_SamusRoll
	jsr SetSamusAnim
*   ldx SamusDir
	jsr SetSamusRunAccel
	jsr ReverseHoriAccelOnType04Collision
	jsr CheckBombLaunch
	lda Joy1Status
	and #$03
	bne +
	jsr ClearHorzData
*   lda #$02
JumpToSetSamusDataFromRoll:
  jmp SetSamusData		;($CD6D)Set Samus control data and animation.

StopSamusVertMovement:		
	ldy #$00				; load 0 to y
	sty ObjVertSpeed		; store in vert speed for samus
	sty VertCountrLinear	; reset linear vertical movement counter
	rts						; return, with 0 in y

; CheckBombLaunch
; ===============
; This routine is called only when Samus is rolled into a ball.
; It does the following:
; - Checks if Samus has bombs
; - If so, checks if the FIRE button has been pressed
; - If so, checks if there are any object "slots" available
;   (only 3 bullets/bombs can be active at the same time)
; - If so, a bomb is launched.

	CheckBombLaunch:
	lda SamusGear
	lsr
	bcc ++	  ; exit if Samus doesn't have Bombs
	lda Joy1Change
	ora Joy1Retrig
	asl		; bit 7 = status of FIRE button
	bpl ++	  ; exit if FIRE not pressed
	lda ObjVertSpeed
	ora SamusOnElevator
	bne ++
	ldx #$D0	; try object slot D
	lda ObjAction,x
	beq +	   ; launch bomb if slot available
	ldx #$E0	; try object slot E
	lda ObjAction,x
	beq +	   ; launch bomb if slot available
	ldx #$F0	; try object slot F
	lda ObjAction,x
	bne ++	  ; no bomb slots available, exit
; launch bomb... give it same coords as Samus
*       lda ObjectHi
	sta ObjectHi,x
	lda ObjectX
	sta ObjectX,x
	lda ObjectY
	clc
	adc #$04	; 4 pixels further down than Samus' center
	sta ObjectY,x
	lda #wa_LayBomb
	sta ObjAction,x
	jsr SFX_BombLaunch
*	rts

	SamusPntUp:
	lda Joy1Status
	and #$08     ; UP still pressed?
	bne +	   ; branch if yes
	lda #sa_Stand   ; stand handler
	sta ObjAction
*       lda Joy1Status
	and #$07	; DOWN, LEFT, RIGHT pressed?
	beq ++	  ; branch if no
	jsr BitScan			;($E1E1)
	cmp #$02
	bcs +
	sta SamusDir
*       tax
	lda SamusActionTbl,x
	sta ObjAction
*	lda Joy1Change
	ora Joy1Retrig
	asl
	bpl +	   ; branch if FIRE not pressed
	jsr FireWeapon			;($D1EE)Shoot up.
*       bit Joy1Change
	bpl +	   ; branch if JUMP not pressed
	lda #sa_PntJump
	sta ObjAction
*       lda #$04
	jsr SetSamusData		;($CD6D)Set Samus control data and animation.
	lda ObjAction
	jsr ChooseRoutine

; Pointer table to code

	.word StopHorzMovement
	.word SetSamusRun
	.word ExitSub       ;($C45C)rts
	.word SetSamusRoll
	.word ExitSub       ;($C45C)rts
	.word ExitSub       ;($C45C)rts
	.word SetSamusJumpPntUp
	.word ExitSub       ;($C45C)rts
	.word ExitSub       ;($C45C)rts
	.word ExitSub       ;($C45C)rts

; Table used by above subroutine

SamusActionTbl:
	.byte sa_Run
	.byte sa_Run
	.byte sa_Roll

FireWeapon:
	lda Joy1Status
	and #UpButton
	beq FireHorizontal
	jmp FireVertical

FindAndInitProjectileSlot:
	ldy #$D0
*   lda ObjAction,y
	beq +
	jsr Yplus16
	bne -
	iny
	rts

*   sta $030A,y
	lda MissileToggle
	beq +
	cpy #$D0
*   rts

FireHorizontal:
	lda MetroidOnSamus				; is a metroid on samus?
	bne +							; 	if so, skip bullet fire but play anim
	jsr FindAndInitProjectileSlot	; no metroid- find free anim slot
	bne +							; 	no slot- skip bullet fire but play anim
	jsr InitBullet					; slot found - activate slot and set collision radius
	jsr FireWaveBeam				; try to fire bullet as the wave beam
	jsr FireIceBeam					; try to fire bullet as the ice beam
	lda #$0C						; load 0C
	sta $030F,y						; set as bullet lifetime
	ldx SamusDir					; load samus 
	lda BulletSpeedTbl,x   	  		; get bullet speed
	sta ObjHorzSpeed,y     			; -4 or 4, depending on Samus' direction
	lda #$00						; 
	sta ObjVertSpeed,y				; clear vert speed
	lda #$01						; 
	sta ObjectOnScreen,y			; set object on screen
	jsr CheckMissileLaunch			; set the projectile as a missle if we were on a missle toggle
	lda ObjAction,y					; load object action (which should include projectile type)
	asl								; shift left
	ora SamusDir					; set LSB with samus direction
	and #$03						; get an index out of this 0->4
	tax								; move index into X
	lda HoriBulletSpawnOffsetTbl,x	; wave beam has a different offset than other projectiles			
	sta $05							; store X offset into 5
	lda #$FA						; load a constant Y offset - always the same
	sta $04							; store Y offset into 4
	jsr InitBulletPosition			; init bullet at offset from samus
	lda SamusGear					; load samus gear
	and #gr_LONGBEAM				; isloate long beam  0000 0L00
	lsr								; 
	lsr
	lsr								; move long beam bit to carry 0000 0000 L
	ror								; and then move it to MSB L000 0000
	ora HasBeamSFX					; 
	sta HasBeamSFX					; set long beam flag on HasBeamSFX
	ldx ObjAction,y					; load object action 
	dex								; dec
	bne +							; 	not zero? not a normal bullet, jump ahead
	jsr SFX_BulletFire				; normal bullet- play normal bullet SFX
*   ldy #an_SamusFireStand			; load samus fire anim to Y
PlayFireAnimation:
	tya								; move Y to A
	jmp PlayAnimOnSamus				; play fire animation on samus

HoriBulletSpawnOffsetTbl:
	.byte $0C	; wave beam right facing offset
	.byte $F4	; wave beam left facing offset
	.byte $08	; bullet right facing offset
	.byte $F8	; buttet left facing offset

BulletSpeedTbl:
	.byte $04	; positive speed
	.byte $FC	; negative speed

FireVertical:
    lda MetroidOnSamus				; metroid on samus?
	bne +							; 	if so, skip bullet spawn and just play fire animation
	jsr FindAndInitProjectileSlot	; look for free projectile slot
	bne +							; 	no free slot- skip bullet spawn and play fire anim
	jsr InitBullet					; activate bullet in slot and set collision radius
	jsr FireWaveBeamUp				; attempt to fire wave beam up 
	jsr FireIceBeam					; attemp to fire ice beam
	lda #$0C						; load 0C
	sta $030F,y						; set as bullet lifetime
	lda #$FC						; set -4 upward velocity (we can't point down)
	sta ObjVertSpeed,y				;
	lda #$00						; clear horizontal velocity
	sta ObjHorzSpeed,y				;
	lda #$01						;
	sta ObjectOnScreen,y			; set object as on screen
	jsr CheckMissleLaunchUp			; set projectile as missle if need be
	ldx SamusDir					; load samus direction to X
	lda VertBulletSpawnOffsetTbl,x	; load possible X offset based on fire direction
	sta $05
	lda ObjAction,y					; load object action (will be the type of projectile)
	and #$01						; isolate odd types (everything BUT wave)
	tax								; move A to X
	lda VertBulletSpawnOffsetTbl+2,x; load possible Y offset based on bullet type (wave or other) 
	sta $04							
	jsr InitBulletPosition			; init bullet position 
	lda SamusGear					; load samus gear
	and #gr_LONGBEAM				; isolate long beam
	lsr
	lsr
	lsr
	ror
	ora HasBeamSFX
	sta HasBeamSFX					; set longbeam flag on HasBEamSFX
	lda ObjAction,y					; load bullet action
	cmp #$01						; check if normal bullet
	bne +							; 	not a normal bullet, jump ahead
	jsr SFX_BulletFire				; normal bullet, play normal bullet SFX
*   ldx SamusDir					; load samus direction (0 = right, 1 = left)
	ldy SamusVertFireAnimTbl,x		; load fire animation for direction
	lda SamusGravity				; load samus gravity
	beq +							; 	gravity is 0? on the ground - skip next check
	ldy SamusVertFireAnimTbl+2,x	; gravity is not 0 - get a vertical fire anim instead
*   lda ObjAction					; load samus object action - not sure what these could be
	cmp #$01						; 
	beq +							; 	if samus is action 1 (running), don't play the anim, just return
	jmp PlayFireAnimation			; play our fire animation

; Table used by above subroutine

SamusVertFireAnimTbl:
	.byte an_SamusFirePntUp		; samus point up shooting anim
	.byte an_SamusFirePntUp
	.byte an_SamusFireJumpPntUp	; samus jump and shoot up anim
	.byte an_SamusFireJumpPntUp
	
VertBulletSpawnOffsetTbl:
	.byte $01	; right facing bullet X offset
	.byte $FF	; left facing bullet X offset
	.byte $EC	; wave bullet vertical offset
	.byte $F0	; other bullt vertical offset

InitBullet:
	tya				; move bullet slot index into A
	tax				; move slot index to X
	inc ObjAction,x	; inc bullet action to active
	lda #$02		; load 2
	sta ObjRadY,y	; set the collision radius of the bullet
	sta ObjRadX,y	; 
	lda #an_Bullet	; load bullet anim

SetProjectileAnim:
	sta AnimResetIndex,x	; 
PlayProjectileAnim:
	sta AnimIndex,x
	lda #$00
	sta AnimDelay,x
*   rts

InitBulletPosition:
	ldx #$00							
	jsr MakeObjectLocationStruct98B		; get samus location struct into 9 8 and B
	tya									; put object index into A
	tax									; and then A into X
	jsr CalcPotentialPosition			; calculate new potential position into 9 8 and B (velocity in 4 and 5 is set outside of call)
	txa									; restore object index to Y
	tay									; from X
	jmp SetPositionDataFromStruct98B	; set position for bullet

CheckMissileLaunch:
	lda MissileToggle
	beq Exit4       ; exit if Samus not in "missile fire" mode
	cpy #$D0		; check if it was the first slot
	bne Exit4		; not the first slot? leave
	ldx SamusDir
	lda MissileAnims,x
*   jsr SetBulletAnim
	jsr SFX_MissileLaunch
	lda #wa_Missile	; missile handler
	sta ObjAction,y
	lda #$FF
	sta $030F,y     ; # of frames projectile should last
	dec MissileCount
	bne Exit4       ; exit if not the last missile
; Samus has no more missiles left
	dec MissileToggle       ; put Samus in "regular fire" mode
	jmp SelectSamusPal      ; update Samus' palette to reflect this

MissileAnims:
	.byte an_MissileRight
	.byte an_MissileLeft

CheckMissleLaunchUp:
	lda MissileToggle	; load missle toggle
	beq Exit4			; 	not a missle? leave
	cpy #$D0			; was not a missle, check if the bullet slot matches the first slot
	bne Exit4			; 	not the first slot? leave
	lda #$8F			; we're the first bullet slot, load 8F
	bne -				; jump behind and finish missle launch

SetBulletAnim:
	sta AnimIndex,y
	sta AnimResetIndex,y
	lda #$00
	sta AnimDelay,y
Exit4:  rts

FireWaveBeam:
    lda SamusDir
*   sta $0502,y
	bit SamusGear
	bvc Exit4       ; branch if Samus doesn't have Wave Beam
	lda MissileToggle
	bne Exit4
	lda #$00
	sta $0501,y
	sta $0304,y
	tya				;move the projectile index into A
	jsr Adiv32      ; / 32
	lda #$00		;if the projectile is D0 or F0 (projectile 0 or 2), start the wave table index at 0 
	bcs +			;else
	lda #$0C		;if it's projectile index E0 (projectile 1), start the wave table index at 12- the halfway mark
*   sta $0500,y	
	lda #wa_WaveBeam
	sta ObjAction,y
	lda #an_WaveBeam
	jsr SetBulletAnim
	jmp SFX_WaveFire

FireWaveBeamUp:
	lda #$02			; the UP direction
	bne --				; branch always
	
FireIceBeam:
	lda MissileToggle	; check missile toggle 
	bne Exit4			; 	missles active? leave
	lda SamusGear		; load samus gear flags
	bpl Exit4       	; branch if Samus doesn't have Ice Beam
	lda #wa_IceBeam		; load ice beam action
	sta ObjAction,y		; store that into the object action
	lda HasBeamSFX		; load beam SFX 
	ora #$01			; set the LSB to 1
	sta HasBeamSFX		; store back into has beam FX
	jmp SFX_BulletFire	; play bullet fire SFX

; SamusDoor
; =========

SamusDoor:
	lda SamusDoorStatus
	cmp #$05
	bcc +++++++ ; less than 5 - not the exit status, jump ahead
    ; move Samus out of door, how far depends on initial value of DoorDelay
	dec DoorDelay
	bne MoveOutDoor
    ; done moving
	asl										; pop out MSB of status to carry 
	bcc +									; 	clear? not door entered, jump ahead
	lsr										; door entered, set status back with door entered clear
	sta SamusDoorStatus						; set the door status
	bne +++++++								; should (maybe) always branch
*   jsr DisableMostOffscreenEnemies			; disable half the offscreen enemies, one projectile, and all the spawn pipes in the last room
	jsr DeactivateOffscreenDoors
	jsr TourianRoutine95AB
	lda ItemRoomMusicStatus
	beq ++
	pha
	jsr StartMusic      		; start music
	pla
	bpl ++
	lda #$00
	sta ItemRoomMusicStatus
	beq ++
*   lda #$80
	sta ItemRoomMusicStatus
*   lda KraidRidleyPresent
	beq +
	jsr TourianMusic
	lda #$00
	sta KraidRidleyPresent
	beq --	   					; branch always
*   lda SamusDoorData
	and #$0F
	sta ObjAction
	lda #$00
	sta SamusDoorData
	sta SamusDoorStatus
	jsr StopSamusVertMovement		;($D147)

MoveOutDoor:
	lda SamusDoorDir
	beq ++	  				; branch if door leads to the right
	ldy ObjectX
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*   dec ObjectX
	jmp ++

*	inc ObjectX
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*	jsr CheckHealthStatus	;($CDFA)Check if Samus hit, blinking or Health low.
	jsr SetmirrorCntrlBit
	jmp DrawFrame       	; display Samus

SamusDead:
D41A:	lda #$01
	jmp SetSamusData		;($CD6D)Set Samus control data and animation.

SamusDead2:
	dec AnimDelay
	rts

; SamusElevator
; =============

SamusElevator:
	lda ElevatorStatus		; load current elevator status
	cmp #$03				; compare to elevator move
	beq +					; 	moving? jump ahead
	cmp #$08				; else, compare to OTHER move
	bne +++++++				; 	not the other move? jump WAY ahead and check for the interstitial states instead
*   lda $032F				; load elevator direction
	bmi +++					; 	negative? jump ahead - moving in the opposite direction
	lda ObjectY				; load samus Y position
	sec						; set carry flag
	sbc ScrollY     		; A = Samus' Y position on the visual screen
	cmp #$84				; 
	bcc +	  				; if ScreenY < $84, don't scroll
	jsr ScrollDown  		; otherwise, attempt to scroll
*   ldy ObjectY				; load samus position
	cpy #239				; wrap-around required?
	bne +					; 	no wrap around, skip name table toggle
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
	ldy #$FF				; ObjectY will now be FF
*   iny						; increment object position (0 on a wrap around)
	sty ObjectY				; store new position into samus Y 
	jmp DrawElevatorSamus			; jump to odd frame draw call

*	lda ObjectY				; 
	sec
	sbc ScrollY     		; A = Samus' Y position on the visual screen
	cmp #$64
	bcs +	   				; if ScreenY >= $64, don't scroll
	jsr ScrollUp    		; otherwise, attempt to scroll
*   ldy ObjectY
	bne +	   				; wraparound required? (branch if not)
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
	ldy #240				; ObjectY will now be 239
*   dey
	sty ObjectY
	jmp DrawElevatorSamus

;elevator NOT in the moving state
*	ldy #$00
	sty ObjVertSpeed			; samus not moving
	cmp #$05					; A is elevator status
	beq +						;  if state is 5 or 7, draw samus
	cmp #$07					
	beq +						; else, we're going to gate draws by frame
	
DrawElevatorSamus:
	lda FrameCount				; load frame count
	lsr							; pop out lsb into carry
	bcc ++						; if carry is clear (even frame), leave
*   jsr SetmirrorCntrlBit		; on odd frame- Mirror Samus, if necessary.
	lda #$01					; load 1 as our frame delay for the anim draw call
	jmp AnimDrawObject			; draw samus
*	rts

DisableMostOffscreenEnemies:
	ldx #$60					; load first enprojectile index	
	sec							; set carry 
*   jsr DisableOffscreenEnemy	; disable projectile if offscreen
	txa							; move enemy index to A
	sbc #$20					; subtract two rows
	tax							; move A back into X
	bpl -						; go until we flip (what exactly are we disabling? one projectile and every other enemy?)
	jsr GetNameTable			; Get desired nametable we're adding to
	tay							; place in Y
	ldx #$18					; load 18 to X
*   jsr DisableSpawnerOutOfRoom ; disable any spawner in previous room
	txa
	sec
	sbc #$08
	tax
	bne -						;do last spawner naturally
	
DisableSpawnerOutOfRoom:
	tya							; transfer desired nametable into A
	cmp EnSpawnerNametable,x	; compare with spawner name table
	bne +						; not the same name table, must be in old room
	lda #$FF					; load inactive status 
	sta EnSpawnerStatus,x		; set spawner inactive
*   rts							; return

DisableOffscreenEnemy:
	lda $0405,x			; load enemy status flags
	and #$02			; isolate screen visibility
	bne +				; 	visible? leave
	sta EnStatus,x		; not visible, deactivate enemy
*	rts

; UpdateProjectiles
; =================

UpdateProjectiles:
	ldx #$D0
jsr DoOneProjectile
	ldx #$E0
jsr DoOneProjectile
	ldx #$F0
DoOneProjectile:
	stx PageIndex
	lda ObjAction,x
	jsr ChooseRoutine

	.word ExitSub     		; rts, inactive
	.word UpdateBullet 		; regular beam
	.word UpdateWaveBullet  ; wave beam
	.word UpdateIceBullet   ; ice beam
	.word BulletExplode    	; bullet/missile explode
	.word LayBomb       	; lay bomb
	.word UpdateBomb      	; bomb countdown
	.word ExplodeBomb       ; bomb explode
	.word LayBomb      		; lay bomb
	.word UpdateBomb       	; bomb countdown
	.word ExplodeBomb       ; bomb explode
	.word UpdateBullet  	; missile

UpdateBullet:
	lda #$01
	sta UpdatingProjectile
	jsr DeleteProjectileIfOffscreen
	jsr ExplodeProjectileOnHit
	jsr BulletCrashDetection
CheckBulletStat:
	ldx PageIndex					; load the projectile
	bcc +							; if the carry flag is clear, we collided with a wall
	lda SamusGear					; 	else, check our gear
	and #gr_LONGBEAM				; 	against the long beam
	bne DrawBullet  				; 	skip bullet timer if we have the long beam
	dec ObjectTimer,x     			; 	else, decrement bullet timer
	bne DrawBullet					; 	while the timer isn't 0, keep drawing it
	lda #$00						; 	timer hit 0, kill bullet
	sta ObjAction,x					; 	set the bullet action to 0 - the projectile is now free to overwrite
	beq DrawBullet  				; 	skip to draw the bullet
*   lda ObjAction,x					; if there was wall collision, check the object action
	beq +							; if it's 0, we're done updating
	jsr StartProjectileExplosion	; else, blow it up
DrawBullet:			
	lda #$01						; load 01
	jsr AnimDrawObject				; go draw the object
*   dec UpdatingProjectile			; done updating the projectile
	rts								; return

DoubleIncrementWaveTableIndex:
* 	inc ProjectileWaveTableIndex,x			;increment table index

IncrementWaveTableIndex:
	inc ProjectileWaveTableIndex,x			; increment table index
	lda #$00								; reset timer for next speed
	sta ProjectileWaveSpeedTimer,x			
	beq +	   ; branch always				; go to sample the table again

UpdateWaveBullet:
	lda #$01								; Set UpdatingProjectile to true
	sta UpdatingProjectile					;
	jsr DeleteProjectileIfOffscreen			; remove projectile if off screen
	jsr ExplodeProjectileOnHit				; explode projectile if it's been hit
UpdateWaveBulletSpeed:
	lda ProjectileDirection,x				; get $05X2 - facing direction of projectile
	and #$FE								; and with FE to make left and right = 0 and up = 2
	tay										; trnsfer A into y
	lda WaveDirectionTable,y				; load the pointer to the correct wave table based on our direction
	sta $0A									; and store it into 0A/0B
	lda WaveDirectionTable+1,y				;
	sta $0B									;
*   ldy ProjectileWaveTableIndex ,x			;
	lda ($0A),y								; load the first byte of the byte pair from the table
	cmp #$FF								; if it's not FF, jump ahead 
	bne +									; else, we've got some special work to do
	sta ProjectileWaveTableIndex ,x			; store FF into $05X0- our special data for the projectile
	jmp IncrementWaveTableIndex				; increment the wave table index back to 0 and loop the table

*   cmp ProjectileWaveSpeedTimer,x			; if the timer for this speed is up, 
	beq ---									; then go and double increment the wave table index to the next speed
	inc ProjectileWaveSpeedTimer,x			; else, increment wave speed timer
	iny										; increment y,
	lda ($0A),y								; to get the speed from the table
	jsr ExtractVerticalNibble				; Extract the high nibble for vertical speed this update
	ldx PageIndex							; load the object index
	sta ObjVertSpeed,x						; store the vert speed in the object data for this projectile
	lda ($0A),y								; get the speed again	
	jsr ExtractHorizontalNibble				; extract the low nibble for the horizontal speed this update
	ldx PageIndex							; load the object index
	sta ObjHorzSpeed,x						; store the hori speed in the object data for this projectile
	tay										; pass the horizontal speed to Y
	lda ProjectileDirection,x				; load the direction of the projectile
	lsr										; shift right - if it's (left? right? whatever has bit 0 as 1)
	bcc +									; if the bit was set, skip over to collision detection
	tya										; else, take the horizontal speed back out of y
	jsr TwosCompliment						; flip the horizontal speed, if we're facing the other direction
	sta ObjHorzSpeed,x						; and store it back in the object
	
UpdateWaveBulletCollision:
*   jsr BulletCrashDetection
	bcs +									; no collision? no problem
	jsr MoveObjectByVelocity				; if there WAS collision- fuck it, move anyway, WAVE BEAM!!
*   jmp CheckBulletStat		

WaveDirectionTable:
	.word HorizontalWaveTable     			; pointer to table #1 below
	.word VerticalWaveTable     			; pointer to table #2 below

; Table #1 (size: 25 bytes)
; wave table is made of byte pairs
; byte 1: 
;  the amount of frames to hold a speed, table terminator if FF
; byte 2:
;  vertical and horizontal speed, stored in a single byte of two nibbles
; the speed is clamped btween 7 and -7 speed, since it's stored in two nibbles and can include neagatives
; format:
;
; VVVV HHHH
; |||| ||||
; |||| ++++-- Horizontal Speed, a 1 in the highest bit means a literal negative (ex 1111 = -7)
; ||||
; ++++------- Vertical Speed, a 1 in the highest bit means a literal negative (ex 1111 = -7)
;
;  negative vertical speed goes up

HorizontalWaveTable:
	.byte $01	;			 D         x
	.byte $F3	;-7, 3		 C      x     x
	.byte $01	;			 B
	.byte $D3	;-5, 3		 A
	.byte $01	;			 9
	.byte $93	;-1, 3		 8
	.byte $01	;			 7   x           x                      
	.byte $13	; 1, 3		 6
	.byte $01	;			 5 
	.byte $53	; 5, 3		 4 
	.byte $01	;			 3 
	.byte $73	; 7, 3   	 2 
  	.byte $01	;			 1 
	.byte $73	; 7, 3		 0x                 x                 x
	.byte $01	;		    -1
	.byte $53	; 5, 3		-2
	.byte $01	;			-3
	.byte $13	; 1, 3		-4
	.byte $01	;			-5
	.byte $93	;-1, 3		-6
	.byte $01	;			-7                     x           x
	.byte $D3	;-5, 3		-8
	.byte $01	;			-9
	.byte $F3	;-7, 3		-A
	.byte $FF	;			-B
	;						-C					      x     x  
	;						-D						     x	
; Table #2 (size: 25 bytes)

VerticalWaveTable:
	.byte $01	; 
	.byte $B7	;-3, 7
	.byte $01	; 
	.byte $B5	;-3, 5
	.byte $01	;
	.byte $B1	;-3, 1
	.byte $01	;
	.byte $B9	;-3,-1
	.byte $01	;
	.byte $BD	;-3,-5
	.byte $01	;
	.byte $BF	;-3,-7
	.byte $01	;
	.byte $BF	;-3,-7
	.byte $01	;
	.byte $BD	;-3,-5
	.byte $01	;
	.byte $B9	;-3,-1
	.byte $01	;
	.byte $B1	;-3, 1
	.byte $01	;
	.byte $B5	;-3, 5
	.byte $01	;
	.byte $B7	;-3, 7
	.byte $FF	;
	
; UpdateIceBullet
; ===============

	UpdateIceBullet:
	lda #$81
	sta ObjectCntrl
	bit SamusGear
	bvs +
	jmp UpdateBullet
*	jmp UpdateWaveBullet

; BulletExplode
; =============
; bullet/missile explode

	BulletExplode:
	lda #$01
	sta UpdatingProjectile
	lda $0303,x
	sec
	sbc #$F7
	bne +
	sta ObjAction,x	 ; kill bullet
*   jmp DrawBullet

ExplodeProjectileOnHit:
	lda ObjectHit,x
	beq Exit5
	lda #$00
	sta ObjectHit,x
StartProjectileExplosion:
	lda #$1D
	ldy ObjAction,x
	cpy #wa_BulletExplode
	beq Exit5
	cpy #wa_Missile
	bne +
	lda #an_MissileExplode
*       jsr SetProjectileAnim
	lda #wa_BulletExplode
*       sta ObjAction,x
Exit5:  rts

DeleteProjectileIfOffscreen:
	lda ObjectOnScreen,x
	lsr
	bcs Exit5				; if the object is on screen, leave early 		
*	lda #$00				;
	beq --	 				; else, set the object action to 0 and free the projectile slot by jumping to the code above
	
*   jmp ProcessObjectDoorCrash	; process door collision

; bullet <--> background crash detection

BulletCrashDetection:
	jsr GetObjCoords23B45
	ldy #$00
	lda ($04),y    						; get tile # that bullet touches IN WRAM (not the actual nametable)
	cmp #$A0							; compare with tile index A0
	bcs MoveObjectByVelocity			; if the tile index is >= than A0 - no tile collision, move bullet
	jsr MotherBrainCollision 					; jump to 95C0 - will return immediately if not in Tourian
	cmp #$4E							; if it's tile 4E - the door tile
	beq -								; jump up to E81E - obj hori speed in 5, obj vert speed in 4
	jsr SetCarryIfCollisionTile			; else check for generic collision
	bcc ++								; 	if tile index is < 80 (and less than 70 outside brinstar), return
	clc									; clear the carry flag if it wasn't 
	jmp BulletIsBlastTile						; jump to blast tile (will return immediately if we got here not updating a projectile)

MoveObjectByVelocity:
	ldx PageIndex						; get bullet index
	lda ObjHorzSpeed,x					; get horizontal speed of object
	sta $05								;
	lda ObjVertSpeed,x					;
	sta $04								; cache bullet velocity into 05 (hori) and 04 (vert)
	jsr MakeObjectLocationStruct98B		; create a location struct in 8 (obj Y), 9(obj X), and B(obj nt)
	jsr CalcPotentialPosition			; make new potential position for projectile
	bcc --								; potential positioning failed, clear bullet
	
SetPositionDataFromStruct98B:
	lda $08								; copy from pos sturct to object y
	sta ObjectY,x
	lda $09								; copy from pos struct to object x
	sta ObjectX,x
	lda $0B
	and #$01							; truncate nametable value
	bpl +	   ; branch always			
ToggleObjectHi:
	lda ObjectHi,x						; load object high
	eor #$01							; flip it
*   sta ObjectHi,x						; store object name table
*	rts									; return 

SetCarryIfCollisionTile:
	ldy InArea
	cpy #$10	;check if we're in brinstar
	beq +		;if we're in brinstar, skip to comapre with 80
	cmp #$70	;check tile index against 70
	bcs ++		;if it's >= to 70, leave with set carry
*   cmp #$80	;check tile indeax against 80, deal with outcome outside of method
*	rts

LayBomb:
	lda #an_BombTick
	jsr SetProjectileAnim
	lda #$18	; fuse length :-)
	sta $030F,x
	inc ObjAction,x       ; bomb update handler
	DrawBomb:
	lda #$03
	jmp AnimDrawObject

UpdateBomb:
	lda FrameCount
	lsr
	bcc ++	  ; only update counter on odd frames
	dec $030F,x
	bne ++
	lda #$37
	ldy ObjAction,x
	cpy #$09
	bne +
	lda #an_BombExplode
*   jsr SetProjectileAnim
	inc ObjAction,x
	jsr SFX_BombExplode
*	jmp DrawBomb

;; checks a direction each frame to explode 
;; 030F = 1 : check tile at bomb location
;; 030F = 2 : check tile above bomb location
;; 030F = 3 : check tile below bomb location
;; 030F = 4 : check tile to left of bomb location
;; 030F = 5 : check tile to right of bomb location
;; 030F = anything else - exit early
ExplodeBomb:			
	inc $030F,x		 			;was a fuse, now we inc up the other way as a tile direction step
	jsr LookForExplodableTiles	
	ldx PageIndex				; load bomb index
	lda $0303,x					; load 
	sec							; 
	sbc #$F7					; 
	bne +				
	sta ObjAction,x     		; kill bomb
*   jmp DrawBomb

;;when we're blowing the bomb up, look for the META tiles around it to see what should explode
;;we keep tra
LookForExplodableTiles:
	jsr GetObjCoords23B45				; get bomb coordinate into 2/3, with tile position in 4/5
	lda $04								; cache off tile position lo byte in A
	sta $0A								;
	lda $05								; cache off tile position high byte in B
	sta $0B								;	
	ldx PageIndex						; load bomb index 
	ldy $030F,x							; load direction to process
	dey									; decrement 
	beq ++								; 	0? jump ahead to check tile detection at bomb location
	dey									; dec it again
	bne +++								; 	not zero? jump to other directions
	lda #$40							; hit zero on the second dec - process tile above us
	jsr RegressUpScreenTiles45byA		; 0110 0nyy yyyx xxxx - look two tiles up into same location on above meta tile
	txa									; put bomb index into A
	bne +								; 	not 0? jump ahead 
	lda $04								; 0 - load updated tile position lo 
	and #$20							; isolate odd (bottom of meta tile) or even (top of meta tile)
	beq Exit6							; 	even row? leave- only check for tile bottoms
*   lda $05								; load high byte of tile position
	and #$03							; isolate highest Y pos bits ( which quarter of the screen vertically? )
	cmp #$03							; check against bottom quater
	bne +								; 	not in the bottom quarter of the screen - just go to explosion check
	lda $04								; load the lo tile position byte again
	cmp #$C0							; check against the clip off area (we wrapped from above, probably)
	bcc +								; less than that, we're on a screen meta tile
	lda ScrollDir						; load scroll direction
	and #$02							; isolate horizontal scrolling bit
	bne Exit6							; 	horizontal scrolling? leave
	lda #$80							; look an extra meta tile up instead
	jsr RegressUpScreenTiles45byA		; 
*	jsr ExplosionTileDetection			; 

Exit6:  
	rts
	
LookforExplodableTilesBelow:
*	dey									; dec y again
	bne +++								; 	not 0 - jump ahead to other direction
	lda #$40							; y hit zero on third dec - check tile below bomb
	jsr ProgressDownScreenTiles45byA
	txa
	bne +
	lda $04
	and #$20
	bne Exit6
*   lda $05
	and #$03
	cmp #$03
	bne +
	lda $04
	cmp #$C0
	bcc +
	lda ScrollDir
	and #$02
	bne Exit6
	lda #$80
	jsr ProgressDownScreenTiles45byA
*   jmp ExplosionTileDetection

LookForExplodableTilesLeft:
*	dey								; dec y again
	bne +++							; 	not 0- jump to see if we check tile to right
	lda #$02						; hit 0 on 4th dec - check tile to the left
	jsr RegressUpScreenTiles45byA	; 
	txa
	bne +
	lda $04
	lsr
	bcc Exit7
*   lda $04
	and #$1F
	cmp #$1E
	bcc +
	lda ScrollDir
	and #$02
	beq Exit7
	lda #$1E
	jsr ProgressDownScreenTiles45byA
	lda $05
	eor #$04
	sta $05
*   jmp ExplosionTileDetection

LookForExplodableTilesRight:
*	dey									; dec y
	bne Exit7							; somehow not 0, leave
	lda #$02
	jsr ProgressDownScreenTiles45byA
	txa
	bne +
	lda $04
	lsr
	bcs Exit7
*   lda $04
	and #$1F
	cmp #$02
	bcs ExplosionTileDetection
	lda ScrollDir
	and #$02
	beq Exit7
	lda #$1E
	jsr RegressUpScreenTiles45byA
	lda $05
	eor #$04
	sta $05
	
ExplosionTileDetection:
	txa							; move bome index into A
	pha							; put A on stack
	ldy #$00					; load 0 into Y
	lda ($04),y					; get tile at location
	jsr SetCarryIfCollisionTile	; check if collision tile at location
	bcc +						; 	carry clear? put the bomb index back to A and return
	cmp #$A0					; was a collision tile- check against A0
	bcs +						; 	>=? put bomb index back into A and return 
	jsr IsBlastTile				; less than A0, check for blast tile
*   pla							; return bomb index to A
	tax							; and then to X
Exit7:  
	rts							; return 

ProgressDownScreenTiles45byA:
	clc						; clear carry 
	adc $0A					; add the tile position lo byte 
	sta $04					; store back to 4
	lda $0B					; carry over to the high byte
	adc #$00				; and add
	jmp CleanUpTileHiByte	; jump down to store to B

RegressUpScreenTiles45byA:
	sta $00		; cache A into 0
	lda $0A		; load cached tile position lo byte
	sec			; set carry
	sbc $00		; subtract the value from 0
	sta $04		; store in original tile position lo byte cache
	lda $0B		; load cached tile position hi byte
	sbc #$00	; carry the subtraction 
	
CleanUpTileHiByte:
	and #$07	; clear any garbage data on the high byte from the subtraction
	ora #$60	; turn on the 6 again
	sta $05		; store back
*   rts

GetObjCoords23B45:
	ldx PageIndex
	lda ObjectY,x
	sta $02
	lda ObjectX,x
	sta $03
	lda ObjectHi,x
	sta $0B
	jmp MakeWRAMPtr

UpdateElevator:
	ldx #$20
	stx PageIndex
	lda ObjAction,x ;equivalent to ElevatorStatus
	jsr ChooseRoutine

; Pointer table to elevator handlers

	.word ExitSub       			;($C45C) rts
	.word ElevatorIdle
	.word ElevatorCorrectScrollX
	.word ElevatorMove
	.word ElevatorScroll
	.word ElevatorPrepBetweenAreas
	.word ElevatorAreaSwitch		; elevator reached new area
	.word ElevatorPrepBetweenAreas
	.word ElevatorMove
	.word ElevatorStop

ElevatorIdle:
	lda SamusOnElevator			; load flag to see if samus is on the elevator
	beq ShowElevator			; 	not on it? jsut idly animate
	lda #DownButton				; else, load down button mask
	bit ElevatorDirection   	; elevator direction in bit 7 (1 = up)
	bpl +						; 	not up, keep down mask	 
	asl							; 	going up, get up button mask instead
*   and Joy1Status				; isolate button on controller status
	beq ShowElevator			; 	neither button set? just animate
								; start elevator!
	jsr StopSamusVertMovement	; clear samus vertical movement, 0 in Y
	sty AnimDelay				; set samus anim delay to 0
	sty SamusGravity			; set samus gravity to 0
	tya							; move 0 into A
	sta ObjVertSpeed,x			; clear vert speed of elevator
	inc ObjAction,x				; increment elevator to scroll X correction state
	lda #sa_Elevator			; 
	sta ObjAction				; set samus action to elevator
	lda #an_SamusFront			;
	jsr SetSamusAnim			; set samus to front facing anim
	lda #128					;
	sta ObjectX     			; place samus at room hori center
	lda #112					;
	sta ObjectY     			; place samus at room vert center

ShowElevator:					; dispaly elevator on odd frames
	lda FrameCount				;
	lsr							;
	bcc --	  					; only display elevator at odd frames
	jmp DrawFrame       		; display elevator

 
ElevatorCorrectScrollX:
	lda ScrollX				; load X scroll 
	bne +					; 	not 0 yet? jump ahead
	lda MirrorCntrl			; scroll was 0 - load Mirror Control
	ora #$08				; set the MSB of mirror control
	sta MirrorCntrl			; store new mirror control
	lda ScrollDir			; load scroll direction
	and #$01				; remove horizontal scroll
	sta ScrollDir			; store new scroll direction
	inc ObjAction,x			; inc to moving 
	jmp ShowElevator		; draw elevator

;; the scroll wasn't 0, so attempt to fix that before moving the elevator - in case there's an elevator mid room
*   lda #$80				; load #$80 - position 128
	sta ObjectX				; store that in samus X position
	lda ObjectX,x			; load the elevator X position
	sec						; set carry
	sbc ScrollX				; subtract the X scroll
	bmi +					; 	negative? scroll right
	jsr ScrollLeft			; positive- scroll screen left
	jmp ShowElevator		; draw elevator

*   jsr ScrollRight			; negative difference- scroll screen right instead
	jmp ShowElevator		; draw elevator

ElevatorMove:
	lda ElevatorDirectionNoOffset,x		; use X for elevator offset- could probably change this to a direct check at 030F since there's only one elevator
	bpl ++	  							; 	positive - branch if elevator going down
		
	; move elevator up one pixel
	ldy ObjectY,x						; direction was negative- going up. load elevator Y pos
	bne +								; 	if it's not 0, jump ahead
	jsr ToggleObjectHi					; elevator position is 0, we hit the top of the screen- toggle our name table
	ldy #240							; load bottom of room position
*   dey									; move Ypos up one pixel
	tya									; transfer new position to A
	sta ObjectY,x						; store new object position
	jmp ++								; jump to check for scroll

    ; move elevator down one pixel
*	inc ObjectY,x						; increment elevator position, moving it down one
	lda ObjectY,x						; load the position
	cmp #240							; if we hit 240, the bottom Y position for a room, we need to switch rooms
	bne +								; 	not at the bottom of the room yet- jump ahead
	jsr ToggleObjectHi					; at the bottom of the room- switch nametable
	lda #$00							; load 0
	sta ObjectY,x						; store as new object room position

;switch to screen scroll 
*	cmp #131							; below screen center
	bne +	   							; 	if we haven't hit there, just update anims
	inc ObjAction,x						; 	we hit the location - start screen scroll state
*   jmp ShowElevator					; animate elevator

;;includes some vestigial code for handling a special fade out animation
ElevatorScroll:
	lda ScrollY
	bne ElevScrollRoom  ; scroll until ScrollY = 0
	
;;fade out anim before FDS disk loading
	lda #an_SamElevDisLoop
	sta AnimResetIndex
	lda #an_SamusElevDisappear
	sta AnimIndex
	lda #an_ElevDisLoop
	sta AnimResetIndex,x
	lda #an_ElevDisappear
	sta AnimIndex,x
	inc ObjAction,x
	lda #$40
	sta Timer1
	
	jmp ShowElevator

ElevScrollRoom:
	lda ElevatorDirectionNoOffset ,x	; load elevator direction
	bpl +	   							; 	positive- branch if elevator going down
	jsr ScrollUp						; negative, screen going up, scroll it up
	jmp ShowElevator					; animate elevator
										
*   jsr ScrollDown						; scroll screen down
	jmp ShowElevator					; animate elevator

ElevatorPrepBetweenAreas:

;;  bring these two lines back if you want to see the pre-disk load FDS anim
	;lda Timer1
	;bne +
	inc ObjAction,x		; inc elevator action
	lda ObjAction,x		; load elevator action
	cmp #$08			; second ElevatorMove state after area switch
	bne +				; 	not move? jump to just draw
	lda #af_Elevator		; else, move is up next  - load 23, the anim frame for the elevator
	sta AnimFrame,x			; store 23 in elevator 3rd byte sets
	lda #an_SamusFront	; load samus front facign anim
	jsr SetSamusAnim	; set samus to front facing anim
	jmp ShowElevator	; attempt elevator draw

*   lda #$01			; load 1 for anim frame delay
	jmp AnimDrawObject	; draw elevator

ElevatorAreaSwitch:
	lda $030F,x			; load elevator something or other
	tay					; move to Y
	cmp #$8F			; Leads-To-Ending elevator?
	bne +				; if not, jump ahead
	
; Samus made it! YAY! run ending routine
	lda #$07			
	sta MainRoutine
	inc AtEnding
	ldy #$00
	sty $33
	iny
	sty SwitchPending   ; switch to bank 0
	lda #$1D	; ending
	sta TitleRoutine
	rts

*   tya								; put Y back into A
	bpl ++							; positive number? jump ahead
	ldy #$00						; else, load 0 into Y
	cmp #$84						; check against 0x84 instead
	bne +							; not equal? jump ahead
	iny								; else, increment y 
*   tya								; move Y into A
*	ora #$10						; set the last bit 
	jsr SwitchBankIfEngineRunning	; A has the bank we want to load... somehow
	lda PalToggle					; load pallete toggle (01 / 06)
	eor #$07						; toggle it			  (06 / 01)
	sta PalToggle					; store back in palette toggle
	ldy InArea						; load area index into Y
	cpy #$12						; compare with Kraid
	bcc +							; less than kraid (brinstar, norfair), jump ahead
	lda #$01						;	kraid, tourian or ridley - load 1 , completely ignoring the 06 value of the toggle
*   sta PalDataPending				; store current toggle into palette data pending
	jsr WaitNMIPass_				; wait for NMI
	jsr SelectSamusPal				; select Samus Pal
	jsr StartMusic					; start new music
	jsr ScreenOn					; turn on screen 
	jsr CopyPtrs					; copy area pointers to RAM
	jsr DestroyEnemies				; clear out enemies
	
;;fade in anim after FDS disk loading
	ldx #$20						; load elevator offset into X
	stx PageIndex					; store into page index
	lda #an_SamElevAppLoop			; 
	sta AnimResetIndex				;	
	lda #an_SamusElevAppear			;
	sta AnimIndex					;
	lda #an_ElevAppLoop				; 
	sta AnimResetIndex,x			;
	lda #an_ElevAppear				;
	sta AnimIndex,x					;
	inc ObjAction,x					; inc object action for elevator
	lda #$40						; load 0x40 frames into Timer
	sta Timer1						;
	rts								; return

StartMusic:
	lda ElevatorStatus			; load the elevator status
	cmp #$06					; if we're elevator step 6, we'll check the elevator direction to set the music
	bne +						; else, we'll jump ahead to start either the item room or level music depending on item room status
	lda $032F					; 	Load elevator direction (bit 7 - 1 = up)
	bmi ++						; 	elevator going up? jump to setting up item room music
*   lda AreaMusicFlag			; else, just Load proper bit flag for area music.
	ldy ItemRoomMusicStatus		; load item room music status into y
	bmi ++						; bit seven of Item room music status is set? jump to set up area music
	beq ++						; item room music status is 0? jump to set up area music - else, set up item room music... for some reason
*	lda #$81					; 	load 81 to item room music status, start item room music
	sta ItemRoomMusicStatus		; 	store into item room music status
	lda #$20					; Set flag to play item room music.

*	ora MusicInitFlag			;
	sta MusicInitFlag			; Store music flag info.
	rts							;

ElevatorStop:
	lda ScrollY
	bne ++	  ; scroll until ScrollY = 0
	lda #sa_Stand
	sta ObjAction
	jsr StopHorzMovement
	ldx PageIndex   ; #$20
	lda #$01	; ElevatorIdle
	sta ObjAction,x
	lda $030F,x
	eor #$80	; switch elevator direction
	sta $030F,x
	bmi +
	jsr ToggleScroll
	sta MirrorCntrl
*   jmp ShowElevator
*	jmp ElevScrollRoom

SamusOnElevatorOrEnemy:
	lda #$00									;
	sta SamusOnElevator							; Assume Samus is not on an elevator or on a frozen enemy.
	sta OnFrozenEnemy							;
	tay											; put 0 into Y - samus object index
	ldx #$50									; start ith enemy 1 and go down the 6 possible enemies
	jsr CreateObjYLocationStruct68PPUdiffA	; create position struct for samus
*   lda EnStatus,x								; load enemy status
	cmp #$04									; check enemy is frozen		
	bne +										; 	not frozen? jump ahead	
	jsr CreateEnemyLocationStruct79PPUdiffB		; enemy frozen, create enemy position struct
	jsr AddAndCacheEnObjRadYX45					; add enemy and player radius
	jsr CheckHitboxOverlap						; figure out of the hitboxes are overlapping
	bcs +										; 	failed, since carry was set- check next enemy instead 
	jsr CheckVerticalPenetration				; check if we're too deep inside the hitbox
	bne +										; 	penetrating, hit generated - try next enemy
SetSamusOnFrozenEnemy:	
	inc OnFrozenEnemy					; Samus is standing on a frozen enemy.
	bne ++								;	branch always to check elevator as well - can be on both!
*   jsr Xminus16						; loop back to check next ememy
	bpl --								;
	
CheckSamusOnElevator:
*	lda ElevatorStatus					; check on elevator
	beq +								; inactive, leave 
	ldy #$00							; else, load 0 to y
	ldx #$20							; load elevator object index offset 
	jsr Check68AYObjXHBOverlap			; create position struct for elevator, add collision rads with player, and jump to F1FA
	bcs +								; 	carry set, check failed- not on elevator
	jsr CheckVerticalPenetration		; check if we're inside the elevator sprite
	bne +								; 	penetrating, hit generated - leave
	inc SamusOnElevator					; Samus is standing on elevator.
*   rts

CheckVerticalPenetration:	; TODO: figure out the nitty gritty
	lda $10					; not sure what this was, but we fudge with it in F1FA
	and #$02				; isolate .... ..x.
	bne +					; 	if it's a scroll direction - jump ahead if horizontal
	ldy $11					; vert scroll - load $11, also fudged in F1FA
	iny						; not sure what this is either
	cpy $04					; compare to combined Y rad
	beq Exit8				; 	same as the combined radius? leave with success - else, penetrating, need to generate hit
*   lda SamusHit			; load samus hit
	and #$38				; isloate ..xx x...
	ora $10					; or with value from $10
	ora #$40				; make sure .1.. .... is set, ensuring this fails 
	sta SamusHit			; store in SamusHit
	
Exit8:  
	rts

; UpdateStatues
; =============

UpdateStatues:
	lda #$60		; load 60
	sta PageIndex	; store into page index- first projectile slot
	ldy $0360		; 
	beq Exit8	   	; exit if no statue present
	dey				; 
	bne +
	jsr UnknownDAB0
	ldy #$01
	jsr UnknownDAB0
	bcs +
	inc $0360
*   ldy $0360
	cpy #$02
	bne +++
	lda KraidStatueStatus
	bpl +
	ldy #$02
	jsr UnknownDAB0
*   lda RidleyStatueStatus
	bpl +
	ldy #$03
	jsr UnknownDAB0
*   bcs +
	inc $0360
*	ldx #$60
	jsr UnknownDA1A
	ldx #$61
	jsr UnknownDA1A
	jmp UnknownDADA
	
UnknownDA1A:
	jsr UnknownDA3D
	jsr UnknownDA7C
	txa
	and #$01
	tay
	lda TableDA3B,y
	sta $0363
	lda $681B,x
	beq +
	bmi +
	lda FrameCount
	lsr
	bcc ++	  ; only display statue at odd frames
*       jmp DrawFrame       ; display statue

TableDA39:
	.byte $88
	.byte $68
TableDA3B:
	.byte $65
	.byte $66

UnknownDA3D:
	lda $0304,x
	bmi +
	lda #$01
	sta $0304,x
	lda $030F,x
	and #$0F
	beq +
	inc $0304,x
	dec $030F,x
	lda $030F,x
	and #$0F
	bne +
	lda $0304,x
	ora #$80
	sta $0304,x
	sta $681B,x
	inc $0304,x
	txa
	pha
	and #$01
	pha
	tay
	jsr UnknownDAB0
	pla
	tay
	iny
	iny
	jsr UnknownDAB0
	pla
	tax
*	rts

UnknownDA7C:
	lda $030F,x
	sta $036D
	txa
	and #$01
	tay
	lda TableDA39,y
	sta $036E
	lda $681B,x
	beq +
	bmi +
	lda $0304,x
	cmp #$01
	bne +
	lda $0306,x
	beq +
	dec $030F,x
	lda $0683
	ora #$10
	sta $0683
*   lda #$00
	sta $0306,x
	rts

UnknownDAB0:
	lda Table0E,y
	sta $05C8
	lda $036C
	asl
	asl
	ora Table1B,y
	sta $05C9
	lda #$09
	sta $05C3
	lda #$C0
	sta PageIndex
	jsr DrawTileBlast
	lda #$60
	sta PageIndex
	rts

; Table used by above subroutine

Table0E:
	.byte $30
	.byte $AC
	.byte $F0
	.byte $6C
Table1B:
	.byte $61
	.byte $60
	.byte $60
	.byte $60

UnknownDADA:
	lda $54
	bmi Exit0
	lda SamusDoorStatus
	bne Exit0
	lda KraidStatueStatus
	and RidleyStatueStatus
	bpl Exit0
	sta $54
	ldx #$70
	ldy #$08
*   lda #$03
	sta $0500,x
	tya
	asl
	sta $0507,x
	lda #$04
	sta TileType,x
	lda $036C
	asl
	asl
	ora #$62
	sta TileWRAMHi,x
	tya
	asl
	adc #$08
	sta TileWRAMLo,x
	jsr Xminus16
	dey
	bne -
	
Exit0:  
	rts

; CheckMissileToggle
; ==================
; Toggles between bullets/missiles (if Samus has any missiles).

	CheckMissileToggle:
	lda MissileCount
	beq Exit0       ; exit if Samus has no missiles
	lda Joy1Change
	ora Joy1Retrig
	and #$20	
	beq Exit0       ; exit if SELECT not pressed
	lda MissileToggle
	eor #$01	; 0 = fire bullets, 1 = fire missiles
	sta MissileToggle
	jmp SelectSamusPal

; MakeBitMask
; ===========
; in: Y = bit index
; out: A = bit Y set, other 7 bits zero

	MakeBitMask:
	sec
	lda #$00
*       rol
	dey
	bpl -
*       rts

; UpdateItems
; ===========

UpdateItems:
	lda #$40
	sta PageIndex
	ldx #$00			;Check first item slot.
	jsr CheckOneItem		;($DB42)Check current item slot.
	ldx #$08			;Check second item slot.

CheckOneItem:
	stx ItemIndex			;First or second item slot index(#$00 or #$08).
	ldy PowerUpType,x		;
	iny				;Is no item present in item slot(#$FF)?-->
	beq -				;If so, branch to exit.

	lda PowerUpYCoord,x
	sta $034D
	lda PowerUpXCoord,x
	sta $034E
	lda PowerUpNameTable,x
	sta $034C
	jsr GetObjCoords23B45
	ldx ItemIndex
	ldy #$00
	lda ($04),y
	cmp #$A0
	bcc -
	lda PowerUpType,x
	and #$0F
	ora #$50
	sta $0343
	lda FrameCount
	lsr
	and #$03
	ora #$80
	sta ObjectCntrl
	lda SpritePagePos
	pha
	lda $074F,x
	jsr DrawFrame       ; display special item
	pla
	cmp SpritePagePos
	beq Exit9
	tax
	ldy ItemIndex
	lda PowerUpType,y
	ldy #$01
	cmp #$07
	beq +
	dey
	cmp #$06
	beq +
	cmp #$02
	bne ++
*       tya
	sta Sprite01RAM+2,x
	lda #$FF
*	pha
	ldx #$00
	ldy #$40
	jsr CheckObjYObjXHBOverlap
	pla
	bcs Exit9
	tay
	jsr PowerUpMusic
	ldx ItemIndex
	iny
	beq +
	lda PowerUpNameTable,x
	sta $08
	lda PowerUpType,x
	sta $09
	jsr UnknownDC1C
*       lda PowerUpType,x
	tay
	cpy #$08
	bcs ++++
	cpy #$06
	bcc +
	lda SamusGear
	and #$3F
	sta SamusGear
*       jsr MakeBitMask
	ora SamusGear
	sta SamusGear
*       lda #$FF
	sta PowerUpDelay
	sta PowerUpType,x
	ldy ItemRoomMusicStatus
	beq +
	ldy #$01
*       sty ItemRoomMusicStatus
	jmp SelectSamusPal
Exit9:  rts

*	beq +
	lda #5
	jsr AddToMaxMissiles
	bne ---	   ; branch always
*       lda TankCount
	cmp #$06	; has Samus got 6 energy tanks?
	beq +	   ; then she can't have any more
	inc TankCount   ; otherwise give her a new tank
*       lda TankCount
	jsr Amul16      ; shift into upper nibble
	ora #$09
	sta HealthHi
	lda #$99
	sta HealthLo    ; health is now FULL!
	bne -----	   ; branch always

UnknownDC1C:
  lda MapIdxX
UnknownDC1E:
  sta $07
	lda MapIdxY
	sta $06
	lda ScrollDir
	lsr
	php
	beq +
	bcc ++
	lda ScrollX
	beq ++
	dec $07
	bcs ++
*	bcc +
	lda ScrollY
	beq +
	dec $06
*   lda PPUCNT0ZP
	eor $08
	and #$01
	plp
	clc
	beq +
	adc $07
	sta $07
	jmp UnknownDC51

*   adc $06
	sta $06
	
UnknownDC51:
  jsr CreateItemID	
UnknownDC54:  
  ldy NumberOfUniqueItems
	lda $06
	sta UniqueItemHistory,y
	lda $07
	sta UniqueItemHistory+1,y
	iny
	iny
	sty NumberOfUniqueItems
	rts

;------------------------------------------[ Create item ID ]-----------------------------------------

;The following routine creates a unique two byte item ID number for that item.  The description
;of the format of the item ID number is as follows:
;
;IIIIIIXX XXXYYYYY. I = item type, X = X coordinate on world map, Y = Y coordinate
;on world map.  The items have the following values of IIIIII:
;High jump     = 000001
;Long beam     = 000010 (Not considered a unique item).
;Screw attack  = 000011
;Maru Mari     = 000100
;Varia suit    = 000101
;Wave beam     = 000110 (Not considered a unique item).
;Ice beam      = 000111 (Not considered a unique item).
;Energy tank   = 001000
;Missiles      = 001001
;Missile door  = 001010
;Bombs         = 001100
;Mother brain  = 001110
;1st Zeebetite = 001111
;2nd Zeebetite = 010000
;3rd Zeebetite = 010001
;4th Zeebetite = 010010
;5th Zeebetite = 010011
;
;The results are stored in $06(upper byte) and $07(lower byte).

CreateItemID:
	lda $07				;Load x map position of item.
	jsr Amul32			;($C2C$)*32. Move lower 3 bytes to upper 3 bytes.
	ora $06				;combine Y coordinates into data byte.
	sta $06				;Lower data byte complete. Save in $06.
	lsr $07				;
	lsr $07				;Move upper two bits of X coordinate to LSBs.
	lsr $07				;
	lda $09				;Load item type bits.
	asl				;Move the 6 bits of item type to upper 6 bits of byte.
	asl				;
	ora $07				;Add upper two bits of X coordinate to byte.
	sta $07				;Upper data byte complete. Save in #$06.
	rts				;

;-----------------------------------------------------------------------------------------------------


CheckObjYObjXHBOverlap: 
	jsr CreateObjYLocationStruct68PPUdiffA
Check68AYObjXHBOverlap:  
	jsr CreateObjectLocationStruct79PPUdiffB
Check68AY79BXHBOverlap:	
	jsr AddandCacheObjObjRadYX45
CallCheckHitboxOverlap:	
	jmp CheckHitboxOverlap

;The following table is used to rotate the sprites of both Samus and enemies when they explode.

ExplodeRotationTbl:
	.byte $00			;No sprite flipping.
	.byte $80			;Flip sprite vertically.
	.byte $C0			;Flip sprite vertically and horizontally.
	.byte $40			;Flip sprite horizontally.

; UpdateObjAnim
; =============
; Advance to object's next frame of animation
; A is an anim delay

UpdateObjAnim:
	ldx PageIndex
	ldy AnimDelay,x
	beq +	   					; is it time to advance to the next anim frame?
	dec AnimDelay,x     		; nope
	bne +++	  					; exit if still not zero (don't update animation)
*   sta AnimDelay,x     		; set initial anim countdown value
	ldy AnimIndex,x
*   lda ObjectAnimIndexTbl,y	;($8572)Load frame number.
	cmp #$FF					; has end of anim been reached?
	beq ++
	sta AnimFrame,x     		; store frame number
	iny	     					; inc anim index
	tya
	sta AnimIndex,x    			; store anim index
*	rts

*   ldy AnimResetIndex,x    	; reset anim frame index
	jmp ---	   					; do first frame of animation

  pha
	lda #$00
	sta $06
	pla
	bpl +
	dec $06
*   clc
	rts

;--------------------------------[ Get sprite control byte ]-----------------------------------------

;The sprite control byte extracted from the frame data has the following format: AABBXXXX.
;Where AA are the two bits used to control the horizontal and verticle mirroring of the
;sprite and BB are the two bits used control the sprite colors. XXXX is the entry number
;in the PlacePtrTbl used to place the sprite on the screen.

GetSpriteCntrlData:
	ldy #$00			;
	sty $0F				;Clear index into placement data.
	lda ($00),y			;Load control byte from frame pointer data.
	sta $04				;Store value in $04 for processing below.
	tax					;Keep a copy of the value in x as well.
	jsr Adiv16			;($C2BF)Move upper 4 bits to lower 4 bits.
	and #$03			;
	sta $05				;The following lines take the upper 4 bits in the-->
	txa					;control byte and transfer bits 4 and 5 into $05 bits 0-->
	and #$C0			;and 1(sprite color bits).  Bits 6 and 7 are-->
	ora #$20			;transferred into $05 bits 6 and 7(sprite flip bits).-->
	ora $05				;bit 5 is then set(sprite always drawn behind background).
	sta $05				;
	lda ObjectCntrl			;Extract bit from control byte that controls the
	and #$10			;object mirroring.
	asl				;
	asl				;
	eor $04				;Move it to the bit 6 position and use it to flip the-->
	sta $04				;horizontal mirroring of the sprite if set.
	lda ObjectCntrl			;
	bpl +				;If MSB is set in ObjectCntrl, use its flip bits(6 and 7).
	asl ObjectCntrl			;
	jsr SpriteFlipBitsOveride	;($E038)Use object flip bits as priority over sprite flip bits. 
*	txa				;Discard upper nibble so only entry number into-->
	and #$0F			;PlacePtrTbl remains.
	asl				;*2. pointers in PlacePntrTbl are 2 bytes in size.
	tax				;Transfer to X to use as an index to find proper-->
	rts				;placement data segment.

;-----------------------------------------------------------------------------------------------------

ClearEnExplosionAndDropItem:
	jsr ClearObjectCntrl		; remove object properties
	pla							; remove last call back from stack
	pla	
	ldx PageIndex				; load enemy index
KillEnemyAndDropItem:
	lda InArea					; load current area
	cmp #$13					; compare to Tourian
	bne +						; 	not tourian? skip ahead
	lda EnDataIndex,x			; in tourian - load the enemy data index
	cmp #$04					; TODO: if the tourian enemy was either index 2 or 4, kill object with no item drop
	beq +++++
	cmp #$02
	beq +++++
*   lda $040C,x					; TODO: load enemy mystery byte
	asl							; shift left, putting bit 6 into MSB
	bmi DefeatMiniBoss			; 	bit 6 was set, defeat the miniboss
	jsr LoadFromEn968B			; normal enemy - load 968B data
	sta $00						; cache off in $0
	jsr LoadFrom977B					; load 977B data, MSB moved to carry
	and #$20					; isolate metroid bit
	sta EnDataIndex,x			; store off in enemy data index (used to powerup the powerups)
	lda #$05					; load 5
	sta EnStatus,x				; store into enemy status (powerup)
	lda #$60					; load 60
	sta $040D,x					; store in powerup lifetime timer
	lda RandomNumber1			; roll to spawn
	cmp #$10					; check against $10
	bcc KillEnemy				; less than, kill enemy
*	and #$07					; %7 of random number
	tay							; move to y
	lda ItemDropTbl,y			; load item from drop table
	sta EnAnimFrame,x			; store into enemy anim frame directly
	cmp #$80					; compare to missle
	bne ++						; 	not a missle, was an energy pickup, or no pickup - check if we can spawn it
	ldy RoomMaxMissileSpawns	; was missle- check if we can spawn it
	cpy RoomCurrMissileSpawns	; 
	beq KillEnemy				; 	reached max missle pickups, kill enemy
	lda MaxMissiles				; alternatively, if we don't have a missle containter to our name
	beq KillEnemy				; 	kill enemy if no missle containers
	inc RoomCurrMissileSpawns	; inc missle pickup spawns
*   rts							; return withut killing enemy

*   ldy MaxEnergyPickup			; check if we can spawn an energy pickup
	cpy CurrentEnergyPickups
	beq KillEnemy				; no more available pickups for room- kill enemy
	inc CurrentEnergyPickups	; increment pickups
	cmp #$89					; compare to strong enemy energy drop
	bne --						; not equal, return without killing enemy
	lsr $00						; load up cached enemy data, get LSB - check if we can spawn the strong powerup
	bcs --						; 	carry set, strong powerup spawned- else, kill enemy
	
KillEnemy:
	ldx PageIndex				; 	
	lda InArea					; load area
	cmp #$13					; check against tourian
	beq ++						; 	tourian? jum pahead
*	jmp KillObject				; not tourian, kill object outright

;;in tourian, no pickup limit- reset max and current pickups and re-roll item
*   lda RandomNumber1			; pick a random number
	ldy #$00					; load 0 into y
	sty CurrentEnergyPickups	; clear energy pickup counter for room
	sty RoomCurrMissileSpawns	; clear missile pickup counter for room
	iny							; set max pickups for the room 
	sty RoomMaxMissileSpawns	; to 1
	sty MaxEnergyPickup			; 
	bne -----					; branch always

DefeatMiniBoss:
	jsr PowerUpMusic			; play power up music
	lda InArea					; load area - 0 brin, 1 nor, 2 kraid, 3 tor, 4 rid
	and #$0F					; TODO:
	sta FanfareMusicDelay		; why are we storing a variable time intp the music delay?
	lsr							; move out lowest bit/div by 2 - 0 brin, 0 nor, 1 kraid, 1 tor, 2 rid
	tay							; 
	sta MaxMissiles,y			; maxmiss + 1 = kraid statue status, maxmiss+2 = ridley statue status
	lda #75						; 
	jsr AddToMaxMissiles		; add 75 to max missiles
	bne KillEnemy

UpdateObjectCntrl:
	ldx PageIndex				; load enemy index
	lda EnAnimFrame,x			; load enemy frame
	cmp #$F7					; compare to F7 (invalid frame)
	bne +++						; not equal? jump way ahead
	jmp ClearObjectCntrl		;($DF2D)Clear object control byte.

; AddToMaxMissiles
; ================
; Adds A to both MissileCount & MaxMissiles, storing the new count
; (255 if it overflows)

AddToMaxMissiles:
	pha					; Temp storage of # of missiles to add.
	clc					; clear carry 
	adc MissileCount	; add to current missle count
	bcc +				; 	no roll over? skip ahead
	lda #$FF			; 	roll over, clamp to 255 
*   sta MissileCount	; store missle count 
	pla					; get missle count to add back to A
	clc					; clear carry
	adc MaxMissiles		; add to max missile count
	bcc +				; 	no roll over? skip ahead
	lda #$FF			; 	rollover, clamp to 255
*   sta MaxMissiles		; set max missile count
	rts					; return

;anim frame was not invalid, so we need to do some stuff
*	lda EnYRoomPos,x		 	;cache off enemy position in 
	sta $0A	 				 	; A = Y coord
	lda EnXRoomPos,x
	sta $0B	 				 	; B = X coord
	lda EnNameTable,x
	sta $06	 				 	; 6 = NT coord
	lda EnAnimFrame,x		 	; load enemy current anim frame
	asl						 	; double it - will shift a bit into carry to tell us if we need to use the second table- also for two byte table reads, we're getting a pointer
	tay							; move to Y 
	lda (EnmyFrameTbl1Ptr),y	; by default, load anim frame lo byte from table 1
	bcc +						; 	carry clear, go get hi byte
	lda (EnmyFrameTbl2Ptr),y	; carry set- using the second table
*   sta $00						; store anim frame lo byte to 0
	iny							; inc y for next byte
	lda (EnmyFrameTbl1Ptr),y	; by default, load anim frame hi byte from table 1 
	bcc +						; 	carry clear, skip ahead
	lda (EnmyFrameTbl2Ptr),y	; carry set- using second table
*   sta $01						; store anim frame high byte into 1
	jsr GetSpriteCntrlData		;($DCC3)Get place pointer index and sprite control data.
	tay							; we already stored the place pointer index into X, but we can move it to Y for indirect indexing
	lda (EnmyPlaceTblPtr),y		; get enemy placement lo
	sta $02						; store in $2
	iny							; inc y for high byte
	lda (EnmyPlaceTblPtr),y		; get enemy placement hi
	sta $03						; store in $3
	ldy #$00					; set y to 0
	cpx #$02					; compare x (the placement table index still, unincremented) to 2 - this is the enemy explosion placement
	bne +						; 	not equal, jump ahead
	ldx PageIndex				; enemy explosion placement- load enemy index
	inc EnCounter,x				; inc the enemy counter
	lda EnCounter,x				; then load it
	pha							; then cache it on the stack
	and #$03					; %4, to get 0, 1, 2, or 3
	tax							; move into X 
	lda $05						; load 
	and #$3F
	ora ExplodeRotationTbl,x
	sta $05
	pla
	cmp #$19
	bne +
	jmp ClearEnExplosionAndDropItem

*   ldx PageIndex
	iny
	lda ($00),y
	sta EnRadY,x
	jsr ReduceYRadius		;($DE3D)Reduce temp y radius by #$10.
	iny
	lda ($00),y
	sta EnRadX,x
	sta $09
	iny
	sty $11
	jsr IsObjectVisible		;($DFDF)Determine if object is within screen boundaries.
	txa						;move result to a (1 if visible, 0 if not)
	asl						;shift to the left (bit 1 is the visible flag
	sta $08					;store value in zero page
	ldx PageIndex			;load enemy
	lda $0405,x				;load 405 of enemy
	and #$FD				;mask with 1111 1101 to clear current visibility flag
	ora $08					;set the visibility flag from zero page
	sta $0405,x				;store into 405
	lda $08					;load the visibility flag
	beq ++					;if it's not visible, jump ahead (note- not a fan of this kind of intertwining)
	jmp UpdateObjectSprite	;else, jump to sprite drawing routine
	
;----------------------------------------[ Item drop table ]-----------------------------------------

;The following table determines what, if any, items an enemy will drop when it is killed.

ItemDropTbl:
	.byte $80			;Missile.
	.byte $81			;Energy.
	.byte $89			;No item.
	.byte $80			;Missile.
	.byte $81			;Energy.
	.byte $89			;No item.
	.byte $81			;Energy.
	.byte $89			;No item.

;------------------------------------[ Object drawing routines ]-------------------------------------

;The following function effectively sets an object's temporary y radius to #$00 if the object
;is 4 tiles tall or less.  If it is taller, #$10 is subtracted from the temporary y radius.

ReduceYRadius:
	sec				;
	sbc #$10			;Subtract #$10 from object y radius.
	bcs +				;If number is still a positive number, branch to store value.
	lda #$00			;Number is negative.  Set Y radius to #$00.
*	sta $08				;Store result and return.
	rts				;

AnimDrawObject:
	jsr UpdateObjAnim		;($DC8F)Update animation if needed.

DrawFrame:
	ldx PageIndex			;Get index to proper object to work with.
	lda AnimFrame,x			;
	cmp #$F7			;Is the frame valid?-->
	bne ++				;Branch if yes.
*	jmp ClearObjectCntrl		;($DF2D)Clear object control byte and exit
*	cmp #$07			;Is the animation of Samus facing forward?-->
	bne +				;If not, branch.

	lda ObjectCntrl			;Ensure object mirroring bit is clear so Samus'-->
	and #$EF			;sprite appears properly when going up and down-->
	sta ObjectCntrl			;elevators.

*	lda ObjectY,x			;
	sta $0A				;
	lda ObjectX,x			;Copy object y and x room position and name table-->
	sta $0B				;data into $0A, $0B and $06 respectively.
	lda ObjectHi,x			;
	sta $06				;
	lda AnimFrame,x			;Load A with index into FramePtrTable.
	asl				;*2. Frame pointers are two bytes.
	tax				;X is now the index into the FramePtrTable.
	lda FramePtrTable,x		;
	sta $00				;
	lda FramePtrTable+1,x		;Entry from FramePtrTable is stored in $0000.
	sta $01				;
	jsr GetSpriteCntrlData		;($DCC3)Get place pointer index and sprite control data.
	lda PlacePtrTable,x		;
	sta $02				;
	lda PlacePtrTable+1,x		;Store pointer from PlacePtrTbl in $0002.
	sta $03				;
	lda IsSamus			;Is Samus the object being drawn?-->
	beq +				;If not, branch.

;Special case for Samus exploding.
	cpx #$0E			;Is Samus exploding?-->
	bne +				;If not, branch to skip this section of code.
	ldx PageIndex			;X=0.
	inc ObjectCounter		;Incremented every frame during explode sequence.-->
	lda ObjectCounter		;Bottom two bits used for index into ExplodeRotationTbl.
	pha				;Save value of A.
	and #$03			;Use 2 LSBs for index into ExplodeRotationTbl.
	tax				;
	lda $05				;Drop mirror control bits from sprite control byte.
	and #$3F			;
	ora ExplodeRotationTbl,x	;Use mirror control bytes from table(Base is $DC8B).
	sta $05				;Save modified sprite control byte.
	pla				;Restore A
	cmp #$19			;After 25 frames, Move on to second part of death--> 
	bne +				;handler, else branch to skip the rest of this code.
	ldx PageIndex			;X=0.
	lda #sa_Dead2			;
	sta ObjAction,x			;Move to next part of the death handler.
	lda #$28			;
	sta AnimDelay,x			;Set animation delay for 40 frames(.667 seconds).
	pla				;Pull last return address off of the stack.
	pla				;
	jmp ClearObjectCntrl		;($DF2D)Clear object control byte.

*	ldx PageIndex			;
	iny				;Increment to second frame data byte.
	lda ($00),y			;
	sta ObjRadY,x			;Get verticle radius in pixles of object.
	jsr ReduceYRadius		;($DE3D)Reduce temp y radius by #$10.
	iny				;Increment to third frame data byte.
	lda ($00),y			;Get horizontal radius in pixels of object.
	sta ObjRadX,x			;
	sta $09				;Temp storage for object x radius.
	iny				;Set index to 4th byte of frame data.
	sty $11				;Store current index into frame data.
	jsr IsObjectVisible		;($DFDF)Determine if object is within the screen boundaries.
	txa				;
	ldx PageIndex			;Get index to object.
	sta ObjectOnScreen,x		;Store visibility status of object.
	tax				;
	beq +				;Branch if object is not within the screen boundaries.
	
UpdateObjectSprite:
	ldx SpritePagePos		;Load index into next unused sprite RAM segment.
	jmp DrawSpriteObject		;($DF19)Start drawing object.

*	jmp ClearObjectCntrl		;($DF2D)Clear object control byte then exit.

WriteSpriteRAM:
*	ldy $0F				;Load index for placement data.
	jsr YDisplacement		;($DF6B)Get displacement for y direction.
	adc $10				;Add initial Y position.
	sta Sprite00RAM,x		;Store sprite Y coord.
	dec Sprite00RAM,x		;Because PPU uses Y + 1 as real Y coord.
	inc $0F				;Increment index to next byte of placement data.
	ldy $11				;Get index to frame data.
	lda ($00),y			;Tile value.
	sta Sprite00RAM+1,x		;Store tile value in sprite RAM.
	lda ObjectCntrl			;
	asl				;Move horizontal mirror control byte to bit 6 and-->
	asl				;discard all other bits.
	and #$40			;
	eor $05				;Use it to override sprite horz mirror bit.
	sta Sprite00RAM+2,x		;Store sprite control byte in sprite RAM.
	inc $11				;Increment to next byte of frame data.
	ldy $0F				;Load index for placement data.
	jsr XDisplacement		;($DFA3)Get displacement for x direction.
	adc $0E				;Add initial X pos
	sta Sprite00RAM+3,x		;Store sprite X coord
	inc $0F				;Increment to next placement data byte.
	inx				;
	inx				;
	inx				;Advance to next sprite.
	inx				;

DrawSpriteObject:
  ldy $11				;Get index into frame data.

GetNextFrameByte:
  lda ($00),y			;Get next frame data byte.
	cmp #$FC			;If byte < #$FC, byte is tile data. If >= #$FC, byte is--> 
	bcc WriteSpriteRAM		;frame data control info. Branch to draw sprite.
	beq OffsetObjectPosition	;#$FC changes object's x and y position.
	cmp #$FD			;
	beq GetNewControlByte		;#$FD sets new control byte information for the next sprites.
	cmp #$FE			;#$FE skips next sprite placement x and y bytes.
	beq SkipPlacementData		;
	stx SpritePagePos		;Keep track of current position in sprite RAM.

ClearObjectCntrl:
	lda #$00			;
	sta ObjectCntrl		;Clear object control byte.
	rts					;

SkipPlacementData:
*	inc $0F				;Skip next y and x placement data bytes.
	inc $0F				;
	inc $11				;Increment to next data item in frame data.
	jmp DrawSpriteObject		;($DF19)Draw next sprite.

GetNewControlByte:
*	iny				;Increment index to next byte of frame data.
	asl ObjectCntrl			;If MSB of ObjectCntrl is not set, no overriding of-->
	bcc +				;flip bits needs to be performed.
	jsr SpriteFlipBitsOveride	;($E038)Use object flip bits as priority over sprite flip bits.
	bne ++				;Branch always.
*	lsr ObjectCntrl			;Restore MSB of ObjectCntrl.
	lda ($00),y			;
	sta $05				;Save new sprite control byte.
*	iny				;Increment past sprite control byte.
	sty $11				;Save index of frame data.
	jmp GetNextFrameByte		;($DF1B)Load next frame data byte.

OffsetObjectPosition:
*	iny				;Increment index to next byte of frame data.
	lda ($00),y			;This data byte is used to offset the object from-->
	clc				;its current y positon.
	adc $10				;
	sta $10				;Add offset amount to object y screen position.
	inc $11				;
	inc $11				;Increment past control byte and y offset byte.
	ldy $11				;
	lda ($00),y			;Load x offset data byte.
	clc				;
	adc $0E				;Add offset amount to object x screen position.
	sta $0E				;
	inc $11				;Increment past x offset byte.
	jmp DrawSpriteObject		;($DF19)Draw next sprite.

;----------------------------------[ Sprite placement routines ]-------------------------------------

YDisplacement:
  lda ($02),y			;Load placement data byte.
	tay				;
	and #$F0			;Check to see if this is placement data for the object-->
	cmp #$80			;exploding.  If so, branch.
	beq ++				;
	tya				;Restore placement data byte to A.
*	bit $04				;
	bmi NegativeDisplacement	;Branch if MSB in $04 is set(Flips object).
	clc				;Clear carry before returning.
	rts				;

ExplodeYDisplace:
*	tya				;Transfer placement byte back into A.
	and #$0E			;Discard bits 7,6,5,4 and 0.
	lsr				;/2.
	tay				;
	lda ExplodeIndexTbl,y		;Index into ExplodePlacementTbl.
	ldy IsSamus			;
	bne +				;Is Samus the object exploding? if so, branch.
	ldy PageIndex			;Load index to proper enemy data.
	adc EnCounter,y			;Increment every frame enemy is exploding. Initial=#$01.
	jmp ++				;Jump to load explode placement data.


;Special case for Samus exploding.
*	adc ObjectCounter		;Increments every frame Samus is exploding. Initial=#$01.
*	tay				;
	lda ExplodeIndexTbl+2,y		;Get data from ExplodePlacementTbl.
	pha				;Save data on stack.
	lda $0F				;Load placement data index.
	clc				;
	adc #$0C			;Move index forward by 12 bytes. to find y-->
	tay				;placement data.
	pla				;Restore A with ExplodePlacementTbl data.
	clc				;
	adc ($02),y			;Add table displacements with sprite placement data.
	jmp ----			;Branch to add y placement values to sprite coords.

XDisplacement:
  lda ($02),y			;Load placement data byte.
	tay				;
	and #$F0			;Check to see if this is placement data for the object-->
	cmp #$80			;exploding.  If so, branch.
	beq +++				;
	tya				;Restore placement data byte to A.
*	bit $04				;
	bvc +				;Branch if bit 6 cleared, else data is negative displacement.

NegativeDisplacement:
  eor #$FF			;
	sec				;NOTE:Setting carry makes solution 1 higher than expected.
	adc #$F8			;If flip bit is set in $04, this function flips the-->
*	clc				;object by using two compliment minus 8(Each sprite is-->
	rts				;8x8 pixels).

ExplodeXDisplace:
*	ldy PageIndex			;Load index to proper enemy slot.
	lda EnCounter,y			;Load counter value.
	ldy IsSamus			;Is Samus the one exploding?-->
	beq +				;If not, branch.
	lda ObjectCounter		;Load object counter if it is Samus who is exploding.
*	asl				;*2. Move sprite in x direction 2 pixels every frame.
	pha				;Store value on stack.
	ldy $0F				;
	lda ($02),y			;Load placement data byte.
	lsr				;
	bcs +				;Check if LSB is set. If not, the byte stored on stack-->
	pla				;Will be twos complimented and used to move sprite in-->
	eor #$FF			;the negative x direction.
	adc #$01			;
	pha				;
*	lda $0F				;Load placement data index.
	clc				;
	adc #$0C			;Move index forward by 12 bytes. to find x-->
	tay				;placement data.
	pla				;Restore A with x displacement data.
	clc				;
	adc ($02),y			;Add x displacement with sprite placement data.
	jmp -----			;Branch to add x placement values to sprite coords.

;---------------------------------[ Check if object is on screen ]----------------------------------

;The following set of functions determine if an object is visible on the screen.  If the object
;is visible, X-1 when the function returns, X=0 if the object is not within the boundaries of the
;current screen.  The function needs to know what nametable is currently in the PPU, what nametable
;the object is on and what the scroll offsets are. 

IsObjectVisible:
  ldx #$01			;Assume object is visible on screen.
	lda $0A				;Object Y position in room.
	tay				;
	sec				;Subtract y scroll to find sprite's y position on screen.
	sbc ScrollY			;
	sta $10				;Store result in $10.
	lda $0B				;Object X position in room.
	sec				;
	sbc ScrollX			;Subtract x scroll to find sprite's x position on screen.
	sta $0E				;Store result in $0E.
	lda ScrollDir			;
	and #$02			;Is Samus scrolling left or right?-->
	bne HorzScrollCheck		;($E01C)If so, branch.

VertScrollCheck:
	cpy ScrollY			;If object room pos is >= scrollY, set carry.
	lda $06				;Check if object is on different name table as current-->
	eor PPUCNT0ZP			;name table active in PPU.-->
	and #$01			;If not, branch.
	beq +				;
	bcs ++				;If carry is still set, sprite is not in screen boundaries.
	lda $10				;
	sbc #$0F			;Move sprite y position up 15 pixles.
	sta $10				;
	lda $09				;
	clc				;If a portion of the object is outside the sceen-->
	adc $10				;boundaries, treat object as if the whole thing is-->
	cmp #$F0			;not visible.
	bcc +++				;
	clc				;Causes next statement to branch always.
*	bcc +				;
	lda $09				;If object is on same name table as the current one in-->
	cmp $10				;the PPU, check if part of object is out of screen--> 
	bcc ++				;boundaries.  If so, branch.
*	dex				;Sprite is not within screen boundaries. Decrement X.
*	rts				;

HorzScrollCheck:
  lda $06				;
	eor PPUCNT0ZP			;Check if object is on different name table as current-->
	and #$01			;name table active in PPU.-->
	beq +				;If not, branch.
	bcs ++				;If carry is still set, sprite is not in screen boundaries.
	lda $09				;
	clc				;If a portion of the object is outside the sceen-->
	adc $0E				;boundaries, treat object as if the whole thing is-->
	bcc +++				;not visible.
	clc				;Causes next statement to branch always.
*	bcc +				;
	lda $09				;If object is on same name table as the current one in-->
	cmp $0E				;the PPU, check if part of object is out of screen--> 
	bcc ++				;boundaries.  If so, branch.
*	dex				;Sprite is not within screen boundaries. Decrement X.
*	rts				;

;------------------------[ Override sprite flip bits with object flip bits ]-------------------------

;If the MSB is set in ObjectCntrl, its two upper bits that control sprite flipping take priority
;over the sprite control bits.  This function modifies the sprite control byte with any flipping
;bits found in ObjectCntrl.

SpriteFlipBitsOveride:
	lsr ObjectCntrl			;Restore MSB.
	lda ($00),y			;Reload frame data control byte into A.
	and #$C0			;Extract the two sprite flip bytes from theoriginal-->
	ora ObjectCntrl			;control byte and set any additional bits from ObjectCntrl.
	sta $05				;Store modified byte to load in sprite control byte later.
	lda ObjectCntrl			;
	ora #$80			;
	sta ObjectCntrl			;Ensure MSB of object control byte remains set.
	rts				;

;--------------------------------[ Explosion placement data ]---------------------------------------

;The following table has the index values into the table after it for finding the placement data
;for an exploding object.

ExplodeIndexTbl:
	.byte $00, $18, $30

;The following table is used to produce the arcing motion of exploding objects.  It is displacement
;data for the y directions only.  The x displacement is constant.

ExplodePlacementTbl:

;Bottom sprites.
	.byte $FC, $F8, $F4, $F0, $EE, $EC, $EA, $E8, $E7, $E6, $E6, $E5, $E5, $E4, $E4, $E3
	.byte $E5, $E7, $E9, $EB, $EF, $F3, $F7, $FB

;Middle sprites.
	.byte $FE, $FC, $FA, $F8, $F6, $F4, $F2, $F0, $EE, $ED, $EB, $EA, $E9, $E8, $E7, $E6
	.byte $E6, $E6, $E6, $E6, $E8, $EA, $EC, $EE

;Top sprites.
	.byte $FE, $FC, $FA, $F8, $F7, $F6, $F5, $F4, $F3, $F2, $F1, $F1, $F0, $F0, $EF, $EF
	.byte $EF, $EF, $EF, $EF, $F0, $F0, $F1, $F2

;--------------------------------------[ Update enemy animation ]-----------------------------------

;Advance to next frame of enemy's animation. Basically the same as UpdateObjAnim, only for enemies.

UpdateEnemyAnim:
	ldx PageIndex				; Load index to desired enemy.
	ldy EnStatus,x				;
	cpy #$05					; Is enemy in the process of dying?-->
	beq +++						; If so, branch to exit.
	ldy EnAnimDelay,x			;
	beq +						; Check if current anumation frame is ready to be updated.
	dec EnAnimDelay,x			;	Not ready to update. decrement delay timer and-->
	bne +++						;	branch to exit.
*	sta EnAnimDelay,x			; Save new animation delay value.
	ldy EnAnimIndex,x			; Load enemy animation index.
*	lda (EnemyAnimPtr),y		; Get animation data.
	cmp #$FF					; End of animation?
	beq ++						; If so, branch to reset animation.
	sta EnAnimFrame,x			; Store current animation frame data.
	iny							; Increment to next animation data index.
	tya							;
	sta EnAnimIndex,x			; Save new animation index.
*	rts							;

*	ldy EnResetAnimIndex,x		; reset animation index.
	bcs ---						; Branch always.

;---------------------------------------[ Display status bar ]---------------------------------------

;Displays Samus' status bar components.

DisplayBar:
	ldy #$00			;Reset data index.
	lda SpritePagePos		;Load current sprite index.
	pha				;save sprite page pos.
	tax				;
*	lda DataDisplayTbl,y		;
	sta Sprite00RAM,x		;Stor contents of DataDisplayTbl in sprite RAM.
	inx				;
	iny				;
	cpy #$28			;10*4. At end of DataDisplayTbl? If not, loop to-->
	bne -				;load next byte from table.

;Display 2-digit health count.
	stx SpritePagePos		;Save new location in sprite RAM.
	pla				;Restore initial sprite page pos.
	tax				;
	lda HealthHi			;
	and #$0F			;Extract upper health digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	lda HealthLo			;
	jsr Adiv16			;($C2BF)Move lower health digit to 4 LSBs.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	ldy EndTimerHi			;
	iny				;Is Samus in escape sequence?-->
	bne ++				;If so, branch.
	ldy MaxMissiles			;
	beq +				;Don't show missile count if Samus has no missile containers.

;Display 3-digit missile count.
	lda MissileCount		;
	jsr HexToDec			;($E198)Convert missile hex count to decimal cout.
	lda $02				;Upper digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	lda $01				;Middle digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	lda $00				;Lower digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	bne +++				;Branch always.

;Samus has no missiles, erase missile sprite.
*	lda #$FF			;"Blank" tile.
	cpx #$F4			;If at last 3 sprites, branch to skip.
	bcs ++				;
	sta Sprite03RAM+1,x		;Erase left half of missile.
	cpx #$F0			;If at last 4 sprites, branch to skip.
	bcs ++				;
	sta Sprite04RAM+1,x		;Erase right half of missile.
	bne ++				;Branch always.

;Display 3-digit end sequence timer.
*	lda EndTimerHi			;
	jsr Adiv16			;($C2BF)Upper timer digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	lda EndTimerHi			;
	and #$0F			;Middle timer digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	lda EndTimerLo			;
	jsr Adiv16			;($C2BF)Lower timer digit.
	jsr SPRWriteDigit		;($E173)Display digit on screen.
	lda #$58			;"TI" sprite(left half of "TIME").
	sta Sprite00RAM+1,x		;
	inc Sprite00RAM+2,x		;Change color of sprite.
	cpx #$FC			;If at last sprite, branch to skip.
	bcs +				;
	lda #$59			;"ME" sprite(right half of "TIME").
	sta Sprite01RAM+1,x		;
	inc Sprite01RAM+2,x		;Change color of sprite.

*	ldx SpritePagePos		;Restore initial sprite page pos.
	lda TankCount			;
	beq ++				;Branch to exit if Samus has no energy tanks.

;Display full/empty energy tanks.
	sta $03				;Temp store tank count.
	lda #$40			;X coord of right-most energy tank.
	sta $00				;Energy tanks are drawn from right to left.
	ldy #$6F			;"Full energy tank" tile.
	lda HealthHi			;
	jsr Adiv16			;($C2BF)/16. A contains # of full energy tanks.
	sta $01				;Storage of full tanks.
	bne AddTanks			;Branch if at least 1 tank is full.
	dey				;Else switch to "empty energy tank" tile.

AddTanks:
	jsr AddOneTank			;($E17B)Add energy tank to display.
	dec $01				;Any more full energy tanks left?-->
	bne +				;If so, then branch.-->
	dey				;Otherwise, switch to "empty energy tank" tile.
*	dec $03				;done all tanks?-->
	bne AddTanks			;if not, loop to do another.

	stx SpritePagePos		;Store new sprite page position.
*	rts				;

;----------------------------------------[Sprite write digit ]---------------------------------------

;A=value in range 0..9. #$A0 is added to A(the number sprites begin at $A0), and the result is stored
;as the tile # for the sprite indexed by X.

SPRWriteDigit:
	ora #$A0			;#$A0 is index into pattern table for numbers.
	sta Sprite00RAM+1,x		;Store proper nametable pattern in sprite RAM.
	jmp Xplus4			;Find next sprite pattern table byte.

;----------------------------------[ Add energy tank to display ]------------------------------------

;Add energy tank to Samus' data display.

AddOneTank:
	lda #$17			;Y coord-1.
	sta Sprite00RAM,x		;
	tya				;Tile value.
	sta Sprite00RAM+1,x		;
	lda #$01			;Palette #.
	sta Sprite00RAM+2,x		;
	lda $00				;X coord.
	sta Sprite00RAM+3,x		;
	sec				;
	sbc #$0A			;Find x coord of next energy tank.
	sta $00				;

;-----------------------------------------[ Add 4 to x ]---------------------------------------------

Xplus4:
	inx				;
	inx				;
	inx				;Add 4 to value stored in X.
	inx				;
	rts				;

;------------------------------------[ Convert hex to decimal ]--------------------------------------

;Convert 8-bit value in A to 3 decimal digits. Upper digit put in $02, middle in $01 and lower in $00.

HexToDec:
	ldy #100			;Find upper digit.
	sty $0A				;
	jsr GetDigit			;($E1AD)Extract hundreds digit.
	sty $02				;Store upper digit in $02.
	ldy #10				;Find middle digit.
	sty $0A				;
	jsr GetDigit			;($E1AD)Extract tens digit.
	sty $01				;Store middle digit in $01.
	sta $00				;Store lower digit in $00
	rts				;

GetDigit:
	ldy #$00			;
	sec				;
*	iny				;
	sbc $0A				;Loop and subtract value in $0A from A until carry flag-->
	bcs -				;is not set.  The resulting number of loops is the decimal-->
	dey				;number extracted and A is the remainder.
	adc $0A				;
	rts				;

;-------------------------------------[ Status bar sprite data ]-------------------------------------

;Sprite data for Samus' data display

DataDisplayTbl:
	.byte $21,$A0,$01,$30		;Upper health digit.
	.byte $21,$A0,$01,$38		;Lower health digit.
	.byte $2B,$FF,$01,$28		;Upper missile digit.
	.byte $2B,$FF,$01,$30		;Middle missile digit.
	.byte $2B,$FF,$01,$38		;Lower missile digit.
	.byte $2B,$5E,$00,$18		;Left half of missile.
	.byte $2B,$5F,$00,$20		;Right half of missile.
	.byte $21,$76,$01,$18		;E
	.byte $21,$7F,$01,$20		;N
	.byte $21,$3A,$00,$28		;..

;-------------------------------------------[ Bit scan ]---------------------------------------------

;This function takes the value stored in A and right shifts it until a set bit is encountered.
;Once a set bit is encountered, the function exits and returns the bit number of the set bit.
;The returned value is stored in A. 

BitScan:
	stx $0E				;Save X.
	ldx #$00			;First bit is bit 0.
*	lsr					;Transfer bit to carry flag.
	bcs +				;If the shifted bit was 1, Branch out of loop.
	inx					;Increment X to keep of # of bits checked.
	cpx #$08			;Have all 8 bit been tested?-->
	bne -				;If not, branch to check the next bit.
*	txa					;Return which bit number was set.
	ldx $0E				;Restore X.
*	rts					;

;------------------------------------------[ Scroll door ]-------------------------------------------

;Scrolls the screen if Samus is inside a door.

ScrollDoor:
	ldx SamusDoorStatus			;
	beq -				;Exit if Samus isn't in a door.
	dex				;
	bne +				;Not in right door. branch to check left door.
	jsr ScrollRight			;($E6D2)SamusDoorStatus=1, scroll 1 pixel right.
	jmp ++				;Jump to check if door scroll is finished.

*	dex				;Check if in left door.
	bne ++				;
	jsr ScrollLeft			;($E6A7)SamusDoorStatus=2, scroll 1 pixel left.
*	ldx ScrollX			;Has x scroll offset reached 0?-->
	bne Exit15			;If not, branch to exit.

;Scrolled one full screen, time to exit door.
	ldx #$05			;Samus is exiting the door.
	bne DoOneDoorScroll		;Branch always.

*	dex				;
	bne +				;Check if need to scroll down to center door.
	jsr ScrollDown			;($E519)SamusDoorStatus=3, scroll 1 pixel down.
	jmp ++				;Jump to check y scrolling value.
*	dex				;
	bne Exit15			;Check if need to scroll up to center door.
	jsr ScrollUp			;($E4F1)SamusDoorStatus=4, scroll 1 pixel up.

VerticalRoomCentered:
*	ldx ScrollY			;Has room been centered on screen?-->
	bne Exit15			;If not, branch to exit.
	stx DoorOnNameTable3		;
	stx DoorOnNameTable0		;Erase door nametable data.
	inx				;X=1.
	lda ObjectX			;Did Samus enter in the right hand door?-->
	bmi ++				;If so, branch.
	inx				;X=2. Samus is in left door.
	bne ++				;Branch always.

;This function is called once after door scrolling is complete.

DoOneDoorScroll:
	lda #$20			;Set DoorDelay to 32 frames(comming out of door).
	sta DoorDelay			;
	lda SamusDoorData		;Check if scrolling should be toggled.
	jsr Amul8			;($C2C6)*8. Is door not to toggle scrolling(item room,-->
	bcs +				;bridge room, etc.)? If so, branch to NOT toggle scrolling.
	ldy DoorScrollStatus		;If comming from vertical shaft, skip ToggleScroll because-->
	cpy #$03			;the scroll was already toggled after room was centered-->
	bcc ++				;by the routine just above.
*	lda #$47			;Set mirroring for vertical mirroring(horz scrolling).
	bne ++				;Branch always.

*	jsr ToggleScroll		;($E252)Toggle scrolling and mirroring.
*	sta MirrorCntrl			;Store new mirror control data.
	stx SamusDoorStatus			;SamusDoorStatus=5. Done with door scrolling.

Exit15:
	rts				;Exit for several routines above.

;------------------------------------[ Toggle Samus nametable ]--------------------------------------

ToggleSamusHi:
	lda ObjectHi			;
	eor #$01			;Change Samus' current nametable from one to the other.
	sta ObjectHi			;
	rts				;

;-------------------------------------------[ Toggle scroll ]----------------------------------------

;Toggles both mirroring and scroll direction when Samus has moved from
;a horizontal shaft to a vertical shaft or vice versa.

ToggleScroll:
	lda ScrollDir			;
	eor #$03			;Toggle scroll direction.
	sta ScrollDir			;
	lda MirrorCntrl			;Toggle mirroring.
	eor #$08			;
	rts				;

;----------------------------------------[ Is Samus in lava ]----------------------------------------

;The following function checks to see if Samus is in lava.  If she is, the carry bit is cleared,
;if she is not, the carry bit is set. Samus can only be in lava if in a horizontally scrolling
;room. If Samus is 24 pixels or less away from the bottom of the screen, she is considered to be
;in lava whether its actually there or not.

IsSamusInLava:
  lda #$01			;
	cmp ScrollDir			;Set carry bit(and exit) if scrolling up or down.
	bcs +				;
	lda #$D8			;If Samus is Scrolling left or right and within 24 pixels-->
	cmp ObjectY			;of the bottom of the screen, she is in lava. Clear carry bit.
*	rts				;

;----------------------------------[ Check lava and movement routines ]------------------------------

LavaAndMoveCheck:
  lda ObjAction			;
	cmp #sa_Elevator		;Is Samus on elevator?-->
	beq +				;If so, branch.
	cmp #sa_Dead			;Is Samus Dead-->
	bcs -				;If so, branch to exit.
*	jsr IsSamusInLava		;($E25D)Clear carry flag if Samus is in lava.
	ldy #$FF			;Assume Samus not in lava.
	bcs ++++			;Samus not in lava so branch.

;Samus is in lava.
	sty DamagePushDirection		;Don't push Samus from lava damage.
	jsr ClearHealthChange		;($F323)Clear any pending health changes to Samus.
	lda #$32			;
	sta SamusBlink			;Make Samus blink.
	lda FrameCount			;
	and #$03			;Start the jump SFX every 4th frame while in lava.
	bne +				;
	jsr SFX_SamusJump		;($CBAC)Initiate jump SFX.
*	lda FrameCount			;
	lsr				;This portion of the code causes Samus to be damaged by-->
	and #$03			;lava twice every 8 frames if she does not have the varia-->
	bne ++				;but only once every 8 frames if she does.
	lda SamusGear			;
	and #gr_VARIA			;Does Samus have the Varia?-->
	beq +				;If not, branch.
	bcc ++				;Samus has varia. Carry set every other frame. Half damage.
*	lda #$07			;
	sta HealthLoChange		;Samus takes lava damage.
	jsr SubtractHealth		;($CE92)
*	ldy #$00			;Prepare to indicate Samus is in lava.
*	iny				;Set Samus lava status.
	sty SamusInLava			;

SamusMoveVertically:
	jsr VertAccelerate		;($E37A)Calculate vertical acceleration.
	lda ObjectY			;
	sec				;
	sbc ScrollY			;Calculate Samus' screen y position.
	sta SamusScrY			;
	lda $00				;Load temp copy of vertical speed.
	bpl ++++			;If Samus is moving downwards, branch.

	jsr TwosCompliment		;($C3D4)Get twos compliment of vertical speed.
	ldy SamusInLava			;Is Samus in lava?
	beq +				;If not, branch,-->
	lsr				;else cut vertical speed in half.
	beq SamusMoveHorizontally	;($E31A)Branch if no vertical mvmnt to Check left/right mvmnt.

;Samus is moving upwards.
*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
*	jsr MoveSamusUp			;($E457)Attempt to move Samus up 1 pixel.
	bcs +				;Branch if Samus successfully moved up 1 pixel.

	sec				;Samus blocked upwards. Divide her speed by 2 and set the
	ror ObjVertSpeed		;MSB to reverse her direction of travel.
	ror VertCountrLinear		;
	jmp SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

*	dec ObjectCounter		;1 pixel movement is complete.
	bne --				;Branch if Samus needs to be moved another pixel.

;Samus is moving downwards.
*	beq SamusMoveHorizontally	;($E31A)Branch if no vertical mvmnt to Check left/right mvmnt.
	ldy SamusInLava			;Is Samus in lava?
	beq +				;If not, branch,-->
	lsr				;Else reduce Samus speed by 75%(divide by 4).
	lsr				;
	beq SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
*	jsr MoveSamusDown		;($E4A3)Attempt to move Samus 1 pixel down.
	bcs +++				;Branch if Samus successfully moved down 1 pixel.

;Samus bounce after hitting the ground in ball form.
	lda ObjAction			;
	cmp #sa_Roll			;Is Samus rolled into a ball?-->
	bne +				;If not, branch.
	lsr ObjVertSpeed		;Divide verticle speed by 2.
	beq ++				;Speed not fast enough to bounce. branch to skip.
	ror VertCountrLinear		;Move carry bit into MSB to reverse Linear counter.
	lda #$00			;
	sec				;
	sbc VertCountrLinear		;Subtract linear counter from 0 and save the results.-->
	sta VertCountrLinear		;Carry will be cleared.
	lda #$00			;
	sbc ObjVertSpeed		;Subtract vertical speed from 0. this will reverse the-->
	sta ObjVertSpeed		;vertical direction of travel(bounce up).
	jmp SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

;Samus has hit the ground after moving downwards. 
*	jsr SFX_SamusWalk		;($CB96)Play walk SFX.
*	jsr StopSamusVertMovement		;($D147)Clear vertical movement data.
	sty SamusGravity		;Clear Samus gravity value.
	beq SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

*	dec ObjectCounter		;1 pixel movement is complete.
	bne ----			;Branch if Samus needs to be moved another pixel.

SamusMoveHorizontally:
  jsr HorzAccelerate		;($E3E5)Horizontally accelerate Samus.
	lda ObjectX			;
	sec				;Calculate Samus' x position on screen.
	sbc ScrollX			;
	sta SamusScrX			;Save Samus' x position.
	lda $00				;Load Samus' current horizontal speed.
	bpl +++				;Branch if moving right.

;Samus is moving left.
	jsr TwosCompliment		;($C3D4)Get twos compliment of horizontal speed.
	ldy SamusInLava			;Is Samus in lava?-->
	beq +				;If not, branch,-->
	lsr				;else cut horizontal speed in half.
	beq Exit10			;Branch to exit if Samus not going to move this frame.

*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
*	jsr MoveSamusLeft		;($E626)Attempt to move Samus 1 pixel to the left.
	jsr CheckStopHorzMvmt		;($E365)Check if horizontal movement needs to be stopped.
	dec ObjectCounter		;1 pixel movement is complete.
	bne -				;Branch if Samus needs to be moved another pixel.

	lda SamusDoorData		;Has Samus entered a door?-->
	beq Exit10			;If not, branch to exit.
	lda #$01			;Door leads to the left.
	bne ++++			;Branch always.

;Samus is moving right.
*	beq Exit10			;Branch to exit if Samus not moving horizontally.
	ldy SamusInLava			;Is Samus in lava?-->
	beq +				;If not, branch,-->
	lsr				;else cut horizontal speed in half.
	beq Exit10			;Branch to exit if Samus not going to move this frame.

*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
*	jsr MoveSamusRight		;($E668)Attempt to move Samus 1 pixel to the right.
	jsr CheckStopHorzMvmt		;($E365)Check if horizontal movement needs to be stopped.
	dec ObjectCounter		;1 pixel movement is complete.
	bne -				;Branch if Samus needs to be moved another pixel.

	lda SamusDoorData		;Has Samus entered a door?-->
	beq Exit10			;If not, branch to exit.
	lda #$00			;
*	sta SamusDoorDir		;Door leads to the right.

Exit10:
	rts				;Exit for routines above and below.

CheckStopHorzMvmt:
  bcs Exit10			;Samus moved successfully. Branch to exit.
	lda #$01			;Load counter with #$01 so this function will not be-->
	sta ObjectCounter		;called again.
	lda SamusGravity		;Is Samus on the ground?-->
	bne Exit10			;If not, branch to exit.
	lda ObjAction			;
	cmp #sa_Roll			;Is Samus rolled into a ball?-->
	beq Exit10			;If so, branch to exit.
	jmp StopHorzMovement		;($CF55)Stop horizontal movement or play walk SFX if stopped.

;-------------------------------------[ Samus vertical acceleration ]--------------------------------

;The following code accelerates/decelerates Samus vertically.  There are 4 possible values for
;gravity used in the acceleration calculation. The higher the number, the more intense the gravity.
;The possible values for gravity are as follows:
;#$38-When Samus has been hit by an enemy.
;#$1A-When Samus is falling.
;#$18-Jump without high jump boots.
;#$12-Jump with high jump boots.

VertAccelerate:
  lda SamusGravity		;Is Samus rising or falling?-->
	bne ++				;Branch if yes.
	lda #$18			;
	sta SamusHorzSpeedMax		;Set Samus maximum running speed.
	lda ObjectY			;
	clc				;
	adc ObjRadY			;Check is Samus is obstructed downwards on y room-->
	and #$07			;positions divisible by 8(every 8th pixel).
	bne +				;
	jsr CheckMoveDown		;($E7AD)Is Samus obstructed downwards?-->
	bcc ++				;Branch if yes.
*	jsr SamusOnElevatorOrEnemy	;($D976)Calculate if Samus standing on elevator or enemy.
	lda SamusOnElevator		;Is Samus on an elevator?-->
	bne +				;Branch if yes.
	lda OnFrozenEnemy		;Is Samus standing on a frozen enemy?-->
	bne +				;Branch if yes.
	lda #$1A			;Samus is falling. Store falling gravity value.
	sta SamusGravity		;

*	ldx #$05			;Load X with maximum downward speed.
	lda VertCountrLinear		;
	clc				;The higher the gravity, the faster this addition overflows-->
	adc SamusGravity		;and the faster ObjVertSpeed is incremented.
	sta VertCountrLinear		;
	lda ObjVertSpeed		;Every time above addition sets carry bit, ObjVertSpeed is-->
	adc #$00			;incremented. This has the effect of speeding up a fall-->
	sta ObjVertSpeed		;and slowing down a jump.
	bpl +				;Branch if Samus is moving downwards.

;Check if maximum upward speed has been exceeded. If so, prepare to set maximum speed.
	lda #$00			;
	cmp VertCountrLinear		;Sets carry bit.
	sbc ObjVertSpeed		;Subtract ObjVertSpeed to see if maximum speed has-->
	cmp #$06			;been exceeded.
	ldx #$FA			;Load X with maximum upward speed.
	bne ++				;Branch always.

;Check if maximum downward speed has been reached. If so, prepare to set maximum speed.
*	cmp #$05			;Has maximum downward speed been reached?-->
*	bcc +				;If not, branch.

;Max verticle speed reached or exceeded. Adjust Samus verticle speed to max.
	jsr StopSamusVertMovement		;($D147)Clear verticle movement data.
	stx ObjVertSpeed		;Set Samus vertical speed to max.

;This portion of the function creates an exponential increase/decrease in verticle speed. This is the
;part of the function that does all the work to make Samus' jump seem natural.
*	lda VertCountrNonLinear		;
	clc				;This function adds itself plus the linear verticle counter-->
	adc VertCountrLinear		;onto itself every frame.  This causes the non-linear-->
	sta VertCountrNonLinear		;counter to increase exponentially.  This function will-->
	lda #$00			;cause Samus to reach maximum speed first in most-->
	adc ObjVertSpeed		;situations before the linear counter.
	sta $00				;$00 stores temp copy of current verticle speed.
	rts				;

;----------------------------------------------------------------------------------------------------

HorzAccelerate:
	lda SamusHorzSpeedMax
	jsr Amul16       ; * 16
	sta $00
	sta $02
	lda SamusHorzSpeedMax
	jsr Adiv16       ; / 16
	sta $01
	sta $03

	lda HorzCountrLinear
	clc
	adc SamusHorzAccel
	sta HorzCountrLinear
	tax
	lda #$00
	bit SamusHorzAccel
	bpl +				;Branch if Samus accelerating to the right.

	lda #$FF

*   adc ObjHorzSpeed
	sta ObjHorzSpeed
	tay
	bpl +				;Branch if Samus accelerating to the right.

	lda #$00
	sec
	sbc HorzCountrLinear
	tax
	lda #$00
	sbc ObjHorzSpeed
	tay
	jsr Negate0Resolve1

*   cpx $02
	tya
	sbc $03
	bcc +
	lda $00
	sta HorzCountrLinear
	lda $01
	sta ObjHorzSpeed
*   lda HorzCountrNonLinear
	clc
	adc HorzCountrLinear
	sta HorzCountrNonLinear
	lda #$00
	adc ObjHorzSpeed
	sta $00				;$00 stores temp copy of current horizontal speed.
	rts				;

Negate0Resolve1:  
	lda #$00		; load 0
	sec				; set carry flag
	sbc $00			; negate $00
	sta $00			; 
	lda #$00		; load 0
	sbc $01			; 
	sta $01			; store result back, negating $1 (with possible extra -1 if $0 != 0)
	rts				; return

;----------------------------------------------------------------------------------------------------

;Attempt to move Samus one pixel up.

MoveSamusUp:
	lda ObjectY					;Get Samus' y position in room.
	sec							;
	sbc ObjRadY					;Subtract Samus' vertical radius.
	and #$07					;Check if result is a multiple of 8. If so, branch to-->
	bne +						;Only call crash detection every 8th pixel.
	jsr CheckMoveUp				;($E7A2)Check if Samus obstructed UPWARDS.-->
	bcc +++++++					;If so, branch to exit(can't move any further).
*   lda ObjAction			;
	cmp #sa_Elevator			;Is Samus riding elevator?-->
	beq +						;If so, branch.
	jsr SamusOnElevatorOrEnemy	;($D976)Calculate if Samus standing on elevator or enemy.
	lda SamusHit
	and #$42
	cmp #$42
	clc
	beq ++++++
*   lda SamusScrY
	cmp #$66					; reached up scroll limit?
	bcs +	   					; branch if not
	jsr ScrollUp
	bcc ++
*    dec SamusScrY
*	lda ObjectY
	bne ++
	lda ScrollDir
	and #$02
	bne +
	jsr ToggleSamusHi       	; toggle 9th bit of Samus' Y coord
*       lda #240
	sta ObjectY
*	dec ObjectY
	inc SamusJumpDsplcmnt
	sec
*	rts

; attempt to move Samus one pixel down

MoveSamusDown:
	lda ObjectY
	clc
	adc ObjRadY
	and #$07
	bne +		   ; only call crash detection every 8th pixel
	jsr CheckMoveDown       ; check if Samus obstructed DOWNWARDS
	bcc +++++++	 ; exit if yes
*       lda ObjAction
	cmp #sa_Elevator	; is Samus in elevator?
	beq +
	jsr SamusOnElevatorOrEnemy
	lda SamusOnElevator
	clc
	bne ++++++
	lda OnFrozenEnemy
	bne ++++++
*       lda SamusScrY
	cmp #$84	; reached down scroll limit?
	bcc +	   ; branch if not
	jsr ScrollDown
	bcc ++
*       inc SamusScrY
*	lda ObjectY
	cmp #239
	bne ++
	lda ScrollDir
	and #$02
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
*       lda #$FF
	sta ObjectY
*	inc ObjectY
	dec SamusJumpDsplcmnt
	sec
*	rts

; Attempt to scroll UP

ScrollUp:
	lda ScrollDir	; load scroll direction 
	beq +			; 	0? jump ahead
	cmp #$01		; not zero, check to see if we're anything BUT 1
	bne ++++		; 	not 1 either, invalid vertical scroll direction - set carry and leave
	dec ScrollDir	; scroll direction was down - set scroll direction to up
	lda ScrollY		; load current scroll 
	beq +			; 	if it's 0, don't update map Y index
	dec MapIdxY		; 	scroll was not 0, we need to change the map position up one room since we're changing direction and it's still on screen

*   ldx ScrollY				; load Y scroll into X
	bne +					; 	not 0, don't update map Y index
	dec MapIdxY     		; 	was 0, decrement MapY - we need to draw the room above us next
	jsr UpdateRoomNum		; 	update room number in $5A, if room above is valid
	bcs ++   				; 	if function returns CF = 1, no more scroll- restore drawing map to the current room we're in, set carry, and leave
	jsr InvertCurrentPPUNT 	; 	switch PPU to draw to the opposite Name Table
	ldx #240				; 	wrapping around to next room from bottom- adjust to lowest Y pos
*	dex						; 	move the new position up one
	jmp UpdateScrollY		; update Y scroll

*	inc MapIdxY				; restore map index to current room we're in
*	sec						; set carry 
	rts						; return

; Attempt to scroll DOWN

ScrollDown:
	ldx ScrollDir
	dex
	beq +
	bpl +++++
	inc ScrollDir
	lda ScrollY
	beq +
	inc MapIdxY
*   lda ScrollY
	bne +
	inc MapIdxY     ; increment MapY
	jsr UpdateRoomNum  ; put room # at current map pos in $5A
	bcs +++   ; if function returns CF = 1, moving down is not possible
*   ldx ScrollY
	cpx #239
	bne +
	jsr InvertCurrentPPUNT       ; switch to the opposite Name Table
	ldx #$FF
*   inx

UpdateScrollY:  
	stx ScrollY
	jsr UpdateRoomNametable      ; check if it's time to update Name Table
	clc
	rts

*	dec MapIdxY
*	sec
*   rts

UpdateRoomNametable:  
	jsr SetupRoom		; start loading the room, if valid, store final control code into room number 
	ldx RoomNumber		; load the room load control code into X
	inx					; increment room number
	bne -				; 	if it's not 0, we haven't finished loading the room, return
	lda ScrollDir		; room finished loading - load Scroll dir
	and #$02			; isolate horizontal scroll flag
	bne +				; 	not 0, scrolling horizontally- skip ahead
	jmp UnknownE571		; TODO: something to do with updating the nametables vertically
*   jmp UnknownE701		; TODO: something to do with updating the nametables horizontally

; Table

ScrollTileCheckBoundaryTable:
	.byte $07			; check bottom/right edge of next tile when going left or up
	.byte $00			; check top/left edge of next tile when going right or down

;---------------------------------[ Get PPU and RoomRAM addresses ]----------------------------------

PPUAddrs:
	.byte $20			;High byte of nametable #0(PPU).
	.byte $2C			;High byte of nametable #3(PPU)

WRAMAddrs:
	.byte $60			;High byte of RoomRAMA(cart RAM).
	.byte $64			;High byte of RoomRAMB(cart RAM).

GetNameAddrs:
	jsr GetNameTable		;($EB85)Get current name table number.
	and #$01			;Update name table 0 or 3.
	tay				;
	lda PPUAddrs,y			;Get high PPU addr of nametable(dest).
	ldx WRAMAddrs,y			;Get high cart RAM addr of nametable(src).
	rts				;

;----------------------------------------------------------------------------------------------------

; check if it's time to update nametable (when scrolling is VERTICAL)

UnknownE571:  
	ldx ScrollDir						; load scroll dir to x (will be 0 when up, 1 when down)
	lda ScrollY							; load current Y scroll to A
	and #$07							; scroll pos %7- check pixel location within tile space
	cmp ScrollTileCheckBoundaryTable,x	; check if we hit the edge of the next tile 
	bne --	   							; 	exit if not equal (no nametable update)

UnknownE57C:  
	ldx ScrollDir			;
	cpx TempScrollDir		;Still scrolling same direction when room was loaded?-->
	bne --					;If not, branch to exit.
	lda ScrollY
	and #$F8				; keep upper 5 bits
	sta $00
	lda #$00
	asl $00
	rol
	asl $00
	rol

UnknownE590:  
	sta $01	 ; $0001 = (ScrollY & 0xF8) << 2 = row offset
	jsr GetNameAddrs
	ora $01
	sta $03
	txa
	ora $01
	sta $01
	lda $00
	sta $02
	lda ScrollDir
	lsr		; A = 0 if vertical scrolling, 1 if horizontal
	tax
	lda Table01,x
	sta $04
	ldy #$01
	sty PPUDataPending      ; data pending = YES
	dey
	ldx PPUStrIndex
	lda $03
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	lda $02
	jsr WritePPUByte
	lda $04
	jsr SeparateControlBits		;($C3C6)
*       lda ($00),y
	jsr WritePPUByte
	sty $06
	ldy #$01	; WRAM pointer increment = 1...
	bit $04	 ; ... if bit 7 (PPU inc) of $04 clear
	bpl +
	ldy #$20	; else ptr inc = 32
*       jsr AddYToPtr00			;($C2A8)
	ldy $06
	dec $05
	bne --
	stx PPUStrIndex
	jsr EndPPUString

Table01:
	.byte $20			;Horizontal write. PPU inc = 1, length = 32 tiles.
	.byte $9E			;Vertical write... PPU inc = 32, length = 30 tiles.

;---------------------------------[Write PPU attribute table data ]----------------------------------

WritePPUAttribTbl:
  ldx #$C0			;Low byte of First row of attribute table.
	lda RoomNumber			;
	cmp #$F2			;Is this the second pass through the routine?-->
	beq +				;If so, branch.
	ldx #$E0			;Low byte of second row of attribute table.
*	stx $00				;$0000=RoomRAM atrrib table starting address.
	stx $02				;$0002=PPU attrib table starting address.
	jsr GetNameAddrs		;($E564)Get name table addr and corresponding RoomRAM addr.
	ora #$03			;#$23 for attrib table 0, #$2F for attrib table 3.
	sta $03				;Store results.
	txa				;move high byte of RoomRAM to A.
	ora #$03			;#$63 for RoomRAMA, #$67 for RoomRAMB(Attrib tables).
	sta $01				;Store results.
	lda #$01			;
	sta PPUDataPending		;Data pending = YES.
	ldx PPUStrIndex			;Load current index into PPU strng to append data.
	lda $03				;Store high byte of starting address(attrib table).
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	lda $02				;Store low byte of starting address(attrib table).
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	lda #$20			;Length of data to write(1 row of attrib data).
	sta $04				;
	jsr WritePPUByte		;($C36B)Write control byte. Horizontal write.
	ldy #$00			;Reset index into data string.
*	lda ($00),y			;Get data byte.
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	iny				;Increment to next attrib data byte.
	dec $04				;
	bne -				;Loop until all attrib data loaded into PPU.
	stx PPUStrIndex			;Store updated PPU string index.
	jsr EndPPUString		;($C376)Append end marker(#$00) and exit writing routines.

;----------------------------------------------------------------------------------------------------

; attempt to move Samus one pixel left

MoveSamusLeft:
	lda ObjectX
	sec
	sbc ObjRadX
	and #$07
	bne +		   ; only call crash detection every 8th pixel
	jsr CheckMoveLeft       ; check if player is obstructed to the LEFT
	bcc +++++	 ; branch if yes! (CF = 0)
*   jsr SamusOnElevatorOrEnemy
	lda SamusHit
	and #$41
	cmp #$41
	clc
	beq ++++
	lda SamusScrX
	cmp #$71	; reached left scroll limit?
	bcs +	   ; branch if not
	jsr ScrollLeft
	bcc ++
*       dec SamusScrX
*	lda ObjectX
	bne +
	lda ScrollDir
	and #$02
	beq +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       dec ObjectX
	sec
	rts

; crash with object on the left

*	lda #$00
	sta SamusDoorData
	rts

; attempt to move Samus one pixel right

MoveSamusRight:
	lda ObjectX
	clc
	adc ObjRadX
	and #$07
	bne +		   ; only call crash detection every 8th pixel
	jsr CheckMoveRight      ; check if Samus is obstructed to the RIGHT
	bcc +++++       ; branch if yes! (CF = 0)
*   jsr SamusOnElevatorOrEnemy
	lda SamusHit
	and #$41
	cmp #$40
	clc
	beq ++++
	lda SamusScrX
	cmp #$8F	; reached right scroll limit?
	bcc +	   ; branch if not
	jsr ScrollRight
	bcc ++
*       inc SamusScrX
*	inc ObjectX      ; go right, Samus!
	bne +
	lda ScrollDir
	and #$02
	beq +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       sec
	rts

; crash with object on the right

*	lda #$00
	sta SamusDoorData
	rts

; Attempt to scroll LEFT

ScrollLeft:
	lda ScrollDir
	cmp #$02
	beq +
	cmp #$03
	bne ++++
	dec ScrollDir
	lda ScrollX
	beq +
	dec MapIdxX
*       lda ScrollX
	bne +
	dec MapIdxX     ; decrement MapX
	jsr UpdateRoomNum  ; put room # at current map pos in $5A
	bcs ++  ; if function returns CF=1, scrolling left is not possible
	jsr InvertCurrentPPUNT       ; switch to the opposite Name Table
*       dec ScrollX
	jsr UpdateRoomNametable       ; check if it's time to update Name Table
	clc
	rts

*	inc MapIdxX
*	sec
	rts

; Attempt to scroll RIGHT

ScrollRight:
	lda ScrollDir
	cmp #$03
	beq +
	cmp #$02
	bne +++++
	inc ScrollDir
	lda ScrollX
	beq +
	inc MapIdxX
*       lda ScrollX
	bne +
	inc MapIdxX
	jsr UpdateRoomNum  ; put room # at current map pos in $5A
	bcs +++   ; if function returns CF=1, scrolling right is not possible
*       inc ScrollX
	bne +
	jsr InvertCurrentPPUNT       ; switch to the opposite Name Table
*	jsr UpdateRoomNametable       ; check if it's time to update Name Table
	clc
	rts

*	dec MapIdxX
*	sec
*       rts

TileBoundaryEdgeTable:
	.byte $07
	.byte $00

; check if it's time to update nametable (when scrolling is HORIZONTAL)

UnknownE701:  
	ldx ScrollDir					; load scroll direction into X
	lda ScrollX						; load horizontal scroll value into A
	and #$07						; keep lower 3 bits (position within sinlge tile)
	cmp TileBoundaryEdgeTable-2,x	; compare to either 07 (right/bottom) or 00 (left/top) of tile boundary, based on scroll direction
	bne -	  						; 	inside tile - can't continue load

UnknownE70C:  						
	ldx ScrollDir					; was on boundary edge,load scroll direction back into X
	cpx TempScrollDir				; comp with the cached scroll direction
	bne -							; 	scroll direction changed at some point where we have a mismatch at this point, don't load 
	lda ScrollX						; load X scroll back into A
	and #$F8						; keep upper five bits
	jsr Adiv8       				; / 8 (make 'em lower five)
	sta $00
	lda #$00
	jmp UnknownE590

;---------------------------------------[ Get room number ]-------------------------------------------

;Gets room number at current map position. Sets carry flag if room # at map position is FF.
;If valid room number, the room number is stored in $5A.

UpdateRoomNum:
	lda ScrollDir			;
	lsr				;Branch if scrolling vertical.
	beq +				;

	rol				;Restore value of a
	adc #$FF			;A=#$01 if scrolling left, A=#$02 if scrolling right.
	pha				;Save A.
	jsr OnNameTable0		;($EC93)Y=1 if name table=0, Y=0 if name table=3.
	pla				;Restore A.
	and $006C,y			;
	sec				;
	bne +++++			;Can't load room, a door is in the way. This has the-->
					;effect of stopping the scrolling until Samus walks-->
					;through the door(horizontal scrolling only).

*	lda MapIdxY			;Map pos y.
	jsr Amul16			;($C2C5)Multiply by 16.
	sta $00				;Store multiplied value in $00.
	lda #$00			;
	rol				;Save carry, if any.
	rol $00				;Multiply value in $00 by 2.
	rol				;Save carry, if any.
	sta $01				;
	lda $00				;
	adc MapIdxX			;Add map pos X to A.
	sta $00				;Store result.
	lda $01				;
	adc #$70			;Add #$7000 to result.
	sta $01				;$0000 = (MapY*32)+MapX+#$7000.
	ldy #$00			;
	lda ($00),y			;Load room number.
	cmp #$FF			;Is it unused?-->
	beq ++++			;If so, branch to exit with carry flag set.

	sta RoomNumber			;Store room number.

*	cmp AreaSpecialRoomTable,y			;Is it a special room?-->
	beq +				;If so, branch to set flag to play item room music.
	iny				;
	cpy #$07			;
	bne -				;Loop until all special room numbers are checked.

	lda ItemRoomMusicStatus		;Load item room music status.
	beq ++				;Branch if not in special room.
	lda #$80			;Ptop playing item room music after next music start.
	bne ++				;Branch always.

*	lda #$01			;Start item room music on next music start.
*	sta ItemRoomMusicStatus		;
	clc				;Clear carry flag. was able to get room number.
*	rts				;

;-----------------------------------------------------------------------------------------------------

CreateEnemyYRadLowerBound:
	ldx PageIndex					; load enemy index
	lda EnRadY,x					; load enemy radius in Y
	clc								; clear carry
	adc #$08						; add 8 to radius - in current example, will create a radius that reaches next tile below
	jmp CreateEnemyCollisionCheck	; jump ahead


CreateEnemyYRadUpperBound:
	ldx PageIndex	;load enemy index
	lda #$00		;load 0
	sec				;set carry 
	sbc EnRadY,x	;subtract Y radius - negates radius
	
	
CreateEnemyCollisionCheck:  
	sta $02							; store adjusted Y radius into $02
	lda #$08						; load 8
	sta $04							; store that into $04
	jsr MakeEnemyLocationStruct98B	; make enemy location struct 
	lda EnRadX,x					; load enemy Horizontal radius in X
	jmp UnknownE7BD					; 

MakeEnemyLocationStruct98B:  
	lda EnXRoomPos,x		
	sta $09     ; X coord
	lda EnYRoomPos,x
	sta $08     ; Y coord
	lda EnNameTable,x
	sta $0B     ; hi coord
	rts

CheckMoveUp:
	ldx PageIndex						; load object index
	lda ObjRadY,x						; load object hit box Y radius
	clc									; clear carry
	adc #$08							; add 8 (down
	jmp +

CheckMoveDown:
	ldx PageIndex
	lda #$00
	sec
	sbc ObjRadY,x
*   sta $02								; store adjusted hitbox radius into 2
	jsr MakeObjectLocationStruct98B		; make locaiton struct for object
	lda ObjRadX,x						; load object X radius into A

UnknownE7BD:  
	bne +			; X radius is not 0, continue
	sec				; 	else, set carry flag - invalid x radius, no collision
	rts				;	return

*   sta $03			; has x radius - store to 03 (Y is in 02, #8 is in 04)
	tay				; put x radius in Y 
	ldx #$00		; load #0 into x
	lda $09			; load cached enemy room x pos 
	sec				; set carry 
	sbc $03			; subtract x radius
	and #$07		; %8 the result
	beq +			; 	0, radius reaches tile edge - jump ahead
	inx				; 	else, inx first
*   jsr UnknownE8CE ; jump to routine
	sta $04			; store the updated X value into 4
	jsr UnknownE90F 
	ldx #$00
	ldy #$08
	lda $00
UnknownE7DE:  
	bne +++			; if A wasn't 0, set carry and return - no collision
	stx $06			; store X into 6
	sty $07			; store Y into 7
	ldx $04			; load X from $04

; object<-->background crash detection

UnknownE7E6:  		
	jsr MakeWRAMPtr 				; set up ptr in $0004
	ldy #$00
	lda ($04),y     				; get tile value
	cmp #$4E						; door tile?
	beq ProcessObjectDoorCrash		; 	door tile- deal with that.
	jsr MotherBrainCollision		; else, jump to check if Mother brain was collided with
	jsr SetCarryIfCollisionTile		; check if we hit a normal collision instead
	bcc Exit16      				; 	no collision? leave
	cmp #$A0						; check against walkable tiles
	bcs IsWalkableTile				; 	walkable tile? process that
	jmp BulletIsBlastTile  				; tile is $80-$9F (blastable tiles) - should return since bullets aren't updating...

IsWalkableTile:
	ldy IsSamus					; load up if this object is samus
	beq ++						; 	not samus? jump ahead
	dey	     					; we're samus, set Y back to 0
	sty SamusDoorData			; reset samus door data
	cmp #$A0					; crash with tile #$A0? (scroll toggling door)
	beq +						; 	if vertical toggle, only increment door data once
	cmp #$A1					; crash with tile #$A1? (horizontal scrolling door)
	bne ++						; 	neither? jump to x dec - not a door tile
	inc SamusDoorData			; increment once for horizontal
*   inc SamusDoorData			; twice for vertical
*	dex							; dec x
	beq +						; if 0, set carry and leave
	jsr UnknownE98E				; else, 
	jmp UnknownE7E6

*	sec	     					; no crash
	Exit16:
	rts

ProcessObjectDoorCrash:  
	ldx UpdatingProjectile		; load projectile update
	beq ClcExit					; if not updating projectile, exit and clear carry
	ldx #$06					; load 6 to x - we're about to loop 4 times (06, 04, 02, 00)
*   lda $05						; load hi byte of projectile position (0110 0nyy)
	eor DoorWRAMPos + 1,x		; flip bits with DoorPositon hi byte
	and #$04					; and with 0000 0100 to isolate n, the name table (the eor will be 0 if they were the same nametable, 1 if not)
	bne +++						; 	not the same name table- go to the next door
	lda $04						; same name table - load lo byte of projectile position (yyyx xxxx)
	eor DoorWRAMPos,x			; flip bits with Door position lo byte
	and #$1F					; isolate X position bits (the eor will return 0 if it's the same X position)
	bne +++						; 	not the same X position- go to the next door
	txa							; same X position and name table - put the X index into A
	jsr Amul8       			; A will be even numbers between 6 and 0 - 0000 0aa0 -> 00aa 0000
	ora #$80					; 10aa 0000
	tay							; we converted our index to the correct offset in the object table for doors
	lda ObjAction,y				; load the door state
	beq +++						; 	door inactive? go to the next door
	lda DoorType,y				; load up byte 7 of the door data- 00 for red door, 01 for blue door
	lsr							; get the lowest bit into carry
	bcs ++						; 	carry was set? blue door- just open
	ldx PageIndex				; carry not set red door. load current object index (our bullet)
	lda ObjAction,x				; load the current bullet update type 
	eor #$0B					; flip the lower 1011
	beq +						; 	missle update type? play missle door hit
	lda ObjAction,x				; load the current bullet action again - if it wasn;t a missle, it might be something else that can damage the door
	eor #$04					; will return 0 iff the byte is 04 - bullet explosion
	bne PlaySnd4				; 	anything else, metal sound
	lda AnimResetIndex,x		; else, we'll load the reset anim for the exploding bullet
	eor #an_MissileExplode		; eor with missle explode
	bne PlaySnd4				;	not a bullet explosion? metal sound
*   lda TriangleSFXFlag			; load the triangle SFX flag
	ora #$02					; set 0000 0010
	sta TriangleSFXFlag			; store that back into the triangle SFX flag for hitting the door 
*	lda #$04					; load 0000 0100
	sta $030A,y					; TODO store into some kinda door data - probably an opening thing
	bne ClcExit					; jump to clear carry and leave- no need to keep looking for the correct door
*	dex							; 
	dex							; dec X to get to the next door index
	bpl ----					; 	still positive? loop
	lda $04						; we never found the door we hit - load the low byte of the bullet position (yyyx xxxx)
	jsr Adiv8       			; yyyx xxxx -> 000y yyxx
	and #$01					; 000y yyxx -> 0000 000x	
	tax							; move A to X
	inc $0366,x					; increment the byte at either 0366 or 0367 - based on which quater of the screen the bullet was in
ClcExit:
	clc							; clear carry
	rts							; leave

PlaySnd4:
	jmp SFX_Metal

CheckMoveLeft:
	ldx PageIndex
	lda ObjRadX,x
	clc
	adc #$08
	jmp +

CheckMoveRight:
	ldx PageIndex
	lda #$00
	sec
	sbc ObjRadX,x
*   sta $03
	jsr MakeObjectLocationStruct98B
	ldy ObjRadY,x
	
UnknownE89B:  
	bne +				; jump ahead if the Y radius of the hitbox was not 0
	sec					; 	else, no y radius- invliad collision check, return
	rts					; 	return

*   sty $02				; store the Y radius into $2
	ldx #$00			; load #0 into x
	lda $08				; load $8, the cached eneym Y pos
	sec					; clear carry
	sbc $02				; subtract the Y radius to pull it up
	and #$07			; %8
	beq +				; 	we're at a tile edge
	inx					; 	inc x
*   jsr UnknownE8CE		;
	sta $04
	jsr UnknownE90F
	ldx #$08
	ldy #$00
	lda $01
	jmp UnknownE7DE

MakeObjectLocationStruct98B:
	lda ObjectHi,x
	sta $0B
	lda ObjectY,x
	sta $08
	lda ObjectX,x
	sta $09
	rts

UnknownE8CE:  
	eor #$FF		; 
	clc				; 
	adc #$01		; 
	and #$07		; get the difference from our original intra-tile position and 8 (unless it's 0, then it is 0)
	sta $04			; store out into 4
	tya				; put the radius we stored off into Y into X
	asl				; double it
	sec				; set carry flag
	sbc $04			; subtract the value we got from the difference above
	bcs +			; 	carry set if double radius was greater than the difference- normal stuff, jum pahead
	adc #$08		; 	carry not set, difference was LARGER than the radius- add 8 back to get it positive
*   tay				; put this weird adjusted radius back into Y
	lsr
	lsr
	lsr				; div 8?
	sta $04			; store back into 4??
	tya				; restore the undivided A?
	and #$07		; %8 again?
	beq +			; 	0? skip x inc 
	inx				; 	not 0? inc x
*   txa				; move x into A
	clc				; clear carry
	adc $04			; add the weird adjusted radius into 4
	rts				; return

UnknownE8F1:
	ldx PageIndex
	lda EnRadX,x
	clc
	adc #$08
	jmp UnknownE904

UnknownE8FC:
	ldx PageIndex					; load enemy index
	lda #$00						; load #0
	sec								;
	sbc EnRadX,x					; invert enemy X radius
UnknownE904:  
	sta $03							; store off into $03
	jsr MakeEnemyLocationStruct98B	; make enemy location struct
	ldy EnRadY,x					; load y radius into y
	jmp UnknownE89B					

UnknownE90F:  
	lda $02				; load Y radius
	bpl ++				; 	positive, jump ahead
	jsr UnknownE95F		; negative, jump ahead and do a thing
	bcs +				;	carry flag was set, subtraction was normal, jump ahead 
	cpx #$F0			; x holds the position of the edge of the hitbox on an overflow - compare to F0 (very bottom of table)
	bcc +++				; 	less than, jump way ahead
*   txa					; greater than/equal to- put the location in A
	adc #$0F			; add 0F to loop it back, or put it at the very tip of the attribute table
	jmp UnknownE934

*	jsr UnknownE95F
	lda $08
	sec
	sbc $02
	tax
	and #$07
	sta $00
	bcs +
	txa
	sbc #$0F
	
UnknownE934:  
	tax
	lda ScrollDir
	and #$02
	bne +
	inc $0B
*	stx $02
	ldx #$00
	lda $03
	bmi +
	dex
*   lda $09
	sec
	sbc $03
	sta $03
	and #$07
	sta $01
	txa
	adc #$00
	beq +
	lda ScrollDir
	and #$02
	beq +
	inc $0B
*   rts

UnknownE95F:  
	lda $08		; load Y position
	sec			; set carry
	sbc $02		; subtract Y radius (up)
	tax			; transfer to X
	and #$07	; %8
	sta $00		; store to $0	
	rts			; return

; MakeWRAMPtr
; ===========
; Makes pointer to WRAM nametable based on object's coordinates.
; In: $02 = ObjectY, $03 = ObjectX, $0B = ObjectHi
; Out: $04 = WRAM pointer

MakeWRAMPtr:
	lda #$18	; load 0001 1000
	sta $05		; store into 5
	lda $02	 	; ObjectY pos
	and #$F8	;   yyyy y000 keep upper 5 bits - truncates to tile index 
	asl			; y yyyy 0000 
	rol $05		;   0011 000y
	asl			; y yyy0 0000
	rol $05		;   0110 00yy
	sta $04		; store into 4- should now be the lower 8 bits of the tile position at the top
	lda $03		; ObjectX pos
	lsr			; 
	lsr			; 
	lsr	   		; A now is the tile index across X
	ora $04		; or with 4
	sta $04		; tile location is now stored between 05 and 04 (0110 00yy yyyx xxxx)
	lda $0B	 	; ObjectYHi
	asl			; 
	asl	   		; A = ObjectYHi * 4
	and #$04	; just in case, clear out other possible bits so we keep 0000 0n00
	ora $05		; or with 5 
	sta $05		; store in 5 (0110 0nyy yyyx xxxx)
	rts			; return 

UnknownE98E:  
	lda $02			; load what I assume to be the position data in 23B
	clc				; clear carry
	adc $06			; add the value stored in 6
	sta $02			; store in two 
	cmp #$F0		; i think this is going to be a movement attempt
	bcc +
	adc #$0F
	sta $02
	lda ScrollDir
	and #$02
	bne +
	inc $0B
*   lda $03
	clc
	adc $07
	sta $03
	bcc +
	lda ScrollDir
	and #$02
	beq +
	inc $0B
*   rts

InvertCurrentPPUNT:  
	lda PPUCNT0ZP		; load PPU control
	eor #$03			; flip NT bits
	sta PPUCNT0ZP		; store back into PPU control
	rts					; return

BulletIsBlastTile:
	ldy UpdatingProjectile
	beq Exit18
IsBlastTile:
	tay						; store tile index into y
	jsr $95BD
	cpy #$98				; if the tile is >= 98
	bcs +++++				; clear carry flag and return - not a blast tile
							; attempt to find a vacant tile slot
	ldx #$C0
*   lda TileRoutine,x
	beq +	   				; 0 = free slot
	jsr Xminus16
	bne -
	lda TileRoutine,x
	bne ++++	 			; no more slots, can't blast tile
*   inc TileRoutine,x
	lda $04
	and #$DE
	sta TileWRAMLo,x
	lda $05
	sta TileWRAMHi,x
	lda InArea
	cmp #$11
	bne +
	cpy #$76
	bne +
	lda #$04
	bne ++
*   tya
	clc
	adc #$10
	and #$3C
	lsr
*	lsr
	sta TileType,x
*	clc

Exit18: 
	rts

;------------------------------------------[ Select room RAM ]---------------------------------------

SelectRoomRAM:
	jsr GetNameTable		;($EB85)Find name table to draw room on.
	asl						;
	asl						;
	ora #$60				;A=#$64 for name table 3, A=#$60 for name table 0.
	sta CartRAMPtr+1		;
	lda #$00				;
	sta CartRAMPtr			;Save two byte pointer to start of proper room RAM.
	rts						;

;------------------------------------[ write   table data ]----------------------------------

AttribTableWrite:
*	lda RoomNumber			;
	and #$0F				;Determine what row of PPU attribute table data, if any,-->
	inc RoomNumber			;to load from RoomRAM into PPU.
	jsr ChooseRoutine		;

;The following table is used by the code above to determine when to write to the PPU attribute table.

	.word ExitSub			; Rts.
	.word WritePPUAttribTbl	; Write first row of PPU attrib data.
	.word ExitSub			; Rts.
	.word WritePPUAttribTbl	; Write second row of PPU attrib data.
	.word RoomFinished		; Finished writing attribute table data.

;-----------------------------------[ Finished writing room data ]-----------------------------------

RoomFinished:
	lda #$FF		;No more tasks to perform on current room.-->
	sta RoomNumber	;Set RoomNumber to #$FF.
*	rts				;

;------------------------------------------[ Setup room ]--------------------------------------------

SetupRoom:
	lda RoomNumber		;Room number.
	cmp #$FF			;
	beq -				;Branch to exit if room is undefined.
	cmp #$FE			;
	beq +				;Branch if empty place holder byte found in room data.
	cmp #$F0			;
	bcs --				;Branch if time to write PPU attribute table data.
	jsr UpdateRoomSpriteInfo	;($EC9B)Update which sprite belongs on which name table.

	jsr ScanForItems		;($ED98)Set up any special items.
	lda RoomNumber			;Room number to load.
	asl				;*2(for loading address of room pointer).
	tay				;
	lda (RoomPtrTable),y		;Low byte of 16-bit room pointer.-->
	sta RoomPtr			;Base copied from $959A to $3B.
	iny				;
	lda (RoomPtrTable),y		;High byte of 16-bit room pointer.-->
	sta RoomPtr+1			;Base copied from $959B to $3C.
	ldy #$00			;
	lda (RoomPtr),y			;First byte of room data.
	sta RoomPal			;store initial palette # to fill attrib table with.
	lda #$01			;
	jsr AddToRoomPtr		;($EAC0)Increment room data pointer.
	jsr SelectRoomRAM		;($EA05)Determine where to draw room in RAM, $6000 or $6400.
	jsr InitTables			;($EFF8)clear Name Table & do initial Attrib table setup.
*	jmp DrawRoom			;($EAAA)Load room contents into room RAM.

;---------------------------------------[ Draw room object ]-----------------------------------------

DrawObject:
	sta $0E				;Store object position byte(%yyyyxxxx).
	lda CartRAMPtr			;
	sta CartRAMWorkPtr		;Set the working pointer equal to the room pointer-->
	lda CartRAMPtr+1		;(start at beginning of the room).
	sta CartRAMWorkPtr+1		;
	lda $0E				;Reload object position byte.
	jsr Adiv16			;($C2BF)/16. Lower nibble contains object y position.-->
	tax				;Transfer it to X, prepare for loop.
	beq +++				;Skip y position calculation loop as y position=0 and-->
					;does not need to be calculated.
*	lda CartRAMWorkPtr		;LoW byte of pointer working in room RAM.
	clc				;
	adc #$40			;Advance two rows in room RAM(one y unit).
	sta CartRAMWorkPtr		;
	bcc +				;If carry occurred, increment high byte of pointer-->
	inc CartRAMWorkPtr+1		;in room RAM.
*	dex				;
	bne --				;Repeat until at desired y position(X=0).

*	lda $0E				;Reload object position byte.
	and #$0F			;Remove y position upper nibble.
	asl				;Each x unit is 2 tiles.
	adc CartRAMWorkPtr		;
	sta CartRAMWorkPtr		;Add x position to room RAM work pointer.
	bcc +				;If carry occurred, increment high byte of room RAM work-->
	inc CartRAMWorkPtr+1		;pointer, else branch to draw object.

;CartRAMWorkPtr now points to the object's starting location (upper left corner)
;on the room RAM which will eventually be loaded into a name table.

*	iny				;Move to the next byte of room data which is-->
	lda (RoomPtr),y			;the index into the structure pointer table.
	tax				;Transfer structure pointer index into X.
	iny				;Move to the next byte of room data which is-->
	lda (RoomPtr),y			;the attrib table info for the structure.
	sta ObjectPal			;Save attribute table info.
	txa				;Restore structure pointer to A.
	asl				;*2. Structure pointers are two bytes in size.
	tay				;
	lda (StructPtrTable),y		;Low byte of 16-bit structure ptr.
	sta StructPtr			;
	iny				;
	lda (StructPtrTable),y		;High byte of 16-bit structure ptr.
	sta StructPtr+1			;
	jsr DrawStruct			;($EF8C)Draw one structure.
	lda #$03			;Move to next set of structure data.
	jsr AddToRoomPtr		;($EAC0)Add A to room data pointer.

;-------------------------------------------[ Draw room ]--------------------------------------------

;The following function draws a room in the room RAM which is eventually loaded into a name table.

DrawRoom:
	ldy #$00				;Zero index.
	lda (RoomPtr),y			;Load byte of room data.-->
	cmp #$FF				;Is it #$FF(end-of-room)?-->
	beq EndOfRoom			;If so, branch to exit.
	cmp #$FE				;Place holder for empty room objects(not used).
	beq +					;
	cmp #$FD				;is A=#$FD(end-of-objects)?-->
	bne DrawObject			;If not, branch to draw room object.-->
	beq EndOfObjs			;Else branch to set up enemies/doors.
*	sta RoomNumber			;Store #$FE if room object is empty.
	lda #$01				;Prepare to increment RoomPtr.

;-------------------------------------[ Add A to room pointer ]--------------------------------------

AddToRoomPtr:
	clc				;Prepare to add index in A to room pointer.
	adc RoomPtr			;
	sta RoomPtr			;
	bcc +				;Did carry occur? If not branch to exit.
	inc RoomPtr+1			;Increment high byte of room pointer if carry occured.
*	rts				;

;----------------------------------------------------------------------------------------------------

EndOfObjs:
	lda RoomPtr			;
	sta $00				;Store room pointer in $0000.
	lda RoomPtr+1		;
	sta $01				;
	lda #$01			;Prepare to increment to enemy/door data.

EnemyLoop:
	jsr AddToPtr00		;($EF09)Add A to pointer at $0000.
	ldy #$00			;
	lda ($00),y			;Get first byte of enemy/door data.
	cmp #$FF			;End of enemy/door data?-->
	beq EndOfRoom		;If so, branch to finish room setup.
	and #$0F			;Discard upper four bits of data.
	jsr ChooseRoutine	;Jump to proper enemy/door handling routine.

;Pointer table to code.

	.word ExitSub			;($C45C)Rts.
	.word LoadEnemy			;($EB06)Room enemies.
	.word LoadDoor			;($EB8C)Room doors.
	.word ExitSub			;($C45C)Rts.
	.word LoadElevator		;($EC04)Elevator.
	.word ExitSub			;($C45C)Rts.
	.word LoadStatues		;($EC2F)Kraid & Ridley statues.
	.word LoadPipeSpawner	;($EC57)Regenerating enemies(such as Zeb).

EndOfRoom:
	ldx #$F0				; Prepare for PPU attribute table write.
	stx RoomNumber			;
	lda ScrollDir			;
	sta TempScrollDir		; Make temp copy of ScrollDir.
	and #$02				; Check if scrolling horizontal
	bne +					; 	horizontal scrolling, do E70C 
	jmp UnknownE57C
*   jmp UnknownE70C

LoadEnemy:
	jsr GetEnemyData		;($EB0C)Get enemy data from room data.
	jmp EnemyLoop			;($EAD4)Do next room object.

GetEnemyData:
	lda ($00),y					;Get 1st byte again.
	and #$F0					;Get object slot that enemy will occupy.
	tax							;
	jsr IsSlotTaken				;($EB7A)Check if object slot is already in use.
	bne ++						;Exit if object slot taken.
	iny							;
	lda ($00),y					;Get enemy type.
	jsr SetEnTypeAndAttributes	;($EB28)Load data about enemy.
	ldy #$02					;
	lda ($00),y					;Get enemy initial position(%yyyyxxxx).
	jsr SpawnEnemyAtLocation
	pha
*   pla
*   lda #$03			;Number of bytes to add to ptr to find next room item.
	rts				;

SetEnTypeAndAttributes:	
	pha						; Store A - top two bits are special attributes, bottom 6 are enemy index
	and #$C0				; If MSB is set, the "tough" version of the enemy  
	sta EnSpecialAttribs,x	; is to be loaded(more hit points, except rippers).
	asl						;
	bpl ++					;	If bit 6 isn't set, normal enemy - jump ahead
	lda InArea				; 	bit 6 set- mini boss. Load current area Samus is in(to check if Kraid or-->
	and #$06				; 	Ridley is alive or dead).
	lsr						; 	Use InArea to find status of Kraid/Ridley statue.
	tay						;
	lda MaxMissiles,y		; 	Load status of Kraid/Ridley statue.
	beq +					; 	Branch if Kraid or Ridley needs to be loaded.
	; pla					;
	pla						; 	Mini boss is dead so pull enemy info and last address off-->
	jmp --					; 	stack so next enemy/door item can be loaded.

*   lda #$01				; 	Samus is in Kraid or Ridley's room and the-->
	sta KraidRidleyPresent	; 	mini boss is alive and needs to be loaded.

*	pla						; Restore enemy type data.
	and #$3F				; Keep 6 lower bits to use as index for enemy data tables.
	sta EnDataIndex,x		; Store index byte.
	rts						;

SpawnEnemyAtLocation:  
	tay						;Save enemy position data in Y.
	and #$F0				;Extract Enemy y position.
	ora #$08				;Add 8 pixels to y position so enemy is always on screen. 
	sta EnYRoomPos,x		;Store enemy y position.
	tya						;Restore enemy position data.
	jsr Amul16				;*16 to extract enemy x position.
	ora #$0C				;Add 12 pixels to x position so enemy is always on screen.
	sta EnXRoomPos,x		;Store enemy x position.
	lda #$01				;
	sta EnStatus,x			;Indicate object slot is taken.
	lda #$00
	sta $0404,x
	jsr GetNameTable		;($EB85)Get name table to place enemy on.
	sta EnNameTable,x		;Store name table.

UnknownEB6E:
	ldy EnDataIndex,x		; Load y with index to enemy data.
	asl $0405,x				; pop out the direction check 
	jsr UnknownFB7B			; shove in the new direction check from 977B
	jmp EnemyInitHealth		

IsSlotTaken:
	lda EnStatus,x			; 
	beq +					; 	slot is 0, free slot - leave
	lda $0405,x				; slot taken - load enemy status flags
	and #$02				; isolate on screen flag - if offscreen, slot is free
*   rts

;------------------------------------------[ Get name table ]----------------------------------------

;The following routine is small but is called by several other routines so it is important and
;requires some explaining to understand its function.  First of all, as Samus moves from one room
;to the next, she is also moving from one name table to the next.  Samus does not move from one
;name table to the next as one might think. Samus moves diagonally through the name tables. To
;understand this concept, one must first know how the name tables are arranged.  They are arranged
;like so:
;
; +-----+-----+							 						 +-----+-----+
; |     |     | The following is an incorrect example of how	 |     |     |
; |  2  |  3  | Samus goes from one name table to the next-----> |  2  |  3  |
; |     |     |							 						 |     |     |
; +-----+-----+													 +-----+-----+
; |     |     |													 |     |     |
; |  0  |  1  |				     			  INCORRECT!------>  |  0<-|->1  |
; |     |     |													 |     |     |
; +-----+-----+													 +-----+-----+
;
;The following are examples of how the name tables are properly traversed while walking through rooms:
;
;	   +-----+-----+				     				 +-----+-----+
;	   |     |     |				     				 |     |     |
;	   |  2  | ->3 |				     				 |  2  |  3<-|-+
;	   |     |/    |				     				 |     |     | |
;	   +-----+-----+     <--------CORRECT!-------->      +-----+-----+ |
;	   |    /|     |				     				 |     |     | |
;	   | 0<- |  1  |				   				   +-|->0  |  1  | |
;	   |     |     |				   				   | |     |     | |
;	   +-----+-----+				   				   | +-----+-----+ |
;							   						   +---------------+
;
;The same diagonal traversal of the name tables illustrated above applies to vetricle traversal as
;well. Since Samus can only travel between 2 name tables and not 4, the name table placement for
;objects is simplified.  The following code determines which name table to use next:

GetNameTable:
	lda PPUCNT0ZP			;
	eor ScrollDir			;Store #$01 if object should be loaded onto name table 3-->
	and #$01				;store #$00 if it should be loaded onto name table 0.
	rts						;

;----------------------------------------------------------------------------------------------------

; LoadDoor
; ========

	LoadDoor:
	jsr UnknownEB92
*       jmp EnemyLoop    ; do next room object

UnknownEB92:  iny
	lda ($00),y     ; door info byte
	pha
	jsr Amul16      ; CF = door side (0=right, 1=left)
	php
	lda MapIdxX
	clc
	adc MapIdxY
	plp
	rol
	and #$03
	tay
	ldx UnknownEC00,y
	pla	     ; retrieve door info
	and #$03
	sta $0307,x     ; door palette
	tya
	pha
	lda $0307,x
	cmp #$01
	beq ++
	cmp #$03
	beq ++
	lda #$0A
	sta $09
	ldy MapIdxX
	txa
	jsr Amul16       ; * 16
	bcc +
	dey
*       tya
	jsr UnknownEE41
	jsr CheckForItem
	bcs ++
*	lda #$01
	sta ObjAction,x
*       pla
	and #$01	; A = door side (0=right, 1=left)
	tay
	jsr GetNameTable		;($EB85)
	sta ObjectHi,x
	lda DoorXs,y    ; get door's X coordinate
	sta ObjectX,x
	lda #$68	; door Y coord is always #$68
	sta ObjectY,x
	lda UnknownEBFE,y
	tay
	jsr GetNameTable		;($EB85)
	eor #$01
	tax
	tya
	ora DoorOnNameTable3,x
	sta DoorOnNameTable3,x
	lda #$02
	rts

DoorXs:
	.byte $F0	 ; X coord of RIGHT door
	.byte $10	 ; X coord of LEFT door
UnknownEBFE:  
	.byte $02
	.byte $01
UnknownEC00:  
	.byte $80
	.byte $B0
	.byte $A0
	.byte $90

; LoadElevator
; ============

LoadElevator:
	jsr UnknownEC09
	bne ----	   			; branch always

UnknownEC09:  

	lda ElevatorStatus
	bne +	   				; exit if elevator already present
	iny						
	lda ($00),y
	sta $032F
	ldy #$83
	sty $032D       		; elevator Y coord
	lda #$80
	sta $032E       		; elevator X coord
	jsr GetNameTable		;($EB85)
	sta $032C       		; high Y coord
	lda #$23
	sta $0323       		; elevator frame
	inc ElevatorStatus		;1
*   lda #$02
	rts

; LoadStatues
; ===========

LoadStatues:
	jsr GetNameTable		;
	sta $036C
	lda #$40
	ldx RidleyStatueStatus
	bpl +	   				; branch if Ridley statue not hit
	lda #$30
*   sta $0370
	lda #$60
	ldx KraidStatueStatus
	bpl +	   				; branch if Kraid statue not hit
	lda #$50
*   sta $036F
	sty $54
	lda #$01
	sta $0360
*	jmp EnemyLoop   		; do next room object

LoadPipeSpawner:
	ldx #$20
*   txa
	sec
	sbc #$08
	bmi +
	tax
	ldy EnSpawnerStatus,x
	iny
	bne -
	ldy #$00
	lda ($00),y
	and #$F0
	sta EnSpawnerEnSlot,x
	iny
	lda ($00),y
	sta EnSpawnerStatus,x
	iny
	lda ($00),y
	tay
	and #$F0
	ora #$08
	sta EnSpawnerYRoomPos,x
	tya
	jsr Amul16      		; * 16
	ora #$00
	sta EnSpawnerXRoomPos,x
	jsr GetNameTable		;($EB85)
	sta EnSpawnerNametable,x
*   lda #$03
	bne ---

OnNameTable0:
	lda PPUCNT0ZP			;
	eor #$01				;If currently on name table 0,-->
	and #$01				;return #$01. Else return #$00.
	tay						;
	rts						;

UpdateRoomSpriteInfo:
	ldx ScrollDir
	dex
	ldy #$00
	jsr UpdateDoorData		;($ED51)Update name table 0 door data.
	iny
	jsr UpdateDoorData		;($ED51)Update name table 3 door data.
	ldx #$50
	jsr GetNameTable		;($EB85)
	tay
*   tya
	eor EnNameTable,x
	lsr
	bcs +
	lda $0405,x
	and #$02
	bne +
	sta EnStatus,x
*   jsr Xminus16
	bpl --
	ldx #$18
*   tya
	eor $B3,x
	lsr
	bcs +
	lda #$00
	sta $B0,x
*   txa
	sec
	sbc #$08
	tax
	bpl --
	jsr DeactivateOffscreenDoors
	jsr UnknownED5B
	jsr GetNameTable		;(EB85)
	asl
	asl
	tay
	ldx #$C0
*   tya
	eor TileWRAMHi,x
	and #$04
	bne +
	sta $0500,x
*   jsr Xminus16
	cmp #$F0
	bne --
	tya
	lsr
	lsr
	tay
	ldx #$D0
	jsr UnknownED7A
	ldx #$E0
	jsr UnknownED7A
	ldx #$F0
	jsr UnknownED7A
	tya
	sec
	sbc $032C
	bne +
	sta ElevatorStatus
*   ldx #$1E
*   lda $0704,x
	bne +
	lda #$FF
	sta $0700,x
*   txa
	sec
	sbc #$06
	tax
	bpl --
	cpy $036C
	bne +
	lda #$00
	sta $0360
*   ldx #$18
*   tya
	cmp EnSpawnerNametable,x
	bne +
	lda #$FF
	sta EnSpawnerStatus,x
*   txa
	sec
	sbc #$08
	tax
	bpl --
	ldx #$00
	jsr UnknownED8C
	ldx #$08
	jsr UnknownED8C
	jmp $95AE

UpdateDoorData:
	txa				;
	eor #$03			;
	and $006C,y			;Moves door info from one name table to the next-->
*	sta $006C,y			;when the room is transferred across name tables.
	rts				;

UnknownED5B:  
	jsr GetNameTable		;($EB85)
	eor #$01
	tay
	lda #$00
	beq -
DeactivateOffscreenDoors:
	ldx #$B0				; for the four doors
*   lda ObjAction,x			; load object action
	beq +					; 	and skip if already inactive
	lda ObjectOnScreen,x	; else, check if on screen
	bne +					; 	on screen, skip  
	sta ObjAction,x			; else, deactivate object
*   jsr Xminus16			; sub 16 from object index
	bmi --					; finish when no longer doing door indexes
	rts						; return

UnknownED7A:  
	lda ObjAction,x
	cmp #$05
	bcc +
	tya
	eor ObjectHi,x
	lsr
	bcs +
	sta ObjAction,x
*   rts

UnknownED8C:  
	tya
	cmp PowerUpNameTable,x
	bne Exit11
	lda #$FF
	sta PowerUpType,x
	
Exit11: 
	rts

;---------------------------------------[ Setup special items ]--------------------------------------

;The following routines look for special items on the game map and jump to
;the appropriate routine to handle those items.

ScanForItems:
	lda SpecItmsTable		;Low byte of ptr to 1st item data.
	sta $00				;
	lda SpecItmsTable+1		;High byte of ptr to 1st item data.

ScanOneItem:
	sta $01				;
	ldy #$00			;Index starts at #$00.
	lda ($00),y			;Load map Ypos of item.-->
	cmp MapIdxY			;Does it equal Samus' Ypos on map?-->
	beq +				;If yes, check Xpos too.

	bcs Exit11			;Exit if item Y pos >  Samus Y Pos.
	iny				;
	lda ($00),y			;Low byte of ptr to next item data.
	tax				;
	iny				;
	and ($00),y			;AND with hi byte of item ptr.
	cmp #$FF			;if result is FFh, then this was the last item-->
	beq Exit11			;(item ptr = FFFF). Branch to exit.

	lda ($00),y			;High byte of ptr to next item data.
	stx $00				;Write low byte for next item.
	jmp ScanOneItem			;Process next item.

*	lda #$03			;Get ready to look at byte containing X pos.
	jsr AddToPtr00			;($EF09)Add 3 to pointer at $0000.

ScanItemX:
	ldy #$00			;
	lda ($00),y			;Load map Xpos of object.-->
	cmp MapIdxX			;Does it equal Samus' Xpos on map?-->
	beq +				;If so, then load object.
	bcs Exit11			;Exit if item pos X > Samus Pos X.

	iny				;
	jsr AnotherItem			;($EF00)Check for another item on same Y pos.
	jmp ScanItemX			;Try next X coord.

*	lda #$02			;Move ahead two bytes to find item data.

ChooseHandlerRoutine:
	jsr AddToPtr00			;($EF09)Add A to pointer at $0000.
	ldy #$00			;
	lda ($00),y			;Object type
	and #$0F			;Object handling routine index stored in 4 LSBs.
	jsr ChooseRoutine		;($C27C)Load proper handling routine from table below.

;Handler routines jumped to by above code.

	.word ExitSub			;($C45C)rts.
	.word SqueeptHandler		;($EDF8)Some squeepts.
	.word PowerUpHandler		;($EDFE)power-ups.
	.word SpecEnemyHandler		;($EE63)Special enemies(Mellows, Melias and Memus).
	.word ElevatorHandler		;($EEA1)Elevators.
	.word CannonHandler		;($EEA6)Mother brain room cannons.
	.word MotherBrainHandler	;($EEAE)Mother brain.
	.word ZeebetiteHandler		;($EECA)Zeebetites.
	.word RinkaHandler		;($EEEE)Rinkas.
	.word DoorHandler		;($EEF4)Some doors.
	.word PaletteHandler		;($EEFA)Background palette change.

;---------------------------------------[ Squeept handler ]------------------------------------------

SqueeptHandler:
	jsr GetEnemyData		;($EB0C)Load Squeept data.
*	jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

;--------------------------------------[ Power-up Handler ]------------------------------------------

PowerUpHandler:
	iny				;Prepare to store item type.
	ldx #$00			;
	lda #$FF			;
	cmp PowerUpType			;Is first power-up item slot available?-->
	beq +				;if yes, branch to load item.

	ldx #$08			;Prepare to check second power-up item slot.
	cmp PowerUpBType		;Is second power-up item slot available?-->			
	bne ++				;If not, branch to exit.
*	lda ($00),y			;Power-up item type.
	jsr PrepareItemID		;($EE3D)Get unique item ID.
	jsr CheckForItem		;($EE4A)Check if Samus already has item.
	bcs +				;Samus already has item. do not load it.

	ldy #$02			;Prepare to load item coordinates.
	lda $09				;
	sta PowerUpType,x		;Store power-up type in available item slot.
	lda ($00),y			;Load x and y screen positions of item.
	tay				;Save position data for later processing.
	and #$F0			;Extract Y coordinate.
	ora #$08			;+ 8 to find  Y coordinate center.
	sta PowerUpYCoord,x		;Store center Y coord
	tya				;Reload position data.
	jsr Amul16			;($C2C5)*16. Move lower 4 bits to upper 4 bits.
	ora #$08			;+ 8 to find X coordinate center.
	sta PowerUpXCoord,x		;Store center X coord
	jsr GetNameTable		;($EB85)Get name table to place item on.
	sta PowerUpNameTable,x		;Store name table Item is located on.

*	lda #$03			;Get next data byte(Always #$00).
	bne ---				;Branch always to exit handler routines.
	
PrepareItemID:
	sta $09				;Store item type.
	lda MapIdxX			;

UnknownEE41:	sta $07				;Store item X coordinate.
	lda MapIdxY			;
	sta $06				;Store item Y coordinate.
	jmp CreateItemID		;($DC67)Get unique item ID.

CheckForItem:	ldy NumberOfUniqueItems		;
	beq +++				;Samus has no unique items. Load item and exit.
*	lda $07				;
	cmp NumberOfUniqueItems,y	;Look for lower byte of unique item.
	bne +				;
	lda $06				;Look for upper byte of unique item.
	cmp DataSlot,y			;
	beq +++				;Samus already has item. Branch to exit.
*	dey				;
	dey				;
	bne --				;Loop until all Samus' unique items are checked.
*	clc				;Samus does not have the item. It will be placed on screen.
*	rts				;

;-----------------------------------------------------------------------------------------------------

SpecEnemyHandler:
  ldx #$18
	lda RandomNumber1
	adc FrameCount
	sta $8A
*       jsr UnknownEE86
	txa
	sec
	sbc #$08
	tax
	bpl -
	lda $95E4
	sta $6BE9
	sta $6BEA
	lda #$01
	sta $6BE4
*   jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

UnknownEE86:  lda $B0,x
	bne +
	txa
	adc $8A
	and #$7F
	sta $B1,x
	adc RandomNumber2
	sta $B2,x
	jsr GetNameTable		;($EB85)
	sta $B3,x
	lda #$01
	sta $B0,x
	rol $8A
*       rts

ElevatorHandler:
  jsr UnknownEC09
  bne --				;Branch always.

CannonHandler:
  jsr $95B1
	lda #$02
*       jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

MotherBrainHandler:
  jsr $95B4
	lda #$38
	sta $07
	lda #$00
	sta $06
	jsr CheckForItem
	bcc UnknownEEC6
	lda #$08
	sta MotherBrainStatus
	lda #$00
	sta MotherBrainHits
UnknownEEC6:  lda #$01
	bne -

ZeebetiteHandler:
  jsr $95B7
	txa
	lsr
	adc #$3C
	sta $07
	lda #$00
	sta $06
	jsr CheckForItem
	bcc +
	lda #$81
	sta $0758,x
	lda #$01
	sta $075D,x
	lda #$07
	sta $075B,x
*       jmp UnknownEEC6

RinkaHandler:
  jsr $95BA
	jmp UnknownEEC6

DoorHandler:
  jsr UnknownEB92
	jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

PaletteHandler:
  lda ScrollDir
	sta $91
	bne UnknownEEC6

AnotherItem:
	lda ($00),y			;Is there another item with same Y pos?-->
	cmp #$FF			;If so, A is amount to add to ptr. to find X pos.
	bne AddToPtr00			;($EF09)
	pla				;
	pla				;No more items to check. Pull last subroutine-->
	rts				;off stack and exit.

AddToPtr00:
	clc				;
	adc $00				;
	sta $00				;A is added to the 16 bit address stored in $0000.
	bcc +				;
	inc $01				;
*       rts				;

;----------------------------------[ Draw structure routines ]----------------------------------------

;Draws one row of the structure.
;A = number of 2x2 tile macros to draw horizontally.

DrawStructRow:
	and #$0F			;Row length(in macros). Range #$00 thru #$0F.
	bne +				;
	lda #$10			;#$00 in row length=16.
*	sta $0E				;Store horizontal macro count.
	lda (StructPtr),y		;Get length byte again.
	jsr Adiv16			;($C2BF)/16. Upper nibble contains x coord offset(if any).
	asl				;*2, because a macro is 2 tiles wide.
	adc CartRAMWorkPtr		;Add x coord offset to CartRAMWorkPtr and save in $00.
	sta $00				;
	lda #$00			;
	adc CartRAMWorkPtr+1		;Save high byte of work pointer in $01.
	sta $01				;$0000 = work pointer.

DrawMacro:
	lda $01				;High byte of current location in room RAM.
	cmp #$63			;Check high byte of room RAM address for both room RAMs-->
	beq +				;to see if the attribute table data for the room RAM has-->
	cmp #$67			;been reached.  If so, branch to check lower byte as well.
	bcc ++				;If not at end of room RAM, branch to draw macro.
	beq +				;
	rts				;Return if have gone past room RAM(should never happen).

*	lda $00				;Low byte of current nametable address.
	cmp #$A0			;Reached attrib table?-->
	bcc +				;If not, branch to draw the macro.
	rts				;Can't draw any more of the structure, exit.

*	inc $10				;Increase struct data index.
	ldy $10				;Load struct data index into Y.
	lda (StructPtr),y		;Get macro number.
	asl				;
	asl				;A=macro number * 4. Each macro is 4 bytes long.
	sta $11				;Store macro index.
	ldx #$03			;Prepare to copy four tile numbers.
*	ldy $11				;Macro index loaded into Y.
	lda (MacroPtr),y		;Get tile number.
	inc $11				;Increase macro index
	ldy TilePosTable,x		;get tile position in macro.
	sta ($00),y			;Write tile number to room RAM.
	dex				;Done four tiles yet?-->
	bpl -				;If not, loop to do another.
	jsr UpdateAttrib		;($EF9E)Update attribute table if necessary
	ldy #$02			;Macro width(in tiles).
	jsr AddYToPtr00			;($C2A8)Add 2 to pointer to move to next macro.
	lda $00				;Low byte of current room RAM work pointer.
	and #$1F			;Still room left in current row?-->
	bne +				;If yes, branch to do another macro.

;End structure row early to prevent it from wrapping on to the next row..
	lda $10				;Struct index.
	clc				;
	adc $0E				;Add number of macros remaining in current row.
	sec				;
	sbc #$01			;-1 from macros remaining in current row.
	jmp AdvanceRow			;($EF78)Move to next row of structure.

*	dec $0E				;Have all macros been drawn on this row?-->
	bne DrawMacro			;If not, branch to draw another macro.
	lda $10				;Load struct index.

AdvanceRow:
	sec				;Since carry bit is set,-->
	adc StructPtr			;addition will be one more than expected.
	sta StructPtr			;Update the struct pointer.
	bcc +				;
	inc StructPtr+1			;Update high byte of struct pointer if carry occured.
*	lda #$40			;
	clc				;
	adc CartRAMWorkPtr		;Advance to next macro row in room RAM(two tile rows).
	sta CartRAMWorkPtr		;
	bcc DrawStruct			;Begin drawing next structure row.
	inc CartRAMWorkPtr+1		;Increment high byte of pointer if necessary.

DrawStruct:
	ldy #$00			;Reset struct index.
	sty $10				;
	lda (StructPtr),y		;Load data byte.
	cmp #$FF			;End-of-struct?-->
	beq +				;If so, branch to exit.
	jmp DrawStructRow		;($EF13)Draw a row of macros.
*	rts				;

;The following table is used to draw macros in room RAM. Each macro is 2 x 2 tiles.
;The following table contains the offsets required to place the tiles in each macro.

TilePosTable:
	.byte $21			;Lower right tile.
	.byte $20			;Lower left tile.
	.byte $01			;Upper right tile.
	.byte $00			;Upper left tile.

;---------------------------------[ Update attribute table bits ]------------------------------------

;The following routine updates attribute bits for one 2x2 tile section on the screen.

UpdateAttrib:
	lda ObjectPal			;Load attribute data of structure.
	cmp RoomPal			;Is it the same as the room's default attribute data?-->
	beq +++++			;If so, no need to modify the attribute table, exit.

;Figure out cart RAM address of the byte containing the relevant bits.

	lda $00				;
	sta $02				;
	lda $01				;
	lsr					;
	ror $02				;
	lsr					;
	ror $02				;
	lda $02				;The following section of code calculates the-->
	and #$07			;proper attribute byte that corresponds to the-->
	sta $03				;macro that has just been placed in the room RAM.
	lda $02				;
	lsr					;
	lsr					;
	and #$38			;
	ora $03				;
	ora #$C0			;
	sta $02				;
	lda #$63			;
	sta $03				;$0002 contains pointer to attribute byte.

	ldx #$00			;
	bit $00				;
	bvc +				;
	ldx #$02			;The following section of code figures out which-->
*	lda $00				;pair of bits to modify in the attribute table byte-->
	and #$02			;for the macro that has just been placed in the-->
	beq +				;room RAM.
	inx				;

;X now contains which macro attribute table bits to modify:
;+---+---+
;| 0 | 1 |
;+---+---+
;| 2 | 3 |
;+---+---+
;Where each box represents a macro(2x2 tiles).

;The following code clears the old attribute table bits and sets the new ones.
*	lda $01				;Load high byte of work pointer in room RAM.
	and #$04			;
	ora $03				;Choose proper attribute table associated with the-->
	sta $03				;current room RAM.
	lda AttribMaskTable,x		;Choose appropriate attribute table bit mask from table below.
	ldy #$00			;
	and ($02),y			;clear the old attribute table bits.
	sta ($02),y			;
	lda ObjectPal			;Load new attribute table data(#$00 thru #$03).
*	dex				;
	bmi +				;
	asl				;
	asl				;Attribute table bits shifted one step left
	bcc -				;Loop until attribute table bits are in the proper location.
*	ora ($02),y			;
	sta ($02),y			;Set attribute table bits.
*	rts				;

AttribMaskTable:
	.byte %11111100			;Upper left macro.
	.byte %11110011			;Upper right macro.
	.byte %11001111			;Lower left macro.
	.byte %00111111			;Lower right macro.

;------------------------[ Initialize room RAM and associated attribute table ]-----------------------

InitTables:
	lda CartRAMPtr+1		;#$60 or #$64.
	tay				;
	tax				;Save value to create counter later.
	iny				;
	iny				;High byte of address to fill to ($63 or $67).
	iny				;
	lda #$FF			;Value to fill room RAM with.
	jsr FillRoomRAM			;($F01C)Fill entire RAM for designated room with #$FF.

	ldx $01				;#$5F or #$63 depening on which room RAM was initialized.
	jsr Xplus4			;($E193)X = X + 4.
	stx $01				;Set high byte for attribute table write(#$63 or #$67).
	ldx RoomPal			;Index into table below (Lowest 2 bits).
	lda ATDataTable,x		;Load attribute table data from table below.
	ldy #$C0			;Low byte of start of all attribute tables.
*	sta ($00),y			;Fill attribute table.
	iny				;
	bne -				;Loop until entire attribute table is filled.
	rts				;

ATDataTable:       
	.byte %00000000			;
	.byte %01010101			;Data to fill attribute tables with.
	.byte %10101010			;
	.byte %11111111			;

FillRoomRAM:
	pha				;Temporarily store A.
	txa				;
	sty $01				;Calculate value to store in X to use as upper byte-->
	clc				;counter for initilaizing room RAM(X=#$FC).-->
	sbc $01				;Since carry bit is cleared, result is one less than expected.
	tax				;
	pla				;Restore value to fill room RAM with(#$FF).
	ldy #$00			;Lower address byte to start at.
	sty $00				;
*	sta ($00),y			;
	dey				;
	bne -				;
	dec $01				;Loop until all the room RAM is filled with #$FF(black).
	inx				;
	bne -				;
	rts				;

;----------------------------------------------------------------------------------------------------

; Crash detection
; ===============
CrashDetection:
  lda #$FF
	sta $73
	sta $010F
; check for crash with Memus
	ldx #$18
*      lda $B0,x
	beq +++++	    ; branch if no Memu in slot
	cmp #$03
	beq +++++
	jsr UnknownF19A
	jsr IsSamusDead
	beq +
	lda SamusBlink
	bne +
	ldy #$00
	jsr UnknownF149
	jsr UnknownF2B4
    ; check for crash with bullets
*       ldy #$D0
*       lda ObjAction,y       ; projectile active?
	beq ++		  ; try next one if not
	cmp #wa_BulletExplode
	bcc +
	cmp #$07
	beq +
	cmp #wa_BombExplode
	beq +
	cmp #wa_Missile
	bne ++
*       jsr UnknownF149
	jsr UnknownF32A
*      jsr Yplus16
	bne ---
*	txa
	sec
	sbc #$08		; each Memu occupies 8 bytes
	tax
	bpl ------

	ldx #$B0
*       lda ObjAction,x
	cmp #$02
	bne +
	ldy #$00
	jsr IsSamusDead
	beq ++
	jsr CheckObjYObjXHBOverlap
	jsr UnknownF277
*       jsr Xminus16
	bmi --
; enemy <--> bullet/missile/bomb detection
*	ldx #$50		; start with enemy slot #5
UnknownF09F:  
	lda EnStatus,x	     ; slot active?
	beq +		   ; branch if not
	cmp #$03
*   beq NextEnemy	   ; next slot
	jsr CreateEnemyLocationStruct79PPUdiffB
	lda EnStatus,x
	cmp #$05
	beq ++++
	ldy #$D0		; first projectile slot
*   lda ObjAction,y	 ; is it active?
	beq ++		  ; branch if not
	cmp #wa_BulletExplode
	bcc +
	cmp #$07
	beq +
	cmp #wa_BombExplode
	beq +
	cmp #wa_Missile
	bne ++
; check if enemy is actually hit
*   jsr UnknownF140
	jsr UnknownF2CA
*	jsr Yplus16	     ; next projectile slot
	bne ---
*	ldy #$00
	lda SamusBlink
	bne NextEnemy
	jsr IsSamusDead
	beq NextEnemy
	jsr UnknownF140
	jsr UnknownF282
	NextEnemy:
	jsr Xminus16
	bmi +
	jmp UnknownF09F

*   ldx #$00
	jsr CreateObjectLocationStruct79PPUdiffB
	ldy #$60
*   lda EnStatus,y
	beq +
	cmp #$05
	beq +
	lda SamusBlink
	bne +
	jsr IsSamusDead
	beq +
	jsr AddAndCacheObjEnRadYX45
	jsr CreateEnemyLocationStruct68PPUdiffA
	jsr CheckHitboxOverlap
	jsr UnknownF2ED
*   jsr Yplus16
	cmp #$C0
	bne --
	ldy #$00
	jsr IsSamusDead
	beq ++++
	jsr CreateObjYLocationStruct68PPUdiffA
	ldx #$F0
*   lda ObjAction,x
	cmp #$07
	beq +
	cmp #$0A
	bne ++
*   jsr Check68AYObjXHBOverlap
	jsr UnknownF311
*	jsr Xminus16
	cmp #$C0
	bne ---			
*	jmp SubtractHealth		;($CE92)

UnknownF140:  
	jsr AddAndCacheEnObjRadYX45
	jsr CreateObjYLocationStruct68PPUdiffA
	jmp CheckHitboxOverlap

UnknownF149:  
	jsr CreateObjYLocationStruct68PPUdiffA
	jsr Add48AndCacheObjRadYX45
	jmp CheckHitboxOverlap

CreateEnemyLocationStruct79PPUdiffB:  
	lda EnYRoomPos,x
	sta $07	 ; Y coord
	lda EnXRoomPos,x
	sta $09	 ; X coord
	lda EnNameTable,x     ; hi coord
	jmp StoreNametableMismatchB

CreateEnemyLocationStruct68PPUdiffA:  
	lda EnYRoomPos,y     ; Y coord
	sta $06
	lda EnXRoomPos,y     ; X coord
	sta $08
	lda EnNameTable,y     ; hi coord
	jmp StoreNametableMismatchA

CreateObjectLocationStruct79PPUdiffB:  
	lda ObjectY,x
	sta $07
	lda ObjectX,x
	sta $09
	lda ObjectHi,x
StoreNametableMismatchB: 
	eor PPUCNT0ZP		;instead of storing off the nametable, store off it the nametable doesn't match with current drawing nametable
	and #$01
	sta $0B
	rts

CreateObjYLocationStruct68PPUdiffA:  
	lda ObjectY,y
	sta $06
	lda ObjectX,y
	sta $08
	lda ObjectHi,y
StoreNametableMismatchA:  
	eor PPUCNT0ZP
	and #$01
	sta $0A
	rts

UnknownF19A:  
	lda $B1,x
	sta $07
	lda $B2,x
	sta $09
	lda $B3,x
	jmp StoreNametableMismatchB

AddandCacheObjObjRadYX45:  
	lda ObjRadY,x
	jsr AddAndCacheObjectRadY4
	lda ObjRadX,x
	jmp AddAndCacheObjectRadX5

AddAndCacheObjEnRadYX45:  
	lda ObjRadY,x
	jsr AddAndCacheEnRadY4
	lda ObjRadX,x
	jmp AddAndCacheEnRadX5

AddAndCacheEnObjRadYX45:  
	lda EnRadY,x		 			; load enemy collision vert radius
	jsr AddAndCacheObjectRadY4		; add to object collision vert radius for object at current Y index
	lda EnRadX,x		 			; load enemy collision hori radius
	jmp AddAndCacheObjectRadX5		; add to object collision hori raduis for object at current Y index

AddAndCacheEnRadX5:  
	clc
	adc EnRadX,y
	sta $05
	rts

Add48AndCacheObjRadYX45:  
	lda #$04
	jsr AddAndCacheObjectRadY4
	lda #$08
	
AddAndCacheObjectRadX5:  
	clc
	adc ObjRadX,y
	sta $05
	rts

AddAndCacheObjectRadY4:  
	clc				; clear carry
	adc ObjRadY,y	; add to current object collision radius
	sta $04			; store in $04
	rts				; return

AddAndCacheEnRadY4:  
	clc
	adc EnRadY,y
	sta $04
	rts

; Y = Y + 16

Yplus16:
	tya
	clc
	adc #$10
	tay
	rts

; X = X - 16

Xminus16:
	txa
	sec
	sbc #$10
	tax
	rts

CheckHitboxOverlap:  
	lda #$02							; load 2
	sta $10								; cache off in $10
	and ScrollDir						; AND with the scroll direction (horizontal scrolling when !0)
	sta $03								; cache off in $03
	lda $07								; load $07, cached object A Y pos
	sec									; set carry 
	sbc $06     						; subtract $06, cached object B y pos - should create "vector" direction from B to A
	sta $00								; store difference in 0
	lda $03								; load cached horizontal flag
	bne ++								; 	not zero, horizontal scrolling- jump ahead
	lda $0B								; vertical scrolling - load enemy name table diff
	eor $0A								; flip bits with object name table diff
	beq ++								; 	was 0 (both tables were different than the drwaing table), jump ahead
	jsr ResolveHBOverlapOverNT			; was not 0 - characters on opposite drawing tables, detour to 
	lda $00								; load Y position diff
	sec									; set carry
	sbc #$10							; subtract an extra 16 to deal with the attribute table space
	sta $00								; restore into 0
	bcs +								; 	carry flag set- no flip over, jump ahead
	dec $01								; 	not set, carry was used for flip over- dec $01 
*   jmp CheckHitboxOverlapHorizontal	; jump ahead, we finished different table stuff 

*   lda #$00							; same table or horizontal scrolling - load 0
	sbc #$00							; subtract with carry- will be 0 if we didn't flip over, FF if we did
	jsr UnknownF266						; send result to $1- if negative (object A was above B), negate $0 and set $1 to 0
	
CheckHitboxOverlapHorizontal:  
	sec							; set the carry flag, aka "false" for this function
	lda $01						; load $1
	bne ++						; 	not zero, return with carry set, check failed
	lda $00						; zero- load the ABS Y position difference
	sta $11						; move over to $11
	cmp $04						; compare to the added object/enemy collision Y radius
	bcs ++						; 	if the difference is equal or greater than, we're not overlapping, fail check
	asl $10						; boxes ARE overlapping, at least in Y - need to check X. - double $10
	lda $09						; load $09 - object A X pos
	sec							; set carry
	sbc $08						; subtract $08 - object B x pos
	sta $00						; store difference into $0
	lda $03						; load cached hori scroll flag
	beq +						; 	0 = vert scrolling, jump ahead
	lda $0B						; horizontal scrolling - load name table differences
	eor $0A						; 
	beq +						; 	will be 0 if they are on the same name table as eachother, jump ahead
	jsr ResolveHBOverlapOverNT	; not the same name table- need to figure out who's on top of who
	jmp UnknownF256

*   sbc #$00
	jsr UnknownF266
UnknownF256:  
	sec				; set carry 
	lda $01			; load 1
	bne +			; not zero, failed- return
	lda $00			; 0, success- load the ABS difference
	sta $0F			; store out to F
	cmp $05			; comp to the added hitboxes' X radius
*   rts				; retiurn with carry set if the difference was greater or equal to

ResolveHBOverlapOverNT:  
	lda $0B			; load enemy name table difference
	sbc $0A			; subtract object name table difference with previous carry from position difference- I think we should have 1, 0, FF, or FE?
					; in case of FE, we should get fails for overlap checks- the difference in table direction is wrong
UnknownF266:  
	sta $01				; store A into $01 
	bpl +				; if 01 was still positive, jump ahead
	jsr Negate0Resolve1	; negate $0 and $1, with $1 going one extra down if $0 is not 0
	inc $10				; int $10 (we cached the horizontal scroll flag (0010) here- inc it to (0011)
*   rts					; return

UnknownF270:  
	ora $030A,x
	sta $030A,x
	rts

UnknownF277:  
	bcs Exit17
UnknownF279:  
	lda $10
UnknownF27B:  
	ora $030A,y
	sta $030A,y
	Exit17:
	rts

UnknownF282:  
	bcs Exit17
	jsr UnknownF2E8
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	ldy #$00
	bcc +++
	lda EnStatus,x
	cmp #$04
	bcs Exit17
	lda EnDataIndex,x
*   sta $010F
	tay
	bmi +
	lda $968B,y
	and #$10
	bne Exit17
*   ldy #$00
	jsr UnknownF338
	jmp UnknownF306

*	lda #$81
	sta $040E,x
	bne ++
UnknownF2B4:  
	bcs +
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	ldy #$00
	lda #$C0
	bcs ---
UnknownF2BF:  
	lda $B6,x
	and #$F8
	ora $10
	eor #$03
	sta $B6,x
*   rts

UnknownF2CA:  
	bcs +++
	lda ObjAction,y
	sta $040E,x
	jsr UnknownF279
*	jsr UnknownF332
*   ora $0404,x
	sta $0404,x
*   rts

UnknownF2DF:  lda $10
	ora $0404,y
	sta $0404,y
	rts

UnknownF2E8:  
	jsr UnknownF340
	bne --
	
UnknownF2ED:  
	bcs +
	jsr UnknownF2DF
	tya
	pha
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	pla
	tay
	bcc +
	lda #$80
	sta $010F
	jsr UnknownF332
	jsr UnknownF270
	
UnknownF306:  
	lda AreaDamageLo
	sta HealthLoChange
	lda AreaDamageHi
	sta HealthHiChange
*   rts

UnknownF311:  
	bcs Exit22
	lda #$E0
	sta $010F
	jsr UnknownF338
	lda $0F
	beq +
	lda #$01
*   sta $73

ClearHealthChange:
	lda #$00
	sta HealthLoChange
	sta HealthHiChange

Exit22: 
	rts				;Return for routine above and below.

UnknownF32A:  bcs Exit22
	jsr UnknownF279
	jmp UnknownF2BF

UnknownF332:  jsr UnknownF340
	jmp Amul8       ; * 8

UnknownF338:  lda $10
	asl
	asl
	asl
	jmp UnknownF27B

UnknownF340:  lda $10
	eor #$03
	rts

; UpdateEnemies
; =============

UpdateEnemies:
	ldx #$50						;Load x with #$50
*   jsr DoOneEnemy						
	ldx PageIndex
	jsr Xminus16
	bne -
DoOneEnemy:
	stx PageIndex					;PageIndex starts at $50 and is subtracted by $F each iteration.
									;There is a max of 6 enemies at a time.
	ldy EnStatus,x					;don't worry about visibility mismatch when slot not in use
	beq +
	cpy #$03						;don't worry about visibility mismatch when dead, frozen, as a power up, or when hit
	bcs +
	jsr BreakOnVisibilityMismatch 	;check if we're visible- if we are in our status, and not in reality, break early
*   jsr SetEnemyUpdateSkip			;update every other enemy per frame
	lda EnStatus,x
	sta EnCachedStatus				;store out enemy status at start of update
	cmp #$07						;compare EnStatus to the max status of 7
	bcs +							;if our status is greater/equal, and therefore invalid, kill the enemy
	jsr ChooseRoutine				;else, jump to this enemy's current routine, then jump back up to enemy update loop

; Pointer table to code

	;#$00= Enemy slot not in use
	;#$01= waiting
	;#$02= plain moving
	;#$03= dying
	;#$04= Enemy frozen.
	;#$05= power up state
	;#$06= enemy hit

EnemyRoutineTable:
	.word ExitSub       	;Enemy slot not in use
	.word EnemyWaitState	;waiting
	.word EnemyMoveState	;plain moving
	.word EnemySpecificUpdate	;dying
	.word EnemyFrozenState	;frozen
	.word EnemyPowerupState	;power up
	.word EnemyHitState		;hit

*   jmp KillObject			;Free enemy data slot.

BreakOnVisibilityMismatch:  
	lda $0405,x			 ; load enemy status flags
	and #$02			 ; check if enemy is visible
	bne +				 ; if not, load enemy and exit
	lda EnYRoomPos,x     ; Y coord
	sta $0A				 ; 
	lda EnXRoomPos,x     ; X coord
	sta $0B
	lda EnNameTable,x    ; name table 
	sta $06
	lda EnRadY,x		 ; enemy hit box
	sta $08
	lda EnRadX,x		 ; enemy hit box
	sta $09
	jsr IsObjectVisible		;($DFDF)Determine if object is within the screen boundaries.
	txa					 ;x = 1 if visible, 0 if not, push to A
	bne +				 ;if not visible, load enemy and return
	pla					 ;else, pop the last call on the stack out
	pla					 ;we want to go back two layers before instead
*   ldx PageIndex		 ;load enemy
	rts					 ;return

;the comments aren't super helpful, but here's what's going on
;on the enemy status flags, $0405, the 6th bit is an update skip Bit
;on every frame, an enemy's skip bit is set based on the current odd/even parity of the frame counter
;and every other enemy has the opposite skip 
;so, only half the enemies are updating per frame
SetEnemyUpdateSkip:  
	lda $0405,x			;  du.. ....	get status flags
	asl					;d u... ...0	pop out check distance flag
	rol					;u .... ..0d	pop out skip update flag, bring back distance flag
	tay					;move A to Y
	txa					;move enemy index to A
	jsr Adiv16			;move high nibble to low nibble
	eor FrameCount		;get current frame count, return 1 if the frame count and enemy are the opposite parity (even v odd)
	lsr					;shift right to get LSB into carry (n)
	tya					;  .... ..0d	return to status flags
	ror					;  n... ...0 d	bring in inverted skip update flag, pop distance flag
	ror					;  dn.. .... 0	bring back distance flag
	sta $0405,x			;store back into status flags
	rts

;;update enemy wait state
EnemyWaitState:  				
	lda $0405,x						; load the status flags
	asl								; push skip update flag left
	bmi +							; skip to damage checks if skipping update (high bit set)
	lda #$00						; load 0
	sta $6B01,x						; store into enemy data
	sta EnCounter,x					; clear enemy counter
	sta $040A,x						; reset enemy orientation
	jsr EnemyDirectionToSamus		; update enemy relation to samus, as well as velocity direction
	jsr CheckDistanceToSamus		; update distance related triggers to samus
	jsr UpdateEnAnimDirection963B	; update direction of wait animation
	jsr PopulateEnemy6B03			; fill in 6B03 with data from 977B
	lda EnDelay,x					; check the enemy delay timer 
	beq +							; 	if the delay timer is 0, we aren't waiting for anything, jump ahead and update damage if it got any
	jsr UpdateEnDelay					; else, dec delay timer and do extra for when the timer runs out 
*   jmp ++							; update damage to waiting enemy

;;update enemy moving state
EnemyMoveState:  	
	lda $0405,x					;load enemy status flags
	asl							;check if the skip update flag is set
	bmi ++						;if set, skip move updates, go straight to damage
	lda $0405,x					;else, load status flags again
	and #$20					;check if we have a delay timer to start
	beq +						;if not, jump ahead
	ldy EnDataIndex,x			;else, load the enemy data index
	lda EnemyInitDelayTbl,y		;get the enemy delay time
	sta EnDelay,x				;set the enemy delay timer
	dec EnStatus,x				;set us back to a delay/wait status
	bne ++						;skip move updates for enemy damage
*   jsr EnemyDirectionToSamus	;for enemies that home in on samus while moving, update it
	jsr CheckDistanceToSamus	;not sure exactly what this does yet
	jsr EnemyUnderRoom			;kill enemies that somehow get under the map in horizontal rooms
*	jsr UpdateEnemyDamage		;if the enemy was shot, update their damage

EnemySpecificUpdate:			
	jmp AreaEnemySpecificUpdate ; update enemy using area routines

UpdateEnemyAnimAndMove:	
	jsr UpdateEnemyAnim				; update the enemy anim frame, loop if need be
	jsr UpdateEnemyMovement						;
CheckObjectAttribs:
	ldx PageIndex					; load enemy index
	lda EnSpecialAttribs,x			; load special attributes (bit 7 = strong, bit 6 = boss)
	bpl +							; jump ahead if not strong enemy
	lda ObjectCntrl					; 	load object controls (mirroring/color)
	bmi +							; 	if the high bit is set, jump ahead
	lda #$A3						; 	else, load A3 and store to object control
UnknownF423:  						
	sta ObjectCntrl					; store to object controls
*   lda EnStatus,x					; load up the enemy status
	beq ResetEnemyHitStatus			; if it's a clear slot, clear rhis hit status and leave
	jsr UpdateObjectCntrl			; else, double check the enemy has a valid frame first, and reset properties if not
ResetEnemyHitStatus:  
	ldx PageIndex					; load enemy index
	lda #$00						;
	sta $0404,x						; reset hit status
	sta $040E,x						; reset hit source
	rts

UpdateEnemyAnimAndCheckAttr:
  jsr UpdateEnemyAnim
  jmp CheckObjectAttribs

EnemyFrozenState:  
    jsr UpdateEnemyDamage
	lda EnStatus,x
	cmp #$03
	beq UpdateEnemyAnimAndMove
	bit ObjectCntrl
	bmi +
	lda #$A1
	sta ObjectCntrl
*   lda FrameCount
	and #$07
	bne +
	dec $040D,x
	bne +
	lda EnStatus,x
	cmp #$03
	beq +
	lda $040C,x
	sta EnStatus,x
	ldy EnDataIndex,x
	lda $969B,y
	sta $040D,x
*   lda $040D,x
	cmp #$0B
	bcs +
	lda FrameCount
	and #$02
	beq +
	asl ObjectCntrl
*   jmp CheckObjectAttribs

EnemyPowerupState:  
	lda $0404,x				;
	and #$24				;I guess we check collision with samus here?
	beq ++++++				;if no collision, just update delete timer
	jsr KillObject			;	($FA18)Free enemy data slot.
	ldy EnAnimFrame,x		;	check the enemy anim frame
	cpy #$80				;	if it's equal to $80, it was a missle
	beq PickupMissile		;		jump to picking up a missle
	tya						;	else, it was health - put the anim frame into A (81 - health, 89 = nothing)
	pha						;	then push it to the stack
	lda EnDataIndex,x		; 	load enemy data index- should be 2 if it was a metroid
	pha						;	push that to the stack too
	ldy #$00				;	
	ldx #$03				;	set up for a 30.0 health boost
	pla						; 	get that metroid flag
	bne ++					; 		metroid flag set, jump ahead to health change
	dex						;	metroid flag clear- only a 20.0 health boost
	pla						;	pop anim frame back into the accumulator
	cmp #$81				;	comapre to $81
	bne +					;		not 81, bigger enemy
	ldx #$01				;	Increase HealthHi by 1.
	ldy #$50				;	Increase HealthLo by 5.
*   pha						; 	push frame back onto stack since we still had it out, so we can pop it out again without removing critical routine poitners
*	pla						; remove frame from stack
	sty HealthLoChange		; set up health change
	stx HealthHiChange
	jsr AddHealth			;($CEF9)Add health to Samus.
	jmp SFX_EnergyPickup

PickupMissile:
	lda #$02				; load 2
	ldy EnDataIndex,x		; load enemy data index- should be 2 with metroid
	beq +					; 	not a metroid, jump ahead- only adding 2 missles
	lda #$1E				; 	is a metroid, load 1E insead (30 missles)
*   clc						; clear carry
	adc MissileCount		; add missles
	bcs +		   			; can't have more than 255 missiles
	cmp MaxMissiles			; can Samus hold this many missiles?
	bcc ++		  		 	; branch if yes
*   lda MaxMissiles		 	; set to max. # of missiles allowed
*	sta MissileCount
	jmp SFX_MissilePickup

EnemyUpdatePowerupLifetimeTimer:
*	lda FrameCount
	and #$03
	bne +
	dec $040D,x
	bne +
	jsr KillObject			;($FA18)Free enemy data slot.
*	lda FrameCount
	and #$02
	lsr
	ora #$A0
	sta ObjectCntrl
	jmp CheckObjectAttribs

EnemyHitState:  
	dec EnSpecialAttribs,x
	bne ++
	lda $040C,x
	tay
	and #$C0
	sta EnSpecialAttribs,x
	tya
	and #$3F
	sta EnStatus,x
	pha
	jsr LoadFrom977B
	and #$20
	beq +
	pla
	jsr UnknownF515
	pha
*       pla
*	lda #$A0
	jmp UnknownF423

UnknownF515:  
	sta $040C,x
FreezeEnemy:  
	lda #$04
	sta EnStatus,x
	rts

EnemyUnderRoom:  
	lda ScrollDir		 ; load scroll direction
	ldx PageIndex		 ; load enemy
	cmp #$02		     ; check scroll direction
	bcc +++				 ; added plus for new damage code - return if we're vertical, else
	lda EnYRoomPos,x     ; check Y coord
	cmp #$EC			 ; compare to bottom of room - if an enemy leaves the bottom of a room, kill it
	bcc +++				 ; added plus for new damage code
	jmp KillObject		 ;($FA18)Free enemy data slot.

PlayMetroidHitSound:
*   jsr SFX_MetroidHit		;
	jmp GetPageIndex		;jump to end of damage code

UpdateEnemyDamage:  
    lda EnSpecialAttribs,x 	; load the special attributes of an enemy, ;Bit 7 set=tough version of enemy, bit 6 set=mini boss. 
	sta $0A					; store the value into $000A for safe keeping
	lda $0404,x				; load up the shot/collision status 
	and #$20				; and with mask out 0010 0000, to check if it's a shot 
	beq ++					; added plus - equal to 0? jump to the RTS - no shot detected
	lda $040E,x				; 	40E is the type of laser we were hit by (01 = laser, 02 = wave, 03 = ice, 0A = bomb, 0B = missle
	cmp #$03				; 	this value is loaded into here from samus' 0300 page ram (from 3D0 down)
	bne ++++				;		added plus - not equal? jump to the hit points checktwo chunks down
	bit $0A					; 		take the enemy attribute we loaded. 
	bvs ++++				; 			added plus - if bit 6 is set IE, we're a miniboss, jump down to the hit point check; ice lasers don't freeze the bosses
	lda EnStatus,x			;		if NOT, then load the current status
	cmp #$04				;		compare to frozen
	beq ++++				;			added plus - frozen? jump to hit point stuff
	
;--------------here's some new damage code for the ice beam to work more like super metroid 
	
SuperMetroidIceBeam:
		jsr LoadFrom977B				
        and #$20						
		bne +				
		clc
		lda SamusGear
		and #gr_WAVEBEAM
		asl						
		asl						
		adc #$01				
		sec						
		sbc EnHitPoints, x
		;if damage was grater, then this will be positive - freeze
		bpl +
		bmi ++++
		
*		lda EnStatus,x
	
;-------------end of new damage code

	jsr UnknownF515			; else, jump back up to where we store whatever we ended up with into 040C, I assume a previous status then load 4 into A, and store that in the enemy status- AKA, freeze it
	lda #$40				; load up #40 frames
	sta $040D,x				; store in delay timer
	jsr LoadFrom977B				; load the enemy info from the current room, bit shifted left 1
	and #$20				; check metroid bit
	beq +					; if it's not a metroid, return
	lda #$01				; 	making this one so the metroid only takes one misile instead of 5
	sta EnHitPoints,x		; 	set metroid health
	jmp $95A8				;	jump to 95A8, which does stuff to the metroid on samus flag, among other things
*   rts						; 	then, we're done with freezing and damage updates

PlayInvicibleSFX:
*	jsr LoadFrom977B				; get 977b attributes
	and #$20				; check if a metroid
	bne ---- 				; 	added minus - if metroid, play metroid hit sound
	jsr SFX_Metal			; play metal hit- nothing 
	jmp ResetEnemyHitStatus

ContinueEnemyDamge0:
*	lda EnHitPoints,x		;load enemy hit points	
	cmp #$FF				;if it's FF
	beq --					;enemy is invincible
	bit $0A					;if not, load up the enemy status from $000A
	bvc +					;if it's still not a boss, jump to just checking the normal SFX
	jsr SFX_BossHit			;	if it was a boss, play the boss hit sfx
	bne ++					;	then, jump to damage dealing
*	jsr LoadFromEn968B		;load 968b attributes
	and #$0C				;get SFX index bits
	beq PlaySnd1			;00 - sound 1
	cmp #$04				
	beq PlaySnd2			;01 - sound 2
	cmp #$08
	beq PlaySnd3			;10 - sound 3
	jsr SFX_MetroidHit		;11 - sound 4, metroid
	bne +       			; branch always
PlaySnd1:
	jsr SFX_EnemyHit
	bne +       			; branch always
PlaySnd2:
	jsr SFX_EnemyHit
	bne +       			; branch always
PlaySnd3:
	jsr SFX_BigEnemyHit	

ContinueEnemyDamge1:
*	ldx PageIndex			; load up the enemy back into x	
	jsr LoadFrom977B				;
	and #$20				; if it's *not a metroid*, jump to enemy status check- we can only get here if the metroid is frozen
	beq +
	lda $040E,x						;else, load up the damage source
	cmp #$0B						;check if it's a missle
	bne ----						;not equal, play the metal sfx and pop out- no damage to deal
*   lda EnStatus,x					; 	load up that stat
	cmp #$04						; 	check if we're frozen
	bne +							; 	not frozen? use the status as is and jump to oring with the special attribute
	lda $040C,x						; 		else, if frozen, load up the previous state we saved out earlier from being frozen
*   ora $0A							; 		or with that stored enemy attribute
	sta $040C,x						;		then store that in back into the previous state data
	asl								;		shift the previous state data left- check the 
	bmi +							;jump down if our multiplication nagated the value
	jsr LoadFrom977B						;
	and #$20
	bne +							; it's a metroid, jump to the load 6 - missles do one hit each, so we treat them like normal lasers
	ldy $040E,x						; if not set, load the missle type into y
	cpy #$0B						; was it a missle?
	beq +++++						; if it's equal, skip normal damage - instant kill
	cpy #$81						; ice beam? maybe? or screw attack?
	beq +++++						; if equal, skip normal damage - instant kill
*   lda #$06						; load 6
	sta EnStatus,x					; store that into the status, probably "hit"
	lda #$0A						; 10 to A
	bit $0A							; check the mini boss tag again
	bvc +							; if it is the boss,
	lda #$03						; load 3 to A instead
*   sta EnSpecialAttribs,x			; store in the special attributes as a freeze timer
	bit SamusGear
	bvs +							; if a wave beam, 2 damage
	bit $0A							; not a wave beam? check if we're a boss
	bvc ++							; not a boss? sub just 1 health
	ldy $040E,x						; load the missle type
	cpy #$0B						; if it's a missle, do a full 4 hits on the boss
	bne ++							
	dec EnHitPoints,x				;sub up to 4 health
	beq +++
	dec EnHitPoints,x				;sub up to 3 health
	beq +++
*   dec EnHitPoints,x				;sub up to 2 health
	beq ++
*   dec EnHitPoints,x				;sub 1 health
	bne GetPageIndex				;does DEC load up the
*   lda #$03						;go in for the kill
	sta EnStatus,x					;load 03 into the status - enemy is dying
	bit $0A							;check if we're a miniboss
	bvs +							;if so, jump down to the mystery load
	lda $040E,x						;check the damage type
	cmp #$02						;compare to wave
	bcs +							;if greater than OR equal to, jump down to the mystery load
	lda #$00						;otherwise, load 0 and jump to 
	jsr KillEnemyAndDropItem		;the enemy kill/item drop routine - for normal explosions
	ldx PageIndex					;I THINK THE REST OF THIS IS FINDING AND POPULATING AN EXPLOSION SLOT 
*   jsr GetEnemyDirectionDataIndex	;get enemy direction animation index
	lda EnDirAnimTb0B,y				;load that value at the index, and take it to the reset anim index
	jsr ResetAnimIndex
	sta EnCounter,x					;store whatever we got out of that into the en counter
	ldx #$C0						;load a specific enemy status, the one at offset #C0- 6BB4, or something
*   lda EnStatus,x					
	beq +							;if it's 0, jump to loading the mystery value in 95DD
	txa								;put x into a for a bit
	clc								;clear carry
	adc #$08						;add 8
	tax								;put a back into x
	cmp #$E0						;compare with #E0, three rows down 
	bne -							;if we haven't reached that max index, keep looping- this time, checking the 6BBC value, etc
	beq GetPageIndex				;if we reached that max index and never found a 0, get the page index or whatever and leave the subroutine
*   lda $95DD						
	jsr ResetAnimIndex
	lda #$0A
	sta EnCounter,x
    inc EnStatus,x					;set the found explosion to active
	lda #$00
	bit $0A
	bvc +
	lda #$03
*   sta $0407,x
	ldy PageIndex
	lda EnYRoomPos,y
	sta EnYRoomPos,x
	lda EnXRoomPos,y
	sta EnXRoomPos,x
	lda EnNameTable,y
	sta EnNameTable,x
GetPageIndex:
	ldx PageIndex
	rts

PopulateEnemy6B03:  
	jsr LoadFrom977B	;get the enemy data at 977B, MSB in carry everything else shifted left
	asl			;
	asl			;
	asl			;shift the rest left to move the low nibble to the high nibble
	and #$C0	;and with 1100 000
	sta $6B03,x	;store into 6B03
	rts

UpdateEnAnimDirection963B:  
	jsr GetEnemyDirectionDataIndex
	lda EnDirAnimTb3B,y
	cmp EnResetAnimIndex,x
	beq +							;same animation, no need to set 
ResetAnimIndex:  
	sta EnResetAnimIndex,x			;else, reset with new anim index
SetEnAnimIndex:  
	sta EnAnimIndex,x				;set anim index
ResetEnAnimDelay:  
	lda #$00						;reset anim delay
	sta EnAnimDelay,x
*   rts

UpdateEnAnimDirection965B:  
	jsr GetEnemyDirectionDataIndex
	lda EnDirAnimTb5B,y
	cmp EnResetAnimIndex,x
	beq Exit12
	jsr ResetAnimIndex
	ldy EnDataIndex,x
	lda $967B,y
	and #$7F			;check if MSB is set... could BPL/BMI instead to save an instruction?
	beq Exit12			;leave if MSB was not set
	tay					;	else, move the set flag to Y
*   dec EnAnimIndex,x	;dec the index until y is 0
	dey					;at 1000 0000 -> takes 128, takes 256 at 0000 0000
	bne -				
Exit12: rts

EnemyDirectionToSamus:  	
	lda #$00				;this gets used per enemy, so we'll need to figure out what this does
	sta $82
	jsr LoadFromEn968B		; get movement attributes for enemy
	tay						; store it off (for constant use)
	lda EnStatus,x			; load the enemy status
	cmp #$02				; if we're moving, we need to do an extra check
	bne +					;
	tya						;	see if this enemy should check it's relation to samus when moving
	and #$02				;	return early if not
	beq Exit12				;	
*   tya						;
	dec $040D,x				; decrement 40D timer
	bne Exit12				; if it's not 0, leave 
	pha						; push EnA? attribute to stack, so we can free up y
	ldy EnDataIndex,x		; load enemy data index to y
	lda $969B,y				; get some the 40D timer amount for this enemy
	sta $040D,x				; reset 40D with the timer
	pla						; return EnA? to the accumulator
	bpl ++++				; if MSB is not set, jump ahead
	
CheckHorizontalToSamus:
	lda #$FE				; 	set enemy horizontal relation flag
	jsr ClearEnStatusFlags	; 	clear horizontal relation flag on enemy
	lda ScrollDir			; 	check the current scroll direction
	cmp #$02
	bcc +					; 	scroll direction is vertical, jump ahead - X values will always be the same
	jsr OnPlayerNametable	; 		if horizontal- check if we're on the same name table as samus
	bcc +					; 		carry is clear if true - we can compare distances
	tya						; 			else, move enemy name table to A
	eor PPUCNT0ZP			; 			use the current PPU nametable to determine direction
	bcs +++					; 		
*   lda EnXRoomPos,x		; 	if in vertical scroll/ same nametable as samus in horizontal scroll
	cmp ObjectX				; 	compare enemy and player horizontal position
	bne +					; 	if they aren't the same, jump over
	inc $82					; 		else, inc 82
*   rol						; 	A = 1 if greater/equal, 0 if less than
*	and #$01				; 	get final horizontal relation flag (same table? use difference : use enemy nametable in relation to current PPU nametable )
	jsr EnableEnStatusFlags	; 	enable horizontal relation flag on enemy
	lsr						; 	get the direction back out to carry flag (0 = left of samus, 1 = right)
	ror						; 	get the flag to the MSB
	eor $0403,x				; 	if the velocity direction does not match, we'll get a negative number
	bpl +					; 	if we're still positive, jump ahead
	jsr InvertHoriVel		; 	flip horizontal velocity to match relation to samus

CheckVerticalToSamus:
*	lda #$FB				;		
	jsr ClearEnStatusFlags	;	clear vertical direction flag on enemy
	lda ScrollDir			;	check the current scroll direction
	cmp #$02				;	
	bcs +					;	scroll direction is horizontal, jump ahead - Y values will always be the same
	jsr OnPlayerNametable	; 		if verttical- check if we're on the same name table as samus
	bcc +					; 		carry is clear if true - we can compare distances
	tya						; 			else, move enemy name table to A
	eor PPUCNT0ZP			; 			use the current PPU nametable to determine direction
	bcs +++					;
*	lda EnYRoomPos,x		; 	if in horizontal scroll/ same nametable as samus in vertical scroll
	cmp ObjectY				; 	compare enemy and player vertical position
	bne +					; 	if they aren't the same, jump over
	inc $82					; 		TODO: else, inc 82
	inc $82					; 		twice!
*	rol						; 	A = 1 if greater/equal, 0 if less than
*	and #$01				;	get final horizontal relation flag (same table? use difference : use enemy nametable in relation to current PPU nametable )
	asl						;
	asl						; 	move to proper flag position
	jsr EnableEnStatusFlags	; 	enable flag
	lsr						;
	lsr						;
	lsr						;	get the direction back out to carry flag (0 = below samus, 1 = above samus)
	ror						;	roll it back to MSB
	eor EnVertSpeed,x				;	if the velocity direction does not match, we'll get a negative number
	bpl +					; 	if we're still positive, jump ahead
	jmp InvertVertVel		; 	flip vertical velocity toward samus

EnableEnStatusFlags:
	ora $0405,x
	sta $0405,x
*       rts

LoadFromEn968B:  
	ldy EnDataIndex,x
	lda $968B,y
	rts

OnPlayerNametable:  
	lda EnNameTable,x	;get enemy name table
	tay					;put A in Y
	eor ObjectHi		;eor with player name table (only return 1 when they are a mismatch)
	lsr					;set the carry flag with the result
	rts					;return

;you may notice we use nametabes to compare distance, but not using the PPU control to check screen direction
;it's probably not needesd, since we're doing an ABS check anyways
;i believe this tracks the trigger direction for a distance check
;TODO: figure out the xxx1 1xxx flags
CheckDistanceToSamus:  
	lda #$E7						; 1110 0111	create default mask
	sta $06							; store in temp 6
	lda #$18						; 0001 1000
	jsr EnableEnStatusFlags			; start with flags set by default
	ldy EnDataIndex,x				; get enemy data index
	lda $96AB,y						; load distance check data
	beq +++++						; if 0, return with flags set
	tay								; 	place distance data into Y
	lda $0405,x						; 	load status flags
	and #$02						; 	check visibility flag
	beq ++++						; 	not visible, clear the flags
	tya								; 		visible, move the distance data back into A
	ldy #$F7						; 		make new mask 1111 0111 
	asl								; 		check if distance data MSB was set
	bcs +							; 		if so, keep current mask
	ldy #$EF						; 			else, create 1110 1111 mask 
*   lsr								; 		get data back with upper flag removed
	sta $02							; 		store distacne in zero page
	sty $06							; 		store mask in zero page
	lda ObjectY						; 	prep for a comparison between samus' Y position
	sta $00							;
	ldy EnYRoomPos,x				; 	and the enemy's Y position
	lda $0405,x						;  
	bmi +							; 	check distance check flag on enemy status flags- 1 = vert, 0 = hori
	ldy ObjectX						; 	prep for a comparison between samus' x position
	sty $00							; 
	ldy EnXRoomPos,x				; 	and the enemy's X position
*   lda ObjectHi					; 	Get Samus' nametable
	lsr								; 	pop nametable out
	ror $00							; 	shift player position right, with name table coming in the MSB
	lda EnNameTable,x				; load enemy name table
	lsr								; pop name table out
	tya								; move enemy position into A
	ror								; shift position right, bring in enemy name table into MSB
	sec								; set the carry flag
	sbc $00							; subtract the player position
	bpl +							; if the result is positive, jump ahead
	jsr TwosCompliment				; else if it's negative, flip it- we want ABS
*   lsr								; 
	lsr								; divide result by 8, fully moving everything back to the low nibble
	lsr								;
	cmp $02							; check if the number is grater than the distance we're checking
	bcc ++							; if we're less than, leave - if greater or equal, leave with flags set
*	lda $06							; use either 1110 0111, 1111 0111, or 1110 1111 for the status flags
ClearEnStatusFlags:  
	and $0405,x				;clear flags from process
	sta $0405,x				;store flags in enemy
*	rts					

UpdateEnDelay:  
	dec EnDelay,x			; decrement delay timer
	bne +					; not equal, return
	lda $0405,x				; timer hit 0 - load the enemy staus flags 
	and #$08				; check distance trigger
	bne ++					; 	if it's set, jump below 
	inc EnDelay,x			; distance trigger not set, delay another frame
*   rts						; return

InitEnemyMovement:
*	lda EnDataIndex,x				; load eney data idx - delay timer is finished, and samus is in range
	cmp #$07						; compare to 7 (the zeb style enemy- must be the same in every area)
	bne +							; 	if it's not a zeb style enemy, jump ahead
	jsr SFX_OutOfHole				; 	else, play the out of hole sound
	ldx PageIndex					; reload the enemy into X
*	inc EnStatus,x					; inc the enemy status from wait to move
	jsr UpdateEnAnimDirection965B	; update the enemy anim direction
	ldy EnDataIndex,x				; load the enemy data index into y
	lda $96CB,y						; load data from table
	clc								; clear carry 
	adc #$D1						; add D1 (zeb case- 0E -> DF)
	sta $00							; store into 0
	lda #$00						; load 0
	adc #$97						; add 97 + carry
	sta $01							; store into 01 (should now contain pointer some offset from 97D1) 
	lda FrameCount					; get the frame count
	eor RandomNumber1				; mask with a random number
	ldy #$00						; load 00 into Y
	and ($00),y						; AND the value at the table we stored off - zeb case 0000 1000
	tay								; put A into Y (zeb case 0000 1000 or 0000 0000)
	iny								; inc Y
	lda ($00),y						; load second value from table (zeb case 00)
	sta $0408,x						; store into 0408
	jsr LoadFrom977B						; get 977B data for enemy
	bpl ++							; 	acceleration bit for enemy set - clear delay timer and bit we checked to get in here
	lda #$00						; not an acceleration enemy- load 0
	sta EnCounter,x					; reset enemy counter
	sta $0407,x						; reset second enemy counter
	ldy $0408,x						; load our data from 408 (0 in zeb case)
	lda $972B,y						; load a number from here
	sta $6AFE,x						; store into vertical accel
	lda $973F,y						; load a number from here
	sta $6AFF,x						; load into horizontal accel
	lda $9753,y						; load from another table
	sta EnVertSpeed,x						; store into  enemy vert speed
	lda $9767,y						; load from one more table 
	sta $0403,x						; store into enemy horizontal speed
	lda $0405,x						; load enemy status flags
	bmi +							; 	negative, we do vertical checks to player
	lsr								; positive, we do horizontal checks- get direction from lowest bit
	bcc ++							; 	carry clear, skip
	jsr InvertHoriAccel				; 	flip initialized accel to match direction to player
	jmp ++							; jump down

*   and #$04						; isolate horizontal check flag
	beq +							; 	zero, skip 
	jsr InvertVertAccel						; 	flip init accel to match direction to player
*	lda #$DF						; 1100 1111
	jmp ClearEnStatusFlags			; clear delay timer flag and 0000 1000

GetEnemyXDirectionDataIndex:
  lda $0405,x			;load enemy status flag as is, get the X relation bit into the carry flag
  jmp +					;jump ahead, skipping the check for Y relation - not sure if this is useful? only used once in AreaCommon and deos nothing

GetEnemyDirectionDataIndex:  
	lda $0405,x			;load enemy status flags
	bpl +				;if we're checking X direction, not Y, only get the X relation bit into carry
	lsr				
	lsr
*	lsr					;else, we'll move a couple more over to get the Y realtion bit into carry
	lda EnDataIndex,x	;load the enemy daya index
	rol					;take our relation bit and put it in the LSB- effectifly double the index, and add 1
	tay					;move this to Y 
	rts					

GetRandom_EnIdxFrCnt:
	txa					;place enemy index into A
	lsr					
	lsr
	lsr					;div by 8, set carry if need be
	adc FrameCount		;add frame count + carry
	lsr					;halve
	rts					;return 

EnemyInitHealth:  
	ldy EnDataIndex,x			;get enemy data index	
	lda $969B,y					;get timer(?)
	sta $040D,x					;set timer(?)
	lda EnemyHitPointTbl,y		;get enemy hit points
	ldy EnSpecialAttribs,x		;see if the enemy is the tough version
	bpl +						;
	asl							;if so, double the health
*   sta EnHitPoints,x			;store into enemy hit points
*   rts

LaunchEnProjectile:
	lda $0405,x							; load enemy status flags
	and #$10							; get the samus in range flag
	beq -								;	0? Return
	lda $87								; else, load the data in $87 
	and EnStatus,x						; AND with the current status
	beq -								; 	not zero? return
	lda $87								; load the data at $87 again
	bpl +								; positive? jump ahead
	ldy $6B01,x							;	else, negative - load 6B01
	bne -								;		not 0? return
*   jsr FindEnProjectileSlot			; load the next available enemy projectile slot into Y
	bcs ++								;	 if the carry flag was set, we had no avaiable slots- return
	sta $0404,y							; we found a slot! clear the hit status here to 0 - though, this might mean soemthing different here
	jsr CreateEnProjectileCollision		; create the collision radius for the projectile
	lda $0405,x							; load the status flags for the current enemy
	lsr									; shift right - placing horizontal relation bit into carry
	lda $85								; load index from $85
	pha									; place onto stack
	rol									; bring the relation flag into the LSB
	tax									; then place A into X
	lda $978B,x							; double width animation table 
	pha									; push to the stack
	tya									; push the projectile slot index into A 
	tax									; then move that to X
	pla									; pop the table data back into A
	jsr ResetAnimIndex					; set the animation for the projectile
	ldx PageIndex						; load back the original enemy index
	lda #$01							; load 1
	sta EnStatus,y						; activate projectile
	and $0405,x							; AND with horizonatal relation bit
	tax									; put that into x
	lda Table15,x						; and use it as our index into the two-byte table below (2 or -2)
	sta $0403,y							; store in horizontal speed
	lda #$00							; load 0
	sta EnVertSpeed,y							; store in vertical speed
	ldx PageIndex						; load the enemy index again, in case we break early in the next routine
	jsr UnknownF8F8						; this sets up some extra data on some projectiles, just not sure what for 
	lda $0405,x							; load status flags
	lsr									; put horizontal relation byte into carry flag
	pla									; get that animation index back ($85)
	tax									; put it into X
	lda $97A3,x							; load the non- doubled data from 97A3
	sta $04								; store into $04
	txa									; 
	rol									; get the double direction index
	tax									;
	lda $979B,x							; load from 979B
	sta $05								; place into $05
	jsr UnknownF91D
	ldx PageIndex
	bit $87
	bvc ++
	lda $0405,x
	and #$01
	tay
	lda $0083,y
	jmp SetEnAnimIndex

FindEnProjectileSlot:  
	ldy #$60			; load the next slot after the enemies in the WRAM section
	clc					; clear carry 	
*   lda EnStatus,y		; load the state byte 
	beq +				; 0? return 
	jsr Yplus16			; 	else, get the next availabe slot
	cmp #$C0			; 	up to 6 slots
	bne -				;	repeat until we find an available slot or run out of slots
*   rts					; return with available slot in Y and 0 in A, if we found one
		
UnknownF8F8:  
	lda $85				; load the cached anim table index for this projectile launcher
	cmp #$02			; compare with 2
	bcc +				; A>=2? return - this projectile doesn't need anything else
	ldx PageIndex		; load the enemy index
	lda $0405,x			; load the enemy status flags
	lsr					; shift right, get horizontal relation bit in carry
	lda $88				; load data at $88
	rol					; double it and add the horizzontal bit to the end, making it a double index
	and #$07			; %8
	sta $040A,y			; store result into orientation byte of projectile
	lda #$02			; load 2
	sta EnStatus,y		; status is moving
	lda #$00			; load 0
	sta EnDelay,y		; no delay on enemy
	sta EnAnimDelay,y	; no delay on anim
	sta $0408,y			; not sure what this is yet, but store to it
*   rts					; return

UnknownF91D:  
	ldx PageIndex					; get current enemy index
	jsr MakeEnemyLocationStruct98B	; make a location struct 
	tya								; put the current projectile index into A
	tax								; then, X
	jsr CalcPotentialPosition
	jmp CopyEnemyLocationStruct98B	; copy enemy location data we cached off into the projectile

; Table used by above subroutine

Table15:
	.byte $02
	.byte $FE

CreateEnProjectileCollision:  
	lda #$02		; load 2
	sta EnRadY,y	; store as collision radius in both x and y
	sta EnRadX,y	; directions
	ora $0405,y		; set the visibility flag in the status flags to true
	sta $0405,y		;
	rts				; return

UpdateEnProjectiles:
    ldx #$B0				; start at offset B0, the last slot index for enemy projectiles
*   jsr DoOneEnProjectile	; update one projectile
	ldx PageIndex			; load the current index
	jsr Xminus16			; sub 16
	cmp #$60				; continue until we've reached the finnal projectile
	bne -
DoOneEnProjectile:  
	stx PageIndex			; store projectile slot into page index
	lda $0405,x				; load the enemy status flags
	and #$02				; check visibility
	bne +					; if visible, jump ahead
	jsr KillObject			; 	else, free the slot
*   lda EnStatus,x			; load the status of the enemy 
	beq Exit19				; if it's 0, return to above and go to next projectile
	jsr ChooseRoutine		; else, go update the projectile

; Pointer table to code

	.word ExitSub     		;($C45C) rts
	.word UnknownF96A
	.word UnknownF991       ; spit dragon's fireball
	.word ExitSub     		;($C45C) rts
	.word UnknownFA6B
	.word UnknownFA91

Exit19: rts

UnknownF96A:  
	jsr UnknownFA5B
	jsr EnemyBGCrashDetection
	ldx PageIndex
	bcs UnknownF97C
	lda EnStatus,x
	beq Exit19
	jsr UnknownFA60
UnknownF97C:  
	lda #$01
UnknownF97E:  
	jsr UpdateEnemyAnim
	jmp UpdateObjectCntrl
*   inc $0408,x
UnknownF987:  
	inc $0408,x
	lda #$00
	sta EnDelay,x
	beq +
	
UnknownF991:  
	jsr UnknownFA5B
	lda $040A,x
	and #$FE
	tay
	lda AreaProjectileMovementTable,y
	sta $0A
	lda AreaProjectileMovementTable+1,y
	sta $0B
*   ldy $0408,x
	lda ($0A),y
	cmp #$FF
	bne +
	sta $0408,x
	jmp UnknownF987
*   cmp EnDelay,x
	beq ---
	inc EnDelay,x
	iny							;y = index into movement data
	lda ($0A),y					;load condensed movement data
	jsr ExtractVerticalNibble	;extract verticle speed
	ldx PageIndex				;get the enemy index
	sta EnVertSpeed,x					;store to enemy vertical speed
	lda ($0A),y					;reload condensed movement data
	jsr ExtractHorizontalNibble ;extract horizontal speed
	ldx PageIndex				;get enemy index
	sta $0403,x					;store to enemy horizontal speed
	tay							;move horizontal speed into Y
	lda $040A,x					;load enemy orientation
	lsr							;get lowest bit (left or right normal/enemy is sideways)
	php							;push status to stack
	bcc +						;if we popped a 1 out, jump ahead
	tya							;else, put horizontal back into A
	jsr TwosCompliment			;flip the speed direction
	sta $0403,x					;store flipped speed into horizontal speed
*   plp							;bring back the status
	bne +						;if not 0, jump ahead
	lda EnVertSpeed,x					;else, load vertical speed
	beq +						;jump ahead if the speed is 0
	bmi +						;or less than 0
	ldy $040A,x					;load orientation (0->3)
	lda $95E0,y					;index ??? in area data
	sta EnResetAnimIndex,x
*   jsr EnemyBGCrashDetection
	ldx PageIndex
	bcs ++
	lda EnStatus,x
	beq Exit20
	ldy #$00
	lda $040A,x
	lsr
	beq +
	iny
*   lda $95E2,y
	jsr ResetAnimIndex
	jsr FreezeEnemy
	lda #$0A
	sta EnDelay,x
*   jmp UnknownF97C

KillObject:
	lda #$00			;
	sta EnStatus,x			;Store #$00 as enemy status(enemy slot is open).
	rts				;

; enemy<-->background crash detection
EnemyBGCrashDetection:  lda InArea
	cmp #$11
	bne +
	lda EnStatus,x
	lsr
	bcc ++
*   jsr GetEnTileCoords23B45
	ldy #$00
	lda ($04),y
	cmp #$A0
	bcc ++
	ldx PageIndex
*       lda $0403,x
	sta $05
	lda EnVertSpeed,x
	sta $04
UnknownFA41:  
	jsr MakeEnemyLocationStruct98B
	jsr CalcPotentialPosition
	bcc KillObject			;($FA18)Free enemy data slot.
CopyEnemyLocationStruct98B: 
	lda $08
	sta EnYRoomPos,x
	lda $09
	sta EnXRoomPos,x
	lda $0B
	and #$01
	sta EnNameTable,x
*   rts

UnknownFA5B:  lda $0404,x
	beq Exit20
UnknownFA60:  lda #$00
	sta $0404,x
	lda #$05
	sta EnStatus,x
Exit20: rts

UnknownFA6B:  
	lda EnAnimFrame,x
	cmp #$F7
	beq +
	dec EnDelay,x
	bne ++
*   jsr KillObject			;($FA18)Free enemy data slot.
*   jmp UnknownF97C

GetEnTileCoords23B45:  
	ldx PageIndex
	lda EnYRoomPos,x
	sta $02
	lda EnXRoomPos,x
	sta $03
	lda EnNameTable,x
	sta $0B
	jmp MakeWRAMPtr

UnknownFA91:  jsr KillObject			;($FA18)Free enemy data slot.
	lda $95DC
	jsr ResetAnimIndex
	jmp UnknownF97C

EnemyDestruction:
  ldx #$C0
*       stx PageIndex
	lda EnStatus,x
	beq +
	jsr UnknownFAB4
*       lda PageIndex
	clc
	adc #$08
	tax
	cmp #$E0
	bne --
*      rts

UnknownFAB4:  dec EnCounter,x
	bne ++
	lda #$0C
	sta EnCounter,x
	dec $0407,x
	bmi +
	bne ++
*   jsr KillObject			;($FA18)Free enemy data slot.
*	lda EnCounter,x
	cmp #$09
	bne +
	lda $0407,x
	asl
	tay
	lda Table16,y
	sta $04
	lda Table16+1,y
	sta $05
	jsr UnknownFA41
*   lda #$80
	sta ObjectCntrl
	lda #$03
	jmp UnknownF97E

; Table used by above subroutine

Table16:
	.byte $00
	.byte $00
	.byte $0C
	.byte $1C
	.byte $10
	.byte $F0
	.byte $F0
	.byte $08

UpdatePipeSpawns:
	ldy #$18			; load spawner offset, doing 3 spawners 
*   jsr DoOneSpawner
	lda PageIndex
	sec
	sbc #$08
	tay
	bne -				; do last spawner naturally

DoOneSpawner:  
	sty PageIndex							; store 0x18 into page index
	ldx EnSpawnerStatus,y					; load spawner status into X
	inx										; inc x (if inactive, will be FF)
	beq -----								; if zero, return 
	ldx EnSpawnerEnSlot,y					; else, load the enemy slot alloted for this spawner
	lda EnStatus,x							; load the enemy status at the slot
	beq +									; if it's an empty slot, jump ahead
	lda $0405,x								; 	else, load the en status flags
	and #$02								; 	check and with visibility flag
	bne Exit13								; 	visible? exit - nothing to do while it exists
*   sta $0404,x								; not visible/inactive - store 0 into enemy hit status
	lda #$FF								; load 0xFF 
	cmp EnDataIndex,x						; check against the enemy data index - should be FF if the enemy is gone enough
	bne +									; not equal, enemy is NOT gone enough
	dec EnDelay,x							; 	else, count down the enemy delay
	bne Exit13								; 		if it's not 0 yet, leave
	lda EnSpawnerStatus,y					; 	load the spawner status again - contains the actual enemy data we want
	jsr SetEnTypeAndAttributes				; 	use spawner status as enemy type
	ldy PageIndex							; load y again (?) 
	lda EnSpawnerYRoomPos,y					; 
	sta EnYRoomPos,x						;
	lda EnSpawnerXRoomPos,y					; copy spawner position info to enemy
	sta EnXRoomPos,x						;
	lda EnSpawnerNametable,y				;
	sta EnNameTable,x						;
	lda #$18								; the spawner enemies apparently have a consistent hit box
	sta EnRadX,x							; 
	lda #$0C								;
	sta EnRadY,x							; set enemy hitbox
	ldy #$00								;
	jsr CreateObjYLocationStruct68PPUdiffA	; Y = player	
	jsr CreateEnemyLocationStruct79PPUdiffB	; "enemy" here is the new spawned enemy
	jsr AddAndCacheEnObjRadYX45				; add collision bounds together
	jsr CheckHitboxOverlap					; check for hitbox overlap
	bcc Exit13								; 	carry flag clear, samus is standing on enemy- can't spawn, return
	lda #$01								; no overlap, load 1
	sta EnDelay,x							; store into enemy delay
	sta EnStatus,x							; and enemy status (waiting)
	and ScrollDir							; get positive (0001) or negative(0000) scroll direction
	asl										; shift left (0010 or 0000)
	sta $0405,x								; init enemy status flags with flag for 0001 0000 Temporarily shoved one space to the left
	ldy EnDataIndex,x						; load the enemy data index 
	jsr UnknownFB7B							; init distance check bit, move 0001 0000 flag into place 		
	jmp EnemyInitHealth						; init enemy health

*   sta EnDataIndex,x						; store into enemy data index
	lda #$01
	sta EnDelay,x
	jmp KillObject			;($FA18)Free enemy data slot.

UnknownFB7B:  
	jsr LoadFrom977B					;load 977b data, pop out MSB into carry 
	ror $0405,x					;pull onto status flags as the direction to check for samus (horizontal or vertical)
	lda EnemyInitDelayTbl,y		;Load initial delay for enemy movement.
	sta EnDelay,x				;

Exit13: 
	rts				;Exit from multiple routines.

UpdateHomeRelativeAnimation:
	ldx PageIndex					; get current enemy index
	jsr GetEnemyDirectionDataIndex	; get the enemy directional data index into Y
	lda $6B01,x						; get 6B01, some kinda home displacement - vertical?
	inc $6B03,x						; get 6B03 - some kinda thing with directions. 
	dec $6B03,x						; see if we get to 0 with this so we don't have to sacrifice out A load
	bne +							; not zero? jump ahead and use 6B03 as our pos/neg check
	pha								;	else push 6B01 to the stack
	pla								;	then pop it right back out - simulates a fresh load for the N and Z flags
*   bpl +							;	check the negative flag- above home? jump ahead
	jsr TwosCompliment				;		get ABS of home displacement
*   cmp #$08						; cmp 6B01 to 8
	bcc +							; less than 8, fallback to a 3B anim
	cmp #$10						; 	else, compare with 10
	bcs Exit13						;	if >= 10, return - probably already set the anim 
	tya								; else, take our directional data index in A
	and #$01						; get lower bit (left = 0, right = 1)
	tay								; put that back into y
	lda $0085,y						; get the value stored at either 85 or 86 (sin wave guy example, 21 and 1E)
	cmp EnResetAnimIndex,x			; check if that's the same as the reset anim
	beq Exit13						; same animation? leave
	sta EnAnimIndex,x				; 	else, set new animation
	dec EnAnimIndex,x				;	and decrease it, I guess? maybe it updates one frame forward in a bit

QueueEnAnimation:					; 
	sta EnResetAnimIndex,x			; set new animation starting index
	jmp ResetEnAnimDelay			; reset the animation delay, and return

FallbackEnAnimDirection963B:
*   lda EnDirAnimTb3B,y				
	cmp EnResetAnimIndex,x
	beq Exit13
	jmp ResetAnimIndex

UpdateEnAnimationDirection:
	ldx PageIndex					; get enemy index
	jsr GetEnemyDirectionDataIndex	; get direction data index
	lda EnDirAnimTb5B,y				; load anim from table
	cmp EnResetAnimIndex,x			; if it's the same anim, 
	beq Exit13						; leave
	sta EnResetAnimIndex,x			; else, set the anim
	jmp SetEnAnimIndex				; and finish setting 

DestroyGeenSpinner:
	lda #$40
	sta PageIndex
	ldx #$0C
*   jsr UnknownFBEC
	dex
	dex
	dex
	dex
	bne -
UnknownFBEC:  		;this has something to do with enemies that explode
	lda $A0,x
	beq ++
	dec $A0,x
	txa
	lsr
	tay
	lda Table17,y
	sta $04
	lda Table17+1,y
	sta $05
	lda $A1,x
	sta $08
	lda $A2,x
	sta $09
	lda $A3,x
	sta $0B
	jsr CalcPotentialPosition
	bcc +++
	lda $08
	sta $A1,x
	sta $034D
	lda $09
	sta $A2,x
	sta $034E
	lda $0B
	and #$01
	sta $A3,x
	sta $034C
	lda $A3,x
	sta $034C
	lda #$5A
	sta $0343
	txa
	pha
	jsr DrawFrame
	lda SamusBlink
	bne +
	ldy #$00
	ldx #$40
	jsr CheckObjYObjXHBOverlap
	bcs +
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	ldy #$00
	bcc +
	clc
	jsr UnknownF311
	lda #$50
	sta HealthLoChange
	jsr SubtractHealth		;($CE92)
*   pla
	tax
*   rts

*   lda #$00
	sta $A0,x
	rts

; Table used by above subroutine

Table17:
	.byte $00
	.byte $FB
	.byte $FB
	.byte $FE
	.byte $FB
	.byte $02
	.byte $00
	.byte $05

UpdateMellowEnemies:
  lda $6BE4
	beq ++
	ldx #$F0
	stx PageIndex
	lda $6BE9
	cmp $95E4
	bne +++
	lda #$03
	jsr UpdateEnemyAnim
	lda RandomNumber1
	sta $8A
	lda #$18
*       pha
	tax
	jsr UnknownFC98
	pla
	tax
	lda $B6,x
	and #$F8
	sta $B6,x
	txa
	sec
	sbc #$08
	bpl -
*       rts

*      jmp KillObject			;($FA18)Free enemy data slot.

UnknownFC98:  lda $B0,x
	jsr ChooseRoutine

; Pointer table to code

	.word ExitSub       ;($C45C) rts
	.word UnknownFCA5
	.word UnknownFCB1
	.word UnknownFCBA

UnknownFCA5:  
	jsr UnknownFD84
	jsr UnknownFD08
	jsr UnknownFD25
	jmp UpdateObjectCntrl

UnknownFCB1:  jsr UnknownFD84
	jsr UnknownFCC1
	jmp UpdateObjectCntrl

UnknownFCBA:  lda #$00
	sta $B0,x
	jmp SFX_EnemyHit

UnknownFCC1:  jsr UnknownFD5F
	lda $B4,x
	cmp #$02
	bcs +
	ldy $08
	cpy ObjectY
	bcc +
	ora #$02
	sta $B4,x
*       ldy #$01
	lda $B4,x
	lsr
	bcc +
	ldy #$FF
*       sty $05
	ldy #$04
	lsr
	lda $B5,x
	bcc +
	ldy #$FD
*       sty $04
	inc $B5,x
	jsr CalcPotentialPosition
	bcs +
	lda $B4,x
	ora #$02
	sta $B4,x
*       bcc +
	jsr UnknownFD6C
*       lda $B5,x
	cmp #$50
	bcc +
	lda #$01
	sta $B0,x
*       rts

UnknownFD08:  lda #$00
	sta $B5,x
	tay
	lda ObjectX
	sec
	sbc $B2,x
	bpl +
	iny
	jsr TwosCompliment		;($C3D4)
*       cmp #$10
	bcs +
	tya
	sta $B4,x
	lda #$02
	sta $B0,x
*       rts

UnknownFD25:  txa
	lsr
	lsr
	lsr
	adc $8A
	sta $8A
	lsr $8A
	and #$03
	tay
	lda Table18,y
	sta $04
	lda Table18+1,y
	sta $05
	jsr UnknownFD5F
	lda $08
	sec
	sbc ScrollY
	tay
	lda #$02
	cpy #$20
	bcc +
	jsr TwosCompliment		;($C3D4)
	cpy #$80
	bcc ++
*   sta $04
*   jsr CalcPotentialPosition
	jmp UnknownFD6C

; Table used by above subroutine

Table18:
	.byte $02
	.byte $FE
	.byte $01
	.byte $FF
	.byte $02

UnknownFD5F:  lda $B3,x
	sta $0B
	lda $B1,x
	sta $08
	lda $B2,x
	sta $09
	rts

UnknownFD6C:  
	lda $08
	sta $B1,x
	sta $04F0
	lda $09
	sta $B2,x
	sta $04F1
	lda $0B
	and #$01
	sta $B3,x
	sta $6BEB
	rts

UnknownFD84:  
	lda $B6,x
	and #$04
	beq +
	lda #$03
	sta $B0,x
*   rts

CalcPotentialPosition:
	lda ScrollDir			; load scroll direction
	and #$02				; and with the horizontal bit
	sta $02					; store in $02
	lda $04					; load cached vert speed of object
	clc						; clear carry
	bmi +++					; 	vert speed negative- jump below to negative process
	beq CalcPotentialXPos			; 	speed zero?, branch below 
	adc $08					; speed positive, moving object down screen
	bcs +					; 	carry flag set when we pass lowest position
	cmp #$F0				; else, compare with F0 (240)
	bcc ++					; 	less than? jump ahead
*   adc #$0F				; else, we're in the last little bit of the screen- add 16 to get us back to the top of the screen early (to skip attribute table data)
	ldy $02					; load the scroll direction to y
	bne ClcExit2			; 	if we're scrolling horizontal, this wrap around stuff means nothing- clear carry and exit
	inc $0B					; else, increment the nametable ?
*   sta $08					; store newly created y position
	jmp CalcPotentialXPos	; go to horizontal speed checks

*   adc $08					; speed negative, moving up screen
	bcs +					;  carry flag set when we pass the highest position
	sbc #$0F				; subtract 0F (no carry)
	ldy $02					; check horizontal movement
	bne ClcExit2			; horizontal movement? clear carry and leave
	inc $0B					; increment nametable ?
*   sta $08					; store Y position in 

CalcPotentialXPos:  
	lda $05					; load cacehd horizontal speed of object 
	clc						; clear carry
	bmi ++					; 	if horizontal speed negative, jump ahead
	beq SecExit				; 	0 speed, set carry flag and leave
	adc $09					; add object x position
	bcc +					; 	carry flag is set when we wrap the position around
	ldy $02					; 		load the scroll direction
	beq ClcExit2			; 		if it's 0, we're vertical scrolling, and don't care about wrap arounds- clear the carry and leave
	inc $0B					; increment the nametable	?
*   jmp ++					; jump ahead to store data in the position struct X position

*   adc $09					; "subtract" from x position
	bcs +					; carry flag set on wrap- just junp ahead and set sttruct
	ldy $02					; 	else, load scroll direction
	beq ClcExit2			; 	vert scrolling? clear and leave
	inc $0B					; inc nametable ?
*   sta $09					; store new position struct's X position
SecExit:
	sec						; set carry flag
	rts						; leave

ClcExit2:
	clc						; clear carry 
*   rts						; leave


UpdateEndTimer:
  lda EndTimerHi
	cmp #$99
	bne +
	clc
	sbc EndTimerLo  ; A = zero if timer just started
	bne +	   ; branch if not
	sta $06
	lda #$38
	sta $07
	jsr UnknownDC54
*       ldx #$20
*       jsr UnknownFE05
	txa
	sec
	sbc #$08
	tax
	bne -

UnknownFE05:  lda $0758,x
	sec
	sbc #$02
	bne ---
	sta $06
	inc $0758,x
	txa
	lsr
	adc #$3C
	sta $07
	jmp UnknownDC54

; Tile degenerate/regenerate

UpdateTiles:
	ldx #$C0
*   jsr DoOneTile
	ldx PageIndex
	jsr Xminus16
	bne -
	DoOneTile:
	stx PageIndex
	lda TileRoutine,x
	beq +	       ; exit if tile not active
	jsr ChooseRoutine

; Pointer table to code

	.word ExitSub       ;($C45C) rts
	.word UnknownFE3D
	.word UnknownFE54
	.word UnknownFE59
	.word UnknownFE54
	.word UnknownFE83

UnknownFE3D:  
	inc TileRoutine,x
	lda #$00
	jsr SetTileAnim
	lda #$50
	sta TileDelay,x
	lda TileWRAMLo,x     ; low WRAM addr of blasted tile
	sta $00
	lda TileWRAMHi,x     ; high WRAM addr
	sta $01

UnknownFE54:  
	lda #$02
	jmp UpdateTileAnim

UnknownFE59:  
	lda FrameCount
	and #$03
	bne +       ; only update tile timer every 4th frame
	dec TileDelay,x
	bne +       ; exit if timer not reached zero
	inc TileRoutine,x
	ldy TileType,x
	lda Table19,y
	SetTileAnim:
	sta TileAnimIndex,x
	sta $0505,x
	lda #$00
	sta TileAnimDelay,x
*   rts

; Table used for indexing the animations in TileBlastAnim (see below)

Table19:
	.byte $18,$1C,$20,$00,$04,$08,$0C,$10,$24,$14

UnknownFE83:  
	lda #$00
	sta TileRoutine,x       ; tile = respawned
	lda TileWRAMLo,x
	clc
	adc #$21
	sta $00
	lda TileWRAMHi,x
	sta $01
	jsr UnknownFF3C
	lda $02
	sta $07
	lda $03
	sta $09
	lda $01
	lsr
	lsr
	and #$01
	sta $0B
	ldy #$00
	jsr CreateObjYLocationStruct68PPUdiffA
	lda #$04
	clc
	adc ObjRadY
	sta $04
	lda #$04
	clc
	adc ObjRadX
	sta $05
	jsr CheckHitboxOverlap
	bcs Exit23
	jsr UnknownF311
	lda #$50
	sta HealthLoChange
	jmp SubtractHealth		;($CE92)

	GetTileFramePtr:
	lda TileAnimFrame,x
	asl
	tay
	lda $97AF,y
	sta $02
	lda $97B0,y
	sta $03
	
Exit23: 
	rts

DrawTileBlast:
	lda PPUStrIndex
	cmp #$1F
	bcs Exit23
	ldx PageIndex
	lda TileWRAMLo,x
	sta $00
	lda TileWRAMHi,x
	sta $01
	jsr GetTileFramePtr
	ldy #$00
	sty $11
	lda ($02),y
	tax
	jsr Adiv16       ; / 16
	sta $04
	txa
	and #$0F
	sta $05
	iny
	sty $10
*   ldx $05
*   ldy $10
	lda ($02),y
	inc $10
	ldy $11
	sta ($00),y
	inc $11
	dex
	bne -
	lda $11
	clc
	adc #$20
	sec
	sbc $05
	sta $11
	dec $04
	bne --
	lda $01
	and #$04
	beq +
	lda $01
	ora #$0C
	sta $01
*   lda $01
	and #$2F
	sta $01
	jsr EraseTile
	clc
	rts

UnknownFF3C:  
	lda $00
	tay
	and #$E0
	sta $02
	lda $01
	lsr
	ror $02
	lsr
	ror $02
	tya
	and #$1F
	jsr Amul8       ; * 8
	sta $03
	rts

UpdateTileAnim:
	ldx PageIndex
	ldy TileAnimDelay,x
	beq +
	dec TileAnimDelay,x
	bne ++
*       sta TileAnimDelay,x
	ldy TileAnimIndex,x
	lda TileBlastAnim,y
	cmp #$FE	    ; end of "tile-blast" animation?
	beq ++
	sta TileAnimFrame,x
	iny
	tya
	sta TileAnimIndex,x
	jsr DrawTileBlast
	bcc +
	ldx PageIndex
	dec TileAnimIndex,x
*	rts

*       inc TileRoutine,x
	pla
	pla
	rts

; Frame data for tile blasts

	TileBlastAnim:
	.byte $06,$07,$00,$FE
	.byte $07,$06,$01,$FE
	.byte $07,$06,$02,$FE
	.byte $07,$06,$03,$FE
	.byte $07,$06,$04,$FE
	.byte $07,$06,$05,$FE
	.byte $07,$06,$09,$FE
	.byte $07,$06,$0A,$FE
	.byte $07,$06,$0B,$FE
	.byte $07,$06,$08,$FE

	.byte $00
	.byte $00

;-----------------------------------------------[ RESET ]--------------------------------------------

RESET:
	SEI						;Disables interrupt
	CLD						;Sets processor to binary mode
	LDX #$00				;
	STX PPUControl0			;Clear PPU control registers
	STX PPUControl1			;
*	LDA PPUStatus			;
	BPL -					;Wait for VBlank
*	LDA PPUStatus			;
	BPL -					;
	ORA #$FF				;
	STA MMC1Reg0			;Reset MMC1 chip
	STA MMC1Reg1			;(MSB is set)
	STA MMC1Reg2			;
	STA MMC1Reg3			;
	JMP Startup				;($C01A)Do preliminary housekeeping.

;we're using ophis to compile, which doesn't let us double org
.advance $FFFA
;-----------------------------------------[ Interrupt vectors ]--------------------------------------

FFFA: 	.word NMI			;($C0D9)NMI vector.
FFFC:	.word RESET			;($FFB0)Reset vector.
FFFE: 	.word RESET			;($FFB0)IRQ vector.