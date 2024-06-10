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

.require "Defines.asm"

;-------------------------------------[ Forward declarations ]--------------------------------------

.alias ObjectAnimIndexTbl	$8572
.alias FramePtrTable		$860B
.alias PlacePtrTable		$86DF
.alias StarPalSwitch		$8AC7
.alias SamusEnterDoor		$8B13
.alias AreaPointers		$9598
.alias AreaRoutine		$95C3
.alias EnemyHitPointTbl		$962B
.alias EnemyInitDelayTbl	$96BB
.alias DecSpriteYCoord		$988A
.alias NMIScreenWrite		$9A07
.alias EndGamePalWrite		$9F54
.alias SpecItmsTable		$9598
.alias CopyMap			$A93E
.alias SoundEngine		$B3B4

.alias Unknown9560 $9560
.alias Unknown9561 $9561
.alias Unknown98BF $98BF
.alias Unknown95D7 $95D7
.alias Unknown95D8 $95D8
.alias Unknown95DA $95DA
.alias Unknown95AB $95AB
.alias Unknown95D9 $95D9
.alias Unknown95C0 $95C0
.alias Unknown95CD $95CD
.alias Unknown832F $832F
.alias DisplayDoors $8B79
.alias Unknown8296 $8296

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
	jmp $8000				;Jump to $8000, where a routine similar to the one-->
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

*	dey						;Palette # = PalDataPending - 1.
	tya						;
	asl						;* 2, each pal data ptr is 2 bytes (16-bit).
	tay						;
	ldx Unknown9560,y		;X = low byte of PPU data pointer.
	lda Unknown9561,y		;
	tay						;Y = high byte of PPU data pointer.
	lda #$00				;Clear A.
	sta PalDataPending		;Reset palette data pending byte.
	
PreparePPUProcess_:
	stx $00					;Lower byte of pointer to PPU string.
	sty $01					;Upper byte of pointer to PPU string.
	jmp ProcessPPUString	;Write data string to PPU.

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
	ldx #$01			;First timer to decrement is Timer2.
	dec TimerDelay			;
	bpl DecTimer			;
	lda #$09			;TimerDelay hits #$00 every 10th frame.
	sta TimerDelay			;Reset TimerDelay after it hits #$00.
	ldx #$02			;Decrement Timer3 every 10 frames.

DecTimer:
	lda Timer1,x			;
	beq +				;Don't decrease if timer is already zero.
	dec Timer1,x			;
*	dex				;Timer1 and Timer2 decremented every frame.
	bpl DecTimer			;
	rts				;

;-----------------------------------------[ Choose routine ]-----------------------------------------

;This is an indirect jump routine. A is used as an index into a code
;pointer table, and the routine at that position is executed. The programmers
;always put the pointer table itself directly after the JSR to ChooseRoutine,
;meaning that its address can be popped from the stack.

ChooseRoutine:
	asl				;* 2, each ptr is 2 bytes (16-bit).
	sty TempY			;Temp storage.
	stx TempX			;Temp storage.
	tay				;
	iny				;
	pla				;Low byte of ptr table address.
	sta CodePtr			;
	pla				;High byte of ptr table address.
	sta CodePtr+1			;
	lda (CodePtr),y			;Low byte of code ptr.
	tax				;
	iny				;
	lda (CodePtr),y			;High byte of code ptr.
	sta CodePtr+1			;
	stx CodePtr			;
	ldx TempX			;Restore X.
	ldy TempY			;Restore Y.
	jmp (CodePtr)			;

;--------------------------------------[ Write to scroll registers ]---------------------------------

WriteScroll:
	lda PPUStatus			;Reset scroll register flip/flop
	lda ScrollX			;
	sta PPUScroll			;
	lda ScrollY			;X and Y scroll offsets are loaded serially.
	sta PPUScroll			;
	rts				;

;----------------------------------[ Add y index to stored addresses ]-------------------------------

;Add Y to pointer at $0000. 

AddYToPtr00:
	tya				;
	clc				;Add value stored in Y to lower address-->
	adc $00				;byte stored in $00.
	sta $00				;
	bcc +				;Increment $01(upper address byte) if carry-->
	inc $01				;has occurred.
*	rts				;

;Add Y to pointer at $0002

AddYToPtr02:
	tya				;
	clc				;Add value stored in Y to lower address-->
	adc $02				;byte stored in $02.
	sta $02				;
	bcc +				;Increment $01(upper address byte) if carry-->
	inc $03				;has occurred.
*	rts				;

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
	lda PPUDataPending		;
	beq +	   			;If zero no PPU data to write, branch to exit.
	lda #$A1			;			
	sta $00				;Sets up PPU writer to start at address $07A1.
	lda #$07			;
	sta $01	 			;$0000 = ptr to PPU data string ($07A1).
	jsr ProcessPPUString    	;($C30C)write it to PPU.
	lda #$00			;
	sta PPUStrIndex			;PPU data string has been written so the data-->
	sta PPUDataString		;stored for the write is now erased.
	sta PPUDataPending		;
*	rts				;

PPUWrite:
	sta PPUAddress			;Set high PPU address.
	iny				;
	lda ($00),y			;
	sta PPUAddress			;Set low PPU address.
	iny				;
	lda ($00),y			;Get data byte containing rep length & RLE status.
	asl				;Carry Flag = PPU address increment (0 = 1, 1 = 32).
	jsr SetPPUInc			;($C318)Update PPUCtrl0 according to Carry Flag.
	asl				;Carry Flag = bit 6 of byte at ($00),y (1 = RLE).
	lda ($00),y			;Get data byte again.
	and #$3F			;Keep lower 6 bits as loop counter.
	tax				;
	bcc PPUWriteLoop		;If Carry Flag not set, the data is not RLE.
	iny				;Data is RLE, advance to data byte.

PPUWriteLoop:
	bcs +				;
	iny				;Only inc Y if data is not RLE.
*	lda ($00),y			;Get data byte.
	sta PPUIOReg			;Write to PPU.
	dex				;Decrease loop counter.
	bne PPUWriteLoop		;Keep going until X=0.
	iny				;
	jsr AddYToPtr00			;($C2A8)Point to next data chunk.

;Write data string at ($00) to PPU.

ProcessPPUString:
	ldx PPUStatus			;Reset PPU address flip/flop.
	ldy #$00			;
	lda ($00),y			;
	bne PPUWrite			;If A is non-zero, PPU data string follows,-->
	jmp WriteScroll			;($C29A)Otherwise we're done.

;In: CF = desired PPU address increment (0 = 1, 1 = 32).
;Out: PPU control #0 ($2000) updated accordingly.

SetPPUInc:
	pha				;Preserve A.
	lda PPUCNT0ZP			;
	ora #$04			;
	bcs +				;PPU increment = 32 only if Carry Flag set,-->
	and #$FB			;else PPU increment = 1.
*	sta PPUControl0			;
	sta PPUCNT0ZP			;
	pla				;Restore A.
	rts				;

;Erase blasted tile on nametable.  Each screen is 16 tiles across and 15 tiles down.
EraseTile:
	ldy #$01			;
	sty PPUDataPending		;data pending = YES.
	dey				;
	lda ($02),y			;
	and #$0F			;
	sta $05				;# of tiles horizontally.
	lda ($02),y			;
	jsr Adiv16			;($C2BF)/ 16.
	sta $04				;# of tiles vertically.
	ldx PPUStrIndex			;
*	lda $01				;
	jsr WritePPUByte		;($C36B)write PPU high address to $07A1,PPUStrIndex.
	lda $00				;
	jsr WritePPUByte		;($C36B)write PPU low address to $07A1,PPUStrIndex.
	lda $05				;data length.
	sta $06				;
	jsr WritePPUByte		;($C36B)write PPU string length to $07A1,PPUStrIndex.
*	iny				;
	lda ($02),y			;Get new tile to replace old tile.
	jsr WritePPUByte		;($C36B)Write it to $07A1,PPUStrIndex, inc x.
	dec $06				;
	bne -				;Branch if more horizontal tiles to replace.
	stx PPUStrIndex			;
	sty $06				;
	ldy #$20			;
	jsr AddYToPtr00			;($C2A8)Move to next name table line.
	ldy $06				;Store index to find next tile info.
	dec $04				;
	bne --				;Branch if more lines need to be changed on name table.
	jsr EndPPUString		;($c376)Finish writing PPU string and exit.

WritePPUByte:
	sta PPUDataString,x		;Store data byte at end of PPUDataString.

NextPPUByte:
	inx				;PPUDataString has increased in size by 1 byte.
	cpx #$4F			;PPU byte writer can only write a maximum of #$4F bytes
	bcc +				;If PPU string not full, branch to get more data.
	ldx PPUStrIndex			;

EndPPUString:
	lda #$00			;If PPU string is already full, or all PPU bytes loaded,-->
	sta PPUDataString,x		;add #$00 as last byte to the PPU byte string.
	pla				;
	pla				;Remove last return address from stack and jump out of-->
*	rts				;PPU writing routines.

;The following routine is only used by the intro routine to load the sprite 
;palette data for the twinkling stars. The following memory addresses are used:
;$00-$01 Destination address for PPU write, $02-$03 Source address for PPU data,
;$04 Temp storage for PPU data byte, $05 PPU data string counter byte,
;$06 Temp storage for index byte.

PrepPPUPaletteString:
	ldy #$01			;
	sty PPUDataPending		;Indicate data waiting to be written to PPU.
	dey				;
	beq ++++			;Branch always

*	sta $04				;$04 now contains next data byte to be put into the PPU string.
	lda $01				;High byte of staring address to write PPU data 
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	lda $00				;Low byte of starting address to write PPU data.
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	lda $04				;A now contains next data byte to be put into the PPU string.
	jsr SeparateControlBits		;($C3C6)Break control byte into two bytes.

	bit $04				;Check to see if RLE bit is set in control byte.-->
	bvc WritePaletteStringByte	;If not set, branch to load byte. Else increment index-->
	iny				;to find repeating data byte.

WritePaletteStringByte:
	bit $04				;Check if RLE bit is set (again). if set, load same-->
	bvs +				;byte over and over again until counter = #$00.
	iny				;Non-repeating data byte. Increment for next byte.
*	lda ($02),y			;
	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
	sty $06				;Temporarily store data index.
	ldy #$01			;PPU address increment = 1.
	bit $04				;If MSB set in control bit, it looks like this routine might-->
	bpl +				;have been used for a software control verticle mirror, but
					;the starting address has already been written to the PPU-->
					;string so this section has no effect whether the MSB is set-->
					;or not. The PPU is always incremented by 1.
	ldy #$20			;PPU address increment = 32.
*	jsr AddYToPtr00			;($C2A8)Set next PPU write address.(Does nothing, already set).
	ldy $06				;Restore data index to Y.
	dec $05				;Decrement counter byte.
	bne WritePaletteStringByte	;If more bytes to write, branch to write another byte.
	stx PPUStrIndex			;Store total length, in bytes, of PPUDataString.
	iny				;Move to next data byte(should be #$00).

*	ldx PPUStrIndex			;X now contains current length of PPU data string.
	lda ($02),y			;
	bne ----			;Is PPU string done loading (#$00)? If so exit,-->
	jsr EndPPUString		;($C376)else branch to process PPU byte.

SeparateControlBits:
	sta $04				;Store current byte 
	and #$BF			;
	sta PPUDataString,x		;Remove RLE bit and save control bit in PPUDataString.
	and #$3F			;
	sta $05				;Extract counter bits and save them for use above.
	jmp NextPPUByte			;($C36E)

;----------------------------------------[ Math routines ]-------------------------------------------

TwosCompliment:
	eor #$FF			;
	clc				;Generate twos compliment of value stored in A.
	adc #$01			;
	rts				;

;The following two routines add a Binary coded decimal (BCD) number to another BCD number.
;A base number is stored in $03 and the number in A is added/subtracted from $03.  $01 and $02 
;contain the lower and upper digits of the value in A respectively.  If an overflow happens after
;the addition/subtraction, the carry bit is set before the routine returns.

Base10Add:
	jsr ExtractNibbles		;($C41D)Separate upper 4 bits and lower 4 bits.
	adc $01				;Add lower nibble to number.
	cmp #$0A			;
	bcc +				;If result is greater than 9, add 5 to create-->
	adc #$05			;valid result(skip #$0A thru #$0F).
*	clc				;
	adc $02				;Add upper nibble to number.
	sta $02				;
	lda $03				;
	and #$F0			;Keep upper 4 bits of HealthLo/HealthHi in A.
	adc $02				;
	bcc ++				;
*	adc #$5F			;If upper result caused a carry, add #$5F to create-->
	sec				;valid result. Set carry indicating carry to next digit.
	rts				;
*	cmp #$A0			;If result of upper nibble add is greater than #$90,-->
	bcs --				;Branch to add #$5F to create valid result.
	rts				;

Base10Subtract:
	jsr ExtractNibbles		;($C41D)Separate upper 4 bits and lower 4 bits.
	sbc $01				;Subtract lower nibble from number.
	sta $01				;
	bcs +				;If result is less than zero, add 10 to create-->
	adc #$0A			;valid result.
	sta $01				;
	lda $02				;
	adc #$0F			;Adjust $02 to account for borrowing.
	sta $02				;
*	lda $03				;Keep upper 4 bits of HealthLo/HealthHi in A.
	and #$F0			;
	sec				;
	sbc $02				;If result is greater than zero, branch to finish.
	bcs +				;
	adc #$A0			;Add 10 to create valid result.
	clc				;
*	ora $01				;Combine A and $01 to create final value.
	rts				;

ExtractNibbles:
	pha				;
	and #$0F			;Lower 4 bits of value to change HealthLo/HealthHi by.
	sta $01				;
	pla				;
	and #$F0			;Upper 4 bits of value to change HealthLo/HealthHi by.
	sta $02				;
	lda $03				;
	and #$0F			;Keep lower 4 bits of HealthLo/HealthHi in A.
	rts				;

;---------------------------[ NMI and PPU control routines ]--------------------------------

; Wait for the NMI to end.

WaitNMIPass:    
	jsr ClearNMIStat		;($C434)Indicate currently in NMI.
*	lda NMIStatus			;
	beq -				;Wait for NMI to end.
	rts				;

ClearNMIStat:
	lda #$00			;Clear NMI byte to indicate the game is-->
	sta NMIStatus			;currently running NMI routines.
	rts				;

ScreenOff:
	lda PPUCNT1ZP			;
	and #$E7			; BG & SPR visibility = off

WriteAndWait:
*	sta PPUCNT1ZP			;Update value to be loaded into PPU control register.

WaitNMIPass_:
	jsr ClearNMIStat		;($C434)Indicate currently in NMI.
*	lda NMIStatus			;
	beq -				;Wait for NMI to end before continuing.
	rts				;

ScreenOn:
	lda PPUCNT1ZP			;
	ora #$1E			;BG & SPR visibility = on
	bne --				;Branch always

;Update the actual PPU control registers.

WritePPUCtrl:
	lda PPUCNT0ZP			;
	sta PPUControl0			;
	lda PPUCNT1ZP			;Update PPU control registers.
	sta PPUControl1			;
	lda MirrorCntrl			;
	jsr PrepPPUMirror		;($C4D9)Setup vertical or horizontal mirroring.

ExitSub:
	rts				;Exit subroutines.

;Turn off both screen and NMI.

ScreenNmiOff:
	lda PPUCNT1ZP			;
	and #$E7			;BG & SPR visibility = off
	jsr WriteAndWait		;($C43D)Wait for end of NMI.
	lda PPUCNT0ZP			;Prepare to turn off NMI in PPU.
	and #$7F			;NMI = off
	sta PPUCNT0ZP			;
	sta PPUControl0			;Actually load PPU register with NMI off value.
	rts				;

;The following routine does not appear to be used.

	lda PPUCNT0ZP			;Enable VBlank.
	ora #$80			;
	sta PPUCNT0ZP			;Write PPU control register 0 and PPU status byte.
	sta PPUControl0			;
	lda PPUCNT1ZP			;Turn sprites and screen on.
	ora #$1E			;
	bne --				;Branch always.

VBOffAndHorzWrite: 
	lda PPUCNT0ZP			;
	and #$7B			;Horizontal write, disable VBlank. 
*	sta PPUControl0			;Save new values in the PPU control register-->
	sta PPUCNT0ZP			;and PPU status byte.
	rts				;

NmiOn:
*	lda PPUStatus			;
	and #$80			;Wait for end of VBlank.
	bne -				;
	lda PPUCNT0ZP			;
	ora #$80			;Enable VBlank interrupts.
	bne --				;Branch always.

;--------------------------------------[ Timer routines ]--------------------------------------------

;The following routines set the timer and decrement it. The timer is set after Samus dies and
;before the GAME OVER message is dispayed.  The timer is also set while the item pickup music
;is playing.

WaitTimer:
	lda Timer3			;Exit if timer hasn't hit zero yet
	bne +				;
	lda NextRoutine			;Set GameOver as next routine.
	cmp #$04			;
	beq SetMainRoutine		;Set GoPassword as main routine.
	cmp #$06			;
	beq SetMainRoutine		;
	jsr StartMusic			;($D92C)Assume power up was picked up and GameEngine-->
	lda NextRoutine			;is next routine. Start area music before exiting.

SetMainRoutine:
	sta MainRoutine			;Set next routine to run.
*	rts				;

SetTimer:
	sta Timer3			;Set Timer3. Frames to wait is value stored in A*10.
	stx NextRoutine			;Save routine to jump to after Timer3 expires.
	lda #$09			;Next routine to run is WaitTimer.
	bne SetMainRoutine		;Branch always.

;-----------------------------------[ PPU mirroring routines ]---------------------------------------

PrepVertMirror:
	nop				;
	nop				;Prepare to set PPU for vertical mirroring (again).
	lda #$47			;

SetPPUMirror:
	lsr				;
	lsr				;Move bit 3 to bit 0 position.
	lsr				;
	and #$01			;Remove all other bits.
	sta $00				;Store at address $00.
	lda MMCReg0Cntrl		;
	and #$FE			;Load MMCReg0Cntrl and remove bit 0.
	ora $00				;Replace bit 0 with stored bit at $00.
	sta MMCReg0Cntrl		;
	sta MMC1Reg0			;
	lsr				;
	sta MMC1Reg0			;
	lsr				;
	sta MMC1Reg0			;
	lsr				;Load new configuration data serially-->
	sta MMC1Reg0			;into MMC1Reg0.
	lsr				;
	sta MMC1Reg0			;
	rts				;

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
	lsr				;
	sta MMC1Reg3			;Write bit 1 of ROM bank #.
	lsr				;
	sta MMC1Reg3			;Write bit 2 of ROM bank #.
	lsr				;
	sta MMC1Reg3			;Write bit 3 of ROM bank #.
	lsr				;
	sta MMC1Reg3			;Write bit 4 of ROM bank #.
	lda $00				;Restore A with current bank number before exiting.
*	rts				;

;Calls the proper routine according to the bank number in A.

GoBankInit:
	asl				;*2 For proper table offset below.
	tay				;
	lda BankInitTable,y		;
	sta $0A				;Load appropriate subroutine address into $0A and $0B.
	lda BankInitTable+1,y		;
	sta $0B				;
	jmp ($000A)			;Jump to appropriate initialization routine.

BankInitTable:
	.word InitBank0			;($C531)Initialize bank 0.
	.word InitBank1			;($C552)Initialize bank 1.
	.word InitBank2			;($C583)Initialize bank 2.
	.word InitBank3			;($C590)Initialize bank 3.
	.word InitBank4			;($C5B6)Initialize bank 4.
	.word InitBank5			;($C5C3)Initialize bank 5.
	.word ExitSub			;($C45C)Rts
	.word ExitSub			;($C45C)Rts
	.word ExitSub			;($C45C)Rts

;Title screen memory page.

InitBank0:
	ldy #$00			;
	sty GamePaused			;Ensure game is not paused.
	iny				;Y=1.
	sty GameMode			;Game is at title routines.
	jsr ScreenNmiOff		;($C45D)Waits for NMI to end then turns it off.
	jsr CopyMap			;($A93E)Copy game map from ROM to cartridge RAM $7000-$73FF
	jsr ClearNameTables		;($C158)Erase name table data.

	ldy #$A0			;
*	lda Unknown98BF,y			;
	sta IntroStarSpriteMem,y			;Loads sprite info for stars into RAM $6E00 thru 6E9F.
	dey				;
	bne -				;

	jsr InitTitleGFX		;($C5D7)Load title GFX.
	jmp NmiOn			;($C487)Turn on VBlank interrupts.

;Brinstar memory page.

InitBank1:
	lda #$00			;
	sta GameMode			;GameMode = play.
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	lda MainRoutine			;
	cmp #$03			;Is game engine running? if so, branch.-->
	beq +				;Else do some housekeeping first.
	lda #$00			;
	sta MainRoutine			;Run InitArea routine next.
	sta InArea			;Start in Brinstar.
	sta GamePaused			;Make sure game is not paused.
	jsr ClearRAM_33_DF		;($C1D4)Clear game engine memory addresses.
	jsr ClearSamusStats		;($C578)Clear Samus' stats memory addresses.
*	ldy #$00			;
	jsr ROMSwitch			;($C4EF)Load Brinstar memory page into lower 16Kb memory.
	jsr InitBrinstarGFX		;($C604)Load Brinstar GFX.
	jmp NmiOn			;($C487)Turn on VBlank interrupts.

ClearSamusStats:
	ldy #$0F			;
	lda #$00			;Clears Samus stats(Health, full tanks, game timer, etc.).
*	sta $0100,y			;Load $100 thru $10F with #$00.
	dey				;
	bpl -				;Loop 16 times.
	rts				;

;Norfair memory page.

InitBank2:
	lda #$00			;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	jsr InitNorfairGFX		;($C622)Load Norfair GFX.
	jmp NmiOn			;($C487)Turn on VBlank interrupts.

;Tourian memory page.

InitBank3:
	lda #$00			;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	ldy #$0D			;
*	lda MetroidData,y		;Load info from table below into-->
	sta $77F0,y			;$77F0 thru $77FD.
	dey				;
	bpl -				;
	jsr InitTourianGFX		;($C645)Load Tourian GFX.
	jmp NmiOn			;($C487)Turn on VBlank interrupts.

;Table used by above subroutine and loads the initial data used to describe
;metroid's behavior in the Tourian section of the game.

MetroidData:
	.byte $F8, $08, $30, $D0, $60, $A0, $02, $04, $00, $00, $00, $00, $00, $00

;Kraid memory page.

InitBank4:
	lda #$00			;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	jsr InitKraidGFX		;($C677)Load Kraid GFX.
	jmp NmiOn			;($C487)Turn on VBlank interrupts.

;Ridley memory page.

InitBank5:
	lda #$00			;GameMode = play.
	sta GameMode			;
	jsr ScreenNmiOff		;($C45D)Disable screen and Vblank.
	jsr InitRidleyGFX		;($C69F)Loag Ridley GFX.
	jmp NmiOn			;($C487)Turn on VBlank interrupts.

InitEndGFX:
	lda #$01			;
	sta GameMode			;Game is at title/end game.
	jmp InitGFX6			;($C6C2)Load end game GFX.

InitTitleGFX:
	ldy #$15			;Entry 21 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.

LoadSamusGFX:
	ldy #$00			;Entry 0 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	lda JustInBailey		;
	beq +				;Branch if wearing suit
	ldy #$1B			;Entry 27 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Switch to girl gfx
*	ldy #$14			;Entry 20 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$17			;Entry 23 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$18			;Entry 24 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitBrinstarGFX:
	ldy #$03			;Entry 3 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$04			;Entry 4 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$05			;Entry 5 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$06			;Entry 6 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitNorfairGFX:
	ldy #$04			;Entry 4 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$05			;Entry 5 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$07			;Entry 7 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$08			;Entry 8 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$09			;Entry 9 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitTourianGFX:
	ldy #$05			;Entry 5 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0A			;Entry 10 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0B			;Entry 11 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0C			;Entry 12 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0D			;Entry 13 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0E			;Entry 14 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$1A			;Entry 26 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$1C			;Entry 28 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitKraidGFX:
	ldy #$04			;Entry 4 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$05			;Entry 5 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0A			;Entry 10 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0F			;Entry 15 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$10			;Entry 16 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$11			;Entry 17 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitRidleyGFX:
	ldy #$04			;Entry 4 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$05			;Entry 5 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$0A			;Entry 10 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$12			;Entry 18 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$13			;Entry 19 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitGFX6:
	ldy #$01			;Entry 1 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$02			;Entry 2 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$19			;Entry 25 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

InitGFX7:
	ldy #$17			;Entry 23 in GFXInfo table.
	jsr LoadGFX			;($C7AB)Load pattern table GFX.
	ldy #$16			;Entry 22 in GFXInfo table.
	jmp LoadGFX			;($C7AB)Load pattern table GFX.

;The table below contains info for each tile data block in the ROM.
;Each entry is 7 bytes long. The format is as follows:
;byte 0: ROM bank where GFX data is located.
;byte 1-2: 16-bit ROM start address (src).
;byte 3-4: 16-bit PPU start address (dest).
;byte 5-6: data length (16-bit).

GFXInfo:
	.byte $06			;[SPR]Samus, items.		Entry 0.
	.word $8000, $0000, $09A0
	.byte $04			;[SPR]Samus in ending.		Entry 1.
	.word $8D60, $0000, $0520
	.byte $01			;[BGR]Partial font, "The End".	Entry 2.
	.word $8D60, $1000, $0400
	.byte $06			;[BGR]Brinstar rooms.		Entry 3.
	.word $9DA0, $1000, $0150
	.byte $05			;[BGR]Misc. objects.		Entry 4.
	.word $8D60, $1200, $0450
	.byte $06			;[BGR]More Brinstar rooms.	Entry 5.
	.word $9EF0, $1800, $0800
	.byte $01			;[SPR]Brinstar enemies.		Entry 6.
	.word $9160, $0C00, $0400
	.byte $06			;[BGR]Norfair rooms.		Entry 7.
	.word $A6F0, $1000, $0260
	.byte $06			;[BGR]More Norfair rooms.	Entry 8.
	.word $A950, $1700, $0070
	.byte $02			;[SPR]Norfair enemies.		Entry 9.
	.word $8D60, $0C00, $0400
	.byte $06			;[BGR]Tourian rooms.		Entry 10.
	.word $A9C0, $1000, $02E0
	.byte $06			;[BGR]More Tourian rooms.	Entry 11.
	.word $ACA0, $1200, $0600
	.byte $06			;[BGR]Mother Brain room.	Entry 12.
	.word $B2A0, $1900, $0090
	.byte $05			;[BGR]Misc. object.		Entry 13.
	.word $91B0, $1D00, $0300
	.byte $02			;[SPR]Tourian enemies.		Entry 14.
	.word $9160, $0C00, $0400
	.byte $06			;[BGR]More Tourian rooms.	Entry 15.
	.word $B330, $1700, $00C0
	.byte $04			;[BGR]Misc. object and fonts.	Entry 16.
	.word $9360, $1E00, $0200
	.byte $03			;[SPR]Miniboss I enemies.	Entry 17.
	.word $8D60, $0C00, $0400
	.byte $06			;[BGR]More Tourian Rooms.	Entry 18.
	.word $B3F0, $1700, $00C0
	.byte $03			;[SPR]Miniboss II enemies.	Entry 19.
	.word $9160, $0C00, $0400
	.byte $06			;[SPR]Inrto/End sprites.	Entry 20.
	.word $89A0, $0C00, $0100
	.byte $06			;[BGR]Title.			Entry 21.
	.word $8BE0, $1400, $0500
	.byte $06			;[BGR]Solid tiles.		Entry 22.
	.word $9980, $1FC0, $0040
	.byte $06			;[BGR]Complete font.		Entry 23.
	.word $B4C0, $1000, $0400
	.byte $06			;[BGR]Complete font.		Entry 24.
	.word $B4C0, $0A00, $00A0
	.byte $06			;[BGR]Solid tiles.		Entry 25.
	.word $9980, $0FC0, $0040
	.byte $06			;[BGR]Complete font.		Entry 26.
	.word $B4C0, $1D00, $02A0
	.byte $06			;[SPR]Suitless Samus.		Entry 27.
	.word $90E0, $0000, $07B0
	.byte $06			;[BGR]Exclaimation point.	Entry 28.
	.word $9890, $1F40, $0010

;--------------------------------[ Pattern table loading routines ]---------------------------------

;Y contains the GFX header to fetch from the table above, GFXInfo.

LoadGFX:
	lda #$FF			;
*	clc				;Every time y decrements, the entry into the table-->
	adc #$07			;is increased by 7.  When y is less than 0, A points-->
	dey				;to the last byte of the entry in the table.
	bpl -				;
	tay				;Transfer offset into table to Y.

	ldx #$06			;
*	lda GFXInfo,y			;
	sta $00,x			;Copy entries from GFXInfo to $00-$06.
	dey				;
	dex				;
	bpl -				;

	ldy $00				;ROM bank containing the GFX data.
	jsr ROMSwitch			;($C4EF)Switch to that bank.
	lda PPUCNT0ZP			;
	and #$FB			;
	sta PPUCNT0ZP			;Set the PPU to increment by 1.
	sta PPUControl0			;
	jsr CopyGFXBlock		;($C7D5)Copy graphics into pattern tables.
	ldy CurrentBank			;
	jmp ROMSwitch			;($C4FE)Switch back to the "old" bank.

;Writes tile data from ROM to VRAM, according to the gfx header data
;contained in $00-$06.

CopyGFXBlock:
	lda $05				;
	bne GFXCopyLoop			;If $05 is #$00, decrement $06 before beginning.
	dec $06				;

GFXCopyLoop:
	lda $04				;
	sta PPUAddress			;Set PPU to proper address for GFX block write.
	lda $03				;
	sta PPUAddress			;
	ldy #$00			;Set offset for GFX data to 0.
*	lda ($01),y			;
	sta PPUIOReg			;Copy GFX data byte from ROM to Pattern table.
	dec $05				;Decrement low byte of data length.
	bne +				;Branch if high byte does not need decrementing.
	lda $06				;
	beq ++				;If copying complete, branch to exit.
	dec $06				;Decrement when low byte has reached 0.
*	iny				;Increment to next byte to copy.
	bne --				;
	inc $02				;After 256 bytes loaded, increment upper bits of-->
	inc $04				;Source and destination addresses.
	jmp GFXCopyLoop			;(&C7DB)Repeat copy routine.
*	rts				;

;-------------------------------------------[ AreaInit ]---------------------------------------------

AreaInit:
	lda #$00			;
	sta ScrollX     		;Clear ScrollX.
	sta ScrollY     		;Clear ScrollY.
	lda PPUCNT0ZP			;	
	and #$FC			;Sets nametable address = $2000.
	sta PPUCNT0ZP			;
	inc MainRoutine			;Increment MainRoutine to MoreInit.
	lda Joy1Status			;
	and #$C0			;Stores status of both the A and B buttons.
	sta ABStatus	 		;Appears to never be accessed.
	jsr EraseAllSprites		;($C1A3)Clear all sprite info.
	lda #$10			;Prepare to load Brinstar memory page.
	jsr IsEngineRunning		;($CA18)Check to see if ok to switch lower memory page.

;------------------------------------------[ MoreInit ]---------------------------------------------

MoreInit:
	ldy #$01			;
	sty PalDataPending		;Palette data pending = yes.
	ldx #$FF			;
	stx SpareMem75			;$75 Not referenced ever again in the game.
	inx				;X=0.
	stx AtEnding			;Not playing ending scenes.
	stx DoorStatus			;Samus not in door.
	stx SamusDoorData		;Samus is not inside a door.
	stx UpdatingProjectile		;No projectiles need to be updated.
	txa				;A=0.

*	cpx #$65			;Check to see if more RAM to clear in $7A thru $DE.
	bcs +				;
	sta $7A,x			;Clear RAM $7A thru $DE.
*	cpx #$FF			;Check to see if more RAM to clear in $300 thru $3FE.
	bcs +				;
	sta ObjAction,x			;Clear RAM $300 thru $3FE.
*	inx				;
	bne ---				;Loop until all required RAM is cleared.

	jsr ScreenOff			;($C439)Turn off Background and visibility.
	jsr ClearNameTables		;($C158)Clear screen data.
	jsr EraseAllSprites		;($C1A3)Erase all sprites from sprite RAM.
	jsr DestroyEnemies		;($C8BB)

	stx DoorOnNameTable3		;Clear data about doors on the name tables.
	stx DoorOnNameTable0		;
	inx				;X=1.
	stx SpareMem30			;Not accessed by game.
	inx				;X=2.
	stx ScrollDir			;Set initial scroll direction as left.

	lda Unknown95D7			;Get Samus start x pos on map.
	sta MapPosX			;
	lda Unknown95D8			;Get Samus start y pos on map.
	sta MapPosY			;

	lda Unknown95DA       ; Get ??? Something to do with palette switch
	sta PalToggle
	lda #$FF
	sta RoomNumber			;Room number = $FF(undefined room).
	jsr CopyPtrs    ; copy pointers from ROM to RAM 
	jsr GetRoomNum			;($E720)Put room number at current map pos in $5A.
*       jsr SetupRoom			;($EA2B)
	ldy RoomNumber  ; load room number
	iny
	bne -

	ldy CartRAMPtr+1
	sty $01
	ldy CartRAMPtr
	sty $00
	lda PPUCNT0ZP
	and #$FB	; PPU increment = 1
	sta PPUCNT0ZP
	sta PPUControl0
	ldy PPUStatus   ; reset PPU addr flip/flop

; Copy room RAM #0 ($6000) to PPU Name Table #0 ($2000)

	ldy #$20
	sty PPUAddress
	ldy #$00
	sty PPUAddress
	ldx #$04	; prepare to write 4 pages
*       lda ($00),y
	sta PPUIOReg
	iny
	bne -
	inc $01
	dex
	bne -

	stx $91
	inx	     ; X = 1
	stx PalDataPending
	stx SpareMem30			;Not accessed by game.
	inc MainRoutine			;SamusInit is next routine to run.
	jmp ScreenOn

; CopyPtrs
; ========
; Copy 7 16-bit pointers from $959A thru $95A7 to $3B thru $48.

CopyPtrs:
	ldx #$0D
*       lda AreaPointers+2,x
	sta RoomPtrTable,x
	dex
	bpl -
	rts

; DestroyEnemies
; ==============

DestroyEnemies:
	lda #$00
	tax
*       cpx #$48
	bcs +
	sta $97,x
*       sta EnStatus,x
	pha
	pla
	inx
	bne --
	stx MetroidOnSamus		;Samus had no Metroid stuck to her.
	jmp Unknown95AB

; SamusInit
; =========
; Code that sets up Samus, when the game is first started.

SamusInit:
	lda #$08			;
  sta MainRoutine			;SamusIntro will be executed next frame.
	lda #$2C			;440 frames to fade in Samus(7.3 seconds).
	sta Timer3			;
	jsr IntroMusic			;($CBFD)Start the intro music.
	ldy #sa_FadeIn0			;
	sty ObjAction			;Set Samus status as fading onto screen.
	ldx #$00
	stx SamusBlink
	dex				;X = $FF
	stx $0728
	stx $0730
	stx $0732
	stx $0738
	stx EndTimerLo			;Set end timer bytes to #$FF as-->
	stx EndTimerHi			;escape timer not currently active.
	stx $8B
	stx $8E
	ldy #$27
	lda InArea
	and #$0F
	beq +				;Branch if Samus starting in Brinstar.
	lsr ScrollDir			;If not in Brinstar, change scroll direction from left-->
	ldy #$2F			;to down. and set PPU for horizontal mirroring.
*       sty MirrorCntrl			;
	sty MaxMissilePickup
	sty MaxEnergyPickup
	lda Unknown95D9			;Samus' initial vertical position
	sta ObjectY			;
	lda #$80			;Samus' initial horizontal position
	sta ObjectX			;
	lda PPUCNT0ZP			;
	and #$01			;Set Samus' name table position to current name table-->
	sta ObjectHi			;active in PPU.
	
	lda TankCount
	jsr Amul16
	ora #$09
	sta HealthHi
	lda #$99
	sta HealthLo
	
	;lda #$00			;
	;sta HealthLo			;Starting health is-->
	;lda #$03			;set to 30 units.
	;sta HealthHi			;
*       rts				;

;------------------------------------[ Main game engine ]--------------------------------------------

GameEngine:
	jsr ScrollDoor			;($E1F1)Scroll doors, if needed. 2 routine calls scrolls-->
	jsr ScrollDoor			;($E1F1)twice as fast as 1 routine call.

	lda NARPASSWORD			;
	beq +				;
	lda #$03			;The following code is only accessed if -->
	sta HealthHi			;NARPASSWORD has been entered at the -->
	lda #$FF			;password screen. Gives you new health,-->
	sta SamusGear			;missiles and every power-up every frame.
	lda #$05			;
	sta MissileCount		;

*	jsr UpdateWorld			;($CB29)Update Samus, enemies and room tiles.
	lda MiniBossKillDelay		;
	ora PowerUpDelay		;Check if mini boss was just killed or powerup aquired.-->
	beq +				;If not, branch.

	lda #$00			;
	sta MiniBossKillDelay		;Reset delay indicators.
	sta PowerUpDelay		;
	lda #$18			;Set timer for 240 frames(4 seconds).
	ldx #$03			;GameEngine routine to run after delay expires
	jsr SetTimer			;($C4AA)Set delay timer and game engine routine.

*	lda ObjAction			;Check is Samus is dead.
	cmp #sa_Dead2   		;Is Samus dead?-->
	bne ---	   			;exit if not.
	lda AnimDelay			;Is Samus still exploding?-->
	bne ---				;Exit if still exploding.
	jsr SilenceMusic		;Turn off music.
	lda MotherBrainStatus		;
	cmp #$0A			;Is mother brain already dead? If so, branch.
	beq +				;
	lda #$04			;Set timer for 40 frames (.667 seconds).
	ldx #$04			;GameOver routine to run after delay expires.
	jmp SetTimer			;($C4AA)Set delay timer and run game over routine.

*	inc MainRoutine			;Next routine to run is GameOver.
	rts				;

;----------------------------------------[ Update age ]----------------------------------------------

;This is the routine which keeps track of Samus' age. It is called in the
;NMI. Basically, this routine just increments a 24-bit variable every
;256th frame. (Except it's not really 24-bit, because the lowest age byte
;overflows at $D0.)

UpdateAge:
	lda GameMode			;
	bne ++				;Exit if at title/password screen.
	lda MainRoutine			;
	cmp #$03			;Is game engine running?
	bne ++				;If not, don't update age.
	ldx FrameCount			;Only update age when FrameCount is zero-->
	bne ++				;(which is approx. every 4.266666666667 seconds).
	inc SamusAge,x			;Minor Age = Minor Age + 1.
	lda SamusAge			;
	cmp #$D0			;Has Minor Age reached $D0?-->
	bcc ++				;If not, we're done.-->
	lda #$00			;Else reset minor age.
	sta SamusAge			;
*	cpx #$03			;
	bcs +				;Loop to update middle age and possibly major age.
	inx				;
	inc SamusAge,x			;
	beq -	   			;Branch if middle age overflowed, need to increment--> 
*	rts				;major age too. Else exit.

;-------------------------------------------[ Game over ]--------------------------------------------

EngineGameOver:
	lda #$1C			;GameOver is the next routine to run.
	sta TitleRoutine		;
	lda #$01			;
	sta SwitchPending		;Prepare to switch to title memory page.
	jmp ScreenOff			;($C439)Turn screen off.

;------------------------------------------[ Pause mode ]--------------------------------------------

PauseMode:
	lda Joy2Status			;Load buttons currently being pressed on joypad 2.
	and #$88			;
	eor #$88			;both A & UP pressed?-->
	bne Exit14			;Exit if not.
	ldy EndTimerHi			;
	iny				;Is escape timer active?-->
	bne Exit14			;Sorry, can't quit if this is during escape scence.
	sta GamePaused			;Clear pause game indicator.
	inc MainRoutine			;Display password is the next routine to run.

Exit14:
	rts				;Exit for routines above and below.

;------------------------------------------[ GoPassword ]--------------------------------------------

GoPassword:
	lda #$19			;DisplayPassword is next routine to run.
	sta TitleRoutine		;
	lda #$01			;
	sta SwitchPending		;Prepare to switch to intro memory page.
	lda NoiseSFXFlag		;
	ora #$01			;Silence music.
	sta NoiseSFXFlag		;
	jmp ScreenOff			;($C439)Turn off screen.

;-----------------------------------------[ Samus intro ]--------------------------------------------

SamusIntro:
	jsr EraseAllSprites		;($C1A3)Clear all sprites off screen.
	ldy ObjAction			;Load Samus' fade in status.
	lda Timer3			;
	bne +				;Branch if Intro still playing.
	
;Fade in complete.
	sta ItemRoomMusicStatus		;Make sure item room music is not playing.
	lda #sa_Begin			;Samus facing forward and can't be hurt.
	sta ObjAction			;
	jsr StartMusic			;($D92C)Start main music.
	jsr SelectSamusPal		;($CB73)Select proper Samus palette.
	lda #$03			;
	sta MainRoutine			;Game engine will be called next frame.

;Still fading in.
*	cmp #$1F			;When 310 frames left of intro, display Samus.
	bcs Exit14			;Branch if not time to start drawing Samus.
	cmp SamusFadeInTimeTbl-20,y	;sa_FadeIn0 is beginning of table.
	bne +				;Every time Timer3 equals one of the entries in the table-->
	inc ObjAction			;below, change the palette used to color Samus.
	sty PalDataPending		;
*	lda FrameCount			;Is game currently on an odd frame?-->
	lsr				;If not, branch to exit.
	bcc Exit14			;Only display Samus on odd frames [the blink effect].
	lda #an_SamusFront		;Samus front animation is animation to display.-->
	jsr SetSamusAnim		;($CF6B)while fading in.
	lda #$00			;
	sta SpritePagePos		;Samus sprites start at Sprite00RAM.
	sta PageIndex			;Samus RAM is first set of RAM.
	jmp AnimDrawObject		;($DE47)Draw Samus on screen.

;The following table marks the time remaining in Timer3 when a palette change should occur during
;the Samus fade-in sequence. This creates the fade-in effect.

SamusFadeInTimeTbl:
	.byte $1E,$14,$0B,$04,$FF

;---------------------------------[ Check if game engine running ]-----------------------------------

IsEngineRunning:
	ldy MainRoutine			;If Samus is fading in or the wait timer is-->
	cpy #$07			;active, return from routine.
	beq +				;
	cpy #$03			;Is game engine running?
	beq ++				;If yes, branch to SwitchBank.
*	rts				;Exit if can't switch bank.

;-----------------------------------------[ Switch bank ]--------------------------------------------

;Switch to appropriate area bank

SwitchBank:
*	sta InArea			;Save current area Samus is in.
	and #$0F			;
	tay				;Use 4 LSB to load switch pending offset from BankTable table.
	lda BankTable,y			;Base is $CA30.
	sta SwitchPending		;Store switch data.
	jmp CheckSwitch			;($C4DE)Switch lower 16KB to appropriate memory page.

;Table used by above subroutine.
;Each value is the area bank number plus one.

BankTable:
	.byte $02			;Brinstar.
	.byte $03			;Norfair.
	.byte $05			;Kraid hideout.
	.byte $04			;Tourian.
	.byte $06			;Ridley hideout.

;----------------------------------[ Saved game routines (not used) ]--------------------------------

;----------------------------------------[ Choose ending ]-------------------------------------------

;Determine what type of ending is to be shown, based on Samus' age
ChooseEnding:
	ldy #$01			;
*	lda SamusAge+2			;If SamusAge+2 anything but #$00, load worst-->
	bne +				;ending(more than 37 hours of gameplay).
	lda SamusAge+1			;
	cmp AgeTable-1,y		;Loop four times to determine-->
	bcs +				;ending type from table below.
	iny				;
	cpy #$05			;
	bne -				;
*	sty EndingType			;Store the ending # (1..5), 5=best ending
	lda #$00			;
	cpy #$04			;Was the best or 2nd best ending achieved?
	bcc +				;Branch if not (suit stays on)
	lda #$01			;
*	sta JustInBailey		;Suit OFF, baby!
	rts				;

;Table used by above subroutine to determine ending type.
AgeTable:
	.byte $7A			;Max. 37 hours
	.byte $16			;Max. 6.7 hours
	.byte $0A			;Max. 3.0 hours
	.byte $04			;Best ending. Max. 1.2 hours

;--------------------------------[ Clear screen data (not used) ]------------------------------------

ClearScreenData:
	jsr ScreenOff			;($C439)Turn off screen.
	lda #$FF			;
	sta $00				;Prepare to fill nametable with #$FF.
	jsr ClearNameTable		;($C175)Clear selected nametable.
	jmp EraseAllSprites		;($C1A3)Clear sprite data.

;----------------------------------------------------------------------------------------------------

; ===== THE REAL GUTS OF THE GAME ENGINE! =====

UpdateWorld:
	ldx #$00			;Set start of sprite RAM to $0200.
	stx SpritePagePos		;

	jsr UpdateEnemies		;($F345)Display of enemies.
	jsr UpdateProjectiles		;($D4BF)Display of bullets/missiles/bombs.
	jsr UpdateSamus			;($CC0D)Display/movement of Samus.
	jsr AreaRoutine			;($95C3)Area specific routine.
	jsr UpdateElevator		;($D7B3)Display of elevators.
	jsr UpdateStatues		;($D9D4)Display of Ridley & Kraid statues.
	jsr EnemyDestruction    ; destruction of enemies
	jsr UpdateMellowEnemies ; update of Mellow/Memu enemies
	jsr UnknownF93B
	jsr DestroyGeenSpinner  ; destruction of green spinners
	jsr SamusEnterDoor		;($8B13)Check if Samus entered a door.
	jsr DisplayDoors      ; display of doors
	jsr UpdateTiles ; tile de/regeneration
	jsr CrashDetection      ; Samus <--> enemies crash detection
	jsr DisplayBar			;($E0C1)Display of status bar.
	jsr UnknownFAF2
	jsr CheckMissileToggle
	jsr UpdateItems ; display of special items
	jsr UpdateEndTimer

;Clear remaining sprite RAM
	ldx SpritePagePos
	lda #$F4
*       sta Sprite00RAM,x
	jsr Xplus4       ; X = X + 4
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
	asl				;CF contains Varia status (1 = Samus has it)
	lda MissileToggle		;A = 1 if Samus is firing missiles, else 0
	rol				;Bit 0 of A = 1 if Samus is wearing Varia
	adc #$02
	ldy JustInBailey		;In suit?
	beq +				;Branch if yes
	clc
	adc #$17			;Add #$17 to the pal # to reach "no suit"-palettes
*       sta PalDataPending		;Palette will be written next NMI
	pla
	tay
	rts

;----------------------------------[ Initiate SFX and music routines ]-------------------------------

;Initiate sound effects.

SilenceMusic:				;The sound flags are stored in memory-->
	lda #$01			;starting at $0680. The following is a-->
	bne SFX_SetX0			;list of sound effects played when the-->
					;flags are set:
PauseMusic:				;
	lda #$02   	     		;$0680: These SFX use noise channel.
	bne SFX_SetX0			;Bit 7 - No sound.
					;Bit 6 - ScrewAttack.
SFX_SamusWalk:				;Bit 5 - MissileLaunch.
	lda #$08			;Bit 4 - BombExplode.
	bne SFX_SetX0			;Bit 3 - SamusWalk.
					;Bit 2 - SpitFlame.
SFX_BombExplode:			;Bit 1 - No sound.
	lda #$10			;Bit 0 - No sound.
	bne SFX_SetX0			;
					;$0681: These SFX use sq1 channel.
SFX_MissileLaunch:			;Bit 7 - MissilePickup.
	lda #$20			;Bit 6 - EnergyPickup.
					;Bit 5 - Metal.
SFX_SetX0:				;Bit 4 - BulletFire.
	ldx #$00			;Bit 3 - OutOfHole.
	beq SFX_SetSoundFlag		;Bit 2 - EnemyHit.
					;Bit 1 - SamusJump.
SFX_OutOfHole:				;Bit 0 - WaveFire.
	lda #$08			;
	bne SFX_SetX1			;$0682: Not used.
					;
SFX_BombLaunch:				;$0683: These SFX use tri channel.
	lda #$01			;Bit 7 - SamusDie.
	bne SFX_SetX3			;Bit 6 - DoorOpenClose.
					;Bit 5 - MetroidHit.
SFX_SamusJump:				;Bit 4 - StatueRaise.
	lda #$02			;Bit 3 - Beep.
	bne SFX_SetX1			;Bit 2 - BigEnemyHit.
					;Bit 1 - SamusBall.
SFX_EnemyHit:				;Bit 0 - BombLaunch.
	lda #$04			;
	bne SFX_SetX1			;$0684: These SFX use multi channels.
					;Bit 7 - FadeInMusic		(music).
SFX_BulletFire:				;Bit 6 - PowerUpMusic		(music).
	lda #$10			;Bit 5 - EndMusic  (Page 0 only)(music).
	bne SFX_SetX1			;Bit 4 - IntroMusic(Page 0 only)(music).
					;Bit 3 - not used		(SFX).
SFX_Metal:				;Bit 2 - SamusHit		(SFX).
	lda #$20			;Bit 1 - BossHit		(SFX).
	bne SFX_SetX1			;Bit 0 - IncorrectPassword	(SFX).
					;
SFX_EnergyPickup:			;$0685: Music flags. The music flags start different-->
	lda #$40			;music depending on what memory page is loaded. The-->
	bne SFX_SetX1			;following lists what bits start what music for each-->
					;memory page.
SFX_MissilePickup:			;
	lda #$80			;Page 0: Intro/ending.
					;Bit 7 - Not used.
SFX_SetX1:				;Bit 6 - TourianMusic.
	ldx #$01			;Bit 5 - ItemRoomMusic.
	bne SFX_SetSoundFlag		;Bit 4 - Not used.
					;Bit 3 - Not used.
SFX_WaveFire:				;Bit 2 - Not used.
	lda #$01			;Bit 1 - Not used.
	bne SFX_SetX1			;Bit 0 - Not used.
					;
SFX_ScrewAttack:			;Page 1: Brinstar.
	lda #$40			;Bit 7 - Not used.
	bne SFX_SetX0			;Bit 6 - TourianMusic.
					;Bit 5 - ItemRoomMusic.
SFX_BigEnemyHit:			;Bit 4 - Not used.
	lda #$04			;Bit 3 - Not used.
	bne SFX_SetX3			;Bit 2 - Not used.
					;Bit 1 - Not used.
SFX_MetroidHit:				;Bit 0 - BrinstarMusic.
	lda #$20			;
	bne SFX_SetX3			;Page 2: Norfair.
					;Bit 7 - Not used.
SFX_BossHit:				;Bit 6 - TourianMusic.
	lda #$02			;Bit 5 - ItemRoomMusic.
	bne SFX_SetX4			;Bit 4 - Not used.
					;Bit 3 - NorfairMusic.
SFX_Door:				;Bit 2 - Not used.
	lda #$40			;Bit 1 - Not used.
	bne SFX_SetX3			;Bit 0 - Not used.
					;
SFX_SamusHit:				;Page 3: Tourian.
	lda #$04			;Bit 7 - Not used.
	bne SFX_SetX4			;Bit 6 - TourianMusic
					;Bit 5 - ItemRoomMusic.
SFX_SamusDie:				;Bit 4 - Not used.
	lda #$80			;Bit 3 - Not used.
	bne SFX_SetX3			;Bit 2 - EscapeMusic.
					;Bit 1 - MotherBrainMusic
SFX_SetX2:				;Bit 0 - Not used.
	ldx #$02			;
					;Page 4: Kraid.
SFX_SetSoundFlag:			;Bit 7 - RidleyAreaMusic.
	ora $0680,x			;Bit 6 - TourianMusic.
	sta $0680,x			;Bit 5 - ItemRoomMusic.
	rts				;Bit 4 - KraidAreaMusic.
					;Bit 3 - Not used.
SFX_SamusBall:				;Bit 2 - Not used.
	lda #$02			;Bit 1 - Not used.
	bne SFX_SetX3			;Bit 0 - Not used.
					;
SFX_Beep:				;Page 5: Ridley.
	lda #$08			;Bit 7 - RidleyAreaMusic.
					;Bit 6 - TourianMusic.
SFX_SetX3:				;Bit 5 - ItemRoomMusic.
	ldx #$03			;Bit 4 - KraidAreaMusic.
	bne SFX_SetSoundFlag		;Bit 3 - Not used.
					;Bit 2 - Not used.
;Initiate music				;Bit 1 - Not used.
					;Bit 0 - Not used.
PowerUpMusic:				;
	lda #$40			;
	bne SFX_SetX4			;
					;
IntroMusic:				;
	lda #$80			;
					;
SFX_SetX4:				;
	ldx #$04			;
	bne SFX_SetSoundFlag		;
					;
MotherBrainMusic:			;
	lda #$02			;
	bne SFX_SetX5			;
					;
TourianMusic:				;
	lda #$40			;
					;
SFX_SetX5:				;
	ldx #$05			;
	bne SFX_SetSoundFlag		;

;--------------------------------------[ Update Samus ]----------------------------------------------

UpdateSamus:
	ldx #$00			;Samus data is located at index #$00.
	stx PageIndex			;
	inx				;x=1.
	stx IsSamus			;Indicate Samus is the object being updated.
	jsr GoSamusHandler		;($CC1A)Find proper Samus handler routine.
	dec IsSamus			;Update of Samus complete.
	rts				;

GoSamusHandler:
	lda ObjAction			;
	bmi SamusStand			;Branch if Samus is standing.
	jsr ChooseRoutine		;($C27C)Goto proper Samus handler routine.

;Pointer table for Samus' action handlers.

	.word SamusStand		;($CC36)Standing.
	.word SamusRun			;($CCC2)Running.
	.word SamusJump			;($D002)Jumping.
	.word SamusRoll			;($D0E1)Rolling.
	.word SamusPntUp		;($D198)Pointing up.
	.word SamusDoor			;($D3A8)Inside door while screen scrolling.
	.word SamusJump			;($D002)Jumping while pointing up.
	.word SamusDead			;($D41A)Dead.
	.word SamusDead2		;($D41F)More dead.
	.word SamusElevator		;($D423)Samus on elevator.

;---------------------------------------[ Samus standing ]-------------------------------------------

SamusStand:
	lda Joy1Status			;Status of joypad 1.
	and #$CF			;Remove SELECT & START status bits.
	beq +				;Branch if no buttons pressed.
	jsr ClearHorzMvmtAnimData	;($CF5D)Set no horiontal movement and single frame animation.
	lda Joy1Status			;
*	and #$07			;Keep status of DOWN/LEFT/RIGHT.
	bne +				;Branch if any are pressed.
	lda Joy1Change			;
	and #$08     			;Check if UP was pressed last frame.-->
	beq +++				;If not, branch.
*	jsr BitScan			;($E1E1)Find which directional button is pressed.
	cmp #$02			;Is down pressed?-->
	bcs +				;If so, branch.
	sta SamusDir			;1=left, 0=right.
*	tax				;
	lda ActionTable,x		;Load proper Samus status from table below.
	sta ObjAction			;Save Samus status.
*	lda Joy1Change			;
	ora Joy1Retrig			;Check if fire was just pressed or needs to retrigger.
	asl				;
	bpl +				;Branch if FIRE not pressed.
	jsr FireWeapon			;($D1EE)Shoot left/right.
*	bit Joy1Change			;Check if jump was just pressed.
	bpl +				;Branch if JUMP not pressed.
	lda #sa_Jump			;
	sta ObjAction			;Set Samus status as jumping.
*	lda #$04			;Prepare to set animation delay to 4 frames.
	jsr SetSamusData		;($CD6D)Set Samus control data and animation.
	lda ObjAction			;
	cmp #sa_Door			;Is Samus inside a door, dead or pointing up and jumping?-->
	bcs +				;If so, branch to exit.
	jsr ChooseRoutine		;Select routine below.

;Pointer table to code.

	.word ExitSub			;($C45C)Rts.
	.word SetSamusRun		;($CC98)Samus is running.
	.word SetSamusJump		;($CFC3)Samus is jumping.
	.word SetSamusRoll		;($D0B5)Samus is in a ball.
	.word SetSamusPntUp		;($CF77)Samus is pointing up.

;Table used by above subroutine.

ActionTable:
	.byte sa_Run			;Run right.
	.byte sa_Run			;Run left.
	.byte sa_Roll
	.byte sa_PntUp

;----------------------------------------------------------------------------------------------------

SetSamusExplode:
  lda #$50
	sta SamusJumpDsplcmnt
	lda #an_Explode
	jsr SetSamusAnim
	sta ObjectCounter
*       rts

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
	jsr SetSamusNextAnim
*       lda RunAnimationTbl,x
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
  .byte $30			;Accelerate right.
	.byte $D0			;Accelerate left.

; SamusRun
; ========

SamusRun:
	ldx SamusDir
	lda SamusGravity
	beq +++++++
	ldy SamusJumpDsplcmnt
	bit ObjVertSpeed
	bmi +
	cpy #$18
	bcs ++++
	lda #an_SamusJump
	sta AnimResetIndex
	bcc ++++	  ; branch always
*       cpy #$18
	bcc +++
	lda AnimResetIndex
	cmp #an_SamusFireJump
	beq +
	lda #an_SamusSalto
	sta AnimResetIndex
*       cpy #$20
	bcc ++
	lda Joy1Status
	and #$08
	beq +
	lda #an_SamusJumpPntUp
	sta AnimResetIndex
*       bit Joy1Status
	bmi +
	jsr StopVertMovement		;($D147)
*	lda #an_SamusRun
	cmp AnimResetIndex
	bne +
	lda #an_SamusJump
	sta AnimResetIndex
*       lda SamusInLava
	beq +
	lda Joy1Change
	bmi JumpPressed       ; branch if JUMP pressed
*       jsr UnknownCF88
	jsr UnknownD09C
	jsr UnknownCF2E
	lda #$02
	bne SetSamusData       ; branch always
*	lda SamusOnElevator
	bne +
	jsr SetSamusRunAccel
*       jsr UnknownCDBF
	dec WalkSoundDelay  ; time to play walk sound?
	bne +	       ; branch if not
	lda #$09
	sta WalkSoundDelay  ; # of frames till next walk sound trigger
	jsr SFX_SamusWalk
*       jsr UnknownCF2E
	lda Joy1Change
	bpl +	   ; branch if JUMP not pressed
JumpPressed:
  jsr SetSamusJump
	lda #$12
	sta SamusHorzSpeedMax
	jmp UnknownCD6B

*       ora Joy1Retrig
	asl
	bpl +	   ; branch if FIRE not pressed
	jsr UnknownCDD7
*       lda Joy1Status
	and #$03
	bne +
	jsr StopHorzMovement
	jmp UnknownCD6B

*       jsr BitScan			;($E1E1)
	cmp SamusDir
	beq UnknownCD6B
	sta SamusDir
	jsr SetSamusRun
UnknownCD6B:
  lda #$03

;---------------------------------------[ Set Samus data ]-------------------------------------------

;The following function sets various animation and control data bytes for Samus.

SetSamusData:
  jsr UpdateObjAnim		;($DC8F)Update animation if needed.
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active to change palette.
	bcs +				;If screw attack not active, branch to skip palette change.
	lda FrameCount			;
	lsr				;
	and #$03			;Every other frame, change Samus palette while screw-->
	ora #$A0			;Attack is active.
	sta ObjectCntrl			;
*	jsr CheckHealthStatus		;($CDFA)Check if Samus hit, blinking or Health low.
	jsr LavaAndMoveCheck		;($E269)Check if Samus is in lava or moving.
	lda MetroidOnSamus		;Is a Metroid stuck to Samus?-->
	beq +				;If not, branch.
	lda #$A1			;Metroid on Samus. Turn Samus blue.
	sta ObjectCntrl			;
*	jsr SetmirrorCntrlBit		;($CD92)Mirror Samus, if necessary.
	jmp DrawFrame			;($DE4A)Display Samus.

;---------------------------------[ Set mirror control bit ]-----------------------------------------

SetmirrorCntrlBit:
  lda SamusDir			;Facing left=#$01, facing right=#$00.
	jsr Amul16			;($C2C5)*16. Move bit 0 to bit 4 position.
	ora ObjectCntrl			;
	sta ObjectCntrl			;Use SamusDir bit to set mirror bit.
	rts				;

;------------------------------[ Check if screw attack is active ]-----------------------------------

IsScrewAttackActive:
  sec				;Assume screw attack is not active.
	ldy ObjAction			;
	dey				;Is Samus running?-->
	bne ++				;If not, branch to exit.
	lda SamusGear			;
	and #gr_SCREWATTACK		;Does Samus have screw attack?-->
	beq ++				;If not, branch to exit.
	lda AnimResetIndex		;
	cmp #an_SamusSalto		;Is Samus summersaulting?-->
	beq +				;If so, branch to clear carry(screw attack active).
	cmp #an_SamusJump		;
	sec				;Is Samus jumping?-->
	bne ++				;If not, branch to exit.
	bit ObjVertSpeed		;If Samus is jumping and still moving upwards, screw--> 
	bpl ++				;attack is active.
*	cmp AnimIndex			;Screw attack will still be active if not spinning, but-->
*	rts				;jumping while running and still moving upwards.

;----------------------------------------------------------------------------------------------------

UnknownCDBF:
  lda Joy1Status
	and #$08
	lsr
	lsr
	lsr
	tax
	lda RunAnimationTbl,x
	cmp AnimResetIndex
	beq -
	jsr SetSamusAnim
	pla
	pla
	jmp UnknownCD6B

UnknownCDD7:
  jsr FireWeapon			;($D1EE)Shoot left/right.
	lda Joy1Status
	and #$08
	bne +
	lda #an_SamusFireRun
	sta AnimIndex
	rts

*       lda AnimIndex
	sec
	sbc AnimResetIndex
	and #$03
	tax
	lda Table05,x
	jmp SetSamusNextAnim

; Table used by above subroutine

Table05:
	.byte $3F
	.byte $3B
	.byte $3D
	.byte $3F

CheckHealthStatus:
  lda SamusHit			;
	and #$20			;Has Samus been hit?-->
	beq +++				;If not, branch to check if still blinking from recent hit.
	lda #$32			;
	sta SamusBlink			;Samus has been hit. Set blink for 32 frames.
	lda #$FF
	sta DamagePushDirection
	lda $73
	sta $77
	beq ++
	bpl +
	jsr SFX_SamusHit
*       lda SamusHit
	and #$08
	lsr
	lsr
	lsr
	sta DamagePushDirection
*	lda #$FD
	sta ObjVertSpeed
	lda #$38			;Samus is hit. Store Samus hit gravity.
	sta SamusGravity		;
	jsr IsSamusDead
	bne +
	jmp CheckHealthBeep

*	lda SamusBlink
	beq CheckHealthBeep
	dec SamusBlink
	ldx DamagePushDirection
	inx
	beq +++
	jsr Adiv16       ; / 16
	cmp #$03
	bcs +
	ldy SamusHorzAccel
	bne +++
	jsr SetHorzMvmntData
*       dex
	bne +
	jsr TwosCompliment		;($C3D4)
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
	bne +				;Only beep every 16th frame.
	jsr SFX_Beep
*	lda #$00
	sta SamusHit
	rts

;----------------------------------------[ Is Samus dead ]-------------------------------------------

IsSamusDead:
	lda ObjAction			;
	cmp #sa_Dead			;
	beq Exit3			;Samus is dead. Zero flag is set.
	cmp #sa_Dead2			;
	beq Exit3			;
	cmp #$FF			;Samus not dead. Clear zero flag.

Exit3:  
	rts				;Exit for routines above and below.

;----------------------------------------[ Subtract health ]-----------------------------------------

SubtractHealth:
	lda HealthLoChange		;Check to see if health needs to be changed.-->
	ora HealthHiChange		;If not, branch to exit.
	beq Exit3			;
	jsr IsSamusDead			;($CE84)Check if Samus is already dead.
	beq ClearDamage			;Samus is dead. Branch to clear damage values.
	ldy EndTimerHi			;If end escape timer is running, Samus cannot be hurt.
	iny				;
	beq +				;Branch if end escape timer not active.

ClearDamage:
	jmp ClearHealthChange		;($F323)Clear health change values.

*	lda MotherBrainStatus		;If mother brain is in the process of dying, receive-->
	cmp #$03			;no damage.
	bcs ClearDamage			;

	lda SamusGear			;
	and #gr_VARIA			;Check is Samus has Varia.
	beq +				;
	lsr HealthLoChange		;If Samus has Varia, divide damage by 2.
	lsr HealthHiChange		;
	bcc +				;If HealthHi moved a bit into the carry flag while-->
	lda #$4F			;dividing, add #$4F to HealthLo for proper-->
	adc HealthLoChange		;division results.
	sta HealthLoChange		;

*	lda HealthLo			;Prepare to subtract from HealthLo.
	sta $03				;
	lda HealthLoChange		;Amount to subtract from HealthLo.
	sec				;
	jsr Base10Subtract		;($C3FB)Perform base 10 subtraction.
	sta HealthLo			;Save results.

   lda HealthHi			;Prepare to subtract from HealthHi.
	sta $03				;
	lda HealthHiChange		;Amount to subtract from HealthHi.
	jsr Base10Subtract		;($C3FB)Perform base 10 subtraction.
	sta HealthHi			;Save Results.

	lda HealthLo			;
	and #$F0			;Is Samus health at 0?  If so, branch to-->
	ora HealthHi			;begin death routine.
	beq +				;
	bcs ++				;Samus not dead. Branch to exit.

*	lda #$00			;Samus is dead.
	sta HealthLo			;
	sta HealthHi			;Set health to #$00.
	lda #sa_Dead			;
	sta ObjAction			;Death handler.
	jsr SFX_SamusDie		;($CBE2)Start Samus die SFX.
	jmp SetSamusExplode		;($CC8B)Set Samus exlpode routine.

;----------------------------------------[ Add health ]----------------------------------------------

AddHealth:
	lda HealthLo			;Prepare to add to HealthLo.
	sta $03				;
	lda HealthLoChange		;Amount to add to HealthLo.
	clc				;
	jsr Base10Add			;($C3DA)Perform base 10 addition.
	sta HealthLo			;Save results.

	lda HealthHi			;Prepare to add to HealthHi.
	sta $03				;
	lda HealthHiChange		;Amount to add to HealthHi.
	jsr Base10Add			;($C3DA)Perform base 10 addition.
	sta HealthHi			;Save results.

	lda TankCount			;
	jsr Amul16			;($C2C5)*16. Move tank count to upper 4 bits.
	ora #$0F			;Set lower 4 bits.
	cmp HealthHi			;
	bcs +				;Is life less than max? if so, branch.
	and #$F9			;Life is more than max amount. 
	sta HealthHi			;
	lda #$99			;Set life to max amount.
	sta HealthLo			;
*	jmp ClearHealthChange		;($F323)

;----------------------------------------------------------------------------------------------------

UnknownCF2E:
	lda SamusHit
	lsr
	and #$02
	beq +++
	bcs +
	lda SamusHorzAccel
	bmi +++
	bpl ++
*       lda SamusHorzAccel
	bmi +
	bne ++
*	jsr TwosCompliment		;($C3D4)
	sta SamusHorzAccel

ClearHorzMvmntData:
  ldy #$00			;
SetHorzMvmntData:
  sty ObjHorzSpeed		;Set Samus Horizontal speed and horizontal-->
	sty HorzCntrLinear		;linear counter to #$00.
*	rts				;

StopHorzMovement:
  lda SamusHorzAccel		;Is Samus moving horizontally?-->
	bne ClearHorzMvmtAnimData	;If so, branch to stop movement.
	jsr SFX_SamusWalk		;($CB96)Play walk SFX.

ClearHorzMvmtAnimData:
  jsr NoHorzMoveNoDelay		;($CF81)Clear horizontal movement and animation delay data.
	sty ObjAction			;Samus is standing.
	lda Joy1Status			;
	and #$08			;Is The up button being pressed?-->
	bne +				;If so, branch.
	lda #an_SamusStand		;Set Samus animation for standing.

SetSamusAnim:
	sta AnimResetIndex		;Set new animation reset index.

SetSamusNextAnim:
	sta AnimIndex			;Set new animation data index.
	lda #$00			;
	sta AnimDelay			;New animation to take effect immediately.
	rts				;

SetSamusPntUp:
*	lda #sa_PntUp			;
	sta ObjAction			;Samus is pointing up.
	lda #an_SamusPntUp		;
	jsr SetSamusAnim		;($CF6B)Set new animation values.

NoHorzMoveNoDelay:
  jsr ClearHorzData		;($CFB7)Clear all horizontal movement data.
	sty AnimDelay			;Clear animation delay data.
	rts				;

UnknownCF88: 
  lda Joy1Status
	and #$03
	beq +
	jsr BitScan			;($E1E1)
	tax
	jsr SetSamusRunAccel
	lda SamusGravity
	bmi ++
	lda AnimResetIndex
	cmp #an_SamusSalto
	beq ++
	stx SamusDir
	lda Table06+1,x
	jmp SetSamusAnim

*       lda SamusGravity
	bmi +
	beq +
	lda AnimResetIndex
	cmp #an_SamusJump
	bne +

ClearHorzData:
  jsr ClearHorzMvmntData		;($CF4C)Clear horizontal speed and linear counter.
	sty SamusHorzAccel		;Clear horizontal acceleration data.
*	rts				;

UnknownCFBE:
  ldy #an_SamusJumpPntUp
	jmp +

SetSamusJump:
  ldy #an_SamusJump
*       sty AnimResetIndex
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
	bpl +	   ; branch if falling down
	cmp #$20
	bcc +	   ; branch if jumped less than 32 pixels upwards
	bit Joy1Status
	bmi +	   ; branch if JUMP button still pressed
	jsr StopVertMovement		;($D147)Stop jump (start falling).
*       jsr UnknownD055
	jsr UnknownCF2E
	lda Joy1Status
	and #$08     ; UP pressed?
	beq +	   ; branch if not
	lda #an_SamusJumpPntUp
	sta AnimResetIndex
	lda #sa_PntJump      ; "jumping & pointing up" handler
	sta ObjAction
*       jsr UnknownD09C
	lda SamusInLava
	beq +
	lda Joy1Change
	bpl +	   ; branch if JUMP not pressed
	jsr SetSamusJump
	jmp UnknownCD6B

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

UnknownD055:
  ldx #$01
	ldy #$00
	lda Joy1Status
	lsr
	bcs +	   ; branch if RIGHT pressed
	dex
	lsr
	bcc ++++       ; branch if LEFT not pressed
	dex
	iny
*       cpy SamusDir
	beq +++
	lda ObjAction
	cmp #sa_PntJump
	bne +
	lda AnimResetIndex
	cmp Table04,y
	bne ++
	lda Table04+1,y
	jmp ++

*       lda AnimResetIndex
	cmp Table06,y
	bne +
	lda Table06+1,y
*	jsr SetSamusAnim
	lda #$08
	sta AnimDelay
	sty SamusDir
*	stx ObjHorzSpeed
*       rts

; Table used by above subroutine

Table06:
	.byte $0C
	.byte $0C
	.byte $0C
Table04:
	.byte $35
	.byte $35
	.byte $35

UnknownD09C:
  lda Joy1Change
	ora Joy1Retrig
	asl
	bpl -	   ; exit if FIRE not pressed
	lda AnimResetIndex
	cmp #an_SamusJumpPntUp
	bne +
	jmp UnknownD275

*       jsr UnknownD210
	lda #an_SamusFireJump
	jmp SetSamusAnim

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

*       lda #sa_Stand
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
	jsr UnknownE8BE
	stx $05
	lda #$F5
	sta $04
	jsr UnknownFD8F
	jsr UnknownD638
	jsr StopHorzMovement
	dec AnimIndex
	jsr StopVertMovement		;($D147)
	lda #$04
	jmp UnknownD144

*	lda Joy1Change
	jsr BitScan			;($E1E1)
	cmp #$02
	bcs +
	sta SamusDir
	lda #an_SamusRoll
	jsr SetSamusAnim
*       ldx SamusDir
	jsr SetSamusRunAccel
	jsr UnknownCF2E
	jsr CheckBombLaunch
	lda Joy1Status
	and #$03
	bne +
	jsr ClearHorzData
*       lda #$02
UnknownD144:
  jmp SetSamusData		;($CD6D)Set Samus control data and animation.

StopVertMovement:
  ldy #$00
	sty ObjVertSpeed
	sty VertCntrLinear
	rts

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
	lda Table07,x
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
	.word UnknownCFBE
	.word ExitSub       ;($C45C)rts
	.word ExitSub       ;($C45C)rts
	.word ExitSub       ;($C45C)rts

; Table used by above subroutine

Table07:
	.byte sa_Run
	.byte sa_Run
	.byte sa_Roll

FireWeapon:
  lda Joy1Status
	and #$08
	beq UnknownD210
	jmp UnknownD275

UnknownD1F7:
  ldy #$D0
*       lda ObjAction,y
	beq +
	jsr Yplus16
	bne -
	iny
	rts

*       sta $030A,y
	lda MissileToggle
	beq +
	cpy #$D0
*       rts

UnknownD210:
  lda MetroidOnSamus
	bne +
	jsr UnknownD1F7
	bne +
	jsr UnknownD2EB
	jsr UnknownD359
	jsr UnknownD38E
	lda #$0C
	sta $030F,y
	ldx SamusDir
	lda Table99,x   ; get bullet speed
	sta ObjHorzSpeed,y     ; -4 or 4, depending on Samus' direction
	lda #$00
	sta ObjVertSpeed,y
	lda #$01
	sta ObjectOnScreen,y
	jsr CheckMissileLaunch
	lda ObjAction,y
	asl
	ora SamusDir
	and #$03
	tax
	lda Table08,x
	sta $05
	lda #$FA
	sta $04
	jsr UnknownD306
	lda SamusGear
	and #gr_LONGBEAM
	lsr
	lsr
	lsr
	ror
	ora $061F
	sta $061F
	ldx ObjAction,y
	dex
	bne +
	jsr SFX_BulletFire
*       ldy #$09
UnknownD26B:
  tya
	jmp SetSamusNextAnim

Table08:
	.byte $0C
	.byte $F4
	.byte $08
	.byte $F8

Table99:
	.byte $04
	.byte $FC

UnknownD275:
  lda MetroidOnSamus
	bne +
	jsr UnknownD1F7
	bne +
	jsr UnknownD2EB
	jsr UnknownD38A
	jsr UnknownD38E
	lda #$0C
	sta $030F,y
	lda #$FC
	sta ObjVertSpeed,y
	lda #$00
	sta ObjHorzSpeed,y
	lda #$01
	sta ObjectOnScreen,y
	jsr UnknownD340
	ldx SamusDir
	lda Table09+4,x
	sta $05
	lda ObjAction,y
	and #$01
	tax
	lda Table09+6,x
	sta $04
	jsr UnknownD306
	lda SamusGear
	and #gr_LONGBEAM
	lsr
	lsr
	lsr
	ror
	ora $061F
	sta $061F
	lda ObjAction,y
	cmp #$01
	bne +
	jsr SFX_BulletFire
*       ldx SamusDir
	ldy Table09,x
	lda SamusGravity
	beq +
	ldy Table09+2,x
*       lda ObjAction
	cmp #$01
	beq +
	jmp UnknownD26B

; Table used by above subroutine

Table09:
	.byte $26
	.byte $26
	.byte $34
	.byte $34
	.byte $01
	.byte $FF
	.byte $EC
	.byte $F0

UnknownD2EB:
  tya
	tax
	inc ObjAction,x
	lda #$02
	sta ObjRadY,y
	sta ObjRadX,y
	lda #an_Bullet

SetProjectileAnim:
	sta AnimResetIndex,x
UnknownD2FD:
 sta AnimIndex,x
	lda #$00
	sta AnimDelay,x
*       rts

UnknownD306:
  ldx #$00
	jsr UnknownE8BE
	tya
	tax
	jsr UnknownFD8F
	txa
	tay
	jmp UnknownD638

CheckMissileLaunch:
	lda MissileToggle
	beq Exit4       ; exit if Samus not in "missile fire" mode
	cpy #$D0
	bne Exit4
	ldx SamusDir
	lda MissileAnims,x
*       jsr SetBulletAnim
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

UnknownD340:
  lda MissileToggle
	beq Exit4
	cpy #$D0
	bne Exit4
	lda #$8F
	bne -

SetBulletAnim:
	sta AnimIndex,y
	sta AnimResetIndex,y
	lda #$00
	sta AnimDelay,y
Exit4:  rts

UnknownD359:
  lda SamusDir
*       sta $0502,y
	bit SamusGear
	bvc Exit4       ; branch if Samus doesn't have Wave Beam
	lda MissileToggle
	bne Exit4
	lda #$00
	sta $0501,y
	sta $0304,y
	tya
	jsr Adiv32      ; / 32
	lda #$00
	bcs +
	lda #$0C
*       sta $0500,y
	lda #wa_WaveBeam
	sta ObjAction,y
	lda #an_WaveBeam
	jsr SetBulletAnim
	jmp SFX_WaveFire

UnknownD38A:
  lda #$02
	bne --
UnknownD38E:
  lda MissileToggle
	bne Exit4
	lda SamusGear
	bpl Exit4       ; branch if Samus doesn't have Ice Beam
	lda #wa_IceBeam
	sta ObjAction,y
	lda $061F
	ora #$01
	sta $061F
	jmp SFX_BulletFire

; SamusDoor
; =========

SamusDoor:
	lda DoorStatus
	cmp #$05
	bcc +++++++
    ; move Samus out of door, how far depends on initial value of DoorDelay
	dec DoorDelay
	bne MoveOutDoor
    ; done moving
	asl
	bcc +
	lsr
	sta DoorStatus
	bne +++++++
*       jsr UnknownD48C
	jsr UnknownED65
	jsr Unknown95AB
	lda ItemRoomMusicStatus
	beq ++
	pha
	jsr StartMusic      ; start music
	pla
	bpl ++
	lda #$00
	sta ItemRoomMusicStatus
	beq ++
*       lda #$80
	sta ItemRoomMusicStatus
*       lda KraidRidleyPresent
	beq +
	jsr TourianMusic
	lda #$00
	sta KraidRidleyPresent
	beq --	   ; branch always
*       lda SamusDoorData
	and #$0F
	sta ObjAction
	lda #$00
	sta SamusDoorData
	sta DoorStatus
	jsr StopVertMovement		;($D147)

MoveOutDoor:
	lda SamusDoorDir
	beq ++	  ; branch if door leads to the right
	ldy ObjectX
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       dec ObjectX
	jmp ++

*	inc ObjectX
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*	jsr CheckHealthStatus		;($CDFA)Check if Samus hit, blinking or Health low.
	jsr SetmirrorCntrlBit
	jmp DrawFrame       ; display Samus

SamusDead:
D41A:	lda #$01
	jmp SetSamusData		;($CD6D)Set Samus control data and animation.

SamusDead2:
	dec AnimDelay
	rts

; SamusElevator
; =============

SamusElevator:
	lda ElevatorStatus
	cmp #$03
	beq +
	cmp #$08
	bne +++++++
*       lda $032F
	bmi +++
	lda ObjectY
	sec
	sbc ScrollY     ; A = Samus' Y position on the visual screen
	cmp #$84
	bcc +	   ; if ScreenY < $84, don't scroll
	jsr ScrollDown  ; otherwise, attempt to scroll
*       ldy ObjectY
	cpy #239	; wrap-around required?
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
	ldy #$FF	; ObjectY will now be 0
*       iny
	sty ObjectY
	jmp UnknownD47E

*	lda ObjectY
	sec
	sbc ScrollY     ; A = Samus' Y position on the visual screen
	cmp #$64
	bcs +	   ; if ScreenY >= $64, don't scroll
	jsr ScrollUp    ; otherwise, attempt to scroll
*       ldy ObjectY
	bne +	   ; wraparound required? (branch if not)
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
	ldy #240	; ObjectY will now be 239
*       dey
	sty ObjectY
	jmp UnknownD47E

*	ldy #$00
	sty ObjVertSpeed
	cmp #$05
	beq +
	cmp #$07
	beq +
UnknownD47E:
  lda FrameCount
	lsr
	bcc ++
*       jsr SetmirrorCntrlBit		;($CD92)Mirror Samus, if necessary.
	lda #$01
	jmp AnimDrawObject
*	rts

UnknownD48C:
  ldx #$60
	sec
*       jsr UnknownD4B4
	txa
	sbc #$20
	tax
	bpl -
	jsr GetNameTable		;($EB85)
	tay
	ldx #$18
*       jsr UnknownD4A8
	txa
	sec
	sbc #$08
	tax
	bne -
	
UnknownD4A8:
  tya
	cmp $072C,x
	bne +
	lda #$FF
	sta $0728,x
*       rts

UnknownD4B4:
	lda $0405,x
	and #$02
	bne +
	sta EnStatus,x
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

	.word ExitSub     	;($C45C) rts
	.word UpdateBullet 	; regular beam
	.word UpdateWaveBullet  ; wave beam
	.word UpdateIceBullet   ; ice beam
	.word BulletExplode    	; bullet/missile explode
	.word LayBomb       	; lay bomb
	.word UpdateBomb      	; bomb countdown
	.word ExplodeBomb       ; bomb explode
	.word LayBomb      		; lay bomb
	.word UpdateBomb       	; bomb countdown
	.word ExplodeBomb       ; bomb explode
	.word UpdateBullet  ; missile

UpdateBullet:
	lda #$01
	sta UpdatingProjectile
	jsr UnknownD5FC
	jsr UnknownD5DA
	jsr BulletCrashDetection
CheckBulletStat:
	ldx PageIndex
	bcc +
	lda SamusGear
	and #gr_LONGBEAM
	bne DrawBullet  ; branch if Samus has Long Beam
	dec $030F,x     ; decrement bullet timer
	bne DrawBullet
	lda #$00	; timer hit 0, kill bullet
	sta ObjAction,x
	beq DrawBullet  ; branch always
*       lda ObjAction,x
	beq +
	jsr UnknownD5E4
DrawBullet:
	lda #$01
	jsr AnimDrawObject
*       dec UpdatingProjectile
	rts

*       inc $0500,x
UnknownD522:
  inc $0500,x
	lda #$00
	sta $0501,x
	beq +	   ; branch always

UpdateWaveBullet:
	lda #$01
	sta UpdatingProjectile
	jsr UnknownD5FC
	jsr UnknownD5DA
	lda $0502,x
	and #$FE
	tay
	lda Table0A,y
	sta $0A
	lda Table0A+1,y
	sta $0B
*       ldy $0500,x
	lda ($0A),y
	cmp #$FF
	bne +
	sta $0500,x
	jmp UnknownD522

*       cmp $0501,x
	beq ---
	inc $0501,x
	iny
	lda ($0A),y
	jsr Unknown8296
	ldx PageIndex
	sta ObjVertSpeed,x
	lda ($0A),y
	jsr Unknown832F
	ldx PageIndex
	sta ObjHorzSpeed,x
	tay
	lda $0502,x
	lsr
	bcc +
	tya
	jsr TwosCompliment		;($C3D4)
	sta ObjHorzSpeed,x
*       jsr BulletCrashDetection
	bcs +
	jsr UnknownD624
*       jmp CheckBulletStat

Table0A:
	.word Table0C     ; pointer to table #1 below
	.word Table0D     ; pointer to table #2 below

; Table #1 (size: 25 bytes)

Table0C:
	.byte $01
	.byte $F3
	.byte $01
	.byte $D3
	.byte $01
	.byte $93
	.byte $01
	.byte $13
	.byte $01
	.byte $53
	.byte $01
	.byte $73
	.byte $01
	.byte $73
	.byte $01
	.byte $53
	.byte $01
	.byte $13
	.byte $01
	.byte $93
	.byte $01
	.byte $D3
	.byte $01
	.byte $F3
	.byte $FF

; Table #2 (size: 25 bytes)

Table0D:
	.byte $01
	.byte $B7
	.byte $01
	.byte $B5
	.byte $01
	.byte $B1
	.byte $01
	.byte $B9
	.byte $01
	.byte $BD
	.byte $01
	.byte $BF
	.byte $01
	.byte $BF
	.byte $01
	.byte $BD
	.byte $01
	.byte $B9
	.byte $01
	.byte $B1
	.byte $01
	.byte $B5
	.byte $01
	.byte $B7
	.byte $FF

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
*       jmp DrawBullet

UnknownD5DA:
  lda $030A,x
	beq Exit5
	lda #$00
	sta $030A,x
UnknownD5E4:
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

UnknownD5FC:
  lda ObjectOnScreen,x
	lsr
	bcs Exit5
*	lda #$00
	beq --	 ; branch always
*       jmp LE81E

; bullet <--> background crash detection

BulletCrashDetection:
  jsr GetObjCoords
	ldy #$00
	lda ($04),y     ; get tile # that bullet touches
	cmp #$A0
	bcs UnknownD624
	jsr Unknown95C0
	cmp #$4E
	beq -
	jsr UnknownD651
	bcc ++
	clc
	jmp IsBlastTile

UnknownD624:
  ldx PageIndex
	lda ObjHorzSpeed,x
	sta $05
	lda ObjVertSpeed,x
	sta $04
	jsr UnknownE8BE
	jsr UnknownFD8F
	bcc --
	
UnknownD638:
  lda $08
	sta ObjectY,x
	lda $09
	sta ObjectX,x
	lda $0B
	and #$01
	bpl +	   ; branch always
	ToggleObjectHi:
	lda ObjectHi,x
	eor #$01
*       sta ObjectHi,x
*	rts

UnknownD651:
  ldy InArea
	cpy #$10
	beq +
	cmp #$70
	bcs ++
*       cmp #$80
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
*       jsr SetProjectileAnim
	inc ObjAction,x
	jsr SFX_BombExplode
*	jmp DrawBomb

ExplodeBomb:
  inc $030F,x
	jsr UnknownD6A7
	ldx PageIndex
	lda $0303,x
	sec
	sbc #$F7
	bne +
	sta ObjAction,x     ; kill bomb
*       jmp DrawBomb

UnknownD6A7:
  jsr GetObjCoords
	lda $04
	sta $0A
	lda $05
	sta $0B
	ldx PageIndex
	ldy $030F,x
	dey
	beq ++
	dey
	bne +++
	lda #$40
	jsr UnknownD78B
	txa
	bne +
	lda $04
	and #$20
	beq Exit6
*       lda $05
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
	jsr UnknownD78B
*	jsr UnknownD76A
Exit6:  rts

*	dey
	bne +++
	lda #$40
	jsr UnknownD77F
	txa
	bne +
	lda $04
	and #$20
	bne Exit6
*       lda $05
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
	jsr UnknownD77F
*       jmp UnknownD76A

*	dey
	bne +++
	lda #$02
	jsr UnknownD78B
	txa
	bne +
	lda $04
	lsr
	bcc Exit7
*       lda $04
	and #$1F
	cmp #$1E
	bcc +
	lda ScrollDir
	and #$02
	beq Exit7
	lda #$1E
	jsr UnknownD77F
	lda $05
	eor #$04
	sta $05
*       jmp UnknownD76A

*	dey
	bne Exit7
	lda #$02
	jsr UnknownD77F
	txa
	bne +
	lda $04
	lsr
	bcs Exit7
*       lda $04
	and #$1F
	cmp #$02
	bcs UnknownD76A
	lda ScrollDir
	and #$02
	beq Exit7
	lda #$1E
	jsr UnknownD78B
	lda $05
	eor #$04
	sta $05
	
UnknownD76A:
  txa
	pha
	ldy #$00
	lda ($04),y
	jsr UnknownD651
	bcc +
	cmp #$A0
	bcs +
	jsr UnknownE9C2
*       pla
	tax
Exit7:  rts

UnknownD77F:
  clc
	adc $0A
	sta $04
	lda $0B
	adc #$00
	jmp UnknownD798

UnknownD78B:
  sta $00
	lda $0A
	sec
	sbc $00
	sta $04
	lda $0B
	sbc #$00
	
UnknownD798:
  and #$07
	ora #$60
	sta $05
*       rts

	GetObjCoords:
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
	lda ObjAction,x
	jsr ChooseRoutine

; Pointer table to elevator handlers

	.word ExitSub       ;($C45C) rts
	.word ElevatorIdle
	.word UnknownD80E
	.word ElevatorMove
	.word ElevatorScroll
	.word UnknownD8A3
	.word UnknownD8BF
	.word UnknownD8A3
	.word ElevatorMove
	.word ElevatorStop

	ElevatorIdle:
	lda SamusOnElevator
	beq ShowElevator
	lda #$04
	bit $032F       ; elevator direction in bit 7 (1 = up)
	bpl +
	asl		; btn_UP
*       and Joy1Status
	beq ShowElevator
    ; start elevator!
	jsr StopVertMovement		;($D147)
	sty AnimDelay
	sty SamusGravity
	tya
	sta ObjVertSpeed,x
	inc ObjAction,x
	lda #sa_Elevator
	sta ObjAction
	lda #an_SamusFront
	jsr SetSamusAnim
	lda #128
	sta ObjectX     ; center
	lda #112
	sta ObjectY     ; center
	ShowElevator:
	lda FrameCount
	lsr
	bcc --	  ; only display elevator at odd frames
	jmp DrawFrame       ; display elevator

UnknownD80E:
  lda ScrollX
	bne +
	lda MirrorCntrl
	ora #$08
	sta MirrorCntrl
	lda ScrollDir
	and #$01
	sta ScrollDir
	inc ObjAction,x
	jmp ShowElevator

*       lda #$80
	sta ObjectX
	lda ObjectX,x
	sec
	sbc ScrollX
	bmi +
	jsr ScrollLeft
	jmp ShowElevator

*       jsr ScrollRight
	jmp ShowElevator

	ElevatorMove:
	lda $030F,x
	bpl ++	  ; branch if elevator going down
    ; move elevator up one pixel
	ldy ObjectY,x
	bne +
	jsr ToggleObjectHi
	ldy #240
*       dey
	tya
	sta ObjectY,x
	jmp ++

    ; move elevator down one pixel
*	inc ObjectY,x
	lda ObjectY,x
	cmp #240
	bne +
	jsr ToggleObjectHi
	lda #$00
	sta ObjectY,x
*	cmp #$83
	bne +	   ; move until Y coord = $83
	inc ObjAction,x
*       jmp ShowElevator

	ElevatorScroll:
	lda ScrollY
	bne ElevScrollRoom  ; scroll until ScrollY = 0
	lda #$4E
	sta AnimResetIndex
	lda #$41
	sta AnimIndex
	lda #$5D
	sta AnimResetIndex,x
	lda #$50
	sta AnimIndex,x
	inc ObjAction,x
	lda #$40
	sta Timer1
	jmp ShowElevator

	ElevScrollRoom:
	lda $030F,x
	bpl +	   ; branch if elevator going down
	jsr ScrollUp
	jmp ShowElevator

*       jsr ScrollDown
	jmp ShowElevator

UnknownD8A3:
  inc ObjAction,x
	lda ObjAction,x
	cmp #$08	; ElevatorMove
	bne +
	lda #$23
	sta $0303,x
	lda #an_SamusFront
	jsr SetSamusAnim
	jmp ShowElevator

*       lda #$01
	jmp AnimDrawObject

UnknownD8BF:
  lda $030F,x
	tay
	cmp #$8F	; Leads-To-Ending elevator?
	bne +
    ; Samus made it! YAY!
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

*       tya
	bpl ++
	ldy #$00
	cmp #$84
	bne +
	iny
*       tya
*	ora #$10
	jsr IsEngineRunning
	lda PalToggle
	eor #$07
	sta PalToggle
	ldy InArea
	cpy #$12
	bcc +
	lda #$01
*       sta PalDataPending
	jsr WaitNMIPass_
	jsr SelectSamusPal
	jsr StartMusic			;($LD92C)Start music.
	jsr ScreenOn
	jsr CopyPtrs
	jsr DestroyEnemies
	ldx #$20
	stx PageIndex
	lda #$6B
	sta AnimResetIndex
	lda #$5F
	sta AnimIndex
	lda #$7A
	sta AnimResetIndex,x
	lda #$6E
	sta AnimIndex,x
	inc ObjAction,x
	lda #$40
	sta Timer1
	rts

StartMusic:
	lda ElevatorStatus
	cmp #$06
	bne +
	lda $032F
	bmi ++
*       lda Unknown95CD			;Load proper bit flag for area music.
	ldy ItemRoomMusicStatus
	bmi ++
	beq ++
*	lda #$81
	sta ItemRoomMusicStatus
	lda #$20			;Set flag to play item room music.

*	ora MusicInitFlag		;
	sta MusicInitFlag		;Store music flag info.
	rts				;

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
*       jmp ShowElevator
*	jmp ElevScrollRoom

SamusOnElevatorOrEnemy:
  lda #$00			;
	sta SamusOnElevator		;Assume Samus is not on an elevator or on a frozen enemy.
	sta OnFrozenEnemy		;
	tay
	ldx #$50
	jsr LF186
*   lda EnStatus,x
	cmp #$04
	bne +
	jsr LF152
	jsr LF1BF
	jsr LF1FA
	bcs +
	jsr UnknownD9BA
	bne +
UnknownD99A:	
	inc OnFrozenEnemy		;Samus is standing on a frozen enemy.
	bne ++
*       jsr Xminus16
	bpl --
*	lda ElevatorStatus
	beq +
	ldy #$00
	ldx #$20
	jsr UnknownDC82
	bcs +
	jsr UnknownD9BA
	bne +
	inc SamusOnElevator		;Samus is standing on elevator.
*   rts

UnknownD9BA:
  lda $10
	and #$02
	bne +
	ldy $11
	iny
	cpy $04
	beq Exit8
*   lda SamusHit
	and #$38
	ora $10
	ora #$40
	sta SamusHit
Exit8:  rts

; UpdateStatues
; =============

	UpdateStatues:
	lda #$60
	sta PageIndex
	ldy $0360
	beq Exit8	   ; exit if no statue present
	dey
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
*   lda $687C
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
	lda DoorStatus
	bne Exit0
	lda KraidStatueStatus
	and $687C
	bpl Exit0
	sta $54
	ldx #$70
	ldy #$08
*       lda #$03
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
Exit0:  rts

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
	jsr GetObjCoords
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
	jsr UnknownDC7F
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
  lda MapPosX
UnknownDC1E:
  sta $07
	lda MapPosY
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


UnknownDC7F: 
	jsr LF186
UnknownDC82:  
	jsr LF172
UnknownDC85:	
	jsr LF1A7
UnknownDC88:	
	jmp LF1FA

;The following table is used to rotate the sprites of both Samus and enemies when they explode.

ExplodeRotationTbl:
	.byte $00			;No sprite flipping.
	.byte $80			;Flip sprite vertically.
	.byte $C0			;Flip sprite vertically and horizontally.
	.byte $40			;Flip sprite horizontally.

; UpdateObjAnim
; =============
; Advance to object's next frame of animation

UpdateObjAnim:
	ldx PageIndex
	ldy AnimDelay,x
	beq +	   ; is it time to advance to the next anim frame?
	dec AnimDelay,x     ; nope
	bne +++	  ; exit if still not zero (don't update animation)
*       sta AnimDelay,x     ; set initial anim countdown value
	ldy AnimIndex,x
*       lda ObjectAnimIndexTbl,y		;($8572)Load frame number.
	cmp #$FF	; has end of anim been reached?
	beq ++
	sta AnimFrame,x     ; store frame number
	iny	     ; inc anim index
	tya
	sta AnimIndex,x     ; store anim index
*	rts

*       ldy AnimResetIndex,x     ; reset anim frame index
	jmp ---	   ; do first frame of animation

  pha
	lda #$00
	sta $06
	pla
	bpl +
	dec $06
*       clc
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
	tax				;Keep a copy of the value in x as well.
	jsr Adiv16			;($C2BF)Move upper 4 bits to lower 4 bits.
	and #$03			;
	sta $05				;The following lines take the upper 4 bits in the-->
	txa				;control byte and transfer bits 4 and 5 into $05 bits 0-->
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

UnknownDCF5:
  jsr ClearObjectCntrl		;($DF2D)Clear object control byte.
	pla
	pla
	ldx PageIndex
UnknownDCFC:
  lda InArea
	cmp #$13
	bne +
	lda EnDataIndex,x
	cmp #$04
	beq +++++
	cmp #$02
	beq +++++
*       lda $040C,x
	asl
	bmi UnknownDD75
	jsr LF74B
	sta $00
	jsr $80B0
	and #$20
	sta EnDataIndex,x
	lda #$05
	sta EnStatus,x
	lda #$60
	sta $040D,x
	lda RandomNumber1
	cmp #$10
	bcc UnknownDD5B
*	and #$07
	tay
	lda ItemDropTbl,y
	sta EnAnimFrame,x
	cmp #$80
	bne ++
	ldy MaxMissilePickup
	cpy CurrentMissilePickups
	beq UnknownDD5B
	lda MaxMissiles
	beq UnknownDD5B
	inc CurrentMissilePickups
*       rts

*       ldy MaxEnergyPickup
	cpy CurrentEnergyPickups
	beq UnknownDD5B
	inc CurrentEnergyPickups
	cmp #$89
	bne --
	lsr $00
	bcs --

UnknownDD5B:
  ldx PageIndex
	lda InArea
	cmp #$13
	beq ++
*	jmp KillObject			;($FA18)Free enemy data slot.

*       lda RandomNumber1
	ldy #$00
	sty CurrentEnergyPickups
	sty CurrentMissilePickups
	iny
	sty MaxMissilePickup
	sty MaxEnergyPickup
	bne -----

UnknownDD75:
  jsr PowerUpMusic
	lda InArea
	and #$0F
	sta MiniBossKillDelay
	lsr
	tay
	sta MaxMissiles,y
	lda #75
	jsr AddToMaxMissiles
	bne UnknownDD5B

ClrObjCntrlIfFrameIsF7:
  ldx PageIndex
	lda EnAnimFrame,x
	cmp #$F7
	bne +++
	jmp ClearObjectCntrl		;($DF2D)Clear object control byte.

; AddToMaxMissiles
; ================
; Adds A to both MissileCount & MaxMissiles, storing the new count
; (255 if it overflows)

AddToMaxMissiles:
	pha				;Temp storage of # of missiles to add.
	clc
	adc MissileCount
	bcc +
	lda #$FF
*   sta MissileCount
	pla
	clc
	adc MaxMissiles
	bcc +
	lda #$FF
*   sta MaxMissiles
	rts

*	lda EnYRoomPos,x
	sta $0A	 ; Y coord
	lda EnXRoomPos,x
	sta $0B	 ; X coord
	lda EnNameTable,x
	sta $06	 ; hi coord
	lda EnAnimFrame,x
	asl
	tay
	lda ($41),y
	bcc +
	lda ($43),y
*       sta $00
	iny
	lda ($41),y
	bcc +
	lda ($43),y
*       sta $01
	jsr GetSpriteCntrlData		;($DCC3)Get place pointer index and sprite control data.
	tay
	lda ($45),y
	sta $02
	iny
	lda ($45),y
	sta $03
	ldy #$00
	cpx #$02
	bne +
	ldx PageIndex
	inc EnCounter,x
	lda EnCounter,x
	pha
	and #$03
	tax
	lda $05
	and #$3F
	ora ExplodeRotationTbl,x
	sta $05
	pla
	cmp #$19
	bne +
	jmp UnknownDCF5

*       ldx PageIndex
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
	txa
	asl
	sta $08
	ldx PageIndex
	lda $0405,x
	and #$FD
	ora $08
	sta $0405,x
	lda $08
	beq ++
	jmp UnknownDEDE

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
*	jmp ClearObjectCntrl		;($DF2D)Clear object control byte.
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
	
UnknownDEDE:
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
	sta ObjectCntrl			;Clear object control byte.
	rts				;

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
LE000:	bcs ++				;If carry is still set, sprite is not in screen boundaries.
LE002:	lda $10				;
LE004:	sbc #$0F			;Move sprite y position up 15 pixles.
LE006:	sta $10				;
LE008:	lda $09				;
LE00A:	clc				;If a portion of the object is outside the sceen-->
LE00B:	adc $10				;boundaries, treat object as if the whole thing is-->
LE00D:	cmp #$F0			;not visible.
LE00F:	bcc +++				;
LE011:	clc				;Causes next statement to branch always.
LE012:*	bcc +				;
LE014:	lda $09				;If object is on same name table as the current one in-->
LE016:	cmp $10				;the PPU, check if part of object is out of screen--> 
LE018:	bcc ++				;boundaries.  If so, branch.
LE01A:*	dex				;Sprite is not within screen boundaries. Decrement X.
LE01B:*	rts				;

HorzScrollCheck:
LE01C:  lda $06				;
LE01E:	eor PPUCNT0ZP			;Check if object is on different name table as current-->
LE020:	and #$01			;name table active in PPU.-->
LE022:	beq +				;If not, branch.
LE024:	bcs ++				;If carry is still set, sprite is not in screen boundaries.
LE026:	lda $09				;
LE028:	clc				;If a portion of the object is outside the sceen-->
LE029:	adc $0E				;boundaries, treat object as if the whole thing is-->
LE02B:	bcc +++				;not visible.
LE02D:	clc				;Causes next statement to branch always.
LE02E:*	bcc +				;
LE030:	lda $09				;If object is on same name table as the current one in-->
LE032:	cmp $0E				;the PPU, check if part of object is out of screen--> 
LE034:	bcc ++				;boundaries.  If so, branch.
LE036:*	dex				;Sprite is not within screen boundaries. Decrement X.
LE037:*	rts				;

;------------------------[ Override sprite flip bits with object flip bits ]-------------------------

;If the MSB is set in ObjectCntrl, its two upper bits that control sprite flipping take priority
;over the sprite control bits.  This function modifies the sprite control byte with any flipping
;bits found in ObjectCntrl.

SpriteFlipBitsOveride:
LE038:	lsr ObjectCntrl			;Restore MSB.
LE03A:	lda ($00),y			;Reload frame data control byte into A.
LE03C:	and #$C0			;Extract the two sprite flip bytes from theoriginal-->
LE03E:	ora ObjectCntrl			;control byte and set any additional bits from ObjectCntrl.
LE040:	sta $05				;Store modified byte to load in sprite control byte later.
LE042:	lda ObjectCntrl			;
LE044:	ora #$80			;
LE046:	sta ObjectCntrl			;Ensure MSB of object control byte remains set.
LE048:	rts				;

;--------------------------------[ Explosion placement data ]---------------------------------------

;The following table has the index values into the table after it for finding the placement data
;for an exploding object.

ExplodeIndexTbl:
LE049:	.byte $00, $18, $30

;The following table is used to produce the arcing motion of exploding objects.  It is displacement
;data for the y directions only.  The x displacement is constant.

ExplodePlacementTbl:

;Bottom sprites.
LE04C:	.byte $FC, $F8, $F4, $F0, $EE, $EC, $EA, $E8, $E7, $E6, $E6, $E5, $E5, $E4, $E4, $E3
LE05C:	.byte $E5, $E7, $E9, $EB, $EF, $F3, $F7, $FB

;Middle sprites.
LE064:	.byte $FE, $FC, $FA, $F8, $F6, $F4, $F2, $F0, $EE, $ED, $EB, $EA, $E9, $E8, $E7, $E6
LE074:	.byte $E6, $E6, $E6, $E6, $E8, $EA, $EC, $EE

;Top sprites.
LE07C:	.byte $FE, $FC, $FA, $F8, $F7, $F6, $F5, $F4, $F3, $F2, $F1, $F1, $F0, $F0, $EF, $EF
LE08C:	.byte $EF, $EF, $EF, $EF, $F0, $F0, $F1, $F2

;--------------------------------------[ Update enemy animation ]-----------------------------------

;Advance to next frame of enemy's animation. Basically the same as UpdateObjAnim, only for enemies.

UpdateEnemyAnim:
LE094:	ldx PageIndex			;Load index to desired enemy.
LE096:	ldy EnStatus,x			;
LE099:	cpy #$05			;Is enemy in the process of dying?-->
LE09B:	beq +++				;If so, branch to exit.
LE09D:	ldy EnAnimDelay,x		;
LE0A0:	beq +				;Check if current anumation frame is ready to be updated.
LE0A2:	dec EnAnimDelay,x		;Not ready to update. decrement delay timer and-->
LE0A5:	bne +++				;branch to exit.
LE0A7:*	sta EnAnimDelay,x		;Save new animation delay value.
LE0AA:	ldy EnAnimIndex,x		;Load enemy animation index.
LE0AD:*	lda (EnemyAnimPtr),y		;Get animation data.
LE0AF:	cmp #$FF			;End of animation?
LE0B1:	beq ++				;If so, branch to reset animation.
LE0B3:	sta EnAnimFrame,x		;Store current animation frame data.
LE0B6:	iny				;Increment to next animation data index.
LE0B7:	tya				;
LE0B8:	sta EnAnimIndex,x		;Save new animation index.
LE0BB:*	rts				;

LE0BC:*	ldy EnResetAnimIndex,x		;reset animation index.
LE0BF:	bcs ---				;Branch always.

;---------------------------------------[ Display status bar ]---------------------------------------

;Displays Samus' status bar components.

DisplayBar:
LE0C1:	ldy #$00			;Reset data index.
LE0C3:	lda SpritePagePos		;Load current sprite index.
LE0C5:	pha				;save sprite page pos.
LE0C6:	tax				;
LE0C7:*	lda DataDisplayTbl,y		;
LE0CA:	sta Sprite00RAM,x		;Stor contents of DataDisplayTbl in sprite RAM.
LE0CD:	inx				;
LE0CE:	iny				;
LE0CF:	cpy #$28			;10*4. At end of DataDisplayTbl? If not, loop to-->
LE0D1:	bne -				;load next byte from table.

;Display 2-digit health count.
LE0D3:	stx SpritePagePos		;Save new location in sprite RAM.
LE0D5:	pla				;Restore initial sprite page pos.
LE0D6:	tax				;
LE0D7:	lda HealthHi			;
LE0DA:	and #$0F			;Extract upper health digit.
LE0DC:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE0DF:	lda HealthLo			;
LE0E2:	jsr Adiv16			;($C2BF)Move lower health digit to 4 LSBs.
LE0E5:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE0E8:	ldy EndTimerHi			;
LE0EB:	iny				;Is Samus in escape sequence?-->
LE0EC:	bne ++				;If so, branch.
LE0EE:	ldy MaxMissiles			;
LE0F1:	beq +				;Don't show missile count if Samus has no missile containers.

;Display 3-digit missile count.
LE0F3:	lda MissileCount		;
LE0F6:	jsr HexToDec			;($E198)Convert missile hex count to decimal cout.
LE0F9:	lda $02				;Upper digit.
LE0FB:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE0FE:	lda $01				;Middle digit.
LE100:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE103:	lda $00				;Lower digit.
LE105:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE108:	bne +++				;Branch always.

;Samus has no missiles, erase missile sprite.
LE10A:*	lda #$FF			;"Blank" tile.
LE10C:	cpx #$F4			;If at last 3 sprites, branch to skip.
LE10E:	bcs ++				;
LE110:	sta Sprite03RAM+1,x		;Erase left half of missile.
LE113:	cpx #$F0			;If at last 4 sprites, branch to skip.
LE115:	bcs ++				;
LE117:	sta Sprite04RAM+1,x		;Erase right half of missile.
LE11A:	bne ++				;Branch always.

;Display 3-digit end sequence timer.
LE11C:*	lda EndTimerHi			;
LE11F:	jsr Adiv16			;($C2BF)Upper timer digit.
LE122:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE125:	lda EndTimerHi			;
LE128:	and #$0F			;Middle timer digit.
LE12A:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE12D:	lda EndTimerLo			;
LE130:	jsr Adiv16			;($C2BF)Lower timer digit.
LE133:	jsr SPRWriteDigit		;($E173)Display digit on screen.
LE136:	lda #$58			;"TI" sprite(left half of "TIME").
LE138:	sta Sprite00RAM+1,x		;
LE13B:	inc Sprite00RAM+2,x		;Change color of sprite.
LE13E:	cpx #$FC			;If at last sprite, branch to skip.
LE140:	bcs +				;
LE142:	lda #$59			;"ME" sprite(right half of "TIME").
LE144:	sta Sprite01RAM+1,x		;
LE147:	inc Sprite01RAM+2,x		;Change color of sprite.

LE14A:*	ldx SpritePagePos		;Restore initial sprite page pos.
LE14C:	lda TankCount			;
LE14F:	beq ++				;Branch to exit if Samus has no energy tanks.

;Display full/empty energy tanks.
LE151:	sta $03				;Temp store tank count.
LE153:	lda #$40			;X coord of right-most energy tank.
LE155:	sta $00				;Energy tanks are drawn from right to left.
LE157:	ldy #$6F			;"Full energy tank" tile.
LE159:	lda HealthHi			;
LE15C:	jsr Adiv16			;($C2BF)/16. A contains # of full energy tanks.
LE15F:	sta $01				;Storage of full tanks.
LE161:	bne AddTanks			;Branch if at least 1 tank is full.
LE163:	dey				;Else switch to "empty energy tank" tile.

AddTanks:
LE164:	jsr AddOneTank			;($E17B)Add energy tank to display.
LE167:	dec $01				;Any more full energy tanks left?-->
LE169:	bne +				;If so, then branch.-->
LE16B:	dey				;Otherwise, switch to "empty energy tank" tile.
LE16C:*	dec $03				;done all tanks?-->
LE16E:	bne AddTanks			;if not, loop to do another.

LE170:	stx SpritePagePos		;Store new sprite page position.
LE172:*	rts				;

;----------------------------------------[Sprite write digit ]---------------------------------------

;A=value in range 0..9. #$A0 is added to A(the number sprites begin at $A0), and the result is stored
;as the tile # for the sprite indexed by X.

SPRWriteDigit:
LE173:	ora #$A0			;#$A0 is index into pattern table for numbers.
LE175:	sta Sprite00RAM+1,x		;Store proper nametable pattern in sprite RAM.
LE178:	jmp Xplus4			;Find next sprite pattern table byte.

;----------------------------------[ Add energy tank to display ]------------------------------------

;Add energy tank to Samus' data display.

AddOneTank:
LE17B:	lda #$17			;Y coord-1.
LE17D:	sta Sprite00RAM,x		;
LE180:	tya				;Tile value.
LE181:	sta Sprite00RAM+1,x		;
LE184:	lda #$01			;Palette #.
LE186:	sta Sprite00RAM+2,x		;
LE189:	lda $00				;X coord.
LE18B:	sta Sprite00RAM+3,x		;
LE18E:	sec				;
LE18F:	sbc #$0A			;Find x coord of next energy tank.
LE191:	sta $00				;

;-----------------------------------------[ Add 4 to x ]---------------------------------------------

Xplus4:
LE193:	inx				;
LE194:	inx				;
LE195:	inx				;Add 4 to value stored in X.
LE196:	inx				;
LE197:	rts				;

;------------------------------------[ Convert hex to decimal ]--------------------------------------

;Convert 8-bit value in A to 3 decimal digits. Upper digit put in $02, middle in $01 and lower in $00.

HexToDec:
LE198:	ldy #100			;Find upper digit.
LE19A:	sty $0A				;
LE19C:	jsr GetDigit			;($E1AD)Extract hundreds digit.
LE19F:	sty $02				;Store upper digit in $02.
LE1A1:	ldy #10				;Find middle digit.
LE1A3:	sty $0A				;
LE1A5:	jsr GetDigit			;($E1AD)Extract tens digit.
LE1A8:	sty $01				;Store middle digit in $01.
LE1AA:	sta $00				;Store lower digit in $00
LE1AC:	rts				;

GetDigit:
LE1AD:	ldy #$00			;
LE1AF:	sec				;
LE1B0:*	iny				;
LE1B1:	sbc $0A				;Loop and subtract value in $0A from A until carry flag-->
LE1B3:	bcs -				;is not set.  The resulting number of loops is the decimal-->
LE1B5:	dey				;number extracted and A is the remainder.
LE1B6:	adc $0A				;
LE1B8:	rts				;

;-------------------------------------[ Status bar sprite data ]-------------------------------------

;Sprite data for Samus' data display

DataDisplayTbl:
LE1B9:	.byte $21,$A0,$01,$30		;Upper health digit.
LE1BD:	.byte $21,$A0,$01,$38		;Lower health digit.
LE1C1:	.byte $2B,$FF,$01,$28		;Upper missile digit.
LE1C5:	.byte $2B,$FF,$01,$30		;Middle missile digit.
LE1C9:	.byte $2B,$FF,$01,$38		;Lower missile digit.
LE1CD:	.byte $2B,$5E,$00,$18		;Left half of missile.
LE1D1:	.byte $2B,$5F,$00,$20		;Right half of missile.
LE1D5:	.byte $21,$76,$01,$18		;E
LE1D9:	.byte $21,$7F,$01,$20		;N
LE1DD:	.byte $21,$3A,$00,$28		;..

;-------------------------------------------[ Bit scan ]---------------------------------------------

;This function takes the value stored in A and right shifts it until a set bit is encountered.
;Once a set bit is encountered, the function exits and returns the bit number of the set bit.
;The returned value is stored in A. 

BitScan:
LE1E1:	stx $0E				;Save X.
LE1E3:	ldx #$00			;First bit is bit 0.
LE1E5:*	lsr				;Transfer bit to carry flag.
LE1E6:	bcs +				;If the shifted bit was 1, Branch out of loop.
LE1E8:	inx				;Increment X to keep of # of bits checked.
LE1E9:	cpx #$08			;Have all 8 bit been tested?-->
LE1EB:	bne -				;If not, branch to check the next bit.
LE1ED:*	txa				;Return which bit number was set.
LE1EE:	ldx $0E				;Restore X.
LE1F0:*	rts				;

;------------------------------------------[ Scroll door ]-------------------------------------------

;Scrolls the screen if Samus is inside a door.

ScrollDoor:
LE1F1:	ldx DoorStatus			;
LE1F3:	beq -				;Exit if Samus isn't in a door.
LE1F5:	dex				;
LE1F6:	bne +				;Not in right door. branch to check left door.
LE1F8:	jsr ScrollRight			;($E6D2)DoorStatus=1, scroll 1 pixel right.
LE1FB:	jmp ++				;Jump to check if door scroll is finished.

LE1FE:*	dex				;Check if in left door.
LE1FF:	bne ++				;
LE201:	jsr ScrollLeft			;($E6A7)DoorStatus=2, scroll 1 pixel left.
LE204:*	ldx ScrollX			;Has x scroll offset reached 0?-->
LE206:	bne Exit15			;If not, branch to exit.

;Scrolled one full screen, time to exit door.
LE208:	ldx #$05			;Samus is exiting the door.
LE20A:	bne DoOneDoorScroll		;Branch always.

LE20C:*	dex				;
LE20D:	bne +				;Check if need to scroll down to center door.
LE20F:	jsr ScrollDown			;($E519)DoorStatus=3, scroll 1 pixel down.
LE212:	jmp ++				;Jump to check y scrolling value.
LE215:*	dex				;
LE216:	bne Exit15			;Check if need to scroll up to center door.
LE218:	jsr ScrollUp			;($E4F1)DoorStatus=4, scroll 1 pixel up.

VerticalRoomCentered:
LE21B:*	ldx ScrollY			;Has room been centered on screen?-->
LE21D:	bne Exit15			;If not, branch to exit.
LE21F:	stx DoorOnNameTable3		;
LE221:	stx DoorOnNameTable0		;Erase door nametable data.
LE223:	inx				;X=1.
LE224:	lda ObjectX			;Did Samus enter in the right hand door?-->
LE227:	bmi ++				;If so, branch.
LE229:	inx				;X=2. Samus is in left door.
LE22A:	bne ++				;Branch always.

;This function is called once after door scrolling is complete.

DoOneDoorScroll:
LE22C:	lda #$20			;Set DoorDelay to 32 frames(comming out of door).
LE22E:	sta DoorDelay			;
LE230:	lda SamusDoorData		;Check if scrolling should be toggled.
LE232:	jsr Amul8			;($C2C6)*8. Is door not to toggle scrolling(item room,-->
LE235:	bcs +				;bridge room, etc.)? If so, branch to NOT toggle scrolling.
LE237:	ldy DoorScrollStatus		;If comming from vertical shaft, skip ToggleScroll because-->
LE239:	cpy #$03			;the scroll was already toggled after room was centered-->
LE23B:	bcc ++				;by the routine just above.
LE23D:*	lda #$47			;Set mirroring for vertical mirroring(horz scrolling).
LE23F:	bne ++				;Branch always.

LE241:*	jsr ToggleScroll		;($E252)Toggle scrolling and mirroring.
LE244:*	sta MirrorCntrl			;Store new mirror control data.
LE246:	stx DoorStatus			;DoorStatus=5. Done with door scrolling.

Exit15:
LE248:	rts				;Exit for several routines above.

;------------------------------------[ Toggle Samus nametable ]--------------------------------------

ToggleSamusHi:
LE249:	lda ObjectHi			;
LE24C:	eor #$01			;Change Samus' current nametable from one to the other.
LE24E:	sta ObjectHi			;
LE251:	rts				;

;-------------------------------------------[ Toggle scroll ]----------------------------------------

;Toggles both mirroring and scroll direction when Samus has moved from
;a horizontal shaft to a vertical shaft or vice versa.

ToggleScroll:
LE252:	lda ScrollDir			;
LE254:	eor #$03			;Toggle scroll direction.
LE256:	sta ScrollDir			;
LE258:	lda MirrorCntrl			;Toggle mirroring.
LE25A:	eor #$08			;
LE25C:	rts				;

;----------------------------------------[ Is Samus in lava ]----------------------------------------

;The following function checks to see if Samus is in lava.  If she is, the carry bit is cleared,
;if she is not, the carry bit is set. Samus can only be in lava if in a horizontally scrolling
;room. If Samus is 24 pixels or less away from the bottom of the screen, she is considered to be
;in lava whether its actually there or not.

IsSamusInLava:
LE25D:  lda #$01			;
LE25F:	cmp ScrollDir			;Set carry bit(and exit) if scrolling up or down.
LE261:	bcs +				;
LE263:	lda #$D8			;If Samus is Scrolling left or right and within 24 pixels-->
LE265:	cmp ObjectY			;of the bottom of the screen, she is in lava. Clear carry bit.
LE268:*	rts				;

;----------------------------------[ Check lava and movement routines ]------------------------------

LavaAndMoveCheck:
LE269:  lda ObjAction			;
LE26C:	cmp #sa_Elevator		;Is Samus on elevator?-->
LE26E:	beq +				;If so, branch.
LE270:	cmp #sa_Dead			;Is Samus Dead-->
LE272:	bcs -				;If so, branch to exit.
LE274:*	jsr IsSamusInLava		;($E25D)Clear carry flag if Samus is in lava.
LE277:	ldy #$FF			;Assume Samus not in lava.
LE279:	bcs ++++			;Samus not in lava so branch.

;Samus is in lava.
LE27B:	sty DamagePushDirection		;Don't push Samus from lava damage.
LE27D:	jsr ClearHealthChange		;($F323)Clear any pending health changes to Samus.
LE280:	lda #$32			;
LE282:	sta SamusBlink			;Make Samus blink.
LE284:	lda FrameCount			;
LE286:	and #$03			;Start the jump SFX every 4th frame while in lava.
LE288:	bne +				;
LE28A:	jsr SFX_SamusJump		;($CBAC)Initiate jump SFX.
LE28D:*	lda FrameCount			;
LE28F:	lsr				;This portion of the code causes Samus to be damaged by-->
LE290:	and #$03			;lava twice every 8 frames if she does not have the varia-->
LE292:	bne ++				;but only once every 8 frames if she does.
LE294:	lda SamusGear			;
LE297:	and #gr_VARIA			;Does Samus have the Varia?-->
LE299:	beq +				;If not, branch.
LE29B:	bcc ++				;Samus has varia. Carry set every other frame. Half damage.
LE29D:*	lda #$07			;
LE29F:	sta HealthLoChange		;Samus takes lava damage.
LE2A1:	jsr SubtractHealth		;($CE92)
LE2A4:*	ldy #$00			;Prepare to indicate Samus is in lava.
LE2A6:*	iny				;Set Samus lava status.
LE2A7:	sty SamusInLava			;

SamusMoveVertically:
LE2A9:	jsr VertAccelerate		;($E37A)Calculate vertical acceleration.
LE2AC:	lda ObjectY			;
LE2AF:	sec				;
LE2B0:	sbc ScrollY			;Calculate Samus' screen y position.
LE2B2:	sta SamusScrY			;
LE2B4:	lda $00				;Load temp copy of vertical speed.
LE2B6:	bpl ++++			;If Samus is moving downwards, branch.

LE2B8:	jsr TwosCompliment		;($C3D4)Get twos compliment of vertical speed.
LE2BB:	ldy SamusInLava			;Is Samus in lava?
LE2BD:	beq +				;If not, branch,-->
LE2BF:	lsr				;else cut vertical speed in half.
LE2C0:	beq SamusMoveHorizontally	;($E31A)Branch if no vertical mvmnt to Check left/right mvmnt.

;Samus is moving upwards.
LE2C2:*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
LE2C4:*	jsr MoveSamusUp			;($E457)Attempt to move Samus up 1 pixel.
LE2C7:	bcs +				;Branch if Samus successfully moved up 1 pixel.

LE2C9:	sec				;Samus blocked upwards. Divide her speed by 2 and set the
LE2CA:	ror ObjVertSpeed		;MSB to reverse her direction of travel.
LE2CD:	ror VertCntrLinear		;
LE2D0:	jmp SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

LE2D3:*	dec ObjectCounter		;1 pixel movement is complete.
LE2D5:	bne --				;Branch if Samus needs to be moved another pixel.

;Samus is moving downwards.
LE2D7:*	beq SamusMoveHorizontally	;($E31A)Branch if no vertical mvmnt to Check left/right mvmnt.
LE2D9:	ldy SamusInLava			;Is Samus in lava?
LE2DB:	beq +				;If not, branch,-->
LE2DD:	lsr				;Else reduce Samus speed by 75%(divide by 4).
LE2DE:	lsr				;
LE2DF:	beq SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

LE2E1:*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
LE2E3:*	jsr MoveSamusDown		;($E4A3)Attempt to move Samus 1 pixel down.
LE2E6:	bcs +++				;Branch if Samus successfully moved down 1 pixel.

;Samus bounce after hitting the ground in ball form.
LE2E8:	lda ObjAction			;
LE2EB:	cmp #sa_Roll			;Is Samus rolled into a ball?-->
LE2ED:	bne +				;If not, branch.
LE2EF:	lsr ObjVertSpeed		;Divide verticle speed by 2.
LE2F2:	beq ++				;Speed not fast enough to bounce. branch to skip.
LE2F4:	ror VertCntrLinear		;Move carry bit into MSB to reverse Linear counter.
LE2F7:	lda #$00			;
LE2F9:	sec				;
LE2FA:	sbc VertCntrLinear		;Subtract linear counter from 0 and save the results.-->
LE2FD:	sta VertCntrLinear		;Carry will be cleared.
LE300:	lda #$00			;
LE302:	sbc ObjVertSpeed		;Subtract vertical speed from 0. this will reverse the-->
LE305:	sta ObjVertSpeed		;vertical direction of travel(bounce up).
LE308:	jmp SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

;Samus has hit the ground after moving downwards. 
LE30B:*	jsr SFX_SamusWalk		;($CB96)Play walk SFX.
LE30E:*	jsr StopVertMovement		;($D147)Clear vertical movement data.
LE311:	sty SamusGravity		;Clear Samus gravity value.
LE314:	beq SamusMoveHorizontally	;($E31A)Attempt to move Samus left/right.

LE316:*	dec ObjectCounter		;1 pixel movement is complete.
LE318:	bne ----			;Branch if Samus needs to be moved another pixel.

SamusMoveHorizontally:
LE31A:  jsr HorzAccelerate		;($E3E5)Horizontally accelerate Samus.
LE31D:	lda ObjectX			;
LE320:	sec				;Calculate Samus' x position on screen.
LE321:	sbc ScrollX			;
LE323:	sta SamusScrX			;Save Samus' x position.
LE325:	lda $00				;Load Samus' current horizontal speed.
LE327:	bpl +++				;Branch if moving right.

;Samus is moving left.
LE329:	jsr TwosCompliment		;($C3D4)Get twos compliment of horizontal speed.
LE32C:	ldy SamusInLava			;Is Samus in lava?-->
LE32E:	beq +				;If not, branch,-->
LE330:	lsr				;else cut horizontal speed in half.
LE331:	beq Exit10			;Branch to exit if Samus not going to move this frame.

LE333:*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
LE335:*	jsr MoveSamusLeft		;($E626)Attempt to move Samus 1 pixel to the left.
LE338:	jsr CheckStopHorzMvmt		;($E365)Check if horizontal movement needs to be stopped.
LE33B:	dec ObjectCounter		;1 pixel movement is complete.
LE33D:	bne -				;Branch if Samus needs to be moved another pixel.

LE33F:	lda SamusDoorData		;Has Samus entered a door?-->
LE341:	beq Exit10			;If not, branch to exit.
LE343:	lda #$01			;Door leads to the left.
LE345:	bne ++++			;Branch always.

;Samus is moving right.
LE347:*	beq Exit10			;Branch to exit if Samus not moving horizontally.
LE349:	ldy SamusInLava			;Is Samus in lava?-->
LE34B:	beq +				;If not, branch,-->
LE34D:	lsr				;else cut horizontal speed in half.
LE34E:	beq Exit10			;Branch to exit if Samus not going to move this frame.

LE350:*	sta ObjectCounter		;Store number of pixels to move Samus this frame.
LE352:*	jsr MoveSamusRight		;($E668)Attempt to move Samus 1 pixel to the right.
LE355:	jsr CheckStopHorzMvmt		;($E365)Check if horizontal movement needs to be stopped.
LE358:	dec ObjectCounter		;1 pixel movement is complete.
LE35A:	bne -				;Branch if Samus needs to be moved another pixel.

LE35C:	lda SamusDoorData		;Has Samus entered a door?-->
LE35E:	beq Exit10			;If not, branch to exit.
LE360:	lda #$00			;
LE362:*	sta SamusDoorDir		;Door leads to the right.

Exit10:
LE364:	rts				;Exit for routines above and below.

CheckStopHorzMvmt:
LE365:  bcs Exit10			;Samus moved successfully. Branch to exit.
LE367:	lda #$01			;Load counter with #$01 so this function will not be-->
LE369:	sta ObjectCounter		;called again.
LE36C:	lda SamusGravity		;Is Samus on the ground?-->
LE36E:	bne Exit10			;If not, branch to exit.
LE370:	lda ObjAction			;
LE373:	cmp #sa_Roll			;Is Samus rolled into a ball?-->
LE375:	beq Exit10			;If so, branch to exit.
LE377:	jmp StopHorzMovement		;($CF55)Stop horizontal movement or play walk SFX if stopped.

;-------------------------------------[ Samus vertical acceleration ]--------------------------------

;The following code accelerates/decelerates Samus vertically.  There are 4 possible values for
;gravity used in the acceleration calculation. The higher the number, the more intense the gravity.
;The possible values for gravity are as follows:
;#$38-When Samus has been hit by an enemy.
;#$1A-When Samus is falling.
;#$18-Jump without high jump boots.
;#$12-Jump with high jump boots.

VertAccelerate:
LE37A:  lda SamusGravity		;Is Samus rising or falling?-->
LE37D:	bne ++				;Branch if yes.
LE37F:	lda #$18			;
LE381:	sta SamusHorzSpeedMax		;Set Samus maximum running speed.
LE384:	lda ObjectY			;
LE387:	clc				;
LE388:	adc ObjRadY			;Check is Samus is obstructed downwards on y room-->
LE38B:	and #$07			;positions divisible by 8(every 8th pixel).
LE38D:	bne +				;
LE38F:	jsr CheckMoveDown		;($E7AD)Is Samus obstructed downwards?-->
LE392:	bcc ++				;Branch if yes.
LE394:*	jsr SamusOnElevatorOrEnemy	;($D976)Calculate if Samus standing on elevator or enemy.
LE397:	lda SamusOnElevator		;Is Samus on an elevator?-->
LE39A:	bne +				;Branch if yes.
LE39C:	lda OnFrozenEnemy		;Is Samus standing on a frozen enemy?-->
LE39E:	bne +				;Branch if yes.
LE3A0:	lda #$1A			;Samus is falling. Store falling gravity value.
LE3A2:	sta SamusGravity		;

LE3A5:*	ldx #$05			;Load X with maximum downward speed.
LE3A7:	lda VertCntrLinear		;
LE3AA:	clc				;The higher the gravity, the faster this addition overflows-->
LE3AB:	adc SamusGravity		;and the faster ObjVertSpeed is incremented.
LE3AE:	sta VertCntrLinear		;
LE3B1:	lda ObjVertSpeed		;Every time above addition sets carry bit, ObjVertSpeed is-->
LE3B4:	adc #$00			;incremented. This has the effect of speeding up a fall-->
LE3B6:	sta ObjVertSpeed		;and slowing down a jump.
LE3B9:	bpl +				;Branch if Samus is moving downwards.

;Check if maximum upward speed has been exceeded. If so, prepare to set maximum speed.
LE3BB:	lda #$00			;
LE3BD:	cmp VertCntrLinear		;Sets carry bit.
LE3C0:	sbc ObjVertSpeed		;Subtract ObjVertSpeed to see if maximum speed has-->
LE3C3:	cmp #$06			;been exceeded.
LE3C5:	ldx #$FA			;Load X with maximum upward speed.
LE3C7:	bne ++				;Branch always.

;Check if maximum downward speed has been reached. If so, prepare to set maximum speed.
LE3C9:*	cmp #$05			;Has maximum downward speed been reached?-->
LE3CB:*	bcc +				;If not, branch.

;Max verticle speed reached or exceeded. Adjust Samus verticle speed to max.
LE3CD:	jsr StopVertMovement		;($D147)Clear verticle movement data.
LE3D0:	stx ObjVertSpeed		;Set Samus vertical speed to max.

;This portion of the function creates an exponential increase/decrease in verticle speed. This is the
;part of the function that does all the work to make Samus' jump seem natural.
LE3D3:*	lda VertCntrNonLinear		;
LE3D6:	clc				;This function adds itself plus the linear verticle counter-->
LE3D7:	adc VertCntrLinear		;onto itself every frame.  This causes the non-linear-->
LE3DA:	sta VertCntrNonLinear		;counter to increase exponentially.  This function will-->
LE3DD:	lda #$00			;cause Samus to reach maximum speed first in most-->
LE3DF:	adc ObjVertSpeed		;situations before the linear counter.
LE3E2:	sta $00				;$00 stores temp copy of current verticle speed.
LE3E4:	rts				;

;----------------------------------------------------------------------------------------------------

HorzAccelerate:
LE3E5:  lda SamusHorzSpeedMax
	jsr Amul16       ; * 16
	sta $00
	sta $02
	lda SamusHorzSpeedMax
	jsr Adiv16       ; / 16
	sta $01
	sta $03

	lda HorzCntrLinear
	clc
	adc SamusHorzAccel
	sta HorzCntrLinear
	tax
	lda #$00
	bit SamusHorzAccel
	bpl +				;Branch if Samus accelerating to the right.

	lda #$FF

*       adc ObjHorzSpeed
	sta ObjHorzSpeed
	tay
	bpl +				;Branch if Samus accelerating to the right.

	lda #$00
	sec
	sbc HorzCntrLinear
	tax
	lda #$00
	sbc ObjHorzSpeed
	tay
	jsr LE449

*       cpx $02
	tya
	sbc $03
	bcc +
	lda $00
	sta HorzCntrLinear
	lda $01
	sta ObjHorzSpeed
*       lda HorzCntrNonLinear
	clc
	adc HorzCntrLinear
	sta HorzCntrNonLinear
	lda #$00
	adc ObjHorzSpeed
	sta $00				;$00 stores temp copy of current horizontal speed.
	rts				;

UnknownE449:
LE449:  lda #$00
	sec
	sbc $00
	sta $00
	lda #$00
	sbc $01
	sta $01
	rts

;----------------------------------------------------------------------------------------------------

;Attempt to move Samus one pixel up.

MoveSamusUp:
LE457:	lda ObjectY			;Get Samus' y position in room.
	sec				;
	sbc ObjRadY			;Subtract Samus' vertical radius.
LE45E:	and #$07			;Check if result is a multiple of 8. If so, branch to-->
LE460:	bne +				;Only call crash detection every 8th pixel.
LE462:	jsr CheckMoveUp			;($E7A2)Check if Samus obstructed UPWARDS.-->
	bcc +++++++			;If so, branch to exit(can't move any further).
*       lda ObjAction			;
	cmp #sa_Elevator		;Is Samus riding elevator?-->
	beq +				;If so, branch.
	jsr SamusOnElevatorOrEnemy	;($D976)Calculate if Samus standing on elevator or enemy.
	lda SamusHit
	and #$42
	cmp #$42
	clc
	beq ++++++
*       lda SamusScrY
	cmp #$66	; reached up scroll limit?
	bcs +	   ; branch if not
	jsr ScrollUp
	bcc ++
*       dec SamusScrY
*	lda ObjectY
	bne ++
	lda ScrollDir
	and #$02
	bne +
	jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
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
	lda ScrollDir
	beq +
	cmp #$01
	bne ++++
	dec ScrollDir
	lda ScrollY
	beq +
	dec MapPosY
*       ldx ScrollY
	bne +
	dec MapPosY     ; decrement MapY
	jsr GetRoomNum  ; put room # at current map pos in $5A
	bcs ++   ; if function returns CF = 1, moving up is not possible
	jsr LE9B7       ; switch to the opposite Name Table
	ldx #240	; new Y coord
*	dex
	jmp LE53F

*	inc MapPosY
*	sec
	rts

; Attempt to scroll DOWN

ScrollDown:
	ldx ScrollDir
	dex
	beq +
	bpl +++++
	inc ScrollDir
	lda ScrollY
	beq +
	inc MapPosY
*       lda ScrollY
	bne +
	inc MapPosY     ; increment MapY
	jsr GetRoomNum  ; put room # at current map pos in $5A
	bcs +++   ; if function returns CF = 1, moving down is not possible
*       ldx ScrollY
	cpx #239
	bne +
	jsr LE9B7       ; switch to the opposite Name Table
	ldx #$FF
*       inx
LE53F:  stx ScrollY
	jsr LE54A       ; check if it's time to update Name Table
	clc
	rts

*	dec MapPosY
*	sec
*       rts

LE54A:  jsr SetupRoom
	ldx RoomNumber
	inx
	bne -
	lda ScrollDir
	and #$02
	bne +
	jmp LE571
*       jmp LE701

; Table

Table11:
	.byte $07
	.byte $00

;---------------------------------[ Get PPU and RoomRAM addresses ]----------------------------------

PPUAddrs:
LE560:	.byte $20			;High byte of nametable #0(PPU).
LE561:	.byte $2C			;High byte of nametable #3(PPU)

WRAMAddrs:
LE562:	.byte $60			;High byte of RoomRAMA(cart RAM).
LE563:	.byte $64			;High byte of RoomRAMB(cart RAM).

GetNameAddrs:
LE564:	jsr GetNameTable		;($EB85)Get current name table number.
LE567:	and #$01			;Update name table 0 or 3.
LE569:	tay				;
LE56A:	lda PPUAddrs,y			;Get high PPU addr of nametable(dest).
LE56D:	ldx WRAMAddrs,y			;Get high cart RAM addr of nametable(src).
LE570:	rts				;

;----------------------------------------------------------------------------------------------------

; check if it's time to update nametable (when scrolling is VERTICAL)

LE571:  ldx ScrollDir
	lda ScrollY
	and #$07	; compare value = 0 if ScrollDir = down, else 7
	cmp Table11,x
	bne --	   ; exit if not equal (no nametable update)

LE57C:  ldx ScrollDir			;
	cpx TempScrollDir		;Still scrolling same direction when room was loaded?-->
	bne --				;If not, branch to exit.
	lda ScrollY
	and #$F8	; keep upper 5 bits
	sta $00
	lda #$00
	asl $00
	rol
	asl $00
	rol

LE590:  sta $01	 ; $0001 = (ScrollY & 0xF8) << 2 = row offset
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
LE5E2:  ldx #$C0			;Low byte of First row of attribute table.
LE5E4:	lda RoomNumber			;
LE5E6:	cmp #$F2			;Is this the second pass through the routine?-->
LE5E8:	beq +				;If so, branch.
LE5EA:	ldx #$E0			;Low byte of second row of attribute table.
LE5EC:*	stx $00				;$0000=RoomRAM atrrib table starting address.
LE5EE:	stx $02				;$0002=PPU attrib table starting address.
LE5F0:	jsr GetNameAddrs		;($E564)Get name table addr and corresponding RoomRAM addr.
LE5F3:	ora #$03			;#$23 for attrib table 0, #$2F for attrib table 3.
LE5F5:	sta $03				;Store results.
LE5F7:	txa				;move high byte of RoomRAM to A.
LE5F8:	ora #$03			;#$63 for RoomRAMA, #$67 for RoomRAMB(Attrib tables).
LE5FA:	sta $01				;Store results.
LE5FC:	lda #$01			;
LE5FE:	sta PPUDataPending		;Data pending = YES.
LE600:	ldx PPUStrIndex			;Load current index into PPU strng to append data.
LE603:	lda $03				;Store high byte of starting address(attrib table).
LE605:	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
LE608:	lda $02				;Store low byte of starting address(attrib table).
LE60A:	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
LE60D:	lda #$20			;Length of data to write(1 row of attrib data).
LE60F:	sta $04				;
LE611:	jsr WritePPUByte		;($C36B)Write control byte. Horizontal write.
LE614:	ldy #$00			;Reset index into data string.
LE616:*	lda ($00),y			;Get data byte.
LE618:	jsr WritePPUByte		;($C36B)Put data byte into PPUDataString.
LE61B:	iny				;Increment to next attrib data byte.
LE61C:	dec $04				;
LE61E:	bne -				;Loop until all attrib data loaded into PPU.
LE620:	stx PPUStrIndex			;Store updated PPU string index.
LE623:	jsr EndPPUString		;($C376)Append end marker(#$00) and exit writing routines.

;----------------------------------------------------------------------------------------------------

; attempt to move Samus one pixel left

MoveSamusLeft:
LE626:	lda ObjectX
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
	dec MapPosX
*       lda ScrollX
	bne +
	dec MapPosX     ; decrement MapX
	jsr GetRoomNum  ; put room # at current map pos in $5A
	bcs ++  ; if function returns CF=1, scrolling left is not possible
	jsr LE9B7       ; switch to the opposite Name Table
*       dec ScrollX
	jsr LE54A       ; check if it's time to update Name Table
	clc
	rts

*	inc MapPosX
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
	inc MapPosX
*       lda ScrollX
	bne +
	inc MapPosX
	jsr GetRoomNum  ; put room # at current map pos in $5A
	bcs +++   ; if function returns CF=1, scrolling right is not possible
*       inc ScrollX
	bne +
	jsr LE9B7       ; switch to the opposite Name Table
*       jsr LE54A       ; check if it's time to update Name Table
	clc
	rts

*	dec MapPosX
*	sec
*       rts

Table02:
	.byte $07,$00

; check if it's time to update nametable (when scrolling is HORIZONTAL)

LE701:  ldx ScrollDir
	lda ScrollX
	and #$07	; keep lower 3 bits
	cmp Table02-2,x ; compare value = 0 if ScrollDir = right, else 7
	bne -	   ; exit if not equal (no nametable update)

LE70C:  ldx ScrollDir
	cpx TempScrollDir
	bne -
	lda ScrollX
	and #$F8	; keep upper five bits
	jsr Adiv8       ; / 8 (make 'em lower five)
	sta $00
	lda #$00
	jmp LE590

;---------------------------------------[ Get room number ]-------------------------------------------

;Gets room number at current map position. Sets carry flag if room # at map position is FF.
;If valid room number, the room number is stored in $5A.

GetRoomNum:
LE720:	lda ScrollDir			;
LE722:	lsr				;Branch if scrolling vertical.
LE723:	beq +				;

LE725:	rol				;Restore value of a
LE726:	adc #$FF			;A=#$01 if scrolling left, A=#$02 if scrolling right.
LE728:	pha				;Save A.
LE729:	jsr OnNameTable0		;($EC93)Y=1 if name table=0, Y=0 if name table=3.
LE72C:	pla				;Restore A.
LE72D:	and $006C,y			;
LE730:	sec				;
LE731:	bne +++++			;Can't load room, a door is in the way. This has the-->
					;effect of stopping the scrolling until Samus walks-->
					;through the door(horizontal scrolling only).

LE733:*	lda MapPosY			;Map pos y.
LE735:	jsr Amul16			;($C2C5)Multiply by 16.
LE738:	sta $00				;Store multiplied value in $00.
LE73A:	lda #$00			;
LE73C:	rol				;Save carry, if any.
LE73D:	rol $00				;Multiply value in $00 by 2.
LE73F:	rol				;Save carry, if any.
LE740:	sta $01				;
LE742:	lda $00				;
LE744:	adc MapPosX			;Add map pos X to A.
LE746:	sta $00				;Store result.
LE748:	lda $01				;
LE74A:	adc #$70			;Add #$7000 to result.
LE74C:	sta $01				;$0000 = (MapY*32)+MapX+#$7000.
LE74E:	ldy #$00			;
LE750:	lda ($00),y			;Load room number.
LE752:	cmp #$FF			;Is it unused?-->
LE754:	beq ++++			;If so, branch to exit with carry flag set.

LE756:	sta RoomNumber			;Store room number.

LE758:*	cmp $95D0,y			;Is it a special room?-->
LE75B:	beq +				;If so, branch to set flag to play item room music.
LE75D:	iny				;
LE75E:	cpy #$07			;
LE760:	bne -				;Loop until all special room numbers are checked.

LE762:	lda ItemRoomMusicStatus		;Load item room music status.
LE764:	beq ++				;Branch if not in special room.
LE766:	lda #$80			;Ptop playing item room music after next music start.
LE768:	bne ++				;Branch always.

LE76A:*	lda #$01			;Start item room music on next music start.
LE76C:*	sta ItemRoomMusicStatus		;
LE76E:	clc				;Clear carry flag. was able to get room number.
LE76F:*	rts				;

;-----------------------------------------------------------------------------------------------------

UnknownE770:
LE770:	ldx PageIndex
	lda EnRadY,x
	clc
	adc #$08
	jmp LE783

UnknownE77B:
LE77B:  ldx PageIndex
	lda #$00
	sec
	sbc EnRadY,x
LE783:  sta $02
	lda #$08
	sta $04
	jsr LE792
	lda EnRadX,x
	jmp LE7BD

LE792:  lda EnXRoomPos,x
	sta $09     ; X coord
	lda EnYRoomPos,x
	sta $08     ; Y coord
	lda EnNameTable,x
	sta $0B     ; hi coord
	rts

CheckMoveUp:
LE7A2:	ldx PageIndex
	lda ObjRadY,x
	clc
	adc #$08
	jmp +

CheckMoveDown:
	ldx PageIndex
	lda #$00
	sec
	sbc ObjRadY,x
*       sta $02
	jsr UnknownE8BE
	lda ObjRadX,x
LE7BD:  bne +
	sec
	rts

*       sta $03
	tay
	ldx #$00
	lda $09
	sec
	sbc $03
	and #$07
	beq +
	inx
*       jsr LE8CE
	sta $04
	jsr LE90F
	ldx #$00
	ldy #$08
	lda $00
LE7DE:  bne +++
	stx $06
	sty $07
	ldx $04

; object<-->background crash detection

LE7E6:  jsr MakeWRAMPtr ; set up ptr in $0004
	ldy #$00
	lda ($04),y     ; get tile value
	cmp #$4E
	beq LE81E
	jsr Unknown95C0
	jsr UnknownD651
	bcc Exit16      ; CF = 0 if tile # < $80 (solid tile)... CRASH!!!
	cmp #$A0	; is tile >= A0h? (walkable tile)
	bcs IsWalkableTile
	jmp IsBlastTile  ; tile is $80-$9F (blastable tiles)

IsWalkableTile:
	ldy IsSamus
	beq ++
    ; special case for Samus
	dey	     ; = 0
	sty SamusDoorData
	cmp #$A0	; crash with tile #$A0? (scroll toggling door)
	beq +
	cmp #$A1	; crash with tile #$A1? (horizontal scrolling door)
	bne ++
	inc SamusDoorData
*       inc SamusDoorData
*	dex
	beq +
	jsr LE98E
	jmp LE7E6

*	sec	     ; no crash
	Exit16:
	rts

LE81E:  ldx UpdatingProjectile
	beq ClcExit
	ldx #$06
*       lda $05
	eor $5D,x
	and #$04
	bne +++
	lda $04
	eor $5C,x
	and #$1F
	bne +++
	txa
	jsr Amul8       ; * 8
	ora #$80
	tay
	lda ObjAction,y
	beq +++
	lda $0307,y
	lsr
	bcs ++
	ldx PageIndex
	lda ObjAction,x
	eor #$0B
	beq +
	lda ObjAction,x
	eor #$04
	bne PlaySnd4
	lda AnimResetIndex,x
	eor #$91
	bne PlaySnd4
*       lda $0683
	ora #$02
	sta $0683
*	lda #$04
	sta $030A,y
	bne ClcExit
*	dex
	dex
	bpl ----
	lda $04
	jsr Adiv8       ; / 8
	and #$01
	tax
	inc $0366,x
	ClcExit:
	clc
	rts

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
*       sta $03
	jsr UnknownE8BE
	ldy ObjRadY,x
LE89B:  bne +
	sec
	rts

*       sty $02
	ldx #$00
	lda $08
	sec
	sbc $02
	and #$07
	beq +
	inx
*       jsr LE8CE
	sta $04
	jsr LE90F
	ldx #$08
	ldy #$00
	lda $01
	jmp LE7DE

UnknownE8BE:
LE8BE:  lda ObjectHi,x
	sta $0B
	lda ObjectY,x
	sta $08
	lda ObjectX,x
	sta $09
	rts

LE8CE:  eor #$FF
	clc
	adc #$01
	and #$07
	sta $04
	tya
	asl
	sec
	sbc $04
	bcs +
	adc #$08
*       tay
	lsr
	lsr
	lsr
	sta $04
	tya
	and #$07
	beq +
	inx
*       txa
	clc
	adc $04
	rts

UnknownE8F1:
LE8F1:  ldx PageIndex
	lda EnRadX,x
	clc
	adc #$08
	jmp LE904

UnknownE8FC:
LE8FC:  ldx PageIndex
	lda #$00
	sec
	sbc EnRadX,x
LE904:  sta $03
	jsr LE792
	ldy EnRadY,x
	jmp LE89B

LE90F:  lda $02
	bpl ++
	jsr LE95F
	bcs +
	cpx #$F0
	bcc +++
*       txa
	adc #$0F
	jmp LE934

*	jsr LE95F
	lda $08
	sec
	sbc $02
	tax
	and #$07
	sta $00
	bcs +
	txa
	sbc #$0F
LE934:  tax
	lda ScrollDir
	and #$02
	bne +
	inc $0B
*	stx $02
	ldx #$00
	lda $03
	bmi +
	dex
*       lda $09
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
*       rts

LE95F:  lda $08
	sec
	sbc $02
	tax
	and #$07
	sta $00
	rts

; MakeWRAMPtr
; ===========
; Makes pointer to WRAM nametable based on object's coordinates.
; In: $02 = ObjectY, $03 = ObjectX, $0B = ObjectHi
; Out: $04 = WRAM pointer

	MakeWRAMPtr:
	lda #$18
	sta $05
	lda $02	 ; ObjectY
	and #$F8	; keep upper 5 bits
	asl
	rol $05
	asl
	rol $05
	sta $04
	lda $03	 ; ObjectX
	lsr
	lsr
	lsr	   ; A = ObjectX / 8
	ora $04
	sta $04
	lda $0B	 ; ObjectYHi
	asl
	asl	   ; A = ObjectYHi * 4
	and #$04
	ora $05
	sta $05
	rts

LE98E:  lda $02
	clc
	adc $06
	sta $02
	cmp #$F0
	bcc +
	adc #$0F
	sta $02
	lda ScrollDir
	and #$02
	bne +
	inc $0B
*       lda $03
	clc
	adc $07
	sta $03
	bcc +
	lda ScrollDir
	and #$02
	beq +
	inc $0B
*       rts

LE9B7:  lda PPUCNT0ZP
	eor #$03
	sta PPUCNT0ZP
	rts

IsBlastTile:
	ldy UpdatingProjectile
	beq Exit18
UnknownE9C2:
LE9C2:  tay
	jsr $95BD
	cpy #$98
	bcs +++++
; attempt to find a vacant tile slot
	ldx #$C0
*       lda TileRoutine,x
	beq +	   ; 0 = free slot
	jsr Xminus16
	bne -
	lda TileRoutine,x
	bne ++++	 ; no more slots, can't blast tile
*       inc TileRoutine,x
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
*       tya
	clc
	adc #$10
	and #$3C
	lsr
*	lsr
	sta TileType,x
*	clc
Exit18: rts

;------------------------------------------[ Select room RAM ]---------------------------------------

SelectRoomRAM:
LEA05:	jsr GetNameTable		;($EB85)Find name table to draw room on.
LEA08:	asl				;
LEA09:	asl				;
LEA0A:	ora #$60			;A=#$64 for name table 3, A=#$60 for name table 0.
LEA0C:	sta CartRAMPtr+1		;
LEA0E:	lda #$00			;
LEA10:	sta CartRAMPtr			;Save two byte pointer to start of proper room RAM.
LEA12:	rts				;

;------------------------------------[ write attribute table data ]----------------------------------

AttribTableWrite:
LEA13:*	lda RoomNumber			;
LEA15:	and #$0F			;Determine what row of PPU attribute table data, if any,-->
LEA17:	inc RoomNumber			;to load from RoomRAM into PPU.
LEA19:	jsr ChooseRoutine		;

;The following table is used by the code above to determine when to write to the PPU attribute table.

LEA1c:	.word ExitSub			;($C45C)Rts.
LEA1E:	.word WritePPUAttribTbl		;($E5E2)Write first row of PPU attrib data.
LEA20:	.word ExitSub			;($C45C)Rts.
LEA22:	.word WritePPUAttribTbl		;($E5E2)Write second row of PPU attrib data.
LEA24:	.word RoomFinished		;($EA26)Finished writing attribute table data.

;-----------------------------------[ Finished writing room data ]-----------------------------------

RoomFinished:
LEA26:  lda #$FF			;No more tasks to perform on current room.-->
LEA28:	sta RoomNumber			;Set RoomNumber to #$FF.
LEA2A:*	rts				;

;------------------------------------------[ Setup room ]--------------------------------------------

SetupRoom:
LEA2B:	lda RoomNumber			;Room number.
LEA2D:	cmp #$FF			;
LEA2F:	beq -				;Branch to exit if room is undefined.
LEA31:	cmp #$FE			;
LEA33:	beq +				;Branch if empty place holder byte found in room data.
LEA35:	cmp #$F0			;
LEA37:	bcs --				;Branch if time to write PPU attribute table data.
LEA39:	jsr UpdateRoomSpriteInfo	;($EC9B)Update which sprite belongs on which name table.

LEA3C:	jsr ScanForItems		;($ED98)Set up any special items.
LEA3F:	lda RoomNumber			;Room number to load.
LEA41:	asl				;*2(for loading address of room pointer).
LEA42:	tay				;
LEA43:	lda (RoomPtrTable),y		;Low byte of 16-bit room pointer.-->
LEA45:	sta RoomPtr			;Base copied from $959A to $3B.
LEA47:	iny				;
LEA48:	lda (RoomPtrTable),y		;High byte of 16-bit room pointer.-->
LEA4A:	sta RoomPtr+1			;Base copied from $959B to $3C.
LEA4C:	ldy #$00			;
LEA4E:	lda (RoomPtr),y			;First byte of room data.
LEA50:	sta RoomPal			;store initial palette # to fill attrib table with.
LEA52:	lda #$01			;
LEA54:	jsr AddToRoomPtr		;($EAC0)Increment room data pointer.
LEA57:	jsr SelectRoomRAM		;($EA05)Determine where to draw room in RAM, $6000 or $6400.
LEA5A:	jsr InitTables			;($EFF8)clear Name Table & do initial Attrib table setup.
LEA5D:*	jmp DrawRoom			;($EAAA)Load room contents into room RAM.

;---------------------------------------[ Draw room object ]-----------------------------------------

DrawObject:
LEA60:	sta $0E				;Store object position byte(%yyyyxxxx).
LEA62:	lda CartRAMPtr			;
LEA64:	sta CartRAMWorkPtr		;Set the working pointer equal to the room pointer-->
LEA66:	lda CartRAMPtr+1		;(start at beginning of the room).
LEA68:	sta CartRAMWorkPtr+1		;
LEA6A:	lda $0E				;Reload object position byte.
LEA6C:	jsr Adiv16			;($C2BF)/16. Lower nibble contains object y position.-->
LEA6F:	tax				;Transfer it to X, prepare for loop.
LEA70:	beq +++				;Skip y position calculation loop as y position=0 and-->
					;does not need to be calculated.
LEA72:*	lda CartRAMWorkPtr		;LoW byte of pointer working in room RAM.
LEA74:	clc				;
LEA75:	adc #$40			;Advance two rows in room RAM(one y unit).
LEA77:	sta CartRAMWorkPtr		;
LEA79:	bcc +				;If carry occurred, increment high byte of pointer-->
LEA7B:	inc CartRAMWorkPtr+1		;in room RAM.
LEA7D:*	dex				;
LEA7E:	bne --				;Repeat until at desired y position(X=0).

LEA80:*	lda $0E				;Reload object position byte.
LEA82:	and #$0F			;Remove y position upper nibble.
LEA84:	asl				;Each x unit is 2 tiles.
LEA85:	adc CartRAMWorkPtr		;
LEA87:	sta CartRAMWorkPtr		;Add x position to room RAM work pointer.
LEA89:	bcc +				;If carry occurred, increment high byte of room RAM work-->
LEA8B:	inc CartRAMWorkPtr+1		;pointer, else branch to draw object.

;CartRAMWorkPtr now points to the object's starting location (upper left corner)
;on the room RAM which will eventually be loaded into a name table.

LEA8D:*	iny				;Move to the next byte of room data which is-->
LEA8E:	lda (RoomPtr),y			;the index into the structure pointer table.
LEA90:	tax				;Transfer structure pointer index into X.
LEA91:	iny				;Move to the next byte of room data which is-->
LEA92:	lda (RoomPtr),y			;the attrib table info for the structure.
LEA94:	sta ObjectPal			;Save attribute table info.
LEA96:	txa				;Restore structure pointer to A.
LEA97:	asl				;*2. Structure pointers are two bytes in size.
LEA98:	tay				;
LEA99:	lda (StructPtrTable),y		;Low byte of 16-bit structure ptr.
LEA9B:	sta StructPtr			;
LEA9D:	iny				;
LEA9E:	lda (StructPtrTable),y		;High byte of 16-bit structure ptr.
LEAA0:	sta StructPtr+1			;
LEAA2:	jsr DrawStruct			;($EF8C)Draw one structure.
LEAA5:	lda #$03			;Move to next set of structure data.
LEAA7:	jsr AddToRoomPtr		;($EAC0)Add A to room data pointer.

;-------------------------------------------[ Draw room ]--------------------------------------------

;The following function draws a room in the room RAM which is eventually loaded into a name table.

DrawRoom:
LEAAA:	ldy #$00			;Zero index.
LEAAC:	lda (RoomPtr),y			;Load byte of room data.-->
LEAAE:	cmp #$FF			;Is it #$FF(end-of-room)?-->
LEAB0:	beq EndOfRoom			;If so, branch to exit.
LEAB2:	cmp #$FE			;Place holder for empty room objects(not used).
LEAB4:	beq +				;
LEAB6:	cmp #$FD			;is A=#$FD(end-of-objects)?-->
LEAB8:	bne DrawObject			;If not, branch to draw room object.-->
LEABA:	beq EndOfObjs			;Else branch to set up enemies/doors.
LEABC:*	sta RoomNumber			;Store #$FE if room object is empty.
LEABE:	lda #$01			;Prepare to increment RoomPtr.

;-------------------------------------[ Add A to room pointer ]--------------------------------------

AddToRoomPtr:
LEAC0:	clc				;Prepare to add index in A to room pointer.
LEAC1:	adc RoomPtr			;
LEAC3:	sta RoomPtr			;
LEAC5:	bcc +				;Did carry occur? If not branch to exit.
LEAC7:	inc RoomPtr+1			;Increment high byte of room pointer if carry occured.
LEAC9:*	rts				;

;----------------------------------------------------------------------------------------------------

EndOfObjs:
LEACA:	lda RoomPtr			;
LEACC:	sta $00				;Store room pointer in $0000.
LEACE:	lda RoomPtr+1			;
LEAD0:	sta $01				;
LEAD2:	lda #$01			;Prepare to increment to enemy/door data.

EnemyLoop:
LEAD4:	jsr AddToPtr00			;($EF09)Add A to pointer at $0000.
LEAD7:	ldy #$00			;
LEAD9:	lda ($00),y			;Get first byte of enemy/door data.
LEADB:	cmp #$FF			;End of enemy/door data?-->
LEADD:	beq EndOfRoom			;If so, branch to finish room setup.
LEADF:	and #$0F			;Discard upper four bits of data.
LEAE1:	jsr ChooseRoutine		;Jump to proper enemy/door handling routine.

;Pointer table to code.

LEAE4:	.word ExitSub			;($C45C)Rts.
LEAE6:	.word LoadEnemy			;($EB06)Room enemies.
LEAE8:	.word LoadDoor			;($EB8C)Room doors.
LEAEA:	.word ExitSub			;($C45C)Rts.
LEAEC:	.word LoadElevator		;($EC04)Elevator.
LEAEE:	.word ExitSub			;($C45C)Rts.
LEAF0:	.word LoadStatues		;($EC2F)Kraid & Ridley statues.
LEAF2:	.word ZebHole			;($EC57)Regenerating enemies(such as Zeb).

EndOfRoom:
LEAF4:	ldx #$F0			;Prepare for PPU attribute table write.
	stx RoomNumber			;
	lda ScrollDir			;
	sta TempScrollDir		;Make temp copy of ScrollDir.
	and #$02			;Check if scrolling left or right.
	bne +				;
	jmp LE57C
*       jmp LE70C

LoadEnemy:
LEB06:	jsr GetEnemyData		;($EB0C)Get enemy data from room data.
LEB09:	jmp EnemyLoop			;($EAD4)Do next room object.

GetEnemyData:
LEB0C:	lda ($00),y			;Get 1st byte again.
	and #$F0			;Get object slot that enemy will occupy.
	tax				;
	jsr IsSlotTaken			;($EB7A)Check if object slot is already in use.
	bne ++				;Exit if object slot taken.
	iny				;
	lda ($00),y			;Get enemy type.
	jsr GetEnemyType		;($EB28)Load data about enemy.
	ldy #$02			;
	lda ($00),y			;Get enemy initial position(%yyyyxxxx).
	jsr LEB4D
	pha
*       pla
*       lda #$03			;Number of bytes to add to ptr to find next room item.
	rts				;

GetEnemyType:
LEB28:	pha				;Store enemy type.
	and #$C0			;If MSB is set, the "tough" version of the enemy  
	sta EnSpecialAttribs,x		;is to be loaded(more hit points, except rippers).
	asl				;
	bpl ++				;If bit 6 is set, the enemy is either Kraid or Ridley.
	lda InArea			;Load current area Samus is in(to check if Kraid or-->
	and #$06			;Ridley is alive or dead).
	lsr				;Use InArea to find status of Kraid/Ridley statue.
	tay				;
	lda MaxMissiles,y		;Load status of Kraid/Ridley statue.
	beq +				;Branch if Kraid or Ridley needs to be loaded.
	pla				;
	pla				;Mini boss is dead so pull enemy info and last address off-->
	jmp --				;stack so next enemy/door item can be loaded.

*       lda #$01			;Samus is in Kraid or Ridley's room and the-->
	sta KraidRidleyPresent		;mini boss is alive and needs to be loaded.

*	pla				;Restore enemy type data.
	and #$3F			;Keep 6 lower bits to use as index for enemy data tables.
	sta EnDataIndex,x		;Store index byte.
	rts				;

LEB4D:  tay				;Save enemy position data in Y.
	and #$F0			;Extract Enemy y position.
	ora #$08			;Add 8 pixels to y position so enemy is always on screen. 
	sta EnYRoomPos,x		;Store enemy y position.
	tya				;Restore enemy position data.
	jsr Amul16			;*16 to extract enemy x position.
	ora #$0C			;Add 12 pixels to x position so enemy is always on screen.
	sta EnXRoomPos,x		;Store enemy x position.
	lda #$01			;
	sta EnStatus,x			;Indicate object slot is taken.
	lda #$00
	sta $0404,x
	jsr GetNameTable		;($EB85)Get name table to place enemy on.
	sta EnNameTable,x		;Store name table.

UnknownEB6E:
LEB6E:	ldy EnDataIndex,x		;Load A with index to enemy data.
	asl $0405,x			;*2
	jsr LFB7B
	jmp LF85A

IsSlotTaken:
LEB7A:	lda EnStatus,x
	beq +
	lda $0405,x
	and #$02
*       rts

;------------------------------------------[ Get name table ]----------------------------------------

;The following routine is small but is called by several other routines so it is important and
;requires some explaining to understand its function.  First of all, as Samus moves from one room
;to the next, she is also moving from one name table to the next.  Samus does not move from one
;name table to the next as one might think. Samus moves diagonally through the name tables. To
;understand this concept, one must first know how the name tables are arranged.  They are arranged
;like so:
;
; +-----+-----+							 +-----+-----+
; |     |     | The following is an incorrect example of how	 |     |     |
; |  2  |  3  | Samus goes from one name table to the next-----> |  2  |  3  |
; |     |     |							 |     |     |
; +-----+-----+							 +-----+-----+
; |     |     |							 |     |     |
; |  0  |  1  |				      INCORRECT!------>  |  0<-|->1  |
; |     |     |							 |     |     |
; +-----+-----+							 +-----+-----+
;
;The following are examples of how the name tables are properly traversed while walking through rooms:
;
;	   +-----+-----+				     +-----+-----+
;	   |     |     |				     |     |     |
;	   |  2  | ->3 |				     |  2  |  3<-|-+
;	   |     |/    |				     |     |     | |
;	   +-----+-----+     <--------CORRECT!-------->      +-----+-----+ |
;	   |    /|     |				     |     |     | |
;	   | 0<- |  1  |				   +-|->0  |  1  | |
;	   |     |     |				   | |     |     | |
;	   +-----+-----+				   | +-----+-----+ |
;							   +---------------+
;
;The same diagonal traversal of the name tables illustrated above applies to vetricle traversal as
;well. Since Samus can only travel between 2 name tables and not 4, the name table placement for
;objects is simplified.  The following code determines which name table to use next:

GetNameTable:
LEB85:	lda PPUCNT0ZP			;
LEB87:	eor ScrollDir			;Store #$01 if object should be loaded onto name table 3-->
LEB89:	and #$01			;store #$00 if it should be loaded onto name table 0.
LEB8B:	rts				;

;----------------------------------------------------------------------------------------------------

; LoadDoor
; ========

	LoadDoor:
	jsr LEB92
*       jmp EnemyLoop    ; do next room object

LEB92:  iny
	lda ($00),y     ; door info byte
	pha
	jsr Amul16      ; CF = door side (0=right, 1=left)
	php
	lda MapPosX
	clc
	adc MapPosY
	plp
	rol
	and #$03
	tay
	ldx LEC00,y
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
	ldy MapPosX
	txa
	jsr Amul16       ; * 16
	bcc +
	dey
*       tya
	jsr LEE41
	jsr LEE4A
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
	lda LEBFE,y
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
LEBFE:  .byte $02
	.byte $01
LEC00:  .byte $80
	.byte $B0
	.byte $A0
	.byte $90

; LoadElevator
; ============

	LoadElevator:
	jsr LEC09
	bne ----	   ; branch always

LEC09:  lda ElevatorStatus
	bne +	   ; exit if elevator already present
	iny
	lda ($00),y
	sta $032F
	ldy #$83
	sty $032D       ; elevator Y coord
	lda #$80
	sta $032E       ; elevator X coord
	jsr GetNameTable		;($EB85)
	sta $032C       ; high Y coord
	lda #$23
	sta $0323       ; elevator frame
	inc ElevatorStatus		;1
*       lda #$02
	rts

; LoadStatues
; ===========

	LoadStatues:
	jsr GetNameTable		;($EB85)
	sta $036C
	lda #$40
	ldx RidleyStatueStatus
	bpl +	   ; branch if Ridley statue not hit
	lda #$30
*       sta $0370
	lda #$60
	ldx KraidStatueStatus
	bpl +	   ; branch if Kraid statue not hit
	lda #$50
*       sta $036F
	sty $54
	lda #$01
	sta $0360
*	jmp EnemyLoop   ; do next room object

ZebHole:
LEC57:  ldx #$20
*       txa
	sec
	sbc #$08
	bmi +
	tax
	ldy $0728,x
	iny
	bne -
	ldy #$00
	lda ($00),y
	and #$F0
	sta $0729,x
	iny
	lda ($00),y
	sta $0728,x
	iny
	lda ($00),y
	tay
	and #$F0
	ora #$08
	sta $072A,x
	tya
	jsr Amul16       ; * 16
	ora #$00
	sta $072B,x
	jsr GetNameTable		;($EB85)
	sta $072C,x
*       lda #$03
	bne ---

OnNameTable0:
LEC93:	lda PPUCNT0ZP			;
	eor #$01			;If currently on name table 0,-->
	and #$01			;return #$01. Else return #$00.
	tay				;
	rts				;

UpdateRoomSpriteInfo:
LEC9B:	ldx ScrollDir
	dex
	ldy #$00
	jsr UpdateDoorData		;($ED51)Update name table 0 door data.
	iny
	jsr UpdateDoorData		;($ED51)Update name table 3 door data.
	ldx #$50
	jsr GetNameTable		;($EB85)
	tay
*       tya
	eor EnNameTable,x
	lsr
	bcs +
	lda $0405,x
	and #$02
	bne +
	sta EnStatus,x
*       jsr Xminus16
	bpl --
	ldx #$18
*       tya
	eor $B3,x
	lsr
	bcs +
	lda #$00
	sta $B0,x
*       txa
	sec
	sbc #$08
	tax
	bpl --
	jsr UnknownED65
	jsr LED5B
	jsr GetNameTable		;(EB85)
	asl
	asl
	tay
	ldx #$C0
*       tya
	eor TileWRAMHi,x
	and #$04
	bne +
	sta $0500,x
*       jsr Xminus16
	cmp #$F0
	bne --
	tya
	lsr
	lsr
	tay
	ldx #$D0
	jsr LED7A
	ldx #$E0
	jsr LED7A
	ldx #$F0
	jsr LED7A
	tya
	sec
	sbc $032C
	bne +
	sta ElevatorStatus
*       ldx #$1E
*       lda $0704,x
	bne +
	lda #$FF
	sta $0700,x
*       txa
	sec
	sbc #$06
	tax
	bpl --
	cpy $036C
	bne +
	lda #$00
	sta $0360
*       ldx #$18
*       tya
	cmp $072C,x
	bne +
	lda #$FF
	sta $0728,x
*       txa
	sec
	sbc #$08
	tax
	bpl --
	ldx #$00
	jsr LED8C
	ldx #$08
	jsr LED8C
	jmp $95AE

UpdateDoorData:
LED51:	txa				;
LED52:	eor #$03			;
LED54:	and $006C,y			;Moves door info from one name table to the next-->
LED57:*	sta $006C,y			;when the room is transferred across name tables.
LED5A:	rts				;

LED5B:  jsr GetNameTable		;($EB85)
	eor #$01
	tay
	lda #$00
	beq -
UnknownED65:
LED65:  ldx #$B0
*       lda ObjAction,x
	beq +
	lda ObjectOnScreen,x
	bne +
	sta ObjAction,x
*       jsr Xminus16
	bmi --
	rts

LED7A:  lda ObjAction,x
	cmp #$05
	bcc +
	tya
	eor ObjectHi,x
	lsr
	bcs +
	sta ObjAction,x
*       rts

LED8C:  tya
	cmp PowerUpNameTable,x
	bne Exit11
	lda #$FF
	sta PowerUpType,x
Exit11: rts

;---------------------------------------[ Setup special items ]--------------------------------------

;The following routines look for special items on the game map and jump to
;the appropriate routine to handle those items.

ScanForItems:
LED98:	lda SpecItmsTable		;Low byte of ptr to 1st item data.
LED9B:	sta $00				;
LED9D:	lda SpecItmsTable+1		;High byte of ptr to 1st item data.

ScanOneItem:
LEDA0:	sta $01				;
LEDA2:	ldy #$00			;Index starts at #$00.
LEDA4:	lda ($00),y			;Load map Ypos of item.-->
LEDA6:	cmp MapPosY			;Does it equal Samus' Ypos on map?-->
LEDA8:	beq +				;If yes, check Xpos too.

LEDAA:	bcs Exit11			;Exit if item Y pos >  Samus Y Pos.
LEDAC:	iny				;
LEDAD:	lda ($00),y			;Low byte of ptr to next item data.
LEDAF:	tax				;
LEDB0:	iny				;
LEDB1:	and ($00),y			;AND with hi byte of item ptr.
LEDB3:	cmp #$FF			;if result is FFh, then this was the last item-->
LEDB5:	beq Exit11			;(item ptr = FFFF). Branch to exit.

LEDB7:	lda ($00),y			;High byte of ptr to next item data.
LEDB9:	stx $00				;Write low byte for next item.
LEDBB:	jmp ScanOneItem			;Process next item.

LEDBE:*	lda #$03			;Get ready to look at byte containing X pos.
LEDC0:	jsr AddToPtr00			;($EF09)Add 3 to pointer at $0000.

ScanItemX:
LEDC3:	ldy #$00			;
LEDC5:	lda ($00),y			;Load map Xpos of object.-->
LEDC7:	cmp MapPosX			;Does it equal Samus' Xpos on map?-->
LEDC9:	beq +				;If so, then load object.
LEDCB:	bcs Exit11			;Exit if item pos X > Samus Pos X.

LEDCD:	iny				;
LEDCE:	jsr AnotherItem			;($EF00)Check for another item on same Y pos.
LEDD1:	jmp ScanItemX			;Try next X coord.

LEDD4:*	lda #$02			;Move ahead two bytes to find item data.

ChooseHandlerRoutine:
LEDD6:	jsr AddToPtr00			;($EF09)Add A to pointer at $0000.
LEDD9:	ldy #$00			;
LEDDB:	lda ($00),y			;Object type
LEDDD:	and #$0F			;Object handling routine index stored in 4 LSBs.
LEDDF:	jsr ChooseRoutine		;($C27C)Load proper handling routine from table below.

;Handler routines jumped to by above code.

LEDE2:	.word ExitSub			;($C45C)rts.
LEDE4:	.word SqueeptHandler		;($EDF8)Some squeepts.
LEDE6:	.word PowerUpHandler		;($EDFE)power-ups.
LEDE8:	.word SpecEnemyHandler		;($EE63)Special enemies(Mellows, Melias and Memus).
LEDEA:	.word ElevatorHandler		;($EEA1)Elevators.
LEDEC:	.word CannonHandler		;($EEA6)Mother brain room cannons.
LEDEE:	.word MotherBrainHandler	;($EEAE)Mother brain.
LEDF0:	.word ZeebetiteHandler		;($EECA)Zeebetites.
LEDF2:	.word RinkaHandler		;($EEEE)Rinkas.
LEDF4:	.word DoorHandler		;($EEF4)Some doors.
LEDF6:	.word PaletteHandler		;($EEFA)Background palette change.

;---------------------------------------[ Squeept handler ]------------------------------------------

SqueeptHandler:
LEDF8:	jsr GetEnemyData		;($EB0C)Load Squeept data.
LEDFB:*	jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

;--------------------------------------[ Power-up Handler ]------------------------------------------

PowerUpHandler:
LEDFE:	iny				;Prepare to store item type.
LEDFF:	ldx #$00			;
LEE01:	lda #$FF			;
LEE03:	cmp PowerUpType			;Is first power-up item slot available?-->
LEE06:	beq +				;if yes, branch to load item.

LEE08:	ldx #$08			;Prepare to check second power-up item slot.
LEE0A:	cmp PowerUpBType		;Is second power-up item slot available?-->			
LEE0D:	bne ++				;If not, branch to exit.
LEE0F:*	lda ($00),y			;Power-up item type.
LEE11:	jsr PrepareItemID		;($EE3D)Get unique item ID.
LEE14:	jsr CheckForItem		;($EE4A)Check if Samus already has item.
LEE17:	bcs +				;Samus already has item. do not load it.

LEE19:	ldy #$02			;Prepare to load item coordinates.
LEE1B:	lda $09				;
LEE1D:	sta PowerUpType,x		;Store power-up type in available item slot.
LEE20:	lda ($00),y			;Load x and y screen positions of item.
LEE22:	tay				;Save position data for later processing.
LEE23:	and #$F0			;Extract Y coordinate.
LEE25:	ora #$08			;+ 8 to find  Y coordinate center.
LEE27:	sta PowerUpYCoord,x		;Store center Y coord
LEE2A:	tya				;Reload position data.
LEE2B:	jsr Amul16			;($C2C5)*16. Move lower 4 bits to upper 4 bits.
LEE2E:	ora #$08			;+ 8 to find X coordinate center.
LEE30:	sta PowerUpXCoord,x		;Store center X coord
LEE33:	jsr GetNameTable		;($EB85)Get name table to place item on.
LEE36:	sta PowerUpNameTable,x		;Store name table Item is located on.

LEE39:*	lda #$03			;Get next data byte(Always #$00).
LEE3B:	bne ---				;Branch always to exit handler routines.
	
PrepareItemID:
LEE3D:	sta $09				;Store item type.
LEE3E:	lda MapPosX			;

LEE41:	sta $07				;Store item X coordinate.
LEE42:	lda MapPosY			;
LEE45:	sta $06				;Store item Y coordinate.
LEE47:	jmp CreateItemID		;($DC67)Get unique item ID.

CheckForItem:
LEE4A:	ldy NumberOfUniqueItems		;
LEE4D:	beq +++				;Samus has no unique items. Load item and exit.
LEE4F:*	lda $07				;
LEE51:	cmp NumberOfUniqueItems,y	;Look for lower byte of unique item.
LEE54:	bne +				;
LEE56:	lda $06				;Look for upper byte of unique item.
LEE58:	cmp DataSlot,y			;
LEE5B:	beq +++				;Samus already has item. Branch to exit.
LEE5D:*	dey				;
LEE5E:	dey				;
LEE5F:	bne --				;Loop until all Samus' unique items are checked.
LEE61:*	clc				;Samus does not have the item. It will be placed on screen.
LEE62:*	rts				;

;-----------------------------------------------------------------------------------------------------

SpecEnemyHandler:
LEE63:  ldx #$18
	lda RandomNumber1
	adc FrameCount
	sta $8A
*       jsr LEE86
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
*       jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

LEE86:  lda $B0,x
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
LEEA1:  jsr LEC09
	bne --				;Branch always.

CannonHandler:
LEEA6:  jsr $95B1
	lda #$02
*       jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

MotherBrainHandler:
LEEAE:  jsr $95B4
	lda #$38
	sta $07
	lda #$00
	sta $06
	jsr LEE4A
	bcc LEEC6
	lda #$08
	sta MotherBrainStatus
	lda #$00
	sta MotherBrainHits
LEEC6:  lda #$01
	bne -

ZeebetiteHandler:
LEECA:  jsr $95B7
	txa
	lsr
	adc #$3C
	sta $07
	lda #$00
	sta $06
	jsr LEE4A
	bcc +
	lda #$81
	sta $0758,x
	lda #$01
	sta $075D,x
	lda #$07
	sta $075B,x
*       jmp LEEC6

RinkaHandler:
LEEEE:  jsr $95BA
	jmp LEEC6

DoorHandler:
LEEF4:  jsr LEB92
	jmp ChooseHandlerRoutine	;($EDD6)Exit handler routines.

PaletteHandler:
LEEFA:  lda ScrollDir
	sta $91
	bne LEEC6

AnotherItem:
LEF00:	lda ($00),y			;Is there another item with same Y pos?-->
	cmp #$FF			;If so, A is amount to add to ptr. to find X pos.
	bne AddToPtr00			;($EF09)
	pla				;
	pla				;No more items to check. Pull last subroutine-->
	rts				;off stack and exit.

AddToPtr00:
LEF09:	clc				;
	adc $00				;
	sta $00				;A is added to the 16 bit address stored in $0000.
	bcc +				;
	inc $01				;
*       rts				;

;----------------------------------[ Draw structure routines ]----------------------------------------

;Draws one row of the structure.
;A = number of 2x2 tile macros to draw horizontally.

DrawStructRow:
LEF13:	and #$0F			;Row length(in macros). Range #$00 thru #$0F.
LEF15:	bne +				;
LEF17:	lda #$10			;#$00 in row length=16.
LEF19:*	sta $0E				;Store horizontal macro count.
LEF1B:	lda (StructPtr),y		;Get length byte again.
LEF1D:	jsr Adiv16			;($C2BF)/16. Upper nibble contains x coord offset(if any).
LEF20:	asl				;*2, because a macro is 2 tiles wide.
LEF21:	adc CartRAMWorkPtr		;Add x coord offset to CartRAMWorkPtr and save in $00.
LEF23:	sta $00				;
LEF25:	lda #$00			;
LEF27:	adc CartRAMWorkPtr+1		;Save high byte of work pointer in $01.
LEF29:	sta $01				;$0000 = work pointer.

DrawMacro:
LEF2B:	lda $01				;High byte of current location in room RAM.
LEF2D:	cmp #$63			;Check high byte of room RAM address for both room RAMs-->
LEF2F:	beq +				;to see if the attribute table data for the room RAM has-->
LEF31:	cmp #$67			;been reached.  If so, branch to check lower byte as well.
LEF33:	bcc ++				;If not at end of room RAM, branch to draw macro.
LEF35:	beq +				;
LEF37:	rts				;Return if have gone past room RAM(should never happen).

LEF38:*	lda $00				;Low byte of current nametable address.
LEF3A:	cmp #$A0			;Reached attrib table?-->
LEF3C:	bcc +				;If not, branch to draw the macro.
LEF3E:	rts				;Can't draw any more of the structure, exit.

LEF3F:*	inc $10				;Increase struct data index.
LEF41:	ldy $10				;Load struct data index into Y.
LEF43:	lda (StructPtr),y		;Get macro number.
LEF45:	asl				;
LEF46:	asl				;A=macro number * 4. Each macro is 4 bytes long.
LEF47:	sta $11				;Store macro index.
LEF49:	ldx #$03			;Prepare to copy four tile numbers.
LEF4B:*	ldy $11				;Macro index loaded into Y.
LEF4D:	lda (MacroPtr),y		;Get tile number.
LEF4F:	inc $11				;Increase macro index
LEF51:	ldy TilePosTable,x		;get tile position in macro.
LEF54:	sta ($00),y			;Write tile number to room RAM.
LEF56:	dex				;Done four tiles yet?-->
LEF57:	bpl -				;If not, loop to do another.
LEF59:	jsr UpdateAttrib		;($EF9E)Update attribute table if necessary
LEF5C:	ldy #$02			;Macro width(in tiles).
LEF5E:	jsr AddYToPtr00			;($C2A8)Add 2 to pointer to move to next macro.
LEF61:	lda $00				;Low byte of current room RAM work pointer.
LEF63:	and #$1F			;Still room left in current row?-->
LEF65:	bne +				;If yes, branch to do another macro.

;End structure row early to prevent it from wrapping on to the next row..
LEF67:	lda $10				;Struct index.
LEF69:	clc				;
LEF6A:	adc $0E				;Add number of macros remaining in current row.
LEF6C:	sec				;
LEF6D:	sbc #$01			;-1 from macros remaining in current row.
LEF6F:	jmp AdvanceRow			;($EF78)Move to next row of structure.

LEF72:*	dec $0E				;Have all macros been drawn on this row?-->
LEF74:	bne DrawMacro			;If not, branch to draw another macro.
LEF76:	lda $10				;Load struct index.

AdvanceRow:
LEF78:	sec				;Since carry bit is set,-->
LEF79:	adc StructPtr			;addition will be one more than expected.
LEF7B:	sta StructPtr			;Update the struct pointer.
LEF7D:	bcc +				;
LEF7F:	inc StructPtr+1			;Update high byte of struct pointer if carry occured.
LEF81:*	lda #$40			;
LEF83:	clc				;
LEF84:	adc CartRAMWorkPtr		;Advance to next macro row in room RAM(two tile rows).
LEF86:	sta CartRAMWorkPtr		;
LEF88:	bcc DrawStruct			;Begin drawing next structure row.
LEF8A:	inc CartRAMWorkPtr+1		;Increment high byte of pointer if necessary.

DrawStruct:
LEF8C:	ldy #$00			;Reset struct index.
LEF8E:	sty $10				;
LEF90:	lda (StructPtr),y		;Load data byte.
LEF92:	cmp #$FF			;End-of-struct?-->
LEF94:	beq +				;If so, branch to exit.
LEF96:	jmp DrawStructRow		;($EF13)Draw a row of macros.
LEF99:*	rts				;

;The following table is used to draw macros in room RAM. Each macro is 2 x 2 tiles.
;The following table contains the offsets required to place the tiles in each macro.

TilePosTable:
LEF9A:	.byte $21			;Lower right tile.
LEF9B:	.byte $20			;Lower left tile.
LEF9C:	.byte $01			;Upper right tile.
LEF9D:	.byte $00			;Upper left tile.

;---------------------------------[ Update attribute table bits ]------------------------------------

;The following routine updates attribute bits for one 2x2 tile section on the screen.

UpdateAttrib:
LEF9E:	lda ObjectPal			;Load attribute data of structure.
LEFA0:	cmp RoomPal			;Is it the same as the room's default attribute data?-->
LEFA2:	beq +++++			;If so, no need to modify the attribute table, exit.

;Figure out cart RAM address of the byte containing the relevant bits.

LEFA4:	lda $00				;
LEFA6:	sta $02				;
LEFA8:	lda $01				;
LEFAA:	lsr				;
LEFAB:	ror $02				;
LEFAD:	lsr				;
LEFAE:	ror $02				;
LEFB0:	lda $02				;The following section of code calculates the-->
LEFB2:	and #$07			;proper attribute byte that corresponds to the-->
LEFB4:	sta $03				;macro that has just been placed in the room RAM.
LEFB6:	lda $02				;
LEFB8:	lsr				;
LEFB9:	lsr				;
LEFBA:	and #$38			;
LEFBC:	ora $03				;
LEFBE:	ora #$C0			;
LEFC0:	sta $02				;
LEFC2:	lda #$63			;
LEFC4:	sta $03				;$0002 contains pointer to attribute byte.

LEFC6:	ldx #$00			;
LEFC8:	bit $00				;
LEFCA:	bvc +				;
LEFCC:	ldx #$02			;The following section of code figures out which-->
LEFCE:*	lda $00				;pair of bits to modify in the attribute table byte-->
LEFD0:	and #$02			;for the macro that has just been placed in the-->
LEFD2:	beq +				;room RAM.
LEFD4:	inx				;

;X now contains which macro attribute table bits to modify:
;+---+---+
;| 0 | 1 |
;+---+---+
;| 2 | 3 |
;+---+---+
;Where each box represents a macro(2x2 tiles).

;The following code clears the old attribute table bits and sets the new ones.
LEFD5:*	lda $01				;Load high byte of work pointer in room RAM.
LEFD7:	and #$04			;
LEFD9:	ora $03				;Choose proper attribute table associated with the-->
LEFDB:	sta $03				;current room RAM.
LEFDD:	lda AttribMaskTable,x		;Choose appropriate attribute table bit mask from table below.
LEFE0:	ldy #$00			;
LEFE2:	and ($02),y			;clear the old attribute table bits.
LEFE4:	sta ($02),y			;
LEFE6:	lda ObjectPal			;Load new attribute table data(#$00 thru #$03).
LEFE8:*	dex				;
LEFE9:	bmi +				;
LEFEB:	asl				;
LEFEC:	asl				;Attribute table bits shifted one step left
LEFED:	bcc -				;Loop until attribute table bits are in the proper location.
LEFEF:*	ora ($02),y			;
LEFF1:	sta ($02),y			;Set attribute table bits.
LEFF3:*	rts				;

AttribMaskTable:
LEFF4:	.byte %11111100			;Upper left macro.
LEFF5:	.byte %11110011			;Upper right macro.
LEFF6:	.byte %11001111			;Lower left macro.
LEFF7:	.byte %00111111			;Lower right macro.

;------------------------[ Initialize room RAM and associated attribute table ]-----------------------

InitTables:
LEFF8:	lda CartRAMPtr+1		;#$60 or #$64.
LEFFA:	tay				;
LEFFB:	tax				;Save value to create counter later.
LEFFC:	iny				;
LEFFD:	iny				;High byte of address to fill to ($63 or $67).
LEFFE:	iny				;
LEFFF:	lda #$FF			;Value to fill room RAM with.
LF001:	jsr FillRoomRAM			;($F01C)Fill entire RAM for designated room with #$FF.

LF004:	ldx $01				;#$5F or #$63 depening on which room RAM was initialized.
LF006:	jsr Xplus4			;($E193)X = X + 4.
LF009:	stx $01				;Set high byte for attribute table write(#$63 or #$67).
LF00B:	ldx RoomPal			;Index into table below (Lowest 2 bits).
LF00D:	lda ATDataTable,x		;Load attribute table data from table below.
LF010:	ldy #$C0			;Low byte of start of all attribute tables.
LF012:*	sta ($00),y			;Fill attribute table.
LF014:	iny				;
LF015:	bne -				;Loop until entire attribute table is filled.
LF017:	rts				;

ATDataTable:       
LF018:	.byte %00000000			;
LF019:	.byte %01010101			;Data to fill attribute tables with.
LF01A:	.byte %10101010			;
LF01B:	.byte %11111111			;

FillRoomRAM:
LF01C:	pha				;Temporarily store A.
LF01D:	txa				;
LF01E:	sty $01				;Calculate value to store in X to use as upper byte-->
LF020:	clc				;counter for initilaizing room RAM(X=#$FC).-->
LF021:	sbc $01				;Since carry bit is cleared, result is one less than expected.
LF023:	tax				;
LF024:	pla				;Restore value to fill room RAM with(#$FF).
LF025:	ldy #$00			;Lower address byte to start at.
LF027:	sty $00				;
LF029:*	sta ($00),y			;
LF02B:	dey				;
LF02C:	bne -				;
LF02E:	dec $01				;Loop until all the room RAM is filled with #$FF(black).
LF030:	inx				;
LF031:	bne -				;
LF033:	rts				;

;----------------------------------------------------------------------------------------------------

; Crash detection
; ===============
CrashDetection:
LF034:  lda #$FF
	sta $73
	sta $010F
; check for crash with Memus
	ldx #$18
*      lda $B0,x
	beq +++++	    ; branch if no Memu in slot
	cmp #$03
	beq +++++
	jsr LF19A
	jsr IsSamusDead
	beq +
	lda SamusBlink
	bne +
	ldy #$00
	jsr LF149
	jsr LF2B4
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
*       jsr LF149
	jsr LF32A
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
	jsr UnknownDC7F
	jsr LF277
*       jsr Xminus16
	bmi --
; enemy <--> bullet/missile/bomb detection
*	ldx #$50		; start with enemy slot #5
LF09F:  lda EnStatus,x	     ; slot active?
	beq +		   ; branch if not
	cmp #$03
*       beq NextEnemy	   ; next slot
	jsr LF152
	lda EnStatus,x
	cmp #$05
	beq ++++
	ldy #$D0		; first projectile slot
*       lda ObjAction,y	 ; is it active?
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
*       jsr LF140
	jsr LF2CA
*	jsr Yplus16	     ; next projectile slot
	bne ---
*	ldy #$00
	lda SamusBlink
	bne NextEnemy
	jsr IsSamusDead
	beq NextEnemy
	jsr LF140
	jsr LF282
	NextEnemy:
	jsr Xminus16
	bmi +
	jmp LF09F

*       ldx #$00
	jsr LF172
	ldy #$60
*       lda EnStatus,y
	beq +
	cmp #$05
	beq +
	lda SamusBlink
	bne +
	jsr IsSamusDead
	beq +
	jsr LF1B3
	jsr LF162
	jsr LF1FA
	jsr LF2ED
*       jsr Yplus16
	cmp #$C0
	bne --
	ldy #$00
	jsr IsSamusDead
	beq ++++
	jsr LF186
	ldx #$F0
*       lda ObjAction,x
	cmp #$07
	beq +
	cmp #$0A
	bne ++
*       jsr UnknownDC82
	jsr LF311
*	jsr Xminus16
	cmp #$C0
	bne ---			
*	jmp SubtractHealth		;($CE92)

LF140:  jsr LF1BF
	jsr LF186
	jmp LF1FA

LF149:  jsr LF186
	jsr LF1D2
	jmp LF1FA

LF152:  lda EnYRoomPos,x
	sta $07	 ; Y coord
	lda EnXRoomPos,x
	sta $09	 ; X coord
	lda EnNameTable,x     ; hi coord
	jmp LF17F

LF162:  lda EnYRoomPos,y     ; Y coord
	sta $06
	lda EnXRoomPos,y     ; X coord
	sta $08
	lda EnNameTable,y     ; hi coord
	jmp LF193

LF172:  lda ObjectY,x
	sta $07
	lda ObjectX,x
	sta $09
	lda ObjectHi,x
LF17F:  eor PPUCNT0ZP
	and #$01
	sta $0B
	rts

LF186:  lda ObjectY,y
	sta $06
	lda ObjectX,y
	sta $08
	lda ObjectHi,y
LF193:  eor PPUCNT0ZP
	and #$01
	sta $0A
	rts

LF19A:  lda $B1,x
	sta $07
	lda $B2,x
	sta $09
	lda $B3,x
	jmp LF17F

LF1A7:  lda ObjRadY,x
	jsr LF1E0
	lda ObjRadX,x
	jmp LF1D9

LF1B3:  lda ObjRadY,x
	jsr LF1E7
	lda ObjRadX,x
	jmp LF1CB

LF1BF:  lda EnRadY,x
	jsr LF1E0
	lda EnRadX,x
	jmp LF1D9

LF1CB:  clc
	adc EnRadX,y
	sta $05
	rts

LF1D2:  lda #$04
	jsr LF1E0
	lda #$08
LF1D9:  clc
	adc ObjRadX,y
	sta $05
	rts

LF1E0:  clc
	adc ObjRadY,y
	sta $04
	rts

LF1E7:  clc
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

LF1FA:  lda #$02
	sta $10
	and ScrollDir
	sta $03
	lda $07
	sec
	sbc $06     ; Y
	sta $00
	lda $03
	bne ++
	lda $0B
	eor $0A
	beq ++
	jsr LF262
	lda $00
	sec
	sbc #$10
	sta $00
	bcs +
	dec $01
*       jmp LF22B

*      lda #$00
	sbc #$00
	jsr LF266
LF22B:  sec
	lda $01
	bne ++
	lda $00
	sta $11
	cmp $04
	bcs ++
	asl $10
	lda $09
	sec
	sbc $08
	sta $00
	lda $03
	beq +
	lda $0B
	eor $0A
	beq +
	jsr LF262
	jmp LF256

*       sbc #$00
	jsr LF266
LF256:  sec
	lda $01
	bne +
	lda $00
	sta $0F
	cmp $05
*      rts

LF262:  lda $0B
	sbc $0A
LF266:  sta $01
	bpl +
	jsr LE449
	inc $10
*       rts

LF270:  ora $030A,x
	sta $030A,x
	rts

LF277:  bcs Exit17
LF279:  lda $10
LF27B:  ora $030A,y
	sta $030A,y
	Exit17:
	rts

LF282:  bcs Exit17
	jsr LF2E8
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	ldy #$00
	bcc +++
	lda EnStatus,x
	cmp #$04
	bcs Exit17
	lda EnDataIndex,x
*       sta $010F
	tay
	bmi +
	lda $968B,y
	and #$10
	bne Exit17
*       ldy #$00
	jsr LF338
	jmp LF306

*	lda #$81
	sta $040E,x
	bne ++
LF2B4:  bcs +
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	ldy #$00
	lda #$C0
	bcs ---
LF2BF:  lda $B6,x
	and #$F8
	ora $10
	eor #$03
	sta $B6,x
*       rts

LF2CA:  bcs +++
	lda ObjAction,y
	sta $040E,x
	jsr LF279
*	jsr LF332
*       ora $0404,x
	sta $0404,x
*       rts

LF2DF:  lda $10
	ora $0404,y
	sta $0404,y
	rts

LF2E8:  jsr LF340
	bne --
LF2ED:  bcs +
	jsr LF2DF
	tya
	pha
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	pla
	tay
	bcc +
	lda #$80
	sta $010F
	jsr LF332
	jsr LF270
LF306:  lda $95CE
	sta HealthLoChange
	lda $95CF
	sta HealthHiChange
*       rts

LF311:  bcs Exit22
	lda #$E0
	sta $010F
	jsr LF338
	lda $0F
	beq +
	lda #$01
*       sta $73

ClearHealthChange:
LF323:	lda #$00
LF325:	sta HealthLoChange
LF327:	sta HealthHiChange

Exit22: 
LF329:	rts				;Return for routine above and below.

LF32A:  bcs Exit22
	jsr LF279
	jmp LF2BF

LF332:  jsr LF340
	jmp Amul8       ; * 8

LF338:  lda $10
	asl
	asl
	asl
	jmp LF27B

LF340:  lda $10
	eor #$03
	rts

; UpdateEnemies
; =============

UpdateEnemies:
LF345:	ldx #$50		;Load x with #$50
*       jsr DoOneEnemy			;($F351)
	ldx PageIndex
	jsr Xminus16
	bne -
DoOneEnemy:
LF351:	stx PageIndex		;PageIndex starts at $50 and is subtracted by $F each iteration.
				;There is a max of 6 enemies at a time.
	ldy EnStatus,x
	beq +
	cpy #$03
	bcs +
	jsr LF37F
*       jsr LF3AA
	lda EnStatus,x
	sta $81
	cmp #$07
	bcs +
	jsr ChooseRoutine

; Pointer table to code

	.word ExitSub       ;($C45C) rts
	.word LF3BE
	.word LF3E6
	.word LF40D
	.word LF43E
	.word LF483
	.word LF4EE

*       jmp KillObject			;($FA18)Free enemy data slot.

LF37F:  lda $0405,x
	and #$02
	bne +
	lda EnYRoomPos,x     ; Y coord
	sta $0A
	lda EnXRoomPos,x     ; X coord
	sta $0B
	lda EnNameTable,x     ; hi coord
	sta $06
	lda EnRadY,x
	sta $08
	lda EnRadX,x
	sta $09
	jsr IsObjectVisible		;($DFDF)Determine if object is within the screen boundaries.
	txa
	bne +
	pla
	pla
*       ldx PageIndex
	rts

LF3AA:  lda $0405,x
	asl
	rol
	tay
	txa
	jsr Adiv16			;($C2BF)/16.
	eor FrameCount
	lsr
	tya
	ror
	ror
	sta $0405,x
	rts

LF3BE:  lda $0405,x
	asl
	bmi +
	lda #$00
	sta $6B01,x
	sta EnCounter,x
	sta $040A,x
	jsr LF6B9
	jsr LF75B
	jsr LF682
	jsr LF676
	lda EnDelay,x
	beq +
	jsr LF7BA
*       jmp ++

LF3E6:  lda $0405,x
	asl
	bmi ++
	lda $0405,x
	and #$20
	beq +
	ldy EnDataIndex,x
	lda EnemyInitDelayTbl,y		;($96BB)
	sta EnDelay,x
	dec EnStatus,x
	bne ++
*       jsr LF6B9
	jsr LF75B
	jsr LF51E
LF40A:*	jsr LF536
LF40D:	jmp $95E5

UpdateEnemyAnim0:
LF410:	jsr UpdateEnemyAnim
	jsr $8058
CheckObjectAttribs:
LF416:  ldx PageIndex
	lda EnSpecialAttribs,x
	bpl +
	lda ObjectCntrl
	bmi +
	lda #$A3
LF423:  sta ObjectCntrl
*       lda EnStatus,x
	beq LF42D
	jsr ClrObjCntrlIfFrameIsF7
LF42D:  ldx PageIndex
	lda #$00
	sta $0404,x
	sta $040E,x
	rts

UpdateEnemyAnim1:
LF438:  jsr UpdateEnemyAnim
LF43B:  jmp LF416

LF43E:  jsr LF536
	lda EnStatus,x
	cmp #$03
	beq LF410
	bit ObjectCntrl
	bmi +
	lda #$A1
	sta ObjectCntrl
*       lda FrameCount
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
*       lda $040D,x
	cmp #$0B
	bcs +
	lda FrameCount
	and #$02
	beq +
	asl ObjectCntrl
*       jmp LF416

LF483:  lda $0404,x
	and #$24
	beq ++++++
	jsr KillObject			;($FA18)Free enemy data slot.
	ldy EnAnimFrame,x
	cpy #$80
	beq PickupMissile
	tya
	pha
	lda EnDataIndex,x
	pha
	ldy #$00
	ldx #$03
	pla
	bne ++
	dex
	pla
	cmp #$81
	bne +
	ldx #$01			;Increase HealthHi by 1.
	ldy #$50			;Increase HealthLo by 5.
*       pha
*	pla				
	sty HealthLoChange
	stx HealthHiChange
	jsr AddHealth			;($CEF9)Add health to Samus.
	jmp SFX_EnergyPickup

PickupMissile:
	lda #$02
	ldy EnDataIndex,x
	beq +
	lda #$1E
*       clc
	adc MissileCount
	bcs +		   ; can't have more than 255 missiles
	cmp MaxMissiles	 ; can Samus hold this many missiles?
	bcc ++		  ; branch if yes
*       lda MaxMissiles	 ; set to max. # of missiles allowed
*	sta MissileCount
	jmp SFX_MissilePickup

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
	jmp LF416

LF4EE:  dec EnSpecialAttribs,x
	bne ++
	lda $040C,x
	tay
	and #$C0
	sta EnSpecialAttribs,x
	tya
	and #$3F
	sta EnStatus,x
	pha
	jsr $80B0
	and #$20
	beq +
	pla
	jsr LF515
	pha
*       pla
*	lda #$A0
	jmp LF423

LF515:  sta $040C,x
LF518:  lda #$04
	sta EnStatus,x
	rts

LF51E:  lda ScrollDir
	ldx PageIndex
	cmp #$02
	bcc +++		; added plus for new damage code
	lda EnYRoomPos,x     ; Y coord
	cmp #$EC
	bcc +++		; added plus for new damage code
	jmp KillObject			;($FA18)Free enemy data slot.

*       jsr SFX_MetroidHit
	jmp GetPageIndex

LF536:  lda EnSpecialAttribs,x
	sta $0A
	lda $0404,x
	and #$20
	beq ++		;added plus
	lda $040E,x
	cmp #$03
	bne ++++	;added plus
	bit $0A
	bvs ++++	;added plus
	lda EnStatus,x
	cmp #$04
	beq ++++	;added plus
	
	;here's some new damage code for the ice beam to work more like super metroid 
	
	
		jsr $80B0				
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
	

	;end of new damage code
	jsr LF515
	lda #$40
	sta $040D,x
	jsr $80B0
	and #$20
	beq +
	lda #$01	;making this one so the metroid only takes one misile instead of 5
	sta EnHitPoints,x
	jmp $95A8
*       rts

*	jsr $80B0
	and #$20
	bne ---- ;added minus
	jsr SFX_Metal
	jmp LF42D

*	lda EnHitPoints,x
	cmp #$FF
	beq --
	bit $0A
	bvc +
	jsr SFX_BossHit
	bne ++
*	jsr LF74B
	and #$0C
	beq PlaySnd1
	cmp #$04
	beq PlaySnd2
	cmp #$08
	beq PlaySnd3
	jsr SFX_MetroidHit
	bne +       ; branch always
PlaySnd1:
	jsr SFX_EnemyHit
	bne +       ; branch always
PlaySnd2:
	jsr SFX_EnemyHit
	bne +       ; branch always
PlaySnd3:
	jsr SFX_BigEnemyHit		;($CBCE)
*	ldx PageIndex
	jsr $80B0
	and #$20
	beq +
	lda $040E,x
	cmp #$0B
	bne ----
*       lda EnStatus,x
	cmp #$04
	bne +
	lda $040C,x
*       ora $0A
	sta $040C,x
	asl
	bmi +
	jsr $80B0
	and #$20
	bne +
	ldy $040E,x
	cpy #$0B
	beq +++++
	cpy #$81
	beq +++++
*       lda #$06
	sta EnStatus,x
	lda #$0A
	bit $0A
	bvc +
	lda #$03
*       sta EnSpecialAttribs,x
	;cpy #$02
	;beq +
	bit SamusGear
	bvs +
	bit $0A
	bvc ++
	ldy $040E,x
	cpy #$0B
	bne ++
	dec EnHitPoints,x
	beq +++
	dec EnHitPoints,x
	beq +++
*       dec EnHitPoints,x
	beq ++
*	dec EnHitPoints,x
	bne GetPageIndex
*	lda #$03
	sta EnStatus,x
	bit $0A
	bvs +
	lda $040E,x
	cmp #$02
	bcs +
	lda #$00
	jsr UnknownDCFC
	ldx PageIndex
*       jsr LF844
	lda $960B,y
	jsr LF68D
	sta EnCounter,x
	ldx #$C0
*       lda EnStatus,x
	beq +
	txa
	clc
	adc #$08
	tax
	cmp #$E0
	bne -
	beq GetPageIndex
*       lda $95DD
	jsr LF68D
	lda #$0A
	sta EnCounter,x
	inc EnStatus,x
	lda #$00
	bit $0A
	bvc +
	lda #$03
*       sta $0407,x
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

LF676:  jsr $80B0
	asl
	asl
	asl
	and #$C0
	sta $6B03,x
	rts

LF682:  jsr LF844
	lda $963B,y
	cmp EnResetAnimIndex,x
	beq +
ResetAnimIndex:
LF68D:  sta EnResetAnimIndex,x
LF690:  sta EnAnimIndex,x
LF693:  lda #$00
	sta EnAnimDelay,x
*       rts

LF699:  jsr LF844
	lda $965B,y
	cmp EnResetAnimIndex,x
	beq Exit12
	jsr LF68D
	ldy EnDataIndex,x
	lda $967B,y
	and #$7F
	beq Exit12
	tay
*       dec EnAnimIndex,x
	dey
	bne -
Exit12: rts

LF6B9:  lda #$00
	sta $82
	jsr LF74B
	tay
	lda EnStatus,x
	cmp #$02
	bne +
	tya
	and #$02
	beq Exit12
*       tya
	dec $040D,x
	bne Exit12
	pha
	ldy EnDataIndex,x
	lda $969B,y
	sta $040D,x
	pla
	bpl ++++
	lda #$FE
	jsr LF7B3
	lda ScrollDir
	cmp #$02
	bcc +
	jsr LF752
	bcc +
	tya
	eor PPUCNT0ZP
	bcs +++
*       lda EnXRoomPos,x
	cmp ObjectX
	bne +
	inc $82
*       rol
*	and #$01
	jsr LF744
	lsr
	ror
	eor $0403,x
	bpl +
	jsr $81DA
*	lda #$FB
	jsr LF7B3
	lda ScrollDir
	cmp #$02
	bcs +
	jsr LF752
	bcc +
	tya
	eor PPUCNT0ZP
	bcs +++
*	lda EnYRoomPos,x
	cmp ObjectY
	bne +
	inc $82
	inc $82
*	rol
*	and #$01
	asl
	asl
	jsr LF744
	lsr
	lsr
	lsr
	ror
	eor $0402,x
	bpl +
	jmp $820F

UnknownF744:
LF744:  ora $0405,x
	sta $0405,x
*       rts

UnknownF74B:
LF74B:  ldy EnDataIndex,x
	lda $968B,y
	rts

LF752:  lda EnNameTable,x
	tay
	eor ObjectHi
	lsr
	rts

LF75B:  lda #$E7
	sta $06
	lda #$18
	jsr LF744
	ldy EnDataIndex,x
	lda $96AB,y
	beq +++++
	tay
	lda $0405,x
	and #$02
	beq ++++
	tya
	ldy #$F7
	asl
	bcs +
	ldy #$EF
*       lsr
	sta $02
	sty $06
	lda ObjectY
	sta $00
	ldy EnYRoomPos,x
	lda $0405,x
	bmi +
	ldy ObjectX
	sty $00
	ldy EnXRoomPos,x
*       lda ObjectHi
	lsr
	ror $00
	lda EnNameTable,x
	lsr
	tya
	ror
	sec
	sbc $00
	bpl +
	jsr TwosCompliment		;($C3D4)
*       lsr
	lsr
	lsr
	cmp $02
	bcc ++
*	lda $06
LF7B3:  and $0405,x
	sta $0405,x
*	rts

LF7BA:  dec EnDelay,x
	bne +
	lda $0405,x
	and #$08
	bne ++
	inc EnDelay,x
*       rts

*	lda EnDataIndex,x
	cmp #$07
	bne +
	jsr SFX_OutOfHole
	ldx PageIndex
*	inc EnStatus,x
	jsr LF699
	ldy EnDataIndex,x
	lda $96CB,y
	clc
	adc #$D1
	sta $00
	lda #$00
	adc #$97
	sta $01
	lda FrameCount
	eor RandomNumber1
	ldy #$00
	and ($00),y
	tay
	iny
	lda ($00),y
	sta $0408,x
	jsr $80B0
	bpl ++
	lda #$00
	sta EnCounter,x
	sta $0407,x
	ldy $0408,x
	lda $972B,y
	sta $6AFE,x
	lda $973F,y
	sta $6AFF,x
	lda $9753,y
	sta $0402,x
	lda $9767,y
	sta $0403,x
	lda $0405,x
	bmi +
	lsr
	bcc ++
	jsr $81D1
	jmp ++

*       and #$04
	beq +
	jsr $8206
*	lda #$DF
	jmp LF7B3

UnknownF83E:
LF83E:  lda $0405,x
LF841:  jmp +

LF844:  lda $0405,x
	bpl +
	lsr
	lsr
*	lsr
	lda EnDataIndex,x
	rol
	tay
	rts

GetRandom_EnIdxFrCnt:
LF852:  txa
	lsr
	lsr
	lsr
	adc FrameCount
	lsr
	rts

UnknownF85A:
LF85A:  ldy EnDataIndex,x
	lda $969B,y
	sta $040D,x
	lda EnemyHitPointTbl,y		;($962B)
	ldy EnSpecialAttribs,x
	bpl +
	asl
*       sta EnHitPoints,x
*       rts

UnknownF870:
LF870:  lda $0405,x
	and #$10
	beq -
	lda $87
	and EnStatus,x
	beq -
	lda $87
	bpl +
	ldy $6B01,x
	bne -
*       jsr LF8E8
	bcs ++
	sta $0404,y
	jsr LF92C
	lda $0405,x
	lsr
	lda $85
	pha
	rol
	tax
	lda $978B,x
	pha
	tya
	tax
	pla
	jsr LF68D
	ldx PageIndex
	lda #$01
	sta EnStatus,y
	and $0405,x
	tax
	lda Table15,x
	sta $0403,y
	lda #$00
	sta $0402,y
	ldx PageIndex
	jsr LF8F8
	lda $0405,x
	lsr
	pla
	tax
	lda $97A3,x
	sta $04
	txa
	rol
	tax
	lda $979B,x
	sta $05
	jsr LF91D
	ldx PageIndex
	bit $87
	bvc ++
	lda $0405,x
	and #$01
	tay
	lda $0083,y
	jmp LF690

LF8E8:  ldy #$60
	clc
*       lda EnStatus,y
	beq +
	jsr Yplus16
	cmp #$C0
	bne -
*       rts

LF8F8:  lda $85
	cmp #$02
	bcc +
	ldx PageIndex
	lda $0405,x
	lsr
	lda $88
	rol
	and #$07
	sta $040A,y
	lda #$02
	sta EnStatus,y
	lda #$00
	sta EnDelay,y
	sta EnAnimDelay,y
	sta $0408,y
*       rts

LF91D:  ldx PageIndex
	jsr LE792
	tya
	tax
	jsr UnknownFD8F
	jmp LFA49

; Table used by above subroutine

Table15:
	.byte $02
	.byte $FE

LF92C:  lda #$02
	sta EnRadY,y
	sta EnRadX,y
	ora $0405,y
	sta $0405,y
	rts

UnknownF93B:
LF93B:  ldx #$B0
*       jsr LF949
	ldx PageIndex
	jsr Xminus16
	cmp #$60
	bne -
LF949:  stx PageIndex
	lda $0405,x
	and #$02
	bne +
	jsr KillObject			;($FA18)Free enemy data slot.
*       lda EnStatus,x
	beq Exit19
	jsr ChooseRoutine

; Pointer table to code

	.word ExitSub     ;($C45C) rts
	.word LF96A
	.word LF991       ; spit dragon's fireball
	.word ExitSub     ;($C45C) rts
	.word LFA6B
	.word LFA91

Exit19: rts

LF96A:  jsr LFA5B
	jsr LFA1E
	ldx PageIndex
	bcs LF97C
	lda EnStatus,x
	beq Exit19
	jsr LFA60
LF97C:  lda #$01
LF97E:  jsr UpdateEnemyAnim
	jmp ClrObjCntrlIfFrameIsF7

*       inc $0408,x
LF987:  inc $0408,x
	lda #$00
	sta EnDelay,x
	beq +
LF991:  jsr LFA5B
	lda $040A,x
	and #$FE
	tay
	lda $97A7,y
	sta $0A
	lda $97A8,y
	sta $0B
*       ldy $0408,x
	lda ($0A),y
	cmp #$FF
	bne +
	sta $0408,x
	jmp LF987

*       cmp EnDelay,x
	beq ---
	inc EnDelay,x
	iny
	lda ($0A),y
	jsr Unknown8296
	ldx PageIndex
	sta $0402,x
	lda ($0A),y
	jsr Unknown832F
	ldx PageIndex
	sta $0403,x
	tay
	lda $040A,x
	lsr
	php
	bcc +
	tya
	jsr TwosCompliment		;($C3D4)
	sta $0403,x
*       plp
	bne +
	lda $0402,x
	beq +
	bmi +
	ldy $040A,x
	lda $95E0,y
	sta EnResetAnimIndex,x
*       jsr LFA1E
	ldx PageIndex
	bcs ++
	lda EnStatus,x
	beq Exit20
	ldy #$00
	lda $040A,x
	lsr
	beq +
	iny
*       lda $95E2,y
	jsr LF68D
	jsr LF518
	lda #$0A
	sta EnDelay,x
*       jmp LF97C

KillObject:
LFA18:	lda #$00			;
LFA1A:	sta EnStatus,x			;Store #$00 as enemy status(enemy slot is open).
LFA1D:	rts				;

; enemy<-->background crash detection
EnemyBGCrashDetection:
LFA1E:  lda InArea
	cmp #$11
	bne +
	lda EnStatus,x
	lsr
	bcc ++
*       jsr LFA7D
	ldy #$00
	lda ($04),y
	cmp #$A0
	bcc ++
	ldx PageIndex
*       lda $0403,x
	sta $05
	lda $0402,x
	sta $04
LFA41:  jsr LE792
	jsr UnknownFD8F
	bcc KillObject			;($FA18)Free enemy data slot.
LFA49:  lda $08
	sta EnYRoomPos,x
	lda $09
	sta EnXRoomPos,x
	lda $0B
	and #$01
	sta EnNameTable,x
*       rts

LFA5B:  lda $0404,x
	beq Exit20
LFA60:  lda #$00
	sta $0404,x
	lda #$05
	sta EnStatus,x
Exit20: rts

LFA6B:  lda EnAnimFrame,x
	cmp #$F7
	beq +
	dec EnDelay,x
	bne ++
*       jsr KillObject			;($FA18)Free enemy data slot.
*       jmp LF97C

LFA7D:  ldx PageIndex
	lda EnYRoomPos,x
	sta $02
	lda EnXRoomPos,x
	sta $03
	lda EnNameTable,x
	sta $0B
	jmp MakeWRAMPtr

LFA91:  jsr KillObject			;($FA18)Free enemy data slot.
	lda $95DC
	jsr LF68D
	jmp LF97C

EnemyDestruction:
LFA9D:  ldx #$C0
*       stx PageIndex
	lda EnStatus,x
	beq +
	jsr LFAB4
*       lda PageIndex
	clc
	adc #$08
	tax
	cmp #$E0
	bne --
*      rts

LFAB4:  dec EnCounter,x
	bne ++
	lda #$0C
	sta EnCounter,x
	dec $0407,x
	bmi +
	bne ++
*       jsr KillObject			;($FA18)Free enemy data slot.
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
	jsr LFA41
*       lda #$80
	sta ObjectCntrl
	lda #$03
	jmp LF97E

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

UnknownFAF2:
LFAF2:  ldy #$18
*       jsr LFAFF
	lda PageIndex
	sec
	sbc #$08
	tay
	bne -

LFAFF:  sty PageIndex
	ldx $0728,y
	inx
	beq -----
	ldx $0729,y
	lda EnStatus,x
	beq +
	lda $0405,x
	and #$02
	bne Exit13
*       sta $0404,x
	lda #$FF
	cmp EnDataIndex,x
	bne +
	dec EnDelay,x
	bne Exit13
	lda $0728,y
	jsr LEB28
	ldy PageIndex
	lda $072A,y
	sta EnYRoomPos,x
	lda $072B,y
	sta EnXRoomPos,x
	lda $072C,y
	sta EnNameTable,x
	lda #$18
	sta EnRadX,x
	lda #$0C
	sta EnRadY,x
	ldy #$00
	jsr LF186
	jsr LF152
	jsr LF1BF
	jsr LF1FA
	bcc Exit13
	lda #$01
	sta EnDelay,x
	sta EnStatus,x
	and ScrollDir
	asl
	sta $0405,x
	ldy EnDataIndex,x
	jsr LFB7B
	jmp LF85A

*       sta EnDataIndex,x
	lda #$01
	sta EnDelay,x
	jmp KillObject			;($FA18)Free enemy data slot.

LFB7B:  jsr $80B0
	ror $0405,x
	lda EnemyInitDelayTbl,y		;($96BB)Load initial delay for enemy movement.
	sta EnDelay,x		;

Exit13: 
	rts				;Exit from multiple routines.

UnknownFB88:
LFB88:  ldx PageIndex
	jsr LF844
	lda $6B01,x
	inc $6B03,x
	dec $6B03,x
	bne +
	pha
	pla
*       bpl +
	jsr TwosCompliment		;($C3D4)
*       cmp #$08
	bcc +
	cmp #$10
	bcs Exit13
	tya
	and #$01
	tay
	lda $0085,y
	cmp EnResetAnimIndex,x
	beq Exit13
	sta EnAnimIndex,x
	dec EnAnimIndex,x

UnknownFBB9:
LFBB9:	sta EnResetAnimIndex,x
	jmp LF693

*       lda $963B,y
	cmp EnResetAnimIndex,x
	beq Exit13
	jmp LF68D

UnknownFBCA:
LFBCA:  ldx PageIndex
	jsr LF844
	lda $965B,y
	cmp EnResetAnimIndex,x
	beq Exit13
	sta EnResetAnimIndex,x
	jmp LF690

DestroyGeenSpinner:
LFBDD:  lda #$40
	sta PageIndex
	ldx #$0C
*       jsr LFBEC
	dex
	dex
	dex
	dex
	bne -
LFBEC:  lda $A0,x
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
	jsr UnknownFD8F
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
	jsr UnknownDC7F
	bcs +
	jsr IsScrewAttackActive		;($CD9C)Check if screw attack active.
	ldy #$00
	bcc +
	clc
	jsr LF311
	lda #$50
	sta HealthLoChange
	jsr SubtractHealth		;($CE92)
*       pla
	tax
*      rts

*     lda #$00
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
LFC65:  lda $6BE4
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
	jsr LFC98
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

LFC98:  lda $B0,x
	jsr ChooseRoutine

; Pointer table to code

	.word ExitSub       ;($C45C) rts
	.word LFCA5
	.word LFCB1
	.word LFCBA

LFCA5:  jsr LFD84
	jsr LFD08
	jsr LFD25
	jmp ClrObjCntrlIfFrameIsF7

LFCB1:  jsr LFD84
	jsr LFCC1
	jmp ClrObjCntrlIfFrameIsF7

LFCBA:  lda #$00
	sta $B0,x
	jmp SFX_EnemyHit

LFCC1:  jsr LFD5F
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
	jsr UnknownFD8F
	bcs +
	lda $B4,x
	ora #$02
	sta $B4,x
*       bcc +
	jsr LFD6C
*       lda $B5,x
	cmp #$50
	bcc +
	lda #$01
	sta $B0,x
*       rts

LFD08:  lda #$00
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

LFD25:  txa
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
	jsr LFD5F
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
*       sta $04
*      jsr UnknownFD8F
	jmp LFD6C

; Table used by above subroutine

Table18:
	.byte $02
	.byte $FE
	.byte $01
	.byte $FF
	.byte $02

LFD5F:  lda $B3,x
	sta $0B
	lda $B1,x
	sta $08
	lda $B2,x
	sta $09
	rts

LFD6C:  lda $08
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

LFD84:  lda $B6,x
	and #$04
	beq +
	lda #$03
	sta $B0,x
*       rts

UnknownFD8F:
LFD8F:  lda ScrollDir
	and #$02
	sta $02
	lda $04
	clc
	bmi +++
	beq LFDBF
	adc $08
	bcs +
	cmp #$F0
	bcc ++
*       adc #$0F
	ldy $02
	bne ClcExit2
	inc $0B
*       sta $08
	jmp LFDBF

*      adc $08
	bcs +
	sbc #$0F
	ldy $02
	bne ClcExit2
	inc $0B
*       sta $08
LFDBF:  lda $05
	clc
	bmi ++
	beq SecExit
	adc $09
	bcc +
	ldy $02
	beq ClcExit2
	inc $0B
*       jmp ++

*       adc $09
	bcs +
	ldy $02
	beq ClcExit2
	inc $0B
*       sta $09
	SecExit:
	sec
	rts

	ClcExit2:
	clc
*      rts


UpdateEndTimer:
LFDE3:  lda EndTimerHi
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
*       jsr LFE05
	txa
	sec
	sbc #$08
	tax
	bne -

LFE05:  lda $0758,x
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
*       jsr DoOneTile
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
	.word LFE3D
	.word LFE54
	.word LFE59
	.word LFE54
	.word LFE83

LFE3D:  inc TileRoutine,x
	lda #$00
	jsr SetTileAnim
	lda #$50
	sta TileDelay,x
	lda TileWRAMLo,x     ; low WRAM addr of blasted tile
	sta $00
	lda TileWRAMHi,x     ; high WRAM addr
	sta $01

LFE54:  lda #$02
	jmp UpdateTileAnim

LFE59:  lda FrameCount
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
*       rts

; Table used for indexing the animations in TileBlastAnim (see below)

Table19:
	.byte $18,$1C,$20,$00,$04,$08,$0C,$10,$24,$14

LFE83:  lda #$00
	sta TileRoutine,x       ; tile = respawned
	lda TileWRAMLo,x
	clc
	adc #$21
	sta $00
	lda TileWRAMHi,x
	sta $01
	jsr LFF3C
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
	jsr LF186
	lda #$04
	clc
	adc ObjRadY
	sta $04
	lda #$04
	clc
	adc ObjRadX
	sta $05
	jsr LF1FA
	bcs Exit23
	jsr LF311
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
Exit23: rts

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
*       ldx $05
*       ldy $10
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
*       lda $01
	and #$2F
	sta $01
	jsr EraseTile
	clc
	rts

LFF3C:  lda $00
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
LFFB0:	SEI				;Disables interrupt
LFFB1:	CLD				;Sets processor to binary mode
LFFB2:	LDX #$00			;
LFFB4:	STX PPUControl0			;Clear PPU control registers
LFFB7:	STX PPUControl1			;
LFFBA:*	LDA PPUStatus			;
LFFBD:	BPL -				;Wait for VBlank
LFFBF:*	LDA PPUStatus			;
LFFC2:	BPL -				;
LFFC4:	ORA #$FF			;
LFFC6:	STA MMC1Reg0			;Reset MMC1 chip
LFFC9:	STA MMC1Reg1			;(MSB is set)
LFFCC:	STA MMC1Reg2			;
LFFCF:	STA MMC1Reg3			;
LFFD2:	JMP Startup			;($C01A)Do preliminary housekeeping.

;Not used.
;LFFD5:	.byte $FF, $FF, $FF, $4C, $E4, $B3, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
;LFFE5:	.byte $FF, $FF, $FF, $FF, $4D, $45, $54, $52, $4F, $49, $44, $E4, $8D, $00, $00, $38
;LFFF5:	.byte $04, $01, $06, $01, $BC
.advance $FFFA
;-----------------------------------------[ Interrupt vectors ]--------------------------------------

LBFFA: 	.word NMI			;($C0D9)NMI vector.
LBFFC:	.word RESET			;($FFB0)Reset vector.
LBFFE: 	.word RESET			;($FFB0)IRQ vector.