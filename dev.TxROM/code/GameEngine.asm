; METROID Disassembly (c) 1986 NINTENDO
; Programmed by HAI YUKAMI, ZARU SOBAJIMA, GPZ SENGOKU, N.SHIOTANI, & M.HOUDAI
; Disassembled by Kent Hansen. Commented by Nick Mikstas.
; This version is organized and ported to use the MMC3.
; Game engine (Fixed ROM Bank 7)
.require "Defines.asm"

;---------------------------[ Forward declarations ]----------------------------
; These addresses are located in area banks.
.alias RunObjectRoutine         $8058
.alias Unknown80B0              $80B0
.alias Unknown81DA              $81DA
.alias Unknown8206              $8206
.alias Unknown820F              $820F
.alias Unknown8296              $8296
.alias Unknown832F              $832F
.alias ObjectAnimIndexTbl       $8572
.alias FramePtrTable            $860B
.alias PlacePtrTable            $86DF
.alias SamusEnterDoor           $8B13
.alias DisplayDoors             $8B79

.alias PalPntrTbl               $9560
.alias AreaPointers             $9598
.alias AreaRoutine              $95C3
.alias EnemyHitPointTbl         $962B
.alias EnemyDataTable8B         $968B
.alias EnemyInitDelayTbl        $96BB
.alias SpecItmsTable            $9598
; The sound engine is located in all area banks and the title bank:
.alias SoundEngine              $B3B4
; These are indexes in the Title bank address table.
.alias Bank0_TitleRoutine       $8000       ; Index 0
.alias Bank0_StarPalSwitch      $8002       ; Index 1
.alias Bank0_DecSpriteYCoord    $8004       ; Index 2
.alias Bank0_NMIScreenWrite     $8006       ; Index 3
.alias Bank0_EndGamePalWrite    $8008       ; Index 4
.alias Bank0_CopyMap            $800A       ; Index 5

;------------------------------[ Start of code ]--------------------------------
.org $C000
.include "GameEngine/BankSwitching.asm"
.include "GameEngine/Core.asm"
.include "GameEngine/Display.asm"
.include "GameEngine/Input.asm"
.include "GameEngine/Math.asm"
.include "GameEngine/PPU.asm"
.include "GameEngine/Rooms.asm"
.include "GameEngine/Sound.asm"
.include "GameEngine/StatusBar.asm"
    
;-------------------------------[ Main loop ]-----------------------------------

;The main loop runs all the routines that take place outside of the NMI.

MainLoop:
    jsr CheckBankSwitch             ;Check to see if memory page needs to be switched.
    jsr UpdateTimer                 ;Update Timers 1, 2 and 3.
    jsr GoMainRoutine               ;Go to main routine for updating game.
    inc FrameCount                  ;Increment frame counter.
    lda #$00                        ;
    sta NMIStatus                   ;Wait for next NMI to end.
    jmp WaitNMIEnd

;---------------------------------[ Update timer ]------------------------------

;This routine is used for timing - or for waiting around, rather.
;TimerDelay is decremented every frame. When it hits zero, $2A, $2B and $2C are
;decremented if they aren't already zero. The program can then check
;these variables (it usually just checks $2C) to determine when it's time
;to "move on". This is used for the various sequences of the intro screen,
;when the game is started, when Samus takes a special item, and when GAME
;OVER is displayed, to mention a few examples.

UpdateTimer:
    ldx #$01                        ;First timer to decrement is Timer2.
    dec TimerDelay                  ;
    bpl DecTimer                    ;
    lda #$09                        ;TimerDelay hits #$00 every 10th frame.
    sta TimerDelay                  ;Reset TimerDelay after it hits #$00.
    ldx #$02                        ;Decrement Timer3 every 10 frames.

DecTimer:
    lda Timer1,x                    ;
    beq +                           ;Don't decrease if timer is already zero.
    dec Timer1,x                    ;
*   dex                             ;Timer1 and Timer2 decremented every frame.
    bpl DecTimer                    ;
    rts                             ;
    
;----------------------------------------[ GoMainRoutine ]-------------------------------------------

;This is where the real code of each frame is executed.
;MainRoutine or TitleRoutine (Depending on the value of GameMode)
;is used as an index into a code pointer table, and this routine
;is executed.

GoMainRoutine:
    lda GameMode                    ;0 if game is running, 1 if at intro screen.
    beq +                           ;Branch if mode=Play.
    jmp (Bank0_TitleRoutine)        ;Jump to address in Bank 0, where a routine similar to the one-->
                                    ;below is executed, only using TitleRoutine instead
                                    ;of MainRoutine as index into a jump table.
*   lda Joy1Change                  ;
    and #$10                        ;Has START been pressed?-->
    beq +++                         ;if not, execute current routine as normal.

    lda MainRoutine                 ;
    cmp #$03                        ;Is game engine running?-->
    beq +                           ;If yes, check for routine #5 (pause game).
    cmp #$05                        ;Is game paused?-->
    bne +++                         ;If not routine #5 either, don't care about START being pressed.
    lda #$03                        ;Otherwise, execute routine #3 (game engine).
    bne ++                          ;Branch always.
*   lda #$05                        ;Execute pause routine.
*   sta MainRoutine                 ;(MainRoutine = 5 if game paused, 3 if game engine running).
    lda GamePaused                  ;
    eor #$01                        ;Toggle game paused.
    sta GamePaused                  ;
    jsr PauseMusic                  ;Silences music while game paused.

*   lda MainRoutine                 ;
    jsr ChooseRoutine               ;Use MainRoutine as index into routine table below.

;Pointer table to code.

    .word AreaInit                  ;Area init.
    .word MoreInit                  ;More area init.
    .word SamusInit                 ;Samus init.
    .word GameEngine                ;Game engine.
    .word GameOver                  ;Display GAME OVER.
    .word PauseMode                 ;Pause game.
    .word GoPassword                ;Display password.
    .word IncrementRoutine          ;Just advances to next routine in table.
    .word SamusIntro                ;Intro.
    .word WaitTimer                 ;Delay.

IncrementRoutine:
    inc MainRoutine                 ;Increment to next routine in above table.
    rts                             ;

;---------------------------[Clear RAM $33 thru $DF]----------------------------
;The routine below clears RAM associated with rooms and enemies. It is called
;only once, from the Title ROM bank, when the game is initialized (either as a
;new game or loaded from a password).
ClearRAM_33_DF:
    ldx #$33                        ;
    lda #$00                        ;
*   sta $00,x                       ;Clear RAM addresses $33 through $DF.
    inx                             ;
    cpx #$E0                        ;
    bcc -                           ;Loop until all addresses are cleared.
    rts                             ;

;-----------------------------------------[ Choose routine ]-----------------------------------------

;This is an indirect jump routine. A is used as an index into a code
;pointer table, and the routine at that position is executed. The programmers
;always put the pointer table itself directly after the JSR to ChooseRoutine,
;meaning that its address can be popped from the stack.

ChooseRoutine:
LC27C:  asl                             ;* 2, each ptr is 2 bytes (16-bit).
LC27D:  sty TempY                       ;Temp storage.
LC27F:  stx TempX                       ;Temp storage.
LC281:  tay                             ;
LC282:  iny                             ;
LC283:  pla                             ;Low byte of ptr table address.
LC284:  sta CodePtr                     ;
LC286:  pla                             ;High byte of ptr table address.
LC287:  sta CodePtr+1                   ;
LC289:  lda (CodePtr),y                 ;Low byte of code ptr.
LC28B:  tax                             ;
LC28C:  iny                             ;
LC28D:  lda (CodePtr),y                 ;High byte of code ptr.
LC28F:  sta CodePtr+1                   ;
LC291:  stx CodePtr                     ;
LC293:  ldx TempX                       ;Restore X.
LC295:  ldy TempY                       ;Restore Y.
LC297:  jmp (CodePtr)                   ;

;--------------------------------------[ Timer routines ]--------------------------------------------

;The following routines set the timer and decrement it. The timer is set after Samus dies and
;before the GAME OVER message is dispayed.  The timer is also set while the item pickup music
;is playing.

WaitTimer:
LC494:  lda Timer3                      ;Exit if timer hasn't hit zero yet
LC496:  bne +                           ;
LC498:  lda NextRoutine                 ;Set GameOver as next routine.
LC49A:  cmp #$04                        ;
LC49C:  beq SetMainRoutine              ;Set GoPassword as main routine.
LC49E:  cmp #$06                        ;
LC4A0:  beq SetMainRoutine              ;
LC4A2:  jsr StartMusic                  ;Assume power up was picked up and GameEngine-->
LC4A5:  lda NextRoutine                 ;is next routine. Start area music before exiting.

SetMainRoutine:
LC4A7:  sta MainRoutine                 ;Set next routine to run.
LC4A9:* rts                             ;

SetTimer:
LC4AA:  sta Timer3                      ;Set Timer3. Frames to wait is value stored in A*10.
LC4AC:  stx NextRoutine                 ;Save routine to jump to after Timer3 expires.
LC4AE:  lda #$09                        ;Next routine to run is WaitTimer.
LC4B0:  bne SetMainRoutine              ;Branch always.

;-------------------------------------------[ AreaInit ]---------------------------------------------

AreaInit:
    lda #$00                        ;
    sta ScrollX                     ;Clear ScrollX.
    sta ScrollY                     ;Clear ScrollY.
    lda PPUCNT0ZP                   ;       
    and #$FC                        ;Sets nametable address = $2000.
    sta PPUCNT0ZP                   ;
    inc MainRoutine                 ;Increment MainRoutine to MoreInit.
    jsr EraseAllSprites             ;Clear all sprite info.
    lda #$10                        ;Prepare to load Brinstar memory page.
    jsr IsEngineRunning             ;Check to see if ok to switch lower memory page.

;------------------------------------------[ MoreInit ]---------------------------------------------

MoreInit:
LC81D:  ldy #$01                        ;
LC81F:  sty PalDataPending              ;Palette data pending = yes.
LC821:  ldx #$00                        ;
LC826:  stx AtEnding                    ;Not playing ending scenes.
LC829:  stx DoorStatus                  ;Samus not in door.
LC82B:  stx SamusDoorData               ;Samus is not inside a door.
LC82D:  stx UpdatingProjectile          ;No projectiles need to be updated.
LC82F:  txa                             ;A=0.

LC830:* cpx #$65                        ;Check to see if more RAM to clear in $7A thru $DE.
LC832:  bcs +                           ;
LC834:  sta $7A,x                       ;Clear RAM $7A thru $DE.
LC836:* cpx #$FF                        ;Check to see if more RAM to clear in $300 thru $3FE.
LC838:  bcs +                           ;
LC83A:  sta ObjAction,x                 ;Clear RAM $300 thru $3FE.
LC83D:* inx                             ;
LC83E:  bne ---                         ;Loop until all required RAM is cleared.

LC840:  jsr ScreenOff                   ;Turn off Background and visibility.
LC843:  jsr ClearNameTables             ;Clear screen data.
LC846:  jsr EraseAllSprites             ;Erase all sprites from sprite RAM.
LC849:  jsr DestroyEnemies              ;

        stx DoorOnNameTable3            ;Clear data about doors on the name tables.
        stx DoorOnNameTable0            ;
        inx                             ;X=1.
        inx                             ;X=2.
LC854:  stx ScrollDir                   ;Set initial scroll direction as left.

        lda $95D7                       ;Get Samus start x pos on map.
        sta MapPosX                     ;
        lda $95D8                       ;Get Samus start y pos on map.
        sta MapPosY                     ;

LC860:  lda $95DA       ; Get ??? Something to do with palette switch
        sta PalToggle
        lda #$FF
        sta RoomNumber                  ;Room number = $FF(undefined room).
LC869:  jsr CopyPtrs                    ; copy pointers from ROM to RAM 
LC86C:  jsr GetRoomNum                  ;Put room number at current map pos in $5A.
*       jsr SetupRoom                   ;
        ldy RoomNumber                  ; load room number
        iny
        bne -

        ldy CartRAMPtr+1
        sty $01
        ldy CartRAMPtr
        sty $00
        lda PPUCNT0ZP
        and #$FB        ; PPU increment = 1
        sta PPUCNT0ZP
        sta PPUControl0
        ldy PPUStatus   ; reset PPU addr flip/flop

; Copy room RAM #0 ($6000) to PPU Name Table #0 ($2000)

        ldy #$20
        sty PPUAddress
        ldy #$00
        sty PPUAddress
        ldx #$04        ; prepare to write 4 pages
*       lda ($00),y
        sta PPUIOReg
        iny
        bne -
        inc $01
        dex
        bne -

        stx $91
        inx      ; X = 1
        stx PalDataPending
        inc MainRoutine                 ;SamusInit is next routine to run.
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
LC8BB:  lda #$00
        tax
*       cpx #$48
        bcs +
        sta $97,x
*       sta EnStatus,x
        pha
        pla
        inx
        bne --
        stx MetroidOnSamus              ;Samus had no Metroid stuck to her.
        jmp $95AB

; SamusInit
; =========
; Code that sets up Samus, when the game is first started.

SamusInit:
LC8D1:  lda #$08                        ;
LC8D3:  sta MainRoutine                 ;SamusIntro will be executed next frame.
LC8D5:  lda #$2C                        ;440 frames to fade in Samus(7.3 seconds).
LC8D7:  sta Timer3                      ;
LC8D9:  jsr IntroMusic                  ;Start the intro music.
LC8DC:  ldy #sa_FadeIn0                 ;
        sty ObjAction                   ;Set Samus status as fading onto screen.
        ldx #$00
        stx SamusBlink
        dex                             ;X = $FF
        stx $0728
        stx $0730
        stx $0732
        stx $0738
        stx EndTimerLo                  ;Set end timer bytes to #$FF as-->
        stx EndTimerHi                  ;escape timer not currently active.
        stx $8B
        stx $8E
        ldy #$27
        lda InArea
        and #$0F
        beq +                           ;Branch if Samus starting in Brinstar.
        lsr ScrollDir                   ;If not in Brinstar, change scroll direction from left-->
        ldy #$2F                        ;to down. and set PPU for horizontal mirroring.
*   sty MirrorCntrl                     ;
        sty MaxMissilePickup
        sty MaxEnergyPickup
        lda $95D9                       ;Samus' initial vertical position
        sta ObjectY                     ;
        lda #$80                        ;Samus' initial horizontal position
        sta ObjectX                     ;
        lda PPUCNT0ZP                   ;
        and #$01                        ;Set Samus' name table position to current name table-->
        sta ObjectHi                    ;active in PPU.
        lda #$00                        ;
        sta HealthLo                    ;Starting health is-->
        lda #$03                        ;set to 30 units.
        sta HealthHi                    ;
*   rts                         ;

;------------------------------------[ Main game engine ]--------------------------------------------

GameEngine:
LC92B:  jsr ScrollDoor                  ;Scroll doors, if needed. 2 routine calls scrolls-->
LC92E:  jsr ScrollDoor                  ;twice as fast as 1 routine call.

LC931:  lda NARPASSWORD                 ;
LC934:  beq +                           ;
LC936:  lda #$03                        ;The following code is only accessed if -->
LC938:  sta HealthHi                    ;NARPASSWORD has been entered at the -->
LC93B:  lda #$FF                        ;password screen. Gives you new health,-->
LC93D:  sta SamusGear                   ;missiles and every power-up every frame.
LC940:  lda #$05                        ;
LC942:  sta MissileCount                ;

LC945:* jsr UpdateWorld                 ;Update Samus, enemies and room tiles.
LC948:  lda MiniBossKillDelay           ;
LC94B:  ora PowerUpDelay                ;Check if mini boss was just killed or powerup aquired.-->
LC94E:  beq +                           ;If not, branch.

LC950:  lda #$00                        ;
LC952:  sta MiniBossKillDelay           ;Reset delay indicators.
LC955:  sta PowerUpDelay                ;
LC958:  lda #$18                        ;Set timer for 240 frames(4 seconds).
LC95A:  ldx #$03                        ;GameEngine routine to run after delay expires
LC95C:  jsr SetTimer                    ;Set delay timer and game engine routine.

LC95F:* lda ObjAction                   ;Check is Samus is dead.
LC962:  cmp #sa_Dead2                   ;Is Samus dead?-->
LC964:  bne ---                         ;exit if not.
LC966:  lda AnimDelay                   ;Is Samus still exploding?-->
LC969:  bne ---                         ;Exit if still exploding.
LC96B:  jsr SilenceMusic                ;Turn off music.
LC96E:  lda MotherBrainStatus           ;
LC970:  cmp #$0A                        ;Is mother brain already dead? If so, branch.
LC972:  beq +                           ;
LC974:  lda #$04                        ;Set timer for 40 frames (.667 seconds).
LC976:  ldx #$04                        ;GameOver routine to run after delay expires.
LC978:  jmp SetTimer                    ;Set delay timer and run game over routine.

LC97B:* inc MainRoutine                 ;Next routine to run is GameOver.
LC97D:  rts                             ;

;----------------------------------------[ Update age ]----------------------------------------------

;This is the routine which keeps track of Samus' age. It is called in the
;NMI. Basically, this routine just increments a 24-bit variable every
;256th frame. (Except it's not really 24-bit, because the lowest age byte
;overflows at $D0.)

UpdateAge:
LC97E:  lda GameMode                    ;
LC980:  bne ++                          ;Exit if at title/password screen.
LC982:  lda MainRoutine                 ;
LC984:  cmp #$03                        ;Is game engine running?
LC986:  bne ++                          ;If not, don't update age.
LC988:  ldx FrameCount                  ;Only update age when FrameCount is zero-->
LC98A:  bne ++                          ;(which is approx. every 4.266666666667 seconds).
LC98C:  inc SamusAge,x                  ;Minor Age = Minor Age + 1.
LC98F:  lda SamusAge                    ;
LC992:  cmp #$D0                        ;Has Minor Age reached $D0?-->
LC994:  bcc ++                          ;If not, we're done.-->
LC996:  lda #$00                        ;Else reset minor age.
LC998:  sta SamusAge                    ;
LC99B:* cpx #$03                        ;
LC99D:  bcs +                           ;Loop to update middle age and possibly major age.
LC99F:  inx                             ;
LC9A0:  inc SamusAge,x                  ;
LC9A3:  beq -                           ;Branch if middle age overflowed, need to increment--> 
LC9A5:* rts                             ;major age too. Else exit.

;-------------------------------------------[ Game over ]--------------------------------------------

GameOver:
LC9A6:  lda #$1C                        ;GameOver is the next routine to run.
LC9A8:  sta TitleRoutine                ;
LC9AA:  lda #$01                        ;
LC9AC:  sta SwitchPending               ;Prepare to switch to title memory page.
LC9AE:  jmp ScreenOff                   ;Turn screen off.

;------------------------------------------[ Pause mode ]--------------------------------------------

PauseMode:
LC9B1:  lda Joy2Status                  ;Load buttons currently being pressed on joypad 2.
LC9B3:  and #$88                        ;
LC9B5:  eor #$88                        ;both A & UP pressed?-->
LC9B7:  bne Exit14                      ;Exit if not.
LC9B9:  ldy EndTimerHi                  ;
LC9BC:  iny                             ;Is escape timer active?-->
LC9BD:  bne Exit14                      ;Sorry, can't quit if this is during escape scence.
LC9BF:  sta GamePaused                  ;Clear pause game indicator.
LC9C1:  inc MainRoutine                 ;Display password is the next routine to run.

Exit14:
LC9C3:  rts                             ;Exit for routines above and below.

;------------------------------------------[ GoPassword ]--------------------------------------------

GoPassword:
LC9C4:  lda #$19                        ;DisplayPassword is next routine to run.
LC9C6:  sta TitleRoutine                ;
LC9C8:  lda #$01                        ;
LC9CA:  sta SwitchPending               ;Prepare to switch to intro memory page.
LC9CC:  lda NoiseSFXFlag                ;
LC9CF:  ora #$01                        ;Silence music.
LC9D1:  sta NoiseSFXFlag                ;
LC9D4:  jmp ScreenOff                   ;Turn off screen.

;-----------------------------------------[ Samus intro ]--------------------------------------------

SamusIntro:
LC9D7:  jsr EraseAllSprites             ;Clear all sprites off screen.
LC9DA:  ldy ObjAction                   ;Load Samus' fade in status.
LC9DD:  lda Timer3                      ;
LC9E0:  bne +                           ;Branch if Intro still playing.
        
;Fade in complete.
LC9E2:  sta ItemRoomMusicStatus         ;Make sure item room music is not playing.
LC9E4:  lda #sa_Begin                   ;Samus facing forward and can't be hurt.
LC9E6:  sta ObjAction                   ;
LC9E8:  jsr StartMusic                  ;Start main music.
LC9EB:  jsr SelectSamusPal              ;Select proper Samus palette.
LC9EE:  lda #$03                        ;
LC9F0:  sta MainRoutine                 ;Game engine will be called next frame.

;Still fading in.
LC9F2:* cmp #$1F                        ;When 310 frames left of intro, display Samus.
LC9F4:  bcs Exit14                      ;Branch if not time to start drawing Samus.
LC9F6:  cmp SamusFadeInTimeTbl-20,y     ;sa_FadeIn0 is beginning of table.
LC9F9:  bne +                           ;Every time Timer3 equals one of the entries in the table-->
LC9FB:  inc ObjAction                   ;below, change the palette used to color Samus.
LC9FE:  sty PalDataPending              ;
LCA00:* lda FrameCount                  ;Is game currently on an odd frame?-->
LCA02:  lsr                             ;If not, branch to exit.
LCA03:  bcc Exit14                      ;Only display Samus on odd frames [the blink effect].
LCA05:  lda #an_SamusFront              ;Samus front animation is animation to display.-->
LCA07:  jsr SetSamusAnim                ;while fading in.
LCA0A:  lda #$00                        ;
LCA0C:  sta SpritePagePos               ;Samus sprites start at Sprite00RAM.
LCA0E:  sta PageIndex                   ;Samus RAM is first set of RAM.
LCA10:  jmp AnimDrawObject              ;Draw Samus on screen.

;The following table marks the time remaining in Timer3 when a palette change should occur during
;the Samus fade-in sequence. This creates the fade-in effect.

SamusFadeInTimeTbl:
LCA13:  .byte $1E,$14,$0B,$04,$FF

;---------------------------------[ Check if game engine running ]-----------------------------------

IsEngineRunning:
LCA18:  ldy MainRoutine                 ;If Samus is fading in or the wait timer is-->
LCA1A:  cpy #$07                        ;active, return from routine.
LCA1C:  beq +                           ;
LCA1E:  cpy #$03                        ;Is game engine running?
LCA20:  beq SwitchBank                  ;If yes, branch to SwitchBank.
LCA22:* rts                             ;Exit if can't switch bank.

;-----------------------------------------[ Switch bank ]--------------------------------------------

;Switch to appropriate area bank

SwitchBank:
LCA23:  sta InArea                      ;Save current area Samus is in.
LCA25:  and #$0F                        ;
LCA27:  tay                             ;Use 4 LSB to load switch pending offset from BankTable table.
LCA28:  lda BankTable,y                 ;Base is BankTable.
LCA2B:  sta SwitchPending               ;Store switch data.
LCA2D:  jmp CheckBankSwitch             ;Switch lower 16KB to appropriate memory page.

;Table used by above subroutine.
;Each value is the area bank number plus one.

BankTable:
LCA30:  .byte $02                       ;Brinstar.
LCA31:  .byte $03                       ;Norfair.
LCA32:  .byte $05                       ;Kraid hideout.
LCA33:  .byte $04                       ;Tourian.
LCA34:  .byte $06                       ;Ridley hideout.

;----------------------------------------[ Choose ending ]-------------------------------------------

;Determine what type of ending is to be shown, based on Samus' age
ChooseEnding:
LCAF5:  ldy #$01                        ;
LCAF7:* lda SamusAge+2                  ;If SamusAge+2 anything but #$00, load worst-->
LCAFA:  bne +                           ;ending(more than 37 hours of gameplay).
LCAFC:  lda SamusAge+1                  ;
LCAFF:  cmp AgeTable-1,y                ;Loop four times to determine-->
LCB02:  bcs +                           ;ending type from table below.
LCB04:  iny                             ;
LCB05:  cpy #$05                        ;
LCB07:  bne -                           ;
LCB09:* sty EndingType                  ;Store the ending # (1..5), 5=best ending
LCB0C:  lda #$00                        ;
LCB0E:  cpy #$04                        ;Was the best or 2nd best ending achieved?
LCB10:  bcc +                           ;Branch if not (suit stays on)
LCB12:  lda #$01                        ;
LCB14:* sta JustInBailey                ;Suit OFF, baby!
LCB17:  rts                             ;

;Table used by above subroutine to determine ending type.
AgeTable:
LCB18:  .byte $7A                       ;Max. 37 hours
LCB19:  .byte $16                       ;Max. 6.7 hours
LCB1A:  .byte $0A                       ;Max. 3.0 hours
LCB1B:  .byte $04                       ;Best ending. Max. 1.2 hours

;----------------------------------------------------------------------------------------------------

; ===== THE REAL GUTS OF THE GAME ENGINE! =====

UpdateWorld:
LCB29:  ldx #$00                        ;Set start of sprite RAM to $0200.
LCB2B:  stx SpritePagePos               ;

LCB2D:  jsr UpdateEnemies               ;Display of enemies.
LCB30:  jsr UpdateProjectiles           ;Display of bullets/missiles/bombs.
LCB33:  jsr UpdateSamus                 ;Display/movement of Samus.
LCB36:  jsr AreaRoutine                 ;Area specific routine.
LCB39:  jsr UpdateElevator              ;Display of elevators.
LCB3C:  jsr UpdateStatues               ;Display of Ridley & Kraid statues.
LCB3F:  jsr LFA9D                       ; destruction of enemies
LCB42:  jsr LFC65                       ; update of Mellow/Memu enemies
LCB45:  jsr LF93B                       ;
LCB48:  jsr LFBDD                       ; destruction of green spinners
LCB4B:  jsr SamusEnterDoor              ;Check if Samus entered a door.
LCB4E:  jsr DisplayDoors                ; display of doors
LCB51:  jsr UpdateTiles                 ; tile de/regeneration
LCB54:  jsr LF034                       ; Samus <--> enemies crash detection
LCB57:  jsr DisplayBar                  ;Display of status bar.
        jsr LFAF2                       ;
        jsr CheckMissileToggle          ;
        jsr UpdateItems                 ; display of special items
        jsr LFDE3

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
        asl                             ;CF contains Varia status (1 = Samus has it)
        lda MissileToggle               ;A = 1 if Samus is firing missiles, else 0
        rol                             ;Bit 0 of A = 1 if Samus is wearing Varia
        adc #$02
        ldy JustInBailey                ;In suit?
        beq +                           ;Branch if yes
        clc
        adc #$17                        ;Add #$17 to the pal # to reach "no suit"-palettes
*       sta PalDataPending              ;Palette will be written next NMI
        pla
        tay
        rts

;--------------------------------------[ Update Samus ]----------------------------------------------

UpdateSamus:
LCC0D:  ldx #$00                        ;Samus data is located at index #$00.
LCC0F:  stx PageIndex                   ;
LCC11:  inx                             ;x=1.
LCC12:  stx IsSamus                     ;Indicate Samus is the object being updated.
LCC14:  jsr GoSamusHandler              ;Find proper Samus handler routine.
LCC17:  dec IsSamus                     ;Update of Samus complete.
LCC19:  rts                             ;

GoSamusHandler:
LCC1A:  lda ObjAction                   ;
LCC1D:  bmi SamusStand                  ;Branch if Samus is standing.
LCC1F:  jsr ChooseRoutine               ;Goto proper Samus handler routine.

;Pointer table for Samus' action handlers.

LCC22:  .word SamusStand                ;Standing.
LCC24:  .word SamusRun                  ;Running.
LCC26:  .word SamusJump                 ;Jumping.
LCC28:  .word SamusRoll                 ;Rolling.
LCC2A:  .word SamusPntUp                ;Pointing up.
LCC2C:  .word SamusDoor                 ;Inside door while screen scrolling.
LCC2E:  .word SamusJump                 ;Jumping while pointing up.
LCC30:  .word SamusDead                 ;Dead.
LCC32:  .word SamusDead2                ;More dead.
LCC34:  .word SamusElevator             ;Samus on elevator.

;---------------------------------------[ Samus standing ]-------------------------------------------

SamusStand:
LCC36:  lda Joy1Status                  ;Status of joypad 1.
LCC38:  and #$CF                        ;Remove SELECT & START status bits.
LCC3A:  beq +                           ;Branch if no buttons pressed.
LCC3C:  jsr ClearHorzMvmtAnimData       ;Set no horiontal movement and single frame animation.
LCC3F:  lda Joy1Status                  ;
LCC41:* and #$07                        ;Keep status of DOWN/LEFT/RIGHT.
LCC43:  bne +                           ;Branch if any are pressed.
LCC45:  lda Joy1Change                  ;
LCC47:  and #$08                        ;Check if UP was pressed last frame.-->
LCC49:  beq +++                         ;If not, branch.
LCC4B:* jsr BitScan                     ;Find which directional button is pressed.
LCC4E:  cmp #$02                        ;Is down pressed?-->
LCC50:  bcs +                           ;If so, branch.
LCC52:  sta SamusDir                    ;1=left, 0=right.
LCC54:* tax                             ;
LCC55:  lda ActionTable,x               ;Load proper Samus status from table below.
LCC58:  sta ObjAction                   ;Save Samus status.
LCC5B:* lda Joy1Change                  ;
LCC5D:  ora Joy1Retrig                  ;Check if fire was just pressed or needs to retrigger.
LCC5F:  asl                             ;
LCC60:  bpl +                           ;Branch if FIRE not pressed.
LCC62:  jsr FireWeapon                  ;Shoot left/right.
LCC65:* bit Joy1Change                  ;Check if jump was just pressed.
LCC67:  bpl +                           ;Branch if JUMP not pressed.
LCC69:  lda #sa_Jump                    ;
LCC6B:  sta ObjAction                   ;Set Samus status as jumping.
LCC6E:* lda #$04                        ;Prepare to set animation delay to 4 frames.
LCC70:  jsr SetSamusData                ;Set Samus control data and animation.
LCC73:  lda ObjAction                   ;
LCC76:  cmp #sa_Door                    ;Is Samus inside a door, dead or pointing up and jumping?-->
LCC78:  bcs +                           ;If so, branch to exit.
LCC7A:  jsr ChooseRoutine               ;Select routine below.

;Pointer table to code.

LCC7D:  .word ExitSub                   ;Rts.
LCC7F:  .word SetSamusRun               ;Samus is running.
LCC81:  .word SetSamusJump              ;Samus is jumping.
LCC83:  .word SetSamusRoll              ;Samus is in a ball.
LCC85:  .word SetSamusPntUp             ;Samus is pointing up.

;Table used by above subroutine.

ActionTable:
LCC87:  .byte sa_Run                    ;Run right.
LCC88:  .byte sa_Run                    ;Run left.
LCC89:  .byte sa_Roll
LCC8A:  .byte sa_PntUp

;----------------------------------------------------------------------------------------------------

SetSamusExplode:
LCC8B:  lda #$50
        sta SamusJumpDsplcmnt
        lda #an_Explode
        jsr SetSamusAnim
        sta ObjectCounter
*       rts

SetSamusRun:
LCC98:  lda #$09
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
LCCB7:  lda RunAccelerationTbl,x
        sta SamusHorzAccel
        rts

RunAnimationTbl:
LCCBE:  .byte an_SamusRun
        .byte an_SamusRunPntUp

RunAccelerationTbl:
LCCC0:  .byte $30                       ;Accelerate right.
        .byte $D0                       ;Accelerate left.

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
        bcc ++++          ; branch always
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
        jsr StopVertMovement            ;
*       lda #an_SamusRun
        cmp AnimResetIndex
        bne +
        lda #an_SamusJump
        sta AnimResetIndex
*       lda SamusInLava
        beq +
        lda Joy1Change
        bmi LCD40       ; branch if JUMP pressed
*       jsr LCF88
        jsr LD09C
        jsr LCF2E
        lda #$02
        bne SetSamusData       ; branch always
*       lda SamusOnElevator
        bne +
        jsr LCCB7
*       jsr LCDBF
        dec WalkSoundDelay  ; time to play walk sound?
        bne +      ; branch if not
        lda #$09
        sta WalkSoundDelay  ; # of frames till next walk sound trigger
        jsr SFX_SamusWalk
*       jsr LCF2E
        lda Joy1Change
        bpl +      ; branch if JUMP not pressed
LCD40:  jsr LCFC3
        lda #$12
        sta SamusHorzSpeedMax
        jmp LCD6B

*       ora Joy1Retrig
        asl
        bpl +      ; branch if FIRE not pressed
        jsr LCDD7
*       lda Joy1Status
        and #$03
        bne +
        jsr StopHorzMovement
        jmp LCD6B

*       jsr BitScan                     ;
        cmp SamusDir
        beq LCD6B
        sta SamusDir
        jsr LCC98
LCD6B:  lda #$03

;---------------------------------------[ Set Samus data ]-------------------------------------------

;The following function sets various animation and control data bytes for Samus.

SetSamusData:
LCD6D:  jsr UpdateObjAnim               ;Update animation if needed.
LCD70:  jsr IsScrewAttackActive         ;Check if screw attack active to change palette.
LCD73:  bcs +                           ;If screw attack not active, branch to skip palette change.
LCD75:  lda FrameCount                  ;
LCD77:  lsr                             ;
LCD78:  and #$03                        ;Every other frame, change Samus palette while screw-->
LCD7A:  ora #$A0                        ;Attack is active.
LCD7C:  sta ObjectCntrl                 ;
LCD7E:* jsr CheckHealthStatus           ;Check if Samus hit, blinking or Health low.
LCD81:  jsr LavaAndMoveCheck            ;Check if Samus is in lava or moving.
LCD84:  lda MetroidOnSamus              ;Is a Metroid stuck to Samus?-->
LCD86:  beq +                           ;If not, branch.
LCD88:  lda #$A1                        ;Metroid on Samus. Turn Samus blue.
LCD8A:  sta ObjectCntrl                 ;
LCD8C:* jsr SetmirrorCntrlBit           ;Mirror Samus, if necessary.
LCD8F:  jmp DrawFrame                   ;Display Samus.

;---------------------------------[ Set mirror control bit ]-----------------------------------------

SetmirrorCntrlBit:
LCD92:  lda SamusDir                    ;Facing left=#$01, facing right=#$00.
LCD94:  jsr Amul16                      ;*16. Move bit 0 to bit 4 position.
LCD97:  ora ObjectCntrl                 ;
LCD99:  sta ObjectCntrl                 ;Use SamusDir bit to set mirror bit.
LCD9B:  rts                             ;

;------------------------------[ Check if screw attack is active ]-----------------------------------

IsScrewAttackActive:
LCD9C:  sec                             ;Assume screw attack is not active.
LCD9D:  ldy ObjAction                   ;
LCDA0:  dey                             ;Is Samus running?-->
LCDA1:  bne ++                          ;If not, branch to exit.
LCDA3:  lda SamusGear                   ;
LCDA6:  and #gr_SCREWATTACK             ;Does Samus have screw attack?-->
LCDA8:  beq ++                          ;If not, branch to exit.
LCDAA:  lda AnimResetIndex              ;
LCDAD:  cmp #an_SamusSalto              ;Is Samus summersaulting?-->
LCDAF:  beq +                           ;If so, branch to clear carry(screw attack active).
LCDB1:  cmp #an_SamusJump               ;
LCDB3:  sec                             ;Is Samus jumping?-->
LCDB4:  bne ++                          ;If not, branch to exit.
LCDB6:  bit ObjVertSpeed                ;If Samus is jumping and still moving upwards, screw--> 
LCDB9:  bpl ++                          ;attack is active.
LCDBB:* cmp AnimIndex                   ;Screw attack will still be active if not spinning, but-->
LCDBE:* rts                             ;jumping while running and still moving upwards.

;----------------------------------------------------------------------------------------------------

LCDBF:  lda Joy1Status
        and #$08
        lsr
        lsr
        lsr
        tax
        lda LCCBE,x
        cmp AnimResetIndex
        beq -
        jsr SetSamusAnim
        pla
        pla
        jmp LCD6B

LCDD7:  jsr FireWeapon                  ;Shoot left/right.
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
LCDFA:  lda SamusHit                    ;
        and #$20                        ;Has Samus been hit?-->
        beq +++                         ;If not, branch to check if still blinking from recent hit.
        lda #$32                        ;
        sta SamusBlink                  ;Samus has been hit. Set blink for 32 frames.
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
*       lda #$FD
        sta ObjVertSpeed
        lda #$38                        ;Samus is hit. Store Samus hit gravity.
        sta SamusGravity                ;
        jsr IsSamusDead
        bne +
        jmp CheckHealthBeep

*       lda SamusBlink
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
        jsr LCF4E
*       dex
        bne +
        jsr TwosCompliment              ;
*       sta ObjHorzSpeed
*       lda $77
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
        bne +                           ;Only beep every 16th frame.
        jsr SFX_Beep
*       lda #$00
        sta SamusHit
LCE83:  rts

;----------------------------------------[ Is Samus dead ]-------------------------------------------

IsSamusDead:
LCE84:  lda ObjAction                   ;
LCE87:  cmp #sa_Dead                    ;
LCE89:  beq Exit3                       ;Samus is dead. Zero flag is set.
LCE8B:  cmp #sa_Dead2                   ;
LCE8D:  beq Exit3                       ;
LCE8F:  cmp #$FF                        ;Samus not dead. Clear zero flag.

Exit3:  
LCE91:  rts                             ;Exit for routines above and below.

;----------------------------------------[ Subtract health ]-----------------------------------------

SubtractHealth:
LCE92:  lda HealthLoChange              ;Check to see if health needs to be changed.-->
LCE94:  ora HealthHiChange              ;If not, branch to exit.
LCE96:  beq Exit3                       ;
LCE98:  jsr IsSamusDead                 ;Check if Samus is already dead.
LCE9B:  beq ClearDamage                 ;Samus is dead. Branch to clear damage values.
LCE9D:  ldy EndTimerHi                  ;If end escape timer is running, Samus cannot be hurt.
LCEA0:  iny                             ;
LCEA1:  beq +                           ;Branch if end escape timer not active.

ClearDamage:
LCEA3:  jmp ClearHealthChange           ;Clear health change values.

LCEA6:* lda MotherBrainStatus           ;If mother brain is in the process of dying, receive-->
LCEA8:  cmp #$03                        ;no damage.
LCEAA:  bcs ClearDamage                 ;

LCEAC:  lda SamusGear                   ;
LCEAF:  and #gr_VARIA                   ;Check is Samus has Varia.
LCEB1:  beq +                           ;
LCEB3:  lsr HealthLoChange              ;If Samus has Varia, divide damage by 2.
LCEB5:  lsr HealthHiChange              ;
LCEB7:  bcc +                           ;If HealthHi moved a bit into the carry flag while-->
LCEB9:  lda #$4F                        ;dividing, add #$4F to HealthLo for proper-->
LCEBB:  adc HealthLoChange              ;division results.
LCEBD:  sta HealthLoChange              ;

LCEBF:* lda HealthLo                    ;Prepare to subtract from HealthLo.
LCEC2:  sta $03                         ;
LCEC4:  lda HealthLoChange              ;Amount to subtract from HealthLo.
LCEC6:  sec                             ;
LCEC7:  jsr Base10Subtract              ;Perform base 10 subtraction.
LCECA:  sta HealthLo                    ;Save results.

LCECD:   lda HealthHi                   ;Prepare to subtract from HealthHi.
LCED0:  sta $03                         ;
LCED2:  lda HealthHiChange              ;Amount to subtract from HealthHi.
LCED4:  jsr Base10Subtract              ;Perform base 10 subtraction.
LCED7:  sta HealthHi                    ;Save Results.

LCEDA:  lda HealthLo                    ;
LCEDD:  and #$F0                        ;Is Samus health at 0?  If so, branch to-->
LCEDF:  ora HealthHi                    ;begin death routine.
LCEE2:  beq +                           ;
LCEE4:  bcs ++                          ;Samus not dead. Branch to exit.

LCEE6:* lda #$00                        ;Samus is dead.
LCEE8:  sta HealthLo                    ;
LCEEB:  sta HealthHi                    ;Set health to #$00.
LCEEE:  lda #sa_Dead                    ;
LCEF0:  sta ObjAction                   ;Death handler.
LCEF3:  jsr SFX_SamusDie                ;Start Samus die SFX.
LCEF6:  jmp SetSamusExplode             ;Set Samus exlpode routine.

;----------------------------------------[ Add health ]----------------------------------------------

AddHealth:
LCEF9:  lda HealthLo                    ;Prepare to add to HealthLo.
LCEFC:  sta $03                         ;
LCEFE:  lda HealthLoChange              ;Amount to add to HealthLo.
LCF00:  clc                             ;
LCF01:  jsr Base10Add                   ;Perform base 10 addition.
LCF04:  sta HealthLo                    ;Save results.

LCF07:  lda HealthHi                    ;Prepare to add to HealthHi.
LCF0A:  sta $03                         ;
LCF0C:  lda HealthHiChange              ;Amount to add to HealthHi.
LCF0E:  jsr Base10Add                   ;Perform base 10 addition.
LCF11:  sta HealthHi                    ;Save results.

LCF14:  lda TankCount                   ;
LCF17:  jsr Amul16                      ;*16. Move tank count to upper 4 bits.
LCF1A:  ora #$0F                        ;Set lower 4 bits.
LCF1C:  cmp HealthHi                    ;
LCF1F:  bcs +                           ;Is life less than max? if so, branch.
LCF21:  and #$F9                        ;Life is more than max amount. 
LCF23:  sta HealthHi                    ;
LCF26:  lda #$99                        ;Set life to max amount.
LCF28:  sta HealthLo                    ;
LCF2B:* jmp ClearHealthChange           ;

;----------------------------------------------------------------------------------------------------

LCF2E:  lda SamusHit
LCF31:  lsr
        and #$02
        beq +++
        bcs +
        lda SamusHorzAccel
        bmi +++
        bpl ++
*       lda SamusHorzAccel
        bmi +
        bne ++
*       jsr TwosCompliment              ;
        sta SamusHorzAccel

ClearHorzMvmntData:
LCF4C:  ldy #$00                        ;
LCF4E:  sty ObjHorzSpeed                ;Set Samus Horizontal speed and horizontal-->
        sty HorzCntrLinear              ;linear counter to #$00.
*       rts                             ;

StopHorzMovement:
LCF55:  lda SamusHorzAccel              ;Is Samus moving horizontally?-->
        bne ClearHorzMvmtAnimData       ;If so, branch to stop movement.
        jsr SFX_SamusWalk               ;Play walk SFX.

ClearHorzMvmtAnimData:
LCF5D:  jsr NoHorzMoveNoDelay           ;Clear horizontal movement and animation delay data.
        sty ObjAction                   ;Samus is standing.
        lda Joy1Status                  ;
        and #$08                        ;Is The up button being pressed?-->
        bne +                           ;If so, branch.
        lda #an_SamusStand              ;Set Samus animation for standing.

SetSamusAnim:
LCF6B:  sta AnimResetIndex              ;Set new animation reset index.

SetSamusNextAnim:
        sta AnimIndex                   ;Set new animation data index.
        lda #$00                        ;
        sta AnimDelay                   ;New animation to take effect immediately.
        rts                             ;

SetSamusPntUp:
LCF77:* lda #sa_PntUp                   ;
        sta ObjAction                   ;Samus is pointing up.
        lda #an_SamusPntUp              ;
        jsr SetSamusAnim                ;Set new animation values.

NoHorzMoveNoDelay:
LCF81:  jsr ClearHorzData               ;Clear all horizontal movement data.
        sty AnimDelay                   ;Clear animation delay data.
        rts                             ;

LCF88:  lda Joy1Status
        and #$03
        beq +
        jsr BitScan                     ;
        tax
        jsr LCCB7
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
LCFB7:  jsr ClearHorzMvmntData          ;Clear horizontal speed and linear counter.
        sty SamusHorzAccel              ;Clear horizontal acceleration data.
*       rts                             ;

LCFBE:  ldy #an_SamusJumpPntUp
        jmp +

SetSamusJump:
LCFC3:  ldy #an_SamusJump
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
        bne +      ; branch if Samus is standing still
        lda SamusGear
        and #gr_SCREWATTACK
        beq +      ; branch if Samus doesn't have Screw Attack
        lda #$00
        sta $0686
        jsr SFX_ScrewAttack
*       jsr SFX_SamusJump
LCFF3:  ldy #$18        ; gravity (high value -> low jump)
        lda SamusGear
        and #gr_HIGHJUMP
        beq +      ; branch if Samus doesn't have High Jump
        ldy #$12        ; lower gravity value -> high jump!
*       sty SamusGravity
        rts

SamusJump:
        lda SamusJumpDsplcmnt
        bit ObjVertSpeed
        bpl +      ; branch if falling down
        cmp #$20
        bcc +      ; branch if jumped less than 32 pixels upwards
        bit Joy1Status
        bmi +      ; branch if JUMP button still pressed
        jsr StopVertMovement            ;Stop jump (start falling).
*       jsr LD055
        jsr LCF2E
        lda Joy1Status
        and #$08     ; UP pressed?
        beq +      ; branch if not
        lda #an_SamusJumpPntUp
        sta AnimResetIndex
        lda #sa_PntJump      ; "jumping & pointing up" handler
        sta ObjAction
*       jsr LD09C
        lda SamusInLava
        beq +
        lda Joy1Change
        bpl +      ; branch if JUMP not pressed
        jsr LCFC3
        jmp LCD6B

*       lda SamusGravity
        bne ++
        lda ObjAction
        cmp #sa_PntJump
        bne +
        jsr LCF77
        bne ++
*       jsr StopHorzMovement
*       lda #$03
        jmp SetSamusData                ;Set Samus control data and animation.

LD055:  ldx #$01
        ldy #$00
        lda Joy1Status
        lsr
        bcs +      ; branch if RIGHT pressed
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
*       jsr SetSamusAnim
        lda #$08
        sta AnimDelay
        sty SamusDir
*       stx ObjHorzSpeed
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

LD09C:  lda Joy1Change
        ora Joy1Retrig
        asl
        bpl -      ; exit if FIRE not pressed
        lda AnimResetIndex
        cmp #an_SamusJumpPntUp
        bne +
        jmp LD275

*       jsr LD210
        lda #an_SamusFireJump
        jmp SetSamusAnim

SetSamusRoll:
LD0B5:  lda SamusGear
        and #gr_MARUMARI
        beq +      ; branch if Samus doesn't have Maru Mari
        lda SamusGravity
        bne +

;Turn Samus into ball
        ldx SamusDir
        lda #an_SamusRoll
        sta AnimResetIndex
        lda #an_SamusRunJump
        sta AnimIndex
        lda LCCC0,x
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
        bne +      ; branch if yes
        bit Joy1Change  ; JUMP pressed?
        bpl ++    ; branch if no
*       lda Joy1Status
        and #$04           ; DOWN pressed?
        bne +     ; branch if yes
;break out of "ball mode"
        lda ObjRadY
        clc
        adc #$08
        sta ObjRadY
        jsr CheckMoveUp
        bcc +     ; branch if not possible to stand up
        ldx #$00
        jsr LE8BE
        stx $05
        lda #$F5
        sta $04
        jsr LFD8F
        jsr LD638
        jsr StopHorzMovement
        dec AnimIndex
        jsr StopVertMovement            ;
        lda #$04
        jmp LD144

*       lda Joy1Change
        jsr BitScan                     ;
        cmp #$02
        bcs +
        sta SamusDir
        lda #an_SamusRoll
        jsr SetSamusAnim
*       ldx SamusDir
        jsr LCCB7
        jsr LCF2E
        jsr CheckBombLaunch
        lda Joy1Status
        and #$03
        bne +
        jsr LCFB7
*       lda #$02
LD144:  jmp SetSamusData                ;Set Samus control data and animation.

StopVertMovement:
LD147:  ldy #$00
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
        bcc ++    ; exit if Samus doesn't have Bombs
        lda Joy1Change
        ora Joy1Retrig
        asl             ; bit 7 = status of FIRE button
        bpl ++    ; exit if FIRE not pressed
        lda ObjVertSpeed
        ora SamusOnElevator
        bne ++
        ldx #$D0        ; try object slot D
        lda ObjAction,x
        beq +      ; launch bomb if slot available
        ldx #$E0        ; try object slot E
        lda ObjAction,x
        beq +      ; launch bomb if slot available
        ldx #$F0        ; try object slot F
        lda ObjAction,x
        bne ++    ; no bomb slots available, exit
; launch bomb... give it same coords as Samus
*       lda ObjectHi
        sta ObjectHi,x
        lda ObjectX
        sta ObjectX,x
        lda ObjectY
        clc
        adc #$04        ; 4 pixels further down than Samus' center
        sta ObjectY,x
        lda #wa_LayBomb
        sta ObjAction,x
        jsr SFX_BombLaunch
*       rts

        SamusPntUp:
        lda Joy1Status
        and #$08     ; UP still pressed?
        bne +      ; branch if yes
        lda #sa_Stand   ; stand handler
        sta ObjAction
*       lda Joy1Status
        and #$07        ; DOWN, LEFT, RIGHT pressed?
        beq ++    ; branch if no
        jsr BitScan                     ;
        cmp #$02
        bcs +
        sta SamusDir
*       tax
        lda Table07,x
        sta ObjAction
*       lda Joy1Change
        ora Joy1Retrig
        asl
        bpl +      ; branch if FIRE not pressed
        jsr FireWeapon                  ;Shoot up.
*       bit Joy1Change
        bpl +      ; branch if JUMP not pressed
        lda #sa_PntJump
        sta ObjAction
*       lda #$04
        jsr SetSamusData                ;Set Samus control data and animation.
        lda ObjAction
        jsr ChooseRoutine

; Pointer table to code

        .word StopHorzMovement
        .word LCC98
        .word ExitSub       ;rts
        .word SetSamusRoll
        .word ExitSub       ;rts
        .word ExitSub       ;rts
        .word LCFBE
        .word ExitSub       ;rts
        .word ExitSub       ;rts
        .word ExitSub       ;rts

; Table used by above subroutine

Table07:
        .byte sa_Run
        .byte sa_Run
        .byte sa_Roll

FireWeapon:
LD1EE:  lda Joy1Status
        and #$08
        beq LD210
        jmp LD275

LD1F7:  ldy #$D0
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

LD210:  lda MetroidOnSamus
        bne +
        jsr LD1F7
        bne +
        jsr LD2EB
        jsr LD359
        jsr LD38E
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
        jsr LD306
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
LD26B:  tya
        jmp SetSamusNextAnim

Table08:
        .byte $0C
        .byte $F4
        .byte $08
        .byte $F8

Table99:
        .byte $04
        .byte $FC

LD275:  lda MetroidOnSamus
        bne +
        jsr LD1F7
        bne +
        jsr LD2EB
        jsr LD38A
        jsr LD38E
        lda #$0C
        sta $030F,y
        lda #$FC
        sta ObjVertSpeed,y
        lda #$00
        sta ObjHorzSpeed,y
        lda #$01
        sta ObjectOnScreen,y
        jsr LD340
        ldx SamusDir
        lda Table09+4,x
        sta $05
        lda ObjAction,y
        and #$01
        tax
        lda Table09+6,x
        sta $04
        jsr LD306
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
        jmp LD26B

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

LD2EB:  tya
        tax
        inc ObjAction,x
        lda #$02
        sta ObjRadY,y
        sta ObjRadX,y
        lda #an_Bullet

SetProjectileAnim:
LD2FA:  sta AnimResetIndex,x
SetProjectileAnim2:
        sta AnimIndex,x
        lda #$00
        sta AnimDelay,x
*       rts

LD306:  ldx #$00
        jsr LE8BE
        tya
        tax
        jsr LFD8F
        txa
        tay
        jmp LD638

CheckMissileLaunch:
        lda MissileToggle
        beq Exit4       ; exit if Samus not in "missile fire" mode
        cpy #$D0
        bne Exit4
        ldx SamusDir
        lda MissileAnims,x
*       jsr SetBulletAnim
        jsr SFX_MissileLaunch
        lda #wa_Missile ; missile handler
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

LD340:  lda MissileToggle
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

LD359:  lda SamusDir
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

LD38A:  lda #$02
        bne --
LD38E:  lda MissileToggle
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
*       jsr LD48C
        jsr LED65
        jsr $95AB
        lda ItemRoomMusicStatus
        beq ++
        pha
        jsr LD92C       ; start music
        pla
        bpl ++
        lda #$00
        sta ItemRoomMusicStatus
        beq ++
*       lda #$80
        sta ItemRoomMusicStatus
*       lda KraidRidleyPresent
        beq +
        jsr LCC07
        lda #$00
        sta KraidRidleyPresent
        beq --     ; branch always
*       lda SamusDoorData
        and #$0F
        sta ObjAction
        lda #$00
        sta SamusDoorData
        sta DoorStatus
        jsr StopVertMovement            ;

MoveOutDoor:
        lda SamusDoorDir
        beq ++    ; branch if door leads to the right
        ldy ObjectX
        bne +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       dec ObjectX
        jmp ++

*       inc ObjectX
        bne +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       jsr CheckHealthStatus           ;Check if Samus hit, blinking or Health low.
        jsr SetmirrorCntrlBit
        jmp DrawFrame       ; display Samus

SamusDead:
D41A:   lda #$01
        jmp SetSamusData                ;Set Samus control data and animation.

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
        bcc +      ; if ScreenY < $84, don't scroll
        jsr ScrollDown  ; otherwise, attempt to scroll
*       ldy ObjectY
        cpy #239        ; wrap-around required?
        bne +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
        ldy #$FF        ; ObjectY will now be 0
*       iny
        sty ObjectY
        jmp LD47E

*       lda ObjectY
        sec
        sbc ScrollY     ; A = Samus' Y position on the visual screen
        cmp #$64
        bcs +      ; if ScreenY >= $64, don't scroll
        jsr ScrollUp    ; otherwise, attempt to scroll
*       ldy ObjectY
        bne +      ; wraparound required? (branch if not)
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
        ldy #240        ; ObjectY will now be 239
*       dey
        sty ObjectY
        jmp LD47E

*       ldy #$00
        sty ObjVertSpeed
        cmp #$05
        beq +
        cmp #$07
        beq +
LD47E:  lda FrameCount
        lsr
        bcc ++
*       jsr SetmirrorCntrlBit           ;Mirror Samus, if necessary.
        lda #$01
        jmp AnimDrawObject
*       rts

LD48C:  ldx #$60
        sec
*       jsr LD4B4
        txa
        sbc #$20
        tax
        bpl -
        jsr GetNameTable                ;
        tay
        ldx #$18
*       jsr LD4A8
        txa
        sec
        sbc #$08
        tax
        bne -
LD4A8:  tya
        cmp $072C,x
        bne +
        lda #$FF
        sta $0728,x
*       rts

LD4B4:  lda EnData05,x
LD4B7:  and #$02
LD4B9:  bne +
LD4BB:  sta EnStatus,x
LD4BE:* rts

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
LD4D0:  jsr ChooseRoutine

LD4D3:  .word ExitSub               ; rts
LD4D5:  .word UpdateBullet          ; regular beam
        .word UpdateWaveBullet      ; wave beam
        .word UpdateIceBullet       ; ice beam
        .word BulletExplode         ; bullet/missile explode
        .word LD65E                 ; lay bomb
        .word LD670                 ; lay bomb
        .word LD691                 ; lay bomb
        .word LD65E                 ; lay bomb
        .word LD670                 ; bomb countdown
        .word LD691                 ; bomb explode
        .word UpdateBullet          ; missile

UpdateBullet:
        lda #$01
        sta UpdatingProjectile
        jsr LD5FC
        jsr LD5DA
        jsr LD609
CheckBulletStat:
        ldx PageIndex
        bcc +
        lda SamusGear
        and #gr_LONGBEAM
        bne DrawBullet  ; branch if Samus has Long Beam
        dec $030F,x     ; decrement bullet timer
        bne DrawBullet
        lda #$00        ; timer hit 0, kill bullet
        sta ObjAction,x
        beq DrawBullet  ; branch always
*       lda ObjAction,x
        beq +
        jsr LD5E4
DrawBullet:
        lda #$01
        jsr AnimDrawObject
*       dec UpdatingProjectile
        rts

*       inc $0500,x
LD522:  inc $0500,x
        lda #$00
        sta $0501,x
        beq +      ; branch always

UpdateWaveBullet:
        lda #$01
        sta UpdatingProjectile
        jsr LD5FC
        jsr LD5DA
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
        jmp LD522
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
        jsr TwosCompliment              ;
        sta ObjHorzSpeed,x
*       jsr LD609
        bcs +
        jsr LD624
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
        jmp UpdateBullet

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
        sta ObjAction,x  ; kill bullet
*       jmp DrawBullet

LD5DA:  lda $030A,x
        beq Exit5
        lda #$00
        sta $030A,x
LD5E4:  lda #$1D
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

LD5FC:  lda ObjectOnScreen,x
        lsr
        bcs Exit5
*       lda #$00
        beq --   ; branch always
*       jmp LE81E

; bullet <--> background crash detection

LD609:  jsr GetObjCoords
        ldy #$00
        lda ($04),y     ; get tile # that bullet touches
        cmp #$A0
        bcs LD624
        jsr $95C0
        cmp #$4E
        beq -
        jsr LD651
        bcc ++
        clc
        jmp IsBlastTile

LD624:  ldx PageIndex
        lda ObjHorzSpeed,x
        sta $05
        lda ObjVertSpeed,x
        sta $04
        jsr LE8BE
        jsr LFD8F
        bcc --
LD638:  lda $08
        sta ObjectY,x
        lda $09
        sta ObjectX,x
        lda $0B
        and #$01
        bpl +      ; branch always
        ToggleObjectHi:
        lda ObjectHi,x
        eor #$01
*       sta ObjectHi,x
*       rts

LD651:  ldy InArea
        cpy #$10
        beq +
        cmp #$70
        bcs ++
*       cmp #$80
*       rts

LD65E:  lda #an_BombTick
        jsr SetProjectileAnim
        lda #$18        ; fuse length :-)
        sta $030F,x
        inc ObjAction,x       ; bomb update handler
        DrawBomb:
        lda #$03
        jmp AnimDrawObject

LD670:  lda FrameCount
        lsr
        bcc ++    ; only update counter on odd frames
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
*       jmp DrawBomb

LD691:  inc $030F,x
        jsr LD6A7
        ldx PageIndex
        lda $0303,x
        sec
        sbc #$F7
        bne +
        sta ObjAction,x     ; kill bomb
*       jmp DrawBomb

LD6A7:  jsr GetObjCoords
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
        jsr LD78B
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
        jsr LD78B
*       jsr LD76A
Exit6:  rts

*       dey
        bne +++
        lda #$40
        jsr LD77F
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
        jsr LD77F
*       jmp LD76A

*       dey
        bne +++
        lda #$02
        jsr LD78B
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
        jsr LD77F
        lda $05
        eor #$04
        sta $05
*       jmp LD76A

*       dey
        bne Exit7
        lda #$02
        jsr LD77F
        txa
        bne +
        lda $04
        lsr
        bcs Exit7
*       lda $04
        and #$1F
        cmp #$02
        bcs LD76A
        lda ScrollDir
        and #$02
        beq Exit7
        lda #$1E
        jsr LD78B
        lda $05
        eor #$04
        sta $05
LD76A:  txa
        pha
        ldy #$00
        lda ($04),y
        jsr LD651
        bcc +
        cmp #$A0
        bcs +
        jsr LE9C2
*       pla
        tax
Exit7:  rts

LD77F:  clc
        adc $0A
        sta $04
        lda $0B
        adc #$00
        jmp LD798

LD78B:  sta $00
        lda $0A
        sec
        sbc $00
        sta $04
        lda $0B
        sbc #$00
LD798:  and #$07
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

        .word ExitSub       ; rts
        .word ElevatorIdle
        .word LD80E
        .word ElevatorMove
        .word ElevatorScroll
        .word LD8A3
        .word LD8BF
        .word LD8A3
        .word ElevatorMove
        .word ElevatorStop

        ElevatorIdle:
        lda SamusOnElevator
        beq ShowElevator
        lda #$04
        bit $032F       ; elevator direction in bit 7 (1 = up)
        bpl +
        asl             ; btn_UP
*       and Joy1Status
        beq ShowElevator
    ; start elevator!
        jsr StopVertMovement            ;
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
        bcc --    ; only display elevator at odd frames
        jmp DrawFrame       ; display elevator

LD80E:  lda ScrollX
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
        bpl ++    ; branch if elevator going down
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
*       inc ObjectY,x
        lda ObjectY,x
        cmp #240
        bne +
        jsr ToggleObjectHi
        lda #$00
        sta ObjectY,x
*       cmp #$83
        bne +      ; move until Y coord = $83
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
        bpl +      ; branch if elevator going down
        jsr ScrollUp
        jmp ShowElevator

*       jsr ScrollDown
        jmp ShowElevator

LD8A3:  inc ObjAction,x
        lda ObjAction,x
        cmp #$08        ; ElevatorMove
        bne +
        lda #$23
        sta $0303,x
        lda #an_SamusFront
        jsr SetSamusAnim
        jmp ShowElevator

*       lda #$01
        jmp AnimDrawObject

LD8BF:  lda $030F,x
        tay
        cmp #$8F        ; Leads-To-Ending elevator?
        bne +
    ; Samus made it! YAY!
        lda #$07
        sta MainRoutine
        inc AtEnding
        ldy #$00
        sty $33
        iny
        sty SwitchPending   ; switch to bank 0
        lda #$1D        ; ending
        sta TitleRoutine
        rts

*       tya
        bpl ++
        ldy #$00
        cmp #$84
        bne +
        iny
*       tya
*       ora #$10
        jsr LCA18
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
        jsr StartMusic                  ;($LD92C)Start music.
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
LD92C:  lda ElevatorStatus
        cmp #$06
        bne +
        lda $032F
        bmi ++
*       lda $95CD                       ;Load proper bit flag for area music.
        ldy ItemRoomMusicStatus
        bmi ++
        beq ++
*       lda #$81
        sta ItemRoomMusicStatus
        lda #$20                        ;Set flag to play item room music.

*       ora MusicInitFlag               ;
        sta MusicInitFlag               ;Store music flag info.
        rts                             ;

ElevatorStop:
        lda ScrollY
        bne ++    ; scroll until ScrollY = 0
        lda #sa_Stand
        sta ObjAction
        jsr StopHorzMovement
        ldx PageIndex   ; #$20
        lda #$01        ; ElevatorIdle
        sta ObjAction,x
        lda $030F,x
        eor #$80        ; switch elevator direction
        sta $030F,x
        bmi +
        jsr ToggleScroll
        sta MirrorCntrl
*       jmp ShowElevator
*       jmp ElevScrollRoom

SamusOnElevatorOrEnemy:
LD976:  lda #$00                        ;
        sta SamusOnElevator             ;Assume Samus is not on an elevator or on a frozen enemy.
        sta OnFrozenEnemy               ;
        tay
        ldx #$50
        jsr LF186
*       lda EnStatus,x
        cmp #$04
        bne +
        jsr LF152
        jsr LF1BF
        jsr LF1FA
        bcs +
        jsr LD9BA
        bne +
D99A:   inc OnFrozenEnemy               ;Samus is standing on a frozen enemy.
        bne ++
*       jsr Xminus16
        bpl --
*       lda ElevatorStatus
        beq +
        ldy #$00
        ldx #$20
        jsr LDC82
        bcs +
        jsr LD9BA
        bne +
        inc SamusOnElevator             ;Samus is standing on elevator.
*       rts

LD9BA:  lda $10
        and #$02
        bne +
        ldy $11
        iny
        cpy $04
        beq Exit8
*       lda SamusHit
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
        beq Exit8          ; exit if no statue present
        dey
        bne +
        jsr LDAB0
        ldy #$01
        jsr LDAB0
        bcs +
        inc $0360
*       ldy $0360
        cpy #$02
        bne +++
        lda KraidStatueStatus
        bpl +
        ldy #$02
        jsr LDAB0
*       lda $687C
        bpl +
        ldy #$03
        jsr LDAB0
*       bcs +
        inc $0360
*       ldx #$60
        jsr LDA1A
        ldx #$61
        jsr LDA1A
        jmp LDADA

LDA1A:  jsr LDA3D
        jsr LDA7C
        txa
        and #$01
        tay
        lda LDA3B,y
        sta $0363
        lda $681B,x
        beq +
        bmi +
        lda FrameCount
        lsr
        bcc ++    ; only display statue at odd frames
*       jmp DrawFrame       ; display statue

LDA39:  .byte $88
        .byte $68
LDA3B:  .byte $65
        .byte $66

LDA3D:  lda $0304,x
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
        jsr LDAB0
        pla
        tay
        iny
        iny
        jsr LDAB0
        pla
        tax
*       rts

LDA7C:  lda $030F,x
        sta $036D
        txa
        and #$01
        tay
        lda LDA39,y
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
*       lda #$00
        sta $0306,x
        rts

LDAB0:  lda Table0E,y
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

LDADA:  lda $54
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
        eor #$01        ; 0 = fire bullets, 1 = fire missiles
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
        ldx #$00                        ;Check first item slot.
        jsr CheckOneItem                ;Check current item slot.
        ldx #$08                        ;Check second item slot.

CheckOneItem:
LDB42:  stx ItemIndex                   ;First or second item slot index(#$00 or #$08).
        ldy PowerUpType,x               ;
        iny                             ;Is no item present in item slot(#$FF)?-->
        beq -                           ;If so, branch to exit.

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
*       pha
        ldx #$00
        ldy #$40
        jsr LDC7F
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
        jsr LDC1C
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

*       beq +
        lda #5
        jsr AddToMaxMissiles
        bne ---    ; branch always
*       lda TankCount
        cmp #$06        ; has Samus got 6 energy tanks?
        beq +      ; then she can't have any more
        inc TankCount   ; otherwise give her a new tank
*       lda TankCount
        jsr Amul16      ; shift into upper nibble
        ora #$09
        sta HealthHi
        lda #$99
        sta HealthLo    ; health is now FULL!
        bne -----          ; branch always

LDC1C:  lda MapPosX
MapScrollRoutine:
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
*       bcc +
        lda ScrollY
        beq +
        dec $06
*       lda PPUCNT0ZP
        eor $08
        and #$01
        plp
        clc
        beq +
        adc $07
        sta $07
        jmp LDC51

*       adc $06
        sta $06
LDC51:  jsr CreateItemID                ;
LDC54:  ldy NumberOfUniqueItems
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
LDC67:  lda $07                         ;Load x map position of item.
LDC69:  jsr Amul32                      ;($C2C$)*32. Move lower 3 bytes to upper 3 bytes.
LDC6C:  ora $06                         ;combine Y coordinates into data byte.
LDC6E:  sta $06                         ;Lower data byte complete. Save in $06.
LDC70:  lsr $07                         ;
LDC72:  lsr $07                         ;Move upper two bits of X coordinate to LSBs.
LDC74:  lsr $07                         ;
LDC76:  lda $09                         ;Load item type bits.
LDC78:  asl                             ;Move the 6 bits of item type to upper 6 bits of byte.
LDC79:  asl                             ;
LDC7A:  ora $07                         ;Add upper two bits of X coordinate to byte.
LDC7C:  sta $07                         ;Upper data byte complete. Save in #$06.
LDC7E:  rts                             ;

;-----------------------------------------------------------------------------------------------------

LDC7F:  jsr LF186
LDC82:  jsr LF172
LDC85:  jsr LF1A7
LDC88:  jmp LF1FA

;The following table is used to rotate the sprites of both Samus and enemies when they explode.

ExplodeRotationTbl:
LDC8B:  .byte $00                       ;No sprite flipping.
LDC8C:  .byte $80                       ;Flip sprite vertically.
LDC8D:  .byte $C0                       ;Flip sprite vertically and horizontally.
LDC8E:  .byte $40                       ;Flip sprite horizontally.

; UpdateObjAnim
; =============
; Advance to object's next frame of animation

UpdateObjAnim:
LDC8F:  ldx PageIndex
        ldy AnimDelay,x
        beq +      ; is it time to advance to the next anim frame?
        dec AnimDelay,x     ; nope
        bne +++   ; exit if still not zero (don't update animation)
*       sta AnimDelay,x     ; set initial anim countdown value
        ldy AnimIndex,x
*       lda ObjectAnimIndexTbl,y                ;Load frame number.
        cmp #$FF        ; has end of anim been reached?
        beq ++
        sta AnimFrame,x     ; store frame number
        iny      ; inc anim index
        tya
        sta AnimIndex,x     ; store anim index
*       rts

*       ldy AnimResetIndex,x     ; reset anim frame index
        jmp ---    ; do first frame of animation

LDCB7:  pha
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
LDCC3:  ldy #$00                        ;
LDCC5:  sty $0F                         ;Clear index into placement data.
LDCC7:  lda ($00),y                     ;Load control byte from frame pointer data.
LDCC9:  sta $04                         ;Store value in $04 for processing below.
LDCCB:  tax                             ;Keep a copy of the value in x as well.
LDCCC:  jsr Adiv16                      ;Move upper 4 bits to lower 4 bits.
LDCCF:  and #$03                        ;
LDCD1:  sta $05                         ;The following lines take the upper 4 bits in the-->
LDCD3:  txa                             ;control byte and transfer bits 4 and 5 into $05 bits 0-->
LDCD4:  and #$C0                        ;and 1(sprite color bits).  Bits 6 and 7 are-->
LDCD6:  ora #$20                        ;transferred into $05 bits 6 and 7(sprite flip bits).-->
LDCD8:  ora $05                         ;bit 5 is then set(sprite always drawn behind background).
LDCDA:  sta $05                         ;
LDCDC:  lda ObjectCntrl                 ;Extract bit from control byte that controls the
LDCDE:  and #$10                        ;object mirroring.
LDCE0:  asl                             ;
LDCE1:  asl                             ;
LDCE2:  eor $04                         ;Move it to the bit 6 position and use it to flip the-->
LDCE4:  sta $04                         ;horizontal mirroring of the sprite if set.
LDCE6:  lda ObjectCntrl                 ;
LDCE8:  bpl +                           ;If MSB is set in ObjectCntrl, use its flip bits(6 and 7).
LDCEA:  asl ObjectCntrl                 ;
LDCEC:  jsr SpriteFlipBitsOveride       ;Use object flip bits as priority over sprite flip bits. 
LDCEF:* txa                             ;Discard upper nibble so only entry number into-->
LDCF0:  and #$0F                        ;PlacePtrTbl remains.
LDCF2:  asl                             ;*2. pointers in PlacePntrTbl are 2 bytes in size.
LDCF3:  tax                             ;Transfer to X to use as an index to find proper-->
LDCF4:  rts                             ;placement data segment.

;-----------------------------------------------------------------------------------------------------

LDCF5:  jsr ClearObjectCntrl            ;Clear object control byte.
        pla
        pla
        ldx PageIndex
LDCFC:  lda InArea
        cmp #$13
        bne +
        lda EnDataIndex,x
        cmp #$04
        beq +++++
        cmp #$02
        beq +++++
*       lda $040C,x
        asl
        bmi LDD75
        jsr GetEnemy8BValue
        sta $00
        jsr Unknown80B0
        and #$20
        sta EnDataIndex,x
        lda #$05
        sta EnStatus,x
        lda #$60
        sta $040D,x
        lda RandomNumber1
        cmp #$10
        bcc LDD5B
*       and #$07
        tay
        lda ItemDropTbl,y
        sta EnAnimFrame,x
        cmp #$80
        bne ++
        ldy MaxMissilePickup
        cpy CurrentMissilePickups
        beq LDD5B
        lda MaxMissiles
        beq LDD5B
        inc CurrentMissilePickups
*       rts

*       ldy MaxEnergyPickup
        cpy CurrentEnergyPickups
        beq LDD5B
        inc CurrentEnergyPickups
        cmp #$89
        bne --
        lsr $00
        bcs --

LDD5B:  ldx PageIndex
        lda InArea
        cmp #$13
        beq ++
*       jmp KillObject                  ;Free enemy data slot.

*       lda RandomNumber1
        ldy #$00
        sty CurrentEnergyPickups
        sty CurrentMissilePickups
        iny
        sty MaxMissilePickup
        sty MaxEnergyPickup
        bne -----

LDD75:  jsr PowerUpMusic
        lda InArea
        and #$0F
        sta MiniBossKillDelay
        lsr
        tay
        sta MaxMissiles,y
        lda #75
        jsr AddToMaxMissiles
        bne LDD5B

ClrObjCntrlIfFrameIsF7:
        ldx PageIndex
        lda EnAnimFrame,x
        cmp #$F7
        bne +++
        jmp ClearObjectCntrl            ;Clear object control byte.

; AddToMaxMissiles
; ================
; Adds A to both MissileCount & MaxMissiles, storing the new count
; (255 if it overflows)

AddToMaxMissiles:
        pha                             ;Temp storage of # of missiles to add.
        clc
        adc MissileCount
        bcc +
        lda #$FF
*       sta MissileCount
        pla
        clc
        adc MaxMissiles
        bcc +
        lda #$FF
*       sta MaxMissiles
        rts

*       lda EnYRoomPos,x
        sta $0A  ; Y coord
        lda EnXRoomPos,x
        sta $0B  ; X coord
        lda EnNameTable,x
        sta $06  ; hi coord
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
        jsr GetSpriteCntrlData          ;Get place pointer index and sprite control data.
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
        jmp LDCF5

*       ldx PageIndex
        iny
        lda ($00),y
        sta EnRadY,x
        jsr ReduceYRadius               ;Reduce temp y radius by #$10.
        iny
        lda ($00),y
        sta EnRadX,x
        sta $09
        iny
        sty $11
        jsr IsObjectVisible             ;Determine if object is within screen boundaries.
        txa
        asl
        sta $08
        ldx PageIndex
        lda EnData05,x
        and #$FD
        ora $08
        sta EnData05,x
        lda $08
        beq ++
        jmp LDEDE

;----------------------------------------[ Item drop table ]-----------------------------------------

;The following table determines what, if any, items an enemy will drop when it is killed.

ItemDropTbl:
LDE35:  .byte $80                       ;Missile.
LDE36:  .byte $81                       ;Energy.
LDE37:  .byte $89                       ;No item.
LDE38:  .byte $80                       ;Missile.
LDE39:  .byte $81                       ;Energy.
LDE3A:  .byte $89                       ;No item.
LDE3B:  .byte $81                       ;Energy.
LDE3C:  .byte $89                       ;No item.

;---------------------------------[ Check if object is on screen ]----------------------------------

;The following set of functions determine if an object is visible on the screen.  If the object
;is visible, X-1 when the function returns, X=0 if the object is not within the boundaries of the
;current screen.  The function needs to know what nametable is currently in the PPU, what nametable
;the object is on and what the scroll offsets are. 

IsObjectVisible:
LDFDF:  ldx #$01                        ;Assume object is visible on screen.
LDFE1:  lda $0A                         ;Object Y position in room.
LDFE3:  tay                             ;
LDFE4:  sec                             ;Subtract y scroll to find sprite's y position on screen.
LDFE5:  sbc ScrollY                     ;
LDFE7:  sta $10                         ;Store result in $10.
LDFE9:  lda $0B                         ;Object X position in room.
LDFEB:  sec                             ;
LDFEC:  sbc ScrollX                     ;Subtract x scroll to find sprite's x position on screen.
LDFEE:  sta $0E                         ;Store result in $0E.
LDFF0:  lda ScrollDir                   ;
LDFF2:  and #$02                        ;Is Samus scrolling left or right?-->
LDFF4:  bne HorzScrollCheck             ;If so, branch.

VertScrollCheck:
LDFF6:  cpy ScrollY                     ;If object room pos is >= scrollY, set carry.
LDFF8:  lda $06                         ;Check if object is on different name table as current-->
LDFFA:  eor PPUCNT0ZP                   ;name table active in PPU.-->
LDFFC:  and #$01                        ;If not, branch.
LDFFE:  beq +                           ;
LE000:  bcs ++                          ;If carry is still set, sprite is not in screen boundaries.
LE002:  lda $10                         ;
LE004:  sbc #$0F                        ;Move sprite y position up 15 pixles.
LE006:  sta $10                         ;
LE008:  lda $09                         ;
LE00A:  clc                             ;If a portion of the object is outside the sceen-->
LE00B:  adc $10                         ;boundaries, treat object as if the whole thing is-->
LE00D:  cmp #$F0                        ;not visible.
LE00F:  bcc +++                         ;
LE011:  clc                             ;Causes next statement to branch always.
LE012:* bcc +                           ;
LE014:  lda $09                         ;If object is on same name table as the current one in-->
LE016:  cmp $10                         ;the PPU, check if part of object is out of screen--> 
LE018:  bcc ++                          ;boundaries.  If so, branch.
LE01A:* dex                             ;Sprite is not within screen boundaries. Decrement X.
LE01B:* rts                             ;

HorzScrollCheck:
LE01C:  lda $06                         ;
LE01E:  eor PPUCNT0ZP                   ;Check if object is on different name table as current-->
LE020:  and #$01                        ;name table active in PPU.-->
LE022:  beq +                           ;If not, branch.
LE024:  bcs ++                          ;If carry is still set, sprite is not in screen boundaries.
LE026:  lda $09                         ;
LE028:  clc                             ;If a portion of the object is outside the sceen-->
LE029:  adc $0E                         ;boundaries, treat object as if the whole thing is-->
LE02B:  bcc +++                         ;not visible.
LE02D:  clc                             ;Causes next statement to branch always.
LE02E:* bcc +                           ;
LE030:  lda $09                         ;If object is on same name table as the current one in-->
LE032:  cmp $0E                         ;the PPU, check if part of object is out of screen--> 
LE034:  bcc ++                          ;boundaries.  If so, branch.
LE036:* dex                             ;Sprite is not within screen boundaries. Decrement X.
LE037:* rts                             ;

;------------------------[ Override sprite flip bits with object flip bits ]-------------------------

;If the MSB is set in ObjectCntrl, its two upper bits that control sprite flipping take priority
;over the sprite control bits.  This function modifies the sprite control byte with any flipping
;bits found in ObjectCntrl.

SpriteFlipBitsOveride:
LE038:  lsr ObjectCntrl                 ;Restore MSB.
LE03A:  lda ($00),y                     ;Reload frame data control byte into A.
LE03C:  and #$C0                        ;Extract the two sprite flip bytes from theoriginal-->
LE03E:  ora ObjectCntrl                 ;control byte and set any additional bits from ObjectCntrl.
LE040:  sta $05                         ;Store modified byte to load in sprite control byte later.
LE042:  lda ObjectCntrl                 ;
LE044:  ora #$80                        ;
LE046:  sta ObjectCntrl                 ;Ensure MSB of object control byte remains set.
LE048:  rts                             ;

;--------------------------------[ Explosion placement data ]---------------------------------------

;The following table has the index values into the table after it for finding the placement data
;for an exploding object.

ExplodeIndexTbl:
LE049:  .byte $00, $18, $30

;The following table is used to produce the arcing motion of exploding objects.  It is displacement
;data for the y directions only.  The x displacement is constant.

ExplodePlacementTbl:

;Bottom sprites.
LE04C:  .byte $FC, $F8, $F4, $F0, $EE, $EC, $EA, $E8, $E7, $E6, $E6, $E5, $E5, $E4, $E4, $E3
LE05C:  .byte $E5, $E7, $E9, $EB, $EF, $F3, $F7, $FB

;Middle sprites.
LE064:  .byte $FE, $FC, $FA, $F8, $F6, $F4, $F2, $F0, $EE, $ED, $EB, $EA, $E9, $E8, $E7, $E6
LE074:  .byte $E6, $E6, $E6, $E6, $E8, $EA, $EC, $EE

;Top sprites.
LE07C:  .byte $FE, $FC, $FA, $F8, $F7, $F6, $F5, $F4, $F3, $F2, $F1, $F1, $F0, $F0, $EF, $EF
LE08C:  .byte $EF, $EF, $EF, $EF, $F0, $F0, $F1, $F2

;--------------------------------------[ Update enemy animation ]-----------------------------------

;Advance to next frame of enemy's animation. Basically the same as UpdateObjAnim, only for enemies.

UpdateEnemyAnim:
LE094:  ldx PageIndex                   ;Load index to desired enemy.
LE096:  ldy EnStatus,x                  ;
LE099:  cpy #$05                        ;Is enemy in the process of dying?-->
LE09B:  beq +++                         ;If so, branch to exit.
LE09D:  ldy EnAnimDelay,x               ;
LE0A0:  beq +                           ;Check if current anumation frame is ready to be updated.
LE0A2:  dec EnAnimDelay,x               ;Not ready to update. decrement delay timer and-->
LE0A5:  bne +++                         ;branch to exit.
LE0A7:* sta EnAnimDelay,x               ;Save new animation delay value.
LE0AA:  ldy EnAnimIndex,x               ;Load enemy animation index.
LE0AD:* lda (EnemyAnimPtr),y            ;Get animation data.
LE0AF:  cmp #$FF                        ;End of animation?
LE0B1:  beq ++                          ;If so, branch to reset animation.
LE0B3:  sta EnAnimFrame,x               ;Store current animation frame data.
LE0B6:  iny                             ;Increment to next animation data index.
LE0B7:  tya                             ;
LE0B8:  sta EnAnimIndex,x               ;Save new animation index.
LE0BB:* rts                             ;

LE0BC:* ldy EnResetAnimIndex,x          ;reset animation index.
LE0BF:  bcs ---                         ;Branch always.

;-------------------------------------------[ Bit scan ]---------------------------------------------

;This function takes the value stored in A and right shifts it until a set bit is encountered.
;Once a set bit is encountered, the function exits and returns the bit number of the set bit.
;The returned value is stored in A. 

BitScan:
LE1E1:  stx $0E                         ;Save X.
LE1E3:  ldx #$00                        ;First bit is bit 0.
LE1E5:* lsr                             ;Transfer bit to carry flag.
LE1E6:  bcs +                           ;If the shifted bit was 1, Branch out of loop.
LE1E8:  inx                             ;Increment X to keep of # of bits checked.
LE1E9:  cpx #$08                        ;Have all 8 bit been tested?-->
LE1EB:  bne -                           ;If not, branch to check the next bit.
LE1ED:* txa                             ;Return which bit number was set.
LE1EE:  ldx $0E                         ;Restore X.
LE1F0:* rts                             ;

;------------------------------------------[ Scroll door ]-------------------------------------------

;Scrolls the screen if Samus is inside a door.

ScrollDoor:
LE1F1:  ldx DoorStatus                  ;
LE1F3:  beq -                           ;Exit if Samus isn't in a door.
LE1F5:  dex                             ;
LE1F6:  bne +                           ;Not in right door. branch to check left door.
LE1F8:  jsr ScrollRight                 ;DoorStatus=1, scroll 1 pixel right.
LE1FB:  jmp ++                          ;Jump to check if door scroll is finished.

LE1FE:* dex                             ;Check if in left door.
LE1FF:  bne ++                          ;
LE201:  jsr ScrollLeft                  ;DoorStatus=2, scroll 1 pixel left.
LE204:* ldx ScrollX                     ;Has x scroll offset reached 0?-->
LE206:  bne Exit15                      ;If not, branch to exit.

;Scrolled one full screen, time to exit door.
LE208:  ldx #$05                        ;Samus is exiting the door.
LE20A:  bne DoOneDoorScroll             ;Branch always.

LE20C:* dex                             ;
LE20D:  bne +                           ;Check if need to scroll down to center door.
LE20F:  jsr ScrollDown                  ;DoorStatus=3, scroll 1 pixel down.
LE212:  jmp ++                          ;Jump to check y scrolling value.
LE215:* dex                             ;
LE216:  bne Exit15                      ;Check if need to scroll up to center door.
LE218:  jsr ScrollUp                    ;DoorStatus=4, scroll 1 pixel up.

VerticalRoomCentered:
LE21B:* ldx ScrollY                     ;Has room been centered on screen?-->
LE21D:  bne Exit15                      ;If not, branch to exit.
LE21F:  stx DoorOnNameTable3            ;
LE221:  stx DoorOnNameTable0            ;Erase door nametable data.
LE223:  inx                             ;X=1.
LE224:  lda ObjectX                     ;Did Samus enter in the right hand door?-->
LE227:  bmi ++                          ;If so, branch.
LE229:  inx                             ;X=2. Samus is in left door.
LE22A:  bne ++                          ;Branch always.

;This function is called once after door scrolling is complete.

DoOneDoorScroll:
LE22C:  lda #$20                        ;Set DoorDelay to 32 frames(comming out of door).
LE22E:  sta DoorDelay                   ;
LE230:  lda SamusDoorData               ;Check if scrolling should be toggled.
LE232:  jsr Amul8                       ;*8. Is door not to toggle scrolling(item room,-->
LE235:  bcs +                           ;bridge room, etc.)? If so, branch to NOT toggle scrolling.
LE237:  ldy DoorScrollStatus            ;If comming from vertical shaft, skip ToggleScroll because-->
LE239:  cpy #$03                        ;the scroll was already toggled after room was centered-->
LE23B:  bcc ++                          ;by the routine just above.
LE23D:* lda #$47                        ;Set mirroring for vertical mirroring(horz scrolling).
LE23F:  bne ++                          ;Branch always.

LE241:* jsr ToggleScroll                ;Toggle scrolling and mirroring.
LE244:* sta MirrorCntrl                 ;Store new mirror control data.
LE246:  stx DoorStatus                  ;DoorStatus=5. Done with door scrolling.

Exit15:
LE248:  rts                             ;Exit for several routines above.

;------------------------------------[ Toggle Samus nametable ]--------------------------------------

ToggleSamusHi:
LE249:  lda ObjectHi                    ;
LE24C:  eor #$01                        ;Change Samus' current nametable from one to the other.
LE24E:  sta ObjectHi                    ;
LE251:  rts                             ;

;-------------------------------------------[ Toggle scroll ]----------------------------------------

;Toggles both mirroring and scroll direction when Samus has moved from
;a horizontal shaft to a vertical shaft or vice versa.

ToggleScroll:
LE252:  lda ScrollDir                   ;
LE254:  eor #$03                        ;Toggle scroll direction.
LE256:  sta ScrollDir                   ;
LE258:  lda MirrorCntrl                 ;Toggle mirroring.
LE25A:  eor #$08                        ;
LE25C:  rts                             ;

;----------------------------------------[ Is Samus in lava ]----------------------------------------

;The following function checks to see if Samus is in lava.  If she is, the carry bit is cleared,
;if she is not, the carry bit is set. Samus can only be in lava if in a horizontally scrolling
;room. If Samus is 24 pixels or less away from the bottom of the screen, she is considered to be
;in lava whether its actually there or not.

IsSamusInLava:
LE25D:  lda #$01                        ;
LE25F:  cmp ScrollDir                   ;Set carry bit(and exit) if scrolling up or down.
LE261:  bcs +                           ;
LE263:  lda #$D8                        ;If Samus is Scrolling left or right and within 24 pixels-->
LE265:  cmp ObjectY                     ;of the bottom of the screen, she is in lava. Clear carry bit.
LE268:* rts                             ;

;----------------------------------[ Check lava and movement routines ]------------------------------

LavaAndMoveCheck:
LE269:  lda ObjAction                   ;
LE26C:  cmp #sa_Elevator                ;Is Samus on elevator?-->
LE26E:  beq +                           ;If so, branch.
LE270:  cmp #sa_Dead                    ;Is Samus Dead-->
LE272:  bcs -                           ;If so, branch to exit.
LE274:* jsr IsSamusInLava               ;Clear carry flag if Samus is in lava.
LE277:  ldy #$FF                        ;Assume Samus not in lava.
LE279:  bcs ++++                        ;Samus not in lava so branch.

;Samus is in lava.
LE27B:  sty DamagePushDirection         ;Don't push Samus from lava damage.
LE27D:  jsr ClearHealthChange           ;Clear any pending health changes to Samus.
LE280:  lda #$32                        ;
LE282:  sta SamusBlink                  ;Make Samus blink.
LE284:  lda FrameCount                  ;
LE286:  and #$03                        ;Start the jump SFX every 4th frame while in lava.
LE288:  bne +                           ;
LE28A:  jsr SFX_SamusJump               ;Initiate jump SFX.
LE28D:* lda FrameCount                  ;
LE28F:  lsr                             ;This portion of the code causes Samus to be damaged by-->
LE290:  and #$03                        ;lava twice every 8 frames if she does not have the varia-->
LE292:  bne ++                          ;but only once every 8 frames if she does.
LE294:  lda SamusGear                   ;
LE297:  and #gr_VARIA                   ;Does Samus have the Varia?-->
LE299:  beq +                           ;If not, branch.
LE29B:  bcc ++                          ;Samus has varia. Carry set every other frame. Half damage.
LE29D:* lda #$07                        ;
LE29F:  sta HealthLoChange              ;Samus takes lava damage.
LE2A1:  jsr SubtractHealth              ;
LE2A4:* ldy #$00                        ;Prepare to indicate Samus is in lava.
LE2A6:* iny                             ;Set Samus lava status.
LE2A7:  sty SamusInLava                 ;

SamusMoveVertically:
LE2A9:  jsr VertAccelerate              ;Calculate vertical acceleration.
LE2AC:  lda ObjectY                     ;
LE2AF:  sec                             ;
LE2B0:  sbc ScrollY                     ;Calculate Samus' screen y position.
LE2B2:  sta SamusScrY                   ;
LE2B4:  lda $00                         ;Load temp copy of vertical speed.
LE2B6:  bpl ++++                        ;If Samus is moving downwards, branch.

LE2B8:  jsr TwosCompliment              ;Get twos compliment of vertical speed.
LE2BB:  ldy SamusInLava                 ;Is Samus in lava?
LE2BD:  beq +                           ;If not, branch,-->
LE2BF:  lsr                             ;else cut vertical speed in half.
LE2C0:  beq SamusMoveHorizontally       ;Branch if no vertical mvmnt to Check left/right mvmnt.

;Samus is moving upwards.
LE2C2:* sta ObjectCounter               ;Store number of pixels to move Samus this frame.
LE2C4:* jsr MoveSamusUp                 ;Attempt to move Samus up 1 pixel.
LE2C7:  bcs +                           ;Branch if Samus successfully moved up 1 pixel.

LE2C9:  sec                             ;Samus blocked upwards. Divide her speed by 2 and set the
LE2CA:  ror ObjVertSpeed                ;MSB to reverse her direction of travel.
LE2CD:  ror VertCntrLinear              ;
LE2D0:  jmp SamusMoveHorizontally       ;Attempt to move Samus left/right.

LE2D3:* dec ObjectCounter               ;1 pixel movement is complete.
LE2D5:  bne --                          ;Branch if Samus needs to be moved another pixel.

;Samus is moving downwards.
LE2D7:* beq SamusMoveHorizontally       ;Branch if no vertical mvmnt to Check left/right mvmnt.
LE2D9:  ldy SamusInLava                 ;Is Samus in lava?
LE2DB:  beq +                           ;If not, branch,-->
LE2DD:  lsr                             ;Else reduce Samus speed by 75%(divide by 4).
LE2DE:  lsr                             ;
LE2DF:  beq SamusMoveHorizontally       ;Attempt to move Samus left/right.

LE2E1:* sta ObjectCounter               ;Store number of pixels to move Samus this frame.
LE2E3:* jsr MoveSamusDown               ;Attempt to move Samus 1 pixel down.
LE2E6:  bcs +++                         ;Branch if Samus successfully moved down 1 pixel.

;Samus bounce after hitting the ground in ball form.
LE2E8:  lda ObjAction                   ;
LE2EB:  cmp #sa_Roll                    ;Is Samus rolled into a ball?-->
LE2ED:  bne +                           ;If not, branch.
LE2EF:  lsr ObjVertSpeed                ;Divide verticle speed by 2.
LE2F2:  beq ++                          ;Speed not fast enough to bounce. branch to skip.
LE2F4:  ror VertCntrLinear              ;Move carry bit into MSB to reverse Linear counter.
LE2F7:  lda #$00                        ;
LE2F9:  sec                             ;
LE2FA:  sbc VertCntrLinear              ;Subtract linear counter from 0 and save the results.-->
LE2FD:  sta VertCntrLinear              ;Carry will be cleared.
LE300:  lda #$00                        ;
LE302:  sbc ObjVertSpeed                ;Subtract vertical speed from 0. this will reverse the-->
LE305:  sta ObjVertSpeed                ;vertical direction of travel(bounce up).
LE308:  jmp SamusMoveHorizontally       ;Attempt to move Samus left/right.

;Samus has hit the ground after moving downwards. 
LE30B:* jsr SFX_SamusWalk               ;Play walk SFX.
LE30E:* jsr StopVertMovement            ;Clear vertical movement data.
LE311:  sty SamusGravity                ;Clear Samus gravity value.
LE314:  beq SamusMoveHorizontally       ;Attempt to move Samus left/right.

LE316:* dec ObjectCounter               ;1 pixel movement is complete.
LE318:  bne ----                        ;Branch if Samus needs to be moved another pixel.

SamusMoveHorizontally:
LE31A:  jsr HorzAccelerate              ;Horizontally accelerate Samus.
LE31D:  lda ObjectX                     ;
LE320:  sec                             ;Calculate Samus' x position on screen.
LE321:  sbc ScrollX                     ;
LE323:  sta SamusScrX                   ;Save Samus' x position.
LE325:  lda $00                         ;Load Samus' current horizontal speed.
LE327:  bpl +++                         ;Branch if moving right.

;Samus is moving left.
LE329:  jsr TwosCompliment              ;Get twos compliment of horizontal speed.
LE32C:  ldy SamusInLava                 ;Is Samus in lava?-->
LE32E:  beq +                           ;If not, branch,-->
LE330:  lsr                             ;else cut horizontal speed in half.
LE331:  beq Exit10                      ;Branch to exit if Samus not going to move this frame.

LE333:* sta ObjectCounter               ;Store number of pixels to move Samus this frame.
LE335:* jsr MoveSamusLeft               ;Attempt to move Samus 1 pixel to the left.
LE338:  jsr CheckStopHorzMvmt           ;Check if horizontal movement needs to be stopped.
LE33B:  dec ObjectCounter               ;1 pixel movement is complete.
LE33D:  bne -                           ;Branch if Samus needs to be moved another pixel.

LE33F:  lda SamusDoorData               ;Has Samus entered a door?-->
LE341:  beq Exit10                      ;If not, branch to exit.
LE343:  lda #$01                        ;Door leads to the left.
LE345:  bne ++++                        ;Branch always.

;Samus is moving right.
LE347:* beq Exit10                      ;Branch to exit if Samus not moving horizontally.
LE349:  ldy SamusInLava                 ;Is Samus in lava?-->
LE34B:  beq +                           ;If not, branch,-->
LE34D:  lsr                             ;else cut horizontal speed in half.
LE34E:  beq Exit10                      ;Branch to exit if Samus not going to move this frame.

LE350:* sta ObjectCounter               ;Store number of pixels to move Samus this frame.
LE352:* jsr MoveSamusRight              ;Attempt to move Samus 1 pixel to the right.
LE355:  jsr CheckStopHorzMvmt           ;Check if horizontal movement needs to be stopped.
LE358:  dec ObjectCounter               ;1 pixel movement is complete.
LE35A:  bne -                           ;Branch if Samus needs to be moved another pixel.

LE35C:  lda SamusDoorData               ;Has Samus entered a door?-->
LE35E:  beq Exit10                      ;If not, branch to exit.
LE360:  lda #$00                        ;
LE362:* sta SamusDoorDir                ;Door leads to the right.

Exit10:
LE364:  rts                             ;Exit for routines above and below.

CheckStopHorzMvmt:
LE365:  bcs Exit10                      ;Samus moved successfully. Branch to exit.
LE367:  lda #$01                        ;Load counter with #$01 so this function will not be-->
LE369:  sta ObjectCounter               ;called again.
LE36C:  lda SamusGravity                ;Is Samus on the ground?-->
LE36E:  bne Exit10                      ;If not, branch to exit.
LE370:  lda ObjAction                   ;
LE373:  cmp #sa_Roll                    ;Is Samus rolled into a ball?-->
LE375:  beq Exit10                      ;If so, branch to exit.
LE377:  jmp StopHorzMovement            ;Stop horizontal movement or play walk SFX if stopped.

;-------------------------------------[ Samus vertical acceleration ]--------------------------------

;The following code accelerates/decelerates Samus vertically.  There are 4 possible values for
;gravity used in the acceleration calculation. The higher the number, the more intense the gravity.
;The possible values for gravity are as follows:
;#$38-When Samus has been hit by an enemy.
;#$1A-When Samus is falling.
;#$18-Jump without high jump boots.
;#$12-Jump with high jump boots.

VertAccelerate:
LE37A:  lda SamusGravity                ;Is Samus rising or falling?-->
LE37D:  bne ++                          ;Branch if yes.
LE37F:  lda #$18                        ;
LE381:  sta SamusHorzSpeedMax           ;Set Samus maximum running speed.
LE384:  lda ObjectY                     ;
LE387:  clc                             ;
LE388:  adc ObjRadY                     ;Check is Samus is obstructed downwards on y room-->
LE38B:  and #$07                        ;positions divisible by 8(every 8th pixel).
LE38D:  bne +                           ;
LE38F:  jsr CheckMoveDown               ;Is Samus obstructed downwards?-->
LE392:  bcc ++                          ;Branch if yes.
LE394:* jsr SamusOnElevatorOrEnemy      ;Calculate if Samus standing on elevator or enemy.
LE397:  lda SamusOnElevator             ;Is Samus on an elevator?-->
LE39A:  bne +                           ;Branch if yes.
LE39C:  lda OnFrozenEnemy               ;Is Samus standing on a frozen enemy?-->
LE39E:  bne +                           ;Branch if yes.
LE3A0:  lda #$1A                        ;Samus is falling. Store falling gravity value.
LE3A2:  sta SamusGravity                ;

LE3A5:* ldx #$05                        ;Load X with maximum downward speed.
LE3A7:  lda VertCntrLinear              ;
LE3AA:  clc                             ;The higher the gravity, the faster this addition overflows-->
LE3AB:  adc SamusGravity                ;and the faster ObjVertSpeed is incremented.
LE3AE:  sta VertCntrLinear              ;
LE3B1:  lda ObjVertSpeed                ;Every time above addition sets carry bit, ObjVertSpeed is-->
LE3B4:  adc #$00                        ;incremented. This has the effect of speeding up a fall-->
LE3B6:  sta ObjVertSpeed                ;and slowing down a jump.
LE3B9:  bpl +                           ;Branch if Samus is moving downwards.

;Check if maximum upward speed has been exceeded. If so, prepare to set maximum speed.
LE3BB:  lda #$00                        ;
LE3BD:  cmp VertCntrLinear              ;Sets carry bit.
LE3C0:  sbc ObjVertSpeed                ;Subtract ObjVertSpeed to see if maximum speed has-->
LE3C3:  cmp #$06                        ;been exceeded.
LE3C5:  ldx #$FA                        ;Load X with maximum upward speed.
LE3C7:  bne ++                          ;Branch always.

;Check if maximum downward speed has been reached. If so, prepare to set maximum speed.
LE3C9:* cmp #$05                        ;Has maximum downward speed been reached?-->
LE3CB:* bcc +                           ;If not, branch.

;Max verticle speed reached or exceeded. Adjust Samus verticle speed to max.
LE3CD:  jsr StopVertMovement            ;Clear verticle movement data.
LE3D0:  stx ObjVertSpeed                ;Set Samus vertical speed to max.

;This portion of the function creates an exponential increase/decrease in verticle speed. This is the
;part of the function that does all the work to make Samus' jump seem natural.
LE3D3:* lda VertCntrNonLinear           ;
LE3D6:  clc                             ;This function adds itself plus the linear verticle counter-->
LE3D7:  adc VertCntrLinear              ;onto itself every frame.  This causes the non-linear-->
LE3DA:  sta VertCntrNonLinear           ;counter to increase exponentially.  This function will-->
LE3DD:  lda #$00                        ;cause Samus to reach maximum speed first in most-->
LE3DF:  adc ObjVertSpeed                ;situations before the linear counter.
LE3E2:  sta $00                         ;$00 stores temp copy of current verticle speed.
LE3E4:  rts                             ;

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
        bpl +                           ;Branch if Samus accelerating to the right.

        lda #$FF

*       adc ObjHorzSpeed
        sta ObjHorzSpeed
        tay
        bpl +                           ;Branch if Samus accelerating to the right.

        lda #$00
        sec
        sbc HorzCntrLinear
        tax
        lda #$00
        sbc ObjHorzSpeed
        tay
        jsr SubtractFromZero00And01

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
        sta $00                         ;$00 stores temp copy of current horizontal speed.
        rts                             ;

SubtractFromZero00And01:
        lda #$00
        sec                             ; C set
        sbc $00
        sta $00                         ; M[$00] = 0 - M[$00], C clear
        lda #$00
        sbc $01
        sta $01                         ; M[$01] = 0 - M[$01] - 1
        rts

;----------------------------------------------------------------------------------------------------

;Attempt to move Samus one pixel up.

MoveSamusUp:
LE457:  lda ObjectY                     ;Get Samus' y position in room.
        sec                             ;
        sbc ObjRadY                     ;Subtract Samus' vertical radius.
LE45E:  and #$07                        ;Check if result is a multiple of 8. If so, branch to-->
LE460:  bne +                           ;Only call crash detection every 8th pixel.
LE462:  jsr CheckMoveUp                 ;Check if Samus obstructed UPWARDS.-->
        bcc +++++++                     ;If so, branch to exit(can't move any further).
*       lda ObjAction                   ;
        cmp #sa_Elevator                ;Is Samus riding elevator?-->
        beq +                           ;If so, branch.
        jsr SamusOnElevatorOrEnemy      ;Calculate if Samus standing on elevator or enemy.
        lda SamusHit
        and #$42
        cmp #$42
        clc
        beq ++++++
*       lda SamusScrY
        cmp #$66        ; reached up scroll limit?
        bcs +      ; branch if not
        jsr ScrollUp
        bcc ++
*       dec SamusScrY
*       lda ObjectY
        bne ++
        lda ScrollDir
        and #$02
        bne +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
*       lda #240
        sta ObjectY
*       dec ObjectY
        inc SamusJumpDsplcmnt
        sec
*       rts

; attempt to move Samus one pixel down

MoveSamusDown:
        lda ObjectY
        clc
        adc ObjRadY
        and #$07
        bne +              ; only call crash detection every 8th pixel
        jsr CheckMoveDown       ; check if Samus obstructed DOWNWARDS
        bcc +++++++      ; exit if yes
*       lda ObjAction
        cmp #sa_Elevator        ; is Samus in elevator?
        beq +
        jsr LD976
        lda SamusOnElevator
        clc
        bne ++++++
        lda OnFrozenEnemy
        bne ++++++
*       lda SamusScrY
        cmp #$84        ; reached down scroll limit?
        bcc +      ; branch if not
        jsr ScrollDown
        bcc ++
*       inc SamusScrY
*       lda ObjectY
        cmp #239
        bne ++
        lda ScrollDir
        and #$02
        bne +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' Y coord
*       lda #$FF
        sta ObjectY
*       inc ObjectY
        dec SamusJumpDsplcmnt
        sec
*       rts

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
        ldx #240        ; new Y coord
*       dex
        jmp LE53F

*       inc MapPosY
*       sec
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

*       dec MapPosY
*       sec
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
LE560:  .byte $20                       ;High byte of nametable #0(PPU).
LE561:  .byte $2C                       ;High byte of nametable #3(PPU)

WRAMAddrs:
LE562:  .byte $60                       ;High byte of RoomRAMA(cart RAM).
LE563:  .byte $64                       ;High byte of RoomRAMB(cart RAM).

GetNameAddrs:
LE564:  jsr GetNameTable                ;Get current name table number.
LE567:  and #$01                        ;Update name table 0 or 3.
LE569:  tay                             ;
LE56A:  lda PPUAddrs,y                  ;Get high PPU addr of nametable(dest).
LE56D:  ldx WRAMAddrs,y                 ;Get high cart RAM addr of nametable(src).
LE570:  rts                             ;

;----------------------------------------------------------------------------------------------------

; check if it's time to update nametable (when scrolling is VERTICAL)

LE571:  ldx ScrollDir
        lda ScrollY
        and #$07        ; compare value = 0 if ScrollDir = down, else 7
        cmp Table11,x
        bne --     ; exit if not equal (no nametable update)

LE57C:  ldx ScrollDir                   ;
        cpx TempScrollDir               ;Still scrolling same direction when room was loaded?-->
        bne --                          ;If not, branch to exit.
        lda ScrollY
        and #$F8        ; keep upper 5 bits
        sta $00
        lda #$00
        asl $00
        rol
        asl $00
        rol

LE590:  sta $01  ; $0001 = (ScrollY & 0xF8) << 2 = row offset
        jsr GetNameAddrs
        ora $01
        sta $03
        txa
        ora $01
        sta $01
        lda $00
        sta $02
        lda ScrollDir
        lsr             ; A = 0 if vertical scrolling, 1 if horizontal
        tax
        lda Table01,x
        sta $04
        ldy #$01
        sty PPUDataPending      ; data pending = YES
        dey
        ldx PPUStrIndex
        lda $03
        jsr WritePPUByte                ;Put data byte into PPUDataString.
        lda $02
        jsr WritePPUByte
        lda $04
        jsr SeparateControlBits         ;
*       lda ($00),y
        jsr WritePPUByte
        sty $06
        ldy #$01        ; WRAM pointer increment = 1...
        bit $04  ; ... if bit 7 (PPU inc) of $04 clear
        bpl +
        ldy #$20        ; else ptr inc = 32
*       jsr AddYToPtr00                 ;
        ldy $06
        dec $05
        bne --
        stx PPUStrIndex
        jsr EndPPUString

Table01:
        .byte $20                       ;Horizontal write. PPU inc = 1, length = 32 tiles.
        .byte $9E                       ;Vertical write... PPU inc = 32, length = 30 tiles.

;----------------------------------------------------------------------------------------------------

; attempt to move Samus one pixel left

MoveSamusLeft:
LE626:  lda ObjectX
        sec
        sbc ObjRadX
        and #$07
        bne +              ; only call crash detection every 8th pixel
        jsr CheckMoveLeft       ; check if player is obstructed to the LEFT
        bcc +++++        ; branch if yes! (CF = 0)
*       jsr LD976
        lda SamusHit
        and #$41
        cmp #$41
        clc
        beq ++++
        lda SamusScrX
        cmp #$71        ; reached left scroll limit?
        bcs +      ; branch if not
        jsr ScrollLeft
        bcc ++
*       dec SamusScrX
*       lda ObjectX
        bne +
        lda ScrollDir
        and #$02
        beq +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       dec ObjectX
        sec
        rts

; crash with object on the left

*       lda #$00
        sta SamusDoorData
        rts

; attempt to move Samus one pixel right

MoveSamusRight:
        lda ObjectX
        clc
        adc ObjRadX
        and #$07
        bne +              ; only call crash detection every 8th pixel
        jsr CheckMoveRight      ; check if Samus is obstructed to the RIGHT
        bcc +++++       ; branch if yes! (CF = 0)
*       jsr LD976
        lda SamusHit
        and #$41
        cmp #$40
        clc
        beq ++++
        lda SamusScrX
        cmp #$8F        ; reached right scroll limit?
        bcc +      ; branch if not
        jsr ScrollRight
        bcc ++
*       inc SamusScrX
*       inc ObjectX      ; go right, Samus!
        bne +
        lda ScrollDir
        and #$02
        beq +
        jsr ToggleSamusHi       ; toggle 9th bit of Samus' X coord
*       sec
        rts

; crash with object on the right

*       lda #$00
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

*       inc MapPosX
*       sec
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

*       dec MapPosX
*       sec
*       rts

Table02:
        .byte $07,$00

; check if it's time to update nametable (when scrolling is HORIZONTAL)

LE701:  ldx ScrollDir
        lda ScrollX
        and #$07        ; keep lower 3 bits
        cmp Table02-2,x ; compare value = 0 if ScrollDir = right, else 7
        bne -      ; exit if not equal (no nametable update)

LE70C:  ldx ScrollDir
        cpx TempScrollDir
        bne -
        lda ScrollX
        and #$F8        ; keep upper five bits
        jsr Adiv8       ; / 8 (make 'em lower five)
        sta $00
        lda #$00
        jmp LE590

;-----------------------------------------------------------------------------------------------------

CheckYPlus8:
LE770:  ldx PageIndex
        lda EnRadY,x
        clc
        adc #$08        ; A = EnRadY + 8
        jmp LE783

CheckNegativeYPlus8:
LE77B:  ldx PageIndex
        lda #$00
        sec
        sbc EnRadY,x    ; A = (0 - EnRadY) + 8
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
LE7A2:  ldx PageIndex
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
        jsr LE8BE
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
        jsr $95C0
        jsr LD651
        bcc Exit16      ; CF = 0 if tile # < $80 (solid tile)... CRASH!!!
        cmp #$A0        ; is tile >= A0h? (walkable tile)
        bcs IsWalkableTile
        jmp IsBlastTile  ; tile is $80-$9F (blastable tiles)

IsWalkableTile:
        ldy IsSamus
        beq ++
    ; special case for Samus
        dey      ; = 0
        sty SamusDoorData
        cmp #$A0        ; crash with tile #$A0? (scroll toggling door)
        beq +
        cmp #$A1        ; crash with tile #$A1? (horizontal scrolling door)
        bne ++
        inc SamusDoorData
*       inc SamusDoorData
*       dex
        beq +
        jsr LE98E
        jmp LE7E6

*       sec      ; no crash
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
*       lda #$04
        sta $030A,y
        bne ClcExit
*       dex
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
        jsr LE8BE
        ldy ObjRadY,x
CheckYIntercept:
LE89B:  bne +           ; if RadY (En or Obj) == 0, sec and return.
        sec
        rts             ; else...
*       sty $02         ; M[$02] = RadY
        ldx #$00        ; X = 0
        lda $08         ; A = EnXRoomPos
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
LE8FC:  ldx PageIndex   ; X = Object Index
        lda #$00        ; M[$03] = #$00 - EnRadX
        sec             ; M[$09] = EnXRoomPos
        sbc EnRadX,x    ; M[$08] = EnYRoomPos
LE904:  sta $03         ; M[$0B] = EnNameTable
        jsr LE792       ; Y = EnRadY
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

*       jsr LE95F
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
*       stx $02
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
        lda $02  ; ObjectY
        and #$F8        ; keep upper 5 bits
        asl
        rol $05
        asl
        rol $05
        sta $04
        lda $03  ; ObjectX
        lsr
        lsr
        lsr        ; A = ObjectX / 8
        ora $04
        sta $04
        lda $0B  ; ObjectYHi
        asl
        asl        ; A = ObjectYHi * 4
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
LE9C2:  tay
        jsr $95BD
        cpy #$98
        bcs +++++
; attempt to find a vacant tile slot
        ldx #$C0
*       lda TileRoutine,x
        beq +      ; 0 = free slot
        jsr Xminus16
        bne -
        lda TileRoutine,x
        bne ++++         ; no more slots, can't blast tile
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
*       lsr
        sta TileType,x
*       clc
Exit18: rts

;----------------------------------------------------------------------------------------------------

; Crash detection
; ===============

LF034:  lda #$FF
        sta $73
        sta $010F
; check for crash with Memus
        ldx #$18
*      lda $B0,x
        beq +++++       ; branch if no Memu in slot
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
        beq ++            ; try next one if not
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
*       txa
        sec
        sbc #$08                ; each Memu occupies 8 bytes
        tax
        bpl ------

        ldx #$B0
*       lda ObjAction,x
        cmp #$02
        bne +
        ldy #$00
        jsr IsSamusDead
        beq ++
        jsr LDC7F
        jsr LF277
*       jsr Xminus16
        bmi --
; enemy <--> bullet/missile/bomb detection
*       ldx #$50                ; start with enemy slot #5
LF09F:  lda EnStatus,x   ; slot active?
        beq +              ; branch if not
        cmp #$03
*       beq NextEnemy      ; next slot
        jsr LF152
        lda EnStatus,x
        cmp #$05
        beq ++++
        ldy #$D0                ; first projectile slot
*       lda ObjAction,y  ; is it active?
        beq ++            ; branch if not
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
*       jsr Yplus16      ; next projectile slot
        bne ---
*       ldy #$00
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
*       jsr LDC82
        jsr LF311
*       jsr Xminus16
        cmp #$C0
        bne ---                 
*       jmp SubtractHealth              ;

LF140:  jsr LF1BF
        jsr LF186
        jmp LF1FA

LF149:  jsr LF186
        jsr LF1D2
        jmp LF1FA

LF152:  lda EnYRoomPos,x
        sta $07  ; Y coord
        lda EnXRoomPos,x
        sta $09  ; X coord
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
        jsr SubtractFromZero00And01
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
        jsr IsScrewAttackActive         ;Check if screw attack active.
        ldy #$00
        bcc +++
        lda EnStatus,x
        cmp #$04
        bcs Exit17
        lda EnDataIndex,x
*       sta $010F
        tay
        bmi +
        lda EnemyDataTable8B,y
        and #$10
        bne Exit17
*       ldy #$00
        jsr LF338
        jmp LF306

*       lda #$81
        sta $040E,x
        bne ++
LF2B4:  bcs +
        jsr IsScrewAttackActive         ;Check if screw attack active.
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
*       jsr LF332
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
        jsr IsScrewAttackActive         ;Check if screw attack active.
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
LF323:  lda #$00
LF325:  sta HealthLoChange
LF327:  sta HealthHiChange

Exit22: 
LF329:  rts                             ;Return for routine above and below.

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
LF345:  ldx #$50                ;Load x with #$50
*       jsr DoOneEnemy                  ;
        ldx PageIndex
        jsr Xminus16
        bne -
DoOneEnemy:
LF351:  stx PageIndex           ;PageIndex starts at $50 and is subtracted by $F each iteration.
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

        .word ExitSub       ; rts
        .word LF3BE
        .word LF3E6
        .word LF40D
        .word LF43E
        .word LF483
        .word LF4EE

*       jmp KillObject                  ;Free enemy data slot.

LF37F:  lda EnData05,x
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
        jsr IsObjectVisible             ;Determine if object is within the screen boundaries.
        txa
        bne +
        pla
        pla
*       ldx PageIndex
        rts

LF3AA:  lda EnData05,x
        asl
        rol
        tay
        txa
        jsr Adiv16                      ;/16.
        eor FrameCount
        lsr
        tya
        ror
        ror
        sta EnData05,x
        rts

LF3BE:  lda EnData05,x
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

LF3E6:  lda EnData05,x
        asl
        bmi ++
        lda EnData05,x
        and #$20
        beq +
        ldy EnDataIndex,x
        lda EnemyInitDelayTbl,y         ;
        sta EnDelay,x
        dec EnStatus,x
        bne ++
*       jsr LF6B9
        jsr LF75B
        jsr LF51E
LF40A:* jsr LF536
LF40D:  jmp $95E5                       ; Area Enemy update

UpdateEnemyAnim0:
        jsr UpdateEnemyAnim
        jsr RunObjectRoutine

CheckObjectAttribs:
        ldx PageIndex
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
        jsr UpdateEnemyAnim
        jmp CheckObjectAttribs

LF43E:  jsr LF536
        lda EnStatus,x
        cmp #$03
        beq UpdateEnemyAnim0
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
*       jmp CheckObjectAttribs

LF483:  lda $0404,x
        and #$24
        beq ++++++
        jsr KillObject                  ;Free enemy data slot.
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
        ldx #$00                        ;Increase HealthHi by 0.
        ldy #$50                        ;Increase HealthLo by 5.
*       pha
*       pla                             
        sty HealthLoChange
        stx HealthHiChange
        jsr AddHealth                   ;Add health to Samus.
        jmp SFX_EnergyPickup

PickupMissile:
        lda #$02
        ldy EnDataIndex,x
        beq +
        lda #$1E
*       clc
        adc MissileCount
        bcs +              ; can't have more than 255 missiles
        cmp MaxMissiles  ; can Samus hold this many missiles?
        bcc ++            ; branch if yes
*       lda MaxMissiles  ; set to max. # of missiles allowed
*       sta MissileCount
        jmp SFX_MissilePickup

*       lda FrameCount
        and #$03
        bne +
        dec $040D,x
        bne +
        jsr KillObject                  ;Free enemy data slot.
*       lda FrameCount
        and #$02
        lsr
        ora #$A0
        sta ObjectCntrl
        jmp CheckObjectAttribs

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
        jsr Unknown80B0
        and #$20
        beq +
        pla
        jsr LF515
        pha
*       pla
*       lda #$A0
        jmp LF423

LF515:  sta $040C,x
LF518:  lda #$04
        sta EnStatus,x
        rts

LF51E:  lda ScrollDir
        ldx PageIndex
        cmp #$02
        bcc ++
        lda EnYRoomPos,x     ; Y coord
        cmp #$EC
        bcc ++
        jmp KillObject                  ;Free enemy data slot.

*       jsr SFX_MetroidHit
        jmp GetPageIndex

LF536:  lda EnSpecialAttribs,x
        sta $0A
        lda $0404,x
        and #$20
        beq +
        lda $040E,x
        cmp #$03
        bne +++
        bit $0A
        bvs +++
        lda EnStatus,x
        cmp #$04
        beq +++
        jsr LF515
        lda #$40
        sta $040D,x
        jsr Unknown80B0
        and #$20
        beq +
        lda #$05
        sta EnHitPoints,x
        jmp $95A8
*       rts

*       jsr Unknown80B0
        and #$20
        bne ---
        jsr SFX_Metal
        jmp LF42D

*       lda EnHitPoints,x
        cmp #$FF
        beq --
        bit $0A
        bvc +
        jsr SFX_BossHit
        bne ++
*       jsr GetEnemy8BValue
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
        jsr SFX_BigEnemyHit             ;
*       ldx PageIndex
        jsr Unknown80B0
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
        jsr Unknown80B0
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
        cpy #$02
        beq +
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
*       dec EnHitPoints,x
        bne GetPageIndex
*       lda #$03
        sta EnStatus,x
        bit $0A
        bvs +
        lda $040E,x
        cmp #$02
        bcs +
        lda #$00
        jsr LDCFC
        ldx PageIndex
*       jsr LF844
        lda $960B,y
        jsr ResetAnimIndex
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
        jsr ResetAnimIndex
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

LF676:  jsr Unknown80B0
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
        sta EnResetAnimIndex,x
LF690:  sta EnAnimIndex,x
LF693:  lda #$00
        sta EnAnimDelay,x
*       rts

LF699:  jsr LF844
        lda $965B,y
        cmp EnResetAnimIndex,x
        beq Exit12
        jsr ResetAnimIndex
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
        jsr GetEnemy8BValue
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
*       and #$01
        jsr AddFlagToEnData05
        lsr
        ror
        eor $0403,x
        bpl +
        jsr Unknown81DA
*       lda #$FB
        jsr LF7B3
        lda ScrollDir
        cmp #$02
        bcs +
        jsr LF752
        bcc +
        tya
        eor PPUCNT0ZP
        bcs +++
*       lda EnYRoomPos,x
        cmp ObjectY
        bne +
        inc $82
        inc $82
*       rol
*       and #$01
        asl
        asl
        jsr AddFlagToEnData05
        lsr
        lsr
        lsr
        ror
        eor $0402,x
        bpl +
        jmp Unknown820F

AddFlagToEnData05:
        ora EnData05,x
        sta EnData05,x
*       rts


GetEnemy8BValue:
        ldy EnDataIndex,x
        lda EnemyDataTable8B,y
        rts

LF752:  lda EnNameTable,x
        tay
        eor ObjectHi
        lsr
        rts

LF75B:  lda #$E7
        sta $06
        lda #$18
        jsr AddFlagToEnData05
        ldy EnDataIndex,x
        lda $96AB,y
        beq +++++
        tay
        lda EnData05,x
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
        lda EnData05,x
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
        jsr TwosCompliment              ;
*       lsr
        lsr
        lsr
        cmp $02
        bcc ++
*       lda $06
LF7B3:  and EnData05,x
        sta EnData05,x
*       rts

LF7BA:  dec EnDelay,x
        bne +
        lda EnData05,x
        and #$08
        bne ++
        inc EnDelay,x
*       rts

*       lda EnDataIndex,x
        cmp #$07
        bne +
        jsr SFX_OutOfHole
        ldx PageIndex
*       inc EnStatus,x
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
        jsr Unknown80B0
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
        lda EnData05,x
        bmi +
        lsr
        bcc ++
        jsr Unknown81DA
        jmp ++

*       and #$04
        beq +
        jsr Unknown8206
*       lda #$DF
        jmp LF7B3

UnusedF83E:
LF83E:  lda EnData05,x
LF841:  jmp +

LF844:  lda EnData05,x
        bpl +
        lsr
        lsr
*       lsr
        lda EnDataIndex,x
        rol
        tay
        rts

GetRandom_EnIdxFrCnt:
        txa
        lsr
        lsr
        lsr
        adc FrameCount
        lsr
        rts
        
UnknownF85A:
        ldy EnDataIndex,x
        lda $969B,y
        sta $040D,x
        lda EnemyHitPointTbl,y          ;
        ldy EnSpecialAttribs,x
        bpl +
        asl
*       sta EnHitPoints,x
*       rts

UnknownF870:
LF870:  lda EnData05,x
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
        lda EnData05,x
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
        jsr ResetAnimIndex
        ldx PageIndex
        lda #$01
        sta EnStatus,y
        and EnData05,x
        tax
        lda Table15,x
        sta $0403,y
        lda #$00
        sta $0402,y
        ldx PageIndex
        jsr LF8F8
        lda EnData05,x
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
        lda EnData05,x
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
        lda EnData05,x
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
        jsr LFD8F
        jmp LFA49

; Table used by above subroutine

Table15:
        .byte $02
        .byte $FE

LF92C:  lda #$02
        sta EnRadY,y
        sta EnRadX,y
        ora EnData05,y
        sta EnData05,y
        rts

LF93B:  ldx #$B0
*       jsr LF949
        ldx PageIndex
        jsr Xminus16
        cmp #$60
        bne -
LF949:  stx PageIndex
        lda EnData05,x
        and #$02
        bne +
        jsr KillObject                  ;Free enemy data slot.
*       lda EnStatus,x
        beq Exit19
        jsr ChooseRoutine

; Pointer table to code

        .word ExitSub     ; rts
        .word LF96A
        .word LF991       ; spit dragon's fireball
        .word ExitSub     ; rts
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
        jsr TwosCompliment              ;
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
        jsr ResetAnimIndex
        jsr LF518
        lda #$0A
        sta EnDelay,x
*       jmp LF97C

KillObject:
LFA18:  lda #$00                        ;
LFA1A:  sta EnStatus,x                  ;Store #$00 as enemy status(enemy slot is open).
LFA1D:  rts                             ;

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
        jsr LFD8F
        bcc KillObject                  ;Free enemy data slot.
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
*       jsr KillObject                  ;Free enemy data slot.
*       jmp LF97C

LFA7D:  ldx PageIndex
        lda EnYRoomPos,x
        sta $02
        lda EnXRoomPos,x
        sta $03
        lda EnNameTable,x
        sta $0B
        jmp MakeWRAMPtr

LFA91:  jsr KillObject                  ;Free enemy data slot.
        lda $95DC
        jsr ResetAnimIndex
        jmp LF97C

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
*       jsr KillObject                  ;Free enemy data slot.
*       lda EnCounter,x
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
        lda EnData05,x
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
        sta EnData05,x
        ldy EnDataIndex,x
        jsr LFB7B
        jmp UnknownF85A

*       sta EnDataIndex,x
        lda #$01
        sta EnDelay,x
        jmp KillObject                  ;Free enemy data slot.

LFB7B:  jsr Unknown80B0                 ;
        ror EnData05,x                  ;
        lda EnemyInitDelayTbl,y         ;(Load initial delay for enemy movement.
        sta EnDelay,x                   ;

Exit13: 
        rts                             ;Exit from multiple routines.

UnknownFB88:
        ldx PageIndex
        jsr LF844
        lda $6B01,x
        inc $6B03,x
        dec $6B03,x
        bne +
        pha
        pla
*       bpl +
        jsr TwosCompliment              ;
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
        sta EnResetAnimIndex,x
        jmp LF693

*       lda $963B,y
        cmp EnResetAnimIndex,x
        beq Exit13
        jmp ResetAnimIndex
        
UnknownFBCA:
        ldx PageIndex
        jsr LF844
        lda $965B,y
        cmp EnResetAnimIndex,x
        beq Exit13
        sta EnResetAnimIndex,x
        jmp LF690

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
        jsr LFD8F
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
        jsr LDC7F
        bcs +
        jsr IsScrewAttackActive         ;Check if screw attack active.
        ldy #$00
        bcc +
        clc
        jsr LF311
        lda #$50
        sta HealthLoChange
        jsr SubtractHealth              ;
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

*      jmp KillObject                   ;Free enemy data slot.

LFC98:  lda $B0,x
        jsr ChooseRoutine

; Pointer table to code

        .word ExitSub       ; rts
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
        jsr LFD8F
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
        jsr TwosCompliment              ;
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
        jsr TwosCompliment              ;
        cpy #$80
        bcc ++
*       sta $04
*       jsr LFD8F
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

*       adc $08
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

LFDE3:  lda EndTimerHi
        cmp #$99
        bne +
        clc
        sbc EndTimerLo  ; A = zero if timer just started
        bne +      ; branch if not
        sta $06
        lda #$38
        sta $07
        jsr LDC54
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
        jmp LDC54

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
        beq +      ; exit if tile not active
        jsr ChooseRoutine

; Pointer table to code

        .word ExitSub       ; rts
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
        jmp SubtractHealth              ;

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
        jsr LC328
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
        cmp #$FE        ; end of "tile-blast" animation?
        beq ++
        sta TileAnimFrame,x
        iny
        tya
        sta TileAnimIndex,x
        jsr DrawTileBlast
        bcc +
        ldx PageIndex
        dec TileAnimIndex,x
*       rts

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
        

;-------------------------------[ Interrupt vectors ]---------------------------
.advance $FFFA, $00
.word NMI                       ;NMI vector.
.word RESET                     ;Reset vector.
.word RESET                     ;IRQ vector.