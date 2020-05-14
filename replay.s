; *************************************************************
; ** OPTIONS -- this will set some defaults, but you can     **
; ** override them from your code before including the       **
; ++ replay routine and it will back away ...                **
; *************************************************************
        IFND        opt_USECODE
opt_USECODE = 1
        ENDC

bit_TEMPO = 0
bit_FILTER = 1
bit_SYNC = 2

        IFND        opt_CIA
opt_CIA = 1                                                     ; 1=use CIA timer for playback, 0=use vBlank
                                                                ; vBlank only supports F command parameters <= 1F (no BPM tempo)
                                                                ; and you need to call rc_Music every frame from your code, but
                                                                ; saves quite a few bytes in the resulting binary
        ENDC
        IFND        opt_RASTER
opt_RASTER = 0                                                  ; 1=show rastertime (only for vblank mode)
        ENDC

        INCLUDE     "Includes/custom.i"
        IFND        bgColor
bgColor = $aaa                                                  ; kick 2.0+ background color
;bgCOlor = $05a                                                 ; kick 1.x background color
        ENDC

; amiga PAL clock = 7093789.2 Hz
; Paula clock divider - 2 == PAL video clock
; CIA clock divider - 10
; lowest PT period - 907

; 907 * 2 * 2 = 3620 clock cycles to play a word at lowest samplerate
; /10 = 362 - CIA timer value for first delay
DMADelay1           = 362
; 1.1 scanines = 227 * 1.1 = 250 PAL cycles = 500 CPU cycles
; /10 = 50 - CIA timer value for second delay
DMADelay2           = 100

TotalDMADelay       = DMADelay1+DMADelay2

_custom             = $dff000

;.mod instrument structure offsets
offs_Sample_Start   =       0
offs_Loop_Start     =       4
offs_Sample_Length  =       8
offs_Loop_Length    =      10

OFFSET_TABLE_SIZE   =      12

        ;CIA - related defines
        IFNE        opt_CIA
OpenResource        =    -498
AddICRVector        =      -6
RemICRVector        =     -12
AbleICR             =     -18
SetICR              =     -24
ciatalo             =    $400
ciatahi             =    $500
ciatblo             =    $600
ciatbhi             =    $700
ciacra	            =	 $E00
ciacrb	            =	 $F00
        ENDC

; other LVO's
AllocMem            =    -198
FreeMem             =    -210

; put pointer to mod data in A0 and call rc_Init
rc_Init:
        movem.l     d0-d2/a1-a2/a6,-(sp)

        lea         rc_Ch0(pc),a1                               ;channel var pointer

        moveq       #0,d2                                       ;number of channels
.loopStart:
        move.l      a0,rc_Ch0_DataStart-rc_Ch0(a1)              ;store beginning of ch0 pattern data
        move.l      a0,rc_Ch0_DataRestart-rc_Ch0(a1)            ;init resetart ptr to the beginning of pattern data as well
        move.l      a0,rc_Ch0_DataPtr-rc_Ch0(a1)                ;also init the current note pointer to the same place

.innerLoop:
        cmp.w       #$ffff,(a0)+                                ;check if end of current channel data
        addq.l      #2,a0
        bne.s       .innerLoop                                  ;no? read on ...

        subq.l      #2,a0

        move.l      (a0)+,d0                                    ;restart offset
        asl.l       #2,d0                                       ;offset is in longwords
        add.l       d0,rc_Ch0_DataRestart-rc_Ch0(a1)            ;add offset to beginning of data

        cmp.w       #$ffff,(a0)+                                ;check if this was the last channel ?
        beq.s       .noteLoopEnd                                ;yeah, exit loop!

        lea         rc_Ch1-rc_Ch0(a1),a1                        ;no? next channel structure
        addq.b      #1,d2                                       ;moar channels
        subq.l      #2,a0                                       ;rewind pointer one place b/c it wasn't a marker
      
        bra.s       .loopStart            

.noteLoopEnd:
        lea         rc_Vars(pc),a2                              ;var block pointer
        move.b      d2,rc_NumChannels-rc_Vars(a2)               ;store for later
        move.w      (a0)+,rc_DmaBits-rc_Vars(a2)                ;get initial state of DMACON for this mod

        IFNE        opt_CIA

        move.w      (a0)+,d0                                    ;get initial tempo
        ;this might take some explaining
        ;we keep the vars pointer in init in a2 because of the subsequent OS calls which
        ;will trash a1 HOWEVER, the main routine keeps it in A1 because the interrupt handler is
        ;nice enough to put it there for us when in CIA mode
        ;therefore rc_setTempo expects it in A1 and we can't have it there all the time because
        ;the below calls to AllocMem and OpenResource would trash it and we still need it afterwards
        movea.l     a2,a1   
        bsr         rc_setTempo                                  
        ELSE
        addq        #2,a0                                       ;skip initial tempo in vblank mode
        ENDC

        move.l      a0,rc_SampleOffsetTable-rc_Vars(a2)         ;store pointer to sample offset table

.loop:
        cmp.w       #$ffff,(a0)+
        beq.s       .sampleLoopEnd
        lea         OFFSET_TABLE_SIZE-2(a0),a0
        bra.s       .loop

.sampleLoopEnd:
        ;d2 still has number of channels, no need to load
        move.l      4.w,a6                                      ;execbase
        lea         rc_Ch0(pc),a1                               ;go back from 1st channel        
        
        moveq       #0,d0
.bufferLoop:
        moveq       #0,d1
        
        move.w      (a0)+,d1                                    ;read buffer length

        sne         rc_Compress-rc_Vars(a2)                     ;set compression used flag if either buffer length is non-zero
        beq.s       .noCompress                                 ;but if it's zero skip the rest ...

        move.l      d0,rc_Ch0_BufferStart-rc_Ch0(a1)            ;store buffer start
        move.l      d0,rc_Ch0_BufferWritePtr-rc_Ch0(a1)         ;store buffer write pointer

        asl         #2,d1                                       ;length is in longwords - convert to bytes
        add.l       d1,d0                                       ;keep track of total amount of mem needed for buffer in d0

        move.l      d0,rc_Ch0_BufferEnd-rc_Ch0(a1)              ;store buffer end

        lea         rc_Ch1-rc_Ch0(a1),a1                        ;next channel structure
        dbf         d2,.bufferLoop

        move.l      a0,rc_SampleStart-rc_Vars(a2)               ;store sample pointer
        move.l      d0,rc_BufferSize-rc_Vars(a2)                ;store size of decompression buffer

        ;try to alloc memory
        moveq       #0,d1                                       ;MEMF_ANY
        jsr         AllocMem(a6) 
        move.l      d0,rc_BufferPtr-rc_Vars(a2)                 ;store ptr to decompression buffer

        moveq       #0,d2                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a2),d2
        lea         rc_Ch0(pc),a1                               ;go back from 1st channel 
.updatePtrs:
        ; add offset to the allocated memory buffer to all the buffer ptrs
        add.l       d0,rc_Ch0_BufferStart-rc_Ch0(a1)
        add.l       d0,rc_Ch0_BufferWritePtr-rc_Ch0(a1)
        add.l       d0,rc_Ch0_BufferEnd-rc_Ch0(a1) 
        lea         rc_Ch1-rc_Ch0(a1),a1                        ;next channel structure
        dbf         d2,.updatePtrs
        bra.s       .compress

.noCompress:
        move.l      a0,rc_SampleStart-rc_Vars(a2)               ;store sample pointer

.compress:
        ; CIA interrupt setup starts here
        IFNE        opt_CIA
.setupCIA:
        lea	    rc_CIAName-rc_Vars(a2),a1                   ;a1 was ch0 ptr - no longer needed as we already initialized the ch structures
                                                                ;put ptr to cia resource name in there

        jsr         OpenResource(a6)                            ;d0-d1/a0-a1 are scratch registers for system calls (a6 still has execbase)
        move.l      d0,rc_CIAResource-rc_Vars(a2)               ;store CIA resource pointer
        beq.s	    .return                                     ;exit if OpenResource failed  

        ;init the interrupt structure
        lea         rc_MusicInterrupt(pc),a0
        lea         rc_CIAServer(pc),a1
        move.l      a0,rc_CodePtr-rc_CIAServer(a1)              ;poke replay routine address to interrupt structure
        move.l      a2,rc_DataPtr-rc_CIAServer(a1)              ;poke var block address to interrupt structure
        lea         rc_IntName(pc),a0
        move.l      a0,rc_IntnamePtr-rc_CIAServer(a1)           ;and poke the location of the interrupt name

        move.l      d0,a6                                       ;move cia resource to a6 for the AddICRVector call
        moveq       #1,d2
        ;try to own the timer, first timer b, then timer a
.timerLoop:
        move.l	    d2,d0                                       ;which timer bit goes in d0

        jsr	    AddICRVector(a6)                            ;try to own the timer
        tst.l       d0                                          ;did we get it?
        beq.s       .success                                    ;yes!
        dbf         d2,.timerLoop                               ;no, try the other one

        ;we didn't get either timer, exit
        bra.s       .return

.success:
        move.w      d2,rc_CIATimer-rc_Vars(a2)                  ;store the timer we got
        
        move.l      a2,a1                                       ;rc_setTimer wants var pointer in a1 ... comes in handy later when exec fills this for us when calling the interrupt
        move.w      #DMADelay1,d0
        bsr.s       rc_setTimer                                 ;and set it
        move.w      #2,rc_IntSwitch-rc_Vars(a2)                 ;init jumptable position
.return:
        ENDC
        movem.l     (sp)+,d0-d2/a1-a2/a6

        rts
        
        IFNE        opt_CIA
; now set timer
rc_setTimer:
        ;can we use a6 here ???? check in the main routine when processing the tempo command
        lea         $bfd000,a6                                  ;CIA B
        move.w      d0,d1
        lsr.w       #8,d1

        tst.w       rc_CIATimer-rc_Vars(a1)                     ;check which timer we got                    
        bne.s       .setTimerB

.setTimerA:
        move.b	    d0,ciatalo(a6)
        move.b	    d1,ciatahi(a6)
        rts

.setTimerB:
        move.b	    d0,ciatblo(a6)
        move.b	    d1,ciatbhi(a6)
        rts
        ENDC
        
; main playroutine, call this every vblank interrupt
rc_Music:
        ;CIA - just call rc_Music once to start playing, this will start the CIA timer which calls rc_MusicInner 
        IFNE        opt_CIA

        movem.l     d0/a4-a6,-(sp)
        lea         rc_Vars(pc),a4                              ;pointer to vars block 
        move.l      rc_CIAResource(pc),a6                       ;ciab.resource
        lea         $bfd000,a5                                  ;CIA B
        tst.w       rc_CIATimer-rc_Vars(a4)                     ;which timer are we using?
        bne.s       .timerB
        
.timerA:
        bclr        #3,ciacra(a5)                               ;runmode = continous
        bset        #0,ciacra(a5)                               ;timer started
        move.w      #1,d0                                       ;clear timer a interrupt
        jsr         SetICR(a6)
        move.w      #$81,d0                                     ;enable timer a interrupts
        jsr         AbleICR(a6)
        bra.b       .exit

.timerB:
        bclr        #3,ciacrb(a5)                               ;runmode = continous
        bset        #0,ciacrb(a5)                               ;start timer
        move.w      #2,d0                                       ;clear timer b interrupt
        jsr         SetICR(a6)
        move.w      #$82,d0                                     ;enable timer a interrupts
        jsr         AbleICR(a6)
.exit:
        movem.l     (sp)+,d0/a4-a6
        rts

rc_MusicInterrupt:
        move.l      a2,-(sp)
        ;jumptable for CIA mode
        move.w      rc_IntSwitch-rc_Vars(a1),d0
        add.w       #1,d0
        cmpi.w      #3,d0
        bne.s       .noOverflow
        moveq       #0,d0
.noOverflow:
        move.w      d0,rc_IntSwitch-rc_Vars(a1)                 ;save next position
        add.w       d0,d0                                       ;jumptable is words
        move.w      rc_CiaJumpTable(pc,d0.w),d0                 ;get offset from jumptable
        jmp         rc_CiaJumpTable(pc,d0.w)

rc_CiaJumpTable:
        dc.w        rc_Music1-rc_CiaJumpTable
        dc.w        rc_Music2-rc_CiaJumpTable
        dc.w        rc_Music3-rc_CiaJumpTable

rc_Music1:
        movem.l     d2-d6/a3/a4,-(sp)

        move.w      #DMADelay2,d0
        bsr.w       rc_setTimer

        ELSE
        movem.l     d0-d6/a0-a6,-(sp)
        lea         _custom,a0
        ENDC

        IFNE        opt_RASTER
        move.w      #$fff,$180(a0)
        ENDC

        IFEQ        opt_CIA
        lea         rc_Vars(pc),a1                              ;pointer to vars block  
        ENDC

        lea         rc_Ch0(pc),a2                               ;channel structure pointer into A2
        move.l      rc_SampleOffsetTable(pc),a3
        move.l      rc_SampleStart(pc),a4

        move.w      rc_DmaBits-rc_Vars(a1),d6
        move.w      d6,dmacon(a0)                               ;stop DMA for selected channels

        IFEQ        opt_CIA
        ; store current raster position for later
        move.l      vposr(a0),d5
        and.l       #$1ffff,d5
        add.l       #$0780,d5
        
        ELSE
        move.w      d6,rc_DmaBitsTemp-rc_Vars(a1)               ;store DMA bits for rc_Music2 if CIA
        ENDC

.readNotes:
        moveq       #0,d2                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a1),d2

        moveq       #0,d4                                       ;init DMA bits
.loop:

.getNextNote:
        tst.w       rc_Ch0_ReadLength-rc_Ch0(a2)                ;do we have bytes to read from the buffer still?
        bne         .lookBack                                   ;yes, do it!

        move.l      rc_Ch0_DataPtr-rc_Ch0(a2),a6                ;get current note pointer
.readNote:
        move.l      (a6)+,d1                                    ;read current note into D1
        move.l      a6,rc_Ch0_DataPtr-rc_Ch0(a2)                ;store current channel note pointer
        cmpi.l      #$c0000000,d1                               ;check for control words
        bhi         .controlWord

        ; store note into decompression buffer
.processNote:
        bsr         .writeBack

.processVolume
        ;process volume
        move.l      d1,d0                                       ;transfer to d0 to extract volume
        rol.l       #7,d0                                       ;rotate to beginning of register
        and.w       #$7F,d0                                     ;mask unnecessary bits
        move.w      d0,rc_Ch0_VOL-rc_Ch0(a2)                    ;store the volume

        btst        #23,d1
        beq.s       .noStopDma
        bset        d2,d4                                       ;store that we need to stop DMA in order to trigger a new note
.noStopDma:

        btst        #24,d1                                      ;check format of instruction
        beq.s       .noNewNote

.newNote:
        move.l      d1,d0
        moveq       #10,d3                                      ;d3 is shift amount (greater than 7 can't use immediate addressing)
        ror.l       d3,d0                                       ;d3 is free now
        and.w       #$1FFF,d0
        beq.s       .noPointerChange
        sub.w       #1,d0
        mulu.w      #OFFSET_TABLE_SIZE,d0

        ; copy values from sample offset table to channel vars
        move.l      offs_Sample_Start(a3,d0),a5
        adda.l      a4,a5
        move.l      a5,rc_Ch0_PTR-rc_Ch0(a2)
        move.l      offs_Loop_Start(a3,d0),a5
        adda.l      a4,a5        
        move.l      a5,rc_Ch0_PTR_loop-rc_Ch0(a2)
        
        move.w      offs_Sample_Length(a3,d0),rc_Ch0_LEN-rc_Ch0(a2)
        move.w      offs_Loop_Length(a3,d0),rc_Ch0_LEN_loop-rc_Ch0(a2)

.noPointerChange:
        and.w       #$3ff,d1
        move.w      d1,rc_Ch0_PER-rc_Ch0(a2)
        bra.s       .nextChannel

.noNewNote:
        ;process sample offset
        move.l      d1,d0
        ror.l       #8,d0        
        and.l       #$7fff,d0      
        beq.s       .noPointerChange2

        move.l      (rc_Ch0_PTR_loop-rc_Ch0)(a2),d3             ;get old pointer value
        add.l       d0,d3                                       ;add offset
        move.l      d3,(rc_Ch0_PTR_loop-rc_Ch0)(a2)             ;store it back for later

.noPointerChange2:
        ;process period change
        and.w       #$ff,d1
        ext.w       d1
        beq.s       .noPeriodChange
        move.w      (rc_Ch0_PER-rc_Ch0)(a2),d3                  ;get old period
        add.w       d1,d3                                       ;add offset
        move.w      d3,(rc_Ch0_PER-rc_Ch0)(a2)                  ;store it back for later

.noPeriodChange:
.nextChannel:
        lea         rc_Ch1-rc_Ch0(a2),a2                        ;next channel structure
        dbf         d2,.loop

        move.w      d4,rc_DmaBits-rc_Vars(a1)                   ;store DMA stop flags for next tick

        IFNE        opt_RASTER
        move.w      #bgColor,$180(a0)
        ENDC
        ; in vblank mode continue onto the next part of the playrouting
        IFEQ        opt_CIA
        bra         rc_Music2
        ;for cia mode return from interrupt, next interrupt should trigger the second part
        ELSE
        movem.l     (sp)+,d2-d6/a3/a4                           ;but make sure to restore the registers when returning from interrupt
        move.l      (sp)+,a2                                    ;also restore a2 which gets stored at the top of the interrupt dispatcher
        rts
        ENDC

.writeBack
        tst.b       rc_Compress-rc_Vars(a1)                     ;only do this if the mod is actually compressed
        beq.s       .exit

        move.l      rc_Ch0_BufferWritePtr-rc_Ch0(a2),a5         ;read the buffer write pointer
        cmpa.l      rc_Ch0_BufferEnd-rc_Ch0(a2),a5              ;check if we need to wrap around
        bne.s       .noWrap2

        move.l      rc_Ch0_BufferStart-rc_Ch0(a2),a5            ;wrap back around to the beginning of the buffer
.noWrap2:
        move.l      d1,(a5)+                                    ;store the current note into the buffer
        move.l      a5,rc_Ch0_BufferWritePtr-rc_Ch0(a2)         ;store the buffer write pointer
.exit:
        rts

.controlWord:
        cmpi.l      #$ffff0000,d1                               ;is it the end of channel data?
        bhs.s       .channelEnd
        
        IFNE        opt_USECODE
        cmpi.l      #$c0007fff,d1                               ;is it a player control command?
        bhi.s       .compression 
        bsr.b       .playerCommand
        bsr.b       .writeBack
        bra         .getNextNote
        ENDC 

.compression:
        ; not a player command - compression ...
        ; compression lookback only at this time
        move.l      d1,d0
        and.w       #$7fff,d1                                   ;read length is in d1
        move.w      d1,(rc_Ch0_ReadLength-rc_Ch0)(a2)           ;store it
        asl.l       #1,d0
        and.l       #$7fff0000,d0
        swap        d0                                          ;read offset is in d0
        add.w       d0,d0                                       ;offset is in longwords
        add.w       d0,d0

        move.l      (rc_Ch0_BufferWritePtr-rc_Ch0)(a2),d1       ;get end of buffer
        sub.l       d0,d1

        cmp.l       (rc_Ch0_BufferStart-rc_Ch0)(a2),d1          ;check for wrap
        blt.s       .wrapBuffer

        move.l      d1,(rc_Ch0_BufferReadPtr-rc_Ch0)(a2)        ;store new read ptr
        bra         .getNextNote

.channelEnd:
        ;rewind channel data to the restart pointer
        move.l      rc_Ch0_DataRestart-rc_Ch0(a2),a6
        bra         .readNote

.lookBack:
        move.l      rc_Ch0_BufferReadPtr-rc_Ch0(a2),a5          ;current buffer read ptr
        cmpa.l      rc_Ch0_BufferEnd-rc_Ch0(a2),a5              ;check if we need to wrap around
        bne.s       .noWrap       
       
        move.l      rc_Ch0_BufferStart-rc_Ch0(a2),a5            ;wrap back around to the beginning of the buffer
.noWrap:
        move.l      (a5)+,d1
        subq.w      #1,rc_Ch0_ReadLength-rc_Ch0(a2)
        move.l      a5,rc_Ch0_BufferReadPtr-rc_Ch0(a2)
        bra         .processNote

.wrapBuffer:
        sub.l       (rc_Ch0_BufferStart-rc_Ch0)(a2),d1
        add.l       (rc_Ch0_BufferEnd-rc_Ch0)(a2),d1

        move.l      d1,(rc_Ch0_BufferReadPtr-rc_Ch0)(a2)        ;store new read ptr
        bra         .getNextNote

        IFNE        opt_USECODE
.playerCommand:
        move.w      d1,d0                                       ;low 15 bits is player command
        and.w       #$0fff,d0                                   ;d0 is command param
        rol.w       #4,d1                                       
        and.w       #7,d1                                       ;d1 is the command  

        ; command jumptable
        add.w       d1,d1                                       ;jumptable is words
        move.w      .cmdJumpTable(pc,d1.w),d1                   ;get offset from jumptable
        jmp         .cmdJumpTable(pc,d1.w)
.nopCmd:
        rts

.cmdJumpTable:
        IFNE        (opt_USECODE&1<<bit_TEMPO)&&opt_CIA
        dc.w        rc_setTempo-.cmdJumpTable
        ELSE
        dc.w        .nopCmd-.cmdJumpTable
        ENDC
        
        IFNE        opt_USECODE&1<<bit_FILTER        
        dc.w        .setFilter-.cmdJumpTable
        ELSE
        dc.w        .nopCmd-.cmdJumpTable
        ENDC
        
        IFNE        opt_USECODE&1<<bit_SYNC
        dc.w        .nopCmd-.cmdJumpTable
        ; no need to pad the last command jumptable entry if not used, 
        ; as no other command will try to look past this spot in the 
        ; jumptable
        ENDC
        
        ENDC

        ;we need this for CIA tempo init even if there are no tempo change commands
        IFNE        opt_CIA
rc_setTempo:
        move.b      d0,d1 
        and.w       #$ff,d1                                     ;d1 is bpm
        asr.w       #8,d0                                       ;d0 is multiplier
        addq        #1,d0
        mulu        d1,d0
        move.l      rc_CIAMagicNumber-rc_Vars(a1),d1
        divu        d0,d1
        sub.l       #TotalDMADelay,d1
        move.w      d1,rc_TimerValue-rc_Vars(a1)
        rts

        ;move.l      rc_CIAMagicNumber-rc_Vars(a1),d1
        ;divu        d2,d1
        ;sub.l       #TotalDMADelay,d1
        ;move.w      d1,rc_TimerValue-rc_Vars(a1)                ;store the new timer value      
        ;bra         .getNextNote                                ;end
        ENDC
        IFNE        opt_USECODE&1<<bit_FILTER
.setFilter:
        rts                                                     ;end
        ENDC
        IFNE        opt_USECODE&1<<bit_SYNC
.emitSync:
        rts                                                     ;end
        ENDC                

rc_Music2:
        IFNE        opt_CIA
        movem.l     d4-d6,-(sp)
        move.w      rc_TimerValue-rc_Vars(a1),d0
        bsr.w       rc_setTimer        
        ENDC

        IFEQ        opt_CIA
.rasterWait1:
        move.l      vposr(a0),d4
        and.l       #$1ffff,d4
        cmp.l       d4,d5
        bgt.s       .rasterWait1    
        ENDC   

        IFNE        opt_RASTER
        move.w      #$fff,$180(a0)
        ENDC
        
        ;poke Paula for all channels 
        lea         rc_Ch0(pc),a2                               ;channel structure pointer into A1
        moveq       #0,d0                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a1),d0
        moveq       #0,d1
        
        IFNE        opt_CIA
        move.w      rc_DmaBitsTemp-rc_Vars(a1),d6               ;if CIA timing we need to reload DMA bits into D6 coz it might be trashed outside the interrupt
        ENDC                                                    ;for vblank it will stay loaded from the first part of the routine

.pokePaula:
        move.b      rc_AudioOffsets-rc_Vars(a1,d0),d1           ;get audio register offset for current channel (we go last to first)

        btst        d0,d6				
        beq.s       .noPokePtrs
        move.l      rc_Ch0_PTR-rc_Ch0(a2),ac_ptr(a0,d1)         ;poke ac_ptr
        move.w      rc_Ch0_LEN-rc_Ch0(a2),ac_len(a0,d1)         ;poke ac_len
.noPokePtrs:
        move.w      rc_Ch0_PER-rc_Ch0(a2),ac_per(a0,d1)         ;poke ac_per
        move.w      rc_Ch0_VOL-rc_Ch0(a2),ac_vol(a0,d1)         ;poke ac_vol

        lea         rc_Ch1-rc_Ch0(a2),a2                        ;next channel structure

        dbf         d0,.pokePaula

        ;re-enable audio DMA
        or.w        #$8000,d6
        move.w      d6,dmacon(a0)

        IFNE        opt_RASTER
        move.w      #bgColor,$180(a0)
        ENDC

        IFNE        opt_CIA

        movem.l     (sp)+,d4-d6/a2                              ;also restore A2 which was stored in the interrupt dispatcher
        rts                                                     ;return from interrupt in CIA mode, next one will trigger the rest of the code

        ELSE

        ;wait 1.1 rasterlines here
        move.l      vposr(a0),d2
        and.l       #$1ffff,d2
        add.l       #$0110,d2
.rasterWait2:
        move.l      vposr(a0),d3
        and.l       #$1ffff,d3
        cmp.l       d3,d2
        bgt.s       .rasterWait2  

        ENDC

rc_Music3:
        IFNE        opt_RASTER
        move.w      #$fff,$180(a0)
        ENDC

        IFNE        opt_CIA
        movem.l     d2/d3,-(sp)
        move.w      #DMADelay1,d0
        bsr.w       rc_setTimer 
        ENDC

        ;re-poke the sample pointers for looped sounds
        lea         rc_Ch0(pc),a2                               ;channel structure pointer into A1   
        moveq       #0,d0                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a1),d0

.rePokePaula:
        move.b      rc_AudioOffsets-rc_Vars(a1,d0),d1           ;a2 has ptr to audiooffsets,(we go last to first)

        move.l      rc_Ch0_PTR_loop-rc_Ch0(a2),ac_ptr(a0,d1)    ;always repoke lthe pointers so we can do wavetable stuff
                                                                ;replay code makes sure this doesn't change if not needes, so we can safely do this
 
        move.w      rc_Ch0_LEN_loop-rc_Ch0(a2),ac_len(a0,d1)    ;poke ac_len
        lea         rc_Ch1-rc_Ch0(a2),a2                        ;next channel structure

        dbf         d0,.rePokePaula 
        
        IFNE        opt_RASTER
        move.w      #bgColor,$180(a0)
        ENDC

        IFNE        opt_CIA
        movem.l     (sp)+,d2/d3/a2                              ;also restore A2 which was stored in the interrupt dispatcher
        ELSE
        movem.l     (sp)+,d0-d6/a0-a6
        ENDC
        rts                                                     ;all done!

rc_StopMusic:
        IFNE        opt_CIA
         
        movem.l     d0-d1/a0-a1/a4-a6,-(sp)
        lea         rc_Vars(pc),a4                              ;pointer to vars block 
        move.l      rc_CIAResource(pc),a6                       ;ciab.resource
        lea         $bfd000,a5                                  ;CIA B
        tst.w       rc_CIATimer-rc_Vars(a4)                     ;which timer are we using?
        bne.s       .timerB       
        
.timerA:
        move.w      #1,d0                                       ;disable timer 1 ICR
        jsr         AbleICR(a6)
        and.b       #$fe,ciacra(a5)                             ;clear CIA start bit

        moveq       #1,d0                                       ;try timer A
        lea         rc_CIAServer(pc),a1                         ;my interrupt struct
        jsr         RemICRVector(a6)

        bra.s       .exit
.timerB:
        move.w      #2,d0
        jsr         AbleICR(a6)
        and.b       #$fe,ciacrb(a5)

        moveq       #2,d0                                       ;try timer B
        lea         rc_CIAServer(pc),a1                         ;my interrupt struct
        jsr         RemICRVector(a6)
.exit:

        ELSE

        movem.l     d0-d1/a0-a1/a6,-(sp)
        
        ENDC

        lea         _custom,a1
        move.w	    #$000f,dmacon(a1)

        ;free decompression buffer memory
        move.l      4.w,a6                                      ;execbase
        move.l      rc_BufferSize(pc),d0
        move.l      rc_BufferPtr(pc),a1
        jsr         FreeMem(a6)

        IFNE        opt_CIA
        movem.l     (sp)+,d0-d1/a0-a1/a4-a6
        ELSE
        movem.l     (sp)+,d0-d1/a0-a1/a6
        ENDC

        rts
        
;
; variables
;
rc_Vars:
rc_NumChannels:
        dc.b        0
rc_Compress:
        dc.b        0
        EVEN

; decompression buffer location and size (to be able to free it on exit)
rc_BufferSize:
        dc.l        0
rc_BufferPtr:
        dc.l        0

rc_DmaBits:
        dc.w        0
        IFNE        opt_CIA
rc_DmaBitsTemp:
        dc.w        0
        ENDC

rc_SampleOffsetTable:
        dc.l        0        

rc_SampleStart:
        dc.l        0

rc_AudioOffsets:
        dc.b        aud0,aud1,aud2,aud3

;CIA player variables - only use if opt_CIA is set to non-zero
        IFNE        opt_CIA

rc_IntSwitch:
        dc.w        0

rc_CIAName:	
        dc.b        "ciab.resource",0
rc_CIAResource:	
        dc.l        0
rc_CIATimer:
        dc.w        0
rc_CIAMagicNumber:
        dc.l        1773448                                     ;PAL
        ;dc.l       1789773                                     ;NTSC
rc_TimerValue:	
        ;dc.w        $376c-TotalDMADelay                        ;125 bpm
        dc.w        $2b4c-TotalDMADelay                         ;160 bpm

rc_CIAServer:
        dc.l        0,0
        dc.b        2,127                                       ;type, priority
rc_IntnamePtr:        
        dc.l        0
rc_DataPtr:
        dc.l        0
rc_CodePtr:
        dc.l        0                                           ;poke playroutine 

rc_IntName:
        dc.b        "RetroReplay Interrupt",0

        ENDC
        EVEN

rc_Ch0:
rc_Ch0_DataStart:
        dc.l        0
rc_Ch0_DataRestart:
        dc.l        0
rc_Ch0_DataPtr:
        dc.l        0

; decompression pointers
rc_Ch0_BufferStart:
        dc.l        0
rc_Ch0_BufferEnd:
        dc.l        0
rc_Ch0_BufferReadPtr:
        dc.l        0
rc_Ch0_BufferWritePtr:
        dc.l        0
rc_Ch0_ReadLength:
        dc.w        0

; paula registers
rc_Ch0_PTR:
        dc.l        0
rc_Ch0_PTR_loop:
        dc.l        0	
rc_Ch0_LEN:
        dc.w        0
rc_Ch0_LEN_loop:
        dc.w        0	
rc_Ch0_PER:
        dc.w        0
rc_Ch0_VOL:
        dc.w        0        

        EVEN

rc_Ch1:
rc_Ch1_DataStart:
        dc.l        0
rc_Ch1_DataRestart:
        dc.l        0
rc_Ch1_DataPtr:
        dc.l        0       

; decompression pointers
rc_Ch1_BufferStart:
        dc.l        0
rc_Ch1_BufferEnd:
        dc.l        0
rc_Ch1_BufferReadPtr:
        dc.l        0
rc_Ch1_BufferWritePtr:
        dc.l        0
rc_Ch1_ReadLength:
        dc.w        0

; paula registers
rc_Ch1_PTR:
        dc.l        0
rc_Ch1_PTR_loop:
        dc.l        0	
rc_Ch1_LEN:
        dc.w        0
rc_Ch1_LEN_loop:
        dc.w        0
rc_Ch1_PER:
        dc.w        0
rc_Ch1_VOL:
        dc.w        0

        EVEN

rc_Ch2:
rc_Ch2_DataStart:
        dc.l        0
rc_Ch2_DataRestart:
        dc.l        0
rc_Ch2_DataPtr:
        dc.l        0

; decompression pointers
rc_Ch2_BufferStart:
        dc.l        0
rc_Ch2_BufferEnd:
        dc.l        0
rc_Ch2_BufferReadPtr:
        dc.l        0
rc_Ch2_BufferWritePtr:
        dc.l        0
rc_Ch2_ReadLength:
        dc.w        0

; paula registers
rc_Ch2_PTR:
        dc.l        0
rc_Ch2_PTR_loop:
        dc.l        0	
rc_Ch2_LEN:
        dc.w        0
rc_Ch2_LEN_loop:
        dc.w        0
rc_Ch2_PER:
        dc.w        0
rc_Ch2_VOL:
        dc.w        0

        EVEN

rc_Ch3:
rc_Ch3_DataStart:
        dc.l        0
rc_Ch3_DataRestart:
        dc.l        0
rc_Ch3_DataPtr:
        dc.w        0

; decompression pointers
rc_Ch3_BufferStart:
        dc.l        0
rc_Ch3_BufferEnd:
        dc.l        0
rc_Ch3_BufferReadPtr:
        dc.l        0
rc_Ch3_BufferWritePtr:
        dc.l        0
rc_Ch3_ReadLength:
        dc.w        0

; paula registers
rc_Ch3_PTR:
        dc.l        0
rc_Ch3_PTR_loop:
        dc.l        0	
rc_Ch3_LEN:
        dc.w        0
rc_Ch3_LEN_loop:
        dc.w        0
rc_Ch3_PER:
        dc.w        0
rc_Ch3_VOL:
        dc.w        0

        EVEN