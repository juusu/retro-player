; *************************************************************
; ** OPTIONS -- this will set some defaults, but you can     **
; ** override them from your code before including the       **
; ++ replay routine and it will back away ...                **
; *************************************************************

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

; put pointer to mod data in A0 and call rc_Init
rc_Init:
        IFNE        opt_CIA
        movem.l     d0-d2/a1-a2/a6,-(sp)
        ELSE
        movem.l     d0-d1/a1-a2,-(sp)
        ENDC

        lea         rc_Ch0(pc),a1                               ;channel var pointer
        lea         rc_Vars(pc),a2                              ;var block pointer
        moveq       #0,d0                                       ;number of channels
.loopStart:
        move.l      a0,rc_Ch0_DataStart-rc_Ch0(a1)              ;store beginning of ch0 pattern data
        move.l      a0,rc_Ch0_DataPtr-rc_Ch0(a1)                ;also init the current note pointer to the same place

.innerLoop:
        cmp.w       #$ffff,(a0)+                                ;check if end of current channel data
        addq.l      #2,a0
        bne.s       .innerLoop                                  ;no? read on ...

        subq.l      #2,a0
        cmp.w       #$ffff,(a0)+                                ;check if this was the last channel ?
        beq.s       .noteLoopEnd                                ;yeah, exit loop!

        lea         rc_Ch1-rc_Ch0(a1),a1                        ;no? next channel structure
        addq.b      #1,d0                                       ;moar channels
        subq.l      #2,a0                                       ;rewind pointer one place b/c it wasn't a marker
      
        bra.s       .loopStart            

.noteLoopEnd:
        move.b      d0,rc_NumChannels-rc_Vars(a2)               ;store for later
        move.w      (a0)+,rc_DmaBits-rc_Vars(a2)                ;get initial state of DMACON for this mod
        move.l      a0,rc_SampleOffsetTable-rc_Vars(a2)         ;store pointer to sample offset table

.loop:
        cmp.w       #$ffff,(a0)+
        beq.s       .sampleLoopEnd
        lea         OFFSET_TABLE_SIZE-2(a0),a0
        bra.s       .loop

.sampleLoopEnd:
        ;d0 still has number of channels, no need to load
        lea         rc_Ch0(pc),a1                               ;go back from 1st channel        
.bufferLoop:
        moveq       #0,d1
        move.w      (a0),d1                                     ;buffer length
        sne         rc_Compress-rc_Vars(a2)                     ;set compression used flag if either buffer length is non-zero
        beq.s       .noCompress                                 ;but if it's zero skip the rest ...

        move.l      a0,rc_Ch0_BufferStart-rc_Ch0(a1)            ;store buffer start
        move.l      a0,rc_Ch0_BufferWritePtr-rc_Ch0(a1)         ;store buffer write pointer
        asl         #2,d1                                       ;length is in longwords - convert to bytes
        adda.l      d1,a0                                       ;calculate next channel's buffer location
        move.l      a0,rc_Ch0_BufferEnd-rc_Ch0(a1)              ;store buffer end

        lea         rc_Ch1-rc_Ch0(a1),a1                        ;next channel structure
        dbf         d0,.bufferLoop

.noCompress:
        move.l      a0,rc_SampleStart-rc_Vars(a2)               ;store sample pointer

        ; CIA interrupt setup starts here
        IFNE        opt_CIA
.setupCIA:
        lea	        rc_CIAName-rc_Vars(a2),a1                   ;a1 was ch0 ptr - no longer needed as we already initialized the ch structures
                                                                ;put ptr to cia resource name in there
        move.l      4.w,a6                                      ;execbase
        jsr         OpenResource(a6)                            ;d0-d1/a0-a1 are scratch registers for system calls
        move.l      d0,rc_CIAResource-rc_Vars(a2)               ;store CIA resource pointer
        beq.s	    .return                                     ;exit if OpenResource failed  

        ;init the interrupt structure
        lea         rc_MusicInterrupt(pc),a0
        lea         rc_CIAServer(pc),a1
        move.l      a0,rc_ReplayPtr-rc_CIAServer(a1)            ;poke replay routine address to interrupt structure
        lea         rc_IntName(pc),a0
        move.l      a0,rc_IntnamePtr-rc_CIAServer(a1)           ;and poke the location of the interrupt name

        move.l      d0,a6                                       ;move cia resource to a6 for the AddICRVector call
        moveq       #1,d2
        ;try to own the timer, first timer b, then timer a
.timerLoop:
        move.l	    d2,d0                                       ;which timer bit goes in d0

        jsr	        AddICRVector(a6)                            ;try to own the timer
        tst.l       d0                                          ;did we get it?
        beq.s       .success                                    ;yes!
        dbf         d2,.timerLoop                               ;no, try the other one

        ;we didn't get either timer, exit
        bra.s       .return

.success:
        move.w      d2,rc_CIATimer-rc_Vars(a2)                  ;store the timer we got
        
        move.w	    rc_TimerValue(pc),d0                        ;get the initial timer value 
        bsr.s       .setTimer                                   ;and set it

        bra.s       .return

; now set timer
.setTimer:
        ;can we use a6 here ???? check in the main routine when processing the tempo command
        lea         $bfd000,a6                                  ;CIA B
        move.w      d0,d1
        lsr.w       #8,d1

        tst.w       rc_CIATimer-rc_Vars(a2)                     ;check which timer we got                    
        bne.s       .setTimerB

.setTimerA:
        move.b	    d0,ciatalo(a6)
        move.b	    d1,ciatahi(a6)
        rts

.setTimerB:
        move.b	    d0,ciatblo(a6)
        move.b	    d1,ciatbhi(a6)
        rts

.return:
        movem.l     (sp)+,d0-d2/a1-a2/a6
        ELSE
        movem.l     (sp)+,d0-d1/a1-a2
        ENDC

        rts
        
; main playroutine, call this every interrupt
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
        ENDC

        movem.l     d0-d6/a0-a6,-(sp)
        lea         _custom,a0
        
        ;IFNE        opt_RASTER&~opt_CIA
        move.w      #$fff,$180(a0)
        ;ENDC

        lea         rc_Vars(pc),a2                              ;pointer to vars block  
        lea         rc_Ch0(pc),a1                               ;channel structure pointer into A1
        move.l      rc_SampleOffsetTable(pc),a3
        move.l      rc_SampleStart(pc),a4

        move.w      rc_DmaBits-rc_Vars(a2),d6
        move.w      d6,dmacon(a0)                               ;stop DMA for selected channels

        ; store current raster position for later
        move.l      vposr(a0),d5
        and.l       #$1ffff,d5
        add.l       #$0780,d5

.readNotes:
        moveq       #0,d0                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a2),d0

        moveq       #0,d4                                       ;init DMA bits
.loop:

.getNextNote:
        tst.w       rc_Ch0_ReadLength-rc_Ch0(a1)                ;do we have bytes to read from the buffer still?
        bne         .lookBack                                   ;yes, do it!

        move.l      rc_Ch0_DataPtr-rc_Ch0(a1),a6                ;get current note pointer
.readNote:
        move.l      (a6)+,d1                                    ;read current note into D1
        move.l      a6,rc_Ch0_DataPtr-rc_Ch0(a1)                ;store current channel note pointer
        cmpi.l      #$c0000000,d1                               ;check for control words
        bhi         .controlWord

        ; store note into decompression buffer
.processNote:
        tst.b       rc_Compress-rc_Vars(a2)                     ;but only if the mod is actually compressed
        beq.s       .processVolume

        move.l      rc_Ch0_BufferWritePtr-rc_Ch0(a1),a5         ;read the buffer write pointer
        cmpa.l      rc_Ch0_BufferEnd-rc_Ch0(a1),a5              ;check if we need to wrap around
        bne.s       .noWrap2

        move.l      rc_Ch0_BufferStart-rc_Ch0(a1),a5            ;wrap back around to the beginning of the buffer
.noWrap2:
        move.l      d1,(a5)+                                    ;store the current note into the buffer
        move.l      a5,rc_Ch0_BufferWritePtr-rc_Ch0(a1)         ;store the buffer write pointer

.processVolume
        ;process volume
        move.l      d1,d2                                       ;transfer to d2 to extract volume
        rol.l       #7,d2                                       ;rotate to beginning of register
        and.w       #$7F,d2                                     ;mask unnecessary bits
        move.w      d2,rc_Ch0_VOL-rc_Ch0(a1)     	            ;store the volume

        btst        #23,d1
        beq.s       .noStopDma
        bset        d0,d4                                       ;store that we need to stop DMA in order to trigger a new note
.noStopDma:

        btst        #24,d1                                      ;check format of instruction
        beq.s       .noNewNote

.newNote:
        move.l      d1,d2
        moveq       #10,d3                                      ;d3 is shift amount (greater than 7 can't use immediate addressing)
        ror.l       d3,d2                                       ;d3 is free now
        and.w       #$1FFF,d2
        beq.s       .noPointerChange
        sub.w       #1,d2
        mulu.w      #OFFSET_TABLE_SIZE,d2

        ; copy values from sample offset table to channel vars
        move.l      offs_Sample_Start(a3,d2),a5
        adda.l      a4,a5
        move.l      a5,rc_Ch0_PTR-rc_Ch0(a1)
        move.l      offs_Loop_Start(a3,d2),a5
        adda.l      a4,a5        
        move.l      a5,rc_Ch0_PTR_loop-rc_Ch0(a1)
        
        move.w      offs_Sample_Length(a3,d2),rc_Ch0_LEN-rc_Ch0(a1)
        move.w      offs_Loop_Length(a3,d2),rc_Ch0_LEN_loop-rc_Ch0(a1)

.noPointerChange:
        and.w       #$3ff,d1
        move.w      d1,rc_Ch0_PER-rc_Ch0(a1)
        bra.s       .nextChannel

.noNewNote:
        ;process sample offset
        move.l      d1,d2
        ror.l       #8,d2        
        and.l       #$7fff,d2      
        beq.s       .noPointerChange2

        move.l      (rc_Ch0_PTR_loop-rc_Ch0)(a1),d3             ;get old pointer value
        add.l       d2,d3                                       ;add offset
        move.l      d3,(rc_Ch0_PTR_loop-rc_Ch0)(a1)             ;store it back for later

.noPointerChange2:
        ;process period change
        and.w       #$ff,d1
        ext.w       d1
        beq.s       .noPeriodChange
        move.w      (rc_Ch0_PER-rc_Ch0)(a1),d3                  ;get old period
        add.w       d1,d3                                       ;add offset
        move.w      d3,(rc_Ch0_PER-rc_Ch0)(a1)                  ;store it back for later

.noPeriodChange:
.nextChannel:

        lea         rc_Ch1-rc_Ch0(a1),a1                        ;next channel structure
        dbf         d0,.loop

        move.w      d4,rc_DmaBits-rc_Vars(a2)                   ;store DMA stop flags for next tick
        bra.s       .rc_Music2

.controlWord:
        cmpi.l      #$ffff0000,d1                               ;is it the end of channel data?
        bge.s       .channelEnd
        
        ; process control commands here
        ; compression lookback only at this time
        move.l      d1,d2
        and.w       #$7fff,d1                                   ;read length is in d1
        move.w      d1,(rc_Ch0_ReadLength-rc_Ch0)(a1)           ;store it
        asl.l       #1,d2
        and.l       #$7fff0000,d2
        swap        d2                                          ;read offset is in d2
        add.w       d2,d2                                       ;offset is in longwords
        add.w       d2,d2

        move.l      (rc_Ch0_BufferWritePtr-rc_Ch0)(a1),d1       ;get end of buffer
        sub.l       d2,d1

        cmp.l       (rc_Ch0_BufferStart-rc_Ch0)(a1),d1          ;check for wrap
        blt.s       .wrapBuffer

        move.l      d1,(rc_Ch0_BufferReadPtr-rc_Ch0)(a1)        ;store new read ptr
        bra         .getNextNote

.channelEnd:
        ;go back to beginning of channel data
        move.l      rc_Ch0_DataStart-rc_Ch0(a1),a6
        bra         .readNote

.lookBack:
        move.l      rc_Ch0_BufferReadPtr-rc_Ch0(a1),a5          ;current buffer read ptr
        cmpa.l      rc_Ch0_BufferEnd-rc_Ch0(a1),a5              ;check if we need to wrap around
        bne.s       .noWrap       
       
        move.l      rc_Ch0_BufferStart-rc_Ch0(a1),a5            ;wrap back around to the beginning of the buffer
.noWrap:
        move.l      (a5)+,d1
        subq.w      #1,rc_Ch0_ReadLength-rc_Ch0(a1)
        move.l      a5,rc_Ch0_BufferReadPtr-rc_Ch0(a1)
        bra         .processNote

.wrapBuffer:
        sub.l       (rc_Ch0_BufferStart-rc_Ch0)(a1),d1
        add.l       (rc_Ch0_BufferEnd-rc_Ch0)(a1),d1

        move.l      d1,(rc_Ch0_BufferReadPtr-rc_Ch0)(a1)        ;store new read ptr
        bra         .getNextNote

.rc_Music2:
        IFNE        opt_RASTER&~opt_CIA
        move.w      #$f00,$180(a0)
        ENDC

.rasterWait1:
        move.l      vposr(a0),d4
        and.l       #$1ffff,d4
        cmp.l       d4,d5
        bgt.s       .rasterWait1      

        IFNE        opt_RASTER&~opt_CIA
        move.w      #$fff,$180(a0)
        ENDC
        
        ;poke Paula for all channels 
        lea         rc_Ch0(pc),a1                               ;channel structure pointer into A1
        moveq       #0,d0                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a2),d0
        moveq       #0,d1
        
.pokePaula:
        move.b      rc_AudioOffsets-rc_Vars(a2,d0),d1           ;get audio register offset for current channel (we go last to first)

        btst        d0,d6				
        beq.s       .noPokePtrs
        move.l      rc_Ch0_PTR-rc_Ch0(a1),ac_ptr(a0,d1)         ;poke ac_ptr
        move.w      rc_Ch0_LEN-rc_Ch0(a1),ac_len(a0,d1)         ;poke ac_len
.noPokePtrs:
        move.w      rc_Ch0_PER-rc_Ch0(a1),ac_per(a0,d1)         ;poke ac_per
        move.w      rc_Ch0_VOL-rc_Ch0(a1),ac_vol(a0,d1)         ;poke ac_vol

        lea         rc_Ch1-rc_Ch0(a1),a1                        ;next channel structure

        dbf         d0,.pokePaula

        ;re-enable audio DMA
        or.w        #$8000,d6
        move.w      d6,dmacon(a0)

        IFNE        opt_RASTER&~opt_CIA
        move.w      #$0f0,$180(a0)
        ENDC

        ;wait 1.1 rasterlines here
        move.l      vposr(a0),d2
        and.l       #$1ffff,d2
        add.l       #$0110,d2
.rasterWait2:
        move.l      vposr(a0),d3
        and.l       #$1ffff,d3
        cmp.l       d3,d2
        bgt.s       .rasterWait2  

        IFNE        opt_RASTER&~opt_CIA
        move.w      #$fff,$180(a0)
        ENDC

        ;re-poke the sample pointers for looped sounds
        lea         rc_Ch0(pc),a1                               ;channel structure pointer into A1   
        moveq       #0,d0                                       ;loop for all channels
        move.b      rc_NumChannels-rc_Vars(a2),d0

.rePokePaula:
        move.b      rc_AudioOffsets-rc_Vars(a2,d0),d1           ;a2 has ptr to audiooffsets,(we go last to first)

        move.l      rc_Ch0_PTR_loop-rc_Ch0(a1),ac_ptr(a0,d1)    ;always repoke lthe pointers so we can do wavetable stuff
                                                                ;replay code makes sure this doesn't change if not needes, so we can safely do this
 
        move.w      rc_Ch0_LEN_loop-rc_Ch0(a1),ac_len(a0,d1)    ;poke ac_len
        adda.l      #rc_Ch1-rc_Ch0,a1                           ;next channel structure

        dbf         d0,.rePokePaula 
        
        ;IFNE        opt_RASTER&~opt_CIA
        move.w      #$05a,$180(a0)
        ;ENDC

        movem.l     (sp)+,d0-d6/a0-a6
        rts                                                     ;all done!

rc_StopMusic:
        IFNE        opt_CIA
         
        movem.l     d0/a1/a4-a6,-(sp)
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

        move.l      a0,-(sp)
        
        ENDC

        lea         _custom,a0
        move.w	    #$000f,dmacon(a0)

        IFNE        opt_CIA
        movem.l     (sp)+,d0/a1/a4-a6
        ELSE
        movem.l     (sp)+,a0
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

rc_DmaBits:
        dc.w        0

rc_SampleOffsetTable:
        dc.l        0        

rc_SampleStart:
        dc.l        0

rc_AudioOffsets:
        dc.b        aud0,aud1,aud2,aud3

;CIA player variables - only use if opt_CIA is set to non-zero
        IFNE        opt_CIA

rc_CIAName:	
        dc.b        "ciab.resource",0
rc_CIAResource:	
        dc.l        0
rc_CIATimer:
        dc.w        0
rc_TimerValue:	
        dc.w        $376c                                       ;125 bpm

rc_CIAServer:
        dc.l        0,0
        dc.b        2,127                                       ;type, priority
rc_IntnamePtr:        
        dc.l        0
        dc.l        0
rc_ReplayPtr:
        dc.l        0                                           ;poke playroutine 

rc_IntName:
        dc.b        "RetroReplay Interrupt",0

        ENDC
        EVEN

rc_Ch0:
rc_Ch0_DataStart:
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