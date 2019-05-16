        INCLUDE    "Includes/custom.i"

_custom = $dff000

;.mod instrument structure offsets
offs_Sample_Start = 0
offs_Loop_Start = 4
offs_Sample_Length = 8
offs_Loop_Length = 10

OFFSET_TABLE_SIZE = 12

; put pointer to mod data in A0 and call init
rc_Init:
        lea        rc_Ch0,a1                                   ;channel var pointer
        moveq      #0,d0                                       ;number of channels
.loopStart:
        move.l     a0,rc_Ch0_DataStart-rc_Ch0(a1)              ;store beginning of ch0 pattern data
        move.l     a0,rc_Ch0_DataPtr-rc_Ch0(a1)                ;also init the current note pointer to the same place

.innerLoop:
        cmp.l      #$ffffffff,(a0)+                            ;check if end of current channel data
        bne        .innerLoop                                  ;no? read on ...

        cmp.l      #$ffffffff,(a0)+                            ;check if this was the last channel ?
        beq        .noteLoopEnd                                ;yeah, exit loop!

        adda.l     #rc_Ch1-rc_Ch0,a1                           ;no? next channel structure
        addq.b     #1,d0                                       ;moar channels
        subq.l     #4,a0                                       ;rewind pointer one place b/c it wasn't a marker
      
        bra        .loopStart            

.noteLoopEnd:
        move.b     d0,rc_NumChannels                           ;store for later
        move.l     a0,rc_SampleOffsetTable                     ;store pointer to sample offset table

.loop
        cmp.l      #$ffffffff,(a0)+
        beq        .sampleLoopEnd
        add.l      #OFFSET_TABLE_SIZE-4,a0
        bra        .loop
.sampleLoopEnd:
        move.l     a0,rc_SampleStart                           ;store sample pointer

.endInit:
        rts
        
; main playroutine, call this every interrupt
rc_Music:
        lea        _custom,a0
        lea        rc_Ch0,a1                                   ;channel structure pointer into A1
        lea        rc_AudioOffsets,a2                          ;audio offset for the starting channel        
        move.l     rc_SampleOffsetTable,a3
        move.l     rc_SampleStart,a4

	; stop DMA if needed for each channel
        moveq      #0,d0                                       ;loop for all channels
        move.b     rc_NumChannels,d0
        moveq      #0,d1                                       ;dma bits
.nextDMABit:
        tst.b      (rc_Ch0_StopDMA-rc_Ch0)(a1)                 ;should we stop DMA for this channel?					
        beq        .no
        bset       d0,d1
.no
        adda.l     #rc_Ch1-rc_Ch0,a1
        dbf        d0,.nextDMABit

        move.w     d1,dmacon(a0)                               ;stop DMA for selected channels

        move.w     #$f00,$dff180
   	; TODO: wait 7 rasterlines here
        move.l     #300,d2
.waitDMA:
        sub.w      #1,d2
        bne        .waitDMA


   	; poke Paula for all channels 
        lea        rc_Ch0,a1                                   ;channel structure pointer into A1
        moveq      #0,d0                                       ;loop for all channels
        move.b     rc_NumChannels,d0

.pokePaula:
        move.b     (a2,d0),d3                                  ;(we go last to first)

        tst.b      (rc_Ch0_StopDMA-rc_Ch0)(a1)                 ;if we needed to stop DMA then poke the ptrs too					
        beq        .noPokePtrs
        move.l     rc_Ch0_PTR-rc_Ch0(a1),ac_ptr(a0,d3)         ;poke ac_ptr
        move.w     rc_Ch0_LEN-rc_Ch0(a1),ac_len(a0,d3)         ;poke ac_len
        clr.b      rc_Ch0_StopDMA-rc_Ch0(a1)                   ;make sure that we don't reset DMA next time
.noPokePtrs
        move.w     rc_Ch0_PER-rc_Ch0(a1),ac_per(a0,d3)         ;poke ac_per
        move.w     rc_Ch0_VOL-rc_Ch0(a1),ac_vol(a0,d3)         ;poke ac_vol

        adda.l     #rc_Ch1-rc_Ch0,a1                           ;next channel structure

        dbf        d0,.pokePaula

	; re-enable audio DMA
        or.w       #$8000,d1
        move.w     d1,dmacon(a0)

          	; TODO: wait 7 rasterlines here
        move.l     #300,d2
.waitDMA2:
        sub.w      #1,d2
        bne        .waitDMA2

        move.w     #$fff,$dff180

	; re-poke the sample pointers for looped sounds
        lea        rc_Ch0,a1                                   ;channel structure pointer into A1   
        moveq      #0,d0                                       ;loop for all channels
        move.b     rc_NumChannels,d0

.rePokePaula:
        move.b     (a2,d0),d1                                  ;a2 has ptr to audiooffsets,(we go last to first)

        move.l     rc_Ch0_PTR_loop-rc_Ch0(a1),ac_ptr(a0,d1)    ;always repoke lthe pointers so we can do wavetable stuff
                                                               ;replay code makes sure this doesn't change if not needes, so we can safely do this
 
        move.w     rc_Ch0_LEN_loop-rc_Ch0(a1),ac_len(a0,d1)    ;poke ac_len
        adda.l     #rc_Ch1-rc_Ch0,a1                           ;next channel structure

        dbf        d0,.rePokePaula 

.readNotes:
        lea        rc_Ch0,a1                            t       ;channel structure pointer into A1
        moveq      #0,d0                                       ;loop for all channels

        move.b     rc_NumChannels,d0

.loop:
        move.l     rc_Ch0_DataPtr-rc_Ch0(a1),a0                 ;get current note pointer
.getNextNote:
        move.l     (a0)+,d1                                     ;read current note into D1
        cmpi.l     #$c0000000,d1                                ;check for control words
        bhi        .controlWord

        ;process volume
        move.l     d1,d2                                        ;transfer to d2 to extract volume
        rol.l      #7,d2                                        ;rotate to beginning of register
        and.w      #$7F,d2                                      ;mask unnecessary bits
        move.w     d2,rc_Ch0_VOL-rc_Ch0(a1)     	        ;store the volume

        btst       #24,d1                                       ;check format of instruction
        beq        .noNewNote

.newNote
        btst       #23,d1
        beq        .noStopDma
        move.b     #1,rc_Ch0_StopDMA-rc_Ch0(a1)                 ;store that we need to stop DMA in order to trigger a new note
.noStopDma:
        move.l     d1,d2
        moveq      #10,d3                                       ;d3 is shift amount (greater than 7 can't use immediate addressing)
        ror.l      d3,d2                                        ;d3 is free now
        and.w      #$1FFF,d2
        beq        .noPointerChange
        sub.w      #1,d2
        mulu.w     #OFFSET_TABLE_SIZE,d2

        ; copy values from sample offset table to channel vars
        move.l     offs_Sample_Start(a3,d2),a5
        adda.l     a4,a5
        move.l     a5,rc_Ch0_PTR-rc_Ch0(a1)
        move.l     offs_Loop_Start(a3,d2),a5
        adda.l     a4,a5        
        move.l     a5,rc_Ch0_PTR_loop-rc_Ch0(a1)
        
        move.w     offs_Sample_Length(a3,d2),rc_Ch0_LEN-rc_Ch0(a1)
        move.w     offs_Loop_Length(a3,d2),rc_Ch0_LEN_loop-rc_Ch0(a1)

.noPointerChange:
        and.w      #$3ff,d1
        move.w     d1,rc_Ch0_PER-rc_Ch0(a1)
        bra        .nextChannel

.noNewNote:

        ;process sample offset
        move.l     d1,d2
        ror.l      #8,d2        
        and.l      #$ffff,d2      
        beq        .noPointerChange2

        move.l     (rc_Ch0_PTR_loop-rc_Ch0)(a1),d3              ;get old pointer value
        add.l      d2,d3                                        ;add offset
        move.l     d3,(rc_Ch0_PTR_loop-rc_Ch0)(a1)              ;store it back for later

.noPointerChange2:
        ;process period change
        and.w      #$ff,d1
        ext.w      d1
        beq        .noPeriodChange
        move.w     (rc_Ch0_PER-rc_Ch0)(a1),d3                   ;get old period
        add.w      d1,d3                                        ;add offset
        move.w     d3,(rc_Ch0_PER-rc_Ch0)(a1)                   ;store it back for later

.noPeriodChange
.nextChannel:
        move.l     a0,rc_Ch0_DataPtr-rc_Ch0(a1)                 ;store current channel note pointer
        adda.l     #rc_Ch1-rc_Ch0,a1                            ;next channel structure
        dbf        d0,.loop
        
        rts                                                     ;all done!

.controlWord:
        cmpi.l  #$ffffffff,d1                                   ;is it the end of channel data?
        beq     .channelEnd
        ; process control commands here
        bra     .getNextNote
.channelEnd:
; go back to beginning of channel data
        move.l  rc_Ch0_DataStart-rc_Ch0(a1),a0
        bra     .getNextNote

rc_StopMusic:
	lea		_custom,a0
	move.w	#$000f,dmacon(a0)
	rts
        
;
; variables
;
rc_NumChannels:
        dc.b       0

        EVEN

rc_SampleOffsetTable:
        dc.l       0        

rc_SampleStart:
        dc.l       0

rc_AudioOffsets:
        dc.b       aud0,aud1,aud2,aud3
        ;dc.b       aud3,aud2,aud1,aud0

rc_Ch0:
rc_Ch0_DataStart:
        dc.l       0
rc_Ch0_DataPtr:
        dc.l       0
rc_Ch0_StopDMA:
        dc.b       0
        EVEN

; paula registers
rc_Ch0_PTR:
        dc.l       0
rc_Ch0_PTR_loop:
        dc.l       0	
rc_Ch0_LEN:
        dc.w       0
rc_Ch0_LEN_loop:
        dc.w       0	
rc_Ch0_PER:
        dc.w       0
rc_Ch0_VOL:
        dc.w       0        

rc_Ch1:
rc_Ch1_DataStart:
        dc.l       0
rc_Ch1_DataPtr:
        dc.l       0       
rc_Ch1_StopDMA:
        dc.b       0  
        EVEN

; paula registers
rc_Ch1_PTR:
        dc.l       0
rc_Ch1_PTR_loop:
        dc.l       0	
rc_Ch1_LEN:
        dc.w       0
rc_Ch1_LEN_loop:
        dc.w       0
rc_Ch1_PER:
        dc.w       0
rc_Ch1_VOL:
        dc.w       0

rc_Ch2:
rc_Ch2_DataStart:
        dc.l       0
rc_Ch2_DataPtr:
        dc.l       0
rc_Ch2_StopDMA:
        dc.b       0  
        EVEN

; paula registers
rc_Ch2_PTR:
        dc.l       0
rc_Ch2_PTR_loop:
        dc.l       0	
rc_Ch2_LEN:
        dc.w       0
rc_Ch2_LEN_loop:
        dc.w       0
rc_Ch2_PER:
        dc.w       0
rc_Ch2_VOL:
        dc.w       0

rc_Ch3:
rc_Ch3_DataStart:
        dc.l       0
rc_Ch3_DataPtr:
        dc.w       0
rc_Ch3_StopDMA:
        dc.b       0  
        EVEN

; paula registers
rc_Ch3_PTR:
        dc.l       0
rc_Ch3_PTR_loop:
        dc.l       0	
rc_Ch3_LEN:
        dc.w       0
rc_Ch3_LEN_loop:
        dc.w       0
rc_Ch3_PER:
        dc.w       0
rc_Ch3_VOL:
        dc.w       0

        EVEN

