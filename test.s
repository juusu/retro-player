	SECTION code,CODE_P

	move.l 	d0,-(a7)

	move 	$dff01c,d1
	move	#$7fff,$dff09a		;disable interrupts

	lea		mod_Module,a0
	jsr		rc_Init

	;call rc_Music every frame
waitras1:
	move.l   $dff004,d0
    and.l    #$1ff00,d0
    cmp.l    #88<<8,d0
	bne 	 waitras1
	move.w 	#$fff,$dff180

	jsr		rc_Music	

waitras2:
	move.l   $dff004,d0
    and.l    #$1ff00,d0
    cmp.l    #88<<8,d0
	beq		waitras2

	move.w	#$05a,$dff180

	btst	#6,$bfe001
	bne		waitras1

	jsr		rc_StopMusic

	or		#$c000,d1
	move    d1,$dff09a		; enable interrupts
	move.l  (a7)+,d0	
	rts

	INCLUDE "replay.s"

	SECTION data,DATA_C

mod_Module:
	INCBIN "converted.nmod"