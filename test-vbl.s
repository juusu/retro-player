opt_CIA = 0
	SECTION code,CODE_P

	movem.l 	d0/a0,-(a7)

	move 	$dff01c,d1
	move	#$7fff,$dff09a		;disable interrupts

	lea		mod_Module,a0
	jsr		rc_Init

	bset.b  #1,$bfe001          ;turn off led filter

	;call rc_Music every frame
waitras1:
	move.l   $dff004,d0
    and.l    #$1ff00,d0
    cmp.l    #55<<8,d0
	bne 	 waitras1
	move.w 	#$f44,$dff180

	jsr		rc_Music	

waitras2:
	move.l   $dff004,d0
    and.l    #$1ff00,d0
    cmp.l    #55<<8,d0
	beq		waitras2

	move.w	#$05a,$dff180

	btst	#6,$bfe001
	bne		waitras1

	jsr		rc_StopMusic

	or		#$c000,d1
	move    d1,$dff09a		; enable interrupts
	movem.l  (a7)+,d0/a0	
	rts

	INCLUDE "replay.s"

	SECTION data,DATA_C

mod_Module:
	INCBIN "transform.rcm"