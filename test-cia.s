opt_CIA = 1
	SECTION code,CODE_P

	movem.l 	d0-d7/a0-a6,-(a7)

	lea		mod_Module,a0
	jsr		rc_Init

	bset.b  #1,$bfe001          ;turn off led filter

	jsr		rc_Music	

.waitMouse
	btst	#6,$bfe001
	bne		.waitMouse

	jsr		rc_StopMusic

	movem.l  (a7)+,d0-d7/a0-a6	
	rts

	INCLUDE "replay.s"

	SECTION data,DATA_C

mod_Module:
	INCBIN "transform.rcm"