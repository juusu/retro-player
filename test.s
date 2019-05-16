	SECTION code,CODE_P

	lea		mod_Module,a0
	jsr		rc_Init

	move.l 	d0,-(a7)
	;call rc_Music every frame
waitras1:
	move.l   $dff004,d0
    and.l    #$1ff00,d0
    cmp.l    #88<<8,d0
	bne 	waitras1
	move.w	#$fff,$dff180

	jsr		rc_Music	

waitras2:
	move.l   $dff004,d0
    and.l    #$1ff00,d0
    cmp.l    #88<<8,d0
	beq		waitras2

	move.w	#$aaa,$dff180

	btst	#6,$bfe001
	bne		waitras1

	jsr		rc_StopMusic
	move.l  (a7)+,d0	
	rts

	INCLUDE "replay.s"

	SECTION data,DATA_C

mod_Module:
	INCBIN "converted.nmod"