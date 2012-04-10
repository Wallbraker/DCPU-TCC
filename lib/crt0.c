
/*
    Init file for the DCPU16 target.

    This file is included first for any exe
    and is placed at the start of the binary.
*/

#ifndef __DCPU16__
#error "Using crt0.c on wrong target"
#endif


/*
    main to be run, should be implemented by the program.
*/

int main();


/*
    The following code is all used to exit to the VM
    and never return.

    We first generate the syscall opcode and store
    that in a value. We then get the address to that
    and jump to it in order run that syscall.
*/

#define SYS(__sysn) ((((__sysn) + 0x20) << 10) | (2 << 4))

static short dexit_body = SYS(0);

#define dexit ((void (*)(void))&dexit_body)


/*
    _start gets placed at address 0x0 and is the first that is executed.

    Its job is to make sure the stack and other things are setup
    correctly for the main function to start running.

    It also signals the VM that it has stop executing, the current
    way it does this is a bit non-standard, via a syscall.
*/

void _start()
{
    main();
    dexit();
}

/*

void memcpy(void *dst, const void *src, unsigned length)

;
; Swap SP and B around, allowing to read fast with [SP++]
; and skipping a ADD B, 1, saving use cycles instructions.
;
	SET O, SP                            ; 0x0000 | 6dd1
	SET SP, B                            ; 0x0001 | 05b1
	SET B, O                             ; 0x0002 | 7411
;
; Calculate last address, by comparing addresses
; instead we negate the need for a counter.
;
	ADD C, SP                            ; 0x0003 | 6c22
	IFE SP, C                            ; 0x0004 | 09bc
	ADD PC, 0x03                         ; 0x0005 | 8dc2
	SET [A], [SP++]                      ; 0x0006 | 6081
	ADD A, 0x01                          ; 0x0007 | 8402
	SUB PC, 0x05                         ; 0x0008 | 95c3
;
; Restore SP and return.
;
	SET SP, B                            ; 0x0009 | 05b1
	SET PC, [SP++]                       ; 0x000a | 61c1
*/
unsigned short memcpy[11] = {
	0x6dd1, 0x05b1,
	0x7411, 0x6c22,
	0x09bc, 0x8dc2,
	0x6081, 0x8402,
	0x95c3, 0x05b1,
	0x61c1,
};
