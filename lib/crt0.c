
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
