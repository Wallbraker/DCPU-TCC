#define SYS(__sysn) ((((__sysn) + 0x20) << 10) | (2 << 4))

unsigned short sys1_value[] = {0x01a1, SYS(2), 0x61c1};
#define dputs(__str) ((void (*)(const char*))sys1_value)(__str)

#define vram ((char*)0x8000)

char hello[] = "Hello world!\n";

void main()
{
	dputs(hello);

	vram[0] = 'H';
	vram[1] = 'e';
	vram[2] = 'l';
	vram[3] = 'l';
	vram[4] = 'o';


	hello[0] = 'B';
	hello[6] = 'g';

	dputs(hello);
}
