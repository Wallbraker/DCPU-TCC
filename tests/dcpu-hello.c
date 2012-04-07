#define SYS(__sysn) ((((__sysn) + 0x20) << 10) | (2 << 4))

unsigned short sys1_value[] = {0x85b2, SYS(2), 0x89b3, 0x61c1};
#define dputs(__str) ((void (*)(const char*))sys1_value)(__str)

char hello[] = "Hello world!\n";

void main()
{
	dputs(hello);
}
