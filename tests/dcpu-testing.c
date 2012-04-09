#define SYS(__sysn) ((((__sysn) + 0x20) << 10) | (2 << 4))

unsigned short sys1_value[] = {0x01a1, SYS(2), 0x61c1};
#define dputs(__str) ((void (*)(const char*))sys1_value)(__str)

struct program {
	int state;
	int foo;
} program;

int test(struct program *p);
void printResult(int a);


int main()
{
	program.state = 3;
	program.foo = 4;

	int ret = test(&program);

	printResult(ret);

	ret = foo(6);

	printResult(ret);

	return foo(ret);
}

int foo(int a)
{
	return a - 4;
}

int test(struct program *p)
{
	return p->state + p->foo;
}

void printResult(int a)
{
	static char string[] = "result is 0\n\0";

	string[10] = '0' + a;
	dputs(string);
}
