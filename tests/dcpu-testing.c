int test(int a, int b, int c, int d, int e);

typedef (*funcCall)();

short array = {0x61c1};

int main()
{
	((funcCall)array)();

	return test(1, 2, 3, 4, 5);
}

int test(int a, int b, int c, int d, int e)
{
	return (e + (a + b + c) * a / d) + (a * 6);
}
