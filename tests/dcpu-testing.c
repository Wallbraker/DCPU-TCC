int test(int a, int b);

typedef (*funcCall)();

short array = {0x61c1};

int main()
{
	((funcCall)array)();

	return test(3, 5);
}

int test(int a, int b)
{
	return ((a + b + 1) * a / b) + (a * 6);
}
