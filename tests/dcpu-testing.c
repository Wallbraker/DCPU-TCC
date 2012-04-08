int test(int a, int b, int c, int d, int e);

typedef (*funcCall)();

short array = {0x61c1};

int main()
{
	((funcCall)array)();

	int some_var = 3;

	int ret = test(1, 2, 3, 4, 5);

	return some_var + ret;
}

int foo(int a)
{
	return 9 + a;
}

int test(int a, int b, int c, int d, int e)
{
	a = foo(a);
	return ((a  + b + c) * d / a) + (d * e);
}
