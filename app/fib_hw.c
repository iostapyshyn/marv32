#include "util.h"

void outi(int x)
{
    char s[10];
    itoa(x, s, 10);
    outsz(s);
}

int factorial(int n)
{
    return n < 2 ? 1 : n * factorial(n - 1);
}

int fibbonacci(int n)
{
    return n < 2 ? n : fibbonacci(n-1) + fibbonacci(n-2);
}

int main()
{
    while (1) {
        char input = gpio_in();

        gpio_out((char) fibbonacci(input));
    }

    return 0;
}
