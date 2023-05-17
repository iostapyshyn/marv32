#include "util.h"

void outsn(char *s, unsigned n)
{
    for (int i = 0; i < n; i++)
	outc(s[i]);
}

void outsz(char *s)
{
    while (*s) {
	outc(*(s++));
    }
}

void *memset(void *str, int c, unsigned n)
{
    volatile char *s = str;
    for (int i = 0; i < n; i++)
	s[i] = c;

    return str;
}

int itoa(int value, char *sp, int radix)
{
    char tmp[16];
    char *tp = tmp;
    int i;
    unsigned v;

    int sign = (radix == 10 && value < 0);
    if (sign) {
	v = -value;
    } else {
	v = (unsigned)value;
    }

    while (v || tp == tmp) {
	i = v % radix;
	v /= radix;
	if (i < 10)
	    *tp++ = i+'0';
	else
	    *tp++ = i + 'a' - 10;
    }

    int len = tp - tmp;

    if (sign) {
	*sp++ = '-';
	len++;
    }

    while (tp > tmp)
	*sp++ = *--tp;

    return len;
}
