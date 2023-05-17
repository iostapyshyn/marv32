#ifndef UTIL_H
#define UTIL_H

static inline void outc(char c)
{
    static volatile char *out = (char *) 0xDEADBEEF;
    *out = c;
}

void *memset(void *str, int c, unsigned n);
int itoa(int value, char *sp, int radix);

void outsz(char *s);
void outsn(char *s, unsigned n);

#endif
