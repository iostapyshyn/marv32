#ifndef UTIL_H
#define UTIL_H

static volatile char *GPIO_ADDR = (char *) 0xDEAD0000;

static inline void outc(char c)
{
    static volatile char *out = (char *) 0xDEADBEEF;
    *out = c;
}

static inline char gpio_in(void)
{
    return *GPIO_ADDR;
}

static inline void gpio_out(char c)
{
    *GPIO_ADDR = c;
}


void *memset(void *str, int c, unsigned n);
int itoa(int value, char *sp, int radix);

void outsz(char *s);
void outsn(char *s, unsigned n);

#endif
