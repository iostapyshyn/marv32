extern void main(void);

__attribute__ ((section(".bss")))
static char _stack[4096];

__attribute__ ((section(".start")))
__attribute__ ((naked))
__attribute__ ((noreturn))
void _start(void)
{
    asm volatile ("mv sp, %0" : : "r" (_stack + sizeof(_stack) - 1));

    main();

    asm volatile ("ebreak");
    while (1);
}
