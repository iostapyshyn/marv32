ENTRY(_start)
SECTIONS
{
        . = 0x0;
        .start : { *(.start) }
        .text : { *(.text) }
        .data : { *(.data) }
        .bss : { *(.bss) }
}
