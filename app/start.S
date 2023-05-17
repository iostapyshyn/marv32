	.section .start, "awx"

	.global _start
_start:
	la	sp, _stack
	jal	ra, main

	ebreak
_die:	j _die

	/* Allocate stack, linker must include bss in the binary */
	.section .bss
	.space 4096, 0
_stack:
