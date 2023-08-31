NAME = fib_hw

COMMON_OBJS = util.o start.o

XCC = clang -target riscv32 -mcpu=generic-rv32
XLD = ld.lld

XOBJCOPY = llvm-objcopy
XOBJDUMP = llvm-objdump

XCFLAGS = -march=rv32im -mabi=ilp32 -mno-relax -O2 -g -ffreestanding
XLDFLAGS = -nostdlib -T bare.x --build-id=none
XOBJCOPY_FLAGS = --set-section-flags .bss=alloc,load
# include .bss explicitly

all: $(NAME).bin

.PRECIOUS: %.o
%.o: %.S
	$(XCC) $(XCFLAGS) -c $< -o $@

%.o: %.c
	$(XCC) $(XCFLAGS) -c $< -o $@

%.elf: %.o $(COMMON_OBJS)
	$(XLD) $(XLDFLAGS) $^ -o $@

%.bin: %.elf
	$(XOBJCOPY) -O binary $(XOBJCOPY_FLAGS) -S $< $@

.PHONY: disas
disas: $(NAME).elf
	$(XOBJDUMP) -S $<

.PHONY: clean
clean:
	rm -rf *.o *.elf *.bin
