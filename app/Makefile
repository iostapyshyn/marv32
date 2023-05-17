NAME = fib

COMMON_OBJS = util.o start.o

CFLAGS = -march=rv32im -mabi=ilp32 -O3 -g
LDFLAGS = -nostdlib -T bare.x -Wl,--build-id=none
OBJCOPY_FLAGS = --set-section-flags .bss=alloc,load,contents --strip-debug
# include .bss explicitly

TARGET = riscv64-linux-gnu-
CC = gcc

all: $(NAME).bin

.PRECIOUS: %.o
%.o: %.S
	$(TARGET)$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(TARGET)$(CC) $(CFLAGS) -c $< -o $@

%.elf: %.o $(COMMON_OBJS)
	$(TARGET)$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

%.bin: %.elf
	riscv64-linux-gnu-objcopy -O binary $(OBJCOPY_FLAGS) -S $< $@

.PHONY: disas
disas: $(NAME).elf
	riscv64-linux-gnu-objdump -S \
		--visualize-jumps=color \
		--disassembler-color=color $<

.PHONY: clean
clean:
	rm -rf *.o *.elf *.bin
