CROSS ?= riscv64-unknown-elf-
AS    := $(CROSS)gcc
OBJCOPY := $(CROSS)objcopy

all: hello.bin

hello.elf: hello.S link.ld
	$(AS) -march=rv64gc -mabi=lp64 -nostdlib -nostartfiles -T link.ld \
	      -Wl,--build-id=none -o $@ hello.S

hello.bin: hello.elf
	$(OBJCOPY) -O binary $< $@

clean:
	rm -f hello.elf hello.bin

.PHONY: all clean
