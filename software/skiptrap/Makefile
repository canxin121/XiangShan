CROSS ?= riscv64-unknown-elf-
CC    := $(CROSS)gcc
OBJCOPY := $(CROSS)objcopy

all: skiptrap.bin

skiptrap.elf: skiptrap.S link.ld
	$(CC) -march=rv64gc -mabi=lp64 -nostdlib -nostartfiles -T link.ld \
	      -Wl,--build-id=none -o $@ skiptrap.S

skiptrap.bin: skiptrap.elf
	$(OBJCOPY) -O binary $< $@

clean:
	rm -f skiptrap.elf skiptrap.bin

.PHONY: all clean
