OUTPUT_ARCH(riscv)
ENTRY(_start)

MEMORY {
  ram  (rwx) : ORIGIN = 0x80000000, LENGTH = 128M
}

SECTIONS {
  . = ORIGIN(ram);
  .text : {
    *(.text*)
    *(.rodata*)
  } > ram

  .data : {
    *(.data*)
    *(.sdata*)
  } > ram

  .bss (NOLOAD) : {
    __bss_start = .;
    *(.bss*)
    *(.sbss*)
    *(COMMON)
    __bss_end = .;
  } > ram

  PROVIDE(_stack_top = ORIGIN(ram) + LENGTH(ram));
}
