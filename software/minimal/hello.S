    .section .text
    .globl _start
_start:
    la   sp, _stack_top
    la   t2, data_buf
    li   t1, 'Z'
    sb   t1, 0(t2)
    li   t1, 'Y'
    addi t3, t2, 1
    sb   t1, 0(t3)
    la   a0, message

1:  lbu  t1, 0(a0)
    beqz t1, finish

    li   t2, 0x40600004
    sb   t1, 0(t2)

    addi a0, a0, 1
    j    1b

finish:
    li   a7, 93
    li   a0, 0
    ecall

    .section .rodata
message:
    .asciz "Hello from XiangShan!\n"

    .section .bss
    .align 12
    .space 4096
data_buf:
    .space 16
