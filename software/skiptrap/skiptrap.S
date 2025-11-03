    .section .text
    .globl _start
_start:
    la      t0, trap_handler
    csrw    mtvec, t0

    csrr    t0, mstatus
    li      t1, 0x00003000
    or      t0, t0, t1
    csrw    mstatus, t0
    csrw    fcsr, x0

    j       user_code

user_code:
    c.unimp                     # 故意触发非法指令异常
    addi    s0, zero, 42         # 若成功跳过异常，将执行到这里
    li      t0, 1
    la      t1, tohost
    sd      t0, 0(t1)
1:
    j       1b

    .align  2
trap_handler:
    csrr    t0, mepc             # 触发异常的指令地址
    csrr    t1, mcause           # 异常原因
    csrr    t4, mtval

    slli    t5, t1, 1
    srli    t1, t5, 1            # 去掉 msb 的 interrupt bit

    li      t2, 2                # 默认假定是 16bit 指令
    li      t3, 2                # mcause=2 -> illegal instruction
    beq     t1, t3, use_mtval
    li      t3, 1                # instruction access fault
    beq     t1, t3, fetch_inst
    li      t3, 12               # instruction page fault
    beq     t1, t3, fetch_inst

fetch_inst:
    lhu     t4, 0(t0)
    j       decode_length

use_mtval:
    j       decode_length

decode_length:
    andi    t4, t4, 3
    li      t3, 3
    bne     t4, t3, compressed_len
    li      t2, 4
    j       update_mepc

compressed_len:
    li      t2, 2

update_mepc:
    add     t0, t0, t2           # 跳过异常指令
    csrw    mepc, t0
    csrw    mcause, x0
    csrw    mtval, x0
    mret

    .section .tohost,"aw",@progbits
    .align  3
    .globl tohost
    .globl fromhost
    tohost:
        .dword 0
    fromhost:
        .dword 0
