.intel_syntax noprefix
# uint32_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc)
# arguments:
#    rcx: arrays
#    rdx: jumptable
#    r8: registers
#    r9d: initial_pc
# registers:
#    rbx (callee-save): arrays
#    r9 (caller-save): jumptable
#    esi (callee-save): VM's R0
#    edi (callee-save): VM's R1
#    r10d (caller-save): VM's R2
#    r11d (caller-save): VM's R3
#    r12d (callee-save): VM's R4
#    r13d (callee-save): VM's R5
#    r14d (callee-save): VM's R6
#    r15d (callee-save): VM's R7
# stack:
#    prev rbp
#    prev r15
#    prev r14
#    prev r13
#    prev r12
#    prev rsi
#    prev rdi
#    prev rbx
#    rsp+32: registers
#    rsp+24: jumptable (8 bytes)
#    rsp+20: R2
#    rsp+16: R3
#    rsp+0: struct alloc_result
prologue_win64:
    push rbp
    mov rbp, rsp
    push r15
    push r14
    push r13
    push r12
    push rsi
    push rdi
    push rbx
    sub rsp, 48
    mov rbx, rcx  # set arrays
    mov esi, dword ptr [r8]
    mov edi, dword ptr [r8 + 4]
    mov r10d, dword ptr [r8 + 8]
    mov r11d, dword ptr [r8 + 12]
    mov r12d, dword ptr [r8 + 16]
    mov r13d, dword ptr [r8 + 20]
    mov r14d, dword ptr [r8 + 24]
    mov r15d, dword ptr [r8 + 28]
    mov qword ptr [rsp + 32], r8       # registers
    mov qword ptr [rsp + 24], rdx      # jumptable (stack)
    mov rax, qword ptr [rdx + 8 * r9]  # jumptable[initial_pc]
    mov r9, rdx                        # jumptable (register)
    jmp rax

epilogue_win64:
    mov r8, qword ptr [rsp + 32]  # registers
    mov dword ptr [r8], esi
    mov dword ptr [r8 + 4], edi
    mov dword ptr [r8 + 8], r10d
    mov dword ptr [r8 + 12], r11d
    mov dword ptr [r8 + 16], r12d
    mov dword ptr [r8 + 20], r13d
    mov dword ptr [r8 + 24], r14d
    mov dword ptr [r8 + 28], r15d
    add rsp, 48
    pop rbx
    pop rdi
    pop rsi
    pop r12
    pop r13
    pop r14
    pop r15
    pop rbp
    ret

thunk1_win64:
    mov dword ptr [rsp + 28], r10d
    mov dword ptr [rsp + 24], r11d
    sub rsp, 40
    # mov r10, <absolute address>
thunk2_win64:
    call r10
    add rsp, 40
    mov r11d, dword ptr [rsp + 24]
    mov r10d, dword ptr [rsp + 28]
    mov r9, qword ptr [rsp + 32]
    ret
