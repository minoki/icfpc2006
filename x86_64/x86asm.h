#ifndef X86ASM_H
#define X86ASM_H

#include <assert.h>
#include <stdint.h>
#include <string.h>

enum reg32 {
    eax,
    ecx,
    edx,
    ebx,
    esp,
    ebp,
    esi,
    edi,
    r8d,
    r9d,
    r10d,
    r11d,
    r12d,
    r13d,
    r14d,
    r15d
};

enum reg64 {
    rax,
    rcx,
    rdx,
    rbx,
    rsp,
    rbp,
    rsi,
    rdi,
    r8,
    r9,
    r10,
    r11,
    r12,
    r13,
    r14,
    r15
};

// r: extension of the ModR/M reg field,
// x: extension of the SIB index field
// b: extension of the ModR/M r/m field, SIB base field, or Opcode reg field
#define REX(r,x,b) (0x40 | (((r) & 1) << 2) | (((x) & 1) << 1) | ((b) & 1))

// r: extension of the ModR/M reg field,
// x: extension of the SIB index field
// b: extension of the ModR/M r/m field, SIB base field, or Opcode reg field
#define REX_W(r,x,b) (0x48 | (((r) & 1) << 2) | (((x) & 1) << 1) | ((b) & 1))

// ModR/M
// mod
// reg: reg/opcode
// rm: r/m
#define ModRM(mod,reg,rm) ((((mod) & 3) << 6) | (((reg) & 7) << 3) | ((rm) & 7))

// ss -> scale: 0b00 -> *1, 0b01 -> *2, 0b10 -> *4, 0b11 -> *8
#define SIB(ss,index,base) ((((ss) & 3) << 6) | (((index) & 7) << 3) | ((base) & 7))

enum address_scale {
    SCALE_BY_1 = 0,
    SCALE_BY_2 = 1,
    SCALE_BY_4 = 2,
    SCALE_BY_8 = 3
};

static inline uint8_t *push_r64(uint8_t *instr, enum reg64 r)
{
    // PUSH r64
    if (r > rdi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0x50 | (r & 7);
    return instr;
}

static inline uint8_t *pop_r64(uint8_t *instr, enum reg64 r)
{
    // POP r64
    if (r > rdi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0x58 | (r & 7);
    return instr;
}

static inline uint8_t *mov_r32_r32(uint8_t *instr, enum reg32 dst, enum reg32 src)
{
    // MOV r/m32, r32
    if (src > edi || dst > edi) {
        *instr++ = REX(src >> 3, 0, dst >> 3);
    }
    *instr++ = 0x89;
    *instr++ = ModRM(3, src & 7, dst & 7);
    return instr;
}

static inline uint8_t *mov_r64_r64(uint8_t *instr, enum reg64 dst, enum reg64 src)
{
    // MOV r/m64, r64
    *instr++ = REX_W(src >> 3, 0, dst >> 3);
    *instr++ = 0x89;
    *instr++ = ModRM(3, src & 7, dst & 7);
    return instr;
}

static inline uint8_t *mov_r32_imm32(uint8_t *instr, enum reg32 dst, uint32_t imm)
{
    // MOV r32, imm32
    if (dst > edi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0xB8 | (dst & 7);
    memcpy(instr, &imm, 4);
    return instr + 4;
}

static inline uint8_t *mov_r64_imm64(uint8_t *instr, enum reg64 dst, uint64_t imm)
{
    // MOV r64, imm64
    *instr++ = REX_W(0, 0, dst >> 3);
    *instr++ = 0xB8 | (dst & 7);
    memcpy(instr, &imm, 8);
    return instr + 8;
}

static inline uint8_t *mov_r32_dword_ptr(uint8_t *instr, enum reg32 dst, enum reg64 base, enum address_scale scale, enum reg64 index, int32_t disp)
{
    // MOV r32, r/m32;  m=[base + scale*index + disp]
    assert(base != rbp);
    assert(index != rsp);
    if (dst > edi || index > rdi || base > rdi) {
        *instr++ = REX(dst >> 3, index >> 3, base >> 3);
    }
    *instr++ = 0x8b;
    if (disp == 0 && base != r13) {
        *instr++ = ModRM(0, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *mov_r32_dword_ptr_rsp(uint8_t *instr, enum reg32 dst, int32_t disp)
{
    // MOV r32, r/m32;  m=[rsp + disp]
    if (dst > edi) {
        *instr++ = REX(dst >> 3, 0, 0);
    }
    *instr++ = 0x8b;
    if (disp == 0) {
        *instr++ = ModRM(0, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(0, 4, rsp);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(0, 4, rsp);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(0, 4, rsp);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *mov_r64_qword_ptr(uint8_t *instr, enum reg64 dst, enum reg64 base, enum address_scale scale, enum reg64 index, int32_t disp)
{
    // MOV r64, r/m64;  m=[base + scale*index + disp]
    assert(base != rbp);
    assert(index != rsp);
    *instr++ = REX_W(dst >> 3, index >> 3, base >> 3);
    *instr++ = 0x8b;
    if (disp == 0 && base != r13) {
        *instr++ = ModRM(0, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *mov_r64_qword_ptr_rsp(uint8_t *instr, enum reg64 dst, int32_t disp)
{
    // MOV r64, r/m64;  m=[rsp + disp]
    *instr++ = REX_W(dst >> 3, 0, 0);
    *instr++ = 0x8b;
    if (disp == 0) {
        *instr++ = ModRM(0, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(0, 4, rsp);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(0, 4, rsp);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(0, 4, rsp);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *mov_dword_ptr_r32(uint8_t *instr, enum reg64 base, enum address_scale scale, enum reg64 index, int32_t disp, enum reg32 src)
{
    // MOV r/m32, r32;  m=[base + scale*index + disp]
    assert(base != rbp);
    assert(index != rsp);
    if (src > edi || index > rdi || base > rdi) {
        *instr++ = REX(src >> 3, index >> 3, base >> 3);
    }
    *instr++ = 0x89;
    if (disp == 0 && base != r13) {
        *instr++ = ModRM(0, src & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, src & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, src & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *call_rel32(uint8_t *instr, int32_t offset)
{
    // CALL rel32
    *instr++ = 0xe8;
    memcpy(instr, &offset, 4);
    return instr + 4;
}

static inline uint8_t *call_rel32_ptr(uint8_t *instr, void *target)
{
    // CALL rel32
    *instr++ = 0xe8;
    ptrdiff_t offset = (uint8_t *)target - (instr + 4);
    assert(INT32_MIN <= offset && offset <= INT32_MAX);
    int32_t offset_i32 = (int32_t)offset;
    memcpy(instr, &offset_i32, 4);
    return instr + 4;
}

static inline uint8_t *call_r64(uint8_t *instr, enum reg64 r)
{
    // CALL r/m64
    if (r > rdi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0xff;
    *instr++ = ModRM(3, 2, r & 7);
    return instr + 4;
}

static inline uint8_t *je_rel8(uint8_t *instr, int offset)
{
    // JE rel8
    assert(-128 <= offset && offset <= 127);
    *instr++ = 0x74;
    *instr++ = (uint8_t)offset;
    return instr;
}

static inline uint8_t *je_rel32(uint8_t *instr, int32_t offset)
{
    // JE rel32
    *instr++ = 0x0f;
    *instr++ = 0x84;
    memcpy(instr, &offset, 4);
    return instr + 4;
}

static inline uint8_t *jne_rel8(uint8_t *instr, int offset)
{
    // JNE rel8
    assert(-128 <= offset && offset <= 127);
    *instr++ = 0x75;
    *instr++ = (uint8_t)offset;
    return instr;
}

static inline uint8_t *jne_rel32(uint8_t *instr, int32_t offset)
{
    // JNE rel32
    *instr++ = 0x0f;
    *instr++ = 0x85;
    memcpy(instr, &offset, 4);
    return instr + 4;
}

static inline uint8_t *jmp_rel32(uint8_t *instr, int32_t offset)
{
    // JMP rel32
    *instr++ = 0xe9;
    memcpy(instr, &offset, 4);
    return instr + 4;
}

static inline uint8_t *jmp_rel32_ptr(uint8_t *instr, void *target)
{
    // JMP rel32
    *instr++ = 0xe9;
    ptrdiff_t offset = (uint8_t *)target - (instr + 4);
    assert(INT32_MIN <= offset && offset <= INT32_MAX);
    int32_t offset_i32 = (int32_t)offset;
    memcpy(instr, &offset_i32, 4);
    return instr + 4;
}

static inline uint8_t *jmp_r64(uint8_t *instr, enum reg64 r)
{
    // JMP r/m64
    if (r > rdi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0xff;
    *instr++ = ModRM(3, 4, r & 7);
    return instr;
}

static inline uint8_t *jmp_qword_ptr(uint8_t *instr, enum reg64 base, enum address_scale scale, enum reg64 index, int32_t disp)
{
    // JMP r/m64
    assert(base != rbp);
    assert(index != rsp);
    if (base > rdi || index > rdi) {
        *instr++ = REX(0, index >> 3, base >> 3);
    }
    *instr++ = 0xff;
    if (disp == 0 && base != r13) {
        *instr++ = ModRM(0, 4, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, 4, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, 4, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *test_r32_r32(uint8_t *instr, enum reg32 r1, enum reg32 r2)
{
    // TEST r/m32, r32
    if (r1 > edi || r2 > edi) {
        *instr++ = REX(r2 >> 3, 0, r1 >> 3);
    }
    *instr++ = 0x85;
    *instr++ = ModRM(3, r2 & 7, r1 & 7);
    return instr;
}

static inline uint8_t *cmovne_r32_r32(uint8_t *instr, enum reg32 r1, enum reg32 r2)
{
    // CMOVNE r32, r/m32
    if (r1 > edi || r2 > edi) {
        *instr++ = REX(r1 >> 3, 0, r2 >> 3);
    }
    *instr++ = 0x0f;
    *instr++ = 0x45;
    *instr++ = ModRM(3, r1 & 7, r2 & 7);
    return instr;
}

static inline uint8_t *lea_r32(uint8_t *instr, enum reg32 dst, enum reg32 base, enum address_scale scale, enum reg32 index, int32_t disp)
{
    // LEA r32, m;  m = [base + scale*index + disp]
    assert(base != ebp);
    assert(index != esp);
    *instr++ = 0x67; // operand size=32, address size=32
    if (dst > edi || base > edi || index > edi) {
        *instr++ = REX(dst >> 3, index >> 3, base >> 3);
    }
    *instr++ = 0x8d;
    if (disp == 0 && base != r13d) {
        *instr++ = ModRM(0, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
    } else if (-128 <= disp && disp <= 127) {
        *instr++ = ModRM(1, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        *instr++ = disp & 0xff;
    } else {
        *instr++ = ModRM(2, dst & 7, /* Use SIB */ 4);
        *instr++ = SIB(scale, index & 7, base & 7);
        memcpy(instr, &disp, 4);
        instr += 4;
    }
    return instr;
}

static inline uint8_t *mul_r32(uint8_t *instr, enum reg32 a)
{
    // MUL r/m32
    // EDX:EAX := EAX * r/m32
    if (a > edi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0xf7;
    *instr++ = ModRM(3, 4, a & 7);
    return instr;
}

static inline uint8_t *div_r32(uint8_t *instr, enum reg32 a)
{
    // DIV r/m32
    // EDX:EAX -> EAX (quotient) * r/m32 + EDX (remainder)
    if (a > edi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0xf7;
    *instr++ = ModRM(3, 6, a & 7);
    return instr;
}

static inline uint8_t *not_r32(uint8_t *instr, enum reg32 a)
{
    // NOT r/m32
    if (a > edi) {
        *instr++ = REX(0, 0, 1);
    }
    *instr++ = 0xf7;
    *instr++ = ModRM(3, 2, a & 7);
    return instr;
}

static inline uint8_t *and_r32_r32(uint8_t *instr, enum reg32 src1_dst, enum reg32 src2)
{
    // AND r/m32, r32
    if (src1_dst > edi || src2 > edi) {
        *instr++ = REX(src2 >> 3, 0, src1_dst >> 3);
    }
    *instr++ = 0x21;
    *instr++ = ModRM(3, src2 & 7, src1_dst & 7);
    return instr;
}

static inline uint8_t *xor_r32_r32(uint8_t *instr, enum reg32 src1_dst, enum reg32 src2)
{
    // XOR r/m32, r32
    if (src1_dst > edi || src2 > edi) {
        *instr++ = REX(src2 >> 3, 0, src1_dst >> 3);
    }
    *instr++ = 0x31;
    *instr++ = ModRM(3, src2 & 7, src1_dst & 7);
    return instr;
}

#undef REX
#undef REX_W
#undef ModRM
#undef SIB

#endif // X86ASM_H
