#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/mman.h> // mmap, mprotect, munmap
#include <unistd.h> // getpagesize
#if defined(__APPLE__)
#include <pthread.h> // pthread_jit_write_protect_np
#include <libkern/OSCacheControl.h> // sys_icache_invalidate
#endif

struct array {
    uint32_t length;
    uint32_t data[];
};

static uint32_t arraysize;
static struct array **arrays; // struct array *arrays[arraysize]
static uint32_t *freelist = NULL, *freelist_end = NULL;
static uint32_t freelist_capacity = 0;
static void **jumptable; // void *jumptable[arr0size + 1]
static void *program; // uintptr_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc)
static void *program_end;
static size_t program_mem_capacity;
static uint32_t *L_epilogue;
static uint32_t *L_jump_to_fn[5]; // um_modify_0, um_alloc, um_free, um_putchar, um_getchar;
static bool verbose = false;
static char *presupplied_input = NULL;
static bool discard_initial_output = false;
static bool *patched;

static void um_modify_0(uint32_t b, uint32_t c)
{
    if (patched[b]) {
        return;
    }
    patched[b] = true;
    uint32_t BL_L_epilogue;
    {
        /* BL L_epilogue */
        ptrdiff_t offset = L_epilogue - (uint32_t *)jumptable[b];
        assert(-0x2000000 <= offset && offset < 0x2000000);
        uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
        BL_L_epilogue = 0x94000000 | imm26;
    }
    if (verbose) {
        fprintf(stderr, "<<<self modification>>>");
    }
#if defined(__APPLE__)
    pthread_jit_write_protect_np(0); // Make the memory writable
#endif
    uint32_t *pstart = (uint32_t *)jumptable[b];
    uint32_t *pend = (uint32_t *)jumptable[b + 1];
    {
        uint32_t *instr = pstart;
        *instr++ = BL_L_epilogue;
        while (instr != pend) {
            /* NOP */
            *instr++ = 0xD503201F;
        }
    }
#if defined(__APPLE__)
    sys_icache_invalidate(pstart, (char *)pend - (char *)pstart);
#else
    __builtin___clear_cache((void *)pstart, (void *)pend);
#endif
#if defined(__APPLE__)
    pthread_jit_write_protect_np(1); // Make the memory executable
#endif
}
struct alloc_result {
    uint32_t identifier;
    struct array **arrays;
};
static struct alloc_result um_alloc(uint32_t capacity)
{
    uint32_t i = 0;
    if (freelist_end == freelist) {
        i = arraysize;
        ++arraysize;
        arrays = realloc(arrays, sizeof(struct array *) * arraysize);
        assert(arrays != NULL);
    } else {
        i = *(--freelist_end);
        *freelist_end = 0;
    }
    struct array *newarr = calloc(1 + capacity, sizeof(uint32_t));
    assert(newarr != NULL);
    newarr->length = capacity;
    arrays[i] = newarr;
    return (struct alloc_result){/* w0 */ i, /* x1 */ arrays};
}
static void um_free(uint32_t id)
{
    assert(id < arraysize);
    assert(id != 0);
    assert(arrays[id] != NULL);
    free(arrays[id]);
    arrays[id] = NULL;
    if (freelist_end - freelist == freelist_capacity) {
        uint32_t freelist_new_capacity = freelist_capacity == 0 ? 32 : freelist_capacity * 2;
        freelist = realloc(freelist, freelist_new_capacity * sizeof(uint32_t));
        memset(freelist + freelist_capacity, 0, (freelist_new_capacity - freelist_capacity) * sizeof(uint32_t));
        freelist_end = freelist + freelist_capacity;
        freelist_capacity = freelist_new_capacity;
    }
    *freelist_end++ = id;
}
static void um_putchar(uint32_t x)
{
    assert(x <= 255);
    if (!discard_initial_output || presupplied_input == NULL || *presupplied_input == '\0') {
        putchar(x);
    }
}
static uint32_t um_getchar(void)
{
    int ch;
    if (presupplied_input == NULL || *presupplied_input == '\0') {
        fflush(stdout);
        ch = getchar();
    } else {
        ch = (unsigned char)*presupplied_input++;
    }
    if (ch == EOF) {
        return (uint32_t)(-1);
    } else {
        assert(0 <= ch && ch <= 255);
        return (uint32_t)ch;
    }
}
static const uint32_t Xarrays = 19;
static const uint32_t Xjumptable = 20;
static const uint32_t REG[8] = {21, 22, 23, 24, 25, 26, 27, 28};
static uint32_t *write_instr(uint32_t *instr, uint32_t op)
{
    switch (op >> 28) {
    case 0: /* Conditional Move */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            if (Wa == Wb) {
                /* NOP */
                *instr++ = 0xD503201F;
            } else {
                {
                    /* CMP Wc, #0 */
                    uint32_t sf = 0; // 0: 32bit, 1: 64bit
                    uint32_t sh = 0; // 0: LSL #0, 1: LSL #12
                    uint32_t imm12 = 0;
                    *instr++ = 0x7100001F | (sf << 31) | (sh << 22) | (imm12 << 10) | (/* Rn */ Wc << 5);
                }
                {
                    /* CSEL Wa, Wa, Wb, eq; Wa = eq ? Wa : Wb */
                    uint32_t sf = 0; // 0: 32bit, 1: 64bit
                    uint32_t cond = 0; // 0: EQ
                    *instr++ = 0x1A800000 | (sf << 31) | (/* Rm */ Wb << 16) | (cond << 12) | (/* Rn */ Wa << 5) | /* Rd */ Wa;
                }
            }
            break;
        }
    case 1: /* Array Index */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            uint32_t Xtmp = 0; // 64-bit, temporary
            {
                /* LDR Xtmp, [Xarrays, Wb, UXTW #3] */
                uint32_t size = 3; // 64-bit variant
                uint32_t option = 2; // UXTW
                uint32_t S = 1; // amount = 3
                *instr++ = 0x38600800 | (size << 30) | (/* Rm */ Wb << 16) | (option << 13) | (S << 12) | (/* Rn */ Xarrays << 5) | /* Rt */ Xtmp;
            }
            {
                /* ADD Xtmp, Xtmp, Wc, UXTW #2 */
                uint32_t sf = 1; // 0: 32bit, 1: 64bit
                uint32_t option = 2; // Rm's width=W, UXTW
                uint32_t imm3 = 2;
                *instr++ = 0x0B200000 | (sf << 31) | (/* Rm */ Wc << 16) | (option << 13) | (imm3 << 10) | (/* Rn */ Xtmp << 5) | /* Rd */ Xtmp;
            }
            {
                /* LDR Wa, [Xtmp, #4] */
                uint32_t size = 2; // 32-bit variant
                uint32_t imm12 = 1;
                *instr++ = 0x39400000 | (size << 30) | (imm12 << 10) | (/* Rn */ Xtmp << 5) | /* Rt */ Wa;
            }
            break;
        }
    case 2: /* Array Amendment */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            uint32_t Xtmp = 0; // 64-bit, temporary
            {
                /* LDR Xtmp, [Xarrays, Wa, UXTW #3] */
                uint32_t size = 3; // 64-bit variant
                uint32_t option = 2; // UXTW
                uint32_t S = 1; // amount = 3
                *instr++ = 0x38600800 | (size << 30) | (/* Rm */ Wa << 16) | (option << 13) | (S << 12) | (/* Rn */ Xarrays << 5) | /* Rt */ Xtmp;
            }
            {
                /* ADD Xtmp, Xtmp, Wb, UXTW #2 */
                uint32_t sf = 1; // 0: 32bit, 1: 64bit
                uint32_t option = 2; // Rm's width=W, UXTW
                uint32_t imm3 = 2;
                *instr++ = 0x0B200000 | (sf << 31) | (/* Rm */ Wb << 16) | (option << 13) | (imm3 << 10) | (/* Rn */ Xtmp << 5) | /* Rd */ Xtmp;
            }
            {
                /* STR Wc, [Xtmp, #4]! */
                uint32_t size = 2; // 32-bit variant
                uint32_t imm9 = 4;
                *instr++ = 0x38000C00 | (size << 30) | (imm9 << 12) | (/* Rn */ Xtmp << 5) | /* Rt */ Wc;
            }
            {
                /* CBNZ Wa, L_next */
                uint32_t sf = 0; // 32-bit variant
                uint32_t imm19 = 4;
                *instr++ = 0x35000000 | (sf << 31) | (imm19 << 5) | /* Rt */ Wa;
            }
            {
                /* MOV W0, Wb */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wb << 16) | /* Rd */ 0;
            }
            {
                /* MOV W1, Wc */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wc << 16) | /* Rd */ 1;
            }
            {
                /* BL L_jump_to_fn[0] (um_modify_0) */
                uint32_t *dest = (uint32_t *)L_jump_to_fn[0];
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            /* L_next: */
            break;
        }
    case 3: /* Addition */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            {
                /* ADD Wa, Wb, Wc; Wa = Wb + Wc */
                uint32_t sf = 0; // 0: 32bit, 1: 64bit
                uint32_t shift = 0; // 0: LSL, 1: LSR, 2: ASR, 3: reserved
                uint32_t imm6 = 0;
                *instr++ = 0x0B000000 | (sf << 31) | (shift << 22) | (/* Rm */ Wc << 16) | (imm6 << 10) | (/* Rn */ Wb << 5) | /* Rd */ Wa;
            }
            break;
        }
    case 4: /* Multiplication */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            {
                /* MUL Wa, Wb, Wc; Wa = Wb * Wc */
                uint32_t sf = 0;
                *instr++ = 0x1B007C00 | (sf << 31) | (/* Rm */ Wc << 16) | (/* Rn */ Wb << 5) | /* Rd */ Wa;
            }
            break;
        }
    case 5: /* Division */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            // assert(value of Wc != 0);
            {
                /* UDIV Wa, Wb, Wc; Wa = Wb / Wc */
                uint32_t sf = 0;
                *instr++ = 0x1AC00800 | (sf << 31) | (/* Rm */ Wc << 16) | (/* Rn */ Wb << 5) | Wa;
            }
            break;
        }
    case 6: /* Not-And */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            if (Wb == Wc) {
                // Bitwise Negation
                /* MVN Wa, Wb, Wa = ~Wb */
                uint32_t sf = 0;
                uint32_t shift = 0; // 0: LSL, 1: LSR, 2: ASR, 3: ROR
                uint32_t imm6 = 0;
                *instr++ = 0x2A2003E0 | (sf << 31) | (shift << 22) | (/* Rm */ Wb << 16) | (imm6 << 10) | /* Rd */ Wa;
            } else {
                // Not-And
                {
                    /* AND Wa, Wb, Wc; Wa = Wb & Wc */
                    uint32_t sf = 0;
                    uint32_t shift = 0; // 0: LSL, 1: LSR, 2: ASR, 3: ROR
                    uint32_t imm6 = 0;
                    *instr++ = 0x0A000000 | (sf << 31) | (shift << 22) | (/* Rm */ Wc << 16) | (imm6 << 10) | (/* Rn */ Wb << 5) | /* Rd */ Wa;
                }
                {
                    /* MVN Wa, Wa; Wa = ~Wa */
                    uint32_t sf = 0;
                    uint32_t shift = 0; // 0: LSL, 1: LSR, 2: ASR, 3: ROR
                    uint32_t imm6 = 0;
                    *instr++ = 0x2A2003E0 | (sf << 31) | (shift << 22) | (/* Rm */ Wa << 16) | (imm6 << 10) | /* Rd */ Wa;
                }
            }
            break;
        }
    case 7: /* Halt */
        {
            /* BL L_epilogue */
            ptrdiff_t offset = L_epilogue - instr;
            assert(-0x2000000 <= offset && offset < 0x2000000);
            uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
            *instr++ = 0x94000000 | imm26;
        }
        break;
    case 8: /* Allocation */
        {
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            {
                /* MOV W0, Wc */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wc << 16) | /* Rd */ 0;
            }
            {
                /* BL L_jump_to_fn[1] (um_alloc) */
                uint32_t *dest = (uint32_t *)L_jump_to_fn[1];
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            {
                /* MOV Wb, W0 */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ 0 << 16) | /* Rd */ Wb;
            }
            {
                /* MOV Xarrays, X1 */
                uint32_t sf = 1; // 64-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ 1 << 16) | /* Rd */ Xarrays;
            }
            break;
        }
    case 9: /* Abandonment */
        {
            uint32_t Wc = REG[op & 7];
            {
                /* MOV W0, Wc */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wc << 16) | /* Rd */ 0;
            }
            {
                /* BL L_jump_to_fn[2] (um_free) */
                uint32_t *dest = (uint32_t *)L_jump_to_fn[2];
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            break;
        }
    case 10: /* Output */
        {
            uint32_t Wc = REG[op & 7];
            {
                /* MOV W0, Wc */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wc << 16) | /* Rd */ 0;
            }
            {
                /* BL L_jump_to_fn[3] (um_putchar) */
                uint32_t *dest = (uint32_t *)L_jump_to_fn[3];
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            break;
        }
    case 11: /* Input */
        {
            uint32_t Wc = REG[op & 7];
            {
                /* BL L_jump_to_fn[4] (um_getchar) */
                uint32_t *dest = (uint32_t *)L_jump_to_fn[4];
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            {
                /* MOV Wc, W0 */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ 0 << 16) | /* Rd */ Wc;
            }
            break;
        }
    case 12: /* Load Program */
        {
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
            uint32_t *cbnz = instr;
            {
                /* HLT #0 (-> CBNZ Wb, L_reload) */
                uint32_t imm16 = 0;
                *instr++ = 0xD4400000 | (imm16 << 5); /* HLT #0 */
            }
            {
                /* LDR X0, [Xjumptable, Wc, UXTW #3] */
                uint32_t size = 3; // 64-bit variant
                uint32_t option = 2; // UXTW
                uint32_t S = 1; // #3
                *instr++ = 0x38600800 | (size << 30) | (/* Rm */ Wc << 16) | (option << 13) | (S << 12) | (/* Rn */ Xjumptable << 5) | /* Rt */ 0;
            }
            {
                /* BR X0 */
                *instr++ = 0xD61F0000 | (/* Rn */ 0 << 5);
            }
            /* L_reload: */
            uint32_t *L_reload = instr;
            {
                /* CBNZ Wb, L_reload */
                uint32_t sf = 0; // 32-bit variant
                ptrdiff_t diff = L_reload - cbnz;
                assert(-0x40000 <= diff && diff < 0x40000);
                uint32_t imm19 = (uint32_t)diff & 0x7FFFF;
                *cbnz = 0x35000000 | (sf << 31) | (imm19 << 5) | /* Rt */ Wb;
            }
            {
                /* BL L_epilogue */
                ptrdiff_t offset = L_epilogue - instr;
                assert(-0x2000000 <= offset && offset < 0x2000000);
                uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            break;
        }
    case 13: /* Orthography */
        {
            uint32_t Wa = REG[(op >> 25) & 7];
            uint32_t value = op & ((UINT32_C(1) << 25) - 1);
            if (value <= 0xffff) {
                // 16-bit immediate
                /* MOVZ Wa, #value */
                uint32_t sf = 0;
                uint32_t hw = 0;
                *instr++ = 0x52800000 | (sf << 31) | (hw << 21) | (value << 5) | /* Rd */ Wa;
            } else {
                // 25-bit immediate
                uint32_t value_lo = value & 0xffff;
                uint32_t value_hi = value >> 16;
                {
                    /* MOVZ Wa, #value_lo */
                    uint32_t sf = 0;
                    uint32_t hw = 0;
                    *instr++ = 0x52800000 | (sf << 31) | (hw << 21) | (value_lo << 5) | /* Rd */ Wa;
                }
                {
                    /* MOVK Wa, #value_hi, LSL #16 */
                    uint32_t sf = 0;
                    uint32_t hw = 1;
                    *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (value_hi << 5) | /* Rd */ Wa;
                }
            }
            break;
        }
    default: /* Invalid */
        {
            /* BL L_epilogue */
            ptrdiff_t offset = L_epilogue - instr;
            assert(-0x2000000 <= offset && offset < 0x2000000);
            uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
            *instr++ = 0x94000000 | imm26;
        }
        break;
    }
    return instr;
}
static void compile(struct array *arr0)
{
    if (program != NULL) {
        munmap(program, program_mem_capacity);
    }
    if (patched != NULL) {
        free(patched);
    }
    patched = calloc(arr0->length, sizeof(bool));
    size_t pagesize = getpagesize();
#if defined(__APPLE__)
    pthread_jit_write_protect_np(0); // Make the memory writable
#endif
    size_t size = 8 * 4 * (size_t)arr0->length + pagesize;
    size = (size + pagesize - 1) / pagesize * pagesize;
    int flags = MAP_ANON | MAP_PRIVATE;
#if defined(__APPLE__)
    flags |= MAP_JIT;
#endif
    void *mem = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, flags, -1, 0);
    if (mem == MAP_FAILED) {
        int e = errno;
        fflush(stdout);
        fprintf(stderr, "<<<mmap failed with errno = %d (%s)>>>\n", e, strerror(e));
        abort();
    }
    program = mem;
    program_mem_capacity = size;
    jumptable = realloc(jumptable, sizeof(void *) * ((size_t)arr0->length + 1));
    assert(jumptable != NULL);
    uint32_t *instr = mem;
    /*
     * x19: arrays
     * x20: jumptable
     * w21: VM's R0
     * w22: VM's R1
     * w23: VM's R2
     * w24: VM's R3
     * w25: VM's R4
     * w26: VM's R5
     * w27: VM's R6
     * w28: VM's R7
     * They are all callee-save registers.
     */

    /* Prologue */
    /* Save x29, x30 */
    {
        /* STP x29, x30, [SP, #-16]! */
        uint32_t opc = 2; // 64-bit variant
        uint32_t imm7 = 0x7e; /* -16, encoded as -16/8=-2 */
        uint32_t SP = 31;
        *instr++ = 0x29800000 | (opc << 30) | (imm7 << 15) | (/* Rt2 */ 30 << 10) | (/* Rn */ SP << 5) | /* Rt */ 29;
    }
    /* Save callee-save registers */
    for (uint32_t i = 0; i < 5; ++i) {
        uint32_t Xa = 19 + 2 * i;
        uint32_t Xb = 20 + 2 * i;
        /* STP Xa, Xb, [SP, #-16]! */
        uint32_t opc = 2; // 64-bit variant
        uint32_t imm7 = 0x7e; /* -16, encoded as -16/8=-2 */
        uint32_t SP = 31;
        *instr++ = 0x29800000 | (opc << 30) | (imm7 << 15) | (/* Rt2 */ Xb << 10) | (/* Rn */ SP << 5) | /* Rt */ Xa;
    }
    /* Save x2 (uint32_t registers[8]) */
    {
        /* STR x2, [SP, #-16]! */
        uint32_t size = 3; // 64-bit variant
        uint32_t imm9 = 0x1ff - 15; /* -16 */
        uint32_t SP = 31;
        *instr++ = 0x38000C00 | (size << 30) | (imm9 << 12) | (/* Rn */ SP << 5) | /* Rt */ 2;
    }
    /* Set Frame Pointer */
    {
        /* ADD X29, SP, #(16 * 6) */
        uint32_t sf = 1; // 64-bit variant
        uint32_t sh = 0;
        uint32_t imm12 = 16 * 6;
        uint32_t SP = 31;
        *instr++ = 0x11000000 | (sf << 31) | (sh << 22) | (imm12 << 10) | (/* Rn */ SP << 5) | /* Rd */ 29;
    }
    /* Load Xarrays and Xjumptable */
    {
        /* MOV Xarrays, X0 */
        uint32_t sf = 1; // 64-bit variant
        *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ 0 << 16) | /* Rd */ Xarrays;
    }
    {
        /* MOV Xjumptable, X1 */
        uint32_t sf = 1; // 64-bit variant
        *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ 1 << 16) | /* Rd */ Xjumptable;
    }
    /* Load VM registers */
    for (uint32_t i = 0; i < 4; ++i) {
        /* LDP W(REG[2*i]), W(REG[2*i+1]), [X2, #(8*i)] */
        uint32_t opc = 0; // 32-bit variant
        uint32_t imm7 = 2 * i;
        uint32_t Xn = 2;
        *instr++ = 0x29400000 | (opc << 30) | (imm7 << 15) | (/* Rt2 */ REG[2 * i + 1] << 10) | (/* Rn */ Xn << 5) | /* Rt */ REG[2 * i];
    }
    /* Jump to initial_pc (W3) */
    {
        /* LDR X0, [Xjumptable, W3, UXTW #3] */
        uint32_t size = 3; // 64-bit variant
        uint32_t option = 2; // UXTW
        uint32_t S = 1; // #3
        *instr++ = 0x38600800 | (size << 30) | (/* Rm */ 3 << 16) | (option << 13) | (S << 12) | (/* Rn */ Xjumptable << 5) | /* Rt */ 0;
    }
    {
        /* BR X0 */
        *instr++ = 0xD61F0000 | (/* Rn */ 0 << 5);
    }

    /* Epilogue */
    /* L_epilogue: */
    L_epilogue = instr;
    {
        /* SUB X0, X30, #4 */
        uint32_t sf = 1;
        uint32_t sh = 0;
        uint32_t imm12 = 4;
        *instr++ = 0x51000000 | (sf << 31) | (sh << 22) | (imm12 << 10) | (/* Rn */ 30 << 5) | /* Rd */ 0;
    }
    /* Restore X2 */
    {
        /* LDR X2, [SP], #16 */
        uint32_t size = 3; // 64-bit variant
        uint32_t imm9 = 16;
        uint32_t SP = 31;
        *instr++ = 0x38400400 | (size << 30) | (imm9 << 12) | (/* Rn */ SP << 5) | /* Rt */ 2;
    }
    /* Save VM registers */
    for (uint32_t i = 0; i < 4; ++i) {
        /* STP W(REG[2*i]), W(REG[2*i+1]), [X2, #(8*i)] */
        uint32_t opc = 0; // 32-bit variant
        uint32_t imm7 = 2 * i;
        uint32_t Xn = 2;
        *instr++ = 0x29000000 | (opc << 30) | (imm7 << 15) | (/* Rt2 */ REG[2 * i + 1] << 10) | (/* Rn */ Xn << 5) | /* Rt */ REG[2 * i];
    }
    /* Restore callee-save registers */
    for (int32_t i = 4; i >= 0; --i) {
        uint32_t Xa = 19 + 2 * i;
        uint32_t Xb = 20 + 2 * i;
        /* LDP Xa, Xb, [SP], #16 */
        uint32_t opc = 2; // 64-bit variant
        uint32_t imm7 = 2;
        uint32_t SP = 31;
        *instr++ = 0x28C00000 | (opc << 30) | (imm7 << 15) | (/* Rt2 */ Xb << 10) | (/* Rn */ SP << 5) | /* Rt */ Xa;
    }
    /* Load x29, x30 */
    {
        /* LDP x29, x30, [SP], #16 */
        uint32_t opc = 2; // 64-bit variant
        uint32_t imm7 = 2;
        uint32_t SP = 31;
        *instr++ = 0x28C00000 | (opc << 30) | (imm7 << 15) | (/* Rt2 */ 30 << 10) | (/* Rn */ SP << 5) | /* Rt */ 29;
    }
    {
        /* RET */
        *instr++ = 0xD65F0000 | (30 << 5); /* RET x30 */
    }

    /* Jumps */
    {
        uintptr_t addrs[5] = {(uintptr_t)um_modify_0, (uintptr_t)um_alloc, (uintptr_t)um_free, (uintptr_t)um_putchar, (uintptr_t)um_getchar};
        for (int i = 0; i < 5; ++i) {
            L_jump_to_fn[i] = instr;
            uintptr_t addr = addrs[i];
            uint32_t Xtmp = 9;
            {
                /* MOVZ Xtmp, #addr0 */
                uint32_t sf = 1;
                uint32_t hw = 0;
                uint32_t imm16 = addr & 0xffff;
                *instr++ = 0x52800000 | (sf << 31) | (hw << 21) | (imm16 << 5) | /* Rd */ Xtmp;
            }
            {
                /* MOVK Xtmp, #addr16, LSL #16 */
                uint32_t sf = 1;
                uint32_t hw = 1;
                uint32_t imm16 = (addr >> 16) & 0xffff;
                *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (imm16 << 5) | /* Rd */ Xtmp;
            }
            {
                /* MOVK Xtmp, #addr32, LSL #16 */
                uint32_t sf = 1;
                uint32_t hw = 2;
                uint32_t imm16 = (addr >> 32) & 0xffff;
                *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (imm16 << 5) | /* Rd */ Xtmp;
            }
            {
                /* MOVK Xtmp, #addr48, LSL #16 */
                uint32_t sf = 1;
                uint32_t hw = 3;
                uint32_t imm16 = (addr >> 48) & 0xffff;
                *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (imm16 << 5) | /* Rd */ Xtmp;
            }
            {
                /* BR Xtmp */
                *instr++ = 0xD61F0000  | (/* Rn */ Xtmp << 5);
            }
        }
    }

    for (uint32_t i = 0; i < arr0->length; ++i) {
        jumptable[i] = instr;
        uint32_t op = arr0->data[i];
        instr = write_instr(instr, op);
    }
    jumptable[arr0->length] = instr;
    {
        uint32_t imm16 = 0;
        *instr++ = 0xD4400000 | (imm16 << 5); /* HLT #0 */
    }
    program_end = instr;
#if defined(__APPLE__)
    pthread_jit_write_protect_np(1); // Make the memory executable
#endif
#if defined(__APPLE__)
    sys_icache_invalidate(mem, (char *)instr - (char *)mem);
#else
    __builtin___clear_cache(mem, (void *)instr);
#endif
}

static int usage(const char *argv0)
{
    fprintf(stderr, "Usage: %s file.um\nOptions:\n  --input [text]\n  --discard-initial-output\n  --dump\n  --verbose\n  --help\n", argv0);
    return 1;
}

int main(int argc, char *argv[])
{
    bool dump = false;
    const char *filename = NULL;
    for (int i = 1; i < argc; ++i) {
        if (strcmp(argv[i], "--input") == 0) {
            if (i + 1 < argc) {
                ++i;
                size_t len0 = presupplied_input == NULL ? 0 : strlen(presupplied_input);
                size_t len1 = strlen(argv[i]);
                presupplied_input = realloc(presupplied_input, len0 + len1 + 2);
                memcpy(presupplied_input + len0, argv[i], len1);
                presupplied_input[len0 + len1] = '\n';
                presupplied_input[len0 + len1 + 1] = '\0';
            }
        } else if (strcmp(argv[i], "--discard-initial-output") == 0) {
            discard_initial_output = true;
        } else if (strcmp(argv[i], "--dump") == 0) {
            dump = true;
        } else if (strcmp(argv[i], "--verbose") == 0) {
            verbose = true;
        } else if (strcmp(argv[i], "--help") == 0) {
            return usage(argv[0]);
        } else {
            filename = argv[i];
        }
    }
    if (filename == NULL) {
        return usage(argv[0]);
    }
    void *rawprogram = NULL;
    size_t rawprogsize = 0;
    {
        FILE *f = fopen(filename, "rb");
        assert(f != NULL);
        int seekresult = fseek(f, 0, SEEK_END);
        assert(seekresult == 0);
        long size = ftell(f);
        assert(size >= 0);
        rawprogsize = size;
        seekresult = fseek(f, 0, SEEK_SET);
        assert(seekresult == 0);
        rawprogram = malloc(rawprogsize);
        assert(rawprogram != NULL);
        size_t result = fread(rawprogram, 1, rawprogsize, f);
        assert(result == rawprogsize);
        fclose(f);
    }
    assert(rawprogsize % 4 == 0);
    struct array *arr0 = malloc(sizeof(uint32_t) + rawprogsize);
    assert(arr0 != NULL);
    arr0->length = rawprogsize / 4;
    {
        uint8_t *rawprogram_ = (uint8_t *)rawprogram;
        for (size_t i = 0; i < rawprogsize / 4; ++i) {
            uint32_t a = (uint32_t)rawprogram_[4 * i];
            uint32_t b = (uint32_t)rawprogram_[4 * i + 1];
            uint32_t c = (uint32_t)rawprogram_[4 * i + 2];
            uint32_t d = (uint32_t)rawprogram_[4 * i + 3];
            arr0->data[i] = (a << 24) | (b << 16) | (c << 8) | d;
        }
        free(rawprogram);
    }
    arrays = malloc(sizeof(struct array *));
    arrays[0] = arr0;
    arraysize = 1;
    freelist = NULL;
    fprintf(stderr, "<<<Loaded program. size=%zu bytes>>>\n", rawprogsize);
    compile(arr0);
    fprintf(stderr, "<<<Compiled>>>\n");
    if (dump) {
        FILE *out = fopen("jit-dump.hex", "w");
        assert(out != NULL);
        uint32_t *instr = program;
        uint32_t *instr_end = program_end;
        for (; instr != instr_end; ++instr) {
            uint32_t i = *instr;
            fprintf(out, "%02X %02X %02X %02X\n", i & 0xFF, (i >> 8) & 0xFF, (i >> 16) & 0xFF, (i >> 24) & 0xFF);
        }
        fclose(out);
        fprintf(stderr, "<<<Dumped>>>\n");
    }
    uint32_t registers[8] = {0};
    uint32_t pc = 0;
    while (1) {
        uintptr_t addr = ((uintptr_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc))program)(arrays, jumptable, registers, pc);

        // Determine the PC
        uint32_t pc0 = 0;
        uint32_t pc1 = arrays[0]->length;
        while (pc0 + 1 < pc1) {
            uint32_t mid = (pc0 + pc1) / 2;
            uintptr_t addrm = (uintptr_t)jumptable[mid];
            if (addr < addrm) {
                pc1 = mid;
            } else {
                pc0 = mid;
            }
        }
        // pc0 + 1 == pc1
        uint32_t op = arrays[0]->data[pc0];
        pc = pc1;

        if (verbose) {
            fflush(stdout);
            fprintf(stderr, "<<<interpreted code; op=%u>>>", op >> 28);
        }

        switch (op >> 28) {
        case 0: /* Conditional Move */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                if (registers[c] != 0) {
                    registers[a] = registers[b];
                }
                break;
            }
        case 1: /* Array Index */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                uint32_t i = registers[b];
                assert(i < arraysize);
                struct array *ai = arrays[i];
                assert(ai != NULL);
                assert(registers[c] < ai->length);
                registers[a] = ai->data[registers[c]];
                break;
            }
        case 2: /* Array Amendment */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                uint32_t i = registers[a];
                assert(i < arraysize);
                struct array *ai = arrays[i];
                assert(ai != NULL);
                assert(registers[b] < ai->length);
                if (i == 0) {
                    uint32_t vb = registers[b];
                    uint32_t vc = registers[c];
                    ai->data[vb] = vc;
                    um_modify_0(vb, vc);
                } else {
                    ai->data[registers[b]] = registers[c];
                }
                break;
            }
        case 3: /* Addition */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                registers[a] = registers[b] + registers[c];
                break;
            }
        case 4: /* Multiplication */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                registers[a] = registers[b] * registers[c];
                break;
            }
        case 5: /* Division */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                assert(registers[c] != 0);
                registers[a] = registers[b] / registers[c];
                break;
            }
        case 6: /* Not-And */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                registers[a] = ~(registers[b] & registers[c]);
                break;
            }
        case 7: /* Halt */
            fflush(stdout);
            fprintf(stderr, "<<<HALT>>>\n");
            return 0;
        case 8: /* Allocation */
            {
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                uint32_t capacity = registers[c];
                struct alloc_result r = um_alloc(capacity);
                registers[b] = r.identifier;
                break;
            }
        case 9: /* Abandonment */
            {
                uint32_t c = op & 7;
                um_free(registers[c]);
                break;
            }
        case 10: /* Output */
            {
                uint32_t c = op & 7;
                um_putchar(registers[c]);
                break;
            }
        case 11: /* Input */
            {
                uint32_t c = op & 7;
                registers[c] = um_getchar();
                break;
            }
        case 12: /* Load Program */
            {
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                uint32_t i = registers[b];
                if (i != 0) {
                    if (verbose) {
                        fflush(stdout);
                        fprintf(stderr, "<<<Reload Program>>>\n");
                    }
                    assert(i < arraysize);
                    assert(arrays[i] != NULL);
                    uint32_t length = arrays[i]->length;
                    struct array *newprogram = malloc((length + 1) * sizeof(uint32_t));
                    assert(newprogram != NULL);
                    memcpy(newprogram, arrays[i], (length + 1) * sizeof(uint32_t));
                    free(arr0);
                    arr0 = newprogram;
                    arrays[0] = newprogram;
                    compile(newprogram);
                }
                pc = registers[c];
                break;
            }
        case 13: /* Orthography */
            {
                uint32_t a = (op >> 25) & 7;
                uint32_t value = op & ((UINT32_C(1) << 25) - 1);
                registers[a] = value;
                break;
            }
        default:
            fflush(stdout);
            fprintf(stderr, "<<<Invalid Instruction: %08X>>>\n", op);
            abort();
        }
    }
}
