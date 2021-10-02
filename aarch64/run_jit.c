#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/mman.h> // mmap, mprotect, munmap
#include <unistd.h> // getpagesize
#if defined(__APPLE__)
#include <libkern/OSCacheControl.h> // sys_icache_invalidate
#endif

#if defined(__GNUC__)
#define LIKELY(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define LIKELY(x) (x)
#define UNLIKELY(x) (x)
#endif

struct array {
    uint32_t length;
    uint32_t data[];
};
struct freelist {
    uint32_t location;
    struct freelist *next;
};

uint32_t arraysize;
struct array **arrays; // struct array *arrays[arraysize]
struct freelist *freelist;
void **jumptable; // void *jumptable[arr0size + 1]
void *program; // uint32_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc)
void *program_end;
size_t program_mem_capacity;

static uint32_t instr_size(uint32_t op);
static uint32_t *write_instr(uint32_t *instr, uint32_t op);

static void um_modify_0(uint32_t b, uint32_t c)
{
    /*
    if (arrays[0]->data[b] != c) {
        arrays[0]->data[b] = c;
    }
    */
    // fprintf(stderr, "<<<self modification>>>");
    int ret = mprotect(program, program_mem_capacity, PROT_READ | PROT_WRITE);
    if (ret != 0) {
        int e = errno;
        fflush(stdout);
        fprintf(stderr, "<<<mprotect failed with errno = %d (%s)>>>\n", e, strerror(e));
        abort();
    }
    uint32_t *pstart = (uint32_t *)jumptable[b];
    uint32_t *pend = (uint32_t *)jumptable[b + 1];
    ptrdiff_t inplace_len = pend - pstart;
    {
        uint32_t *instr = pstart;
        while (instr != pend) {
            /* NOP */
            *instr++ = 0xD503201F;
        }
    }
    uint32_t needed = instr_size(c);
    if (needed <= inplace_len) {
        write_instr(pstart, c);
    } else {
        assert(program_mem_capacity >= (size_t)((char *)program_end - (char *)program) + needed);
        uint32_t *instr = program_end;
        uint32_t *start = instr;
        {
            /* B */
            ptrdiff_t offset = instr - pstart;
            assert(-0x2000000 <= offset && offset < 0x2000000);
            uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
            *pstart = 0x14000000 | imm26;
        }
        /* L: */
        instr = write_instr(instr, c);
        {
            /* B */
            ptrdiff_t offset = pend - instr;
            assert(-0x2000000 <= offset && offset < 0x2000000);
            uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
            *instr++ = 0x14000000 | imm26;
        }
        program_end = instr;
#if defined(__APPLE__)
        sys_icache_invalidate(start, (char *)instr - (char *)start);
#else
        __builtin___clear_cache((void *)start, (void *)instr);
#endif
    }
#if defined(__APPLE__)
    sys_icache_invalidate(pstart, (char *)pend - (char *)pstart);
#else
    __builtin___clear_cache((void *)pstart, (void *)pend);
#endif
    ret = mprotect(program, program_mem_capacity, PROT_READ | PROT_EXEC);
    if (ret != 0) {
        int e = errno;
        fflush(stdout);
        fprintf(stderr, "<<<mprotect failed with errno = %d (%s)>>>\n", e, strerror(e));
        abort();
    }
    // fprintf(stderr, "<<<self modification done>>>");
}
struct alloc_result {
    uint32_t identifier;
    struct array **arrays;
};
static struct alloc_result um_alloc(uint32_t capacity)
{
    uint32_t i = 0;
    if (freelist == NULL) {
        i = arraysize;
        ++arraysize;
        arrays = realloc(arrays, sizeof(struct array *) * arraysize);
        assert(arrays != NULL);
    } else {
        i = freelist->location;
        struct freelist *next = freelist->next;
        free(freelist);
        freelist = next;
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
    {
        struct freelist *f = malloc(sizeof(struct freelist));
        assert(f != NULL);
        f->location = id;
        f->next = freelist;
        freelist = f;
    }
}
static void um_putchar(uint32_t x)
{
    assert(x <= 255);
    putchar(x);
}
static uint32_t um_getchar(void)
{
    fflush(stdout);
    int ch = getchar();
    if (ch == EOF) {
        return (uint32_t)(-1);
    } else {
        assert(0 <= ch && ch <= 255);
        return (uint32_t)ch;
    }
}
static void um_invalid(uint32_t op)
{
    fflush(stdout);
    fprintf(stderr, "<<<Invalid Instruction: %08X>>>\n", op);
    abort();
}
static const uint32_t Xarrays = 19;
static const uint32_t Xjumptable = 20;
static const uint32_t REG[8] = {21, 22, 23, 24, 25, 26, 27, 28};
uint32_t *L_epilogue;
uint32_t instr_size(uint32_t op)
{
    switch (op >> 28) {
    case 0: /* Conditional Move */
        return 2;
    case 1: /* Array Index */
        return 3;
    case 2: /* Array Amendment */
        return 7;
    case 3: /* Addition */
        return 1;
    case 4: /* Multiplication */
        return 1;
    case 5: /* Division */
        return 1;
    case 6: /* Not-And */
        {
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            if (b == c) {
                return 1;
            } else {
                return 2;
            }
        }
    case 7: /* Halt */
        return 3;
    case 8: /* Allocation */
        return 4;
    case 9: /* Abandonment */
        return 2;
    case 10: /* Output */
        return 2;
    case 11: /* Input */
        return 2;
    case 12: /* Load Program */
        return 6;
    case 13: /* Orthography */
        {
            uint32_t value = op & ((UINT32_C(1) << 25) - 1);
            if (value <= 0xffff) {
                return 1;
            } else {
                return 2;
            }
        }
    default:
        return 7;
    }
}
uint32_t *write_instr(uint32_t *instr, uint32_t op)
{
    switch (op >> 28) {
    case 0: /* Conditional Move */
        {
            uint32_t Wa = REG[(op >> 6) & 7];
            uint32_t Wb = REG[(op >> 3) & 7];
            uint32_t Wc = REG[op & 7];
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
                /* BL um_modify_0 */
                uint32_t *dest = (uint32_t *)(void *)&um_modify_0;
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            /* L_next: */
#if 0
            {
                /* MOV W0, Wa */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wa << 16) | /* Rd */ 0;
            }
            {
                /* MOV W1, Wb */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wb << 16) | /* Rd */ 1;
            }
            {
                /* MOV W2, Wc */
                uint32_t sf = 0; // 32-bit variant
                *instr++ = 0x2A0003E0 | (sf << 31) | (/* Rm */ Wc << 16) | /* Rd */ 2;
            }
            {
                /* BL um_modify */
                uint32_t *dest = (uint32_t *)(void *)&um_modify;
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
#endif
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
                    /* MVN Wa, Wb; Wa = ~Wb */
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
            /* MOV W0, #(op) */
            uint32_t value_lo = op & 0xffff;
            uint32_t value_hi = op >> 16;
            {
                /* MOVZ W0, #value_lo */
                uint32_t sf = 0;
                uint32_t hw = 0;
                *instr++ = 0x52800000 | (sf << 31) | (hw << 21) | (value_lo << 5) | /* Rd */ 0;
            }
            {
                /* MOVK W0, #value_hi, LSL #16 */
                uint32_t sf = 0;
                uint32_t hw = 1;
                *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (value_hi << 5) | /* Rd */ 0;
            }
            {
                /* B L_epilogue */
                ptrdiff_t offset = L_epilogue - instr;
                assert(-0x2000000 <= offset && offset < 0x2000000);
                uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
                *instr++ = 0x14000000 | imm26;
            }
            break;
        }
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
                /* BL um_alloc */
                uint32_t *dest = (uint32_t *)(void *)&um_alloc;
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
                /* BL um_free */
                uint32_t *dest = (uint32_t *)(void *)&um_free;
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
                /* BL um_putchar */
                uint32_t *dest = (uint32_t *)(void *)&um_putchar;
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
                /* BL um_getchar */
                uint32_t *dest = (uint32_t *)(void *)&um_getchar;
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
            /* MOV W0, #(op) */
            uint32_t value_lo = op & 0xffff;
            uint32_t value_hi = op >> 16;
            {
                /* MOVZ W0, #value_lo */
                uint32_t sf = 0;
                uint32_t hw = 0;
                *instr++ = 0x52800000 | (sf << 31) | (hw << 21) | (value_lo << 5) | /* Rd */ 0;
            }
            {
                /* MOVK W0, #value_hi, LSL #16 */
                uint32_t sf = 0;
                uint32_t hw = 1;
                *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (value_hi << 5) | /* Rd */ 0;
            }
            {
                /* B L_epilogue */
                ptrdiff_t offset = L_epilogue - instr;
                assert(-0x2000000 <= offset && offset < 0x2000000);
                uint32_t imm26 = (uint32_t)offset & 0x3FFFFFF;
                *instr++ = 0x14000000 | imm26;
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
            uint32_t value_lo = op & 0xffff;
            uint32_t value_hi = op >> 16;
            {
                /* MOVZ W0, #value_lo */
                uint32_t sf = 0;
                uint32_t hw = 0;
                *instr++ = 0x52800000 | (sf << 31) | (hw << 21) | (value_lo << 5) | /* Rd */ 0;
            }
            {
                /* MOVK W0, #value_hi, LSL #16 */
                uint32_t sf = 0;
                uint32_t hw = 1;
                *instr++ = 0x72800000 | (sf << 31) | (hw << 21) | (value_hi << 5) | /* Rd */ 0;
            }
            {
                /* BL um_invalid */
                uint32_t *dest = (uint32_t *)(void *)&um_invalid;
                ptrdiff_t diff = dest - instr;
                assert(-0x2000000 <= diff && diff < 0x2000000);
                uint32_t imm26 = (uint32_t)diff & 0x3FFFFFF;
                *instr++ = 0x94000000 | imm26;
            }
            for (uint32_t i = 0; i < 4; ++i) {
                /* NOP (allow patching) */
                *instr++ = 0xD503201F;
            }
            break;
        }
    }
    return instr;
}
static void compile(struct array *arr0)
{
    if (program != NULL) {
        munmap(program, program_mem_capacity);
    }
    size_t pagesize = getpagesize();
    size_t size = 16 * (size_t)arr0->length + pagesize;
    size = (size + pagesize - 1) / pagesize * pagesize;
    void *mem = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    if (mem == MAP_FAILED) {
        int e = errno;
        fflush(stdout);
        fprintf(stderr, "<<<mmap failed with errno = %d (%s)>>>\n", e, strerror(e));
        abort();
    }
    program = mem;
    program_mem_capacity = size;
    jumptable = realloc(jumptable, sizeof(void *) * (size_t)arr0->length);
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

    for (uint32_t i = 0; i < arr0->length; ++i) {
        jumptable[i] = instr;
        uint32_t op = arr0->data[i];
        instr = write_instr(instr, op);
        assert(instr - (uint32_t *)jumptable[i] == instr_size(op));
    }
    jumptable[arr0->length] = instr;
    {
        uint32_t imm16 = 0;
        *instr++ = 0xD4400000 | (imm16 << 5); /* HLT #0 */
    }
    program_end = instr;
    int ret = mprotect(mem, size, PROT_READ | PROT_EXEC);
    if (ret != 0) {
        int e = errno;
        fflush(stdout);
        fprintf(stderr, "<<<mprotect failed with errno = %d (%s)>>>\n", e, strerror(e));
        abort();
    }
#if defined(__APPLE__)
    sys_icache_invalidate(mem, (char *)instr - (char *)mem);
#else
    __builtin___clear_cache(mem, (void *)instr);
#endif
}

int main(int argc, char *argv[])
{
    if (argc <= 1) {
        fprintf(stderr, "Usage: %s file.um\n", argv[0]);
        return 1;
    }
    void *rawprogram = NULL;
    size_t rawprogsize = 0;
    {
        FILE *f = fopen(argv[1], "rb");
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
    if (0)
    {
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
        uint32_t last_op = ((uint32_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc))program)(arrays, jumptable, registers, pc);
        switch (last_op >> 28) {
        case 7: /* Halt */
            fflush(stdout);
            fprintf(stderr, "<<<HALT>>>\n");
            return 0;
        case 12: /* Load Program */
            {
                if (0) {
                fflush(stdout);
                fprintf(stderr, "<<<Reload Program>>>\n");
                }
                uint32_t b = (last_op >> 3) & 7;
                uint32_t c = last_op & 7;
                uint32_t i = registers[b];
                assert(i < arraysize);
                assert(arrays[i] != NULL);
                uint32_t length = arrays[i]->length;
                struct array *newprogram = malloc((length + 1) * sizeof(uint32_t));
                assert(newprogram != NULL);
                memcpy(newprogram, arrays[i], (length + 1) * sizeof(uint32_t));
                free(arr0);
                arr0 = newprogram;
                arrays[0] = newprogram;
                pc = registers[c];
                compile(newprogram);
                break;
            }
        default:
            fprintf(stderr, "<<<Invalid Instruction: %08X>>>\n", last_op);
            abort();
        }
    }
}
