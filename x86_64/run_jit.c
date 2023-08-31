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
#include "x86asm.h"

struct array {
    uint32_t length;
    uint32_t data[];
};

static uint32_t arraysize;
static struct array **arrays; // struct array *arrays[arraysize]
static uint32_t *freelist = NULL, *freelist_end = NULL;
static uint32_t freelist_capacity = 0;
static void **jumptable; // void *jumptable[arr0size + 1]
static void *program; // uint32_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc)
static void *program_end;
static size_t program_mem_capacity;
static uint8_t *L_epilogue;
static uint8_t *L_call_fn[5]; // um_modify_0, um_alloc, um_free, um_putchar, um_getchar
static bool verbose = false;
static char *presupplied_input = NULL;
static bool discard_initial_output = false;

static void um_modify_0(uint32_t b, uint32_t c, uint32_t origvalue)
{
    if (origvalue == c) {
        return;
    }
    uint8_t buf[10];
    uint8_t *instr = buf;
    uint8_t *target = jumptable[b];
    instr = mov_r32_imm32(instr, eax, b); // mov eax, <b>; 5 bytes
    instr = jmp_rel32(instr, L_epilogue - (target + 10)); // jmp L_epilogue; 5 bytes
    assert(instr == buf + 10);
    if (memcmp(target, buf, 10) == 0) {
        // already patched
        return;
    }
    if (verbose) {
        fprintf(stderr, "<<<self modification>>>");
    }
    memcpy(target, buf, 10);
    __builtin___clear_cache((void *)target, (void *)(target + 10)); // no-op; write barrier
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
    return (struct alloc_result){/* eax */ i, /* rdx */ arrays};
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
static const enum reg64 Rarrays = rbx;
static const enum reg64 Rjumptable = rcx;
static const enum reg32 REG[8] = {r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d};
static const size_t MAX_BYTES_PER_INSTRUCTION = 31;
static uint8_t *write_instr(uint8_t *instr, uint32_t index, uint32_t op)
{
    uint8_t *instr_start = instr;
    switch (op >> 28) {
    case 0: /* Conditional Move */
        {
            // <= 7 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            if (A == B) {
                // no-op
            } else {
                instr = test_r32_r32(instr, C, C); // test C, C; <= 3 bytes
                instr = cmovne_r32_r32(instr, A, B); // cmovne A, B; <= 4 bytes
            }
            break;
        }
    case 1: /* Array Index */
        {
            // <= 10 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            instr = mov_r64_qword_ptr(instr, rax, Rarrays, SCALE_BY_8, (enum reg64)B, 0); // mov rax, qword ptr [Rarrays + 8 * B]; <= 5 bytes
            instr = mov_r32_dword_ptr(instr, A, rax, SCALE_BY_4, (enum reg64)C, 4); // mov A, dword ptr [rax + 4 * C + 4]; <= 5 bytes
            break;
        }
    case 2: /* Array Amendment */
        {
            // <= 31 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            instr = mov_r64_qword_ptr(instr, rax, Rarrays, SCALE_BY_8, (enum reg64)A, 0); // mov rax, qword ptr [Rarrays + 8 * A]; <= 5 bytes
            instr = mov_r32_dword_ptr(instr, edx, rax, SCALE_BY_4, (enum reg64)B, 4); // mov edx, dword ptr [rax + 4 * B + 4]; <= 5 bytes
            instr = mov_dword_ptr_r32(instr, rax, SCALE_BY_4, (enum reg64)B, 4, C); // mov dword ptr [rax + 4 * B + 4], C; <= 5 bytes
            instr = test_r32_r32(instr, A, A); // test A, A; <= 3 bytes
            uint8_t *instr_jne = instr;
            instr += 2; // jne L_next (filled later); 2 bytes
            instr = mov_r32_r32(instr, edi, B); // mov edi, B; <= 3 bytes
            instr = mov_r32_r32(instr, esi, C); // mov esi, C; <= 3 bytes
            instr = call_rel32_ptr(instr, L_call_fn[0]); // call um_modify_0; 5 bytes
            jne_rel8(instr_jne, instr - instr_jne - 2); // jne L_next
            /* L_next: */
            break;
        }
    case 3: /* Addition */
        {
            // <= 6 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            instr = lea_r32(instr, A, B, SCALE_BY_1, C, 0); // lea A, [B + C]; <= 6 bytes
            break;
        }
    case 4: /* Multiplication */
        {
            // <= 9 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            instr = mov_r32_r32(instr, eax, B); // mov eax, B; <= 3 bytes
            instr = mul_r32(instr, C); // mul C; <= 3 bytes
            instr = mov_r32_r32(instr, A, eax); // mov A, eax; <= 3 bytes
            break;
        }
    case 5: /* Division */
        {
            // <= 12 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            // assert(value of C != 0);
            instr = xor_r32_r32(instr, edx, edx); // xor edx, edx; <= 3 bytes
            instr = mov_r32_r32(instr, eax, B); // mov eax, B; <= 3 bytes
            instr = div_r32(instr, C); // div C; <= 3 bytes
            instr = mov_r32_r32(instr, A, eax); // mov A, eax; <= 3 bytes
            break;
        }
    case 6: /* Not-And */
        {
            // <= 9 bytes
            enum reg32 A = REG[(op >> 6) & 7];
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            if (B == C) {
                // Bitwise Negation
                if (A != B) {
                    instr = mov_r32_r32(instr, A, B); // mov A, B; <= 3 bytes
                }
                instr = not_r32(instr, A); // not A; <= 3 bytes
            } else if (A == B) {
                // Not-And
                instr = and_r32_r32(instr, A, C); // and A, C; <= 3 bytes
                instr = not_r32(instr, A); // not A; <= 3 bytes
            } else if (A == C) {
                // Not-And
                instr = and_r32_r32(instr, A, B); // and A, B; <= 3 bytes
                instr = not_r32(instr, A); // not A; <= 3 bytes
            } else {
                // Not-And
                instr = mov_r32_r32(instr, A, B); // mov A, B; <= 3 bytes
                instr = and_r32_r32(instr, A, C); // and A, C; <= 3 bytes
                instr = not_r32(instr, A); // not A; <= 3 bytes
            }
            break;
        }
    case 8: /* Allocation */
        {
            // <= 14 bytes
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            instr = mov_r32_r32(instr, edi, C); // mov edi, C; <= 3 bytes
            instr = call_rel32_ptr(instr, L_call_fn[1]); // call um_alloc; 5 bytes
            // results: eax, rdx
            instr = mov_r32_r32(instr, B, eax); // mov B, eax; <= 3 bytes
            instr = mov_r64_r64(instr, Rarrays, rdx); // mov Rarrays, rdx; <= 3 bytes
            break;
        }
    case 9: /* Abandonment */
        {
            // <= 8 bytes
            enum reg32 C = REG[op & 7];
            instr = mov_r32_r32(instr, edi, C); // mov edi, C; <= 3 bytes
            instr = call_rel32_ptr(instr, L_call_fn[2]); // call um_free; 5 bytes
            break;
        }
    case 10: /* Output */
        {
            // <= 8 bytes
            enum reg32 C = REG[op & 7];
            instr = mov_r32_r32(instr, edi, C); // mov edi, C; <= 3 bytes
            instr = call_rel32_ptr(instr, L_call_fn[3]); // call um_putchar; 5 bytes
            break;
        }
    case 11: /* Input */
        {
            // <= 8 bytes
            enum reg32 C = REG[op & 7];
            instr = call_rel32_ptr(instr, L_call_fn[4]); // call um_getchar; 5 bytes
            // result: eax
            instr = mov_r32_r32(instr, C, eax); // mov C, eax; <= 3 bytes
            break;
        }
    case 12: /* Load Program */
        {
            // <= 20 bytes
            enum reg32 B = REG[(op >> 3) & 7];
            enum reg32 C = REG[op & 7];
            instr = test_r32_r32(instr, B, B); // test B, B; <= 3 bytes
            uint8_t *jne = instr; instr += 2; // jne L_reload; 2 bytes
            instr = jmp_qword_ptr(instr, Rjumptable, SCALE_BY_8, (enum reg64)C, 0); // jmp qword ptr [Rjumptable + 8 * C]; <= 5 bytes
            jne_rel8(jne, instr - jne - 2); // L_reload:
            instr = mov_r32_imm32(instr, eax, index); // mov eax, <index>; 5 bytes
            instr = jmp_rel32_ptr(instr, L_epilogue); // jmp L_epilogue; 5 bytes
            break;
        }
    case 13: /* Orthography */
        {
            // <= 6 bytes
            enum reg32 A = REG[(op >> 25) & 7];
            uint32_t value = op & ((UINT32_C(1) << 25) - 1);
            instr = mov_r32_imm32(instr, A, value); // mov A, imm; <= 6 bytes
            break;
        }
    case 7: /* Halt */
    default: /* Invalid */
        {
            // 10 bytes
            instr = mov_r32_imm32(instr, eax, index); // mov eax, <index>; 5 bytes
            instr = jmp_rel32_ptr(instr, L_epilogue); // jmp L_epilogue; 5 bytes
        }
        break;
    }
    assert(instr - instr_start <= MAX_BYTES_PER_INSTRUCTION);
    if (instr - instr_start < 10) {
        switch (10 - (instr - instr_start)) {
        case 1: *instr++ = 0x90; break;
        case 2: *instr++ = 0x66; *instr++ = 0x90; break;
        case 3: *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x00; break;
        case 4: *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x40; *instr++ = 0x00; break;
        case 5: *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x44; *instr++ = 0x00; *instr++ = 0x00; break;
        case 6: *instr++ = 0x66; *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x44; *instr++ = 0x00; *instr++ = 0x00; break;
        case 7: *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x80; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; break;
        case 8: *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x84; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; break;
        case 9: *instr++ = 0x66; *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x84; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x00; break;
        case 10: *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x44; *instr++ = 0x00; *instr++ = 0x00; *instr++ = 0x0f; *instr++ = 0x1f; *instr++ = 0x44; *instr++ = 0x00; *instr++ = 0x00; break;
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
    size_t size = MAX_BYTES_PER_INSTRUCTION * (size_t)arr0->length + pagesize;
    size = (size + pagesize - 1) / pagesize * pagesize;
    void *mem = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_ANON | MAP_PRIVATE, -1, 0);
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
    uint8_t *instr = mem;
    /*
     * rbx (callee-save): arrays
     * rcx (caller-save): jumptable
     * r8d (caller-save): VM's R0
     * r9d (caller-save): VM's R1
     * r10d (caller-save): VM's R2
     * r11d (caller-save): VM's R3
     * r12d (callee-save): VM's R4
     * r13d (callee-save): VM's R5
     * r14d (callee-save): VM's R6
     * r15d (callee-save): VM's R7
     */

    /* Prologue */
    static const uint8_t prologue[] = {
        0x55,                         // push rbp
        0x48, 0x89, 0xe5,             // mov  rbp, rsp
        0x41, 0x57,                   // push r15
        0x41, 0x56,                   // push r14
        0x41, 0x55,                   // push r13
        0x41, 0x54,                   // push r12
        0x53,                         // push rbx
        0x48, 0x83, 0xec, 0x28,       // sub  rsp, 40
        0x48, 0x89, 0xfb,             // mov  rbx, rdi
        0x44, 0x8b, 0x02,             // mov  r8d, dword ptr [rdx]
        0x44, 0x8b, 0x4a, 0x04,       // mov  r9d, dword ptr [rdx + 4]
        0x44, 0x8b, 0x52, 0x08,       // mov  r10d, dword ptr [rdx + 8]
        0x44, 0x8b, 0x5a, 0x0c,       // mov  r11d, dword ptr [rdx + 12]
        0x44, 0x8b, 0x62, 0x10,       // mov  r12d, dword ptr [rdx + 16]
        0x44, 0x8b, 0x6a, 0x14,       // mov  r13d, dword ptr [rdx + 20]
        0x44, 0x8b, 0x72, 0x18,       // mov  r14d, dword ptr [rdx + 24]
        0x44, 0x8b, 0x7a, 0x1c,       // mov  r15d, dword ptr [rdx + 28]
        0x48, 0x89, 0x54, 0x24, 0x18, // mov  qword ptr [rsp + 24], rdx # registers
        0x48, 0x89, 0x74, 0x24, 0x10, // mov  qword ptr [rsp + 16], rsi # jumptable
        0x48, 0x8b, 0x04, 0xce,       // mov  rax, qword ptr [rsi + 8*rcx] # jumptable[initial_pc]
        0x48, 0x89, 0xf1,             // mov  rcx, rsi
        0xff, 0xe0,                   // jmp  rax
    };
    memcpy(instr, prologue, sizeof(prologue));
    instr += sizeof(prologue);

    /* Epilogue */
    /* L_epilogue: */
    L_epilogue = instr;
    static const uint8_t epilogue[] = {
        0x48, 0x8b, 0x54, 0x24, 0x18, // mov rdx, qword ptr [rsp + 24]
        0x44, 0x89, 0x02,             // mov dword ptr [rdx], r8d
        0x44, 0x89, 0x4a, 0x04,       // mov dword ptr [rdx + 4], r9d
        0x44, 0x89, 0x52, 0x08,       // mov dword ptr [rdx + 8], r10d
        0x44, 0x89, 0x5a, 0x0c,       // mov dword ptr [rdx + 12], r11d
        0x44, 0x89, 0x62, 0x10,       // mov dword ptr [rdx + 16], r12d
        0x44, 0x89, 0x6a, 0x14,       // mov dword ptr [rdx + 20], r13d
        0x44, 0x89, 0x72, 0x18,       // mov dword ptr [rdx + 24], r14d
        0x44, 0x89, 0x7a, 0x1c,       // mov dword ptr [rdx + 28], r15d
        0x48, 0x83, 0xc4, 0x28,       // add rsp, 40
        0x5b,                         // pop rbx
        0x41, 0x5c,                   // pop r12
        0x41, 0x5d,                   // pop r13
        0x41, 0x5e,                   // pop r14
        0x41, 0x5f,                   // pop r15
        0x5d,                         // pop rbp
        0xc3,                         // ret
    };
    memcpy(instr, epilogue, sizeof(epilogue));
    instr += sizeof(epilogue);

    /* Call thunks */
    {
        uintptr_t addrs[5] = {(uintptr_t)um_modify_0, (uintptr_t)um_alloc, (uintptr_t)um_free, (uintptr_t)um_putchar, (uintptr_t)um_getchar};
        for (int i = 0; i < 5; ++i) {
            L_call_fn[i] = instr;
            static const uint8_t code1[] = {
                0x48, 0x83, 0xec, 0x08,       // sub rsp, 8
                0x44, 0x89, 0x44, 0x24, 0x10, // mov dword ptr [rsp + 16], r8d
                0x44, 0x89, 0x4c, 0x24, 0x14, // mov dword ptr [rsp + 20], r9d
                0x44, 0x89, 0x54, 0x24, 0x18, // mov dword ptr [rsp + 24], r10d
                0x44, 0x89, 0x5c, 0x24, 0x1c, // mov dword ptr [rsp + 28], r11d
            };
            memcpy(instr, code1, sizeof(code1));
            instr += sizeof(code1);
            instr = mov_r64_imm64(instr, rax, addrs[i]); // mov rax, <absolute address>
            static const uint8_t code2[] = {
                0xff, 0xd0,                   // call rax
                0x44, 0x8b, 0x44, 0x24, 0x10, // mov r8d, dword ptr [rsp + 16]
                0x44, 0x8b, 0x4c, 0x24, 0x14, // mov r9d, dword ptr [rsp + 20]
                0x44, 0x8b, 0x54, 0x24, 0x18, // mov r10d, dword ptr [rsp + 24]
                0x44, 0x8b, 0x5c, 0x24, 0x1c, // mov r11d, dword ptr [rsp + 28]
                0x48, 0x8b, 0x4c, 0x24, 0x20, // mov rcx, qword ptr [rsp + 32]
                0x48, 0x83, 0xc4, 0x08,       // add rsp, 8
                0xc3,                         // ret
            };
            memcpy(instr, code2, sizeof(code2));
            instr += sizeof(code2);
        }
    }

    for (uint32_t i = 0; i < arr0->length; ++i) {
        jumptable[i] = instr;
        uint32_t op = arr0->data[i];
        instr = write_instr(instr, i, op);
    }
    jumptable[arr0->length] = instr;
    {
        *instr++ = 0x0f; *instr++ = 0x0b; /* UD2 */
    }
    program_end = instr;
    __builtin___clear_cache(mem, (void *)instr);
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
    fprintf(stderr, "<<<loaded program. size=%zu bytes>>>\n", rawprogsize);
    compile(arr0);
    fprintf(stderr, "<<<compiled>>>\n");
    if (dump) {
        FILE *out = fopen("jit-dump.hex", "w");
        assert(out != NULL);
        uint8_t *instr = program;
        uint8_t *instr_end = program_end;
        for (size_t j = 0; instr != instr_end; ++instr, ++j) {
            uint8_t i = *instr;
            fprintf(out, "%02X%c", i, j % 8 == 7 ? '\n' : ' ');
        }
        fclose(out);
        fprintf(stderr, "<<<dumped>>>\n");
    }
    uint32_t registers[8] = {0};
    uint32_t pc = 0;
    while (1) {
        pc = ((uint32_t (*)(struct array **arrays, void **jumptable, uint32_t registers[8], uint32_t initial_pc))program)(arrays, jumptable, registers, pc);

        uint32_t op = arrays[0]->data[pc++];

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
                    uint32_t origvalue = ai->data[vb];
                    ai->data[vb] = vc;
                    um_modify_0(vb, vc, origvalue);
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
                        fprintf(stderr, "<<<reload program>>>\n");
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
            fprintf(stderr, "<<<invalid instruction: %08X>>>\n", op);
            abort();
        }
    }
}
