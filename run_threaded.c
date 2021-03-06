#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
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

    fprintf(stderr, "Loaded program. size=%zu bytes\n", rawprogsize);

    size_t pc = 0;
    uint32_t reg[8] = {0};
    struct array **arr = calloc(1, sizeof(struct array *));
    arr[0] = arr0;
    size_t arraycount = 1;
    uint32_t *freelist = NULL, *freelist_end = NULL;
    uint32_t freelist_capacity = 0;

    static void * const operations[16] = {
        &&OP_CondMove,
        &&OP_ArrayIndex,
        &&OP_ArrayAmendment,
        &&OP_Add,
        &&OP_Mul,
        &&OP_Div,
        &&OP_NotAnd,
        &&OP_Halt,
        &&OP_Alloc,
        &&OP_Abandon,
        &&OP_Output,
        &&OP_Input,
        &&OP_LoadProgram,
        &&OP_Orthography,
        &&OP_Invalid,
        &&OP_Invalid,
    };
    uint32_t op = 0;
    while (1) {
#define JUMP()                                  \
        do {                                    \
            op = arr0->data[pc];                \
            goto *operations[op >> 28];         \
        } while (0)
        JUMP();
    OP_CondMove:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            if (reg[c] != 0) {
                reg[a] = reg[b];
            }
            ++pc;
            JUMP();
        }
    OP_ArrayIndex:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            uint32_t i = reg[b];
            assert(i < arraycount);
            struct array *ai = arr[i];
            assert(ai != NULL);
            assert(reg[c] < ai->length);
            reg[a] = ai->data[reg[c]];
            ++pc;
            JUMP();
        }
    OP_ArrayAmendment:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            uint32_t i = reg[a];
            assert(i < arraycount);
            struct array *ai = arr[i];
            assert(ai != NULL);
            assert(reg[b] < ai->length);
            ai->data[reg[b]] = reg[c];
            ++pc;
            JUMP();
        }
    OP_Add:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            reg[a] = reg[b] + reg[c];
            ++pc;
            JUMP();
        }
    OP_Mul:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            reg[a] = reg[b] * reg[c];
            ++pc;
            JUMP();
        }
    OP_Div:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            assert(reg[c] != 0);
            reg[a] = reg[b] / reg[c];
            ++pc;
            JUMP();
        }
    OP_NotAnd:
        {
            uint32_t a = (op >> 6) & 7;
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            reg[a] = ~(reg[b] & reg[c]);
            ++pc;
            JUMP();
        }
    OP_Halt:
        fflush(stdout);
        fprintf(stderr, "HALT\n");
        return 0;
    OP_Alloc:
        {
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            uint32_t capacity = reg[c];
            uint32_t i = 0;
            if (freelist_end == freelist) {
                i = arraycount;
                ++arraycount;
                arr = realloc(arr, sizeof(struct array *) * arraycount);
                assert(arr != NULL);
            } else {
                i = *(--freelist_end);
                *freelist_end = 0;
            }
            struct array *newarr = calloc(1 + capacity, sizeof(uint32_t));
            assert(newarr != NULL);
            newarr->length = capacity;
            arr[i] = newarr;
            reg[b] = i;
            ++pc;
            JUMP();
        }
    OP_Abandon:
        {
            uint32_t c = op & 7;
            uint32_t i = reg[c];
            assert(i < arraycount);
            assert(i != 0);
            assert(arr[i] != NULL);
            free(arr[i]);
            arr[i] = NULL;
            if (freelist_end - freelist == freelist_capacity) {
                uint32_t freelist_new_capacity = freelist_capacity == 0 ? 32 : freelist_capacity * 2;
                freelist = realloc(freelist, freelist_new_capacity * sizeof(uint32_t));
                memset(freelist + freelist_capacity, 0, (freelist_new_capacity - freelist_capacity) * sizeof(uint32_t));
                freelist_end = freelist + freelist_capacity;
                freelist_capacity = freelist_new_capacity;
            }
            *freelist_end++ = i;
            ++pc;
            JUMP();
        }
    OP_Output:
        {
            uint32_t c = op & 7;
            assert(reg[c] <= 255);
            putchar(reg[c]);
            ++pc;
            JUMP();
        }
    OP_Input:
        {
            uint32_t c = op & 7;
            fflush(stdout);
            int ch = getchar();
            if (ch == EOF) {
                reg[c] = (uint32_t)(-1);
            } else {
                assert(0 <= ch && ch <= 255);
                reg[c] = (uint32_t)ch;
            }
            ++pc;
            JUMP();
        }
    OP_LoadProgram:
        {
            uint32_t b = (op >> 3) & 7;
            uint32_t c = op & 7;
            uint32_t i = reg[b];
            if (UNLIKELY(i != 0)) {
                assert(i < arraycount);
                assert(arr[i] != NULL);
                uint32_t length = arr[i]->length;
                struct array *newprogram = malloc((length + 1) * sizeof(uint32_t));
                assert(newprogram != NULL);
                memcpy(newprogram, arr[i], (length + 1) * sizeof(uint32_t));
                free(arr0);
                arr0 = newprogram;
                arr[0] = newprogram;
            }
            pc = reg[c];
            assert(pc < arr0->length);
            JUMP();
        }
    OP_Orthography:
        {
            uint32_t a = (op >> 25) & 7;
            uint32_t value = op & ((UINT32_C(1) << 25) - 1);
            reg[a] = value;
            ++pc;
            JUMP();
        }
    OP_Invalid:
        {
            fprintf(stderr, "Invalid Instruction: %08X\n", op);
            abort();
        }
    }
}
