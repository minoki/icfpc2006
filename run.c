#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
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
static int usage(const char *argv0)
{
    fprintf(stderr, "Usage: %s file.um\nOptions:\n  --input [text]\n  --discard-initial-output\n  --help\n", argv0);
    return 1;
}
int main(int argc, char *argv[])
{
    const char *filename = NULL;
    char *presupplied_input = NULL;
    bool discard_initial_output = false;
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
    struct array *program = malloc(sizeof(uint32_t) + rawprogsize);
    assert(program != NULL);
    program->length = rawprogsize / 4;
    {
        uint8_t *rawprogram_ = (uint8_t *)rawprogram;
        for (size_t i = 0; i < rawprogsize / 4; ++i) {
            uint32_t a = (uint32_t)rawprogram_[4 * i];
            uint32_t b = (uint32_t)rawprogram_[4 * i + 1];
            uint32_t c = (uint32_t)rawprogram_[4 * i + 2];
            uint32_t d = (uint32_t)rawprogram_[4 * i + 3];
            program->data[i] = (a << 24) | (b << 16) | (c << 8) | d;
        }
        free(rawprogram);
    }
    fprintf(stderr, "<<<Loaded program. size=%zu bytes>>>\n", rawprogsize);
    size_t pc = 0;
    uint32_t reg[8] = {0};
    struct array **arr = calloc(1, sizeof(struct array *));
    arr[0] = program;
    size_t arraycount = 1;
    uint32_t *freelist = NULL, *freelist_end = NULL;
    uint32_t freelist_capacity = 0;
    while (1) {
        assert(pc < program->length);
        uint32_t op = program->data[pc];
        ++pc;
        switch (op >> 28) {
        case 0: /* Conditional Move */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                if (reg[c] != 0) {
                    reg[a] = reg[b];
                }
                break;
            }
        case 1: /* Array Index */
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
                break;
            }
        case 2: /* Array Amendment */
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
                break;
            }
        case 3: /* Addition */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                reg[a] = reg[b] + reg[c];
                break;
            }
        case 4: /* Multiplication */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                reg[a] = reg[b] * reg[c];
                break;
            }
        case 5: /* Division */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                assert(reg[c] != 0);
                reg[a] = reg[b] / reg[c];
                break;
            }
        case 6: /* Not-And */
            {
                uint32_t a = (op >> 6) & 7;
                uint32_t b = (op >> 3) & 7;
                uint32_t c = op & 7;
                reg[a] = ~(reg[b] & reg[c]);
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
                break;
            }
        case 9: /* Abandonment */
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
                break;
            }
        case 10: /* Output */
            {
                uint32_t c = op & 7;
                assert(reg[c] <= 255);
                if (!discard_initial_output || presupplied_input == NULL || *presupplied_input == '\0') {
                    putchar(reg[c]);
                }
                break;
            }
        case 11: /* Input */
            {
                uint32_t c = op & 7;
                int ch;
                if (presupplied_input == NULL || *presupplied_input == '\0') {
                    fflush(stdout);
                    ch = getchar();
                } else {
                    ch = *presupplied_input++;
                }
                if (ch == EOF) {
                    reg[c] = (uint32_t)(-1);
                } else {
                    assert(0 <= ch && ch <= 255);
                    reg[c] = (uint32_t)ch;
                }
                break;
            }
        case 12: /* Load Program */
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
                    free(program);
                    program = newprogram;
                    arr[0] = newprogram;
                }
                pc = reg[c];
                break;
            }
        case 13: /* Orthography */
            {
                uint32_t a = (op >> 25) & 7;
                uint32_t value = op & ((UINT32_C(1) << 25) - 1);
                reg[a] = value;
                break;
            }
        default:
            fflush(stdout);
            fprintf(stderr, "<<<Invalid Instruction: %08X>>>\n", op);
            abort();
        }
    }
}
