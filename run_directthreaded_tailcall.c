#include <assert.h>
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
struct freelist {
    uint32_t location;
    struct freelist *next;
};
uint32_t reg[8] = {0};
struct array **arr;
size_t arrsize = 1;
struct freelist *freelist = NULL;
#define PARAMS size_t pc, struct array *arr0
#define ARGS pc, arr0
typedef void (*OP)(PARAMS);
OP *program;
const OP operations[16];
void recalc(void);
void OP_CondMove(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    if (reg[c] != 0) {
        reg[a] = reg[b];
    }
    ++pc;
    return program[pc](ARGS);
}
void OP_ArrayIndex(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    uint32_t i = reg[b];
    assert(i < arrsize);
    struct array *ai = arr[i];
    assert(ai != NULL);
    assert(reg[c] < ai->length);
    reg[a] = ai->data[reg[c]];
    ++pc;
    return program[pc](ARGS);
}
void OP_ArrayAmendment(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    uint32_t i = reg[a];
    assert(i < arrsize);
    struct array *ai = arr[i];
    assert(ai != NULL);
    assert(reg[b] < ai->length);
    ai->data[reg[b]] = reg[c];
    if (i == 0) {
        program[reg[b]] = operations[reg[c] >> 28];
    }
    ++pc;
    return program[pc](ARGS);
}
void OP_Add(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    reg[a] = reg[b] + reg[c];
    ++pc;
    return program[pc](ARGS);
}
void OP_Mul(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    reg[a] = reg[b] * reg[c];
    ++pc;
    return program[pc](ARGS);
}
void OP_Div(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    assert(reg[c] != 0);
    reg[a] = reg[b] / reg[c];
    ++pc;
    return program[pc](ARGS);
}
void OP_NotAnd(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 6) & 7;
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    reg[a] = ~(reg[b] & reg[c]);
    ++pc;
    return program[pc](ARGS);
}
void OP_Halt(PARAMS)
{
    (void)pc;
    (void)arr0;
    fflush(stdout);
    fprintf(stderr, "HALT\n");
    exit(0);
}
void OP_Alloc(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    uint32_t capacity = reg[c];
    uint32_t i = 0;
    if (freelist == NULL) {
        i = arrsize;
        ++arrsize;
        arr = realloc(arr, sizeof(struct array *) * arrsize);
        assert(arr != NULL);
    } else {
        i = freelist->location;
        struct freelist *next = freelist->next;
        free(freelist);
        freelist = next;
    }
    struct array *newarr = calloc(1 + capacity, sizeof(uint32_t));
    assert(newarr != NULL);
    newarr->length = capacity;
    arr[i] = newarr;
    reg[b] = i;
    ++pc;
    return program[pc](ARGS);
}
void OP_Abandon(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t c = op & 7;
    uint32_t i = reg[c];
    assert(i < arrsize);
    assert(i != 0);
    assert(arr[i] != NULL);
    free(arr[i]);
    arr[i] = NULL;
    {
        struct freelist *f = malloc(sizeof(struct freelist));
        assert(f != NULL);
        f->location = i;
        f->next = freelist;
        freelist = f;
    }
    ++pc;
    return program[pc](ARGS);
}
void OP_Output(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t c = op & 7;
    assert(reg[c] <= 255);
    putchar(reg[c]);
    ++pc;
    return program[pc](ARGS);
}
void OP_Input(PARAMS)
{
    uint32_t op = arr0->data[pc];
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
    return program[pc](ARGS);
}
void OP_LoadProgram(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t b = (op >> 3) & 7;
    uint32_t c = op & 7;
    uint32_t i = reg[b];
    if (UNLIKELY(i != 0)) {
        assert(i < arrsize);
        assert(arr[i] != NULL);
        uint32_t length = arr[i]->length;
        struct array *newprogram = malloc((length + 1) * sizeof(uint32_t));
        assert(newprogram != NULL);
        memcpy(newprogram, arr[i], (length + 1) * sizeof(uint32_t));
        free(arr[0]);
        arr[0] = newprogram;
        arr0 = newprogram;
        recalc();
    }
    pc = reg[c];
    assert(pc < arr[0]->length);
    return program[pc](ARGS);
}
void OP_Orthography(PARAMS)
{
    uint32_t op = arr0->data[pc];
    uint32_t a = (op >> 25) & 7;
    uint32_t value = op & ((UINT32_C(1) << 25) - 1);
    reg[a] = value;
    ++pc;
    return program[pc](ARGS);
}
void OP_Invalid(PARAMS)
{
    uint32_t op = arr0->data[pc];
    fprintf(stderr, "Invalid Instruction: %08X\n", op);
    abort();
}

const OP operations[16] = {
    OP_CondMove,
    OP_ArrayIndex,
    OP_ArrayAmendment,
    OP_Add,
    OP_Mul,
    OP_Div,
    OP_NotAnd,
    OP_Halt,
    OP_Alloc,
    OP_Abandon,
    OP_Output,
    OP_Input,
    OP_LoadProgram,
    OP_Orthography,
    OP_Invalid,
    OP_Invalid,
};
void recalc(void)
{
    struct array *a0 = arr[0];
    uint32_t length = a0->length;
    if (program) {
        free(program);
    }
    program = malloc(sizeof(OP) * length);
    assert(program != NULL);
    for (uint32_t i = 0; i < length; ++i) {
        uint32_t op = a0->data[i];
        program[i] = operations[op >> 28];
    }
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
    struct array *prog = malloc(sizeof(uint32_t) + rawprogsize);
    assert(prog != NULL);
    prog->length = rawprogsize / 4;
    {
        uint8_t *rawprogram_ = (uint8_t *)rawprogram;
        for (size_t i = 0; i < rawprogsize / 4; ++i) {
            uint32_t a = (uint32_t)rawprogram_[4 * i];
            uint32_t b = (uint32_t)rawprogram_[4 * i + 1];
            uint32_t c = (uint32_t)rawprogram_[4 * i + 2];
            uint32_t d = (uint32_t)rawprogram_[4 * i + 3];
            prog->data[i] = (a << 24) | (b << 16) | (c << 8) | d;
        }
        free(rawprogram);
    }
    fprintf(stderr, "Loaded program. size=%zu bytes\n", rawprogsize);
    arr = calloc(1, sizeof(struct array *));
    arr[0] = prog;
    recalc();
    program[0](0, arr[0]);
}