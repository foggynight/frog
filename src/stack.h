#ifndef FROG_STACK_H
#define FROG_STACK_H

#include <stdint.h>

#include "cell.h"

typedef struct stack {
    cell_t *data;      // buffer of cells
    size_t size, next; // size of buffer, index of next
} stack_t;

stack_t *make_stack(size_t size);
void dest_stack(stack_t *stack);

size_t stack_size(stack_t *stack);
cell_t stack_ref(stack_t *stack, size_t i);
cell_t stack_set(stack_t *stack, size_t i, cell_t e);

#endif // FROG_STACK_H
