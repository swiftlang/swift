#ifndef PASS_OBJECT_SIZE_H
#define PASS_OBJECT_SIZE_H

#include <stddef.h>

/// The size argument is computed with __builtin_object_size(p, 0).
void pos_max(int *p __attribute__((pass_object_size(0))));

/// Type 1 is still a "max" query as far as llvm.objectsize is concerned; the
/// sub-object bit only affects Clang's AST-level constant folding.
void pos_sub(int *p __attribute__((pass_object_size(1))));

/// Type 2 sets the "min" bit on llvm.objectsize.
void pos_min(int *p __attribute__((pass_object_size(2))));

/// pass_dynamic_object_size sets the "dynamic" bit on llvm.objectsize.
void pos_dyn(int *p __attribute__((pass_dynamic_object_size(0))));

/// Two annotated parameters: each gets its own size argument, immediately
/// following the pointer it describes.
void pos_two(int *p __attribute__((pass_object_size(0))),
             int *q __attribute__((pass_object_size(2))));

/// Annotated parameter in the middle of unannotated ones.
void pos_mixed(int *a, int *p __attribute__((pass_object_size(0))), int *b);

/// Non-void result, and a trailing unannotated parameter.
int pos_result(char *p __attribute__((pass_object_size(0))), int n);

/// A struct large enough to be passed indirectly, to exercise the mapping
/// between Clang argument indices and SIL parameter indices.
typedef struct {
  long a, b, c, d, e;
} PosBigStruct;

void pos_big(PosBigStruct s, int *p __attribute__((pass_object_size(0))));

/// Control: no implicit argument at all.
void pos_plain(int *p);

#endif // PASS_OBJECT_SIZE_H
