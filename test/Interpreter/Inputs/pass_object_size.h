#ifndef PASS_OBJECT_SIZE_H
#define PASS_OBJECT_SIZE_H

#include <stddef.h>

// Inside a function with a pass_object_size parameter, __builtin_object_size
// on that parameter returns the value the caller passed in the implicit
// argument, so these report exactly what Swift handed them.

static inline size_t pos_report_max(char *const p __attribute__((pass_object_size(0)))) {
  return __builtin_object_size(p, 0);
}

static inline size_t pos_report_min(char *const p __attribute__((pass_object_size(2)))) {
  return __builtin_object_size(p, 2);
}

// The annotated parameter is not first, so a mistake in the argument-index
// mapping shows up here.
static inline size_t pos_report_second(char *a,
                                       char *const p __attribute__((pass_object_size(0))),
                                       int tag) {
  return (a == p && tag == 7) ? __builtin_object_size(p, 0) : 1;
}

// Both parameters are annotated; each gets its own implicit argument.
static inline size_t pos_report_both(char *const p __attribute__((pass_object_size(0))),
                                     char *const q __attribute__((pass_object_size(2)))) {
  return __builtin_object_size(p, 0) ^ __builtin_object_size(q, 2);
}

// Calling back through a plain C function pointer: this is the shape Clang
// refuses to let you form at all. Swift reaches it through a thunk.
typedef size_t (*pos_callback_t)(char *);

static inline size_t pos_call_through(pos_callback_t cb, char *p) {
  return cb(p);
}

#endif // PASS_OBJECT_SIZE_H
