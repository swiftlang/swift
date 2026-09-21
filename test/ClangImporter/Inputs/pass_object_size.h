#ifndef PASS_OBJECT_SIZE_H
#define PASS_OBJECT_SIZE_H

#include <stddef.h>

void pos_max(int *p __attribute__((pass_object_size(0))));
void pos_plain(int *p);

// Clang's own overload resolution prefers the pass_object_size candidate.
// Both of these import into Swift with the same type, so Swift needs a
// tie-breaker to match.
__attribute__((overloadable)) size_t ovl(char *p);
__attribute__((overloadable)) size_t ovl(char *p __attribute__((pass_object_size(0))));

// Clang rejects an overload set whose members disagree about pass_object_size
// on only *some* parameters ("conflicting pass_object_size attributes on
// parameters"), so the realistic multi-parameter shape is all-or-nothing.
__attribute__((overloadable)) size_t ovl2(char *p, char *q);
__attribute__((overloadable)) size_t ovl2(char *p __attribute__((pass_object_size(0))),
                                          char *q __attribute__((pass_object_size(2))));

// A separate overload pair for the C-function-pointer test, so that no single
// test file references both members of the same pair. Two members of one pair
// import with the same Swift name *and* the same Swift type, so their Swift
// manglings collide, and referencing both in one file hits that collision.
__attribute__((overloadable)) size_t ovl_fp(char *p);
__attribute__((overloadable)) size_t ovl_fp(char *p __attribute__((pass_object_size(0))));

#endif // PASS_OBJECT_SIZE_H
