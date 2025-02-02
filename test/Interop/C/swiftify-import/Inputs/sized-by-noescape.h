#pragma once

#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __noescape __attribute__((noescape))

void simple(int len, const void * __sized_by(len) __noescape p);

void swiftAttr(int len, const void *p) __attribute__((swift_attr(
    "@_SwiftifyImport(.sizedBy(pointer: .param(2), size: \"len\"), .nonescaping(pointer: .param(2)))")));

void shared(int len, const void * __sized_by(len) __noescape p1, const void * __sized_by(len) __noescape p2);

void complexExpr(int len, int offset, const void * __sized_by(len - offset) __noescape p);

void nullUnspecified(int len, const void * __sized_by(len) __noescape _Null_unspecified p);

void nonnull(int len, const void * __sized_by(len) __noescape _Nonnull p);

// Nullable ~Escapable types not supported yet
//void nullable(int len, const void * __sized_by(len) __noescape _Nullable p);

const void * __sized_by(len) __noescape _Nonnull returnPointer(int len);

typedef struct foo opaque_t;
void opaque(int len, opaque_t * __sized_by(len) __noescape p);

