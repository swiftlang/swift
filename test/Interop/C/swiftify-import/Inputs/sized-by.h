#pragma once

#define __sized_by(x) __attribute__((__sized_by__(x)))

void simple(int len, void * __sized_by(len) p);

void swiftAttr(int len, void *p) __attribute__((swift_attr(
    "@_SwiftifyImport(.sizedBy(pointer: .param(2), size: \"len\"))")));

void shared(int len, void * __sized_by(len) p1, void * __sized_by(len) p2);

void complexExpr(int len, int offset, void * __sized_by(len - offset) p);

void nullUnspecified(int len, void * __sized_by(len) _Null_unspecified p);

void nonnull(int len, void * __sized_by(len) _Nonnull p);

void nullable(int len, void * __sized_by(len) _Nullable p);

void * __sized_by(len) returnPointer(int len);

typedef struct foo opaque_t;
void opaque(int len, opaque_t * __sized_by(len) p);
