#pragma once

#include <stdint.h>

#ifndef __sized_by
#define __sized_by(x) __attribute__((__sized_by__(x)))
#endif

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

typedef opaque_t *opaqueptr_t;
void opaqueptr(int len, opaqueptr_t __sized_by(len) p);

void charsized(char *__sized_by(size), int size);

uint8_t *__sized_by(size) bytesized(int size);

void doublebytesized(uint16_t *__sized_by(size), int size);

typedef uint8_t * bytesizedptr_t;
void aliasedBytesized(bytesizedptr_t __sized_by(size) p, int size);
