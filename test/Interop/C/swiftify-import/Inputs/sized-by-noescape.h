#pragma once

#include <stdint.h>

#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __noescape __attribute__((noescape))

void simple(int len, const void * __sized_by(len) __noescape p);

void swiftAttr(int len, const void *p) __attribute__((swift_attr(
    "@_SwiftifyImport(.sizedBy(pointer: .param(2), size: \"len\"), .nonescaping(pointer: .param(2)), spanAvailability: \"visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4\")")));

void shared(int len, const void * __sized_by(len) __noescape p1, const void * __sized_by(len) __noescape p2);

void complexExpr(int len, int offset, const void * __sized_by(len - offset) __noescape p);

void nullUnspecified(int len, const void * __sized_by(len) __noescape _Null_unspecified p);

void nonnull(int len, const void * __sized_by(len) __noescape _Nonnull p);

void nullable(int len, const void * __sized_by(len) __noescape _Nullable p);

const void * __sized_by(len) __noescape _Nonnull returnPointer(int len);

typedef struct foo opaque_t;
void opaque(int len, opaque_t * __sized_by(len) __noescape p);

void bytesized(int size, const uint8_t *__sized_by(size) __noescape);

void charsized(char *__sized_by(size) __noescape, int size);

void doublebytesized(uint16_t *__sized_by(size) __noescape, int size);
