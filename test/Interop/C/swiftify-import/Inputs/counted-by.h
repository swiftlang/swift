#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

void simple(int len, int * __counted_by(len) p);

void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

void shared(int len, int * __counted_by(len) p1, int * __counted_by(len) p2);

void complexExpr(int len, int offset, int * __counted_by(len - offset) p);

void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified p);

void nonnull(int len, int * __counted_by(len) _Nonnull p);

void nullable(int len, int * __counted_by(len) _Nullable p);

int * __counted_by(len) returnPointer(int len);
