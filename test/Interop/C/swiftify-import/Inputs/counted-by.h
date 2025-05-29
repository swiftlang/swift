#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

void simple(int len, int * __counted_by(len) p);

void simpleFlipped(int * __counted_by(len) p, int len);

void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

void shared(int len, int * __counted_by(len) p1, int * __counted_by(len) p2);

void complexExpr(int len, int offset, int * __counted_by(len - offset) p);

void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified p);

void nonnull(int len, int * __counted_by(len) _Nonnull p);

void nullable(int len, int * __counted_by(len) _Nullable p);

int * __counted_by(len) returnPointer(int len);

void offByOne(int len, int * __counted_by(len + 1) p);

void offBySome(int len, int offset, int * __counted_by(len + (1 + offset)) p);

void scalar(int m, int n, int * __counted_by(m * n) p);

void bitwise(int m, int n, int o, int * __counted_by(m & n | ~o) p);

void bitshift(int m, int n, int o, int * __counted_by(m << (n >> o)) p);

void constInt(int * __counted_by(42 * 10) p);

void constFloatCastedToInt(int * __counted_by((int) (4.2 / 12)) p);

void sizeofType(int * __counted_by(sizeof(int *)) p);

void sizeofParam(int * __counted_by(sizeof(p)) p);

void derefLen(int * len, int * __counted_by(*len) p);

void lNot(int len, int * __counted_by(!len) p);

void lAnd(int len, int * __counted_by(len && len) p);

void lOr(int len, int * __counted_by(len || len) p);

void floatCastToInt(float meters, int * __counted_by((int) meters) p);

void pointerCastToInt(int *square, int * __counted_by((int) square) p);

void nanAsInt(int * __counted_by((int) (0 / 0)) p);
