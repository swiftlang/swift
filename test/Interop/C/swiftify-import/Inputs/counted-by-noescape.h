#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
#define __lifetimebound __attribute__((lifetimebound))

void simple(int len, int * __counted_by(len) __noescape p);

void swiftAttr(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"), .nonescaping(pointer: .param(2)), spanAvailability: \"visionOS 1.1, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4\")")));

void shared(int len, int * __counted_by(len) __noescape p1, int * __counted_by(len) __noescape p2);

void complexExpr(int len, int offset, int * __counted_by(len - offset) __noescape p);

void nullUnspecified(int len, int * __counted_by(len) _Null_unspecified __noescape p);

void nonnull(int len, int * __counted_by(len) _Nonnull __noescape p);

void nullable(int len, int * __counted_by(len) _Nullable p __noescape);

int * __counted_by(len) __noescape returnPointer(int len);

int * __counted_by(len1) returnLifetimeBound(int len1, int len2, int * __counted_by(len2) p __lifetimebound);

void anonymous(int len, int * __counted_by(len) _Nullable __noescape);
