#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __lifetimebound __attribute__((lifetimebound))

int * __counted_by(len) simple(int len, int len2, int * p __counted_by(len2) __lifetimebound);

int * __counted_by(len) shared(int len, int * p __counted_by(len) __lifetimebound);

int * __counted_by(len - offset) complexExpr(int len, int offset, int len2, int * p __counted_by(len2) __lifetimebound);

int * __counted_by(len) _Null_unspecified nullUnspecified(int len, int len2, int * _Null_unspecified p __counted_by(len2) __lifetimebound);

int * __counted_by(len) _Nonnull nonnull(int len, int len2, int * _Nonnull p __counted_by(len2) __lifetimebound);

int * __counted_by(len) _Nullable nullable(int len, int len2, int * _Nullable p __counted_by(len2) __lifetimebound);

typedef struct foo opaque_t;
opaque_t * __counted_by(len) opaque(int len, int len2, opaque_t * p __counted_by(len2) __lifetimebound);

int * __counted_by(len) noncountedLifetime(int len, int * p __lifetimebound);

int * __counted_by(13) _Nullable constant(int * _Nullable p __counted_by_or_null(13) __lifetimebound);
