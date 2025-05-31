#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __lifetimebound __attribute__((lifetimebound))

int * __counted_by(len) simple(int len, int len2, int * __counted_by(len2) __lifetimebound p);

int * __counted_by(len) shared(int len, int * __counted_by(len) __lifetimebound p);

int * __counted_by(len - offset) complexExpr(int len, int offset, int len2, int * __counted_by(len2) __lifetimebound p);

int * __counted_by(len) _Null_unspecified nullUnspecified(int len, int len2, int * __counted_by(len2) __lifetimebound _Null_unspecified p);

int * __counted_by(len) _Nonnull nonnull(int len, int len2, int * __counted_by(len2) __lifetimebound _Nonnull p);

int * __counted_by(len) _Nullable nullable(int len, int len2, int * __counted_by(len2) __lifetimebound _Nullable p);

int * __counted_by(len) noncountedLifetime(int len, int * __lifetimebound p);

int * __counted_by(13) _Nullable constant(int * __counted_by(13) __lifetimebound _Nullable p);
