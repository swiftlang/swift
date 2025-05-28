#pragma once

#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __lifetimebound __attribute__((lifetimebound))

const void * __sized_by(len) simple(int len, int len2, const void * __sized_by(len2) __lifetimebound p);

const void * __sized_by(len) shared(int len, const void * __sized_by(len) __lifetimebound p);

const void * __sized_by(len - offset) complexExpr(int len, int offset, int len2, const void * __sized_by(len2) __lifetimebound p);

const void * __sized_by(len) _Null_unspecified nullUnspecified(int len, int len2, const void * __sized_by(len2) __lifetimebound _Null_unspecified p);

const void * __sized_by(len) _Nonnull nonnull(int len, int len2, const void * __sized_by(len2) __lifetimebound _Nonnull p);

const void * __sized_by(len) _Nullable nullable(int len, int len2, const void * __sized_by(len2) __lifetimebound _Nullable p);

typedef struct foo opaque_t;
opaque_t * __sized_by(len) opaque(int len, int len2, opaque_t * __sized_by(len2) __lifetimebound p);

const void * __sized_by(len) nonsizedLifetime(int len, const void * __lifetimebound p);