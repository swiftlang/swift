#pragma once

#include <ptrcheck.h>

// Test __counted_by, __sized_by, __ended_by, __single, __indexable and __bidi_indexable pointers
// in function parameters, return values, nested and unnested, pointing to void, char and int.
// Also test VLAs, and incomplete pointer type with __counted_by, since they are pretty much the same
// as __counted_by pointers in the -fbounds-safety model.

struct a {
    int * __counted_by(len) a;
    int len;
};
struct a *a(struct a);

struct b {
    int * __sized_by(len) a;
    int len;
};
struct b *b(struct b);

struct c {
    char * __sized_by(len) a;
    int len;
};
struct c *c(struct c);

struct d {
    void * __sized_by(len) a;
    int len;
};
struct d *d(struct d);

struct e {
    void * __single a;
    int * __single b;
};
struct e *e(struct e);

struct f {
    const char * a;
    char * __null_terminated b;
};
struct f *f(struct f);

#if __has_ptrcheck
struct g {
    void * __bidi_indexable a;
    int * __bidi_indexable b;
};
struct g *g(struct g);

struct h {
    void * __indexable a;
    int * __indexable b;
};
struct h *h(struct h);
#endif

struct i {
    int len;
    int a[__counted_by(len)]; // expected-note {{field 'a' unavailable (cannot import)}}
};
struct i *__single i(struct i *);

const int len1 = 7;
struct j {
    int * __counted_by(len1) a;
    void * __sized_by(len1) b;
};
struct j *j(struct j);

int len2 __unsafe_late_const;
struct k {
    int * __counted_by(len2) a;
    void * __sized_by(len2) b;
};
struct k *k(struct k);

struct l {
    int * __ended_by(end) a;
    int * end;
};
struct l *l(struct l);

struct m {
    void * __ended_by(end) a;
    void * end;
};
struct m *m(struct m);
