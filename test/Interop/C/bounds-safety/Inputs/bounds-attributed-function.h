#pragma once

#include <ptrcheck.h>

#ifndef __unsafe_late_const
#define __unsafe_late_const
#endif

int * __counted_by(len) a(int *__counted_by(len) p, int len);
char * __counted_by(len) b(char *__counted_by(len) p, int len);
char * __sized_by(len) c(char *__sized_by(len) p, int len);
void * __sized_by(len) d(void *__sized_by(len) p, int len);
int * __sized_by(len) e(int *__sized_by(len) p, int len);

int * __ended_by(end) f(int *__ended_by(end) p, int * end);
void * __ended_by(end) g(void *__ended_by(end) p, void * end);

char * __null_terminated h(char *__null_terminated p);
const char * i(const char * p);

char * __single j(char *__single p);
void *__single k(void *__single p);

#if __has_ptrcheck
char * __indexable l(char *__indexable p);
void *__indexable m(void *__indexable p);

char * __bidi_indexable n(char *__bidi_indexable p);
void * __bidi_indexable o(void *__bidi_indexable p);
#endif

#if !__cplusplus
void p(int len, int p[len]);
#endif
void q(int p[__counted_by(len)], int len);

void r(int * __counted_by(*len) *__single p, int *len);
char * __null_terminated *__null_terminated s(char * __null_terminated *__null_terminated p);
char * __single *__single t(char * __single *__single p);

const int len1 = 7;
int * __counted_by(len1) u(int * __counted_by(len1) p);

int len2 __unsafe_late_const;
int * __counted_by(len2) v(int * __counted_by(len2) p);