#pragma once

#include <ptrcheck.h>

extern int len;
extern int a[__counted_by(len)]; // expected-note{{'a' declared here}}

const char * __null_terminated b = "b";

extern int * __single * __single c;
#if __has_ptrcheck
extern int * __bidi_indexable d;
#endif
