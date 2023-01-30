#ifndef fnptr_h
#define fnptr_h

#include <stddef.h>

typedef struct {
    void (* _Nonnull fnptr)(size_t *);
} Container;

#endif
