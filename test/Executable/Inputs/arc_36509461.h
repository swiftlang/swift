
#ifndef ARC36509461_H
#define ARC36509461_H

#include <stdbool.h>

typedef bool (^fake_apply_t)(const char *key, id value);

bool fake_apply(id obj, fake_apply_t applier);

#endif
