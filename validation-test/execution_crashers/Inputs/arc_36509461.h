
#ifndef DEMOHEADER_H
#define DEMOHEADER_H

#include <stdbool.h>

typedef bool (^fake_apply_t)(const char *key, id value);

bool fake_apply(id obj, fake_apply_t applier);

#endif
