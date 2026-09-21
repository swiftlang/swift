#ifndef PASS_OBJECT_SIZE_H
#define PASS_OBJECT_SIZE_H

#include <stddef.h>

void pos_max(int *p __attribute__((pass_object_size(0))));
void pos_plain(int *p);

#endif // PASS_OBJECT_SIZE_H
