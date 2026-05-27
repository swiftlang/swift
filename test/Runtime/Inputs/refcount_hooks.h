#include <stddef.h>

typedef void *(*AllocObjectFn)(const void *metadata,
                               size_t requiredSize,
                               size_t requiredAlignmentMask);

extern AllocObjectFn _swift_allocObject;
