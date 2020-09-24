#ifndef __cplusplus
#include <stdbool.h>
#endif

#ifdef __cplusplus
extern "C"
#endif
    bool
    isCanonicalStaticallySpecializedGenericMetadata(void *metadata);

#ifdef __cplusplus
extern "C"
#endif
bool isStaticallySpecializedGenericMetadata(void *self);

#ifdef __cplusplus
extern "C"
#endif
void allocateDirtyAndFreeChunk(void);
