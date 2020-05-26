#ifdef __cplusplus
#include <cstdint>
#include <cstddef>
#else
#include <stdint.h>
#include <stddef.h>
#endif

#ifdef __cplusplus
extern "C"
#endif
void *completeMetadata(void *metadata);

#ifdef __cplusplus
extern "C"
#endif
int64_t *trailingFlagsForStructMetadata(void *metadata);

#ifdef __cplusplus
extern "C"
#endif
const uint32_t *fieldOffsetsForStructMetadata(void *metadata);

#ifdef __cplusplus
extern "C"
#endif
int64_t *trailingFlagsForEnumMetadata(void *metadata);

#ifdef __cplusplus
extern "C"
#endif
size_t *payloadSizeForEnumMetadata(void *metadata);
