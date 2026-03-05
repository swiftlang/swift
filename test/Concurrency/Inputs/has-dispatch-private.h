// Swift can't #if __has_include, so do it in a C header.
#if __has_include(<dispatch/swift_concurrency_private.h>)
static inline int HasSwiftConcurrencyPrivateHeader(void) { return 1; }
#else
static inline int HasSwiftConcurrencyPrivateHeader(void) { return 0; }
#endif
