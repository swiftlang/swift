#if defined(__LINUX__) && defined(__ELF__)
#error "This only works on Linux/ELF"  // Needs testing on other ELF platforms
#else
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


// This forces resolving of these weak symbols but keeps them hidden
// externally
const void *unused1 __attribute__ ((unused, visibility("internal"))) = pthread_self;
const void *unused2 __attribute__ ((unused, visibility("internal"))) = pthread_key_create;
const void *unused3 __attribute__ ((unused, visibility("internal"))) = pthread_once;


// linking libdl into static binaries produces this message:
// "warning: Using 'dlopen' in statically linked applications requires at
// runtime the shared libraries from the glibc version used for linking"
//
// Instead of letting the calls silently fail, show an error and quit.
// This is not actually needed, it is just to aid debugging

#define UNSUPPORTED_FUNC(x) void x() {                                \
  fprintf(stderr, "Unsupported dynamic linker call: %s\n", __func__); \
  abort();                                                            \
}

UNSUPPORTED_FUNC(dlopen)
UNSUPPORTED_FUNC(dlsym)
UNSUPPORTED_FUNC(dladdr)
UNSUPPORTED_FUNC(dlclose)

#endif // linux && ELF
