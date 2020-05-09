// These APIs marked unavailable on watchOS and tvOS.

#include <sys/types.h>

#if !defined(_WIN32)
pid_t _fork(void);
int _execv(const char *path, char * const *argv);
#endif

