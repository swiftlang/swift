#include "overrides.h"

#if !defined(_WIN32)
extern pid_t fork(void);
extern int execv(const char *path, char * const *argv);

pid_t _fork(void) {
  return fork();
}

int _execv(const char *path, char * const *argv) {
  return execv(path, argv);
}
#endif

