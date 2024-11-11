#include <stdbool.h>
#include <sys/wait.h>

static inline bool wIfStopped(int status) {
  return WIFSTOPPED(status) != 0;
}

static inline bool wIfExited(int status) {
  return WIFEXITED(status) != 0;
}

static inline bool wIfSignaled(int status) {
  return WIFSIGNALED(status) != 0;
}

static inline int wStopSig(int status) {
  return WSTOPSIG(status);
}
