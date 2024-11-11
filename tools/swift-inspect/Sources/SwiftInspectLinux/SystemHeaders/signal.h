#include <signal.h>

static inline void* siginfo_si_addr(siginfo_t siginfo) {
  return siginfo.si_addr;
}
