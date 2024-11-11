#include <errno.h>

static inline
int get_errno() {
  return errno;
}
