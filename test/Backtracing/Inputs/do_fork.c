#include <unistd.h>

 __attribute__((swiftcall))
 int do_fork() {
  return fork();
}
