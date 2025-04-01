#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <stdint.h>

#ifdef __linux__ 

#ifdef __cplusplus
extern "C"
#endif
ssize_t getrandom(void *buf, size_t len, unsigned int flags);

#ifdef __cplusplus
extern "C"
#endif
void arc4random_buf(void *buf, size_t nbytes);

void arc4random_buf(void *buf, size_t nbytes) {
  while (nbytes > 0) {
    ssize_t actual_nbytes = 0;
    do {
    	actual_nbytes = getrandom(buf, nbytes, 0);
    } while (actual_nbytes == -1 && errno == EINTR);

    if (actual_nbytes == -1) {
      abort();
    }
    
    buf = (uint8_t *)(buf) + actual_nbytes;
    nbytes -= actual_nbytes;
  }
}

#endif
