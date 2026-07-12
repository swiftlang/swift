#include <pthread.h>

extern long _swift_thread_isMain(void);

static void *embedded_platform_worker(void *context) {
  (void)context;
  return _swift_thread_isMain() ? (void *)1 : 0;
}

int embedded_platform_worker_is_main(void) {
  pthread_t thread;
  void *result;
  if (pthread_create(&thread, 0, embedded_platform_worker, 0) != 0) {
    return 1;
  }
  if (pthread_join(thread, &result) != 0) {
    return 1;
  }
  return result != 0;
}
