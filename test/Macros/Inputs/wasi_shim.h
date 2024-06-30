typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

typedef uint8_t wasi_signal_t;
typedef uint16_t wasi_errno_t;
typedef uint32_t wasi_exitcode_t;
typedef int wasi_fd_t;
typedef __SIZE_TYPE__ wasi_size_t;

typedef struct wasi_ciovec_t {
  const uint8_t *buf;
  wasi_size_t buf_len;
} wasi_ciovec_t;

#define WASI_SIGNAL_ABRT (6)

wasi_errno_t wasi_fd_write(
  wasi_fd_t fd,
  const wasi_ciovec_t *iovs,
  wasi_size_t iovs_len,
  wasi_size_t *nwritten
) __attribute__((
  __import_module__("wasi_snapshot_preview1"),
  __import_name__("fd_write"),
  __warn_unused_result__
));

_Noreturn void wasi_proc_exit(
  wasi_exitcode_t code
) __attribute__((
  __import_module__("wasi_snapshot_preview1"),
  __import_name__("proc_exit")
));

wasi_errno_t wasi_proc_raise(
  wasi_signal_t sig
) __attribute__((
  __import_module__("wasi_snapshot_preview1"),
  __import_name__("proc_raise")
));

// libc shims

static inline wasi_size_t swift_strlen(const char *buf) {
  wasi_size_t len = 0;
  while (buf[len]) len++;
  return len;
}

static inline wasi_errno_t swift_write(int fd, const void *buf, wasi_size_t len) {
  struct wasi_ciovec_t vec = { .buf = (const uint8_t *)buf, .buf_len = len };
  wasi_size_t nwritten = 0;
  return wasi_fd_write(fd, &vec, 1, &nwritten);
}

static inline wasi_errno_t swift_puts(int fd, const char *buf) {
  return swift_write(fd, buf, swift_strlen(buf));
}
