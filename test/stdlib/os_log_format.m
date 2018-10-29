// RUN: %empty-directory(%t)
// RUN: %clang %target-cc-options -g -O0 -isysroot %sdk -I %swift_src_root/stdlib/public/SDK/os -framework Foundation %swift_src_root/stdlib/public/SDK/os/format.m %swift_src_root/stdlib/public/SDK/os/os_trace_blob.c %s -o %t/os_log_format
// RUN: %target-run %t/os_log_format | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: executable_test

#import <Foundation/Foundation.h>
#import <stdint.h>

#import "format.h"

// CHECK-NOT: FAIL
// CHECK: PASS

static bool
compare_buffers(char *fail_prefix, char *bufa, char *bufb, size_t size)
{
  if (!memcmp(bufa, bufb, size)) {
    return true;
  }

  os_log_fmt_hdr_t hdra = (os_log_fmt_hdr_t)bufa;
  os_log_fmt_hdr_t hdrb = (os_log_fmt_hdr_t)bufb;

  if (hdra->hdr_flags != hdrb->hdr_flags) {
    printf("FAIL: %s: header flags mismatch (0x%x != 0x%x)\n", fail_prefix,
        hdra->hdr_flags, hdrb->hdr_flags);
    return false;
  }

  if (hdra->hdr_cmd_cnt != hdrb->hdr_cmd_cnt) {
    printf("FAIL: %s: command count mismatch (0x%x != 0x%x)\n", fail_prefix,
        hdra->hdr_cmd_cnt, hdrb->hdr_cmd_cnt);
    return false;
  }

  os_log_fmt_cmd_t cmda = (os_log_fmt_cmd_t)((uint8_t *)hdra + 
      sizeof(os_log_fmt_hdr_s));
  os_log_fmt_cmd_t cmdb = (os_log_fmt_cmd_t)((uint8_t *)hdrb + 
      sizeof(os_log_fmt_hdr_s));

  for (size_t idx = 0; idx < hdra->hdr_cmd_cnt; idx++) {
    if (cmda->cmd_flags != cmdb->cmd_flags) {
      printf("FAIL: %s: command %zu flags mismatch (0x%x != 0x%x)\n",
          fail_prefix, idx, cmda->cmd_flags, cmdb->cmd_flags);
      return false;
    }
    if (cmda->cmd_type != cmdb->cmd_type) {
      printf("FAIL: %s: command %zu type mismatch (0x%x != 0x%x)\n",
          fail_prefix, idx, cmda->cmd_type, cmdb->cmd_type);
      return false;
    }
    if (cmda->cmd_size != cmdb->cmd_size) {
      printf("FAIL: %s: command %zu size mismatch (0x%x != 0x%x)\n",
          fail_prefix, idx, cmda->cmd_size, cmdb->cmd_size);
      return false;
    }
    if (memcmp(cmda->cmd_data, cmdb->cmd_data, cmda->cmd_size)) {
      printf("FAIL: %s: command %zu data mismatch\n", fail_prefix, idx);
      printf("COMPILER: ");
      for (size_t i=0; i<cmda->cmd_size; i++) {
        printf("%02x ", cmda->cmd_data[i]);
      }
      printf("\nOVERLAY:  ");
      for (size_t i=0; i<cmdb->cmd_size; i++) {
        printf("%02x ", cmdb->cmd_data[i]);
      }
      printf("\n");
      return false;
    }
    cmda = (os_log_fmt_cmd_t)((uint8_t *)cmda + sizeof(os_log_fmt_cmd_s)
        + cmda->cmd_size);
    cmdb = (os_log_fmt_cmd_t)((uint8_t *)cmdb + sizeof(os_log_fmt_cmd_s)
        + cmdb->cmd_size);
  }
  return true;
}

static bool
_t_log_encode(char buf[OS_LOG_FMT_BUF_SIZE], int saved_errno,
  os_trace_blob_t blob, const char *format, ...)
{
  va_list va;
  bool rv;

  va_start(va, format);
  rv = _os_log_encode(buf, format, va, saved_errno, blob);
  va_end(va);
  return rv;
}

#define compiler_format(buf, fmt, ...) ({ \
    __builtin_os_log_format(buf, fmt, ##__VA_ARGS__); \
    (uint32_t)__builtin_os_log_format_buffer_size(fmt, ##__VA_ARGS__); \
  })

#define overlay_format(buf, fmt, ...) ({ \
    os_trace_blob_s ob = { \
      .ob_s = os_trace_buf, \
      .ob_size = OS_LOG_FMT_BUF_SIZE, \
      .ob_maxsize = OS_LOG_FMT_BUF_SIZE, \
      .ob_binary = true \
    }; \
    _t_log_encode(os_trace_buf, 0, &ob, fmt, ##__VA_ARGS__); \
    (uint32_t)ob.ob_len; \
  })

int
main(int argc, char **argv)
{
  static char compiler_buf[8192];
  static char os_trace_buf[OS_LOG_FMT_BUF_SIZE];
  uint32_t compiler_size, our_size;
  int i;

#define ACCEPT(_fmt, ...) ({ \
    compiler_size = compiler_format(compiler_buf, _fmt, ##__VA_ARGS__); \
    our_size = overlay_format(os_trace_buf, _fmt, ##__VA_ARGS__); \
    if (!compare_buffers("during: \"" _fmt "\"", compiler_buf, os_trace_buf, \
      our_size)) { \
      exit(1); \
    } \
  })

  ACCEPT("doesn't even have an argument");

  ACCEPT("simple char %hhd", (char)10);
  ACCEPT("simple short %hd", (short)10);
  ACCEPT("simple integer %d", 10);
  ACCEPT("simple long long %lld", (long long)10);
  ACCEPT("simple pointer %p", "sad");

  ACCEPT("simple float %f", (float)1.5);
  ACCEPT("simple double %f", 1.5);
#ifdef __LP64__
  // Disabled because the upstream clang output for long double changed
  // and causes CI fallout
  // ACCEPT("simple long double %Lf", (long double)1.5);
#endif

  ACCEPT("integer with a static width %666d", 10);
  ACCEPT("integer with a static precision %.42d", 10);
  ACCEPT("integer with a static width & precision %666.42d", 10);
  ACCEPT("integer with dynamic width %*d", 666, 10);
  ACCEPT("integer with dynamic precision %.*d", 42, 10);
  ACCEPT("integer with dynamic width & precision %*.*d", 666, 42, 10);

  // All the %s tests below are disabled for the overlay because
  // the overlay assumes %s really means %@.

  // ACCEPT("simple string %s", "sad");
  // ACCEPT("string with a static width %666s", "sad");
  // ACCEPT("string with a static precision %.42s", "sad");
  // ACCEPT("string with a static width & precision %666.42s", "sad");
  // ACCEPT("string with dynamic width %*s", 666, "sad");
  // ACCEPT("string with dynamic precision %.*s", 42, "sad");
  // ACCEPT("string with dynamic width & precision %*.*s", 666, 42, "sad");

  ACCEPT("data with a static precision %.42P", "sad");
  ACCEPT("data with a static width & precision %666.42P", "sad");
  ACCEPT("data with dynamic precision %.*P", 42, "sad");
  ACCEPT("data with dynamic width & precision %*.*P", 666, 42, "sad");

  ACCEPT("simple object %@", @"sad");
  ACCEPT("object with a static width %666@", @"sad");
  ACCEPT("object with dynamic width %*@", 666, @"sad");

  // privacy and annotations
  ACCEPT("simple integer %{public}d", 10);
  ACCEPT("simple integer %{private}d", 10);
  ACCEPT("simple integer %{public, blah}d", 10);
  ACCEPT("simple integer %{public, builtin:blah::bluh}d", 10);

  ACCEPT("integer with a static width %{private}666d", 10);
  ACCEPT("integer with a static precision %{private}.42d", 10);
  ACCEPT("integer with a static width & precision %{private}666.42d", 10);
  ACCEPT("integer with dynamic width %{private}*d", 666, 10);
  ACCEPT("integer with dynamic precision %{private}.*d", 42, 10);
  ACCEPT("integer with dynamic width & precision %{private}*.*d", 666, 42, 10);

  // ACCEPT("simple string %{private}s", "sad");
  // ACCEPT("string with a static width %{private}666s", "sad");
  // ACCEPT("string with a static precision %{private}.42s", "sad");
  // ACCEPT("string with a static width & precision %{private}666.42s", "sad");
  // ACCEPT("string with dynamic width %{private}*s", 666, "sad");
  // ACCEPT("string with dynamic precision %{private}.*s", 42, "sad");
  // ACCEPT("string with dynamic width & precision %{private}*.*s", 666, 42, "sad");

  ACCEPT("data with a static precision %{private}.42P", "sad");
  ACCEPT("data with a static width & precision %{private}666.42P", "sad");
  ACCEPT("data with dynamic precision %{private}.*P", 42, "sad");
  ACCEPT("data with dynamic width & precision %{private}*.*P", 666, 42, "sad");

  ACCEPT("simple object %{private}@", @"sad");
  ACCEPT("object with a static width %{private}666@", @"sad");
  ACCEPT("object with dynamic width %{private}*@", 666, @"sad");

  printf("PASS\n");
}
