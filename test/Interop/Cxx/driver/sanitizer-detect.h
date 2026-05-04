#pragma once

bool is_asan_enabled() {
#if __has_feature(address_sanitizer)
  return true;
#else
  return false;
#endif
}

bool is_tsan_enabled() {
#if __has_feature(thread_sanitizer)
  return true;
#else
  return false;
#endif
}

bool is_ubsan_enabled() {
#if __has_feature(undefined_behavior_sanitizer)
  return true;
#else
  return false;
#endif
}

bool is_memtagstack_enabled() {
#if __has_feature(memtag_stack)
  return true;
#else
  return false;
#endif
}
