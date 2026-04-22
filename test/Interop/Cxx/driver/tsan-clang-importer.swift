// REQUIRES: executable_test
// REQUIRES: tsan_runtime
// REQUIRES: OS=macosx

// RUN: %target-swiftc_driver %s -sanitize=thread -import-objc-header %S/sanitizer-detect.h -o %t_tsan -cxx-interoperability-mode=default
// RUN: %target-codesign %t_tsan
// RUN: %target-run %t_tsan

// RUN: %target-swiftc_driver %s -import-objc-header %S/sanitizer-detect.h -o %t_no_tsan -DEXPECT_NO_SANITIZER -cxx-interoperability-mode=default
// RUN: %target-codesign %t_no_tsan
// RUN: %target-run %t_no_tsan

#if EXPECT_NO_SANITIZER
assert(!is_tsan_enabled(), "TSan should NOT be detected without -sanitize=thread")
assert(!is_asan_enabled(), "ASan should NOT be detected without -sanitize=address")
assert(!is_ubsan_enabled(), "UBSan should NOT be detected without -sanitize=undefined")
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected without -sanitize=memtag-stack")
#else
assert(is_tsan_enabled(), "TSan should be detected with -sanitize=thread")
assert(!is_asan_enabled(), "ASan should NOT be detected with -sanitize=thread")
assert(!is_ubsan_enabled(), "UBSan should NOT be detected with -sanitize=thread")
assert(!is_memtagstack_enabled(), "MemTag should NOT be detected with -sanitize=thread")
#endif
